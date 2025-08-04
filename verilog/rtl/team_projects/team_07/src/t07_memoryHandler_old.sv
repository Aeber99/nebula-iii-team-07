
 typedef enum logic [2:0] {  
        FETCH = 0,
        F_WAIT = 1,
        DATA = 2,
        D_WAIT = 3,
        DELAY = 4,
        F_DELAY = 5,
        LOAD_DELAY = 6
    } state_t0;


module t07_memoryHandler_old (

    // Inputs
    input logic clk, nrst, busy, // Busy signal to indicate if the memory handler is currently processing
    input logic [3:0] memOp,
    input logic memWrite, memRead,
    input logic memSource,          //if we are writing from the FPU or ALU
    input logic [31:0] ALU_address, // Address for memory operations that comes from the ALU
    input logic [31:0] FPU_data_i,    // Data from the FPU register to store in memory
    input logic [31:0] regData_i, // Data from the internal register file to store in memory
    input logic [31:0] dataMMIO_i,     // Data from external memory to read/write
    
    //outputs
    output logic [31:0] dataMMIO_o, // Data to write to external memory
    output logic [31:0] addrMMIO_o, // Address to write to external memory   
    output logic [31:0] regData_o,  // Data to the register
    output logic freeze_o,            // Freeze signal to pause CPU operations during memory access
    output logic [1:0] rwi,          // read - 01, write - 10, idle - 00, fetch -11 
    output state_t0 state,
    output logic addrControl, // control for address mux, 0 when fetch, 1 when l/s
    output logic busy_o_edge

);
    //edge detector
    logic [1:0] load_ct, load_ct_n;
    logic prev_busy_o;
    logic [31:0] regData_o_n;
    state_t0 state_n;


    always_ff @(negedge nrst, posedge clk) begin
        if(~nrst) begin
            prev_busy_o <= '0;
        end else begin
            prev_busy_o <= busy;
        end
    end

    assign busy_o_edge = (~busy && prev_busy_o); //detects falling edge

    //fsm
    always_ff @(negedge nrst, posedge clk) begin
        if (~nrst) begin
            state <= FETCH;
        end else begin
            state <= state_n;
            load_ct <= load_ct_n;
            regData_o <= regData_o_n;
        end
    end

    always_comb begin
        case(state) 
            FETCH: //state 0
                begin
                    addrControl = 1;
                    rwi = 'b11; 
                    freeze_o = 0; //fetch instr 
                    state_n = F_WAIT; 

                    //default/error settings
                    regData_o_n = regData_o;
                    addrMMIO_o = 'hDEADBEEF; 
                    dataMMIO_o = 'hDEADBEEF;
                    load_ct_n = '0;
                end
            F_WAIT: //state 1
                begin 
                    addrControl = 1;
                    load_ct_n = '0; 
                    rwi = 'b11; 
                    freeze_o = 1;
                    if(busy_o_edge == 'b1) begin 
                        state_n = F_DELAY; 
                    end else begin
                        state_n = F_WAIT; 
                    end 

                    regData_o_n = regData_o;
                    addrMMIO_o = 'hDEADBEEF; 
                    dataMMIO_o = 'hDEADBEEF;
                end
            DATA: //state 2
                begin 
                    addrControl = 1;
                    if(memWrite == 1) begin //STORE
                        addrControl = 0;
                        state_n = D_WAIT; 
                        rwi = 'b01; 
                        // freeze_o = 1; 
                        load_ct_n = load_ct;
                        regData_o_n = regData_o; 
                        addrMMIO_o = '0;

                        if(memSource) begin
                            freeze_o = 1;
                            //addrMMIO_o = ALU_address; 
                            if (memOp == 4'd6) begin // store byte - FPU
                                dataMMIO_o = {24'b0, FPU_data_i[7:0]}; 
                                addrMMIO_o = ALU_address; 
                            end else if (memOp == 4'd7) begin // store half-word - FPU
                                dataMMIO_o = {16'b0, FPU_data_i[15:0]}; 
                                addrMMIO_o = ALU_address; 
                            end else if (memOp == 4'd8) begin // store word - FPU
                                dataMMIO_o = FPU_data_i; 
                                addrMMIO_o = ALU_address; 
                            end else begin
                                dataMMIO_o = 32'b0; // Default case, no valid operation
                                addrMMIO_o = ALU_address; 
                            end
                        end else begin
                            //addrMMIO_o = ALU_address; // Use ALU address for memory operations
                            freeze_o = 1;
                            if (memOp == 4'd6) begin // store byte
                                dataMMIO_o = {24'b0, regData_i[7:0]}; 
                                addrMMIO_o = ALU_address;
                            end else if (memOp == 4'd7) begin // store half-word
                                dataMMIO_o = {16'b0, regData_i[15:0]}; 
                                addrMMIO_o = ALU_address;
                            end else if (memOp == 4'd8) begin // store word
                                dataMMIO_o = regData_i; 
                                addrMMIO_o = ALU_address;
                            end else begin
                                dataMMIO_o = 32'b0; // Default case, no valid operation
                                addrMMIO_o = ALU_address;
                            end 
                        end 
                    end else if (memRead == 1) begin //LOAD
                        addrControl = 0;
                        state_n = D_WAIT; 
                        load_ct_n = load_ct + 'd1; 
                        rwi = 'b10; 
                        freeze_o = 1; 

                        addrMMIO_o = ALU_address; 
                        dataMMIO_o = 32'b0; // No data to write in read operation
                        
                        if (memOp == 4'd1) begin //load byte
                            regData_o_n = {{24{dataMMIO_i[7]}}, dataMMIO_i[7:0]}; 
                        end else if (memOp == 4'd2) begin // load half word
                            regData_o_n = {{16{dataMMIO_i[15]}}, dataMMIO_i[15:0]}; 
                        end else if (memOp == 4'd3) begin // load word
                            regData_o_n = dataMMIO_i; 
                        end else if (memOp == 4'd4) begin // load byte unsigned
                            regData_o_n = {24'b0, dataMMIO_i[7:0]}; 
                        end else if (memOp == 4'd5) begin // load half word unisgned
                            regData_o_n = {16'b0, dataMMIO_i[15:0]};
                        end else begin
                            regData_o_n = 32'b0; 
                        end

                    end else begin
                        state_n = FETCH;
                        dataMMIO_o = 32'b0; 
                        addrMMIO_o = ALU_address; 
                        regData_o_n = regData_o;

                        freeze_o = '1;
                        addrControl = 0; 
                        rwi = 'b00; 
                        load_ct_n = load_ct;
                    end
                end
            D_WAIT: //state 3
                begin 
                    addrControl = 0;
                    freeze_o = 1;
                    load_ct_n = load_ct;
                    
                    if(busy_o_edge & load_ct == '0) begin //check that load_ct is correct and not load_count_n
                        load_ct_n = load_ct;
                        state_n = DELAY; 
                        rwi = 'b01;
                    end else if (busy_o_edge & load_ct == 'd1) begin 
                        state_n = LOAD_DELAY; 
                        rwi = 'b10;
                    end else if (busy_o_edge & load_ct == 'd2) begin
                        state_n = DELAY;
                        rwi = 'b10;
                    end else if (busy_o_edge) begin
                        state_n = DELAY;
                        rwi = 'b10; //check
                    end 

                    regData_o_n = dataMMIO_i;
                    addrMMIO_o = ALU_address; 
                    dataMMIO_o = regData_i;
                end
            DELAY: begin
                rwi = 'b00;
                state_n = FETCH;
            end
            F_DELAY: begin
                rwi = 'b00;
                state_n = DATA;
            end
            LOAD_DELAY: begin
                rwi = 'b00;
                state_n = DATA;
            end
            default: begin    
                freeze_o = 1;
                addrControl = 0; //check this
                rwi = 'b11; //check this - default to fetch correct?
                regData_o_n= 'hDEADBEEF;
                addrMMIO_o = 'hDEADBEEF; 
                dataMMIO_o = 'hDEADBEEF;
                load_ct_n = '0;
                state_n = FETCH; 
            end

        endcase
    end

endmodule
