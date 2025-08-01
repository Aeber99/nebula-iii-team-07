
 typedef enum logic [2:0] {  
        FETCH = 0,
        F_WAIT = 1,
        DATA = 2,
        D_WAIT = 3
    } state_t0;


module t07_memoryHandler (

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
    logic load_ct, load_ct_n;
    logic prev_busy_o;
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
        end
    end

    always_comb begin
    //default to prevent latch
        freeze_o = '0;
        addrControl = 0; //check this
        rwi = 'b11; //check this - default to fetch correct?
        regData_o = 'hDEADBEEF;
        addrMMIO_o = 'hDEADBEEF; 
        dataMMIO_o = 'hDEADBEEF;
        
        load_ct_n = '0;
        state_n = FETCH; 

        case(state) 
            FETCH: //state 0
                begin
                    addrControl = 1;
                    load_ct_n  = '0; 
                    rwi = 'b11; 
                    freeze_o = 0; //fetch instr 
                    state_n = F_WAIT; 
                end
            F_WAIT: //state 1
                begin 
                    addrControl = 1;
                    load_ct_n = '0; 
                    rwi = 'b11; 
                    freeze_o = 1;
                    if(busy_o_edge == 'b1) begin 
                        state_n = DATA; 
                    end else begin
                        state_n = F_WAIT; 
                    end 
                end
            DATA: //state 2
                begin 
                    addrControl = 1;
                    if(memWrite == 1) begin //STORE
                        addrControl = 0;
                        state_n = D_WAIT; 
                        rwi = 'b01; 
                        freeze_o = 1; 
                        load_ct_n = 0;
                        regData_o = 32'b0; 

                        if(memSource) begin
                            addrMMIO_o = ALU_address; 
                            if (memOp == 4'd6) begin // store byte - FPU
                                dataMMIO_o = {24'b0, FPU_data_i[7:0]}; 
                            end else if (memOp == 4'd7) begin // store half-word - FPU
                                dataMMIO_o = {16'b0, FPU_data_i[15:0]}; 
                            end else if (memOp == 4'd8) begin // store word - FPU
                                dataMMIO_o = FPU_data_i; 
                            end else begin
                                dataMMIO_o = 32'b0; // Default case, no valid operation
                            end
                        end else begin
                            addrMMIO_o = ALU_address; // Use ALU address for memory operations
                            if (memOp == 4'd6) begin // store byte
                                dataMMIO_o = {24'b0, regData_i[7:0]}; 
                            end else if (memOp == 4'd7) begin // store half-word
                                dataMMIO_o = {16'b0, regData_i[15:0]}; 
                            end else if (memOp == 4'd8) begin // store word
                                dataMMIO_o = regData_i; 
                            end else begin
                                dataMMIO_o = 32'b0; // Default case, no valid operation
                            end 
                        end 

                    end else if (memRead == 1) begin //LOAD
                        addrControl = 0;//
                        state_n = D_WAIT; 
                        load_ct_n = load_ct + 1; 
                        rwi = 'b10; 
                        freeze_o = 1; 

                        addrMMIO_o = ALU_address; 
                        dataMMIO_o = 32'b0; // No data to write in read operation
                        
                        if (memOp == 4'd1) begin //load byte
                            regData_o = {{24{dataMMIO_i[7]}}, dataMMIO_i[7:0]}; 
                        end else if (memOp == 4'd2) begin // load half word
                            regData_o = {{16{dataMMIO_i[15]}}, dataMMIO_i[15:0]}; 
                        end else if (memOp == 4'd3) begin // load word
                            regData_o = dataMMIO_i; 
                        end else if (memOp == 4'd4) begin // load byte unsigned
                            regData_o = {24'b0, dataMMIO_i[7:0]}; 
                        end else if (memOp == 4'd5) begin // load half word unisgned
                            regData_o = {16'b0, dataMMIO_i[15:0]};
                        end else begin
                            regData_o = 32'b0; 
                        end

                    end else begin
                        state_n = FETCH;
                        dataMMIO_o = 32'b0; 
                        addrMMIO_o = ALU_address; 
                        regData_o = 32'b0;
                    end
                end
            D_WAIT: //state 3
                begin 
                    addrControl = 0;
                    freeze_o = 1;
                    if(busy_o_edge & load_ct == 0) begin //check that load_ct is correct and not load_count_n
                        //addrControl = 1;
                        state_n = FETCH; 
                    end else if (busy_o_edge & load_ct == 1) begin 
                        state_n = DATA; 
                    end else if (busy_o_edge) begin
                        //addrControl = 1;
                        state_n = FETCH;
                    end
                end
            default:
            freeze_o = 1;
        endcase
    end

endmodule
