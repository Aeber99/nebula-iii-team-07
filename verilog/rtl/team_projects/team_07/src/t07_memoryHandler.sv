
 typedef enum logic [3:0] {  
        INC = 0,
        FETCH = 1,
        F_WAIT = 2,
        DATA = 3,
        LOAD = 4,
        LOAD_WAIT = 5,
        STORE = 6,
        PRELOAD = 7,
        INC_WAIT = 8
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
    input logic [31:0] instr_i, pc_i, //inputs for fetch cycle
    
    //outputs
    output logic [31:0] dataMMIO_o, // Data to write to external memory
    output logic [31:0] addrMMIO_o, // Address to write to external memory   
    output logic [31:0] addrMMIO_comb_o,
    output logic [31:0] regData_o,  // Data to the register
    output logic freeze_o,            // Freeze signal to pause CPU operations during memory access
    output logic [1:0] rwi,          // read - 01, write - 10, idle - 00, fetch -11 
    output state_t0 state,
    output logic addrControl, // control for address mux, 0 when fetch, 1 when l/s
    output logic busy_o_edge,

    //fetch cycle outputs
    output logic [31:0] instructionOut, pcOut

);

    logic prev_busy_o;
    logic [31:0] regData_o_n, instr_n, pc_n, addrMMIO_o_n;
    logic [2:0] loadCt, loadCt_n; //preload state
    logic [1:0] load2Ct, load2Ct_n; //load state
    logic [2:0] load3Ct, load3Ct_n; //load wait count
    logic [2:0] storeCt, storeCt_n; //store count
    state_t0 state_n;


    //instruction fetching
    // always_ff @(negedge nrst, posedge clk) begin
    //     if (~nrst) begin
    //         instructionOut <= 32'b0; // Reset instruction to zero on reset
    //         pcOut <= 32'b0; // Reset program counter output to zero
    //     end if (busy_o_edge == 1 & instructionOut != 'hDEADBEEF) begin
    //         instructionOut <= instr_i; // Fetch instruction from external memory when not frozen
    //         pcOut <= pc_i; // Update program counter output
    //     end
    // end

    //edge detector
    always_ff @(negedge nrst, posedge clk) begin
        if(~nrst) begin
            prev_busy_o <= '0;
        end else begin
            prev_busy_o <= busy;
        end
    end

    assign busy_o_edge = (~busy && prev_busy_o); //detects falling edge

    always_ff @(negedge nrst, posedge clk) begin
        if (~nrst) begin 
            state <= FETCH;
            regData_o <= '0;
            addrMMIO_o <= '0;
            instructionOut <= 32'b0; 
            // pcOut <= 32'b0; 
            loadCt <= '0;
            load2Ct <= '0;
            load3Ct <= '0;
            storeCt <= '0;
        end else begin
            state <= state_n;
            regData_o <= regData_o_n;
            addrMMIO_o <= addrMMIO_o_n;
            instructionOut <= instr_n; 
            // pcOut <= pc_n; 
            loadCt <= loadCt_n;
            load2Ct <= load2Ct_n;
            load3Ct <= load3Ct_n; 
            storeCt <= storeCt_n;
        end
    end

    assign pcOut = pc_i;
    //state machine
    always_comb begin
        case(state)
            INC: //state 0 - 1 cycle
                begin
                    freeze_o = 0; //only state where pc can increment
                    

                    pc_n = pcOut;
                    instr_n = instructionOut;

                    addrControl = 1;
                    rwi = 'b00;

                    loadCt_n = '0;
                    load2Ct_n = '0;
                    load3Ct_n = '0;
                    storeCt_n = '0;

                    regData_o_n = regData_o;
                    addrMMIO_o_n = addrMMIO_o;
                    dataMMIO_o = 'hDEADBEEF;
                    addrMMIO_comb_o = '0;

                    state_n = INC_WAIT;
                end
            FETCH: //state 1 - busy edge 
                begin
                    freeze_o = 1;

                    addrControl = 1;
                    rwi = 'b11;

                    loadCt_n = '0;
                    load2Ct_n = '0;
                    load3Ct_n = '0;
                    storeCt_n = '0;

                    instr_n = instructionOut;
                    pc_n = pc_i;

                    regData_o_n = regData_o;
                    addrMMIO_o_n = addrMMIO_o;
                    dataMMIO_o = 'hDEADBEEF;
                    addrMMIO_comb_o = '0;

                    if(busy_o_edge) begin
                        state_n = F_WAIT;
                    end else begin
                        state_n = FETCH;
                    end
                end
            F_WAIT: //state 2 - 1 cycle
                begin
                    freeze_o = 1;

                    addrControl = 1;
                    rwi = 'b00;

                    loadCt_n = '0;
                    load2Ct_n = '0;
                    load3Ct_n = '0;
                    storeCt_n = '0;

                    pc_n = pcOut;

                    if(instr_i != 'hDEADBEEF) begin
                        instr_n = instr_i;
                    end

                    regData_o_n = regData_o;
                    addrMMIO_o_n = addrMMIO_o;
                    dataMMIO_o = 'hDEADBEEF;
                    addrMMIO_comb_o = '0;

                    state_n = DATA;
                end
            DATA: //state 3 - 1 cycle (anything other than load or store completed in this state)
                begin 
                    addrControl = 1;
                    freeze_o = 1;
                    rwi = '0;

                    loadCt_n = '0;
                    load2Ct_n = '0;
                    load3Ct_n = '0;
                    storeCt_n = '0;

                    pc_n = pcOut;
                    instr_n = instructionOut;
                    
                    regData_o_n = regData_o; //ALU result does into reg via muxWD
                    addrMMIO_o_n = addrMMIO_o;
                    dataMMIO_o = 'hDEADBEEF;
                    addrMMIO_comb_o = '0;

                    if(memWrite) begin 
                        state_n = STORE;
                    end else if (memRead) begin
                        state_n = PRELOAD;
                    end else begin
                        state_n = INC;
                    end
                end
            LOAD: //state 4 - busy edge
                begin
                    freeze_o = 1;
                    addrControl = 0;
                    rwi = 'b10;

                    loadCt_n = '0;
                    load2Ct_n = load2Ct + 'b1;
                    load3Ct_n = '0;
                    storeCt_n = '0;

                    addrMMIO_o_n = ALU_address; 
                    addrMMIO_comb_o = ALU_address;
                    dataMMIO_o = 32'b0; 

                    pc_n = pcOut;
                    instr_n = instructionOut;
                        
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
                    
                    if(load2Ct == 1) begin
                        state_n = LOAD_WAIT;
                    end else begin
                        state_n = LOAD;
                    end

                    // if(busy_o_edge) begin
                    //     state_n = LOAD_WAIT;
                    // end else begin
                    //     state_n = LOAD;
                    // end
                end
            PRELOAD:
                begin
                    loadCt_n = loadCt + 'd1;
                    load2Ct_n = '0;
                    load3Ct_n = '0;
                    storeCt_n = '0;

                    freeze_o = 1;
                    addrControl = 0;
                    rwi = 'b10;

                    addrMMIO_o_n = ALU_address; 
                    addrMMIO_comb_o = ALU_address;
                    dataMMIO_o = 32'b0; 
                    regData_o_n = regData_o;

                    pc_n = pcOut;
                    instr_n = instructionOut;

                    if(loadCt == 2) begin
                        state_n = LOAD;
                    end else begin
                        state_n = PRELOAD;
                    end
                end
            LOAD_WAIT: //state 5
                begin
                    freeze_o = 1;
                    addrControl = 0;
                    rwi = 'b10;

                    loadCt_n = '0;
                    load2Ct_n = '0;
                    load3Ct_n = load3Ct + 'b1;
                    storeCt_n = '0;

                    pc_n = pcOut;
                    instr_n = instructionOut;

                    addrMMIO_o_n = ALU_address; 
                    addrMMIO_comb_o = ALU_address;
                    dataMMIO_o = 32'b0; 
                    regData_o_n = regData_o;

                    if(busy_o_edge) begin
                        state_n = INC;
                    end else begin
                        state_n = LOAD_WAIT;
                    end

                    //state_n = INC;
                    // if(busy_o_edge) begin
                    //     state_n = INC;
                    // end else begin
                    //     state_n = LOAD_WAIT;
                    // end 
                end
            STORE: //state 6
                begin
                    freeze_o = 1;
                    addrControl = 0;
                    rwi = 'b01;

                    loadCt_n = '0;
                    load2Ct_n = '0;
                    load3Ct_n = '0;
                    //storeCt_n = storeCt + 1;


                    addrMMIO_o_n = ALU_address; 
                    addrMMIO_comb_o = ALU_address;
                    regData_o_n = regData_o; 

                    pc_n = pcOut;
                    instr_n = instructionOut;

                    if(memSource) begin //memSrc determines if data is from FPU reg or normal reg
                    //FPU registers
                        if (memOp == 4'd6) begin // store byte
                            dataMMIO_o = {24'b0, FPU_data_i[7:0]}; 
                        end else if (memOp == 4'd7) begin // store half-word 
                            dataMMIO_o = {16'b0, FPU_data_i[15:0]}; 
                        end else if (memOp == 4'd8) begin // store word 
                            dataMMIO_o = FPU_data_i; 
                        end else begin
                            dataMMIO_o = 32'b0; 
                        end
                    //registers
                    end else begin
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

                    if(busy_o_edge) begin
                        storeCt_n = storeCt + 1;
                    end else begin storeCt_n = storeCt; end

                    if(storeCt > '0) begin
                        if(busy_o_edge) begin
                            state_n = INC;
                        end else begin state_n = STORE; end
                    end else begin
                        state_n = STORE;
                    end
                end
            INC_WAIT: begin
                    freeze_o = 1;

                    pc_n = pcOut;
                    instr_n = instructionOut;

                    addrControl = 1;
                    rwi = 'b00;

                    loadCt_n = '0;
                    load2Ct_n = '0;
                    load3Ct_n = '0;
                    storeCt_n = '0;

                    regData_o_n = regData_o;
                    addrMMIO_o_n = addrMMIO_o;
                    dataMMIO_o = 'hDEADBEEF;
                    addrMMIO_comb_o = '0;

                    state_n = FETCH;
            end
        endcase
    end
endmodule