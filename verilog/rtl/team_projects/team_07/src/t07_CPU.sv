module t07_CPU(
    input logic [31:0]exInst, memData_in, //intruction from data memory, data from MMIO
    output logic [31:0] exMemData_out, externalMemAddr, //PCdata_out to MMIO or instr
    input logic clk, nrst, busy,
    output logic [1:0] rwi,
    output logic busy_edge_o, FPUbusy_o,
    output logic FPU_overflowFlag, FPUcarryout, invalError, //to GPIO
    output logic [6:0] FPUFlags
);
    logic [31:0] inst;
    logic freeze; //to external memory 
    //decoder out
    logic [6:0] Op, funct7;
    logic [2:0] funct3;
    logic [4:0] rs1, rs2, rs3, rd;
    //control out
    logic [3:0] ALUOp, memOp;
    logic ALUSrc, regWrite, branch, jump, memWrite, memRead, FPUSrc, regEnable, memSource;
    logic [4:0] FPUOp;
    logic [1:0] FPUWrite;
    logic [2:0] regWriteSrc, FPURnd;
    //PC output
    logic [31:0] pc_out, linkAddress;
    //ALU output
    logic [6:0] ALUFlags;
    logic [31:0] ALUResult;
    //immediate output
    logic [31:0] immediate; //jumpDist in PC
    //register output
    logic [31:0] dataRead1, dataRead2;
    //mux 
    logic [31:0] memRegSource; //reg and FPU register
    logic [31:0] ALU_in2;
    logic [31:0] regData_in;
    logic [31:0] PCJumpDist;
    //FPU outputs
    logic [31:0] fcsr_out; //FPU register out
    logic [31:0] FPUResult;
    //memory output
    logic [31:0] intMem_out; 
    logic [31:0] intMemAddr;
    logic busy_o_edge;
    logic [2:0] state;
    logic addrControl;
    logic [31:0] pcData_out;
    logic freezeToReg;
    
    t07_fetch fetch_inst(.busy_o_edge(busy_edge_o), .clk(clk), .nrst(nrst), .ExtInstruction(exInst), .programCounter(pc_out), .Instruction_out(inst), .PC_out(pcData_out), .busy_o(busy));
    t07_decoder decoder(.instruction(inst), .Op(Op), .funct7(funct7), .funct3(funct3), .rs1(rs1), .rs2(rs2), .rd(rd));

    t07_control_unit control(.memSrc(memSource), .invalid_Op(invalError), .rs3(rs3), .memOp(memOp), .rs2(rs2), .regWriteSrc(regWriteSrc), .Op(Op), 
    .funct7(funct7), .funct3(funct3), .ALUOp(ALUOp), .ALUSrc(ALUSrc), .regWrite(regWrite), .branch(branch), .jump(jump), 
    .memWrite(memWrite), .memRead(memRead), .FPUSrc(FPUSrc), .regEnable(regEnable), .FPUOp(FPUOp), .FPURnd(FPURnd), .FPUWrite(FPUWrite));

    t07_program_counter pc(.clk(clk), .func3(funct3), .nrst(nrst), .forceJump(jump), .condJump(branch), .ALU_flags(ALUFlags), .JumpDist(PCJumpDist), 
    .programCounter(pc_out), .linkAddress(linkAddress), .freeze(freeze));

    t07_immGen immediate0(.func3(funct3), .instruction(inst), .immediate(immediate));

    t07_registers register(.freeze_i(freezeToReg), .clk(clk), .nrst(nrst), .read_reg1(rs1), .read_reg2(rs2), .write_reg(rd), .write_data(regData_in), 
    .reg_write(regWrite), .enable(regEnable), .read_data1(dataRead1), .read_data2(dataRead2));

    t07_memoryHandler internalMem(.freeze_o(freezeToReg), .state(state), .clk(clk), .nrst(nrst), .busy(busy), .memOp(memOp), .memWrite(memWrite), .memRead(memRead), .memSource(memSource), .ALU_address(ALUResult), 
    .FPU_data_i('0), .regData_i(dataRead2), .dataMMIO_i(memData_in), .dataMMIO_o(exMemData_out), .addrMMIO_o(intMemAddr), .regData_o(intMem_out), 
    .freeze(freeze), .rwi(rwi), .addrControl(addrControl), .busy_o_edge(busy_edge_o));

    t07_ALU ALU(.valA(dataRead1), .valB(ALU_in2), .result(ALUResult), .ALUflags(ALUFlags), .ALUOp(ALUOp));
    t07_muxes muxFPUReg(.a(fcsr_out), .b(dataRead2), .sel(FPUSrc), .out(memRegSource)); //check when FPU is added
    t07_muxes muxImmReg(.a(dataRead2), .b(immediate), .sel(ALUSrc), .out(ALU_in2));
    t07_muxForPC muxPC(.immediate(immediate), .ALUResult(ALUResult), .Op(Op), .PCJump(PCJumpDist));

    t07_MuxWD toReg(.control_in(regWriteSrc), .ALUResult(ALUResult), .PCResult(pc_out), .FPUResult(FPUResult),
    .memResult(intMem_out), .immResult(immediate), .writeData(regData_in));
    
    t07_muxes addrMux(.a(intMemAddr), .b(pcData_out), .sel(addrControl), .out(externalMemAddr));

    t07_FPU FPUmanager(.clk(clk), .nrst(nrst), .valA(), .valB(), .valC(), .fcsr_in(), .FPUOp(FPUOp), .result(FPUResult), 
    .FPUflags(FPUFlags), .overflowFlag(FPU_overflowFlag), .carryout(FPUcarryout), .busy(FPUbusy_o));

endmodule