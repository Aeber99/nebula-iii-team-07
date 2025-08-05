module t07_control_unit (
input logic [6:0] Op, //from decoder
input logic [2:0] funct3, //from decoder
input logic [6:0] funct7, //from decoder
input logic [4:0] rs2, //from decoder, for differentiating FPU convert functions
output logic [3:0] ALUOp, //to ALU
output logic ALUSrc, regWrite, branch, jump, memWrite, memRead, FPUSrc, regEnable, regEnable_FPU,
output logic [2:0] regWriteSrc, //regWriteSrc goes to mux outside PC/memory handler/ALU/FPU/ImmGen -> registers, 000 = PC, 001 = MH, 010 = ALU, 011 = FPU, 100 = ImmGen
output logic [4:0] FPUOp, //to FPU
output logic [2:0] FPURnd, //to FPU
output logic [1:0] FPUWrite, //to FPUReg
output logic [4:0] rs3, //to FPU registers
output logic [3:0] memOp, //to internal memory
output logic invalid_Op, memSrc

//outputs:
//to register: regWrite (register read/write), regEnable
//pc: branch (condJump), jump (forceJump)
//memory: memRead, memWrite, memToReg, memOp (tells memory what instruction type)
//ALU: ALUOp, ALUSrc
//FPU: FPUOp, FPUSrc, FPURnd
//FPU Reg: FPUWrite, rs3 (for 3 input operations)
);

always_comb begin
    case(Op) //what should default case be?
        //ALU Cases
        7'b0110011: begin /*(R-type)*/
            memOp = '0;
            memSrc = 0;
            invalid_Op = 0;
            regWriteSrc = 3'b010; //from ALU
            ALUSrc = 0;
            regWrite = 1;
            branch = 0;
            memWrite = 0;
            memRead = 0;
            //memToReg = 0;
            jump = 0;
            regEnable = 1;
            //FPU outputs - all 0 when ALU op code
            FPUOp = '0;
            FPUSrc = 0;
            FPURnd = '0;
            FPUWrite = '0;
            regEnable_FPU = 0;
            case(funct3) 
                3'b000: if(funct7 == 'b0000000) begin ALUOp = 4'd0; end //add
                else if (funct7 == 'b0100000) begin ALUOp = 4'd8; end //sub
                3'b001: ALUOp = 4'd3; //sll
                3'b010: ALUOp = 4'd4; //slt
                3'b011: ALUOp = 4'd5; //sltu
                3'b100: ALUOp = 4'd9; //xor
                3'b101: if(funct7 == 0000000) begin ALUOp = 4'd7; end //srl
                else if (funct7 == 'b0100000) begin ALUOp = 4'd6; end //sra
                3'b110: ALUOp = 4'd2; //or
                3'b111: ALUOp = 4'd1; //and
            endcase
        end

        7'b 0000011: begin /*(I-Type)*/
            ALUOp = 4'd00; //add rs1 + imm
            memSrc = 0;
            invalid_Op = 0;
            regWriteSrc = 3'b001; //from internal mem
            ALUSrc = 1;
            regWrite = 1;
            branch = 0;
            memWrite = 0;
            memRead = 1;
            //memToReg = 1;
            jump = 0;
            regEnable = 1;
            //FPU outputs - all 0 when ALU op code
            FPUOp = '0;
            FPUSrc = 0;
            FPURnd = '0;
            FPUWrite = '0;
            regEnable_FPU = 0;
            //tells memory what to send to the registers:
            if(funct3 == 'b000) begin memOp = 4'd1; end //lb - load byte 
            if(funct3 == 'b001) begin memOp = 4'd2; end //lh - load half word
            if(funct3 == 'b010) begin memOp = 4'd3; end //lw - load word
            if(funct3 == 'b100) begin memOp = 4'd4; end //lbu - load byte unsigned
            if(funct3 == 'b101) begin memOp = 4'd5; end //lhu - load half word unsigned
        end

        7'b0010011: begin /*I-Type*/
            memOp = '0;
            memSrc = 0;
            invalid_Op = 0;
            regWriteSrc = 'b010;
            ALUSrc = 1;
            regWrite = 1;
            branch = 0;
            memWrite = 0;
            memRead = 0;
            //memToReg = 1;
            jump = 0;
            regEnable = 1;
            //FPU outputs - all 0 when ALU op code
            FPUOp = '0;
            FPUSrc = 0;
            FPURnd = '0;
            FPUWrite = '0;
            regEnable_FPU = 0;
            if(funct3 == 3'b000) begin ALUOp = '0; end //addi
            if(funct3 == 3'b010) begin ALUOp = 4'd4; end //slti
            if(funct3 == 3'b011) begin ALUOp = 4'd5; end //sltiu
            if(funct3 == 3'b100) begin ALUOp = 4'd9; end //xori
            if(funct3 == 3'b110) begin ALUOp = 4'd2; end //ori
            if(funct3 == 3'b111) begin ALUOp = 4'd1; end //andi
            if(funct3 == 3'b001) begin ALUOp = 4'd3; end //slli
            if(funct3 == 3'b101 & funct7 == '0) begin ALUOp = 4'd7; end //srli
            if(funct3 == 3'b101 & funct7 == 7'b0100000) begin ALUOp = 4'd6; end //srai
        end

        7'b0100011: begin /*(S-Type)*/
            memOp = '0;
            ALUOp = 4'd0; //ALU add = add rs1 & imm
            memSrc = 0;
            invalid_Op = 0;
            ALUSrc = 1; //immediate value
            regWrite = 0; //check
            branch = 0;
            memWrite = 1;
            memRead = 0;
            regWriteSrc = 'b001;
            //memToReg = 0; //X
            jump = 0;
            regEnable = 1;
            //FPU outputs - all 0 when ALU op code
            FPUOp = '0;
            FPUSrc = 0;
            FPURnd = '0;
            FPUWrite = '0;
            regEnable_FPU = 0;
            if(funct3 == 'b000) begin memOp = 4'd6; end //sb - store byte
            if(funct3 == 'b001) begin memOp = 4'd7; end //sh - store halfword
            if(funct3 == 'b010) begin memOp = 4'd8; end //sw - store word
        end
        7'b1100011: begin /*(B-Type)*/
            memOp = '0;
            ALUOp = 4'd10; //ALU default
            memSrc = 0;
            invalid_Op = 0;
            ALUSrc = 0;
            regWrite = 0;
            branch = 1;
            memWrite = 0;
            memRead = 0;
            //memToReg = 0; //X
            jump = 0;
            regEnable = 1;
            //FPU outputs - all 0 when ALU op code
            FPUOp = '0;
            FPUSrc = 0;
            FPURnd = '0;
            FPUWrite = '0;
            regEnable_FPU = 0;
        end
        7'b0110111: begin /*(U-type, lui)*/ 
            memOp = '0;
            ALUOp = 4'd10; //ALU default
            memSrc = 0;
            invalid_Op = 0;
            regWriteSrc = 3'b100; //from immGen
            ALUSrc = 0;
            regWrite = 1;
            FPUSrc = 0;
            FPURnd = '0;
            branch = 0;
            memWrite = 0;
            memRead = 0;
            //memToReg = 0;
            jump = 0;
            regEnable = 1;
            //FPU outputs - all 0 when ALU op code
            FPUOp = '0;
            FPUSrc = 0;
            FPURnd = '0;
            FPUWrite = '0;
            regEnable_FPU = 0;
        end
        7'b0010111: begin /*(U-Type, auipc)*/
            memOp = '0;
            ALUOp = 4'd00; //ALU addr + imm
            memSrc = 0;
            invalid_Op = 0;
            regWriteSrc = 3'b010; //from ALU
            ALUSrc = 1;
            regWrite = 1;
            branch = 0;
            memWrite = 0;
            memRead = 0;
            //memToReg = 0;
            jump = 0;
            regEnable = 1;
            //FPU outputs - all 0 when ALU op code
            FPUOp = '0;
            FPUSrc = 0;
            FPURnd = '0;
            FPUWrite = '0;
            regEnable_FPU = 0;
        end
        7'b1101111: begin /*(J-type, jal)*/
            memOp = '0;
            ALUOp = 4'd10; //ALU default
            memSrc = 0;
            invalid_Op = 0;
            regWriteSrc = 3'b000; //from PC
            ALUSrc = 0;
            regWrite = 1;
            branch = 0;
            memWrite = 0;
            memRead = 0;
            //memToReg = 0;
            jump = 1;
            regEnable = 1;
            //FPU outputs - all 0 when ALU op code
            FPUOp = '0;
            FPUSrc = 0;
            FPURnd = '0;
            FPUWrite = '0;
            regEnable_FPU = 0;
        end
        7'b1100111: begin /*(J-type, jalr)*/
            memOp = '0;
            ALUOp = 4'd0; //ALU add rs1 + imm
            memSrc = 0;
            invalid_Op = 0;
            regWriteSrc = 3'b000; //from PC
            ALUSrc = 1;
            regWrite = 1;
            branch = 0;
            memWrite = 0;
            memRead = 0;
            //memToReg = 0;
            jump = 1;
            regEnable = 1;
            //FPU outputs - all 0 when ALU op code
            FPUOp = '0;
            FPUSrc = 0;
            FPURnd = '0;
            FPUWrite = '0;
            regEnable_FPU = 0;
        end

        //FPU Cases
        7'b0000111: begin//(FLW - load word)
            memOp = '0;
            memWrite = 0;
            memRead = 0;
            ALUSrc = 0;
            ALUOp = 4'd10; //ALU default
            memSrc = 1;
            invalid_Op = 0;
            FPUOp = 5'b1;
            FPUSrc = 0;
            FPURnd = 'b010;
            regEnable = 0;
            regEnable_FPU = 1;
            FPUWrite = 1;
        end 
        7'b0100111: begin //(FSW - store word)
            memOp = '0;
            memWrite = 0;
            memRead = 0;
            ALUSrc = 0;
            ALUOp = 4'd10; //ALU default
            memSrc = 1;
            invalid_Op = 0;
            FPUOp = 5'd2;
            FPUSrc = 0;
            FPURnd = 'b010;
            regEnable = 0;
            regEnable_FPU = 1;
            FPUWrite = 0; //reading value from register -> goes to internal mem
        end
        7'b1000011: begin //(FMADD.S) rs1 x rs2 + rs3
            memOp = '0;
            memWrite = 0;
            memRead = 0;
            ALUSrc = 0;
            ALUOp = 4'd10; //ALU default
            memSrc = 1;
            invalid_Op = 0;
            FPUOp = 5'd3;
            FPUSrc = 1;
            regEnable = 0;
            FPUWrite = 1;
            regEnable_FPU = 1;
            rs3 = funct7[6:2];

        end
        7'b1000111: begin //(FMSUB.S) rs1 x rs2 - rs3
            memOp = '0;
            memWrite = 0;
            memRead = 0;
            ALUSrc = 0;
            ALUOp = 4'd10; //ALU default
            memSrc = 1;
            invalid_Op = 0;
            FPUOp = 5'd4;
            FPUSrc = 1;
            regEnable = 0;
            FPUWrite = 1;
            regEnable_FPU = 1;
            rs3 = funct7[6:2];
        end
        7'b1001011: begin //(FNMSUB.S) -(rs1 x rs2 - rs3)
            memOp = '0;
            memWrite = 0;
            memRead = 0;
            ALUSrc = 0;
            ALUOp = 4'd10; //ALU default
            memSrc = 1;
            invalid_Op = 0;
            FPUOp = 5'd5;
            FPUSrc = 1;
            regEnable = 0;
            FPUWrite = 1;
            regEnable_FPU = 1;
            rs3 = funct7[6:2];
        end 
        7'b1001111: begin //(FNMADD.S) -(rs1 x rs2 + rs3)
            memOp = '0;
            memWrite = 0;
            memRead = 0;
            ALUSrc = 0;
            ALUOp = 4'd10; //ALU default
            memSrc = 1;
            invalid_Op = 0;
            FPUOp = 5'd6;
            FPUSrc = 1;
            regEnable = 0;
            FPUWrite = 1;
            regEnable_FPU = 1;
            rs3 = funct7[6:2];
        end
        7'b1010011: begin //Math
            memOp = '0;
            memWrite = 0;
            memRead = 0;
            ALUSrc = 0;
            ALUOp = 4'd10; //ALU default
            memSrc = 1;
            invalid_Op = 0;
            regEnable = 0;
            FPUWrite = 1;
            FPUSrc = 1;
            regEnable_FPU = 1;
            case(funct7)
                'b0000000: begin FPUOp = 5'd7; end //ADD
                'b0000100: begin FPUOp = 5'd8; end //SUB
                'b0001000: begin FPUOp = 5'd9; end //MUL
                'b0001100: begin FPUOp = 5'd10; end //DIV
                'b0101100: begin FPUOp = 5'd11; end //SQRT
                'b0010000: begin 
                    if(funct3 == 'b000) begin FPURnd = 'b000; FPUOp = 5'd12; end //FSGNJ
                    if(funct3 == 'b001) begin FPURnd = 'b001; FPUOp = 5'd13; end //FSGNJN
                    if(funct3 == 'b010) begin FPURnd = 'b010; FPUOp = 5'd14; end //FSGNJX
                end 
                7'b0010100: begin
                    if(funct3 == 3'b000) begin FPURnd = 3'b000; FPUOp = 5'd15; end //FMIN
                    if(funct3 == 3'b001) begin FPURnd = 3'b001; FPUOp = 5'd16; end //FMAX
                end
                7'b1010000: begin
                    if(funct3 == 3'b010) begin FPURnd = 3'b010; FPUOp = 5'd17; end //FEQ
                    if(funct3 == 3'b001) begin FPURnd = 3'b001; FPUOp = 5'd18; end //FLT
                    if(funct3 == 3'b000) begin FPURnd = 3'b000; FPUOp = 5'd19; end //FLE
                end
                7'b1111000: begin FPURnd = 3'b000; FPUOp = 5'd20; end //FMV
                7'b1010011: begin 
                    if(funct7 == 7'b1100000) begin
                        if(rs2 == '0) begin FPUOp = 5'd21; end //FCVT.W
                        if(rs2 == 5'b00001) begin FPUOp = 5'd22; end //FCVT.WU
                    end
                    else if(funct7 == 'b1101000) begin
                        if(rs2 == '0) begin FPUOp = 5'd23; end //FCVT.S.W
                        if(rs2 == 5'b00001) begin FPUOp = 5'd24; end //FCVT.S.WU 
                    end
                end
                7'b1110000: begin FPURnd = 3'b001; FPUOp = 5'd25; end //FCLASS.S
                7'b1111000: begin FPURnd = 3'b000; FPUOp = 5'd26; end //FMV.S
                
            endcase
        end 
        default: invalid_Op = 1;
    endcase
end
endmodule