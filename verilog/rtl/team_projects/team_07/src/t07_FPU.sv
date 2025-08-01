//ALL OPERATIONS ARE PERFORMED IN FIXED POINT, find API that inputs data in fixed point rather than floating

module t07_FPU (
    input logic clk, nrst,
    //input signed [31:0] inA, inB, inC,
    input signed [31:0] valA, valB, valC,
    input logic [31:0]fcsr_in,
    input logic [4:0] FPUOp,
    output logic [31:0] result,
    output logic [6:0] FPUflags,
    output logic overflowFlag,
    output logic carryout, busy
);

    //inputs converted from float to fixed point
    //logic [31:0] valA, valB, valC;
    logic signA, signB, signC;
    logic divDone;

    //all two input mult and div combinations within the FPU are instantiated here
    logic [31:0] prodAB, quotAB, remAB, intFloatRes, floatIntRes;
    logic signABmult, signABdiv;
    logic signSignal, invalid;

    //t07_FPU_mult mult(.clk(clk), .nrst(nrst), .inA(valA), .inB(valB), .busy(busy), .signA(valA[31]), .signB(valB[31]), .result(prodAB), .sign(signABmult), .overflow(overflowFlag));
    t07_FPU_div div(.clk(clk), .nrst(nrst), .inA(valA), .inB(valB), .signA(valA[31]), .signB(valB[31]), .op(FPUOp), .quotient(quotAB), .remainder(remAB), .sign(signABdiv), .busy(busy), .done(divDone));
    t07_FPU_inttofloat intFloat (.in(valA), .signSignal(signSignal), .out(intFloatRes), .overflow(overflowFlag));
    t07_FPU_floattoint floatInt (.in(valA), .signSignal(signSignal), .out(floatIntRes), .frm(fcsr_in[7:5]), .invalidFlag(invalid));

    //other necessary signals from fcsr
    logic [2:0] frm; //page 41, last paragraph- check for static/dynamic rounding m
    assign frm = fcsr_in [7:5];
    
//     logic [4:0] fflags;
//     assign fflags = fcsr_in[4:0];

    //choose operation- Page 51 RVALP
    always_comb begin
        busy = 0;
        result = 32'b0;
        //signSignal = 1'b0;
        case (FPUOp)

            5'd0: begin signSignal = 1'b0; result = {signABmult, prodAB[31:1]} + valC; end //FMADD
            5'd1: begin signSignal = 1'b0; result = {signABmult, prodAB[31:1]} - valC; end //FMSUB
            5'd2: begin signSignal = 1'b0; result = $signed({~signABmult, prodAB[31:1]}) - valC; end //FNMSUB
            5'd3: begin signSignal = 1'b0; result = $signed({~signABmult, prodAB[31:1]}) - valC; end //FNMADD 
            5'd4: begin //FADD
                signSignal = 1'b0; 
                if (valA[31] == 1 && valB[31] == 0) begin
                    result = ~(((~valA + 1) + valB) - 1);
                end else if (valB[31] == 1 && valA[31] == 0) begin
                    result = ~(((~valB + 1) + valA) - 1);
                end else if (valA[31] == 1 && valB[31] == 1) begin
                    result = ~(((~valA + 1) + (~valB + 1)) - 1);
                end else if (valA[31] == 0 && valB[31] == 0) begin
                    result = valA + valB;
                end
            end 
            5'd5: begin //FSUB
                signSignal = 1'b0; 
                if (valA[31] == 1 && valB[31] == 0) begin
                    result = {1'b1, valA[30:0] + valB[30:0]};
                end else if (valB[31] == 1 && valA[31] == 0) begin
                    result = valA + {~valB[31], valB[30:0]};
                end else if (valA[31] == 1 && valB[31] == 1) begin
                    result = ~(((~valA + 1) + valB) - 1);
                end else if (valA[31] == 0 && valB[31] == 0) begin
                    result = ~(((~valB + 1) + valA) - 1);
                end
            end 
            5'd6: begin signSignal = 1'b0; result = {signABmult, prodAB[31:1]}; end //FMUL
            5'd7: begin signSignal = 1'b0; busy = 1'b1; result = {signABdiv, quotAB[31:1]}; 
                if (divDone == 1) begin
                    busy = 0;
                end
            end //FDIV 
            5'd8: begin signSignal = 1'b0; result = 32'b0; end //FSQRT- not implemented, sets result to 0 for now, but change it so that no instruction is passed/idle state
            5'd9: begin signSignal = 1'b0; result = {valB[31], valA[30:0]}; end //FSGNJ
            5'd10: begin signSignal = 1'b0; result = {~valB[31], valA[30:0]}; end //FSGNJN
            5'd11: begin signSignal = 1'b0; result = {(valA[31] ^ valB[31]), valA[30:0]}; end //FSGNJX
            5'd12: begin signSignal = 1'b0; if(valA > valB) begin result = valB; end else if (valA <= valB) begin result = valA; end else begin result = 32'b0; end end //FMIN
            5'd13: begin signSignal = 1'b0; if(valA >= valB) begin result = valA; end else if (valA < valB) begin result = valB; end else begin result = 32'b0; end end //FMAX
            5'd14: begin //FCVT.W.S -> use rounding mode input
                signSignal = 1;
                result = floatIntRes; 
            end 
            5'd15: begin //FCVT.WU.S
                signSignal = 0;
                result = floatIntRes;
            end 
            5'd16: begin signSignal = 1'b0; result = 32'b0; end //FMV.X.S
            5'd17: begin signSignal = 1'b0; if (valA == valB) begin result = 32'b1; end else begin result = 32'b0; end end //FEQ
            5'd18: begin signSignal = 1'b0; if (valA < valB) begin result = 32'b1; end else begin result = 32'b0; end end //FLT
            5'd19: begin signSignal = 1'b0; if (valA <= valB) begin result = 32'b1; end else begin result = 32'b0; end end //FLE
            5'd20: begin signSignal = 1'b0; result = 32'b0; end //FCLASS- same as FSQRT comment
            5'd21: begin //FCVT.S.W
                signSignal = 1;
                result = intFloatRes;
            end
            5'd22: begin //FCVT.S.WU
                signSignal = 0;
                result = intFloatRes;
            end 
            5'd23: begin signSignal = 1'b0; result = 32'b0; end //FMV.S.X
            default: begin signSignal = 1'b0; result = 32'b0; end
        endcase
    end

//     //flag logic- Page 58 RVALP

//     always_comb begin
//         FPUflags = 7'd0;
//         if (result == 32'b0) begin FPUflags[0] = 1; end //zeroFlag
//         if (valA >= valB) begin FPUflags[1] = 1; end //greater than or equal 
//         if ($unsigned(valA) >= $unsigned(valB)) begin FPUflags[2] = 1; end //greater than or equal unsigned
//         if (valA < valB) begin FPUflags[3] = 1; end //less than
//         if ($unsigned(valA) < $unsigned(valB)) begin FPUflags[4] = 1; end //less than unsigned
//         if (valA != valB) begin FPUflags[5] = 1; end //not equal
//         if (valA == valB) begin FPUflags[6] = 1; end //equal
//     end
endmodule