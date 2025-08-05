module t07_muxFPU(
    input logic [31:0] regValA_i, regValB_i, regValC_i,
    input logic [31:0] fpuRegValA_i, fpuRegValB_i, fpuRegValC_i,
    input [4:0] FPUOp,
    output [31:0] FPUValA_o, FPUValB_o, FPUValC_o
);
    always_comb begin
        if(FPUOp == 5'd21 || FPUOp == 5'd22) begin //int to float
            FPUValA_o = regValA_i;
            FPUValB_o = regValB_i;
            FPUValC_o = '0; //val C not needed
        end else if (FPUOp == 5'd0 || FPUOp == 5'd1 || FPUOp == 5'd2 || FPUOp == 5'd3) begin
            FPUValA_o = fpuRegValA_i;
            FPUValB_o = fpuRegValB_i;
            FPUValC_o = fpuRegValC_i;
        end else begin
            FPUValA_o = fpuRegValA_i;
            FPUValB_o = fpuRegValB_i;
            FPUValC_o = '0; //val C not needed
        end
    end
endmodule