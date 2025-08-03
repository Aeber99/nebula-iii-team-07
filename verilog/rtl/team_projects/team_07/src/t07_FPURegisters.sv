module t07_FPURegisters(
    input logic clk, nrst,
    input logic [4:0] rs1, rs2, rd,
    input logic [31:0] data_i,
    input logic regEnable_i, FPUregWrite_i, 
    input logic freeze_i,
    output logic [31:0] FPUreg1_o, FPUreg2_o
);

    logic [31:0] [31:0] registers;

    always_ff @(negedge nrst, posedge clk) begin
        if(~nrst) begin
            registers <= '0;
        end else if (regEnable_i & freeze_i == '0) begin
            if(FPUregWrite_i && rd != '0) begin
                registers[rd] <= data_i;
            end
        end
    end

    always_comb begin
        FPUreg1_o = registers[rs1];
        FPUreg2_o = registers[rs2];
    end

endmodule