module t07_MuxWD (
    input logic [2:0] control_in,
    input logic [31:0] ALUResult, PCResult, FPUResult, memResult, immResult,
    output logic [31:0] writeData
);
  always_comb begin
        if (control_in == 3'b000) begin
            writeData = PCResult;
        end else if (control_in == 3'b001) begin
            writeData = memResult;
        end else if (control_in == 3'b010) begin
            writeData = ALUResult;
        end else if (control_in == 3'b011) begin
            writeData = FPUResult;
        end else if (control_in == 3'b100) begin
            writeData = immResult;
        end else begin writeData = 'hDEADBEEF; end //error case
    end

endmodule