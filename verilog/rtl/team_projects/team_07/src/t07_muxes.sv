module t07_muxes(
    input logic [31:0] a, b, // Inputs to the mux
    input logic sel,          // Select signal
    output logic [31:0] out   // Output of the mux
);
    always_comb begin
        if (sel) begin
            out = b; // If sel is high, output b
        end else begin
            out = a; // If sel is low, output a
        end
    end
endmodule