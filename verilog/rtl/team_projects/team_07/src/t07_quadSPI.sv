module t07_quadSPI (
    //SPI master, ESP32 slave
    input logic [3:0] ESPData_i,
    input logic sclk_i, nrst, enable_i, //from MMIO
    output logic [31:0] MMIOData_o,
    output logic sclk_o, enable_o, ack_o //sclk, enable to ESP32, ack to MMIO
);

logic [31:0] MMIOData_n;
logic enable_n;
logic [3:0] ct, ct_n;

assign enable_o = enable_i;

always_ff @(negedge nrst, posedge sclk_i) begin
    if (~nrst) begin
        MMIOData_o <= '0;
        ct <= '0;
    end else begin
        MMIOData_o <= MMIOData_n;
        ct <= ct_n;
    end
end

always_comb begin
    MMIOData_n = '0;
    ct_n = '0;
    ack_o = '0;

    if(ct < 'd9) begin
        MMIOData_n = {MMIOData_o[31:4], ESPData_i};
        ct_n = ct + 'd1;
        if(ct == 8) begin
            ack_o = '1;
    end else begin 
        ack_o = '0; end
    end else begin
        ct_n = '0;
    end
end

endmodule