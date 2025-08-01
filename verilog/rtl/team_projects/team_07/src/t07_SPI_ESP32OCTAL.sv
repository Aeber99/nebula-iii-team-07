`timescale 1ns/1ps

module t07_SPI_ESP32OCTAL (
    input logic [7:0] ESP_in, 
    input logic clk, nrst,
    input logic ChipSelectIn,
    output logic [4:0] SPI_Address, // Address for the external register],
    output logic [31:0] dataForExtReg, // Data to write to the external register
    output logic ChipSelectOut,
    output logic SCLK_out // Clock signal for the ESP32

);
    logic [4:0] n_address;
    logic [31:0] MOSI_shiftReg;
    logic [31:0] n_MOSI_shiftReg; // Next value for the MOSI shift register
    logic [31:0] f_MOSI_shiftReg; // Final value for the MOSI shift register after 4 bits


    logic [3:0] bit_count;
    logic [3:0] n_bit_count; // Next value for the bit count
    logic [4:0] address; // Current address for the external register

assign SCLK_out = clk; // Output the clock signal directly

always_ff @(negedge nrst, posedge clk) begin
    if (!nrst) begin
        MOSI_shiftReg <= '0;
        bit_count <= 0;
        dataForExtReg <= '0; // Reset data for the external register
        address <= 5'b0; // Reset address to zero
    end else begin  
        MOSI_shiftReg <= n_MOSI_shiftReg; // Update the MOSI shift register
        bit_count <= n_bit_count; // Update the bit count
        dataForExtReg <= f_MOSI_shiftReg; // Update the data for the external register
        address <= n_address; // Update the current address
    end
end

always_comb begin
    ChipSelectOut=ChipSelectIn;
    if (ChipSelectIn == 1) begin 
        n_bit_count = 0; 
        n_address = 0;
        SPI_Address = 0;
        n_MOSI_shiftReg = '0;
        f_MOSI_shiftReg = '0;
    end else if (bit_count == 0) begin
        n_MOSI_shiftReg = {MOSI_shiftReg[23:0], ESP_in}; // Shift in the incoming data
        n_bit_count = 1; // Start counting bits from 1
        n_address = address;
        //f_MOSI_shiftReg = '0; // No valid data to write to the external register yet
    end else if (bit_count < 4) begin // If bit count is less than 4
        n_MOSI_shiftReg = {MOSI_shiftReg[23:0], ESP_in}; // Shift in the incoming data
        n_bit_count = bit_count + 4'b1; // Increment the bit count
        n_address = address; // Keep the address unchanged
        //f_MOSI_shiftReg = '0; // No valid data to write to the external register yet
    end else begin //bit count is 4
        if (address < 5'd31) begin
        n_address = address + 5'b1; // Increment the address for the next byte  
        end else begin
            n_address = 5'd1; // Reset address to 1 if it exceeds 31        
        end       
       // f_MOSI_shiftReg = MOSI_shiftReg; // Keep the shift register unchanged
        n_MOSI_shiftReg = {MOSI_shiftReg[23:0], ESP_in}; // Shift in the incoming data
        n_bit_count = 4'b1; // Reset the bit count to 1 for the next byte
    end

    if (bit_count != 4 && bit_count != 1) begin
        SPI_Address = '0;
        f_MOSI_shiftReg = '0; // No valid data to write to the external register yet
    end else if (bit_count == 1) begin
        SPI_Address = address;
        f_MOSI_shiftReg = '0;
    end else begin
        SPI_Address = '0; // Set the SPI address to the current address
        f_MOSI_shiftReg = MOSI_shiftReg; // Set the final MOSI shift register value
    end
end
    
endmodule