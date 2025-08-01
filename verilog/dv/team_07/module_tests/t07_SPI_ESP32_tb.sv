`timescale 1ns/1ps
module t07_SPI_ESP32_tb;    
    // Testbench signals
    logic [3:0] ESP_in;
    logic clk, nrst;
    logic [5:0] SPI_Address; // Address for the external register
    logic [31:0] dataForExtReg; // Data to write to the external register
    logic SCLK_out; // Clock signal for the ESP32

    // Instantiate the t07_SPI_ESP32 module
    t07_SPI_ESP32 uut (
        .ESP_in(ESP_in),
        .clk(clk),
        .nrst(nrst),
        .SPI_Address(SPI_Address),
        .dataForExtReg(dataForExtReg),
        .SCLK_out(SCLK_out)
    );

    // Clock generation
    always begin 
        clk = 0;
        #5; // Wait for 10 time units
        clk = 1;
        #5; // Wait for 10 time units
    end

    task ESP_in_task; begin
        ESP_in = 4'hA; // Default value
        #10;
        ESP_in = 4'hB; // Example data
        #10;
        ESP_in = 4'hC; // Another example data
        #10;
        ESP_in = 4'hD; // Reset value
        #10;
        ESP_in = 4'hE;
        #10;
        ESP_in = 4'hF; // Final example data
        #10; // Wait for a clock cycle
        ESP_in = 4'hA; // Default value
        #10;
        ESP_in = 4'hB; // Example data
        #10;
        ESP_in = 4'hC; // Another example data
        #10;
        ESP_in = 4'hD; // Reset value
        #10;
        ESP_in = 4'hE;
        #10;
        ESP_in = 4'hF; // Final example data
        #10; // Wait for a clock cycle
        ESP_in = 4'h0; // Reset value
        #10; // Wait for a clock cycle
    end
    endtask

    initial begin
        $dumpfile("t07_SPI_ESP32.vcd");
        $dumpvars(0, t07_SPI_ESP32_tb);

        // Initialize inputs
        nrst = 1'b0; // Reset the system
        ESP_in = 4'h0; // Initial value for ESP input
        #10; // Wait for a few time units
        nrst = 1'b1; // Release reset
        #5;
        ESP_in_task; // Call the task to generate ESP input data
        ESP_in_task; // Call the task again to generate more ESP input data
        #10;
        $finish; // End simulation
    end
endmodule
