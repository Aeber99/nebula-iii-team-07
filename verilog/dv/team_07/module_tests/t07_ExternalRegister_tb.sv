`timescale 1ms / 1ns

module t07_ExternalRegister_tb;

    // Inputs
    logic clk;
    logic nrst;
    logic [4:0] ReadRegister;
    logic [31:0] write_data;
    logic ri;
    logic [4:0] SPIAddress; // Address for the SPI TFT
    logic busy;
    // Outputs
    logic [31:0] read_data;
    logic ack_REG; // Acknowledge signal to the memory handler
    logic [1:0] rwi_in; // Read/Write/Idle signal from the memory handler
    logic [31:0] ExtData_out; // Data to internal memory
    logic [31:0] addr_in; // Address for the external register
    logic [31:0] inst; // Instruction to fetch module in CPU
    logic [31:0] writeData_outTFT; // Data to write to instruction/Data memory
    logic [31:0] addr_outTFT; // Address to write to SPI TFT
    logic wi_out; // Write or idle to SPI TFT
    logic read, write; // Read/Write/Idle to instruction/Data memory
    logic [31:0] addr_out; // Address to instruction/Data memory
    logic [31:0] writeData_out; // Data to write to instruction/Data memory
    logic fetchRead_out, addrControl_out, fetchRead_in, addrControl_in;
    logic ChipSelectIn, ChipSelectOut;
    t07_MMIO mmio (
        .memData_i(32'b0), // Not used in this test
        .rwi_in(rwi_in), // Not used in this test
        .WBData_i(32'b0), // Not used in this test
        .WB_busy_i(1'b0), // Not used in this test
        .WB_busy_edge_i(1'b0),
        .regData_i(read_data),
        .ack_REG_i(ack_REG),
        .ChipSelReg_i(ChipSelectIn),
        .ack_TFT_i(1'b0), // Not used in this test
        .addr_in(addr_in), //address for the external register
        .regRead_o(ri),
        .addr_outREG(ReadRegister), // Address to external register
        .CPUData_out(ExtData_out),
        .CPU_busy_o(busy), 
        .instr_out(inst), // Not used in this test
        .displayData(writeData_outTFT), // Not used in this test
        .displayAddr(addr_outTFT), // Not used in this test
        .displayWrite(wi_out), // Not used in this test
        .WB_read_o(),
        .WB_write_o(),
        .addr_out(addr_out), // Not used in this test
        .WBData_out(writeData_out) // Not used in this test
    );
    // Instantiate the Unit Under Test (UUT)
    t07_ExternalRegister uut (
        .clk(clk),
        .nrst(nrst),
        .ReadRegister(ReadRegister),
        .SPIAddress(SPIAddress),
        .write_data(write_data),
        .ri(ri),
        .ChipSelect(ChipSelectIn),
        .read_data(read_data),
        .ack_REG(ack_REG)
    );

    logic [3:0] ESP_in; // Input from the ESP32
    logic SCLK_out; // Clock signal for the ESP32
   
    t07_SPI_ESP32 spi (
        .ESP_in(ESP_in), 
        .clk(clk),
        .nrst(nrst),
        .SPI_Address(SPIAddress),
        .dataForExtReg(write_data),   
        .ChipSelectIn(ChipSelectIn),
        .ChipSelectOut(ChipSelectOut),
        .SCLK_out(SCLK_out) // Not used in this test
    );

    // Clock generation
    always begin 
        clk = 0;
        #5; // Wait for 10 time units
        clk = 1;
        #5; // Wait for 10 time units
    end

    task ESP_in_task; begin
       // @(posedge clk)
        ESP_in = 4'hA; // Default value
        #12;
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
        ESP_in = 4'h1; // Reset value
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
        ESP_in = 4'h1; // Reset value
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
        ESP_in = 4'h1; // Reset value
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
        ESP_in = 4'h1; // Reset value // Wait for a clock cycle
        #10; // Wait for a clock cycle
        ESP_in = 4'h1; // Reset value
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
        ESP_in = 4'h1; // Reset value
        #10; // Wait for a clock cycle
        ESP_in = 4'h1; // Reset value
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
        ESP_in = 4'h1; // Reset value
        #10; // Wait for a clock cycle
        ESP_in = 4'h1; // Reset value
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
        ESP_in = 4'h1; // Reset value
        #10; // Wait for a clock cycle
        ESP_in = 4'h1; // Reset value
        #10; // ait for a clock cycle
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
        #10; // Waitfor a clock cycle
        ESP_in = 4'h1; // Reset value
        #10; // Wit for a clock cycle
        ESP_in = 4'h1; // Reset value
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
        ESP_in = 4'h1; // Reset value
        #10; // Waitfor a clock cycle
        ESP_in = 4'h1; // Reset value
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
        ESP_in = 4'h1; // Reset value
        #10; // Wait for a clock cycle
        ESP_in = 4'h1; // Reset value
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
        ESP_in = 4'h1; // Reset value
        #10; // Wait for a clock cycle // Wait for a clock cycle
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
        ESP_in = 4'h1; // Reset value
        #10; // Wait for a clock cycle
         // Wait for a clock cycle
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
        ESP_in = 4'h1; // Reset value
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
        ESP_in = 4'h1; // Reset value
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
        ESP_in = 4'h1; // Reset value
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
        ESP_in = 4'h1; // Reset value
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
        ESP_in = 4'h0;
        addr_in =32'd1024;
        #10;
        #10;
        #10
        rwi_in = 2'b10;
        addr_in = 32'd1025;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1026;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1027;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1028;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1029;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1030;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1031;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1032;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1033;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1034;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1035;
            #10;
            #10;
            rwi_in = 2'b01;
        #10;

        rwi_in = 2'b10;
            addr_in = 32'd1036;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1037;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1038;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1039;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1040;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1041;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1042;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1043;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1044;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1045;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1046;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1047;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1048;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        
        rwi_in = 2'b10;
            addr_in = 32'd1049;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        
        rwi_in = 2'b10;
            addr_in = 32'd1050;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1051;
            #10;
            #10;
            rwi_in = 2'b01;
        #10;
        rwi_in = 2'b10;
            addr_in = 32'd1052;
            #10;
            #10;
            rwi_in = 2'b01;
        #10;
        rwi_in = 2'b10;
            addr_in = 32'd1053;
            #10;
            #10;
            rwi_in = 2'b01;
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1054;
            #10;
            #10;
            rwi_in = 2'b01;
        #10;


        rwi_in = 2'b10;
            addr_in = 32'd1055;
                $display("============================");
                $display("time: %d\n", $time);
                $display("============================");
            //
            // BIG LOOP!
            //
            #10;
            //
            // END BIG LOOP!
            //
            #10;
            rwi_in = 2'b01;
            ESP_in = 4'hF; // Final example data
        #10
        rwi_in = 2'b10;
            addr_in = 32'd1056;        
    

        ESP_in = 4'h1; // Reset value // Wait for a clock cycle
        #10; // Wait for a clock cycle
        ESP_in = 4'h1; // Reset value
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
    end
    endtask

    initial begin
        $dumpfile("t07_ExternalRegister.vcd");
        $dumpvars(0, t07_ExternalRegister_tb);
        // Initialize Inputs
        rwi_in = 2'b00; // Idle state for read/write/idle
        addr_in = 32'd1024;
        nrst = 1;
        // ReadRegister = 5'h00000000;
        //write_data = 32'h00000000;
        //SPIAddress = 5'd0; // Initial address for SPI TFT
        // Wait for global reset to finish
        #2;
        nrst = 0; // Release reset
        #1; // Wait for a few clock cycles
        nrst = 1; // Set reset low again 
        // Test writing to a register
        // spiAddress(); // Set SPI address
        
        // Check if the data was written correctly
        ESP_in_task(); // Read from the registers
        #5;

        // readRegister(); // Read from the registers
        #10;
    
        #1; // Wait for a clock cycle
       $finish; // End simulation
    end
endmodule
    