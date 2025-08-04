`timescale 1ms / 1ps
module t07_top_tb();
    logic clk, nrst;
    logic [3:0] ESP_in;
    logic FPUFlag, invalError, chipSelectTFT, bitDataTFT, sclkTFT;

    t07_top top0(.clk(clk), .nrst(nrst), .FPUFlag(FPUFlag), .invalError(invalError), .ESP_in(ESP_in), .chipSelectTFT(chipSelectTFT), .bitDataTFT(bitDataTFT), .sclkTFT(sclkTFT));

    task reset(); begin
        #1
        nrst = ~nrst;        
        #2
        nrst = ~nrst;
    end
    endtask

    task TaskESPWord1(); begin
        ESP_in = 4'hA;
        #4;
        ESP_in = 4'hA;
        #4;
        ESP_in = 4'hB;
        #4;
        ESP_in = 4'hB;
        #4;
        ESP_in = 4'hC;
        #4;
        ESP_in = 4'hC;
        #4;
        ESP_in = 4'hD;
        #4;
        ESP_in = 4'hD;
        #4;
    end
    endtask

    task TaskESPWord(); begin
        ESP_in = 4'hA;
        #4;
        ESP_in = 4'hA;
        #4;
        ESP_in = 4'hB;
        #4;
        ESP_in = 4'hB;
        #4;
        ESP_in = 4'hC;
        #4;
        ESP_in = 4'hC;
        #4;
        ESP_in = 4'hD;
        #4;
        ESP_in = 4'hD;
        #4;
    end
    endtask

    task FullReg();
        TaskESPWord1();
        for(int i = 0; i < 30; i++) begin
            TaskESPWord();
        end
        ESP_in = '0;
    endtask
    
    always begin
        #2
        clk = ~clk;
    end
    
    // task ESPData(); begin
    //     ESP_in = 

    // end
    
    initial begin
        $dumpfile("t07_top.vcd");
        $dumpvars(0, t07_top_tb);
        clk = 0;
        nrst = 1;
        ESP_in = '0;
        reset();
        $display("//////////////////////////////////////////////////////");
        FullReg();
        #500
        $finish;
    end

endmodule