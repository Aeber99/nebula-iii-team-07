`timescale 1ms / 1ps
module t07_top_tb();
    logic clk, nrst;
    logic FPUFlag, invalError;

    t07_top top0(.clk(clk), .nrst(nrst), .FPUFlag(FPUFlag), .invalError(invalError), .ESP_in(ESP_in));

    task reset(); begin
        #2
        nrst = ~nrst;        
        #2
        nrst = ~nrst;
    end
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
        reset();

        #300
        $finish;
    end

endmodule