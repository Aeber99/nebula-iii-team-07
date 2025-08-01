module t07_MMIO(

    input logic clk, nrst,
    //CPU
    input logic [31:0] addr_in, //addr for instruction fetch
    input logic FPUbusy_i,
    output logic [31:0] instr_out, //instr sent to fetch
    output logic CPU_busy_o, //to CPU internal memory handler

    //internal memory handler
    input logic [31:0] memData_i, //data from internal memory
    input logic [1:0] rwi_in, //read write or idle 
    output logic [31:0] CPUData_out, //ExtData to internal memory

    //wishbone manager
    input logic [31:0] WBData_i, // data from instruction/Data memory (for load word)
    input logic WB_busy_i, 
    input logic WB_busy_edge_i,
    output logic WB_read_o, WB_write_o, //rw for wishbone manager
    output logic [31:0] addr_out, //address send to instr or data mem
    output logic [31:0] WBData_out, //data sent to WB during store instr

    //SPI for TFT
    input logic busyTFT_i, 
    output logic [31:0] displayData, // data to write to SPT TFT
    output logic [31:0] displayAddr, // address to write to SPI TFT]
    output logic displayWrite, // write or idle to SPI FTF

    //SPI for ESP32
    input logic [31:0] SPIData_i,
    input logic SPIack_i, //high when SPI has full word
    output logic espSPI_en
);

//registers - for holding SPI-ESP32 data
logic [31:0] SPIregisters;

//sequential logic for registers
always_ff @(negedge nrst, posedge clk) begin
    if (~nrst) begin
        SPIregisters <= '0;
    end else if (SPIack_i) begin //check 
        SPIregisters <= SPIData_i;
    end
end

always_comb begin
    //error cases
    CPUData_out = 'hDEADBEEF;
    instr_out = 'hDEADBEEF;
    CPU_busy_o = '0;
    CPUData_out = 'hDEADBEEF;
    WB_read_o = 0;
    WB_write_o = 0;
    addr_out = 'hDEADBEEF;
    WBData_out = 'hDEADBEEF; 
    displayData = 'hDEADBEEF;
    displayAddr = 'hDEADBEEF;
    displayWrite = '0;
    espSPI_en = '0;

    //busy signal logic -- based on WB busy, SPI busy, Reg busy
    if (busyTFT_i || WB_busy_i || FPUbusy_i) begin 
        CPU_busy_o = '1; 
        end else begin 
            CPU_busy_o = 0; 
                    end

    //read & write for wishbone manager
    if(WB_busy_edge_i) begin
        WB_read_o = 0;
        WB_write_o = 0;
    end else if(rwi_in == 'b11 & addr_in <= 32'd1024) begin //fetch
        WB_read_o = 1;
        WB_write_o = 0;
    end else if(rwi_in == 'b10 & addr_in > 32'd1056 & addr_in <= 32'd1792) begin //read, // address
        WB_read_o = 1;
        WB_write_o = 0;
    end else if(rwi_in == 'b01 & addr_in > 32'd1056 & addr_in <= 32'd1792) begin //write
        WB_read_o = 0;
        WB_write_o = 1;
    end else begin 
        WB_read_o = 0;
        WB_write_o = 0;
    end

    if(addr_in <= 32'd1024) begin //fetch instruction
        addr_out = {8'h33, addr_in[23:0]}; //address for instruction (from PC)
        CPUData_out = 32'b0; //sending instruction to fetch, not internal mem
        instr_out = WBData_i; //next instruction to write to fetch module in CPU
    end else if (addr_in > 32'd1056 & addr_in <= 32'd1792) begin //access data memory
        if(rwi_in == 2'b10) begin //load
            addr_out = {8'h33, addr_in[23:0]}; //addr - read from data mem
            CPUData_out = WBData_i; 
        end
        if(rwi_in == 2'b01) begin //store
            addr_out = {8'h33, addr_in[23:0]}; //addr - write to data mem
            WBData_out = memData_i;
        end
    end else if (addr_in > 32'd1792 & addr_in < 32'd2048) begin //SPI-TFT command
        displayWrite = 1'b1; //write data to SPI TFT
        displayAddr = addr_in; 
        displayData = memData_i; 
    end else if(addr_in > 32'd1024 & addr_in <= 32'd1056 & rwi_in == 2'b10) begin //read from spi
        espSPI_en = '1;
        CPUData_out = SPIregisters;
    end
    end

endmodule
