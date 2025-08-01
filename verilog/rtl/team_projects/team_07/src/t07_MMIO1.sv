module t07_MMIO(
    input logic clk, nrst,

    //CPU
    input logic [31:0] addr_in, //addr for instruction fetch
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
    input logic ack_TFT_i, 
    output logic [31:0] displayData, // data to write to SPT TFT
    output logic [31:0] displayAddr, // address to write to SPI TFT]
    output logic displayWrite, // write or idle to SPI FTF

    //SPI for ESP32
    input logic [31:0] SPIData_i,
    input logic SPIack_i //high when full word is ready

    //external registers
    //input logic [31:0] regData_i, 
    //input logic ack_REG_i,
    //input logic ChipSelReg_i, // chip select from external register to indicate we can read
    //output logic regRead_o, //read or idle signal to external register
    //output logic [4:0] addr_outREG, // address to external register
);

//registers - for holding SPI-ESP32 data
logic [31:0] registers;

//sequential logic for registers
always_ff @(negedge nrst, posedge clk) begin
    if (~nrst) begin
        registers <= '0;
    end else if (SPIack_i) begin //check 
        registers <= SPIData_i;
    end
end

always_comb begin
    //Peripheral Address Control
    //error cases
    CPUData_out = 'hDEADBEEF;
    instr_out = 'hDEADBEEF;
    CPU_busy_o = '0;
    CPUData_out = 'hDEADBEEF;
    //addr_outREG = '0;
    //regRead_o = '0;
    WB_read_o = 0;
    WB_write_o = 0;
    addr_out = 'hDEADBEEF;
    WBData_out = 'hDEADBEEF; 
    displayData = 'hDEADBEEF;
    displayAddr = 'hDEADBEEF;
    displayWrite = '0;

    //busy signal logic -- based on WB busy, SPI busy, Reg busy
    if (ack_TFT_i || WB_busy_i) begin 
        CPU_busy_o = '1; 
        //end else if ((addr_in > 32'd1024 && addr_in <= 32'd1056) & rwi_in == 2'b10 & ChipSelReg_i) begin 
            //CPU_busy_o = ack_REG_i; 
        //end else if ((addr_in > 32'd1024 && addr_in <= 32'd1056) & ChipSelReg_i == '0 & rwi_in == 2'b10) begin 
                //CPU_busy_o = '1; 
        end else begin 
            CPU_busy_o = 0; 
        end

    //read & write for wishbone manager
    if(WB_busy_edge_i) begin
        WB_read_o = 0;
        WB_write_o = 0;
    end else if(rwi_in == 'b11) begin //fetch
        WB_read_o = 1;
        WB_write_o = 0;
    end else if(rwi_in == 'b10) begin //read, // address
        WB_read_o = 1;
        WB_write_o = 0;
        //idle = 0;
    end else if(rwi_in == 'b01) begin //write
        WB_read_o = 0;
        WB_write_o = 1;
        //idle = 0;
    end else begin 
        WB_read_o = 0;
        WB_write_o = 0;
        //idle = 1;
    end

    if(addr_in <= 32'd1024) begin //fetch instruction
        addr_out = {8'h33, addr_in[23:0]}; //address for instruction (from PC)
        CPUData_out = 32'b0; //sending instruction to fetch, not internal mem
        instr_out = WBData_i; //next instruction to write to fetch module in CPU
    end else begin
        /* if(addr_in > 32'd1024 & addr_in <= 32'd1056 & rwi_in == 2'b10) begin //read from external registers
            addr_outREG = addr_in[4:0]; // address to external register (to get correct ESP32 data)
            if(ChipSelReg_i == 1) begin
                regRead_o = 1'b1; //read from external register
                CPUData_out = regData_i; // data from external register to internal memory
            end
        end
        else */ 
        if (addr_in > 32'd1056 & addr_in <= 32'd1792) begin //access data memory
            if(rwi_in == 2'b10) begin //load
                addr_out = {8'h33, addr_in[23:0]}; //addr - read from data mem
                CPUData_out = WBData_i; 
            end
            if(rwi_in == 2'b01) begin //store
                addr_out = {8'h33, addr_in[23:0]}; //addr - write to data mem
                WBData_out = memData_i;
            end
        end
        else if (addr_in > 32'd1792 & addr_in < 32'd2048) begin //SPI-TFT command
            displayWrite = 1'b1; //write data to SPI TFT
            displayAddr = addr_in; 
            displayData = memData_i; 
        end
    end

    //SPI - ESP32 data, sent into CPU
    if (addr_in > 32'd1024 & addr_in <= 32'd1056) begin
        CPUData_out = registers; 
    end

end
endmodule
