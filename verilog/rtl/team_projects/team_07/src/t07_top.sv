module t07_top (
    input logic clk, nrst,
    input logic [3:0] ESP_in,
    output logic FPUFlag, invalError, chipSelectTFT, bitDataTFT, sclkTFT, FPU_overflowFlag, FPUcarryout,
    output logic [6:0] FPUFlags
);

logic [1:0] rwiToWB;
logic read, write, idle;

//inputs/outputs from CPU
logic busyCPU; //sent from MMIO to CPU
logic [31:0] instr, memData_in, memData_out, exMemData_CPU, exMemAddr_CPU;
logic [1:0] rwi_in; //read = 10, write = 01, idle = 00
logic fetchReadToMMIO, addrControl, busy_edge, FPUbusy_o;

//outputs of MMIO
logic [31:0] addrToSRAM, dataToSRAM; //addr_out in MMIO 
logic fetchReadToWB, addrControlWB; //makes sure fetch doesnt run twice

//wishbone manager output to wishbone arbitrator
logic [31:0] addrWMToAr, dataWMToAr;
logic [3:0] selToAr;
logic weToAr, stbToAr, cycToAr;

//wishbone arbitrator output to wishbone decoder
logic cycToDec, stbToDec, weToDec;
logic [31:0] addrToDec, dataToDec; 
logic [3:0] selToDec;

//wishbone arbitrator output to wishbone manger
logic [31:0] ackToWM, dataArToWM;

//wishbone decoder output to wishbone arbitrator
logic ackToAr;
logic [31:0] dataDecToAr;

//wishbone manager output to user design
logic [31:0] dataToMMIO;
logic busyToMMIO;

//outputs of WB decoder
logic [6 + 3:0] cyc_out;
logic [6 + 3:0] stb_out;
logic [6 + 3:0] we_out;
logic [(32 * (10)) - 1:0] addr_out; 
logic [(32 * (10)) - 1:0] data_out;
logic [(4 * (10)) - 1:0] sel_out;

//input SRAM to decoder
logic [6 + 3:0] ackDec_in; //acknowledge
logic [(32 * (10)) - 1:0] dataDec_in; //data from SRAM to WB Dec

//inputs to MMIO from SPI -ESP32
logic [31:0] SPIData_i;
logic espSPI_en;

//outputs to SPI->TFT
logic [31:0] dataToTFT, addrToTFT;
logic displayWrite, busyTFT_o;


t07_CPU CPU(.busy(busyCPU), .externalMemAddr(exMemAddr_CPU), .exMemData_out(exMemData_CPU), .exInst(instr), .memData_in(memData_in), 
.rwi(rwi_in), .invalError(invalError), .clk(clk), .nrst(nrst), .busy_edge_o(busy_edge), .FPU_overflowFlag(FPU_overflowFlag), 
.FPUcarryout(FPUcarryout), .FPUFlags(FPUFlags), .FPUbusy_o(FPUbusy_o));

t07_MMIO MMIO(.clk(clk), .nrst(nrst), .SPIack_i(), .addr_in(exMemAddr_CPU), .memData_i(exMemData_CPU), .rwi_in(rwi_in), .WBData_i(dataToMMIO), 
 .busyTFT_i(busyTFT_o), .CPUData_out(memData_in), .CPU_busy_o(busyCPU), .instr_out(instr), .displayData(dataToTFT), .displayWrite(displayWrite), 
 .displayAddr(addrToTFT), .WB_read_o(read), .WB_write_o(write), .addr_out(addrToSRAM), .WBData_out(dataToSRAM), .WB_busy_i(busyToMMIO),
 .WB_busy_edge_i(busy_edge), .SPIData_i(SPIData_i), .FPUbusy_i(FPUbusy_o), .espSPI_en(espSPI_en));

wishbone_manager wishbone0(.nRST(nrst), .CLK(clk), .DAT_I(dataArToWM), .ACK_I(ackToWM), .CPU_DAT_I(dataToSRAM), 
.ADR_I(addrToSRAM), .SEL_I(4'hF), .WRITE_I(write), .READ_I(read), .ADR_O(addrWMToAr), .DAT_O(dataWMToAr), 
.SEL_O(selToAr), .WE_O(weToAr), .STB_O(stbToAr), .CYC_O(cycToAr), .CPU_DAT_O(dataToMMIO), .BUSY_O(busyToMMIO));

//SRAM
wishbone_arbitrator wishboneA0(.CLK(clk), .nRST(nrst), .A_ADR_I(addrWMToAr), .A_DAT_I(dataWMToAr), .A_SEL_I(selToAr), 
.A_WE_I(weToAr), .A_STB_I(stbToAr), .A_CYC_I(cycToAr), .A_DAT_O(dataArToWM), .A_ACK_O(ackToWM), .DAT_I(dataDecToAr), 
.ACK_I(ackToAr), .ADR_O(addrToDec), .DAT_O(dataToDec), .SEL_O(selToDec), .WE_O(weToDec), .STB_O(stbToDec), .CYC_O(cycToDec));

wishbone_decoder wishboneD0 (.CLK(clk), .nRST(nrst), .wbs_ack_i_periph(ackDec_in), .wbs_dat_i_periph(dataDec_in), .wbs_ack_o_m(ackToAr), 
.wbs_dat_o_m(dataDecToAr), .wbs_cyc_i_m(cycToDec), .wbs_stb_i_m(stbToDec), .wbs_we_i_m(weToDec), .wbs_adr_i_m(addrToDec), 
.wbs_dat_i_m(dataToDec), .wbs_sel_i_m(selToDec), .wbs_cyc_o_periph(cyc_out), .wbs_stb_o_periph(stb_out), .wbs_we_o_periph(we_out), 
.wbs_adr_o_periph(addr_out), .wbs_dat_o_periph(data_out), .wbs_sel_o_periph(sel_out));

sram_WB_Wrapper sramWrapper(.wb_clk_i(clk), .wb_rst_i(nrst), .wbs_stb_i(stb_out), .wbs_cyc_i(cyc_out), .wbs_we_i(we_out), .wbs_sel_i(sel_out),
.wbs_dat_i(data_out), .wbs_adr_i(addr_out), .wbs_ack_o(ackDec_in), .wbs_dat_o(dataDec_in));

t07_spitft display(.data(dataToTFT), .address(addrToTFT), .clk(clk), .nrst(nrst), .wi(displayWrite), .busy_o(busyTFT_o), .chipSelect(chipSelectTFT), .bitData(bitDataTFT), .sclk(sclkTFT));

//t07_quadSPI espSPI(.ESPData_i(ESP_in), .sclk_i(clk), .nrst(nrst), .enable_i(), .MMIOData_o(), .sclk_o(), .enable_o(espSPI_en), .ack_o());

endmodule