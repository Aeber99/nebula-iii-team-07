`default_nettype none

// team 07 registers file
/* internal registers
purpose: implements register file for cpu
contains 32 registers each 32 bit wide

- read registers: read_reg1, read_reg2 <- these are the registers to read from
- write register: write_reg <- this is the register to write to
- write data: write_data <- this is the address to write to
- reg_write_enable: reg_write_enable <- this is the signal to enable writing to the register

*/

module t07_registers (

    input logic clk,
    input logic nrst,
    input logic [4:0] read_reg1, // from decoder
    input logic [4:0] read_reg2, // from decoder
    input logic [4:0] write_reg, // from decoder
    input logic [31:0] write_data, // from memory handler
    input logic reg_write, // from control unit
    input logic enable, // from control unit
    input logic freeze_i,

    output logic [31:0] read_data1, read_data2
);

    // Register file: 32 registers, each 32 bits wide
    logic [31:0] [31:0]registers ;
    
    // write logic
    always_ff @(negedge nrst, negedge clk) begin
        if (~nrst) begin
            // Reset all registers to zero
            registers <= '0;
        end else if (enable & freeze_i == '0) begin
            if (reg_write && write_reg != 5'b0) begin
                registers[write_reg] <= write_data;
            end 
        end
    end

        // read logic
    always_comb begin
        read_data1 = registers[read_reg1];
        read_data2 = registers[read_reg2];
    end 

endmodule

