 typedef enum logic [1:0] {  
        IDLE = 0,
        LOAD = 1,
        OUTPUT = 2
    } state_tft;

module t07_spitft (
    input logic [31:0] data,
    input logic [31:0] address,
    input logic clk,
    input logic nrst,
    input logic wi,

    output logic busy_o, // to memory handler
    
    // to ra8875
    output logic chipSelect,
    output logic bitData,
    output logic sclk                                                                                                  
);

logic [63:0] dataforOutput, next_data;
state_tft state, next_state;
logic printState, busy_o_n;
logic [6:0] counter, next_ctr;


always_ff @(negedge nrst, posedge clk) begin
    if(~nrst) begin
        dataforOutput <= '0;
        counter <= 7'b0;
        state <= IDLE;
        busy_o <= '0;
    end else begin
        dataforOutput <= next_data;
        state <= next_state;
        counter <= next_ctr;
        busy_o <= busy_o_n;
    end
end

always_comb begin
    bitData = 0;
    next_data = dataforOutput;
    next_state = state;
    busy_o_n = 0;
    chipSelect = 1;
    sclk = 0;
    next_ctr = counter;


    case(state)
        IDLE: begin 
            busy_o_n = '0;
            if(wi == '0) begin next_state = IDLE; end 
            else begin next_state = LOAD; end
            end
        LOAD: begin 
            if(wi == 1) begin 
                next_ctr = 7'b0; 
                chipSelect = 0; 
                busy_o_n = 1; 
                sclk = clk; 
                next_data = {address[31:24], data[31:24],  address[23:16], data[23:16], address[15:8], data[15:8], address[7:0], data[7:0]}; 
                next_state = OUTPUT; 
            end 
            else begin next_state = IDLE; end 
            end
        OUTPUT: begin 
            if (wi == 1 && counter < 7'd64) begin
                busy_o_n = '1;
                chipSelect = 0; 
                bitData = dataforOutput[63];
                next_data = dataforOutput << 1;
                next_state = OUTPUT;
                next_ctr = counter + 1;
            end 
            else if (wi == 1 && counter == 7'd64) begin
                busy_o_n = '0;
                chipSelect = 1;
                next_state = IDLE;
            end else if (wi == 0) begin
                busy_o_n = '0;
                chipSelect = 1;
                sclk = 0;
                next_state = IDLE;
            end 
        end
        default: begin sclk = '0; end
    endcase
end
endmodule
