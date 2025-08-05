module t07_spitft (
    input logic [31:0] in,
    input logic clk,
    input logic nrst,
    input logic wi,
    input logic miso_in,

    output logic [7:0] miso_out, //to internal registers

    output logic ack, // to memory handler
    // to ra8875
    
    output logic chipSelect,
    output logic bitData,
    output logic sclk
);

    //logic delay;
    logic [15:0] next_miso, misoShift;
    logic [15:0] dataforOutput, next_data;
    logic [1:0] state, next_state;
    logic [4:0] counter, next_ctr;
    logic [15:0] delayctr, next_delayctr;


    always_ff @(negedge nrst, posedge clk) begin
        if(~nrst) begin
            dataforOutput <= '0;
            counter <= 5'b0;
            state <= 2'b0;
            misoShift <= '0;
            delayctr <= '0;
        end else begin
            dataforOutput <= next_data;
            state <= next_state;
            counter <= next_ctr;
            misoShift <= next_miso;
            delayctr <= next_delayctr;
        end
    end

    always_comb begin
        // bitData = 0;
        // next_data = dataforOutput;
        // next_miso = misoShift;
        // next_state = state;
        // ack = 0;
        // chipSelect = 1;
        // sclk = 0;
        // next_ctr = counter;
        // miso_out = '0;
        // next_delayctr = delayctr;

        case (state) 
            2'b00: begin
            if (wi == 1) begin 
                next_ctr = 5'b0; 
                chipSelect = 0; 
                ack = 1; 
                sclk = clk; 
                next_data = in[15:0];
                next_miso = '0;
                next_state = 2'b01;

                //default
                bitData = 0;
                //next_data = dataforOutput;
                //next_miso = misoShift;
                //next_state = state;
                //ack = 0;
                //chipSelect = 1;
                //sclk = 0;
                //next_ctr = counter;
                miso_out = '0;
                next_delayctr = delayctr;
            end else begin 
                next_state = 2'b00; 

                //default
                bitData = 0;
                next_data = dataforOutput;
                next_miso = misoShift;
                //next_state = state;
                ack = 0;
                chipSelect = 1;
                sclk = 0;
                next_ctr = counter;
                miso_out = '0;
                next_delayctr = delayctr;
            end 
            end //load 16 bits
            2'b01: begin
                if (wi == 1 && counter < 5'd16) begin
                    ack = 1;
                    chipSelect = 0; 
                    sclk = clk;
                    bitData = dataforOutput[15];
                    next_data = dataforOutput << 1;
                    if (in[15:8] == 8'h40) begin
                        next_miso = {misoShift[14:0], miso_in};
                        //next_ctr = counter + 1;

                        //default
                        bitData = 0;
                        next_data = dataforOutput;
                        //next_miso = misoShift;
                        next_state = state;
                        ack = 0;
                        chipSelect = 1;
                        sclk = 0;
                        next_ctr = counter;
                        miso_out = '0;
                        next_delayctr = delayctr;

                    end else begin
                        next_miso = misoShift;

                        //default
                        bitData = 0;
                        next_data = dataforOutput;
                        //next_miso = misoShift;
                        next_state = state;
                        ack = 0;
                        chipSelect = 1;
                        sclk = 0;
                        next_ctr = counter;
                        miso_out = '0;
                        next_delayctr = delayctr;
                    end

                    next_state = 2'b01;
                    next_ctr = counter + 1;
                end 
                else if ((wi == 1 && counter == 5'd16) || wi == 0) begin
                    if ((counter == 5'd16) && in[15:8] == 8'h40) begin
                        sclk = clk;
                        miso_out = misoShift[7:0];
                        //next_state = 2'b01;

                        //default
                        bitData = 0;
                        next_data = dataforOutput;
                        next_miso = misoShift;
                        next_state = state;
                        ack = 0;
                        chipSelect = 1;
                        //sclk = 0;
                        next_ctr = counter;
                        //miso_out = '0;
                        next_delayctr = delayctr;
                    end
                    next_state = 2'b10;

                    //defaults
                    bitData = 0;
                    next_data = dataforOutput;
                    next_miso = misoShift;
                    //next_state = state;
                    ack = 0;
                    chipSelect = 1;
                    sclk = 0;
                    next_ctr = counter;
                    miso_out = '0;
                    next_delayctr = delayctr;
                end //write - shift 16 bits
            end
            2'b10: begin
                if (delayctr < in[31:16]) begin
                    ack = 0;
                    chipSelect = 1;
                    sclk = 0;
                    next_state = 2'b10;

                    //default
                    bitData = 0;
                    next_data = dataforOutput;
                    next_miso = misoShift;
                    //next_state = state;
                    //ack = 0;
                    //chipSelect = 1;
                    //sclk = 0;
                    next_ctr = counter;
                    miso_out = '0;
                    next_delayctr = delayctr;
                end else if (in[31:16] == '0 || delayctr == in[31:16]) begin
                    next_state = 2'b00;

                    //default
                    bitData = 0;
                    next_data = dataforOutput;
                    next_miso = misoShift;
                    //next_state = state;
                    ack = 0;
                    chipSelect = 1;
                    sclk = 0;
                    next_ctr = counter;
                    miso_out = '0;
                    next_delayctr = delayctr;
                end 
            end //idle / delay
            default: begin sclk = 0; ack = 0; 
                    bitData = 0;
                    next_data = dataforOutput;
                    next_miso = misoShift;
                    next_state = state;
                    ack = 0;
                    chipSelect = 1;
                    sclk = 0;
                    next_ctr = counter;
                    miso_out = '0;
                    next_delayctr = delayctr;
            end
    endcase
end
endmodule