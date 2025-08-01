module t07_FPU_div (
    input logic clk, nrst,
    input logic [31:0] inA, inB,
    input logic signA, signB,
    input [4:0] op,
 
    output logic [31:0] quotient, remainder,
    output logic sign, busy
);

    logic [31:0] A, B; //A = dividend, B = divisor
    logic [31:0] next_B, next_quot, next_rem;
    logic workFlag;

    logic [1:0] state, next_state;

    logic [4:0] counter, next_ctr;

    // assign B = curr_B;
    // assign quotient = curr_quot;
    // assign remainder = curr_rem;

    always_ff @(posedge clk, negedge nrst) begin
        if(~nrst) begin
            state <= 2'b0;
            // B <= inB;
            // quotient <= '0;
            // remainder <= inA;
            // counter <= 5'b0;
            // busy = 0;
        end
        else begin
            // busy = 1;
            state <= next_state;
            
            // quotient <= next_quot;
            // remainder <= next_rem;
            // B <= next_B;
            //counter <= next_ctr;
        end
    end    
//set busy back to 0
    always_comb begin
        case (state)
            2'b00: begin remainder = inA; counter = 5'b0; quotient = '0; B = inB; busy = 1; end //load
            2'b01: begin 
                sign = 0;
                next_rem = remainder;
                next_B = B;
                next_quot = quotient;
                next_ctr = counter;
                busy =1;
                
                for (integer i = 0; i < 5'b11111; i++) begin
                    remainder = remainder - B;
                    if (remainder >= B) begin
                        quotient = {quotient[30:0], 1'b0} + 1;
                        B = {1'b0, B[31:1]};
                    end else begin
                        remainder = remainder + B;
                        quotient = {quotient[30:0], 1'b0};
                    end
                    B = {1'b0, B[31:1]};
                end


                next_state = 2'b10;



                // if (counter < 5'b11111) begin
                //     next_rem = remainder - B;
                //     if (remainder >= B) begin //previously remainder == 0 
                //         next_quot =  (quotient << 1) + 1; 
                //         next_B = B >> 1;
                //     end
                //     else begin  
                //         next_rem = B + remainder;
                //         next_quot = quotient << 1;
                //     end
                //     next_B = B >> 1;
                //     next_ctr = counter + 1; 
                // end else if (counter == 5'b11111) begin
                //     next_state = 2'b10;
                // end

                //determines sign of product
                if (signA == signB) begin
                    sign = 0;
                end else begin
                    sign = 1;
                end
            end //divide
            2'b10: if (op == 5'd7) begin next_state = 2'b00; end else begin busy = 0; end //idle
            default: busy = 0;
        endcase
    end






        

    //assign result = quotient[63:32];


endmodule