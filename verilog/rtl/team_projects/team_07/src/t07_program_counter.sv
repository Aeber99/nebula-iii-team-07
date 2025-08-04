// this module takes an input of what type of jump to perform
// and outputs the next program counter address. 


//In the case of a force jump, the link address is set to the 
//next program counter value and the program counter is set to the
//jump distance plus the current program counter.

// In the case of a conditional jump, the link address is zero
// and the program counter is set to the current program counter value plus
// the jump distance if the condition is met.

// In all other cases, the link address is zero and the program counter
// is set to the current program counter value plus 4.

module t07_program_counter (
    //inputs
    input logic clk, nrst,
    input logic freeze, // Freeze signal to pause CPU operations
    input logic forceJump,
    input logic condJump,
    input logic [2:0] func3, // Function code for the jump operation
    input logic [6:0] ALU_flags,
    input logic [31:0] JumpDist,

    //outputs
    output logic [31:0] programCounter, linkAddress
);
logic [31:0] n_programCounter, n_linkAddress; // Next values for program counter and link address

always_ff @(negedge nrst, posedge clk) begin //check if negedge works with fetch timing
    if (~nrst) begin
        programCounter <= 32'b0/*11111111111111111111111111111100*/; // Reset PC to 0
        linkAddress <= 32'b0; // Reset link address to 0
    end else begin
        programCounter <= n_programCounter; // Default increment
        linkAddress <= n_linkAddress; // Default link address
    end
end

always_comb begin
    if (freeze) begin
        n_programCounter = programCounter; // Hold current PC if frozen
        n_linkAddress = linkAddress; // Hold current link address if frozen
    end else begin
        if (forceJump) begin
            // If force jump is requested, set link address to next programCounter and move programCounter by the jump
            n_linkAddress = programCounter + 4;
            n_programCounter = programCounter + JumpDist;
        end else if (condJump) begin 
            if ((func3 == 3'b000 && ALU_flags[6])||  //beq - equal
                (func3 == 3'b001 && ALU_flags[5]) || //bne - not equal
                (func3 == 3'b100 && ALU_flags[3]) || //blt - less than signed
                (func3 == 3'b101 && ALU_flags[1]) || //bge - greater than or equal signed
                (func3 == 3'b110 && ALU_flags[4]) || // bltu - less than unsigned
                (func3 == 3'b111 && ALU_flags[2])    // bgeu - greater than or equal unsigned
            ) begin //conditions to meet a branch
                // If condition is met, set link address and jump
                n_linkAddress = 32'b0;
                n_programCounter = programCounter + JumpDist;
            end else begin
                n_linkAddress = 32'b0;
                n_programCounter = programCounter + 4; // No jump, just increment PC
            end
        end else begin
            //Normal operation, just increment program counter
            n_linkAddress = 32'b0;
            n_programCounter = programCounter + 4;
        end
    end
end

// always_ff @(posedge clk or posedge rst) begin
//     if (rst) begin
//         programCounter <= 32'b0; // Reset PC to 0
//         linkAddress <= 32'b0; // Reset link address to 0
//     end else begin
//         if (forceJump) begin
//             // If force jump is requested, set link address to next programCounter and move programCounter by the jump
//             linkAddress <= programCounter + 4;
//             programCounter <= programCounter + JumpDist;
//         end else if (condJump) begin 
//             if (ALU_flags[2]) begin // Assuming ALU_flags[2] indicates a condition met for a branch
//                 // If condition is met, set link address and jump
//                 linkAddress <= 32'b0;
//                 programCounter <= programCounter + JumpDist;
//             end else begin
//                 linkAddress <= 32'b0;
//                 programCounter <= programCounter + 4; // No jump, just increment PC
//             end
//         end else begin
//             //Normal operation, just increment program counter
//             linkAddress <= 32'b0;
//             programCounter <= programCounter + 4;
//         end
//     end
// end
endmodule