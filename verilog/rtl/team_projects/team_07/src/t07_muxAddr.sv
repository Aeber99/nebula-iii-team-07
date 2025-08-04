module t07_muxAddr(
    input logic [31:0] memAddr_i, memAddr_comb_i, pc_i,
    input logic control, clk, nrst,
    output logic [31:0] addr_o  
);
    logic prev_control, control_negedge;

    //edge detector
    always_ff @(negedge nrst, posedge clk) begin
        if(~nrst) begin
            prev_control <= '0;
        end else begin
            prev_control <= control;
        end
    end

    assign control_negedge = (~control && prev_control); 

    always_comb begin
        if (control) begin
            addr_o = pc_i; 
        end else if (control_negedge) begin
            addr_o = memAddr_comb_i;
        end else begin
            addr_o = memAddr_i;
        end     
    end
endmodule