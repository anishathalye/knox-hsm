module counter #(
    parameter WIDTH = 32
) (
    input clk,
    input resetn,
    input en,
    input [WIDTH-1:0] x,
    output [WIDTH-1:0] out,
);

reg [WIDTH-1:0] counter;

always @(posedge clk) begin
    if (!resetn) begin
        /* do nothing */
    end else if (en) begin
        counter <= (counter + x < counter) ? 32'hffff_ffff : (counter + x);
    end
end

assign out = counter;

endmodule
