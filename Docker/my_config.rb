def percentage_to_status(x)
  [(2*x).to_i].pack('C')
end

Fully_done = percentage_to_status(100)

Accumulator_Done    = Fully_done + 1
Archive_Ready       = Accumulator_Done + 1
Error_while_Zipping = Archive_Ready + 1

def exitcode_to_status(x)
  [x + Error_while_Zipping + 1].pack('C')
end

