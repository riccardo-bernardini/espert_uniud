def impacca(n)
  [n].pack('C')
end

def percentage_to_status(x)
  impacca((2*x).to_i)
end

Fully_done = impacca(200)

Accumulator_Done    = impacca(201)
Archive_Ready       = impacca(202)
Error_while_Zipping = impacca(203)

def exitcode_to_status(x)
  impacca(x + 204)
end

