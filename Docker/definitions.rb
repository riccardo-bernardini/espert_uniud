# coding: utf-8
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

def to_root(path, type)
  File.dirname(path)
end

def root_to(root, type)
  case type
  when :html
    File.join(root, 'htdocs')

  when :cgi
    File.join(root, 'cgi-bin')

  when :exe
    File.join(root, 'library')

  when :lib
    File.join(root, 'library')

  when :jobs
    File.join(root, 'jobs')

  when :log
    File.join(root, 'htdocs/logs')

  else
    raise "Che è 'sta robba? #{type}"
  end
end

def convert_dir(path, from, to)
  root = to_root(path, from)
  return root_to(root, to)
end

  
