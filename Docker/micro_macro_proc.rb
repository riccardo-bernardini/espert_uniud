module Micro_Macro_Proc
  class Immutable_Expansion < String
    #
    # Are you wondering what this class is about?  It is used for 
    # the macro definition defined on the CLI.  These definitions take
    # precedence on any definition found in the expanded file.  In order
    # to mark said definitions are "immutable", we will store them as
    # Immutable_Expansion and not as String.
    #
  end

  def Micro_Macro_Proc.extract_command(line, options)
    
    start = line.index(options[:open])

    return [line, nil, nil] unless start

    stop = line[start+2..-1].index(options[:close])

    if stop.nil?
      return [line, nil, nil] 
    else
      stop += start+2
      return [line[0...start], line[start+2...stop], line[stop+2..-1]]
    end
  end

  def Micro_Macro_Proc.execute_command(command, macros, missing, options)
    return "" if command[0...options[:comment].size] == options[:comment]
    
    name, val=command.split('=', 2)

    if val.nil?
      if macros.has_key?(command)
        return macros[command]

      else
        on_undefined = 

        case options[:on_undefined]
        when String
          missing[command]=true
          return options[:on_undefined]

        when :die
          raise "Macro '#{command}' undefined"

        else
          raise "I shouldn't be here"
          
        end

      end
    else
      unless macros[name].is_a?(Immutable_Expansion)
        macros[name]=val
      end

      return ""
    end
  end

  def Micro_Macro_Proc.expand(input, output, macros={}, options={})
    default_options = {:open         => '%{',
                       :close        => '}%',
                       :comment      => '--',
                       :on_undefined => "" }

    default_options.keys.each do |key|
      unless options.has_key?(key)
        options[key] = default_options[key]
      end
    end
    
    missing = Hash.new
    
    input.each do
      |line|

      loop do
        pre, command, post = Micro_Macro_Proc.extract_command(line, options)

        break if command.nil?

        replacement = Micro_Macro_Proc.execute_command(command, macros, missing, options)

        line = pre + replacement + post
      end

      output << line
    end

    return missing.keys
  end

  def Micro_Macro_Proc.no_override(s)
    Immutable_Expansion.new(s)
  end

  def Micro_Macro_Proc.prefill_from_cli(macros=nil, argv=ARGV)
    macros = Hash.new if macros.nil?

    argv.each do |arg|
      name, val=arg.split('=', 2)

      next if val.nil?

      macros[name]=Micro_Macro_Proc.no_override(val)
    end

    return macros
  end

end
