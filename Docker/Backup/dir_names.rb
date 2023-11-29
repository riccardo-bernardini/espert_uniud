
$my_dir = File.dirname(File.absolute_path(__FILE__))

$dirs=Hash.new

File.open(File.join($my_dir, 'tree.db')) do |input|
  input.each do
    |line|

    next if line =~ /^ *$/

    name, path = line.chomp.split

    p line
    p name
    p path

    $dirs[name.to_sym] = File.join(ENV['HTTPD_PREFIX'], path)
  end
end
