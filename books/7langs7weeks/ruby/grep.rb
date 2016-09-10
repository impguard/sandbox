if ARGV.length < 2
    puts "usage: grep.rb string filename"
    exit
end

searchstring = ARGV.shift
ARGV.shift if ARGV[0] == "-"

ARGF.each_line do |line|
    puts "%i: %s" % [ARGF.lineno, line] if line.index(searchstring)
end
