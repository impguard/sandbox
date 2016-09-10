puts "Using only each"

i = 0
(1..16).each do |num|
    print "%2d " % num
    i += 1

    if i % 4 == 0
        puts
    end
end

puts
puts "Using each_slice"

(1..16).each_slice(4) do |slice|
    slice.each {|num| print("%2d " % num)}
    puts
end
