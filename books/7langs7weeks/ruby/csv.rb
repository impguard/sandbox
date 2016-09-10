module ActsAsCsv
    class CsvRow
        attr_accessor :contents, :headers

        def initialize row, headers
            @contents = row.chomp.split(", ")
            @headers = headers
        end

        def method_missing name, *args
            @contents.at @headers.index(name.to_s)
        end
    end

    attr_accessor :headers, :rows

    def initialize
        read
    end

    def read
        filename = self.class.to_s.downcase.gsub(/csv$/, "") + ".csv"
        file = File.new(filename)
        @headers = file.gets.chomp.split(', ')

        @rows = []
        file.each do |row|
            @rows << CsvRow.new(row, @headers)
        end
    end

    def each
        @rows.each {|row| yield(row)}
    end
end

class SimpleCsv
    include ActsAsCsv
end

csv = SimpleCsv.new
csv.each {|row| puts row.one}
