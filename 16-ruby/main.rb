Field = Struct.new(:name, :ranges)

def parse_field(s)
  name, raw_ranges = s.split(": ")
  Field.new(name, raw_ranges
                    .split(" or ")
                    .map { |r| r.split("-").map(&:to_i) })
end

def field_value_valid?(value, field)
  for low, high in field.ranges do
    if low <= value && value <= high then
      return true
    end
  end
  return false
end

def first_invalid_field(ticket, fields)
  for value in ticket do
    valid = false
    for field in fields do
      if field_value_valid?(value, field) then
        valid = true
        break
      end
    end

    if !valid then
      return value
    end
  end

  return -1
end

def ticket_valid?(ticket, fields)
  first_invalid_field(ticket, fields) < 0
end

def part_1(input)
  result = 0
  for ticket in input.nearby do
    x = first_invalid_field(ticket, input.fields)
    if x >= 0 then
      result += x
    end
  end
  result
end

def valid_fields_for_value(value, fields)
  fields.select do |field|
    valid = false
    for low,high in field.ranges do
      if low <= value && value <= high then
        valid = true
        break
      end
    end
    valid
  end
end

def part_2(input)
  result = input.nearby.select do
    |ticket| ticket_valid?(ticket, input.fields)
  end.map do |ticket|
    ticket.map do |value|
      valid_fields_for_value(value, input.fields).map{|f| f.name}
    end
  end.transpose.each_with_index.map do |col, index|
    result = col[0]
    col.each do |x|
      result = result & x
    end
    Struct.new(:index, :names).new(index, result)
  end.sort_by {|set| set.names.length}

  for i in 0..result.length - 2 do
    for j in i+1..result.length - 1 do
      result[j].names.delete(result[i].names[0])
    end
  end

  result.select do |x|
    x.names[0].start_with?("departure")
  end.map do |x|
    input.my[x.index]
  end.reduce(:*)
end

def solve_file(file_path)
  content = IO.read(file_path)
            .split("\n")

  fields = content
             .take_while{|s| !s.empty?}
             .map{|x| parse_field(x)}
  content  = content.drop_while{|s| !s.empty?}.drop(1)

  my = content
         .take_while{|s| !s.empty?}
         .drop(1)[0]
         .split(",")
         .map(&:to_i)
  content = content.drop_while{|s| !s.empty?}.drop(1)

  nearby = content.take_while{|s| !s.empty?}
             .drop(1)
             .map{|s| s.split(",").map(&:to_i)}

  input = Struct
    .new(:fields, :my, :nearby)
    .new(fields, my, nearby)

  puts "Input file: #{file_path}"
  puts "Part 1: #{part_1(input)}"
  puts "Part 2: #{part_2(input)}"
end

for file_path in ARGV
  solve_file(file_path)
end
