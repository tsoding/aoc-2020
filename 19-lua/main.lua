function dump(o)
    if type(o) == 'table' then
        local s = '{ '
        for k,v in pairs(o) do
                if type(k) ~= 'number' then k = '"'..k..'"' end
                s = s .. '['..k..'] = ' .. dump(v) .. ','
        end
        return s .. '} '
    else
        return tostring(o)
    end
end

function my_split(s, d)
   local result = {}

   while #s > 0 do
      i, j = string.find(s, d)

      if i then
         table.insert(result, string.sub(s, 1, i - 1))
         s = string.sub(s, i + #d)
      else
         table.insert(result, s)
         s = ""
      end
   end

   return result
end

function parse_def(def)
   local match = string.match(def, "\"(.*)\"")
   if match then
      return {kind = "match", value = match}
   else
      local result = {}
      for _, subrule in pairs(my_split(def, " | ")) do
         local seq = {}
         for _, rule in pairs(my_split(subrule, " ")) do
            table.insert(seq, tonumber(rule))
         end
         table.insert(result, {kind = "seq", value = seq})
      end
      return {kind = "alt", value = result}
   end
end

function parse_rule(line)
   local result = {}
   for index, def in string.gmatch(line, "(.+): (.+)") do
      result = {
         index = tonumber(index),
         def = parse_def(def)
      }
   end
   return result
end

function match_rule_seq(line, rules, seq)
   local inputs = {line}
   assert(seq.kind == "seq")
   for _, subindex in pairs(seq.value) do
      if #inputs == 0 then
         break
      end

      local more_inputs = {}
      for _, input in pairs(inputs) do
         for _, rest in pairs(match_rule_index(input, rules, subindex), _) do
            table.insert(more_inputs, rest)
         end
      end

      inputs = more_inputs
   end
   return inputs
end

function match_rule_index(line, rules, index)
   local kind = rules[index].kind
   local value = rules[index].value
   if kind == "match" then
      if string.sub(line, 1, 1) == value then
         return {string.sub(line, 2)}
      else
         return {}
      end
   elseif kind == "seq" then
      return match_rule_seq(line, rules, value)
   elseif kind == "alt" then
      local result = {}

      for _, seq in pairs(value) do
         assert(seq.kind == "seq")
         for _, input in pairs(match_rule_seq(line, rules, seq)) do
            table.insert(result, input)
         end
      end

      return result
   else
      assert(false, "Unknown kind of rule `" .. kind .. "`")
   end
end

function fully_parsed(inputs)
   for _, input in ipairs(inputs) do
      if #input > 0 then
         return false
      end
   end
   return true
end

function part_1(file_path)
   local rules = {}
   local parsing_rules = true
   local result = 0

   for line in io.lines(file_path) do
      if parsing_rules then
         if #line == 0 then
            parsing_rules = false
            goto continue
         end

         local rule = parse_rule(line)
         rules[rule.index] = rule.def
      else
         local inputs = match_rule_index(line, rules, 0)
         if #inputs > 0 and fully_parsed(inputs) then
            -- print(line)
            -- print(dump(rests))
            result = result + 1
         end
      end
      ::continue::
   end

   return result
end

function solve_file(file_path)
   print("Input file:", file_path)
   print("Part 1", part_1(file_path))
end

for i=1,#arg do
   solve_file(arg[i])
end
