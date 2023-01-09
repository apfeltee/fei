#!/usr/bin/ruby

dt = []
$stdin.each_line do |l|
  m = l.match(/^\s*(?:(?:inline|static)\s+){0,2}(?!else|typedef|return)\w+\s+\*?\s*(?<name>\w+)\s*\([^0]+\)\s*;?/)
  if !m then
    $stderr.printf("could not match %p\n", l)
  else
    name = m["name"]
    dt.push(name)
  end
end
print('\b('+dt.join("|")+')\b\s*\((?!(vm|VM))')
