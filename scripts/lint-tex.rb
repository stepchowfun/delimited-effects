#!/usr/bin/env ruby

# This script checks a .tex file for any occurrences of macros applied
# to an incorrect number of arguments.
#
# Usage:
#   ./scripts/lint-tex.rb path

def check(path, line_num, macros, s)
  split = s.split(/\\(\w+)/, 2)

  # Return to the parent if we encounter a closing brace before the next
  # macro to be processed.
  prefix = split[0] || ''
  return s.partition('}').last if prefix.include?('}')

  # If the split failed, then there is no macro in the string, so we
  # are done checking it and return an empty string.
  return '' if split.length < 3
  _, macro, remainder = split

  # Check each argument.
  arg_count = 0
  while remainder[0] == '{'
    remainder = check(path, line_num, macros, remainder[1..-1])
    arg_count += 1
  end

  if macros[macro] && macros[macro] != arg_count
    STDERR.puts(
      "Error: Line #{line_num} of #{path} expects #{macros[macro]} " \
      "argument(s) for macro `#{macro}`, but found #{arg_count}."
    )
    exit(1)
  end
  check(path, line_num, macros, remainder)
end

MACRO_REGEX = /^\\newcommand\s*\\(\w+)\s*\[\s*([0-9]+)\s*\]?\s*{/

path = ARGV[0]
doc = File.read(path)
lines = doc.split("\n", -1)

# Add all the macros to the hash.
macros = doc.scan(MACRO_REGEX).map do |r|
  [r[0], r[1] ? r[1].to_i : 0]
end.to_h

# Check the macros on each line.
lines.each_with_index do |line, index|
  # Don't check commands currently being defined.
  line.slice!(MACRO_REGEX)

  check(path, index + 1, macros, line)
end
