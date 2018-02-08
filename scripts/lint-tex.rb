#!/usr/bin/env ruby

# This script checks a .tex file for any occurrences of macros applied
# to an incorrect number of arguments.
#
# Usage:
#   ./scripts/lint-tex.rb path

MACRO_REGEX = /\\newcommand\s*\\(\w+)\s*(\[\s*([0-9]+)\s*\])?\s*{/

def check(path, line_num, macros, s)
  # Split the string into [prefix, macro, suffix], or [s, '', ''] if no macro
  # was found.
  prefix, macro, suffix = s.partition(/\\(\w+)/)
  macro_name = macro[1..-1]

  # Return to the parent if there are no macros in the string.
  return s.partition('}').last if macro.empty?

  # Return to the parent if we encounter a closing brace before the next
  # macro to be processed.
  return s.partition('}').last if prefix.include?('}')

  # Check each argument recursively.
  arity = 0
  while suffix[0] == '{'
    suffix = check(path, line_num, macros, suffix[1..-1])
    arity += 1
  end

  # Report an error if the arity didn't match what we expected.
  if macros[macro_name] && macros[macro_name] != arity
    STDERR.puts(
      "Error: Expected #{macros[macro_name]} argument(s) for macro " \
        "`#{macro_name}` on line #{line_num} of #{path}, but found #{arity}."
    )
    exit(1)
  end

  # Check the rest of the string recursively.
  check(path, line_num, macros, suffix)
end

# Iterate over the input files.
ARGV.each do |path|
  # Read the contents of the file.
  doc = File.read(path)
  lines = doc.split("\n", -1)

  # Get the arity of all the macros.
  macros = doc.scan(MACRO_REGEX).map do |r|
    [r[0], r[2] ? r[2].to_i : 0]
  end.to_h

  # Check the macro invocations on each line.
  lines.each_with_index do |line, index|
    # Don't check commands currently being defined.
    line.slice!(MACRO_REGEX)

    # Scan the line for macro invocations and check the arities.
    remainder = line
    while !remainder.empty?
      remainder = check(path, index + 1, macros, remainder)
    end
  end
end
