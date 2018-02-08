#!/usr/bin/env ruby

# This script checks a .tex file for any occurrences of macros applied
# to an incorrect number of arguments.
#
# Usage:
#   ./scripts/lint-tex.rb path

MACRO_REGEX = /\\newcommand\s*\\([A-Za-z@]+)\s*(\[\s*([0-9]+)\s*\])?\s*{/

PRELUDE_ARITIES = {
  'AxiomC' => [1],
  'BinaryInfC' => [1],
  'Coloneqq' => [0],
  'Delta' => [0],
  'Downarrow' => [0],
  'Gamma' => [0],
  'Lambda' => [0],
  'RightLabel' => [1],
  'SetWatermarkLightness' => [1],
  'SetWatermarkScale' => [1],
  'SetWatermarkText' => [1],
  'Shortstack' => [1],
  'Sigma' => [0],
  'UnaryInfC' => [1],
  'Uparrow' => [0],
  'Updownarrow' => [0],
  'alpha' => [0],
  'begin' => [1, 2],
  'bfseries' => [0],
  'bigskipamount' => [0],
  'boxed' => [0],
  'caption' => [1],
  'color' => [1],
  'cup' => [0],
  'date' => [1],
  'definecolor' => [3],
  'displaystyle' => [0],
  'documentclass' => [1],
  'end' => [1],
  'fbox' => [1],
  'fboxsep' => [0],
  'fi' => [0],
  'forall' => [0],
  'framebox' => [1],
  'iffalse' => [0],
  'kern' => [0],
  'label' => [1],
  'lambda' => [0],
  'left' => [0],
  'leftarrow' => [0],
  'llbracket' => [0],
  'lstdefinelanguage' => [2],
  'lstset' => [1],
  'm@th' => [0],
  'makeatletter' => [0],
  'makeatother' => [0],
  'maketitle' => [0],
  'mapsto' => [0],
  'medskip' => [0],
  'newcommand' => [0],
  'newtheorem' => [2],
  'notin' => [0],
  'nsubseteq' => [0],
  'renewcommand' => [1],
  'right' => [0],
  'rightarrow' => [0],
  'rrbracket' => [0],
  'section' => [1],
  'subsection' => [1],
  'subseteq' => [0],
  'subsubsection' => [1],
  'tau' => [0],
  'text' => [1],
  'textbf' => [1],
  'textcolor' => [2],
  'textsc' => [1],
  'title' => [1],
  'ttfamily' => [0],
  'usepackage' => [1],
  'varepsilon' => [0],
  'varnothing' => [0],
  'vdash' => [0],
}

def check(path, line_num, macros, s)
  # Split the string into [prefix, macro, suffix], or [s, '', ''] if no macro
  # was found.
  prefix, macro, suffix = s.partition(/\\([A-Za-z@]+)/)
  macro_name = macro[1..-1]

  # Return to the parent if there are no macros in the string.
  return s.partition('}').last if macro.empty?

  # Return to the parent if we encounter a closing brace before the next
  # macro to be processed.
  return s.partition('}').last if prefix.include?('}')

  # Skip anything in square brackets.
  while suffix[0] == '['
    nesting = 1
    pos = 1

    while suffix[pos] && nesting > 0
      if suffix[pos] == '['
        nesting += 1
      end

      if suffix[pos] == ']'
        nesting -= 1
      end

      pos += 1
    end

    suffix = suffix[pos..-1]
  end

  # Check each argument recursively.
  arity = 0
  while suffix[0] == '{'
    suffix = check(path, line_num, macros, suffix[1..-1])
    arity += 1
  end

  # Report an error if the arity didn't match what we expected.
  if macros[macro_name]
    if !macros[macro_name].include?(arity)
      STDERR.puts(
        "Error: Expected #{macros[macro_name]} argument(s) for macro " \
          "`#{macro_name}` on line #{line_num} of #{path}, but found #{arity}."
      )
      exit(1)
    end
  else
    STDERR.puts(
      "Error: Unrecognized macro #{macro_name} on line #{line_num} of #{path}."
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
  macros = PRELUDE_ARITIES.merge(
    doc.scan(MACRO_REGEX).map do |r|
      [r[0], r[2] ? [r[2].to_i] : [0]]
    end.to_h
  )

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
