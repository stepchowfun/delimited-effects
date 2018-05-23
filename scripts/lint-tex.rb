#!/usr/bin/env ruby

# This script checks a .tex file for any occurrences of macros applied
# to an incorrect number of arguments.
#
# Usage:
#   ./scripts/lint-tex.rb path

MACRO_REGEX = /\\newcommand\s*\\([A-Za-z@]+)\s*(\[\s*([0-9]+)\s*\])?\s*{/
CURLY_REGEX = /{(\g<0>|[^{}])*}/
SQUARE_REGEX = /\[(\g<0>|[^\[\]])*\]/

PRELUDE_ARITIES = {
  'AxiomC' => [1],
  'BinaryInfC' => [1],
  'Coloneqq' => [0],
  'Delta' => [0],
  'DisplayProof' => [0],
  'Downarrow' => [0],
  'Gamma' => [0],
  'Lambda' => [0],
  'QuaternaryInfC' => [1],
  'RightLabel' => [1],
  'Rightarrow' => [0],
  'SetWatermarkLightness' => [1],
  'SetWatermarkScale' => [1],
  'SetWatermarkText' => [1],
  'Shortstack' => [1],
  'Sigma' => [0],
  'TrinaryInfC' => [1],
  'UnaryInfC' => [1],
  'Uparrow' => [0],
  'Updownarrow' => [0],
  'acmArticle' => [1],
  'acmDOI' => [1],
  'acmJournal' => [1],
  'acmMonth' => [1],
  'acmNumber' => [1],
  'acmVolume' => [1],
  'acmYear' => [1],
  'affiliation' => [1],
  'alpha' => [0],
  'author' => [1],
  'begin' => [1, 2],
  'beginAcm' => [0],
  'beginNonAcm' => [0],
  'beta' => [0],
  'bfseries' => [0],
  'bibliography' => [1],
  'bibliographystyle' => [1],
  'bigskipamount' => [0],
  'boxed' => [1],
  'caption' => [1],
  'ccsdesc' => [1],
  'circ' => [0],
  'citep' => [1],
  'citestyle' => [1],
  'citet' => [1],
  'city' => [1],
  'color' => [1],
  'copyrightyear' => [1],
  'country' => [1],
  'cup' => [0],
  'date' => [1],
  'definecolor' => [3],
  'diamond' => [0],
  'displaystyle' => [0],
  'documentclass' => [1],
  'else' => [0],
  'email' => [1],
  'emph' => [1],
  'end' => [1],
  'endAcm' => [0],
  'endNonAcm' => [0],
  'enspace' => [0],
  'fbox' => [1],
  'fboxsep' => [0],
  'fi' => [0],
  'forall' => [0],
  'framebox' => [1],
  'iffalse' => [0],
  'iftrue' => [0],
  'ifuseacm' => [0],
  'input' => [1],
  'institution' => [1],
  'kappa' => [0],
  'kern' => [0],
  'keywords' => [1],
  'label' => [1],
  'lambda' => [0],
  'langle' => [0],
  'ldots' => [0],
  'left' => [0],
  'leftarrow' => [0],
  'let' => [0],
  'linewidth' => [0],
  'llbracket' => [0],
  'lstdefinelanguage' => [2],
  'lstset' => [1],
  'm@th' => [0],
  'makeatletter' => [0],
  'makeatother' => [0],
  'maketitle' => [0],
  'mapsto' => [0],
  'mathbf' => [1],
  'mathclose' => [1],
  'mathit' => [1],
  'mathopen' => [1],
  'medskip' => [0],
  'newcommand' => [0],
  'newif' => [0],
  'newtheorem' => [2],
  'noindent' => [0],
  'notin' => [0],
  'nsubseteq' => [0],
  'omega' => [0],
  'operatorname' => [1],
  'orcid' => [1],
  'postcode' => [1],
  'qquad' => [0],
  'quad' => [0],
  'rangle' => [0],
  'received' => [1],
  'ref' => [1],
  'renewcommand' => [0],
  'right' => [0],
  'rightarrow' => [0],
  'rightsquigarrow' => [0],
  'rrbracket' => [0],
  'section' => [1],
  'setcopyright' => [1],
  'small' => [0],
  'star' => [0],
  'state' => [1],
  'streetaddress' => [1],
  'subsection' => [1],
  'subseteq' => [0],
  'subsubsection' => [1],
  'tau' => [0],
  'text' => [1],
  'textbf' => [1],
  'textcolor' => [2],
  'textsc' => [1],
  'textsf' => [1],
  'title' => [1],
  'ttfamily' => [0],
  'useacmtrue' => [0],
  'usepackage' => [1],
  'varepsilon' => [0],
  'varnothing' => [0],
  'vdash' => [0],
}

def prefix_matches?(s, regex)
  (s =~ regex) == 0
end

def count_args(s)
  arity = 0
  loop do
    if prefix_matches?(s, SQUARE_REGEX)
      s.slice!(SQUARE_REGEX)
    elsif prefix_matches?(s, CURLY_REGEX)
      s.slice!(CURLY_REGEX)
      arity += 1
    else
      break
    end
  end
  arity
end

# Get macros from each of the input files.
macros = {}
ARGV.each do |path|
  # Read the contents of the file.
  doc = File.read(path)
  lines = doc.split("\n", -1)

  # Get the arity of all the macros.
  macros = macros.merge(
    PRELUDE_ARITIES.merge(
      doc.scan(MACRO_REGEX).map do |r|
        [r[0], r[2] ? [r[2].to_i] : [0]]
      end.to_h
    )
  )
end

# Check each of the input files.
ARGV.each do |path|
  # Read the contents of the file.
  doc = File.read(path)
  lines = doc.split("\n", -1)

  # Remove newcommands so they don't get checked.
  doc.gsub!(/\\newcommand\s*\\[A-Za-z@]+/, '')

  # Check for undefined macros.
  undefined_macros = doc.scan(/\\[A-Za-z@]+/).select do |r|
    !macros[r[1..-1]]
  end.uniq

  if !undefined_macros.empty?
    STDERR.puts(
      "Error: Unrecognized macros #{undefined_macros} in #{path}."
    )
    exit(1)
  end

  # Check the arities of macro invocations.
  macros.each do |macro, arities|
    line_num = 1
    suffix = doc

    # Iterate while there are still invocations of the macro to be checked.
    while !suffix.empty?
      # Find the next invocation of the macro.
      prefix, match, suffix = suffix.partition(/\\#{macro}\b/)
      break if match.empty?
      line_num += prefix.scan("\n").size

      # Compute the arity of the macro.
      arity = count_args(suffix)

      # Report an error if the arity didn't match what we expected.
      if !macros[macro].include?(arity)
        STDERR.puts(
          "Error: Expected #{macros[macro]} argument(s) for macro " \
            "`#{macro}` on line #{line_num} of #{path}, but found #{arity}."
        )
        exit(1)
      end
    end
  end
end
