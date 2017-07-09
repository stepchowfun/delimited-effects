# Phony targets

.PHONY: \
  all \
  paper lint formalization \
  clean clean-paper clean-formalization \
  docker-deps docker-build

all: paper lint formalization

paper: main.pdf

lint:
	OUTPUT="$$(lacheck paper/main.tex)"; \
	  if [ -n "$$OUTPUT" ]; \
	    then echo "$$OUTPUT"; \
	  exit 1; \
	    else echo "No lacheck errors."; \
	  fi
	OUTPUT="$$(chktex paper/main.tex)"; \
	  if [ -n "$$OUTPUT" ]; \
	    then echo "$$OUTPUT"; \
	  exit 1; \
	    else echo "No ChkTeX errors."; \
	  fi
	./scripts/check-line-lengths.sh \
	  .travis.yml \
	  Makefile \
	  formalization/*.v \
	  scripts/*

formalization: \
  $(addprefix       \
    formalization/, \
    syntax.vo       \
    helpers.vo      \
    typing.vo       \
    subtyping.vo    \
    semantics.vo    \
  )

clean: clean-paper clean-formalization

clean-paper:
	rm -rf .paper-build main.pdf

clean-formalization:
	rm -rf \
	  formalization/*.glob \
	  formalization/*.vo \
	  formalization/.*.vo.aux \
	  .paper-build

docker-deps:
	docker build \
	  -f scripts/Dockerfile \
	  -t stephanmisc/delimited-effects:deps \
	  .

docker-build:
	docker run \
	  --rm \
	  -v $$(pwd):/root \
	  stephanmisc/delimited-effects:deps \
	  sh -c 'cd /root && make'

# The paper

main.pdf: paper/main.tex
	mkdir -p ".paper-build"
	pdflatex \
	  -interaction=nonstopmode \
	  -output-directory ".paper-build" \
	  paper/main.tex
	while ( \
	  grep -qi '^LaTeX Warning: Label(s) may have changed' \
	    '.paper-build/main.log' \
	) do \
	  pdflatex \
	    -interaction=nonstopmode \
	    -output-directory ".paper-build" \
	    paper/main.tex; \
	done
	mv .paper-build/main.pdf .

# The formalization

formalization/syntax.vo: formalization/syntax.v
	COQPATH="$$(pwd)/formalization" coqc formalization/syntax.v

formalization/helpers.vo: \
  $(addprefix formalization/, syntax.vo helpers.v)
	COQPATH="$$(pwd)/formalization" coqc formalization/helpers.v

formalization/typing.vo: \
  $(addprefix formalization/, syntax.vo helpers.vo typing.v)
	COQPATH="$$(pwd)/formalization" coqc formalization/typing.v

formalization/subtyping.vo: \
  $(addprefix formalization/, syntax.vo typing.vo subtyping.v)
	COQPATH="$$(pwd)/formalization" coqc formalization/subtyping.v

formalization/semantics.vo: \
  $(addprefix formalization/, syntax.vo helpers.vo semantics.v)
	COQPATH="$$(pwd)/formalization" coqc formalization/semantics.v
