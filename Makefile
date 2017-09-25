override COQ_SOURCES := $(shell find formalization/*.v -type f) \

# Phony targets

.PHONY: \
  all \
  paper lint formalization \
  clean clean-paper clean-formalization \
  docker-deps docker-build

all: paper lint formalization

paper: main.pdf

lint:
	./scripts/check-line-lengths.sh \
	  .travis.yml \
	  Makefile \
	  formalization/*.v \
	  scripts/*

formalization: $(patsubst %.v,%.vo,$(COQ_SOURCES))

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
