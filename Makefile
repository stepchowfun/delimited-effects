override COQ_SOURCES := $(shell find formalization/*.v -type f) \

# Phony targets

.PHONY: \
  all \
  paper lint formalization implementation \
  clean clean-paper clean-formalization clean-implementation \
  docker-deps docker-build

all: paper lint formalization implementation

paper: main.pdf

lint:
	./scripts/check-line-lengths.sh \
	  .travis.yml \
	  Makefile \
	  formalization/*.v \
	  scripts/*

formalization: $(patsubst %.v,%.vo,$(COQ_SOURCES))

implementation:
	cd implementation && \
	  stack build --install-ghc --allow-different-user && \
	  stack test --allow-different-user

clean: clean-paper clean-formalization clean-implementation

clean-paper:
	rm -rf .paper-build main.pdf

clean-formalization:
	rm -rf \
	  formalization/*.glob \
	  formalization/*.vo \
	  formalization/.*.vo.aux \
	  .paper-build

clean-implementation:
	rm -rf implementation/.stack-work

docker-deps:
	docker build \
	  -f scripts/Dockerfile \
	  -t stephanmisc/delimited-effects:deps \
	  .

docker-build:
	docker run \
	  --rm \
	  --ulimit nofile=65536:65536 \
	  --volume $$(pwd):/root/delimited-effects \
	  stephanmisc/delimited-effects:deps \
	  sh -c 'cd /root/delimited-effects && make'

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
