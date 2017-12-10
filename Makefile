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
	  $(shell \
	    find . \
	      -type d \( \
	        -path ./.git -o \
	        -path ./.github -o \
	        -path ./.paper-build -o \
	        -path ./implementation/.stack-work \
	      \) -prune -o \
	      \( \
		-name '*.hs' -o \
		-name '*.sh' -o \
		-name '*.v' -o \
		-name '*.yml' -o \
		-name 'Dockerfile' -o \
		-name 'Makefile' \
	      \) -print \
	  )

formalization:
	rm -f CoqMakefile _CoqProjectFull
	echo '-R formalization Main' > _CoqProjectFull
	find formalization -type f -name '*.v' >> _CoqProjectFull
	coq_makefile -f _CoqProjectFull -o CoqMakefile
	make -f CoqMakefile
	rm -f CoqMakefile _CoqProjectFull

implementation:
	cd implementation && \
	  stack build --pedantic --install-ghc --allow-different-user && \
	  stack test --pedantic --install-ghc --allow-different-user

clean: clean-paper clean-formalization clean-implementation

clean-paper:
	rm -rf .paper-build main.pdf

clean-formalization:
	rm -f _CoqProjectFull CoqMakefile \
	  $(shell find . -type f \( \
	    -name '*.glob' -o \
	    -name '*.v.d' -o \
	    -name '*.vo' -o \
	    -name '*.vo.aux' \
	  \) -print)

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
