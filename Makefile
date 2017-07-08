# Phony targets

.PHONY: \
  all \
  paper lint formalization \
  clean clean-paper clean-formalization \
  docker-deps docker-build

all: paper lint formalization

paper: main.pdf

lint:
	OUTPUT="$$(lacheck main.tex)"; \
		if [ -n "$$OUTPUT" ]; \
			then echo "$$OUTPUT"; \
		exit 1; \
			else echo "No lacheck errors."; \
		fi
	OUTPUT="$$(chktex main.tex)"; \
		if [ -n "$$OUTPUT" ]; \
			then echo "$$OUTPUT"; \
		exit 1; \
			else echo "No ChkTeX errors."; \
		fi
	./scripts/check-line-lengths.sh \
	  .travis.yml \
	  Makefile \
	  docker/* \
	  formalization/*.v \
	  scripts/*

formalization: \
  $(addprefix formalization/, syntax.vo helpers.vo judgments.vo subtyping.vo)

clean: clean-paper clean-formalization

clean-paper:
	rm -rf paper-build main.pdf

clean-formalization:
	rm -rf \
	  formalization/*.glob \
	  formalization/*.vo \
	  formalization/.*.vo.aux \
	  paper-build

docker-deps:
	docker build \
	  -f docker/Dockerfile-deps \
	  -t stephanmisc/delimited-effects:deps \
	  .

docker-build:
	docker build \
	  -f docker/Dockerfile-build \
	  -t stephanmisc/delimited-effects:build \
	  .

# The paper

main.pdf: main.tex
	mkdir -p "paper-build"
	pdflatex -interaction=nonstopmode -output-directory "paper-build" main.tex
	while ( \
		grep -qi \
			'^LaTeX Warning: Label(s) may have changed' \
			"paper-build/main.log" \
	) do \
		pdflatex -interaction=nonstopmode -output-directory "paper-build" main.tex; \
	done
	mv paper-build/main.pdf .

# The formalization

formalization/syntax.vo: formalization/syntax.v
	COQPATH="$$(pwd)/formalization" coqc formalization/syntax.v

formalization/helpers.vo: \
  $(addprefix formalization/, syntax.vo helpers.v)
	COQPATH="$$(pwd)/formalization" coqc formalization/helpers.v

formalization/judgments.vo: \
  $(addprefix formalization/, syntax.vo helpers.vo judgments.v)
	COQPATH="$$(pwd)/formalization" coqc formalization/judgments.v

formalization/subtyping.vo: \
  $(addprefix formalization/, syntax.vo judgments.vo subtyping.v)
	COQPATH="$$(pwd)/formalization" coqc formalization/subtyping.v
