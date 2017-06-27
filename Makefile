# Phony targets

.PHONY: all paper lint formalization clean clean-paper clean-formalization docker-deps docker-build

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

formalization: $(addprefix formalization/, syntax.vo)

clean: clean-paper clean-formalization

clean-paper:
	rm -rf paper-build main.pdf

clean-formalization:
	rm -rf paper-build formalization/*.vo formalization/.*.vo.aux formalization/*.glob

docker-deps:
	docker build -f docker/Dockerfile-deps -t stephanmisc/delimited-effects:deps .

docker-build:
	docker build -f docker/Dockerfile-build -t stephanmisc/delimited-effects:build .

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
