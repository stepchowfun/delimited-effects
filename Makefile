.PHONY: lint clean

main.pdf: main.tex
	mkdir -p "build"
	pdflatex -interaction=nonstopmode -output-directory "build" main.tex
	while ( \
		grep -qi \
			'^LaTeX Warning: Label(s) may have changed' \
			"build/main.log" \
	) do \
		pdflatex -interaction=nonstopmode -output-directory "build" main.tex; \
	done
	mv build/main.pdf .

certify:
	coqc formalization/definitions.v

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

clean:
	rm -rf build main.pdf formalization/*.vo formalization/.*.vo.aux formalization/*.glob
