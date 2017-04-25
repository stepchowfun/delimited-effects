.PHONY: clean

main.pdf: main.tex
	mkdir -p "build"
	pdflatex -output-directory "build" main.tex
	while ( \
		grep -qi \
			'^LaTeX Warning: Label(s) may have changed' \
			"build/main.log" \
	) do \
		pdflatex -output-directory "build" main.tex; \
	done
	mv build/main.pdf .

clean:
	rm -rf build
