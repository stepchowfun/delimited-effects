SHELL = bash

.PHONY: \
  all test lint format clean \
  paper \
    lint-general \
    lint-paper \
    clean-paper \
  formalization \
    lint-formalization \
    clean-formalization \
  implementation \
    test-implementation \
    lint-implementation \
    format-implementation \
    clean-implementation \
  docker-deps docker-build

all: paper formalization implementation

test: test-implementation

lint: lint-general lint-paper lint-formalization lint-implementation

lint-general:
	./scripts/lint-general.rb $(shell \
	  find . -type d \( \
	    -path ./.git -o \
	    -path ./.paper-build -o \
	    -path ./implementation/.stack-work \
	  \) -prune -o \( \
	    -name '*.hs' -o \
	    -name '*.rb' -o \
	    -name '*.sh' -o \
	    -name '*.v' -o \
	    -name '*.yml' -o \
	    -name 'Dockerfile' -o \
	    -name 'Makefile' \
	  \) -print \
	)

format: format-implementation

clean: clean-paper clean-formalization clean-implementation

paper: main.pdf

lint-paper:
	./scripts/lint-tex.rb $(shell \
	  find . -type d \( \
	    -path ./.git -o \
	    -path ./.paper-build -o \
	    -path ./implementation/.stack-work \
	  \) -prune -o -name '*.tex' -print \
	)

clean-paper:
	rm -rf .paper-build main.pdf

formalization:
	rm -f .coqdeps.d Makefile.coq Makefile.coq.conf _CoqProjectFull
	echo '-R formalization Main' > _CoqProjectFull
	find formalization -type f -name '*.v' >> _CoqProjectFull
	coq_makefile -f _CoqProjectFull -o Makefile.coq || \
	  (rm -f .coqdeps.d Makefile.coq Makefile.coq.conf _CoqProjectFull; exit 1)
	make -f Makefile.coq || \
	  (rm -f .coqdeps.d Makefile.coq Makefile.coq.conf _CoqProjectFull; exit 1)
	rm -f .coqdeps.d Makefile.coq Makefile.coq.conf _CoqProjectFull

lint-formalization: formalization
	./scripts/lint-imports.rb \
	  '^\s*Require ' \
	  'coqc -R formalization Main ?' \
	  $(shell \
	    find . -type d \( \
	      -path ./.git -o \
	      -path ./.paper-build -o \
	      -path ./implementation/.stack-work \
	    \) -prune -o \( \
	      -name '*.v' \
	    \) -print \
	  )
	./scripts/lint-imports.rb \
	  '^\s*Import ' \
	  'coqc -R formalization Main ?' \
	  $(shell \
	    find . -type d \( \
	      -path ./.git -o \
	      -path ./.paper-build -o \
	      -path ./implementation/.stack-work \
	    \) -prune -o \( \
	      -name '*.v' \
	    \) -print \
	  )
	if grep \
	  --recursive \
	  --line-number \
	  --include '*.v' \
	  Admitted \
	  formalization; \
	then \
	  echo "Error: 'Admitted' found in proofs." 1>&2; \
	  exit 1; \
	fi

clean-formalization:
	rm -f _CoqProjectFull Makefile.coq $(shell \
	  find . -type d \( \
	    -path ./.git -o \
	    -path ./.paper-build -o \
	    -path ./implementation/.stack-work \
	  \) -prune -o \( \
	    -name '*.aux' -o \
	    -name '*.glob' -o \
	    -name '*.vo' -o \
	    -name '.coqdeps.d' -o \
	    -name 'Makefile.coq' -o \
	    -name 'Makefile.coq.conf' -o \
	    -name '_CoqProjectFull' \
	  \) -print \
	)

implementation:
	(cd implementation && \
	  set -o pipefail && \
	  stack build --pedantic --install-ghc --allow-different-user 2>&1 | \
	    tee ../build-log.txt) || \
	  (rm -f build-log.txt && false) || \
	  exit 1
	(! grep conflict build-log.txt) || \
	  (rm -f build-log.txt && false) || \
	  exit 1
	rm -f build-log.txt

test-implementation: implementation
	cd implementation && \
	  stack test --pedantic --install-ghc --allow-different-user && \
	  stack exec implementation-exe examples/bools.g && \
	  stack exec implementation-exe examples/church-lists.g && \
	  stack exec implementation-exe examples/ints.g && \
	  stack exec implementation-exe examples/native-lists.g

lint-implementation:
	hlint implementation
	for file in $(shell \
	  find . -type d \( \
	    -path ./.git -o \
	    -path ./.paper-build -o \
	    -path ./implementation/.stack-work \
	  \) -prune -o \( \
	    -name '*.hs' \
	  \) -print \
	); do \
	  cat "$$file" | hindent > "$$file.tmp"; \
	  (cmp "$$file.tmp" "$$file" && rm "$$file.tmp") || \
	    (rm "$$file.tmp" && false) || exit 1; \
	done

format-implementation:
	for file in $(shell \
	  find . -type d \( \
	    -path ./.git -o \
	    -path ./.paper-build -o \
	    -path ./implementation/.stack-work \
	  \) -prune -o \( \
	    -name '*.hs' \
	  \) -print \
	); do \
	  cat "$$file" | hindent > "$$file.tmp"; \
	  (cmp --quiet "$$file.tmp" "$$file" && rm "$$file.tmp") || \
	    mv "$$file.tmp" "$$file"; \
	done

clean-implementation:
	rm -rf implementation/.stack-work

docker-deps:
	docker build \
	  -f scripts/Dockerfile \
	  -t stephanmisc/effect-classes:deps \
	  .

docker-build:
	CONTAINER="$$( \
	  docker create \
	    --env "AWS_ACCESS_KEY_ID=$$AWS_ACCESS_KEY_ID" \
	    --env "AWS_DEFAULT_REGION=$$AWS_DEFAULT_REGION" \
	    --env "AWS_SECRET_ACCESS_KEY=$$AWS_SECRET_ACCESS_KEY" \
	    --env "DEPLOY=$$DEPLOY" \
	    --env "GITHUB_TOKEN=$$GITHUB_TOKEN" \
	    --env "TRAVIS_BRANCH=$$TRAVIS_BRANCH" \
	    --env "TRAVIS_PULL_REQUEST=$$TRAVIS_PULL_REQUEST" \
	    --env "TRAVIS_PULL_REQUEST_BRANCH=$$TRAVIS_PULL_REQUEST_BRANCH" \
	    --env "TRAVIS_REPO_SLUG=$$TRAVIS_REPO_SLUG" \
	    --rm \
	    --user=root \
	    stephanmisc/effect-classes:deps \
	    bash -c ' \
	      chown -R user:user repo && \
	      cd repo && \
	      su user -s /bin/bash -l -c " \
	        cd repo && make clean && make all test lint USEACM=true \
	      " && (test "$$DEPLOY" != true || ./scripts/deploy.rb) \
	    ' \
	)" && \
	docker cp . "$$CONTAINER:/home/user/repo" && \
	docker start --attach "$$CONTAINER"

main.pdf: $(shell find paper -name '*.tex' -print -o -name '*.bib' -print)
	make clean-paper
	test "$$USEACM" = true && \
	  sed -i '' -e 's/^% \\useacmtrue/\\useacmtrue/g' paper/main.tex || true
	test "$$USEACM" = false && \
	  sed -i '' -e 's/^\\useacmtrue/% \\useacmtrue/g' paper/main.tex || true
	mkdir -p ".paper-build"
	pdflatex \
	  -interaction=nonstopmode \
	  -output-directory ".paper-build" \
	  paper/main.tex
	while ( \
	  grep -qi '^LaTeX Warning: Label(s) may have changed.' \
	    '.paper-build/main.log' || \
	  grep -qi '^Package natbib Warning: Citation(s) may have changed.' \
	    '.paper-build/main.log' \
	) do \
	  ( \
	    cd .paper-build && \
	    BIBINPUTS=../paper bibtex main.aux \
	  ); \
	  pdflatex \
	    -interaction=nonstopmode \
	    -output-directory ".paper-build" \
	    paper/main.tex; \
	done
	mv .paper-build/main.pdf .
