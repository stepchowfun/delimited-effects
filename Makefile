# Phony targets

.PHONY: \
  all test lint format clean \
  paper formalization implementation \
  test-implementation \
  lint-implementation \
  format-implementation \
  clean-paper clean-formalization clean-implementation \
  docker-deps docker-build

all: paper formalization implementation

test: test-implementation

lint: lint-implementation
	./scripts/general-lint.rb $(shell \
	  find . -type d \( \
	    -path ./.git -o \
	    -path ./.github -o \
	    -path ./.paper-build -o \
	    -path ./.stack -o \
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

formalization:
	rm -f Makefile.coq _CoqProjectFull
	echo '-R formalization Main' > _CoqProjectFull
	find formalization -type f -name '*.v' >> _CoqProjectFull
	coq_makefile -f _CoqProjectFull -o Makefile.coq || \
	  (rm -f Makefile.coq _CoqProjectFull; exit 1)
	make -f Makefile.coq || \
	  (rm -f Makefile.coq _CoqProjectFull; exit 1)
	rm -f Makefile.coq _CoqProjectFull

implementation:
	cd implementation && \
	  stack build --pedantic --install-ghc --allow-different-user

test-implementation: implementation
	cd implementation && \
	  stack test --pedantic --install-ghc --allow-different-user

lint-implementation:
	for file in $(shell \
	  find . -type d \( \
	    -path ./.git -o \
	    -path ./.github -o \
	    -path ./.paper-build -o \
	    -path ./implementation/.stack-work \
	  \) -prune -o \( \
	    -name '*.hs' \
	  \) -print \
	); do \
	  brittany "$$file" > "$$file.tmp"; \
	  (cmp "$$file.tmp" "$$file" && rm "$$file.tmp") || \
	    (rm "$$file.tmp" && false) || exit 1; \
	done

format-implementation:
	for file in $(shell \
	  find . -type d \( \
	    -path ./.git -o \
	    -path ./.github -o \
	    -path ./.paper-build -o \
	    -path ./implementation/.stack-work \
	  \) -prune -o \( \
	    -name '*.hs' \
	  \) -print \
	); do \
	  brittany --write-mode=inplace "$$file"; \
	done

clean-paper:
	rm -rf .paper-build main.pdf

clean-formalization:
	rm -f _CoqProjectFull Makefile.coq $(shell \
	  find . -type f \( \
	    -name '*.glob' -o \
	    -name '*.v.d' -o \
	    -name '*.vo' -o \
	    -name '*.vo.aux' \
	  \) -print \
	)

clean-implementation:
	rm -rf implementation/.stack-work

docker-deps:
	docker build \
	  -f scripts/Dockerfile \
	  -t stephanmisc/delimited-effects:deps \
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
	    stephanmisc/delimited-effects:deps \
	    bash -c ' \
	      chown -R user:user repo && \
	      cd repo && \
	      su user -s /bin/bash -l -c " \
	        cd repo && \
	        make clean && \
	        make && \
	        make test && \
	        make lint \
	      " && (test "$$DEPLOY" != true || ./scripts/deploy.rb) \
	    ' \
	)" && \
	docker cp . "$$CONTAINER:/home/user/repo" && \
	docker start --attach "$$CONTAINER"

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
