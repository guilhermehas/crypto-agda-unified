AGDA_EXEC ?= agda
FIX_WHITESPACE ?= fix-whitespace
RTS_OPTIONS = --latex --latex-dir .
AGDA = $(AGDA_EXEC) $(RTS_OPTIONS)
DOC = main
PDF = ${DOC}.pdf
DOCTEX = ${DOC}.tex

LAGDA = ${DOC}.lagda.tex
SRC = ${DOC}.tex
LATEX ?= pdflatex

default: pdf

pdf: ${PDF}

${PDF}: ${LAGDA}
	${AGDA}   ${LAGDA}
	${LATEX}  ${DOCTEX}

install: pdf
	mkdir -p $(out)
	cp *.pdf $(out)

clean:
	rm -rf latex
	rm -f *.{log,aux,dvi,toc,ptb,snm,out,nav,fdb_latexmk,fls,sty,pdf}
	rm -f main.tex

distclean: clean
	rm -f *.agdai
	rm -f _region_.tex
	rm -rf .auctex-auto
	rm -f result
