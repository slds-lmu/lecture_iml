TSLIDES = $(shell find . -maxdepth 1 -iname "slides-*.tex")
TPDFS = $(TSLIDES:%.tex=%.pdf)

all: delete $(TPDFS) pax texclean copy

$(TPDFS): %.pdf: %.tex
	latexmk -pdf $<

copy:
	cp -u *.pdf ../../slides-pdf
	cp -u *.pax ../../slides-pdf

pax:
	pdfannotextractor *.pdf

delete:
	rm -rf slides-*.pdf
	rm -rf slides-*.pax

texclean:
	rm -rf *.out
	rm -rf *.dvi
	rm -rf *.log
	rm -rf *.aux
	rm -rf *.bbl
	rm -rf *.blg
	rm -rf *.ind
	rm -rf *.idx
	rm -rf *.ilg
	rm -rf *.lof
	rm -rf *.lot
	rm -rf *.toc
	rm -rf *.nav
	rm -rf *.snm
	rm -rf *.vrb
	rm -rf *.fls
	rm -rf *.bcf
	rm -rf *.run.xml
	rm -rf *.fdb_latexmk
	rm -rf *.synctex.gz
	rm -rf *-concordance.tex
