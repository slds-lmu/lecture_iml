EX = $(shell find . -maxdepth 1 -type f \( -iname "ex_*.tex" -o -iname "hw_*.tex" -o -iname "sol_*.tex" -o -iname "collection_*.tex" -o -iname "ic_*.tex" \))
EXS = $(EX:%.tex=%.pdf)

all: texclean $(EXS) texclean copy

$(EXS): %.pdf: %.tex
	latexmk -pdf $<
	
copy: 
	find . -maxdepth 1 -type f \( -iname "ex_*.pdf" -o -iname "sol_*.pdf" -o -iname "collection_*.pdf" -o -iname "ic_*.pdf" -o -iname "hw_*.pdf" \) -exec cp {}  ../../exercises-pdf \;
	
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
	rm -rf *.synctex.gz
	rm -rf *-concordance.tex
	rm -rf *.fls
	rm -rf *.fdb_latexmk