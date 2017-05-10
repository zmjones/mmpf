TEXCMD := pdflatex -interaction=batchmode

jones.pdf: RJwrapper.tex jones.tex jones.R
	Rscript jones.R
	$(TEXCMD) $<
	bibtex RJwrapper.aux
	$(TEXCMD) $<
	$(TEXCMD) $<
	mv RJwrapper.pdf jones.pdf
