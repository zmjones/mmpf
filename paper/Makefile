TEXCMD := pdflatex -interaction=batchmode

RJwrapper.pdf: RJwrapper.tex
	$(TEXCMD) $<
	bibtex RJwrapper.aux
	$(TEXCMD) $<
	$(TEXCMD) $<

examples.Rout: examples.R
	R CMD BATCH --vanilla examples.R
