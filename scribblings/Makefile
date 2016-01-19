# define default document pathname here
# override on command line with 'make x=newdoc'
x = user-guide
latex = pdf-slatex

$(x).pdf: $(x).tex
	$(latex) $(x).tex

clean:
	rm -f *.div *.aux *.log .Z*.tex *.dvi

reallyclean:
	rm -f *.pdf *.ps
