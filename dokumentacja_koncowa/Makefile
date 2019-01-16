OUTPUT_DIR=_build

NAME = meum_dokumentacja

all: $(NAME).pdf

$(NAME).pdf: $(NAME).tex
	pdflatex $(NAME).tex
	pdflatex $(NAME).tex

show:
	evince $(NAME).pdf

clean:
	rm -r *.blg *.aux *.dvi *.bbl
