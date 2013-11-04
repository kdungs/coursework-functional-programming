NUM = 03

$(NUM).pdf: $(NUM).tex
	lualatex --shell-escape $(NUM)

open: $(NUM).pdf
	open $(NUM).pdf

update: update.hs
	ghc --make update.hs

clean:
	rm -f *.aux
	rm -f *.bcf
	rm -f *.log
	rm -f *.out
	rm -f *.run.xml
	rm -f $(NUM).pdf
	rm -f *.hi
	rm -f *.o
	rm -f update
