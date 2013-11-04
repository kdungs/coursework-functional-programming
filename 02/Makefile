NUM = 02

$(NUM).pdf: $(NUM).tex
	lualatex --shell-escape $(NUM)

collatz: collatz.hs
	ghc --make collatz.hs

open: $(NUM).pdf
	open $(NUM).pdf

clean:
	rm -f *.aux
	rm -f *.bcf
	rm -f *.log
	rm -f *.out
	rm -f *.run.xml
	rm -f $(NUM).pdf
	rm -f *.hi
	rm -f *.o
	rm -f collatz
