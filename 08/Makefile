NUM = 08

$(NUM).pdf: $(NUM).tex
	lualatex --shell-escape $(NUM)

open: $(NUM).pdf
	open $(NUM).pdf

clean:
	rm -f *.aux
	rm -f *.bcf
	rm -f *.hi
	rm -f *.log
	rm -f *.o
	rm -f *.out
	rm -f *.run.xml
	rm -f $(NUM).pdf
	rm -f actionSequence rgb
