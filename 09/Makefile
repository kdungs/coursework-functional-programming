NUM = 09

$(NUM).pdf: $(NUM).tex
	lualatex --shell-escape $(NUM)

open: $(NUM).pdf
	open $(NUM).pdf

clean:
	rm -f *.aux
	rm -f *.bcf
	rm -f *.log
	rm -f *.out
	rm -f *.run.xml
	rm -f $(NUM).pdf
