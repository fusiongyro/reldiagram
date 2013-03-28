DOCBOOK_PATH=/usr/local/share/xsl/docbook

files=$(wildcard *.docbook)
sources=$(patsubst %.docbook,%.hs,$(files))

all: code book-html   # book-pdf

book-html: index.html

index.html: $(files)
	xsltproc --xinclude ${DOCBOOK_PATH}/html/docbook.xsl index.docbook > $@

code: $(sources)

%.hs: %.docbook
	xsltproc code.xsl $< > $@

clean:
	rm *.hs *.html


.PHONY: all code book-html clean
