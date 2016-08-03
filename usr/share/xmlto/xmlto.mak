# Makefile rules for using xmlto

%.pdf %.dvi %.ps %.epub %.txt %.fo: %.xml
	xmlto $(XMLTOFLAGS) $(subst .,,$(suffix $@)) $<

%.pdf %.dvi %.ps: %.fo
	xmlto $(XMLTOFLAGS) $(subst .,,$(suffix $@)) $<

%.html: %.xml
	xmlto $(XMLTOFLAGS) html-nochunks $<

%.xhtml: %.xml
	xmlto $(XMLTOFLAGS) xhtml-nochunks $<
