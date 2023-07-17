# Copyright (C) 1989-2022 Free Software Foundation, Inc.
#      Written by James Clark (jjc@jclark.com)
#
# This file is part of groff.
#
# groff is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# groff is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# This is set up so you can do
#   make -f generate/Makefile
# in the parent directory of this directory.

# The following line is required for DOS/Windows systems, since
# this Makefile needs a Unixy shell to run.
SHELL = /bin/sh

# Directory containing AFM files.  Must not be current directory.
# Either long names (e.g., Times-Roman.afm) or short names (e.g.,
# timesr.afm) may be used. See the afmname script.
afmdir=/usr/local/lib/afm

# Write font descriptions to this directory.
outdir=$(srcdir)/..

# The symbol font which fits to 'Symbol.afm'.  For the creation of
# 'symbolsl.afm' we need the 'printafm' script from ghostscript.
symbolfont=/usr/local/lib/fonts/Symbol.pfb

PRINTAFM=printafm

srcdir=generate

SPECIALFONTS=S
DINGBATSFONTS=ZD ZDR
GREEKFONTS=SS
TEXTFONTS=AB ABI AI AR \
	BMB BMBI BMI BMR \
	CB CBI CI CR \
	HB HBI HI HR \
	HNB HNBI HNI HNR \
	NB NBI NI NR \
	PB PBI PI PR \
	TB TBI TI TR \
	ZCMI

FONTS=$(TEXTFONTS) $(SPECIALFONTS) $(GREEKFONTS) $(DINGBATSFONTS) EURO

DESC=$(srcdir)/../DESC
AFMTODIT=afmtodit -c -d$(DESC)
IFLAG=-i 50
RFLAG=-i 0 -m
NOLIGFLAG=-n
TEXTENC=$(srcdir)/../text.enc
EFLAG=-e $(TEXTENC)
TEXTMAP=$(srcdir)/text.map
AFMNAME=$(srcdir)/afmname

RM=rm -f

all: $(FONTS)

TR:
	$(AFMTODIT) $(EFLAG) $(RFLAG) \
	  `$(AFMNAME) $(afmdir)/Times-Roman.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

TB:
	$(AFMTODIT) $(EFLAG) $(RFLAG) \
	  `$(AFMNAME) $(afmdir)/Times-Bold.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

TI:
	$(AFMTODIT) $(EFLAG) $(IFLAG) -a 7 \
	  `$(AFMNAME) $(afmdir)/Times-Italic.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

TBI:
	$(AFMTODIT) $(EFLAG) $(IFLAG) \
	  `$(AFMNAME) $(afmdir)/Times-BoldItalic.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

HR:
	$(AFMTODIT) $(EFLAG) $(RFLAG) \
	  `$(AFMNAME) $(afmdir)/Helvetica.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

HB:
	$(AFMTODIT) $(EFLAG) $(RFLAG) \
	  `$(AFMNAME) $(afmdir)/Helvetica-Bold.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

HI:
	$(AFMTODIT) $(EFLAG) $(IFLAG) \
	  `$(AFMNAME) $(afmdir)/Helvetica-Oblique.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

HBI:
	$(AFMTODIT) $(EFLAG) $(IFLAG) \
	  `$(AFMNAME) $(afmdir)/Helvetica-BoldOblique.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

CR:
	$(AFMTODIT) $(NOLIGFLAG) $(EFLAG) $(RFLAG) \
	  `$(AFMNAME) $(afmdir)/Courier.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

CB:
	$(AFMTODIT) $(NOLIGFLAG) $(EFLAG) $(RFLAG) \
	  `$(AFMNAME) $(afmdir)/Courier-Bold.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

CI:
	$(AFMTODIT) $(NOLIGFLAG) $(EFLAG) $(IFLAG) \
	  `$(AFMNAME) $(afmdir)/Courier-Oblique.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

CBI:
	$(AFMTODIT) $(NOLIGFLAG) $(EFLAG) $(IFLAG) \
	  `$(AFMNAME) $(afmdir)/Courier-BoldOblique.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

PR:
	$(AFMTODIT) $(EFLAG) $(RFLAG) \
	  `$(AFMNAME) $(afmdir)/Palatino-Roman.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

PB:
	$(AFMTODIT) $(EFLAG) $(RFLAG) \
	  `$(AFMNAME) $(afmdir)/Palatino-Bold.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

PI:
	$(AFMTODIT) $(EFLAG) $(IFLAG) \
	  `$(AFMNAME) $(afmdir)/Palatino-Italic.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

PBI:
	$(AFMTODIT) $(EFLAG) $(IFLAG) \
	  `$(AFMNAME) $(afmdir)/Palatino-BoldItalic.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

NR:
	$(AFMTODIT) $(EFLAG) $(RFLAG) \
	  `$(AFMNAME) $(afmdir)/NewCenturySchlbk-Roman.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

NB:
	$(AFMTODIT) $(EFLAG) $(RFLAG) \
	  `$(AFMNAME) $(afmdir)/NewCenturySchlbk-Bold.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

NI:
	$(AFMTODIT) $(EFLAG) $(IFLAG) \
	  `$(AFMNAME) $(afmdir)/NewCenturySchlbk-Italic.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

NBI:
	$(AFMTODIT) $(EFLAG) $(IFLAG) \
	  `$(AFMNAME) $(afmdir)/NewCenturySchlbk-BoldItalic.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

BMR:
	$(AFMTODIT) $(EFLAG) $(RFLAG) \
	  `$(AFMNAME) $(afmdir)/Bookman-Light.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

BMB:
	$(AFMTODIT) $(EFLAG) $(RFLAG) \
	  `$(AFMNAME) $(afmdir)/Bookman-Demi.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

BMI:
	$(AFMTODIT) $(EFLAG) $(IFLAG) \
	  `$(AFMNAME) $(afmdir)/Bookman-LightItalic.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

BMBI:
	$(AFMTODIT) $(EFLAG) $(IFLAG) \
	  `$(AFMNAME) $(afmdir)/Bookman-DemiItalic.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

AR:
	$(AFMTODIT) $(EFLAG) $(RFLAG) \
	  `$(AFMNAME) $(afmdir)/AvantGarde-Book.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

AB:
	$(AFMTODIT) $(EFLAG) $(RFLAG) \
	  `$(AFMNAME) $(afmdir)/AvantGarde-Demi.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

AI:
	$(AFMTODIT) $(EFLAG) $(IFLAG) \
	  `$(AFMNAME) $(afmdir)/AvantGarde-BookOblique.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

ABI:
	$(AFMTODIT) $(EFLAG) $(IFLAG) \
	  `$(AFMNAME) $(afmdir)/AvantGarde-DemiOblique.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

HNR:
	$(AFMTODIT) $(EFLAG) $(RFLAG) \
	  `$(AFMNAME) $(afmdir)/Helvetica-Narrow.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

HNB:
	$(AFMTODIT) $(EFLAG) $(RFLAG) \
	  `$(AFMNAME) $(afmdir)/Helvetica-Narrow-Bold.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

HNI:
	$(AFMTODIT) $(EFLAG) $(IFLAG) \
	  `$(AFMNAME) $(afmdir)/Helvetica-Narrow-Oblique.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

HNBI:
	$(AFMTODIT) $(EFLAG) $(IFLAG) \
	  `$(AFMNAME) $(afmdir)/Helvetica-Narrow-BoldOblique.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

ZCMI:
	$(AFMTODIT) $(EFLAG) $(IFLAG) \
	  `$(AFMNAME) $(afmdir)/ZapfChancery-MediumItalic.afm` \
	  $(TEXTMAP) $(OUTDIR)/$@

ZD:
	$(AFMTODIT) -s $(RFLAG) \
	  `$(AFMNAME) $(afmdir)/ZapfDingbats.afm` \
	  $(srcdir)/dingbats.map $(OUTDIR)/$@

SS: $(OUTDIR)/symbolsl.afm
	$(AFMTODIT) -s -x $(IFLAG) \
	  $(OUTDIR)/symbolsl.afm $(srcdir)/slanted-symbol.map \
	  $(OUTDIR)/$@

S: symbol.afm
	$(AFMTODIT) -s $(RFLAG) \
	  symbol.afm $(srcdir)/symbol.map $(OUTDIR)/$@

ZDR: $(OUTDIR)/zapfdr.afm
	$(AFMTODIT) -s $(RFLAG) \
	  $(OUTDIR)/zapfdr.afm $(srcdir)/dingbats-reversed.map \
	  $(OUTDIR)/$@

# the map is just a dummy
EURO: freeeuro.afm
	$(AFMTODIT) $(RFLAG) \
	  freeeuro.afm $(srcdir)/symbol.map $(OUTDIR)/$@

freeeuro.afm freeeuro.pfa: $(srcdir)/freeeuro.sfd
	fontforge -script $(srcdir)/sfdtopfa.pe $(srcdir)/freeeuro.sfd \
	  && mv freeeuro.afm freeeuro.pfa $(OUTDIR)

symbol.afm: $(srcdir)/symbol.sed
	sed -f $(srcdir)/symbol.sed `$(AFMNAME) $(afmdir)/Symbol.afm` \
	  >$(OUTDIR)/$@

symbolsl.afm: $(srcdir)/symbolsl.awk $(srcdir)/../symbolsl.ps
	$(RM) Fontmap \
	  && echo "/Symbol-Slanted ($(srcdir)/../symbolsl.ps) ;" \
	    > Fontmap \
	  && echo "/Symbol         ($(symbolfont)) ;" >> Fontmap \
	  && $(PRINTAFM) Symbol-Slanted > tmp.afm \
	  && awk -f $(srcdir)/symbolsl.awk -v SYMAFM=`$(AFMNAME) \
	    $(afmdir)/Symbol.afm` tmp.afm >$(OUTDIR)/$@ \
	  && $(RM) Fontmap tmp.afm

zapfdr.afm: $(srcdir)/apfdr.sed
	sed -f $(srcdir)/zapfdr.sed \
	  `$(AFMNAME) $(afmdir)/ZapfDingbats.afm` >$(OUTDIR)/$@

ZD: $(srcdir)/dingbats.map
ZDR: $(srcdir)/dingbats-reversed.map
$(TEXTFONTS): $(TEXTMAP) $(TEXTENC)
$(SPECIALFONTS): $(srcdir)/symbol.map
$(GREEKFONTS): $(srcdir)/slanted-symbol.map
$(FONTS): $(DESC)

$(srcdir)/symbol.map: $(TEXTMAP) $(srcdir)/symbolchars
	printf \
	  '#\n# This is a list of all predefined groff symbols.\n#\n' \
	  > $(OUTDIR)/$@ \
	  && cat $(TEXTMAP) $(srcdir)/symbolchars >>$@

clean:
	cd $(OUTDIR) \
	  && $(RM) freeeuro.afm freeeuro.pfa symbol.afm \
	    $(srcdir)/symbol.map symbolsl.afm zapfdr.afm

realclean: clean
	cd $(OUTDIR) \
	  && $(RM) $(FONTS)

extraclean: realclean
	$(RM) core *~ "#*"

.PHONY: all clean realclean extraclean

# Local Variables:
# mode: makefile
# fill-column: 72
# End:
# vim: set textwidth=72:
