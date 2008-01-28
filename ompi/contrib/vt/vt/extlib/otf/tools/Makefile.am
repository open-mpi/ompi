TMP1 = otfmerge otfaux otfconfig otfdump

if AMHAVEVTF3
TMP2 = vtf2otf otf2vtf 
else
TMP2= 
endif


if AMHAVEZLIB
TMP3 = otfcompress
else
TMP3 = 
endif

SUBDIRS = $(TMP1) $(TMP2) $(TMP3)
