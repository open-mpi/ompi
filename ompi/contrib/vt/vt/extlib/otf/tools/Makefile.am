TMP1 = otfaux otfconfig otfdump otfinfo otfmerge otfprofile otfshrink

if AMHAVEVTF3
TMP2 = otf2vtf vtf2otf
else
TMP2= 
endif

if AMHAVEZLIB
TMP3 = otfcompress
else
TMP3 = 
endif

if AMHAVEMPI
TMP4 = otfmerge-mpi otfprofile-mpi
else
TMP4 =
endif

SUBDIRS = $(TMP1) $(TMP2) $(TMP3) $(TMP4)
