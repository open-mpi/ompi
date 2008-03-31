INCLUDES = \
	-I$(top_builddir)/otflib -I$(top_srcdir)/otflib \
	$(VTF3_INCLUDE_LINE)

if AMBUILDBINARIES
bin_PROGRAMS = \
	vtf2otf
endif

vtf2otf_LDADD = $(top_builddir)/otflib/libotf.la $(VTF3_LIB_LINE)
if ! AMDEPZLIB
vtf2otf_LDADD += $(ZLIB_LIB_LINE)
endif

vtf2otf_SOURCES = \
	Handler.h \
	Stack.h \
	Hash.h \
	vtf2otf.c \
	Handler.c \
	Stack.c \
	Hash.c

