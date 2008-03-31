INCLUDES = \
	-I$(top_builddir)/otflib -I$(top_srcdir)/otflib \
	$(VTF3_INCLUDE_LINE)

if AMBUILDBINARIES
bin_PROGRAMS = \
	otf2vtf
endif

otf2vtf_LDADD = $(top_builddir)/otflib/libotf.la $(VTF3_LIB_LINE)
if ! AMDEPZLIB
otf2vtf_LDADD += $(ZLIB_LIB_LINE)
endif

otf2vtf_SOURCES = \
	Handler.h \
	Treehash.h \
	otf2vtf.c \
	Handler.c \
	Treehash.c

