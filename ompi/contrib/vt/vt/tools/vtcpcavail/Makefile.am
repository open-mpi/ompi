if AMBUILDBINARIES
bin_PROGRAMS = vtcpcavail
endif

INCLUDES = $(CPCINCDIR)

vtcpcavail_SOURCES = \
	vt_cpcavail.c

vtcpcavail_LDADD = \
	$(top_builddir)/util/libutil.la \
	$(CPCLIBDIR) $(CPCLIB)

vtcpcavail_DEPENDENCIES = \
	$(top_builddir)/util/libutil.la

