#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = config include etc src 
EXTRA_DIST = README INSTALL VERSION Doxyfile

dist-hook:
	csh config/distscript.csh $(PACKAGE)-$(OMPI_VERSION) $(OMPI_VERSION)
