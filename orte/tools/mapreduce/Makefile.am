#
# Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
#                         reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/Makefile.man-page-rules

man_pages = mapreduce.1
EXTRA_DIST = $(man_pages:.1=.1in)

if !ORTE_DISABLE_FULL_SUPPORT
if OMPI_INSTALL_BINARIES

bin_PROGRAMS = mapreduce

nodist_man_MANS = $(man_pages)

# Ensure that the man pages are rebuilt if the opal_config.h file
# changes; a "good enough" way to know if configure was run again (and
# therefore the release date or version may have changed)
$(nodist_man_MANS): $(top_builddir)/opal/include/opal_config.h

dist_pkgdata_DATA = help-mapreduce.txt

endif # OMPI_INSTALL_BINARIES

mapreduce_SOURCES = \
        mapreduce.c

mapreduce_LDADD = $(top_builddir)/orte/libopen-rte.la

endif # !ORTE_DISABLE_FULL_SUPPORT

distclean-local:
	rm -f $(man_pages)
