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

man_pages = mr+.1
EXTRA_DIST = $(man_pages:.1=.1in)

bin_PROGRAMS = 

mrplus_SOURCES = \
	mrplus.c

mrplus_LDADD = $(top_builddir)/orte/libopen-rte.la

# Ensure that the man pages are rebuilt if the opal_config.h file
# changes; a "good enough" way to know if configure was run again (and
# therefore the release date or version may have changed)
$(nodist_man_MANS): $(top_builddir)/opal/include/opal_config.h

dist_pkgdata_DATA = help-mr+.txt

if !ORTE_DISABLE_FULL_SUPPORT
if OMPI_INSTALL_BINARIES

# Only do this stuff if we want Hadoop support
if ORTE_WANT_HADOOP_SUPPORT

bin_PROGRAMS += mrplus

mr+: mrplus.c

nodist_man_MANS = $(man_pages)

install-exec-hook:
	test -z "$(bindir)" || $(MKDIR_P) "$(DESTDIR)$(bindir)"
	(cd $(DESTDIR)$(bindir); rm -f mr+; $(LN_S) mrplus mr+)


uninstall-local:
	rm -f $(DESTDIR)$(bindir)/mr+

endif 

endif # OMPI_INSTALL_BINARIES

endif # !ORTE_DISABLE_FULL_SUPPORT


distclean-local:
	rm -f $(man_pages)
