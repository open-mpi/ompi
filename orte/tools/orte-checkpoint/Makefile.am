#
# Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/Makefile.man-page-rules

# Bug in AM 1.10.1: need to list man pages because dist_man_MANS is
# not picked up inside of an AM_CONDITIONAL
man_pages = orte-checkpoint.1 ompi-checkpoint.1
EXTRA_DIST = $(man_pages) orte-checkpoint.1in

if !ORTE_DISABLE_FULL_SUPPORT
if WANT_FT
if OMPI_INSTALL_BINARIES

bin_PROGRAMS = orte-checkpoint

man_MANS = $(man_pages)

dist_pkgdata_DATA = help-orte-checkpoint.txt

install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f ompi-checkpoint$(EXEEXT); $(LN_S) orte-checkpoint$(EXEEXT) ompi-checkpoint$(EXEEXT))

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/ompi-checkpoint$(EXEEXT)

endif # OMPI_INSTALL_BINARIES

orte_checkpoint_SOURCES = orte-checkpoint.c
orte_checkpoint_LDADD = $(top_builddir)/orte/libopen-rte.la

endif # WANT_FT
endif # !ORTE_DISABLE_FULL_SUPPORT

ompi-checkpoint.1: $(top_srcdir)/orte/tools/orte-checkpoint/orte-checkpoint.1
	cp -f $(top_srcdir)/orte/tools/orte-checkpoint/orte-checkpoint.1 ompi-checkpoint.1

maintainer-clean-local:
	rm -f $(man_pages)
