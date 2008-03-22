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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

if OMPI_INSTALL_BINARIES

bin_PROGRAMS = orte-checkpoint

dist_man_MANS = orte-checkpoint.1 ompi-checkpoint.1

dist_pkgdata_DATA = help-orte-checkpoint.txt

install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f ompi-checkpoint$(EXEEXT); $(LN_S) orte-checkpoint$(EXEEXT) ompi-checkpoint$(EXEEXT))

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/ompi-checkpoint$(EXEEXT)

endif # OMPI_INSTALL_BINARIES

orte_checkpoint_SOURCES = orte-checkpoint.c
orte_checkpoint_LDADD = $(top_builddir)/orte/libopen-rte.la

ompi-checkpoint.1: $(top_srcdir)/orte/tools/orte-checkpoint/orte-checkpoint.1
	cp -f $(top_srcdir)/orte/tools/orte-checkpoint/orte-checkpoint.1 ompi-checkpoint.1

distclean-local:
	rm -f ompi-checkpoint.1
