#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2006 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2014      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2019      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

DEFS="-DEXEEXT=\"$(EXEEXT)\""

real_man_pages = generic_wrapper.1 pmix_wrapper.1
EXTRA_DIST = $(real_man_pages:.1=.1in)

include $(top_srcdir)/Makefile.pmix-rules

if PMIX_INSTALL_BINARIES

bin_PROGRAMS = pmix_wrapper

nodist_man_MANS = pmix_wrapper.1

dist_pmixdata_DATA = help-pmix-wrapper.txt

# Only install the following for developer-level installs
if WANT_INSTALL_HEADERS

nodist_pmixdata_DATA = \
	pmixcc-wrapper-data.txt

nodist_man_MANS += pmixcc.1

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = pmix.pc

install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f pmixcc$(EXEEXT); $(LN_S) pmix_wrapper$(EXEECT) pmixcc$(EXEEXT))

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/pmixcc$(EXEEXT)

endif # WANT_INSTALL_HEADERS

endif # PMIX_INSTALL_BINARIES

pmix_wrapper_SOURCES = pmix_wrapper.c
pmix_wrapper_LDADD = $(top_builddir)/src/libpmix.la

# Ensure that the man pages are rebuilt if the pmix_config.h file
# changes; a "good enough" way to know if configure was run again (and
# therefore the release date or version may have changed)
$(real_man_pages): $(top_builddir)/src/include/pmix_config.h

pmixcc.1: generic_wrapper.1
	rm -f pmixcc.1
	sed -e 's/#COMMAND#/pmixcc/g' -e 's/#PROJECT#/PMIx/g' -e 's/#PROJECT_SHORT#/PMIx/g' -e 's/#LANGUAGE#/C/g' < $(top_builddir)/src/tools/wrapper/generic_wrapper.1 > pmixcc.1

distclean-local:
	rm -f $(real_man_pages) pmixcc.1
