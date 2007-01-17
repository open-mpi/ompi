#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2006 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

DEFS="-DEXEEXT=\"$(EXEEXT)\""

EXTRA_DIST = \
	opal_wrapper.1 \
	opalcc.1 \
	opalc++.1

if OMPI_INSTALL_BINARIES

# install even if we don't want the headers / opalcc, as we need it
# for the MPI layer
opal_wrapper_SOURCES = \
	opal_wrapper.c

opal_wrapper_LDADD = $(top_builddir)/opal/libopen-pal.la
opal_wrapper_DEPENDENCIES = $(top_builddir)/opal/libopen-pal.la

bin_PROGRAMS = opal_wrapper

dist_pkgdata_DATA = \
        help-opal-wrapper.txt

endif # OMPI_INSTALL_BINARIES

if WANT_INSTALL_HEADERS

nodist_pkgdata_DATA = \
	opalcc-wrapper-data.txt \
	opalc++-wrapper-data.txt

# Whether or not --enable-binaries was given to configure (useful for
# multi-lib installations).  If this was not given, we do not build /
# install the applications / man pages

if OMPI_INSTALL_BINARIES

# List all man pages that will be installed

man_MANS = \
	opalcc.1 \
	opalc++.1

install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f opalcc$(EXEEXT); $(LN_S) opal_wrapper opalcc)
	(cd $(DESTDIR)$(bindir); rm -f opalc++$(EXEEXT); $(LN_S) opal_wrapper opalc++)

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/opalcc$(EXEEXT) \
	$(DESTDIR)$(bindir)/opalc++$(EXEEXT)

endif # OPAL_INSTALL_BINARIES

endif # WANT_INSTALL_HEADERS

maintainer-clean-local:
	rm -f opalcc.1 opalc++.1

opalcc.1: opal_wrapper.1
	rm -f opalcc.1
	sed -e 's/@COMMAND@/opalcc/g' -e 's/@PROJECT@/Open PAL/g' -e 's/@PROJECT_SHORT@/OPAL/g' -e 's/@LANGUAGE@/C/g' < $(top_srcdir)/opal/tools/wrappers/opal_wrapper.1 > opalcc.1

opalc++.1: opal_wrapper.1
	rm -f opalc++.1
	sed -e 's/@COMMAND@/opalc++/g' -e 's/@PROJECT@/Open PAL/g' -e 's/@PROJECT_SHORT@/OPAL/g' -e 's/@LANGUAGE@/C++/g' < $(top_srcdir)/opal/tools/wrappers/opal_wrapper.1 > opalc++.1
