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

opal_wrapper_SOURCES = \
	opal_wrapper.c

opal_wrapper_LDADD = $(top_builddir)/opal/libopal.la
opal_wrapper_DEPENDENCIES = $(top_builddir)/opal/libopal.la

DEFS="-DEXEEXT=\"$(EXEEXT)\""

# Whether or not --enable-binaries was given to configure (useful for
# multi-lib installations).  If this was not given, we do not build /
# install anything in this directory.

if OMPI_INSTALL_BINARIES

nodist_pkgdata_DATA = \
	opalcc-wrapper-data.txt \
	opalc++-wrapper-data.txt

dist_pkgdata_DATA = \
	help-opal-wrapper.txt

bin_PROGRAMS = opal_wrapper

# List all man pages that will be installed

man_MANS = opalcc.1 opalc++.1
if CASE_SENSITIVE_FS
man_MANS += opalCC.1
endif

# Only include in the distribution tarball the man pages that are not
# generated (i.e., opalcc.1).

EXTRA_DIST = opalcc.1

# Rules to generate the other man pages

opalc++.1: $(srcdir)/opalcc.1
	cp -f $(srcdir)/opalcc.1 opalc++.1
if CASE_SENSITIVE_FS
opalCC.1: $(srcdir)/opalcc.1
	cp -f $(srcdir)/opalcc.1 opalCC.1
endif

if CASE_SENSITIVE_FS
install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f opalc++$(EXEEXT); $(LN_S) opal_wrapper opalc++)
	(cd $(DESTDIR)$(bindir); rm -f opalCC$(EXEEXT); $(LN_S) opal_wrapper opalCC)

	(cd $(DESTDIR)$(pkgdatadir); rm -f opalCC-wrapper-data.txt; $(LN_S) opalc++-wrapper-data.txt opalCC-wrapper-data.txt)

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/opalcc$(EXEEXT) \
	$(DESTDIR)$(bindir)/opalc++$(EXEEXT) \
	$(DESTDIR)$(bindir)/opalCC$(EXEEXT) \
	$(DESTDIR)$(pkgdatadir)/opalCC-wrapper-data.txt

else
install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f opalcc$(EXEEXT); $(LN_S) opal_wrapper opalcc)
	(cd $(DESTDIR)$(bindir); rm -f opalc++$(EXEEXT); $(LN_S) opal_wrapper opalc++)

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/opalcc$(EXEEXT) \
	$(DESTDIR)$(bindir)/opalc++$(EXEEXT)

endif

else

endif
