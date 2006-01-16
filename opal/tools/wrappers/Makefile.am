#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

nodist_pkgdata_DATA = \
	help-opal-wrapper.txt \
	opalcc-wrapper-data.txt \
	opalc++-wrapper-data.txt

AM_CPPFLAGS = \
            -DOPAL_LIBDIR="\"$(libdir)\"" \
            -DOPAL_INCDIR="\"$(includedir)\"" \
            -DOPAL_PKGDATADIR="\"$(pkgdatadir)\""

bin_PROGRAMS = opal_wrapper

opal_wrapper_SOURCES = \
	opal_wrapper.h \
	opal_wrapper.c

opal_wrapper_LDADD = $(top_builddir)/opal/libopal.la
opal_wrapper_DEPENDENCIES = $(top_builddir)/opal/libopal.la

if CASE_SENSITIVE_FS
install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f opalcc$(EXEEXT); $(LN_S) opal_wrapper opalcc)
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
