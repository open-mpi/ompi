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
# Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

if OMPI_INSTALL_BINARIES

nodist_pkgdata_DATA = \
	ortecc-wrapper-data.txt \
	ortec++-wrapper-data.txt

if CASE_SENSITIVE_FS
install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f ortecc$(EXEEXT); $(LN_S) opal_wrapper ortecc)
	(cd $(DESTDIR)$(bindir); rm -f ortec++$(EXEEXT); $(LN_S) opal_wrapper ortec++)
	(cd $(DESTDIR)$(bindir); rm -f orteCC$(EXEEXT); $(LN_S) opal_wrapper orteCC)
	(cd $(DESTDIR)$(pkgdatadir); rm -f orteCC-wrapper-data.txt; $(LN_S) ortec++-wrapper-data.txt orteCC-wrapper-data.txt)

install-data-hook:
	(cd $(DESTDIR)$(mandir)/man1; rm -f ortecc.1; $(LN_S) opalcc.1 ortecc.1)
	(cd $(DESTDIR)$(mandir)/man1; rm -f ortec++.1; $(LN_S) opalcc.1 ortec++.1)
	(cd $(DESTDIR)$(mandir)/man1; rm -f orteCC.1; $(LN_S) opalcc.1 orteCC.1)

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/ortecc$(EXEEXT) \
	$(DESTDIR)$(bindir)/ortec++$(EXEEXT) \
	$(DESTDIR)$(bindir)/orteCC$(EXEEXT) \
	$(DESTDIR)$(mandir)/man1/ortecc.1 \
	$(DESTDIR)$(mandir)/man1/ortec++.1 \
	$(DESTDIR)$(mandir)/man1/orteCC.1 \
	$(DESTDIR)$(pkgdatadir)/orteCC-wrapper-data.txt

else
install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f ortecc$(EXEEXT); $(LN_S) opal_wrapper ortecc)
	(cd $(DESTDIR)$(bindir); rm -f ortec++$(EXEEXT); $(LN_S) opal_wrapper ortec++)

install-data-hook:
	(cd $(DESTDIR)$(mandir)/man1; rm -f ortecc.1; $(LN_S) opalcc.1 ortecc.1)
	(cd $(DESTDIR)$(mandir)/man1; rm -f ortec++.1; $(LN_S) opalcc.1 ortec++.1)

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/ortecc$(EXEEXT) \
	$(DESTDIR)$(mandir)/man1/ortecc.1 \
	$(DESTDIR)$(mandir)/man1/ortec++.1 \
	$(DESTDIR)$(bindir)/ortec++$(EXEEXT)

endif

endif
