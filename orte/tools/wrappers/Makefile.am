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

EXTRA_DIST = \
	ortecc.1 \
	ortec++.1

if WANT_INSTALL_HEADERS

nodist_pkgdata_DATA = \
	ortecc-wrapper-data.txt \
	ortec++-wrapper-data.txt

if OMPI_INSTALL_BINARIES

man_MANS = \
	ortecc.1 \
	ortec++.1

install-exec-hook:
	test -z "$(bindir)" || $(mkdir_p) "$(DESTDIR)$(bindir)"
	(cd $(DESTDIR)$(bindir); rm -f ortecc$(EXEEXT); $(LN_S) opal_wrapper ortecc)
	(cd $(DESTDIR)$(bindir); rm -f ortec++$(EXEEXT); $(LN_S) opal_wrapper ortec++)

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/ortecc$(EXEEXT) \
	$(DESTDIR)$(bindir)/ortec++$(EXEEXT)

endif # OMPI_INSTALL_BINARIES

endif # WANT_INSTALL_HEADERS

maintainer-clean-local:
	rm -f ortecc.1 ortec++.1

ortecc.1: $(top_srcdir)/opal/tools/wrappers/opal_wrapper.1
	rm -f ortecc.1
	sed -e 's/@COMMAND@/ortecc/g' -e 's/@PROJECT@/OpenRTE/g' -e 's/@PROJECT_SHORT@/ORTE/g' -e 's/@LANGUAGE@/C/g' < $(top_srcdir)/opal/tools/wrappers/opal_wrapper.1 > ortecc.1

ortec++.1: $(top_srcdir)/opal/tools/wrappers/opal_wrapper.1
	rm -f ortec++.1
	sed -e 's/@COMMAND@/ortec++/g' -e 's/@PROJECT@/OpenRTE/g' -e 's/@PROJECT_SHORT@/ORTE/g' -e 's/@LANGUAGE@/C++/g' < $(top_srcdir)/opal/tools/wrappers/opal_wrapper.1 > ortec++.1
