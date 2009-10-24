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
# Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

man_pages = ortecc.1 ortec++.1

if !ORTE_DISABLE_FULL_SUPPORT
if OPAL_WANT_SCRIPT_WRAPPER_COMPILERS

bin_SCRIPTS = orte_wrapper_script
CLEANFILES += $(bin_SCRIPTS)

install-exec-hook:
	test -z "$(bindir)" || $(mkdir_p) "$(DESTDIR)$(bindir)"
	(cd $(DESTDIR)$(bindir); rm -f ortecc; $(LN_S) orte_wrapper_script ortecc)
	(cd $(DESTDIR)$(bindir); rm -f ortec++; $(LN_S) orte_wrapper_script ortec++)

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/ortecc \
	$(DESTDIR)$(bindir)/ortec++

else # OPAL_WANT_SCRIPT_WRAPPER_COMPILERS

if WANT_INSTALL_HEADERS

nodist_pkgdata_DATA = \
	ortecc-wrapper-data.txt \
	ortec++-wrapper-data.txt

if OMPI_INSTALL_BINARIES

man_MANS = $(man_pages)

install-exec-hook:
	test -z "$(bindir)" || $(mkdir_p) "$(DESTDIR)$(bindir)"
	(cd $(DESTDIR)$(bindir); rm -f ortecc$(EXEEXT); $(LN_S) opal_wrapper ortecc)
	(cd $(DESTDIR)$(bindir); rm -f ortec++$(EXEEXT); $(LN_S) opal_wrapper ortec++)

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/ortecc$(EXEEXT) \
	$(DESTDIR)$(bindir)/ortec++$(EXEEXT)

endif # OMPI_INSTALL_BINARIES

endif # WANT_INSTALL_HEADERS
endif # OPAL_WANT_SCRIPT_WRAPPER_COMPILERS

distclean-local:
	rm -f $(man_pages)

ortecc.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f ortecc.1
	sed -e 's/#COMMAND#/ortecc/g' -e 's/#PROJECT#/OpenRTE/g' -e 's/#PROJECT_SHORT#/ORTE/g' -e 's/#LANGUAGE#/C/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > ortecc.1

ortec++.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f ortec++.1
	sed -e 's/#COMMAND#/ortec++/g' -e 's/#PROJECT#/OpenRTE/g' -e 's/#PROJECT_SHORT#/ORTE/g' -e 's/#LANGUAGE#/C++/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > ortec++.1

endif # !ORTE_DISABLE_FULL_SUPPORT
