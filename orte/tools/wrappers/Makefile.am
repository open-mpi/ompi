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
# Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

#man_pages = ortecc.1

if !OPAL_WANT_SCRIPT_WRAPPER_COMPILERS
if OMPI_INSTALL_BINARIES

nodist_ompidata_DATA = \
	ortecc-wrapper-data.txt

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = orte.pc

#man_MANS = $(man_pages)

install-exec-hook-always:
	test -z "$(bindir)" || $(MKDIR_P) "$(DESTDIR)$(bindir)"
	(cd $(DESTDIR)$(bindir); rm -f ortecc$(EXEEXT); $(LN_S) opal_wrapper ortecc)

uninstall-local-always:
	rm -f $(DESTDIR)$(bindir)/ortecc$(EXEEXT)

install-exec-hook: install-exec-hook-always

install-data-hook:

uninstall-local: uninstall-local-always


endif # OMPI_INSTALL_BINARIES

endif # OPAL_WANT_SCRIPT_WRAPPER_COMPILERS

distclean-local:
#	rm -f $(man_pages)

#ortecc.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
#	rm -f ortecc.1
#	sed -e 's/#COMMAND#/ortecc/g' -e 's/#PROJECT#/OpenRTE/g' -e 's/#PROJECT_SHORT#/ORTE/g' -e 's/#LANGUAGE#/C/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > ortecc.1
