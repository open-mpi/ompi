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
# Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# Copyright (c) 2014      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

if !OPAL_WANT_SCRIPT_WRAPPER_COMPILERS
if OMPI_INSTALL_BINARIES

nodist_ompidata_DATA = \
	ortecc-wrapper-data.txt

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = orte.pc

install-exec-hook:
	test -z "$(bindir)" || $(MKDIR_P) "$(DESTDIR)$(bindir)"
	(cd $(DESTDIR)$(bindir); rm -f ortecc$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) ortecc$(EXEEXT))

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/ortecc$(EXEEXT)

endif # OMPI_INSTALL_BINARIES

endif # OPAL_WANT_SCRIPT_WRAPPER_COMPILERS
