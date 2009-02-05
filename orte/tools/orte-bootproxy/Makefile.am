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
# Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/Makefile.man-page-rules

if !ORTE_DISABLE_FULL_SUPPORT
if OMPI_INSTALL_BINARIES

install-exec-hook:
	test -z "$(bindir)" || $(mkdir_p) "$(DESTDIR)$(bindir)"
	cp $(top_srcdir)/orte/tools/orte-bootproxy/orte-bootproxy.sh $(DESTDIR)$(bindir)
	chmod 755 $(DESTDIR)$(bindir)/orte-bootproxy.sh

endif # OMPI_INSTALL_BINARIES

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/orte-bootproxy.sh

endif # !ORTE_DISABLE_FULL_SUPPORT

distclean-local:
