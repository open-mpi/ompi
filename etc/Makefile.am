#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

EXTRA_DIST = \
		openmpi-default-hostfile \
		openmpi-mca-params.conf

sysconf_DATA = 

# Steal a little trickery from a generated Makefile to only install
# lam-bhost.def if one does not already exist at the target.

install-data-local:
	@ p="openmpi-mca-params.conf openmpi-default-hostfile"; \
	for file in $$p; do \
	  if test -f $(DESTDIR)$(sysconfdir)/$$file; then \
	    echo "******************************* WARNING ************************************"; \
	    echo "*** Not installing new $$file over existing file in:"; \
	    echo "***   $(DESTDIR)$(sysconfdir)/$$file"; \
	    echo "******************************* WARNING ************************************"; \
	  else \
	    if test -f "$$file"; then d=; else d="$(srcdir)/"; fi; \
	    f="`echo $$file | sed -e 's|^.*/||'`"; \
	    echo " $(INSTALL_DATA) $$d$$file $(DESTDIR)$(sysconfdir)/$$f"; \
	    $(INSTALL_DATA) $$d$$file $(DESTDIR)$(sysconfdir)/$$f; \
	  fi; \
	done
	@ if test ! -r $(DESTDIR)$(sysconfdir)/hosts && test ! -h $(DESTDIR)$(sysconfdir)/hosts ; then \
            echo "Creating hosts symlink" ; \
	    ( cd $(DESTDIR)$(sysconfdir) ; ln -s openmpi-default-hostfile hosts ) \
          fi
