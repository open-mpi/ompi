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



opal_config_files = \
		openmpi-mca-params.conf

EXTRA_DIST = $(opal_config_files)

# This has to be here, even though it's empty, so that AM thinks that
# "something" will happen here (details fuzzy, but we remember that this
# *needs* to be here -- you have been warned).

sysconf_DATA = 

# Steal a little trickery from a generated Makefile to only install
# files if they do not already exist at the target.

install-data-local:
	@ p="$(opal_config_files)"; \
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

# Only remove if exactly the same as what in our tree
# NOTE TO READER: Bourne shell if ... fi evaluates the body if 
#    the return of the evaluted command is 0 (as opposed to non-zero
#    as used by everyone else)
uninstall-local:
	@ p="$(opal_config_files)"; \
	for file in $$p; do \
	  if test -f "$(DESTDIR)$(sysconfdir)/$$file"; then \
	  	if test -f "$$file"; then d=; else d="$(srcdir)/"; fi; \
	    if diff "$(DESTDIR)$(sysconfdir)/$$file" "$$d$$file" > /dev/null 2>&1 ; then \
	      echo "rm -f $(DESTDIR)$(sysconfdir)/$$file" ; \
	      rm -f "$(DESTDIR)$(sysconfdir)/$$file" ; \
	    fi ; \
	  fi ; \
	done
