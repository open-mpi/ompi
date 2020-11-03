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
# Copyright (c) 2008-2020 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

pmix_config_files = @PMIX_DEFAULT_MCA_PARAM_CONF@
pmix_file_from_platform = @PMIX_PARAM_FROM_PLATFORM@

EXTRA_DIST = $(pmix_config_files)

# Steal a little trickery from a generated Makefile to only install
# files if they do not already exist at the target.  Be sure to read
# thread starting here
# (https://www.open-mpi.org/community/lists/devel/2008/06/4080.php) for
# details why the mkdir is in install-data-local.

install-data-local:
	$(MKDIR_P) $(DESTDIR)$(sysconfdir)
	@ p="$(pmix_config_files)"; \
        if test "$(pmix_file_from_platform)" = "yes"; then \
	    	if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	    	echo " $(INSTALL_DATA) $$d$$p $(DESTDIR)$(sysconfdir)/pmix-mca-params.conf"; \
		    $(INSTALL_DATA) $$d$$p $(DESTDIR)$(sysconfdir)/pmix-mca-params.conf; \
        else \
            for file in $$p; do \
	        	if test -f $(DESTDIR)$(sysconfdir)/pmix-mca-params.conf; then \
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
		    done \
	    fi;


# Only remove if exactly the same as what in our tree
# NOTE TO READER: Bourne shell if ... fi evaluates the body if
#    the return of the evaluted command is 0 (as opposed to non-zero
#    as used by everyone else)
uninstall-local:
	@ p="$(pmix_config_files)"; \
	for file in $$p; do \
	  if test -f "$(DESTDIR)$(sysconfdir)/$$file"; then \
	  	if test -f "$$file"; then d=; else d="$(srcdir)/"; fi; \
	    if diff "$(DESTDIR)$(sysconfdir)/$$file" "$$d$$file" > /dev/null 2>&1 ; then \
	      echo "rm -f $(DESTDIR)$(sysconfdir)/$$file" ; \
	      rm -f "$(DESTDIR)$(sysconfdir)/$$file" ; \
	    fi ; \
	  fi ; \
	done
