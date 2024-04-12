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
# Copyright (c) 2008-2023 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

prte_file_from_platform = @PRTE_PARAM_FROM_PLATFORM@
prte_mca_param_file = @PRTE_DEFAULT_MCA_PARAM_CONF@
prte_config_file = @PRTE_DEFAULT_CONFIG@

EXTRA_DIST = prte-mca-params.conf prte-default-hostfile prte.conf

# Note that prte-mca-params.conf and prte.conf are
# "special" -- we do not list them here.
prte_install_hook_files = prte-default-hostfile

# Steal a little trickery from a generated Makefile to only install
# files if they do not already exist at the target.  Be sure to read
# thread starting here
# (https://www.open-mpi.org/community/lists/devel/2008/06/4080.php) for
# details why the mkdir is in install-data-local.

install-data-local:
	@ $(MKDIR_P) $(DESTDIR)$(sysconfdir); \
	if test "$(prte_file_from_platform)" = "yes"; then \
	    $(INSTALL_DATA) $(prte_mca_param_file) $(DESTDIR)$(sysconfdir)/prte-mca-params.conf; \
	    $(INSTALL_DATA) $(prte_config_file) $(DESTDIR)$(sysconfdir)/prte.conf; \
	else \
	    if test -f $(DESTDIR)$(sysconfdir)/prte-mca-params.conf; then \
		echo "******************************* WARNING ************************************"; \
		echo "*** Not installing new prte-mca-params.conf over existing file in:"; \
		echo "***   $(DESTDIR)$(sysconfdir)/prte-mca-params.conf"; \
		echo "******************************* WARNING ************************************"; \
	    else \
		echo installing prte-mca-params.conf to $(DESTDIR)$(sysconfdir)/; \
		$(INSTALL_DATA) $(srcdir)/prte-mca-params.conf $(DESTDIR)$(sysconfdir)/; \
	    fi; \
	    if test -f $(DESTDIR)$(sysconfdir)/prte.conf; then \
		echo "******************************* WARNING ************************************"; \
		echo "*** Not installing new prte.conf over existing file in:"; \
		echo "***   $(DESTDIR)$(sysconfdir)/prte.conf"; \
		echo "******************************* WARNING ************************************"; \
	    else \
		echo installing prte.conf to $(DESTDIR)$(sysconfdir)/; \
		$(INSTALL_DATA) $(srcdir)/prte.conf $(DESTDIR)$(sysconfdir)/; \
	    fi; \
	fi
	@ for file in $(prte_install_hook_files); do \
	    if test -f $(DESTDIR)$(sysconfdir)/$$file; then \
		echo "******************************* WARNING ************************************"; \
		echo "*** Not installing new $$file over existing file in:"; \
		echo "***   $(DESTDIR)$(sysconfdir)/$$file"; \
		echo "******************************* WARNING ************************************"; \
	    else \
		echo installing $$file to $(DESTDIR)$(sysconfdir)/; \
		$(INSTALL_DATA) $(srcdir)/$$file $(DESTDIR)$(sysconfdir)/; \
	    fi; \
	done


# Only remove if exactly the same as what in our tree
# NOTE TO READER: Bourne shell if ... fi evaluates the body if
#    the return of the evaluated command is 0 (as opposed to non-zero
#    as used by everyone else)
uninstall-local:
	@ p="$(prte_install_hook_files)"; \
	for file in $$p; do \
	  if test -f "$(DESTDIR)$(sysconfdir)/$$file"; then \
	  	if test -f "$$file"; then d=; else d="$(srcdir)/"; fi; \
	    if diff "$(DESTDIR)$(sysconfdir)/$$file" "$$d$$file" > /dev/null 2>&1 ; then \
	      echo "rm -f $(DESTDIR)$(sysconfdir)/$$file" ; \
	      rm -f "$(DESTDIR)$(sysconfdir)/$$file" ; \
	    fi ; \
	  fi ; \
	done ; \
	if test "$(prte_file_from_platform)" = "yes"; then \
	    if test -f $(DESTDIR)$(sysconfdir)/prte-mca-params.conf; then \
		    if diff "$(DESTDIR)$(sysconfdir)/prte-mca-params.conf" "$(prte_mca_param_file)" > /dev/null 2>&1 ; then \
		      echo "rm -f $(DESTDIR)$(sysconfdir)/prte-mca-params.conf" ; \
		      rm -f "$(DESTDIR)$(sysconfdir)/prte-mca-params.conf" ; \
		    fi ; \
		fi ; \
	    if test -f $(DESTDIR)$(sysconfdir)/prte.conf; then \
		    if diff "$(DESTDIR)$(sysconfdir)/prte.conf" "$(prte_config_file)" > /dev/null 2>&1 ; then \
		      echo "rm -f $(DESTDIR)$(sysconfdir)/prte.conf" ; \
		      rm -f "$(DESTDIR)$(sysconfdir)/prte.conf" ; \
		    fi ; \
		fi ; \
	else \
	    if test -f $(DESTDIR)$(sysconfdir)/prte-mca-params.conf; then \
		    if diff "$(DESTDIR)$(sysconfdir)/prte-mca-params.conf" "$(srcdir)/prte-mca-params.conf"> /dev/null 2>&1 ; then \
		      echo "rm -f $(DESTDIR)$(sysconfdir)/prte-mca-params.conf" ; \
		      rm -f "$(DESTDIR)$(sysconfdir)/prte-mca-params.conf" ; \
		    fi ; \
		fi ; \
	    if test -f $(DESTDIR)$(sysconfdir)/prte.conf; then \
		    if diff "$(DESTDIR)$(sysconfdir)/prte.conf" "$(srcdir)/prte.conf"> /dev/null 2>&1 ; then \
		      echo "rm -f $(DESTDIR)$(sysconfdir)/prte.conf" ; \
		      rm -f "$(DESTDIR)$(sysconfdir)/prte.conf" ; \
		    fi ; \
		fi ; \
	fi ;
