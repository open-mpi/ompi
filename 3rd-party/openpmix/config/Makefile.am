# Copyright (c) 2013-2022 Intel, Inc.  All rights reserved.
# Copyright (c) 2016      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2006-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2010      Oracle and/or its affiliates.  All rights
#                         reserved.
# Copyright (c) 2016      Intel, Inc. All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2021      IBM Corporation.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This is a weird place for this, but config/ is the first directory that
# is entered, and the dist-hook runs at the end of the current directory,
# so this is the earliest we can run this test.
dist-hook:
	@if test -n "$(PMIX_MAKEDIST_DISABLE)" ; then \
		echo "#########################################################################"; \
		echo "#"; \
		echo "# make dist is disabled due to the following packages: $(PMIX_MAKEDIST_DISABLE)"; \
		echo "#"; \
		echo "#########################################################################"; \
		exit 1; \
	fi

EXTRA_DIST = \
    distscript.sh \
    getdate.sh \
    pmix_get_version.sh \
    pmix_mca_priority_sort.pl \
    mca_library_paths.txt \
    from-savannah/upstream-config.guess \
    from-savannah/upstream-config.sub \
    ltmain_pgi_tp.diff \
    ltmain_nag_pthread.diff
