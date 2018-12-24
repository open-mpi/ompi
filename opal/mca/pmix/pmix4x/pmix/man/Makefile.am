#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved.
# Copyright (c) 2013-2016 Intel, Inc. All rights reserved
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

if !PMIX_EMBEDDED_MODE

man_MANS = \
	man3/pmix_init.3 \
	man3/pmix_finalize.3 \
	man3/pmix_initialized.3 \
	man3/pmix_abort.3 \
	man3/pmix_put.3 \
	man3/pmix_commit.3 \
	man7/pmix.7 \
	man7/pmix_constants.7

EXTRA_DIST = $(man_MANS)

man3/pmix_init.3:
	$(top_srcdir)/contrib/md2nroff.pl --source=pmix_init.3.md;

man3/pmix_finalize.3:
	$(top_srcdir)/contrib/md2nroff.pl --source=pmix_finalize.3.md;

man3/pmix_initialized.3:
	$(top_srcdir)/contrib/md2nroff.pl --source=pmix_initialized.3.md;

man3/pmix_abort.3:
	$(top_srcdir)/contrib/md2nroff.pl --source=pmix_abort.3.md;

man3/pmix_put.3:
	$(top_srcdir)/contrib/md2nroff.pl --source=pmix_put.3.md;

man3/pmix_commit.3:
	$(top_srcdir)/contrib/md2nroff.pl --source=pmix_commit.3.md;

man7/pmix.7:
	$(top_srcdir)/contrib/md2nroff.pl --source=pmix.7.md;

man7/pmix_constants.7:
	$(top_srcdir)/contrib/md2nroff.pl --source=pmix_constants.7.md;

endif # !PMIX_EMBEDDED_MODE
