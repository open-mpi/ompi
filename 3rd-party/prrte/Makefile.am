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
# Copyright (c) 2006-2022 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2015 Los Alamos National Security, Inc.  All rights reserved.
# Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
# Copyright (c) 2017-2018 Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

SUBDIRS = config contrib src include docs
EXTRA_DIST = README.md VERSION LICENSE autogen.pl

include examples/Makefile.include

# Check for common symbols.  Use a "-hook" to increase the odds that a
# developer will see it at the end of their installation process.
install-exec-hook:
	-@if test -d "$(top_srcdir)/.git"; then \
	    $(top_srcdir)/config/find_common_syms \
	        --brief \
	        --top_builddir=$(top_builddir) \
	        --top_srcdir=$(top_srcdir) \
	        --objext=$(OBJEXT); \
	fi

ACLOCAL_AMFLAGS = -I config
