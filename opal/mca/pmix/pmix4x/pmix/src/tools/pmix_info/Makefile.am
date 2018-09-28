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
# Copyright (c) 2008-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2017-2018 Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CFLAGS = \
            -DPMIX_CONFIGURE_USER="\"@PMIX_CONFIGURE_USER@\"" \
            -DPMIX_CONFIGURE_HOST="\"@PMIX_CONFIGURE_HOST@\"" \
            -DPMIX_CONFIGURE_DATE="\"@PMIX_CONFIGURE_DATE@\"" \
            -DPMIX_BUILD_USER="\"$$USER\"" \
            -DPMIX_BUILD_HOST="\"`(hostname || uname -n) 2> /dev/null | sed 1q`\"" \
            -DPMIX_BUILD_DATE="\"`date`\"" \
            -DPMIX_BUILD_CFLAGS="\"@CFLAGS@\"" \
            -DPMIX_BUILD_CPPFLAGS="\"@CPPFLAGS@\"" \
            -DPMIX_BUILD_LDFLAGS="\"@LDFLAGS@\"" \
            -DPMIX_BUILD_LIBS="\"@LIBS@\"" \
            -DPMIX_CC_ABSOLUTE="\"@PMIX_CC_ABSOLUTE@\"" \
            -DPMIX_GREEK_VERSION="\"@PMIX_GREEK_VERSION@\"" \
            -DPMIX_REPO_REV="\"@PMIX_REPO_REV@\"" \
            -DPMIX_RELEASE_DATE="\"@PMIX_RELEASE_DATE@\""

if PMIX_INSTALL_BINARIES

bin_PROGRAMS = pmix_info

dist_pmixdata_DATA = help-pmix-info.txt

endif # PMIX_INSTALL_BINARIES

pmix_info_SOURCES =  \
    pinfo.h  \
    support.h  \
    pmix_info.c  \
    support.c

pmix_info_LDADD = \
    $(PMIX_EXTRA_LTLIB) \
    $(top_builddir)/src/libpmix.la
