#
# Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# Copyright (c) 2012-2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

headers = common_verbs_usnic.h

sources = common_verbs_usnic_fake.c

# This component is always linked statically.  It has code that is
# registered as a driver for libibverbs.  There is no corresponding
# *un*register API in libibverbs, so this code can never be dlclosed.
# And therefore it must be in the libopen-pal library, not a DSO or
# dependent library.

noinst_LTLIBRARIES = lib@OPAL_LIB_PREFIX@mca_common_verbs_usnic.la

lib@OPAL_LIB_PREFIX@mca_common_verbs_usnic_la_SOURCES = \
        $(headers) $(sources)
lib@OPAL_LIB_PREFIX@mca_common_verbs_usnic_la_CPPFLAGS = \
        $(common_verbs_usnic_CPPFLAGS)
lib@OPAL_LIB_PREFIX@mca_common_verbs_usnic_la_LDFLAGS =  \
        $(common_verbs_usnic_LDFLAGS)
lib@OPAL_LIB_PREFIX@mca_common_verbs_usnic_la_LIBADD = \
        $(common_verbs_usnic_LIBS)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/opal/mca/common/verbs_usnic
opal_HEADERS = $(headers)
else
opaldir = $(includedir)
endif
