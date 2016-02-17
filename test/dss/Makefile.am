# Copyright (c) 2016      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

TESTS = dss_buffer dss_cmp dss_payload dss_print

check_PROGRAMS = $(TESTS) $(MPI_CHECKS)

LDFLAGS = $(OPAL_PKG_CONFIG_LDFLAGS)
LDADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la
