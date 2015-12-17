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
# Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved.
# Copyright (c) 2013-2015 Intel, Inc. All rights reserved
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Note that the -I directory must *exactly* match what was specified
# via AC_CONFIG_MACRO_DIR in configure.ac.
ACLOCAL_AMFLAGS = -I ./config

headers =
sources =
nodist_headers =
EXTRA_DIST =

# Only install the valgrind suppressions file if we're building in
# standalone mode
dist_pmixdata_DATA =
if ! PMIX_EMBEDDED_MODE
dist_pmixdata_DATA += contrib/pmix-valgrind.supp
endif

man_MANS = \
        man/man3/pmix_init.3 \
        man/man3/pmix_finalize.3 \
        man/man3/pmix_initialized.3 \
        man/man3/pmix_abort.3 \
        man/man3/pmix_put.3 \
        man/man3/pmix_commit.3 \
        man/man7/pmix.7 \
        man/man7/pmix_constants.7

include config/Makefile.am
include include/Makefile.am
include src/class/Makefile.am
include src/include/Makefile.am
include src/buffer_ops/Makefile.am
include src/util/Makefile.am
include src/usock/Makefile.am
include src/client/Makefile.am
include src/server/Makefile.am
include src/sec/Makefile.am
include src/common/Makefile.am

if PMIX_EMBEDDED_MODE
noinst_LTLIBRARIES = libpmix.la
libpmix_la_SOURCES = $(headers) $(sources)
libpmix_la_LDFLAGS =
else
lib_LTLIBRARIES = libpmix.la
libpmix_la_SOURCES = $(headers) $(sources)
libpmix_la_LDFLAGS = -version-info $(libpmix_so_version)
endif


if ! PMIX_EMBEDDED_MODE
SUBDIRS = . test
pmixdir = $(pmixincludedir)/$(subdir)
nobase_pmix_HEADERS = $(headers)
endif

nroff:
	@for file in $(man_MANS); do \
	    source=`echo $$file | sed -e 's@/man[0-9]@@'`; \
	    contrib/md2nroff.pl --source=$$source.md; \
	done

EXTRA_DIST += README INSTALL VERSION LICENSE autogen.sh \
             config/pmix_get_version.sh $(man_MANS) \
             contrib/platform/optimized \
             test/test_common.h test/cli_stages.h \
             test/server_callbacks.h test/test_fence.h \
             test/test_publish.h test/test_resolve_peers.h \
             test/test_spawn.h test/utils.h test/test_cd.h \
             examples/client.c examples/dmodex.c examples/dynamic.c \
             examples/fault.c examples/pub.c


dist-hook:
	env LS_COLORS= sh "$(top_srcdir)/config/distscript.sh" "$(top_srcdir)" "$(distdir)" "$(PMIX_VERSION)" "$(PMIX_REPO_REV)"

