# Copyright © 2016-2017 Inria.  All rights reserved.
#
# See COPYING in top-level directory.
#
# $HEADER$
#

AM_CFLAGS = $(NETLOC_CFLAGS)
AM_CPPFLAGS = $(NETLOC_CPPFLAGS)
AM_LDFLAGS = $(NETLOC_LDFLAGS) -lpthread

LDADD = $(top_builddir)/netloc/libnetloc.la \
        $(top_builddir)/netloc/libnetlocscotch.la \
        -lscotch -lscotcherr -lm -lpthread

EXTRA_DIST = \
        data/tests_extract.txt \
        data/tests_draw.txt \
        data/avakas.txz \
        data/plafrim.txz \
        data/plafrim2.txz \
        data/scotch.txz \
        data/tests_scotch.txt \
        data/tests_mpiscotch.txt

if FOUND_XZ
TESTS = \
        data/tests_extract.txt \
        data/tests_draw.txt

if BUILD_NETLOCSCOTCH
TESTS += data/tests_scotch.txt
if BUILD_MPITOOLS
TESTS += data/tests_mpiscotch.txt
endif
endif
endif

TEST_EXTENSIONS = .txt
TXT_LOG_COMPILER = ./tests.sh
