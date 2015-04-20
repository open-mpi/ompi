# -*- Mode: Makefile; -*-
#
# (C) 2011 by Argonne National Laboratory.
#     See COPYRIGHT in top-level directory.
#

# This directory is only listed in DIST_SUBDIRS, not SUBDIRS, so its contents
# will not be built by default, but it will participate in "make distclean" and
# friends.

# override the normal compilers for the tests
CC = $(TEST_CC)

# because := is not universally avalible, we have to play games to use the
# user-specified LDFLAGS and OUR_LIBS env. variables (if set)
OUR_LIBS = $(TEST_LIBNAME) $(MPI_LIB) $(ROMIO_LIBLIST)

LDADD = $(OUR_LIBS)

AM_CPPFLAGS = $(ROMIO_INCLUDE)
AM_CFLAGS = $(USER_CFLAGS)

CTESTS = file_realms_test io_bounds_test heap_test

noinst_PROGRAMS = $(CTESTS)

