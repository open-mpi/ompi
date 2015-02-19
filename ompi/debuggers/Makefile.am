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
# Copyright (c) 2007-2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

noinst_LTLIBRARIES = libdebuggers.la libompi_debugger_canary.la
ompilib_LTLIBRARIES = libompi_dbg_msgq.la

check_PROGRAMS = predefined_gap_test predefined_pad_test dlopen_test

TESTS = $(check_PROGRAMS)

# This is not quite in the Automake spirit, but we have to do it.
# Since the totalview portion of the library must be built with -g, we
# must eliminate the CFLAGS that are passed in here by default (which
# may already have debugging and/or optimization flags).  We use
# post-processed forms of the CFLAGS in the library targets down
# below.

CFLAGS = $(CFLAGS_WITHOUT_OPTFLAGS) $(DEBUGGER_CFLAGS)

# Source code files

headers = \
        debuggers.h \
        ompi_common_dll_defs.h \
        msgq_interface.h ompi_msgq_dll_defs.h

# Simple checks to ensure that the DSOs are functional

dlopen_test_SOURCES = dlopen_test.c
dlopen_test_LDADD = $(top_builddir)/ompi/libmpi.la
dlopen_test_DEPENDENCIES = $(ompi_predefined_LDADD)

predefined_gap_test_SOURCES = predefined_gap_test.c
predefined_gap_test_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
predefined_gap_test_LDADD = $(top_builddir)/ompi/libmpi.la
predefined_gap_test_DEPENDENCIES = $(ompi_predefined_LDADD)

libdebuggers_la_SOURCES = \
        $(headers) \
        ompi_debuggers.c
libdebuggers_la_CPPFLAGS = \
        -DOMPI_MSGQ_DLL=\"$(ompilibdir)/$(OPAL_DYN_LIB_PREFIX)ompi_dbg_msgq.$(OPAL_DYN_LIB_SUFFIX)\" \
        -DOMPI_MSGQ_DLL_PREFIX=\"$(OPAL_DYN_LIB_PREFIX)ompi_dbg_msgq\" \
        -DOMPI_MPIHANDLES_DLL_PREFIX=\"$(OPAL_DYN_LIB_PREFIX)ompi_dbg_mpihandles\"

libompi_debugger_canary_la_SOURCES = \
        ompi_debugger_canary.c

common = ompi_common_dll_defs.h ompi_common_dll.c

# MPI message queue DLL
libompi_dbg_msgq_la_SOURCES = ompi_msgq_dll.c ompi_msgq_dll_defs.h $(common)
libompi_dbg_msgq_la_CFLAGS = -g
libompi_dbg_msgq_la_LDFLAGS = -module -avoid-version

# These are checks for the padding on predefined MPI object types.
# They are here in the debuggers/ directory (vs., for example,
# runtime/) because a) we already had some canary tests here in this
# directory, and b) the runtime/ directory is built by subdir objects,
# and "make check" will *build* a test in runtime/, but it won't *run*
# it.  :-(
predefined_pad_test_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
predefined_pad_test_LDADD = $(top_builddir)/ompi/libmpi.la
predefined_pad_test_DEPENDENCIES = $(ompi_predefined_LDADD)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(ompiincludedir)/$(subdir)
ompi_HEADERS = $(headers)
endif
