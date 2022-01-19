#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2021 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015-2016 Intel, Inc. All rights reserved.
# Copyright (c) 2016      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2020      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

if OPAL_cuda_support
SUBDIRS = \
	include \
        mca/common/cuda \
        datatype \
        etc \
        util \
	mca/base \
	$(MCA_opal_FRAMEWORKS_SUBDIRS) \
	$(MCA_opal_FRAMEWORK_COMPONENT_STATIC_SUBDIRS) \
        . \
	$(MCA_opal_FRAMEWORK_COMPONENT_DSO_SUBDIRS)
# libltdl is included by variable because if --disable-dlopen was
# used, there will be no generated Makefile in that directory (and
# therefore make distclean will fail).
DIST_SUBDIRS = \
	include \
        mca/common/cuda \
        datatype \
        etc \
	util \
	mca/base \
	$(MCA_opal_FRAMEWORKS_SUBDIRS) \
	$(MCA_opal_FRAMEWORK_COMPONENT_ALL_SUBDIRS)
else
SUBDIRS = \
	include \
        datatype \
        etc \
        util \
	mca/base \
	$(MCA_opal_FRAMEWORKS_SUBDIRS) \
	$(MCA_opal_FRAMEWORK_COMPONENT_STATIC_SUBDIRS) \
        . \
	$(MCA_opal_FRAMEWORK_COMPONENT_DSO_SUBDIRS)
# libltdl is included by variable because if --disable-dlopen was
# used, there will be no generated Makefile in that directory (and
# therefore make distclean will fail).
DIST_SUBDIRS = \
	include \
        datatype \
        etc \
	util \
	mca/base \
	$(MCA_opal_FRAMEWORKS_SUBDIRS) \
	$(MCA_opal_FRAMEWORK_COMPONENT_ALL_SUBDIRS)
endif

noinst_LTLIBRARIES = lib@OPAL_LIB_NAME@_core.la lib@OPAL_LIB_NAME@_util.la

# The first convenience library for the core functionality, everything that is shared
# between the base and the util libraries. We need this in order to avoid duplicate
# symbols when lib@OPAL_LIB_NAME@.la depends directly of lib@OPAL_LIB_NAME@_util.la
# (because $MCA_opal_FRAMEWORK_LIBS already contains the _STATIC_LTLIBS added to the
# lib@OPAL_LIB_NAME@_util.la library).
lib@OPAL_LIB_NAME@_core_la_SOURCES =
lib@OPAL_LIB_NAME@_core_la_LIBADD = \
        datatype/libdatatype.la \
        mca/base/libmca_base.la \
        util/libopalutil.la

lib@OPAL_LIB_NAME@_util_la_SOURCES =
lib@OPAL_LIB_NAME@_util_la_LIBADD = \
        lib@OPAL_LIB_NAME@_core.la \
        mca/backtrace/libmca_backtrace.la \
        $(MCA_opal_backtrace_STATIC_LTLIBS) \
        mca/threads/libmca_threads.la \
        $(MCA_opal_threads_STATIC_LTLIBS) \
        mca/timer/libmca_timer.la \
        $(MCA_opal_timer_STATIC_LTLIBS) \
        mca/installdirs/libmca_installdirs.la \
        $(MCA_opal_installdirs_STATIC_LTLIBS) \
        mca/if/libmca_if.la \
        $(MCA_opal_if_STATIC_LTLIBS) \
        mca/dl/libmca_dl.la \
        $(MCA_opal_dl_STATIC_LTLIBS) \
        $(opal_libevent_LIBS)
lib@OPAL_LIB_NAME@_util_la_DEPENDENCIES = \
        lib@OPAL_LIB_NAME@_core.la

# Build the main OPAL library
lib_LTLIBRARIES = lib@OPAL_LIB_NAME@.la
lib@OPAL_LIB_NAME@_la_SOURCES =
lib@OPAL_LIB_NAME@_la_LIBADD = \
	lib@OPAL_LIB_NAME@_core.la \
	$(MCA_opal_FRAMEWORK_LIBS) \
	$(opal_libevent_LIBS) \
	$(opal_hwloc_LIBS) \
	$(opal_pmix_LIBS)
lib@OPAL_LIB_NAME@_la_DEPENDENCIES = \
	lib@OPAL_LIB_NAME@_core.la \
        $(MCA_opal_FRAMEWORK_LIBS)

lib@OPAL_LIB_NAME@_la_LDFLAGS = -version-info @libopen_pal_so_version@ \
	lib@OPAL_LIB_NAME@_core_LDFLAGS \
	$(opal_libevent_LDFLAGS) \
	$(opal_hwloc_LDFLAGS) \
	$(opal_pmix_LDFLAGS)

# included subdirectory Makefile.am's and appended-to variables
headers =
dist_opaldata_DATA =
lib@OPAL_LIB_NAME@_la_SOURCES += $(headers)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/$(subdir)
nobase_opal_HEADERS = $(headers)
endif

include class/Makefile.am
include memoryhooks/Makefile.am
include runtime/Makefile.am
include mca/Makefile.am
include tools/Makefile.am
