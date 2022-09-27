#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2022 The University of Tennessee and The University
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
# Copyright (c) 2020-2022 Amazon.com, Inc. or its affiliates.  All Rights reserved.
# Copyright (c) 2022      Advanced Micro Devices, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

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
DIST_SUBDIRS = \
	include \
        datatype \
        etc \
	util \
	mca/base \
	$(MCA_opal_FRAMEWORKS_SUBDIRS) \
	$(MCA_opal_FRAMEWORK_COMPONENT_ALL_SUBDIRS)

noinst_LTLIBRARIES = libopen-pal_core.la
lib_LTLIBRARIES = lib@OPAL_LIB_NAME@.la

libopen_pal_core_la_SOURCES =
libopen_pal_core_la_LIBADD = \
        mca/base/libmca_base.la \
        util/libopalutil_core.la \
	$(MCA_opal_FRAMEWORK_CORE_LIBS)
libopen_pal_core_la_DEPENDENCIES = \
        mca/base/libmca_base.la \
        util/libopalutil_core.la \
	$(MCA_opal_FRAMEWORK_CORE_LIBS)

lib@OPAL_LIB_NAME@_la_SOURCES =
lib@OPAL_LIB_NAME@_la_LIBADD = \
	libopen-pal_core.la \
        datatype/libdatatype.la \
        util/libopalutil.la \
	$(MCA_opal_FRAMEWORK_LIBS)
lib@OPAL_LIB_NAME@_la_DEPENDENCIES = \
	libopen-pal_core.la \
        datatype/libdatatype.la \
        util/libopalutil.la \
        $(MCA_opal_FRAMEWORK_LIBS)
lib@OPAL_LIB_NAME@_la_LDFLAGS = -version-info @libopen_pal_so_version@

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
