#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2009 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

SUBDIRS = \
	include \
	asm \
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
	asm \
        datatype \
        etc \
	util \
	mca/base \
	$(MCA_opal_FRAMEWORKS_SUBDIRS) \
	$(MCA_opal_FRAMEWORK_COMPONENT_ALL_SUBDIRS)

# Build the main OPAL library

lib_LTLIBRARIES = lib@OPAL_LIB_PREFIX@open-pal.la
lib@OPAL_LIB_PREFIX@open_pal_la_SOURCES =
lib@OPAL_LIB_PREFIX@open_pal_la_LIBADD = \
        asm/libasm.la \
        datatype/libdatatype.la \
        mca/base/libmca_base.la \
        util/libopalutil.la \
	$(MCA_opal_FRAMEWORK_LIBS)
lib@OPAL_LIB_PREFIX@open_pal_la_DEPENDENCIES = $(lib@OPAL_LIB_PREFIX@open_pal_la_LIBADD)
lib@OPAL_LIB_PREFIX@open_pal_la_LDFLAGS = -version-info $(libopen_pal_so_version)

# included subdirectory Makefile.am's and appended-to variables
headers =
noinst_LTLIBRARIES =
dist_opaldata_DATA =
lib@OPAL_LIB_PREFIX@open_pal_la_SOURCES += $(headers)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/$(subdir)
nobase_opal_HEADERS = $(headers)
endif

include class/Makefile.am
include memoryhooks/Makefile.am
include runtime/Makefile.am
include threads/Makefile.am
include mca/Makefile.am
include tools/Makefile.am
include dss/Makefile.am
