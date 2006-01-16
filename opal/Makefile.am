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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST =

SUBDIRS = \
        $(LIBLTDL_SUBDIR) \
	asm \
        event \
        util \
	mca/base \
	$(MCA_opal_FRAMEWORKS_SUBDIRS) \
	$(MCA_opal_FRAMEWORK_COMPONENT_STATIC_SUBDIRS) \
        . \
	$(MCA_opal_FRAMEWORK_COMPONENT_DSO_SUBDIRS)

DIST_SUBDIRS = \
	$(LIBLTDL_SUBDIR) \
	asm \
	event \
	util \
	mca/base \
	$(MCA_opal_FRAMEWORKS_SUBDIRS) \
	$(MCA_opal_FRAMEWORK_COMPONENT_ALL_SUBDIRS)

# Build the main OPAL library

lib_LTLIBRARIES = libopal.la
libopal_la_SOURCES =
libopal_la_LIBADD = \
        $(LIBLTDL_LTLIB) \
        asm/libasm.la \
        event/libevent.la \
        mca/base/libmca_base.la \
        util/libopalutil.la \
	$(MCA_opal_FRAMEWORK_LIBS)

libopal_la_DEPENDENCIES = $(libopal_la_LIBADD)

# included subdirectory Makefile.am's and appended-to variables
headers =
noinst_LTLIBRARIES =
nobase_opal_HEADERS = 
dist_pkgdata_DATA =
libopal_la_SOURCES += $(headers)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
opaldir = $(includedir)/openmpi/opal
nobase_opal_HEADERS += $(headers)

# This is somewhat of a hack -- libltdl is installed by libtoolize,
# and us installing the header here is slightly less kludgey than
# hacking their Makefile.am.
nobase_opal_HEADERS += libltdl/ltdl.h
else
opaldir = $(includedir)
endif

include include/Makefile.am
include class/Makefile.am
include memoryhooks/Makefile.am
include runtime/Makefile.am
include threads/Makefile.am
include mca/Makefile.am
include win32/Makefile.am
include tools/Makefile.am
