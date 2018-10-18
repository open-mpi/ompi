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
# Copyright (c) 2006-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved.
# Copyright (c) 2013-2018 Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Note that the -I directory must *exactly* match what was specified
# via AC_CONFIG_MACRO_DIR in configure.ac.
ACLOCAL_AMFLAGS = -I ./config

SUBDIRS = \
	util/keyval \
    mca/base \
    $(MCA_pmix_FRAMEWORKS_SUBDIRS) \
    $(MCA_pmix_FRAMEWORK_COMPONENT_STATIC_SUBDIRS) \
    . \
    $(MCA_pmix_FRAMEWORK_COMPONENT_DSO_SUBDIRS)

DIST_SUBDIRS = \
	util/keyval \
    mca/base \
    $(MCA_pmix_FRAMEWORKS_SUBDIRS) \
    $(MCA_pmix_FRAMEWORK_COMPONENT_ALL_SUBDIRS)

headers =
sources =
nodist_headers =
EXTRA_DIST =
dist_pmixdata_DATA =

# place to capture sources for backward compatibility libs
pmi1_sources =
pmi2_sources =

libpmix_la_LIBADD = \
	mca/base/libpmix_mca_base.la \
	$(MCA_pmix_FRAMEWORK_LIBS) \
	$(PMIX_EXTRA_LIB)
libpmix_la_DEPENDENCIES = $(libpmix_la_LIBADD)

if PMIX_EMBEDDED_MODE

if WANT_INSTALL_HEADERS

lib_LTLIBRARIES = libpmix.la
libpmix_la_SOURCES = $(headers) $(sources)
libpmix_la_LDFLAGS = -version-info $(libpmix_so_version)

else

noinst_LTLIBRARIES = libpmix.la
libpmix_la_SOURCES = $(headers) $(sources)
libpmix_la_LDFLAGS =

endif

else

lib_LTLIBRARIES = libpmix.la
libpmix_la_SOURCES = $(headers) $(sources)
libpmix_la_LDFLAGS = -version-info $(libpmix_so_version)

if WANT_PMI_BACKWARD
lib_LTLIBRARIES += libpmi.la libpmi2.la
libpmi_la_SOURCES = $(headers) $(sources) $(pmi1_sources)
libpmi_la_LDFLAGS = -version-info $(libpmi_so_version)
libpmi_la_LIBADD = $(libpmix_la_LIBADD)
libpmi_la_DEPENDENCIES = $(libpmi_la_LIBADD)

libpmi2_la_SOURCES = $(headers) $(sources) $(pmi2_sources)
libpmi2_la_LDFLAGS = -version-info $(libpmi2_so_version)
libpmi2_la_LIBADD = $(libpmix_la_LIBADD)
libpmi2_la_DEPENDENCIES = $(libpmi2_la_LIBADD)
endif

endif !PMIX_EMBEDDED_MODE

include atomics/sys/Makefile.include
include threads/Makefile.include
include class/Makefile.include
include event/Makefile.include
include include/Makefile.include
include mca/Makefile.include
include util/Makefile.include
include client/Makefile.include
include server/Makefile.include
include runtime/Makefile.include
include tool/Makefile.include
include tools/Makefile.include
include common/Makefile.include
include hwloc/Makefile.include

MAINTAINERCLEANFILES = Makefile.in config.h config.h.in
DISTCLEANFILES = Makefile
CLEANFILES = core.* *~
