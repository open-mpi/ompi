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

# Include Windows only files in dist
WIN32_FILES = win32/ompi_declspec.h \
              win32/ompi_time.c     \
              win32/ompi_process.c  \
              win32/ompi_uio.c      \
              win32/ompi_util.h     \
              win32/ompi_utsname.h  \
              win32/ompi_misc.h     \
              win32/ompi_process.h  \
              win32/ompi_time.h     \
              win32/ompi_uio.h      \
              win32/ompi_utsname.c  \
              win32/win_compat.h    \
              win32/generated_include/allocator_static-components.h  \
              win32/generated_include/iof_static-components.h   \
              win32/generated_include/errmgr_static-components.h \
              win32/generated_include/rml_static-components.h   \
              win32/generated_include/rmgr_static-components.h  \
              win32/generated_include/schema_static-components.h  \
              win32/generated_include/rmaps_static-components.h \
              win32/generated_include/soh_static-components.h \
              win32/generated_include/ns_static-components.h    \
              win32/generated_include/pcmclient-seed-version.h  \
              win32/generated_include/pml_static-components.h   \
              win32/generated_include/coll-basic-version.h      \
              win32/generated_include/io-romio-version.h        \
              win32/generated_include/ompi_config.h             \
              win32/generated_include/pcmclient-singleton-version.h  \
              win32/generated_include/ptl-prof-version.h        \
              win32/generated_include/coll_static-components.h  \
              win32/generated_include/oob_static-components.h   \
              win32/generated_include/pcmclient-slurm-version.h \
              win32/generated_include/ptl_static-components.h   \
              win32/generated_include/config.h                  \
              win32/generated_include/llm-hostfile-version.h    \
              win32/generated_include/pcmclient-env-version.h   \
              win32/generated_include/pcmclient_static-components.h  \
              win32/generated_include/topo_static-components.h  \
              win32/generated_include/gpr_static-components.h   \
              win32/generated_include/llm_static-components.h   \
              win32/generated_include/pcmclient-rms-version.h   \
              win32/generated_include/pcm_static-components.h   \
              win32/generated_include/topo-unity-version.h      \
              win32/generated_include/pls_static-components.h \
              win32/generated_include/ras_static-components.h \
              win32/generated_include/rds_static-components.h \
              win32/generated_include/pls-fork-version.h \
              win32/generated_include/pls-proxy-version.h \
              win32/generated_include/pls-rsh-version.h \
              win32/generated_include/libltdl/ltdl.h \
              win32/generated_source/ltdl.c
EXTRA_DIST = win_makefile $(WIN32_FILES)

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
