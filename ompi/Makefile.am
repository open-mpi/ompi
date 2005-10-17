#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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

# We always have C bindings, but do we have profiling?

if WANT_MPI_BINDINGS_LAYER
c_mpi_lib = mpi/c/libmpi_c_mpi.la
else
c_mpi_lib = 
endif
if WANT_PMPI_BINDINGS_LAYER
c_pmpi_lib = mpi/c/profile/libmpi_c_pmpi.la
else
c_pmpi_lib = 
endif

# See if we have f77 MPI bindings

if OMPI_WANT_F77_BINDINGS
f77_base_lib = mpi/f77/libmpi_f77.la
if WANT_MPI_F77_BINDINGS_LAYER
f77_mpi_lib = mpi/f77/libmpi_f77_mpi.la
else
f77_mpi_lib = 
endif
if WANT_PMPI_F77_BINDINGS_LAYER
f77_pmpi_lib = mpi/f77/profile/libmpi_f77_pmpi.la
else
f77_pmpi_lib = 
endif
else
f77_base_lib =
f77_mpi_lib =
f77_pmpi_lib =
endif

SUBDIRS = \
        debuggers \
        mpi \
	$(MCA_ompi_FRAMEWORKS_SUBDIRS) \
	$(MCA_ompi_FRAMEWORK_COMPONENT_STATIC_SUBDIRS) \
        . \
	$(MCA_ompi_FRAMEWORK_COMPONENT_DSO_SUBDIRS)

DIST_SUBDIRS = \
	debuggers \
	mpi \
	$(MCA_ompi_FRAMEWORKS_SUBDIRS) \
	$(MCA_ompi_FRAMEWORK_COMPONENT_ALL_SUBDIRS)

# Build the main MPI library

lib_LTLIBRARIES = libmpi.la
libmpi_la_SOURCES =
libmpi_la_LIBADD = \
        debuggers/libdebuggers.la \
        mpi/c/libmpi_c.la \
	$(c_mpi_lib) \
	$(c_pmpi_lib) \
        $(f77_base_lib) \
	$(f77_mpi_lib) \
	$(f77_pmpi_lib) \
	$(MCA_ompi_FRAMEWORK_LIBS) \
	$(top_ompi_builddir)/orte/liborte.la
libmpi_la_DEPENDENCIES = $(libmpi_la_LIBADD)

# included subdirectory Makefile.am's and appended-to variables
headers =
noinst_LTLIBRARIES =
nobase_ompi_HEADERS = 
dist_pkgdata_DATA =
libmpi_la_SOURCES += $(headers)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/ompi
nobase_ompi_HEADERS += $(headers)
else
ompidir = $(includedir)
endif

include include/Makefile.am
include class/Makefile.am

include attribute/Makefile.am
include communicator/Makefile.am
include datatype/Makefile.am
include errhandler/Makefile.am
include file/Makefile.am
include group/Makefile.am
include info/Makefile.am
include op/Makefile.am
include proc/Makefile.am
include request/Makefile.am
include runtime/Makefile.am
include win/Makefile.am
include tools/Makefile.am
