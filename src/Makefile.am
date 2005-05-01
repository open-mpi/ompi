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

include $(top_srcdir)/config/Makefile.options

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
              win32/generated_include/libltdl/ltdl.h            \
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
        include \
        $(LIBLTDL_SUBDIR) \
	asm \
        attribute \
        communicator \
        datatype \
	dps \
        errhandler \
        event \
        file \
        group \
        info \
        class \
        mca \
        mpi \
        op \
        proc \
        request \
        runtime \
        threads \
        util \
        win \
        . \
        dynamic-mca \
        tools

# Build the main MPI library

lib_LTLIBRARIES = libmpi.la
libmpi_la_SOURCES =
libmpi_la_LIBADD = \
        $(LIBLTDL_LTLIB) \
	asm/libasm.la \
        attribute/libattribute.la \
        class/liblfc.la \
        communicator/libcommunicator.la \
        datatype/libdatatype.la \
	dps/libdps.la \
        errhandler/liberrhandler.la \
        event/libevent.la \
        file/libfile.la \
        group/libgroup.la \
        info/libinfo.la \
        mca/base/libmca_base.la \
        mca/allocator/base/libmca_allocator_base.la $(MCA_allocator_STATIC_LTLIBS) \
        mca/coll/base/libmca_coll_base.la $(MCA_coll_STATIC_LTLIBS) \
        $(MCA_common_STATIC_LTLIBS) \
        mca/errmgr/base/libmca_errmgr_base.la \
        mca/gpr/base/pack_api_cmd/libmca_gpr_base_pack.la \
        mca/gpr/base/unpack_api_response/libmca_gpr_base_unpack.la \
        mca/gpr/base/libmca_gpr_base.la $(MCA_gpr_STATIC_LTLIBS) \
        mca/io/base/libmca_io_base.la $(MCA_io_STATIC_LTLIBS) \
        mca/iof/base/libmca_iof_base.la $(MCA_iof_STATIC_LTLIBS) \
        mca/mpool/base/libmca_mpool_base.la $(MCA_mpool_STATIC_LTLIBS) \
        mca/ns/base/libmca_ns_base.la $(MCA_ns_STATIC_LTLIBS) \
        mca/oob/base/libmca_oob_base.la $(MCA_oob_STATIC_LTLIBS) \
        mca/pls/base/libmca_pls_base.la $(MCA_pls_STATIC_LTLIBS) \
        mca/pml/base/libmca_pml_base.la $(MCA_pml_STATIC_LTLIBS) \
        mca/ptl/base/libmca_ptl_base.la $(MCA_ptl_STATIC_LTLIBS) \
        mca/ras/base/libmca_ras_base.la $(MCA_ras_STATIC_LTLIBS) \
        mca/rds/base/libmca_rds_base.la $(MCA_rds_STATIC_LTLIBS) \
        mca/rmaps/base/libmca_rmaps_base.la $(MCA_rmaps_STATIC_LTLIBS) \
        mca/rmgr/base/libmca_orte_rmgr_base.la $(MCA_rmgr_STATIC_LTLIBS) \
        mca/rml/base/libmca_rml_base.la $(MCA_rml_STATIC_LTLIBS) \
	mca/schema/base/libmca_schema_base.la \
        mca/soh/base/libmca_soh_base.la $(MCA_soh_STATIC_LTLIBS) \
        mca/topo/base/libmca_topo_base.la $(MCA_topo_STATIC_LTLIBS) \
        mpi/c/libmpi_c.la $(c_mpi_lib) $(c_pmpi_lib) \
        $(f77_base_lib) $(f77_mpi_lib) $(f77_pmpi_lib) \
        mpi/runtime/libmpiruntime.la \
        op/libop.la \
        proc/libproc.la \
        request/librequest.la \
        runtime/libruntime.la \
        threads/libthreads.la \
        util/libompiutil.la
libmpi_la_DEPENDENCIES = $(libmpi_la_LIBADD)

# Conditionally install the header files

# This is somewhat of a hack -- libltdl is installed by libtoolize,
# and us installing the header here is slightly less kludgey than
# hacking their Makefile.am.

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/libltdl
ompi_HEADERS = libltdl/ltdl.h
else
ompidir = $(includedir)
endif
