#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

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

# See if we have F90 MPI bindings

if OMPI_WANT_F90_BINDINGS
f90_lib =
else
f90_lib =
endif

# See if we have C++ MPI bindings

if WANT_MPI_CXX_BINDINGS
cxx_lib = 
else
cxx_lib =
endif

SUBDIRS = \
        include \
        $(LIBLTDL_SUBDIR) \
        attribute \
        communicator \
        datatype \
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
        attribute/libattribute.la \
        class/liblfc.la \
        communicator/libcommunicator.la \
        datatype/libdatatype.la \
        errhandler/liberrhandler.la \
        event/libevent.la \
        file/libfile.la \
        group/libgroup.la \
        info/libinfo.la \
        mca/base/libmca_base.la \
        mca/allocator/base/libmca_allocator_base.la $(MCA_allocator_STATIC_LTLIBS) \
        mca/coll/base/libmca_coll_base.la $(MCA_coll_STATIC_LTLIBS) \
        $(MCA_common_STATIC_LTLIBS) \
        mca/gpr/base/libmca_gpr_base.la $(MCA_gpr_STATIC_LTLIBS) \
        mca/io/base/libmca_io_base.la $(MCA_io_STATIC_LTLIBS) \
        mca/llm/base/libmca_llm_base.la $(MCA_llm_STATIC_LTLIBS) \
        mca/mpool/base/libmca_mpool_base.la $(MCA_mpool_STATIC_LTLIBS) \
        mca/ns/base/libmca_ns_base.la $(MCA_ns_STATIC_LTLIBS) \
        mca/oob/base/libmca_oob_base.la $(MCA_oob_STATIC_LTLIBS) \
        mca/pcm/base/libmca_pcm_base.la $(MCA_pcm_STATIC_LTLIBS) \
        mca/pcmclient/base/libmca_pcmclient_base.la $(MCA_pcmclient_STATIC_LTLIBS) \
        mca/pml/base/libmca_pml_base.la $(MCA_pml_STATIC_LTLIBS) \
        mca/ptl/base/libmca_ptl_base.la $(MCA_ptl_STATIC_LTLIBS) \
        mca/soh/base/libmca_soh_base.la $(MCA_soh_STATIC_LTLIBS) \
        mca/topo/base/libmca_topo_base.la $(MCA_topo_STATIC_LTLIBS) \
        mpi/c/libmpi_c.la $(c_mpi_lib) $(c_pmpi_lib) \
        $(f77_base_lib) $(f77_mpi_lib) $(f77_pmpi_lib) \
        $(f90_lib) $(cxx_lib) \
	mpi/runtime/libmpiruntime.la \
        op/libop.la \
        proc/libproc.la \
        request/librequest.la \
        runtime/libruntime.la \
        threads/libthreads.la \
        util/libutil.la
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
