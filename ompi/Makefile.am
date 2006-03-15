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

EXTRA_DIST=

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
	datatype \
        debuggers \
	etc \
        mpi \
	$(MCA_ompi_FRAMEWORKS_SUBDIRS) \
	$(MCA_ompi_FRAMEWORK_COMPONENT_STATIC_SUBDIRS) \
        . \
	$(MCA_ompi_FRAMEWORK_COMPONENT_DSO_SUBDIRS)

DIST_SUBDIRS = \
	include \
	debuggers \
	etc \
	mpi \
	$(MCA_ompi_FRAMEWORKS_SUBDIRS) \
	$(MCA_ompi_FRAMEWORK_COMPONENT_ALL_SUBDIRS)

# Build the main MPI library

lib_LTLIBRARIES = libmpi.la
libmpi_la_SOURCES =
libmpi_la_LIBADD = \
	datatype/libdatatype.la \
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

include class/Makefile.am

include attribute/Makefile.am
include communicator/Makefile.am
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
