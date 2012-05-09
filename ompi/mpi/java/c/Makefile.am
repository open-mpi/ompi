# -*- makefile -*-
#
# Copyright (c) 2011	Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012	Oracle and/or its affiliates.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

if OMPI_WANT_JAVA_BINDINGS

# Get the include files that were generated from the .java source files
AM_CPPFLAGS = -I$(top_builddir)/ompi/mpi/java/java $(ORTE_JDK_CPPFLAGS) $(LTDLINCL)

headers = \
        mpiJava.h
ompidir = $(includedir)/openmpi/ompi/mpi/java
ompi_HEADERS = \
        $(headers)

lib_LTLIBRARIES = libmpi_java.la
libmpi_java_la_SOURCES = \
        mpi_Cartcomm.c \
        mpi_Comm.c \
        mpi_Datatype.c \
        mpi_Errhandler.c \
        mpi_Graphcomm.c \
        mpi_Group.c \
        mpi_Intercomm.c \
        mpi_Intracomm.c \
        mpi_MPI.c \
        mpi_Op.c \
        mpi_Request.c \
        mpi_Status.c

libmpi_java_la_LIBADD = $(top_builddir)/ompi/libmpi.la $(LIBLTDL)
libmpi_java_la_LDFLAGS = -version-info $(libmpi_java_so_version)

endif
