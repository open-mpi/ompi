# -*- makefile -*-
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2013 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

fortran_scripts = \
     mpi-f90-interfaces.h.sh \
     mpi_comm_spawn_multiple_f90.f90.sh \
     mpi_sizeof.f90.sh \
     mpi_testall_f90.f90.sh \
     mpi_testsome_f90.f90.sh \
     mpi_unpack_external_f90.f90.sh \
     mpi_unpack_f90.f90.sh \
     mpi_waitall_f90.f90.sh \
     mpi_waitsome_f90.f90.sh \
     mpi_wtick_f90.f90.sh \
     mpi_wtime_f90.f90.sh

EXTRA_DIST = $(fortran_scripts)

if OMPI_BUILD_FORTRAN_USEMPI_TKR_BINDINGS
noinst_SCRIPTS = $(fortran_scripts)
endif

