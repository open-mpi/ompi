/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_F77_BINDINGS_H
#define OMPI_F77_BINDINGS_H

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/request/grequest.h"

#if OMPI_FORTRAN_CAPS
#define OMPI_GENERATE_F77_BINDINGS(upper_case, \
                                  lower_case, \
                                  single_underscore, \
                                  double_underscore, \
                                  wrapper_function, \
                                  signature, \
                                  params) \
            void upper_case signature { wrapper_function params; }
#elif OMPI_FORTRAN_PLAIN
#define OMPI_GENERATE_F77_BINDINGS(upper_case, \
                                  lower_case, \
                                  single_underscore, \
                                  double_underscore, \
                                  wrapper_function, \
                                  signature, \
                                  params) \
            void lower_case signature { wrapper_function params; }
#elif OMPI_FORTRAN_DOUBLE_UNDERSCORE
#define OMPI_GENERATE_F77_BINDINGS(upper_case, \
                                  lower_case, \
                                  single_underscore, \
                                  double_underscore, \
                                  wrapper_function, \
                                  signature, \
                                  params) \
            void double_underscore signature { wrapper_function params; }
#elif OMPI_FORTRAN_SINGLE_UNDERSCORE
#define OMPI_GENERATE_F77_BINDINGS(upper_case, \
                                  lower_case, \
                                  single_underscore, \
                                  double_underscore, \
                                  wrapper_function, \
                                  signature, \
                                  params) \
            void single_underscore signature { wrapper_function params; }
#else
#error Unrecognized Fortran name mangling scheme
#endif

/*
 * Same as OMPI_GENERATE_F77_BINDINGS, but emits a *weak* definition.
 *
 * This is how the public MPI_* Fortran entry points are produced where the
 * platform has no weak aliases (Mach-O cannot express one: there is no way to
 * mark a ".set" alias as a weak definition).  Previously such platforms
 * compiled every binding a second time, with OMPI_BUILD_MPI_PROFILING=0,
 * purely to emit MPI_* -- the body was compiled twice.  Emitting MPI_* here as
 * a weak function that forwards to the same implementation the strong PMPI_*
 * entry point calls means the bindings need be compiled only once.
 *
 * Weak, so that a profiling library's strong MPI_* still overrides ours.
 */
#if OMPI_FORTRAN_CAPS
#define OMPI_GENERATE_WEAK_F77_BINDINGS(upper_case, \
                                  lower_case, \
                                  single_underscore, \
                                  double_underscore, \
                                  wrapper_function, \
                                  signature, \
                                  params) \
            __opal_attribute_weak__ void upper_case signature { wrapper_function params; }
#elif OMPI_FORTRAN_PLAIN
#define OMPI_GENERATE_WEAK_F77_BINDINGS(upper_case, \
                                  lower_case, \
                                  single_underscore, \
                                  double_underscore, \
                                  wrapper_function, \
                                  signature, \
                                  params) \
            __opal_attribute_weak__ void lower_case signature { wrapper_function params; }
#elif OMPI_FORTRAN_DOUBLE_UNDERSCORE
#define OMPI_GENERATE_WEAK_F77_BINDINGS(upper_case, \
                                  lower_case, \
                                  single_underscore, \
                                  double_underscore, \
                                  wrapper_function, \
                                  signature, \
                                  params) \
            __opal_attribute_weak__ void double_underscore signature { wrapper_function params; }
#elif OMPI_FORTRAN_SINGLE_UNDERSCORE
#define OMPI_GENERATE_WEAK_F77_BINDINGS(upper_case, \
                                  lower_case, \
                                  single_underscore, \
                                  double_underscore, \
                                  wrapper_function, \
                                  signature, \
                                  params) \
            __opal_attribute_weak__ void single_underscore signature { wrapper_function params; }
#else
#error Unrecognized Fortran name mangling scheme
#endif
/*
 * The bindings are compiled exactly once, with
 * OMPI_BUILD_MPI_PROFILING=1: that compile emits the strong PMPI_*
 * entry points and -- via OMPI_GENERATE_WEAK_F77_BINDINGS, where the
 * platform has no weak aliases -- the weak MPI_* entry points as well.
 *
 * The prototypes for all MPI / PMPI functions are in
 * prototypes_mpi.h.
 */

#include "ompi/mpi/fortran/mpif-h/prototypes_mpi.h"

#include "ompi/mpi/fortran/base/fint_2_int.h"

#endif /* OMPI_F77_BINDINGS_H */
