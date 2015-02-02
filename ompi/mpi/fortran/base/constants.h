/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Inria.  All rights reserved.
 * Copyright (c) 2011-2012 Universite Bordeaux 1
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_FORTRAN_BASE_CONSTANTS_H
#define OMPI_FORTRAN_BASE_CONSTANTS_H

#include "ompi_config.h"

/*
 * Several variables are used to link against MPI F77 constants which
 * correspond to addresses, e.g. MPI_BOTTOM, and are implemented via
 * common blocks.  
 *
 * We use common blocks so that in the C wrapper functions, we can
 * compare the address that comes in against known addresses (e.g., if
 * the "status" argument in MPI_RECV is the address of the common
 * block for the fortran equivalent of MPI_STATUS_IGNORE, then we know
 * to pass the C MPI_STATUS_IGNORE to the C MPI_Recv function).  As
 * such, we never look at the *value* of these variables (indeed,
 * they're never actually initialized), but instead only ever look at
 * the *address* of these variables.
 *
 * As such, it is not strictly necessary that the size and type of our
 * C variables matches that of the common Fortran block variables.
 * However, good programming form says that we should match, so we do.
 *
 * Note, however, that the alignments of the Fortran common block and
 * the C variable may not match (e.g., Intel 9.0 compilers on 64 bit
 * platforms will put the alignment of a double on 4 bytes, but put
 * the alignment of all common blocks on 16 bytes).  This only matters
 * (to some compilers!), however, if you initialize the C variable in
 * the global scope.  If the C global instantiation is not
 * initialized, the compiler/linker seems to "figure it all out" and
 * make the alignments match.
 *
 * Since we made the fundamental decision to support all 4 common
 * fortran compiler symbol conventions within the same library for
 * those compilers who support weak symbols, we need to have 4 symbols
 * for each of the fortran address constants.  As described above, we
 * have to have known *pointer* values for the fortran addresses
 * (e.g., MPI_STATUS_IGNORE).  So when the fortran wrapper for
 * MPI_RECV gets (MPI_Fint *status), it can check (status ==
 * some_sentinel_value) to know that it got the Fortran equivalent of
 * MPI_STATUS_IGNORE and therefore pass the C MPI_STATUS_IGNORE to the
 * C MPI_Recv.
 *
 * We do this by having a "common" block in mpif.h:
 *
 * INTEGER MPI_STATUS_IGNORE(MPI_STATUS_SIZE)
 * common /mpi_fortran_status_ignore/ MPI_STATUS_IGNORE
 *
 * This makes the fortran variable MPI_STATUS_IGNORE effectively be an
 * alias for the C variable "mpi_fortran_status_ignore" -- but the C
 * symbol name is according to the fortran compiler's naming symbol
 * convention bais.  So it could be MPI_FORTRAN_STATUS_IGNORE,
 * mpi_fortran_status_ignore, mpi_fortran_status_ignore_, or
 * mpi_fortran_status_ignore__.
 *
 * Hence, we have to have *4* C symbols for this, and them compare for
 * all of them in the fortran MPI_RECV wrapper.  :-( I can't think of
 * any better way to do this.
 *
 * I'm putting these 4 comparisons in macros (on systems where we
 * don't support the 4 symbols -- e.g., OSX, where we don't have weak
 * symbols -- it'll only be one comparison), so if anyone things of
 * something better than this, you should only need to modify this
 * file.
 */

#if OMPI_FORTRAN_CAPS
#define DECL(type, upper_case, lower_case, single_u, double_u)   \
OMPI_DECLSPEC extern type upper_case
#elif OMPI_FORTRAN_PLAIN
#define DECL(type, upper_case, lower_case, single_u, double_u)   \
OMPI_DECLSPEC extern type lower_case
#elif OMPI_FORTRAN_SINGLE_UNDERSCORE
#define DECL(type, upper_case, lower_case, single_u, double_u)   \
OMPI_DECLSPEC extern type single_u
#elif OMPI_FORTRAN_DOUBLE_UNDERSCORE
#define DECL(type, upper_case, lower_case, single_u, double_u)   \
OMPI_DECLSPEC extern type double_u
#else
#error Unrecognized Fortran name mangling scheme
#endif

/* Note that the rationale for the types of each of these variables is
   discussed in ompi/include/mpif-common.h.  Do not change the types
   without also changing ompi/runtime/ompi_mpi_init.c and
   ompi/include/mpif-common.h. */

DECL(int, MPI_FORTRAN_BOTTOM, mpi_fortran_bottom,
     mpi_fortran_bottom_, mpi_fortran_bottom__);
DECL(int, MPI_FORTRAN_IN_PLACE, mpi_fortran_in_place,
     mpi_fortran_in_place_, mpi_fortran_in_place__);
DECL(int, MPI_FORTRAN_UNWEIGHTED, mpi_fortran_unweighted,
     mpi_fortran_unweighted_, mpi_fortran_unweighted__);
DECL(int, MPI_FORTRAN_WEIGHTS_EMPTY, mpi_fortran_weights_empty,
     mpi_fortran_weights_empty_, mpi_fortran_weights_empty__);
DECL(char *, MPI_FORTRAN_ARGV_NULL, mpi_fortran_argv_null,
     mpi_fortran_argv_null_, mpi_fortran_argv_null__);
DECL(char *, MPI_FORTRAN_ARGVS_NULL, mpi_fortran_argvs_null,
     mpi_fortran_argvs_null_, mpi_fortran_argvs_null__);
DECL(int *, MPI_FORTRAN_ERRCODES_IGNORE, mpi_fortran_errcodes_ignore,
     mpi_fortran_errcodes_ignore_, mpi_fortran_errcodes_ignore__);
DECL(int *, MPI_FORTRAN_STATUS_IGNORE, mpi_fortran_status_ignore,
     mpi_fortran_status_ignore_, mpi_fortran_status_ignore__);
DECL(int *, MPI_FORTRAN_STATUSES_IGNORE, mpi_fortran_statuses_ignore,
     mpi_fortran_statuses_ignore_, mpi_fortran_statuses_ignore__);

/*
 * Create macros to do the checking.
 * Just check for the one and only relevant symbol.
 */
#if OMPI_FORTRAN_CAPS
#define OMPI_IS_FORTRAN_BOTTOM(addr) \
  (addr == (void*) &MPI_FORTRAN_BOTTOM)
#define OMPI_IS_FORTRAN_IN_PLACE(addr) \
  (addr == (void*) &MPI_FORTRAN_IN_PLACE)
#define OMPI_IS_FORTRAN_UNWEIGHTED(addr) \
  (addr == (void*) &MPI_FORTRAN_UNWEIGHTED)
#define OMPI_IS_FORTRAN_WEIGHTS_EMPTY(addr) \
  (addr == (void*) &MPI_FORTRAN_WEIGHTS_EMPTY)
#define OMPI_IS_FORTRAN_ARGV_NULL(addr) \
  (addr == (void*) &MPI_FORTRAN_ARGV_NULL)
#define OMPI_IS_FORTRAN_ARGVS_NULL(addr) \
  (addr == (void*) &MPI_FORTRAN_ARGVS_NULL)
#define OMPI_IS_FORTRAN_ERRCODES_IGNORE(addr) \
  (addr == (void*) &MPI_FORTRAN_ERRCODES_IGNORE)
#define OMPI_IS_FORTRAN_STATUS_IGNORE(addr) \
  (addr == (void*) &MPI_FORTRAN_STATUS_IGNORE)
#define OMPI_IS_FORTRAN_STATUSES_IGNORE(addr) \
  (addr == (void*) &MPI_FORTRAN_STATUSES_IGNORE)

#elif OMPI_FORTRAN_PLAIN
#define OMPI_IS_FORTRAN_BOTTOM(addr) \
   (addr == (void*) &mpi_fortran_bottom)
#define OMPI_IS_FORTRAN_IN_PLACE(addr) \
   (addr == (void*) &mpi_fortran_in_place)
#define OMPI_IS_FORTRAN_UNWEIGHTED(addr) \
   (addr == (void*) &mpi_fortran_unweighted)
#define OMPI_IS_FORTRAN_WEIGHTS_EMPTY(addr) \
   (addr == (void*) &mpi_fortran_weights_empty)
#define OMPI_IS_FORTRAN_ARGV_NULL(addr) \
   (addr == (void*) &mpi_fortran_argv_null)
#define OMPI_IS_FORTRAN_ARGVS_NULL(addr) \
   (addr == (void*) &mpi_fortran_argvs_null)
#define OMPI_IS_FORTRAN_ERRCODES_IGNORE(addr) \
   (addr == (void*) &mpi_fortran_errcodes_ignore)
#define OMPI_IS_FORTRAN_STATUS_IGNORE(addr) \
   (addr == (void*) &mpi_fortran_status_ignore)
#define OMPI_IS_FORTRAN_STATUSES_IGNORE(addr) \
   (addr == (void*) &mpi_fortran_statuses_ignore)

#elif OMPI_FORTRAN_SINGLE_UNDERSCORE
#define OMPI_IS_FORTRAN_BOTTOM(addr) \
   (addr == (void*) &mpi_fortran_bottom_)
#define OMPI_IS_FORTRAN_IN_PLACE(addr) \
   (addr == (void*) &mpi_fortran_in_place_)
#define OMPI_IS_FORTRAN_UNWEIGHTED(addr) \
   (addr == (void*) &mpi_fortran_unweighted_)
#define OMPI_IS_FORTRAN_WEIGHTS_EMPTY(addr) \
   (addr == (void*) &mpi_fortran_weights_empty_)
#define OMPI_IS_FORTRAN_ARGV_NULL(addr) \
   (addr == (void*) &mpi_fortran_argv_null_)
#define OMPI_IS_FORTRAN_ARGVS_NULL(addr) \
   (addr == (void*) &mpi_fortran_argvs_null_)
#define OMPI_IS_FORTRAN_ERRCODES_IGNORE(addr) \
   (addr == (void*) &mpi_fortran_errcodes_ignore_)
#define OMPI_IS_FORTRAN_STATUS_IGNORE(addr) \
   (addr == (void*) &mpi_fortran_status_ignore_)
#define OMPI_IS_FORTRAN_STATUSES_IGNORE(addr) \
   (addr == (void*) &mpi_fortran_statuses_ignore_)

#elif OMPI_FORTRAN_DOUBLE_UNDERSCORE
#define OMPI_IS_FORTRAN_BOTTOM(addr) \
   (addr == (void*) &mpi_fortran_bottom__)
#define OMPI_IS_FORTRAN_IN_PLACE(addr) \
   (addr == (void*) &mpi_fortran_in_place__)
#define OMPI_IS_FORTRAN_UNWEIGHTED(addr) \
   (addr == (void*) &mpi_fortran_unweighted__)
#define OMPI_IS_FORTRAN_WEIGHTS_EMPTY(addr) \
   (addr == (void*) &mpi_fortran_weights_empty__)
#define OMPI_IS_FORTRAN_ARGV_NULL(addr) \
   (addr == (void*) &mpi_fortran_argv_null__)
#define OMPI_IS_FORTRAN_ARGVS_NULL(addr) \
   (addr == (void*) &mpi_fortran_argvs_null__)
#define OMPI_IS_FORTRAN_ERRCODES_IGNORE(addr) \
   (addr == (void*) &mpi_fortran_errcodes_ignore__)
#define OMPI_IS_FORTRAN_STATUS_IGNORE(addr) \
   (addr == (void*) &mpi_fortran_status_ignore__)
#define OMPI_IS_FORTRAN_STATUSES_IGNORE(addr) \
   (addr == (void*) &mpi_fortran_statuses_ignore__)

#else
#error Unrecognized Fortran name mangling scheme

#endif /* specific symbol type */

/* Convert between Fortran and C MPI_BOTTOM */
#define OMPI_F2C_BOTTOM(addr)      (OMPI_IS_FORTRAN_BOTTOM(addr) ? MPI_BOTTOM : (addr))
#define OMPI_F2C_IN_PLACE(addr)    (OMPI_IS_FORTRAN_IN_PLACE(addr) ? MPI_IN_PLACE : (addr))
#define OMPI_F2C_UNWEIGHTED(addr)  (OMPI_IS_FORTRAN_UNWEIGHTED(addr) ? MPI_UNWEIGHTED : (addr))
#define OMPI_F2C_WEIGHTS_EMPTY(addr)  (OMPI_IS_FORTRAN_WEIGHTS_EMPTY(addr) ? MPI_WEIGHTS_EMPTY : (addr))

#endif /* OMPI_FORTRAN_BASE_CONSTANTS_H */
