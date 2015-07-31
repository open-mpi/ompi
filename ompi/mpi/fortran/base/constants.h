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
 * Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Inria.  All rights reserved.
 * Copyright (c) 2011-2012 Universite Bordeaux 1
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

#define DECL(type, upper_case, lower_case, single_u, double_u)   \
OMPI_DECLSPEC extern type upper_case; \
OMPI_DECLSPEC extern type lower_case; \
OMPI_DECLSPEC extern type single_u; \
OMPI_DECLSPEC extern type double_u

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
 *
 *********
 *
 * These macros have had (at least) 3 generations:
 *
 * Generation 1 / prior to v1.8.8: if you have weak symbols on your
 * platform, the OMPI_IS_FORTRAN_foo() macro would check all four
 * symbols (FOO, foo, foo_, and foo_).  But if you didn't have weak
 * symbols, we had a different set of macros that would check for the
 * one symbol that was relevant for your platform.
 *
 * Generation 2 / v1.8.x, where x>=8: OMPI_IS_FORTRAN_foo() will
 * *always* check all four symbols.
 *
 * Generation 3 / starting with v2.x: OMPI_IS_FORTRAN_foo() will check
 * just the one symbol that is relevant for your compiler/platform
 * (based on what configure figured out).
 *
 *********
 *
 * The Gen3 change was made as a simplification: OMPI now only
 * generates a single C symbol corresponding to each Fortran sentinel
 * value (e.g., MPI_STATUSES_IGNORE) -- instead of always generating 4
 * C symbols, and *one* of them will correspond to the Fortran
 * sentinel.  At the expense of a little perl code to generate some
 * code blocks, other code blocks got simpler and these "always check
 * four symbol" macros went away.
 *
 * The Gen2 change was made because of a bug noticed in the
 * v1.8.6/v1.8.7 timeframe:
 * https://github.com/open-mpi/ompi/issues/739.  Basically: when
 * there's no weak symbols (i.e., either --disable-weak-symbols is
 * used, or when we compile on OS X, which has no weak symbol
 * support), the mpif.h interface and mpi module continued to work
 * fine.  For example, with gfortran 5.x on Linux or OS X, when you
 * passed MPI_STATUSES_IGNORE from an mpif.h/mpi module subroutine, on
 * the back end, it showed up as &mpi_fortran_statuses_ignore_, the
 * OMPI_IS_FORTRAN_STATUSES_IGNORE would correctly identify it as
 * MPI_STATUSES_IGNORE, and life was good.
 */
/*
 * However, we had a bug in the use-mpi-f08/mpi-f08-types.E90 file.
 * Here's an example:
 *
 * type(MPI_STATUS), bind(C, name="mpi_fortran_statuses_ignore") :: MPI_STATUSES_IGNORE(1)
 *
 * Note that it BIND(C)'s to mpi_fortran_statuses_ignore -- note the
 * lack of trailing underscore.  Hence, if you pass
 * MPI_STATUSES_IGNORE from an mpi_f08 subroutine, it'll pass
 * &mpi_fortran_statuses_ignore to C, and the
 * OMPI_IS_FORTRAN_STATUSES_IGNORE macro would fail to identify it
 * correctly.  Bad Things then happened.
 *
 * The minimum-distance fix for the v1.8.8 release was to simply
 * always force comparisons to all four symbols.  Hence, regardless of
 * whether we have weak symbols or not, we'll always check for all
 * four symbols, and we'll always correctly identify all the Fortran
 * sentinel values.
 *
 * This minimum-distance fix for v1.8.8 also means that there are no
 * ABI implications (honestly, there may not have been any ABI
 * implications with bringing the Gen3 change back to v1.8.8, but a)
 * my head hurts just trying to think through the corner cases, and b)
 * the Gen3 change was actually pretty large -- we didn't want such a
 * large change right before the v1.8.8 release).
 */
#define OMPI_IS_FORTRAN_BOTTOM(addr) \
  (addr == (void*) &MPI_FORTRAN_BOTTOM || \
   addr == (void*) &mpi_fortran_bottom || \
   addr == (void*) &mpi_fortran_bottom_ || \
   addr == (void*) &mpi_fortran_bottom__)
#define OMPI_IS_FORTRAN_IN_PLACE(addr) \
  (addr == (void*) &MPI_FORTRAN_IN_PLACE || \
   addr == (void*) &mpi_fortran_in_place || \
   addr == (void*) &mpi_fortran_in_place_ || \
   addr == (void*) &mpi_fortran_in_place__)
#define OMPI_IS_FORTRAN_UNWEIGHTED(addr) \
  (addr == (void*) &MPI_FORTRAN_UNWEIGHTED || \
   addr == (void*) &mpi_fortran_unweighted || \
   addr == (void*) &mpi_fortran_unweighted_ || \
   addr == (void*) &mpi_fortran_unweighted__)
#define OMPI_IS_FORTRAN_WEIGHTS_EMPTY(addr) \
  (addr == (void*) &MPI_FORTRAN_WEIGHTS_EMPTY || \
   addr == (void*) &mpi_fortran_weights_empty || \
   addr == (void*) &mpi_fortran_weights_empty_ || \
   addr == (void*) &mpi_fortran_weights_empty__)
#define OMPI_IS_FORTRAN_ARGV_NULL(addr) \
  (addr == (void*) &MPI_FORTRAN_ARGV_NULL || \
   addr == (void*) &mpi_fortran_argv_null || \
   addr == (void*) &mpi_fortran_argv_null_ || \
   addr == (void*) &mpi_fortran_argv_null__)
#define OMPI_IS_FORTRAN_ARGVS_NULL(addr) \
  (addr == (void*) &MPI_FORTRAN_ARGVS_NULL || \
   addr == (void*) &mpi_fortran_argvs_null || \
   addr == (void*) &mpi_fortran_argvs_null_ || \
   addr == (void*) &mpi_fortran_argvs_null__)
#define OMPI_IS_FORTRAN_ERRCODES_IGNORE(addr) \
  (addr == (void*) &MPI_FORTRAN_ERRCODES_IGNORE || \
   addr == (void*) &mpi_fortran_errcodes_ignore || \
   addr == (void*) &mpi_fortran_errcodes_ignore_ || \
   addr == (void*) &mpi_fortran_errcodes_ignore__)
#define OMPI_IS_FORTRAN_STATUS_IGNORE(addr) \
  (addr == (void*) &MPI_FORTRAN_STATUS_IGNORE || \
   addr == (void*) &mpi_fortran_status_ignore || \
   addr == (void*) &mpi_fortran_status_ignore_ || \
   addr == (void*) &mpi_fortran_status_ignore__)
#define OMPI_IS_FORTRAN_STATUSES_IGNORE(addr) \
  (addr == (void*) &MPI_FORTRAN_STATUSES_IGNORE || \
   addr == (void*) &mpi_fortran_statuses_ignore || \
   addr == (void*) &mpi_fortran_statuses_ignore_ || \
   addr == (void*) &mpi_fortran_statuses_ignore__)

/* Convert between Fortran and C MPI_BOTTOM */
#define OMPI_F2C_BOTTOM(addr)      (OMPI_IS_FORTRAN_BOTTOM(addr) ? MPI_BOTTOM : (addr))
#define OMPI_F2C_IN_PLACE(addr)    (OMPI_IS_FORTRAN_IN_PLACE(addr) ? MPI_IN_PLACE : (addr))
#define OMPI_F2C_UNWEIGHTED(addr)  (OMPI_IS_FORTRAN_UNWEIGHTED(addr) ? MPI_UNWEIGHTED : (addr))
#define OMPI_F2C_WEIGHTS_EMPTY(addr)  (OMPI_IS_FORTRAN_WEIGHTS_EMPTY(addr) ? MPI_WEIGHTS_EMPTY : (addr))

#endif /* OMPI_FORTRAN_BASE_CONSTANTS_H */
