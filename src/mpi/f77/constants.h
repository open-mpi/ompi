/*
 * $HEADER$
 */

#ifndef OMPI_F77_CONSTANTS_H
#define OMPI_F77_CONSTANTS_H


/*
 * Several variables are used to link against MPI F77 constants which
 * correspond to addresses, e.g. MPI_BOTTOM, and are implemented via
 * common blocks.  They must have the same size and alignment
 * constraints as the corresponding F77 common blocks.
 *
 * We use common blocks so that in the C wrapper functions, we can
 * compare the address that comes in against known addresses (e.g., if
 * the "status" argument in MPI_RECV is the address of the common
 * block for the fortran equivalent of MPI_STATUS_IGNORE, then we know
 * to pass the C MPI_STATUS_IGNORE to the C MPI_Recv function.
 *
 * This mojo makes a type that will be aligned on 16 bytes (same as
 * common blocks -- at least it seems to work with all the fortran
 * compilers that we care about... haven't found one yet that doesn't
 * work...)
 */

#if defined(HAVE_LONG_DOUBLE) && OMPI_ALIGNMENT_LONG_DOUBLE == 16
typedef struct { long double bogus; } ompi_fortran_common_t;
#else
typedef struct { double bogus[2]; } ompi_fortran_common_t;
#endif

/*
 * This part sucks.  :-(
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
 * DOUBLE PRECISION MPI_STATUS_IGNORE
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

#define DECL(upper_case, lower_case, single_u, double_u) \
OMPI_DECLSPEC extern ompi_fortran_common_t upper_case; \
OMPI_DECLSPEC extern ompi_fortran_common_t lower_case; \
OMPI_DECLSPEC extern ompi_fortran_common_t single_u; \
OMPI_DECLSPEC extern ompi_fortran_common_t double_u

DECL(MPI_FORTRAN_BOTTOM, mpi_fortran_bottom,
     mpi_fortran_bottom_, mpi_fortran_bottom__);
DECL(MPI_FORTRAN_ARGV_NULL, mpi_fortran_argv_null,
     mpi_fortran_argv_null_, mpi_fortran_argv_null__);
DECL(MPI_FORTRAN_ARGVS_NULL, mpi_fortran_argvs_null,
     mpi_fortran_argvs_null_, mpi_fortran_argvs_null__);
DECL(MPI_FORTRAN_ERRCODES_IGNORE, mpi_fortran_errcodes_ignore,
     mpi_fortran_errcodes_ignore_, mpi_fortran_errcodes_ignore__);
DECL(MPI_FORTRAN_STATUS_IGNORE, mpi_fortran_status_ignore,
     mpi_fortran_status_ignore_, mpi_fortran_status_ignore__);
DECL(MPI_FORTRAN_STATUSES_IGNORE, mpi_fortran_statuses_ignore,
     mpi_fortran_statuses_ignore_, mpi_fortran_statuses_ignore__);

/*
 * Create macros to do the checking.  Only check for all 4 if we have
 * weak symbols.  Otherwise, just check for the one relevant symbol.
 */

#if OMPI_HAVE_WEAK_SYMBOLS
#define OMPI_IS_FORTRAN_BOTTOM(addr) \
  (addr == (void*) &MPI_FORTRAN_BOTTOM || \
   addr == (void*) &mpi_fortran_bottom || \
   addr == (void*) &mpi_fortran_bottom_ || \
   addr == (void*) &mpi_fortran_bottom__)
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

#elif OMPI_F77_CAPS
#define OMPI_IS_FORTRAN_BOTTOM(addr) \
  (addr == (void*) &MPI_FORTRAN_BOTTOM)
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

#elif OMPI_F77_PLAIN
#define OMPI_IS_FORTRAN_BOTTOM(addr) \
   (addr == (void*) &mpi_fortran_bottom)
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

#elif OMPI_F77_SINGLE_UNDERSCORE
#define OMPI_IS_FORTRAN_BOTTOM(addr) \
   (addr == (void*) &mpi_fortran_bottom)
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

#else
#define OMPI_IS_FORTRAN_BOTTOM(addr) \
   (addr == (void*) &mpi_fortran_bottom)
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

#endif /* weak / specific symbol type */

#endif /* OMPI_F77_CONSTANTS_H */
