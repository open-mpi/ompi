/*
 * $HEADER$
 */

#ifndef OMPI_OP_H
#define OMPI_OP_H

#include "ompi_config.h"

#include "mpi.h"
#include "class/ompi_object.h"
#include "class/ompi_pointer_array.h"


/*
 * These must correspond to the fortran handle indices
 */
#define MPI_OP_MAX_FORTRAN 0
#define MPI_OP_MIN_FORTRAN 1
#define MPI_OP_SUM_FORTRAN 2
#define MPI_OP_PROD_FORTRAN 3
#define MPI_OP_LAND_FORTRAN 4
#define MPI_OP_BAND_FORTRAN 5
#define MPI_OP_LOR_FORTRAN 6
#define MPI_OP_BOR_FORTRAN 7
#define MPI_OP_LXOR_FORTRAN 8
#define MPI_OP_BXOR_FORTRAN 9
#define MPI_OP_MAXLOC_FORTRAN 10
#define MPI_OP_MINLOC_FORTRAN 11
#define MPI_OP_REPLACE_FORTRAN 12


/**
 * Typedef for C op functions.  We don't use MPI_User_function because
 * this would create a confusing dependency loop between this file and
 * mpi.h.
 */
typedef void (ompi_op_c_handler_fn_t)(void *, void *, int *, MPI_Datatype *);


/**
 * Typedef for fortran op functions
 */
typedef void (ompi_op_fortran_handler_fn_t)(void *, void *, int *, MPI_Fint *);


/**
 * Back-end type of MPI_Op
 */
struct ompi_op_t {
  ompi_object_t super;

  char o_name[MPI_MAX_OBJECT_NAME];

  /* Flags about the op */

  bool o_is_intrinsic;
  bool o_fortran_function;
  bool o_is_assoc;
  bool o_is_commute;

  /* Function pointers */

  union {
    ompi_op_c_handler_fn_t *c_fn;
    ompi_op_fortran_handler_fn_t *fort_fn;
  } o_func;

  /* index in Fortran <-> C translation array */

  int o_f_to_c_index;
};
typedef struct ompi_op_t ompi_op_t;


/**
 * Global variable for MPI_NULL
 */
extern ompi_op_t ompi_mpi_op_null;
#define OMPI_OP_NULL_FORTRAN 0

/**
 * Global variable for MPI_MAX
 */
extern ompi_op_t ompi_mpi_op_max;
#define OMPI_OP_MAX_FORTRAN 1

/**
 * Global variable for MPI_MIN
 */
extern ompi_op_t ompi_mpi_op_min;
#define OMPI_OP_MIN_FORTRAN 2

/**
 * Global variable for MPI_SUM
 */
extern ompi_op_t ompi_mpi_op_sum;
#define OMPI_OP_SUM_FORTRAN 3

/**
 * Global variable for MPI_PROD
 */
extern ompi_op_t ompi_mpi_op_prod;
#define OMPI_OP_PROD_FORTRAN 4

/**
 * Global variable for MPI_LAND
 */
extern ompi_op_t ompi_mpi_op_land;
#define OMPI_OP_LAND_FORTRAN 5

/**
 * Global variable for MPI_BAND
 */
extern ompi_op_t ompi_mpi_op_band;
#define OMPI_OP_BAND_FORTRAN 6

/**
 * Global variable for MPI_LOR
 */
extern ompi_op_t ompi_mpi_op_lor;
#define OMPI_OP_LOR_FORTRAN 7

/**
 * Global variable for MPI_BOR
 */
extern ompi_op_t ompi_mpi_op_bor;
#define OMPI_OP_BOR_FORTRAN 8

/**
 * Global variable for MPI_LXOR
 */
extern ompi_op_t ompi_mpi_op_lxor;
#define OMPI_OP_LXOR_FORTRAN 9

/**
 * Global variable for MPI_BXOR
 */
extern ompi_op_t ompi_mpi_op_bxor;
#define OMPI_OP_BXOR_FORTRAN 10

/**
 * Global variable for MPI_MAXLOC
 */
extern ompi_op_t ompi_mpi_op_maxloc;
#define OMPI_OP_MAXLOC_FORTRAN 11

/**
 * Global variable for MPI_MINLOC
 */
extern ompi_op_t ompi_mpi_op_minloc;
#define OMPI_OP_MINLOC_FORTRAN 12

/**
 * Global variable for MPI_REPLACE
 */
extern ompi_op_t ompi_mpi_op_replace;
#define OMPI_OP_REPLACE_FORTRAN 13


/**
 * Table for Fortran <-> C op handle conversion
 */
extern ompi_pointer_array_t *ompi_op_f_to_c_table;


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

  /**
   * Initialize the op interface.
   *
   * @returns OMPI_SUCCESS Upon success
   * @returns OMPI_ERROR Otherwise
   *
   * Invoked from ompi_mpi_init(); sets up the op interface, creates
   * the predefined MPI operations, and creates the corresopnding F2C
   * translation table.
   */
  int ompi_op_init(void);

  /**
   * Finalize the op interface.
   *
   * @returns OMPI_SUCCESS Always
   *
   * Invokes from ompi_mpi_finalize(); tears down the op interface, and
   * destroys the F2C translation table.
   */
  int ompi_op_finalize(void);

  /**
   * Create a ompi_op_t
   *
   * @param commute Boolean indicating whether the operation is
   *        communative or not
   * @param func Function pointer of the error handler
   *
   * @returns op Pointer to the ompi_op_t that will be
   *   created and returned
   *
   * This function is called as the back-end of all the MPI_OP_CREATE
   * functions.  It creates a new ompi_op_t object, initializes it to
   * the correct object type, and sets the callback function on it.
   *
   * The type of the function pointer is (arbitrarily) the fortran
   * function handler type.  Since this function has to accept 2
   * different function pointer types (lest we have 2 different
   * functions to create errhandlers), the fortran one was picked
   * arbitrarily.  Note that (void*) is not sufficient because at
   * least theoretically, a sizeof(void*) may not necessarily be the
   * same as sizeof(void(*)).
   *
   * NOTE: It *always* sets the "fortran" flag to false.  The Fortran
   * wrapper for MPI_OP_CREATE is expected to reset this flag to true
   * manually.
   */
  ompi_op_t *ompi_op_create(bool commute, ompi_op_fortran_handler_fn_t *func);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/**
 * Check to see if an op is intrinsic.
 *
 * @param op The op to check
 *
 * @returns true If the op is intrinsic
 * @returns false If the op is not intrinsic
 *
 * Self-explanitory.  This is needed in a few top-level MPI functions;
 * this function is provided to hide the internal structure field
 * names.
 */
static inline bool ompi_op_is_intrinsic(ompi_op_t *op)
{
  return op->o_is_intrinsic;
}

#endif /* OMPI_OP_H */
