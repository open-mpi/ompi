/*
 * $HEADER$
 */

#ifndef LAM_OP_H
#define LAM_OP_H

#include "lam_config.h"

#include "mpi.h"
#include "lfc/lam_object.h"
#include "lfc/lam_pointer_array.h"


/*
 * These must correspond to the fortran handle indices
 */
#define MPI_MAX 0
#define MPI_MIN 1
#define MPI_SUM 2
#define MPI_PROD 3
#define MPI_LAND 4
#define MPI_BAND 5
#define MPI_LOR 6
#define MPI_BOR 7
#define MPI_LXOR 8
#define MPI_BXOR 9
#define MPI_MAXLOC 10
#define MPI_MINLOC 11
#define MPI_REPLACE 12


/**
 * Typedef for C op functions.  We don't use MPI_User_function because
 * this would create a confusing dependency loop between this file and
 * mpi.h.
 */
typedef void (lam_op_c_handler_fn_t)(void *, void *, int *, MPI_Datatype *);


/**
 * Typedef for fortran op functions
 */
typedef void (lam_op_fortran_handler_fn_t)(void *, void *, int *, MPI_Fint *);


/**
 * Back-end type of MPI_Op
 */
struct lam_op_t {
  lam_object_t super;

  char o_name[MPI_MAX_OBJECT_NAME];

  /* Flags about the op */

  bool o_is_intrinsic;
  bool o_fortran_function;
  bool o_is_assoc;
  bool o_is_commun;

  /* Function pointers */

  union {
    lam_op_c_handler_fn_t *c_fn;
    lam_op_fortran_handler_fn_t *fort_fn;
  } o_func;

  /* index in Fortran <-> C translation array */

  int o_f_to_c_index;
};
typedef struct lam_op_t lam_op_t;


/**
 * Global variable for MPI_NULL
 */
extern lam_op_t lam_mpi_op_null;
#define LAM_OP_NULL_FORTRAN 0

/**
 * Global variable for MPI_MAX
 */
extern lam_op_t lam_mpi_op_max;
#define LAM_OP_MAX_FORTRAN 1

/**
 * Global variable for MPI_MIN
 */
extern lam_op_t lam_mpi_op_min;
#define LAM_OP_MIN_FORTRAN 2

/**
 * Global variable for MPI_SUM
 */
extern lam_op_t lam_mpi_op_sum;
#define LAM_OP_SUM_FORTRAN 3

/**
 * Global variable for MPI_PROD
 */
extern lam_op_t lam_mpi_op_prod;
#define LAM_OP_PROD_FORTRAN 4

/**
 * Global variable for MPI_LAND
 */
extern lam_op_t lam_mpi_op_land;
#define LAM_OP_LAND_FORTRAN 5

/**
 * Global variable for MPI_BAND
 */
extern lam_op_t lam_mpi_op_band;
#define LAM_OP_BAND_FORTRAN 6

/**
 * Global variable for MPI_LOR
 */
extern lam_op_t lam_mpi_op_lor;
#define LAM_OP_LOR_FORTRAN 7

/**
 * Global variable for MPI_BOR
 */
extern lam_op_t lam_mpi_op_bor;
#define LAM_OP_BOR_FORTRAN 8

/**
 * Global variable for MPI_LXOR
 */
extern lam_op_t lam_mpi_op_lxor;
#define LAM_OP_LXOR_FORTRAN 9

/**
 * Global variable for MPI_BXOR
 */
extern lam_op_t lam_mpi_op_bxor;
#define LAM_OP_BXOR_FORTRAN 10

/**
 * Global variable for MPI_MAXLOC
 */
extern lam_op_t lam_mpi_op_maxloc;
#define LAM_OP_MAXLOC_FORTRAN 11

/**
 * Global variable for MPI_MINLOC
 */
extern lam_op_t lam_mpi_op_minloc;
#define LAM_OP_MINLOC_FORTRAN 12

/**
 * Global variable for MPI_REPLACE
 */
extern lam_op_t lam_mpi_op_replace;
#define LAM_OP_REPLACE_FORTRAN 13


/**
 * Table for Fortran <-> C op handle conversion
 */
extern lam_pointer_array_t *lam_op_f_to_c_table;


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

  /**
   * Initialize the op interface.
   *
   * @returns LAM_SUCCESS Upon success
   * @returns LAM_ERROR Otherwise
   *
   * Invoked from lam_mpi_init(); sets up the op interface, creates
   * the predefined MPI operations, and creates the corresopnding F2C
   * translation table.
   */
  int lam_op_init(void);

  /**
   * Finalize the op interface.
   *
   * @returns LAM_SUCCESS Always
   *
   * Invokes from lam_mpi_finalize(); tears down the op interface, and
   * destroys the F2C translation table.
   */
  int lam_op_finalize(void);

#if 0
  /**
   * Create a lam_op_t
   *
   * JMS Need to fill in -- template off lam_errhandler_create().
   */
  lam_op_t *lam_op_create(lam_op_fortran_handler_fn_t *func);
#endif
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* LAM_OP_H */
