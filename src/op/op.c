/*
 * $HEADER$
 */

/** @file **/

#include "lam_config.h"

#include "include/constants.h"
#include "op/op.h"
#include "op/op_predefined.h"
#include "lfc/lam_pointer_array.h"


/*
 * Table for Fortran <-> C op handle conversion
 */
lam_pointer_array_t *lam_op_f_to_c_table;


/*
 * Create intrinsic op
 */
static int add_intrinsic(lam_op_t *op, int fort_handle);


/*
 * Class information
 */
static void lam_op_construct(lam_op_t *eh);
static void lam_op_destruct(lam_op_t *eh);


/*
 * Class instance
 */
OBJ_CLASS_INSTANCE(lam_op_t, lam_object_t, lam_op_construct, lam_op_destruct);


/*
 * MPI_OP_NULL
 */
lam_op_t lam_mpi_op_null = {
    { NULL, 0 },

    "MPI_OP_NULL",
    true, false, true, true,
    { NULL }
};


/*
 * MPI_OP_MAX
 */
lam_op_t lam_mpi_op_max = {
    { NULL, 0 },

    "MPI_OP_MAX",
    true, false, true, true,
    { lam_mpi_op_max_func }
};


/*
 * MPI_OP_MIN
 */
lam_op_t lam_mpi_op_min = {
    { NULL, 0 },

    "MPI_OP_MIN",
    true, false, true, true,
    { lam_mpi_op_min_func }
};


/*
 * MPI_OP_SUM
 */
lam_op_t lam_mpi_op_sum = {
    { NULL, 0 },

    "MPI_OP_SUM",
    true, false, true, true,
    { lam_mpi_op_sum_func }
};


/*
 * MPI_OP_PROD
 */
lam_op_t lam_mpi_op_prod = {
    { NULL, 0 },

    "MPI_OP_PROD",
    true, false, true, true,
    { lam_mpi_op_prod_func }
};


/*
 * MPI_OP_LAND
 */
lam_op_t lam_mpi_op_land = {
    { NULL, 0 },

    "MPI_OP_LAND",
    true, false, true, true,
    { lam_mpi_op_land_func }
};


/*
 * MPI_OP_BAND
 */
lam_op_t lam_mpi_op_band = {
    { NULL, 0 },

    "MPI_OP_BAND",
    true, false, true, true,
    { lam_mpi_op_band_func }
};


/*
 * MPI_OP_LOR
 */
lam_op_t lam_mpi_op_lor = {
    { NULL, 0 },

    "MPI_OP_LOR",
    true, false, true, true,
    { lam_mpi_op_lor_func }
};


/*
 * MPI_OP_BOR
 */
lam_op_t lam_mpi_op_bor = {
    { NULL, 0 },

    "MPI_OP_BOR",
    true, false, true, true,
    { lam_mpi_op_bor_func }
};


/*
 * MPI_OP_LXOR
 */
lam_op_t lam_mpi_op_lxor = {
    { NULL, 0 },

    "MPI_OP_LXOR",
    true, false, true, true,
    { lam_mpi_op_lxor_func }
};


/*
 * MPI_OP_BXOR
 */
lam_op_t lam_mpi_op_bxor = {
    { NULL, 0 },

    "MPI_OP_BXOR",
    true, false, true, true,
    { lam_mpi_op_bxor_func }
};


/*
 * MPI_OP_MAXLOC
 */
lam_op_t lam_mpi_op_maxloc = {
    { NULL, 0 },

    "MPI_OP_MAXLOC",
    true, false, true, true,
    { lam_mpi_op_maxloc_func }
};


/*
 * MPI_OP_MINLOC
 */
lam_op_t lam_mpi_op_minloc = {
    { NULL, 0 },

    "MPI_OP_MINLOC",
    true, false, true, true,
    { lam_mpi_op_minloc_func }
};


/*
 * MPI_OP_REPLACE
 */
lam_op_t lam_mpi_op_replace = {
    { NULL, 0 },

    "MPI_OP_REPLACE",
    true, false, true, true,
    { lam_mpi_op_replace_func }
};


/*
 * Initialize LAM op infrastructure
 */
int lam_op_init(void)
{
  /* initialize lam_op_f_to_c_table */

  lam_op_f_to_c_table = OBJ_NEW(lam_pointer_array_t);
  if (NULL == lam_op_f_to_c_table){
    return LAM_ERROR;
  }

  /* Create the intrinsic ops */

  if (add_intrinsic(&lam_mpi_op_null, LAM_OP_NULL_FORTRAN) != LAM_SUCCESS ||
      add_intrinsic(&lam_mpi_op_max, LAM_OP_MAX_FORTRAN) != LAM_SUCCESS ||
      add_intrinsic(&lam_mpi_op_min, LAM_OP_MIN_FORTRAN) != LAM_SUCCESS ||
      add_intrinsic(&lam_mpi_op_sum, LAM_OP_SUM_FORTRAN) != LAM_SUCCESS ||
      add_intrinsic(&lam_mpi_op_prod, LAM_OP_PROD_FORTRAN) != LAM_SUCCESS ||
      add_intrinsic(&lam_mpi_op_land, LAM_OP_LAND_FORTRAN) != LAM_SUCCESS ||
      add_intrinsic(&lam_mpi_op_band, LAM_OP_BAND_FORTRAN) != LAM_SUCCESS ||
      add_intrinsic(&lam_mpi_op_lor, LAM_OP_LOR_FORTRAN) != LAM_SUCCESS ||
      add_intrinsic(&lam_mpi_op_bor, LAM_OP_BOR_FORTRAN) != LAM_SUCCESS ||
      add_intrinsic(&lam_mpi_op_lxor, LAM_OP_LXOR_FORTRAN) != LAM_SUCCESS ||
      add_intrinsic(&lam_mpi_op_bxor, LAM_OP_BXOR_FORTRAN) != LAM_SUCCESS ||
      add_intrinsic(&lam_mpi_op_maxloc, 
                    LAM_OP_MAXLOC_FORTRAN) != LAM_SUCCESS ||
      add_intrinsic(&lam_mpi_op_minloc, 
                    LAM_OP_MINLOC_FORTRAN) != LAM_SUCCESS ||
      add_intrinsic(&lam_mpi_op_replace, 
                    LAM_OP_REPLACE_FORTRAN) != LAM_SUCCESS) {
    return LAM_ERROR;
  }

  /* All done */

  return LAM_SUCCESS;
}


/*
 * Clean up the op resources
 */
int lam_op_finalize(void)
{
  /* Remove op F2C table */
  
  OBJ_RELEASE(lam_op_f_to_c_table);
  
  /* All done */

  return LAM_SUCCESS;
}


lam_op_t *lam_op_create(bool commute,
                        lam_op_fortran_handler_fn_t *func)
{
  lam_op_t *new_op;

  /* Create a new object and ensure that it's valid */

  new_op = OBJ_NEW(lam_op_t);
  if (NULL == new_op) {
    if (LAM_ERROR == new_op->o_f_to_c_index) {
      OBJ_RELEASE(new_op);
      new_op = NULL;
    } else {

      /* The new object is valid -- initialize it.  If this is being
         created from fortran, the fortran MPI API wrapper function
         will override the o_fortran_field directly.  We cast the
         function pointer type to the fortran type arbitrarily -- it
         only has to be a function pointer in order to store properly,
         it doesn't matter what type it is (we'll cast it to the Right
         type when we *use* it). */

      new_op->o_is_intrinsic = false;
      new_op->o_fortran_function = false;
      new_op->o_is_assoc = true;
      new_op->o_is_commute = commute;
      new_op->o_func.fort_fn = func;
    }
  }

  /* All done */

  return LAM_SUCCESS;
}


/**************************************************************************
 *
 * Static functions
 *
 **************************************************************************/

static int add_intrinsic(lam_op_t *op, int fort_handle)
{
  int ret_val;

  /* Add the op to the table */

  ret_val = lam_pointer_array_add(lam_op_f_to_c_table, op);
  if (-1 == ret_val){
    return LAM_ERROR;
  }

  /* Make sure that the op is in the right location in the table */

  if (fort_handle != ret_val) {
    return LAM_ERROR;
  };
  op->o_f_to_c_index = ret_val;

  /* All done */

  return LAM_SUCCESS;
}  


/*
 * Op constructor
 */
static void lam_op_construct(lam_op_t *new_op)
{
  int ret_val;

  /* assign entry in fortran <-> c translation array */

  ret_val = lam_pointer_array_add(lam_op_f_to_c_table, 
                                  new_op);
  new_op->o_f_to_c_index = ret_val;
}


/*
 * Op destructor
 */
static void lam_op_destruct(lam_op_t *op)
{
  /* reset the lam_op_f_to_c_table entry - make sure that the
     entry is in the table */

  if (NULL!= lam_pointer_array_get_item(lam_op_f_to_c_table,
                                        op->o_f_to_c_index)) {
    lam_pointer_array_set_item(lam_op_f_to_c_table,
                               op->o_f_to_c_index, NULL);
  }
}
