/*
 * $HEADER$
 */

/** @file **/

#include "ompi_config.h"

#include "include/constants.h"
#include "op/op.h"
#include "op/op_predefined.h"
#include "class/ompi_pointer_array.h"


/*
 * Table for Fortran <-> C op handle conversion
 */
ompi_pointer_array_t *ompi_op_f_to_c_table;


/*
 * Create intrinsic op
 */
static int add_intrinsic(ompi_op_t *op, int fort_handle);


/*
 * Class information
 */
static void ompi_op_construct(ompi_op_t *eh);
static void ompi_op_destruct(ompi_op_t *eh);


/*
 * Class instance
 */
OBJ_CLASS_INSTANCE(ompi_op_t, ompi_object_t, ompi_op_construct, ompi_op_destruct);


/*
 * MPI_OP_NULL
 */
ompi_op_t ompi_mpi_op_null = {
    { NULL, 0 },

    "MPI_OP_NULL",
    true, false, true, true,
    { NULL }
};


/*
 * MPI_OP_MAX
 */
ompi_op_t ompi_mpi_op_max = {
    { NULL, 0 },

    "MPI_OP_MAX",
    true, false, true, true,
    { ompi_mpi_op_max_func }
};


/*
 * MPI_OP_MIN
 */
ompi_op_t ompi_mpi_op_min = {
    { NULL, 0 },

    "MPI_OP_MIN",
    true, false, true, true,
    { ompi_mpi_op_min_func }
};


/*
 * MPI_OP_SUM
 */
ompi_op_t ompi_mpi_op_sum = {
    { NULL, 0 },

    "MPI_OP_SUM",
    true, false, true, true,
    { ompi_mpi_op_sum_func }
};


/*
 * MPI_OP_PROD
 */
ompi_op_t ompi_mpi_op_prod = {
    { NULL, 0 },

    "MPI_OP_PROD",
    true, false, true, true,
    { ompi_mpi_op_prod_func }
};


/*
 * MPI_OP_LAND
 */
ompi_op_t ompi_mpi_op_land = {
    { NULL, 0 },

    "MPI_OP_LAND",
    true, false, true, true,
    { ompi_mpi_op_land_func }
};


/*
 * MPI_OP_BAND
 */
ompi_op_t ompi_mpi_op_band = {
    { NULL, 0 },

    "MPI_OP_BAND",
    true, false, true, true,
    { ompi_mpi_op_band_func }
};


/*
 * MPI_OP_LOR
 */
ompi_op_t ompi_mpi_op_lor = {
    { NULL, 0 },

    "MPI_OP_LOR",
    true, false, true, true,
    { ompi_mpi_op_lor_func }
};


/*
 * MPI_OP_BOR
 */
ompi_op_t ompi_mpi_op_bor = {
    { NULL, 0 },

    "MPI_OP_BOR",
    true, false, true, true,
    { ompi_mpi_op_bor_func }
};


/*
 * MPI_OP_LXOR
 */
ompi_op_t ompi_mpi_op_lxor = {
    { NULL, 0 },

    "MPI_OP_LXOR",
    true, false, true, true,
    { ompi_mpi_op_lxor_func }
};


/*
 * MPI_OP_BXOR
 */
ompi_op_t ompi_mpi_op_bxor = {
    { NULL, 0 },

    "MPI_OP_BXOR",
    true, false, true, true,
    { ompi_mpi_op_bxor_func }
};


/*
 * MPI_OP_MAXLOC
 */
ompi_op_t ompi_mpi_op_maxloc = {
    { NULL, 0 },

    "MPI_OP_MAXLOC",
    true, false, true, true,
    { ompi_mpi_op_maxloc_func }
};


/*
 * MPI_OP_MINLOC
 */
ompi_op_t ompi_mpi_op_minloc = {
    { NULL, 0 },

    "MPI_OP_MINLOC",
    true, false, true, true,
    { ompi_mpi_op_minloc_func }
};


/*
 * MPI_OP_REPLACE
 */
ompi_op_t ompi_mpi_op_replace = {
    { NULL, 0 },

    "MPI_OP_REPLACE",
    true, false, true, true,
    { ompi_mpi_op_replace_func }
};


/*
 * Initialize OMPI op infrastructure
 */
int ompi_op_init(void)
{
  /* initialize ompi_op_f_to_c_table */

  ompi_op_f_to_c_table = OBJ_NEW(ompi_pointer_array_t);
  if (NULL == ompi_op_f_to_c_table){
    return OMPI_ERROR;
  }

  /* Create the intrinsic ops */

  if (add_intrinsic(&ompi_mpi_op_null, OMPI_OP_NULL_FORTRAN) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_max, OMPI_OP_MAX_FORTRAN) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_min, OMPI_OP_MIN_FORTRAN) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_sum, OMPI_OP_SUM_FORTRAN) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_prod, OMPI_OP_PROD_FORTRAN) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_land, OMPI_OP_LAND_FORTRAN) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_band, OMPI_OP_BAND_FORTRAN) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_lor, OMPI_OP_LOR_FORTRAN) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_bor, OMPI_OP_BOR_FORTRAN) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_lxor, OMPI_OP_LXOR_FORTRAN) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_bxor, OMPI_OP_BXOR_FORTRAN) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_maxloc, 
                    OMPI_OP_MAXLOC_FORTRAN) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_minloc, 
                    OMPI_OP_MINLOC_FORTRAN) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_replace, 
                    OMPI_OP_REPLACE_FORTRAN) != OMPI_SUCCESS) {
    return OMPI_ERROR;
  }

  /* All done */

  return OMPI_SUCCESS;
}


/*
 * Clean up the op resources
 */
int ompi_op_finalize(void)
{
  /* Remove op F2C table */
  
  OBJ_RELEASE(ompi_op_f_to_c_table);
  
  /* All done */

  return OMPI_SUCCESS;
}


ompi_op_t *ompi_op_create(bool commute,
                        ompi_op_fortran_handler_fn_t *func)
{
  ompi_op_t *new_op;

  /* Create a new object and ensure that it's valid */

  new_op = OBJ_NEW(ompi_op_t);
  if (NULL == new_op) {
    if (OMPI_ERROR == new_op->o_f_to_c_index) {
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

  return OMPI_SUCCESS;
}


/**************************************************************************
 *
 * Static functions
 *
 **************************************************************************/

static int add_intrinsic(ompi_op_t *op, int fort_handle)
{
  int ret_val;

  /* Add the op to the table */

  ret_val = ompi_pointer_array_add(ompi_op_f_to_c_table, op);
  if (-1 == ret_val){
    return OMPI_ERROR;
  }

  /* Make sure that the op is in the right location in the table */

  if (fort_handle != ret_val) {
    return OMPI_ERROR;
  };
  op->o_f_to_c_index = ret_val;

  /* All done */

  return OMPI_SUCCESS;
}  


/*
 * Op constructor
 */
static void ompi_op_construct(ompi_op_t *new_op)
{
  int ret_val;

  /* assign entry in fortran <-> c translation array */

  ret_val = ompi_pointer_array_add(ompi_op_f_to_c_table, 
                                  new_op);
  new_op->o_f_to_c_index = ret_val;
}


/*
 * Op destructor
 */
static void ompi_op_destruct(ompi_op_t *op)
{
  /* reset the ompi_op_f_to_c_table entry - make sure that the
     entry is in the table */

  if (NULL!= ompi_pointer_array_get_item(ompi_op_f_to_c_table,
                                        op->o_f_to_c_index)) {
    ompi_pointer_array_set_item(ompi_op_f_to_c_table,
                               op->o_f_to_c_index, NULL);
  }
}
