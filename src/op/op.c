/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "op/op.h"
#include "op/op_predefined.h"
#include "class/ompi_pointer_array.h"
#include "datatype/datatype_internal.h"


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
OBJ_CLASS_INSTANCE(ompi_op_t, ompi_object_t, 
                   ompi_op_construct, ompi_op_destruct);


/*
 * Helpful defines, because there's soooo many names!
 */
#define C_INTEGER(name) \
  { ompi_mpi_op_##name##_int },            /* OMPI_OP_TYPE_INT */ \
  { ompi_mpi_op_##name##_long },           /* OMPI_OP_TYPE_LONG */ \
  { ompi_mpi_op_##name##_short },          /* OMPI_OP_TYPE_SHORT */ \
  { ompi_mpi_op_##name##_unsigned_short }, /* OMPI_OP_TYPE_UNSIGNED_SHORT */ \
  { ompi_mpi_op_##name##_unsigned },       /* OMPI_OP_TYPE_UNSIGNED */ \
  { ompi_mpi_op_##name##_unsigned_long }   /* OMPI_OP_TYPE_UNSIGNED_LONG */
#define C_INTEGER_NULL \
  { NULL }, /* OMPI_OP_TYPE_INT */ \
  { NULL }, /* OMPI_OP_TYPE_LONG */ \
  { NULL }, /* OMPI_OP_TYPE_SHORT */ \
  { NULL }, /* OMPI_OP_TYPE_UNSIGNED_SHORT */ \
  { NULL }, /* OMPI_OP_TYPE_UNSIGNED */ \
  { NULL }  /* OMPI_OP_TYPE_UNSIGNED_LONG */

#define FORTRAN_INTEGER(name) \
  { ompi_mpi_op_##name##_fortran_integer }  /* OMPI_OP_TYPE_INTEGER */
#define FORTRAN_INTEGER_NULL \
  { NULL }  /* OMPI_OP_TYPE_INTEGER */

#define FLOATING_POINT(name) \
  { ompi_mpi_op_##name##_float },                    /* OMPI_OP_TYPE_FLOAT */\
  { ompi_mpi_op_##name##_double },                   /* OMPI_OP_TYPE_DOUBLE */\
  { ompi_mpi_op_##name##_fortran_real },             /* OMPI_OP_TYPE_REAL */ \
  { ompi_mpi_op_##name##_fortran_double_precision }, /* OMPI_OP_TYPE_DOUBLE_PRECISION */ \
  { ompi_mpi_op_##name##_long_double }               /* OMPI_OP_TYPE_LONG_DOUBLE */
#define FLOATING_POINT_NULL \
  { NULL }, /* OMPI_OP_TYPE_FLOAT */ \
  { NULL }, /* OMPI_OP_TYPE_DOUBLE */ \
  { NULL }, /* OMPI_OP_TYPE_REAL */ \
  { NULL }, /* OMPI_OP_TYPE_DOUBLE_PRECISION */ \
  { NULL }  /* OMPI_OP_TYPE_LONG_DOUBLE */

#define LOGICAL(name) \
  { ompi_mpi_op_##name##_fortran_logical }  /* OMPI_OP_TYPE_LOGICAL */
#define LOGICAL_NULL \
  { NULL }  /* OMPI_OP_TYPE_LOGICAL */

#define COMPLEX(name) \
  { ompi_mpi_op_##name##_fortran_complex }  /* OMPI_OP_TYPE_COMPLEX */
#define COMPLEX_NULL \
  { NULL }  /* OMPI_OP_TYPE_COMPLEX */

#define BYTE(name) \
  { ompi_mpi_op_##name##_byte }  /* OMPI_OP_TYPE_BYTE */
#define BYTE_NULL \
  { NULL }  /* OMPI_OP_TYPE_BYTE */

#define TWOLOC(name) \
  { ompi_mpi_op_##name##_2real },             /* OMPI_OP_TYPE_2REAL */ \
  { ompi_mpi_op_##name##_2double_precision }, /* OMPI_OP_TYPE_2DOUBLE_PRECISION */ \
  { ompi_mpi_op_##name##_2integer },          /* OMPI_OP_TYPE_2INTEGER */ \
  { ompi_mpi_op_##name##_float_int },         /* OMPI_OP_TYPE_FLOAT_INT */ \
  { ompi_mpi_op_##name##_double_int },        /* OMPI_OP_TYPE_DOUBLE_INT */ \
  { ompi_mpi_op_##name##_long_int },          /* OMPI_OP_TYPE_LONG_INT */ \
  { ompi_mpi_op_##name##_2int },              /* OMPI_OP_TYPE_2INT */ \
  { ompi_mpi_op_##name##_short_int },         /* OMPI_OP_TYPE_SHORT_INT */ \
  { ompi_mpi_op_##name##_long_double_int }    /* OMPI_OP_TYPE_LONG_DOUBLE_INT */
#define TWOLOC_NULL \
  { NULL }, /* OMPI_OP_TYPE_2REAL */\
  { NULL }, /* OMPI_OP_TYPE_2DOUBLE_PRECISION */ \
  { NULL }, /* OMPI_OP_TYPE_2INTEGER */ \
  { NULL }, /* OMPI_OP_TYPE_FLOAT_INT */ \
  { NULL }, /* OMPI_OP_TYPE_DOUBLE_INT */ \
  { NULL }, /* OMPI_OP_TYPE_LONG_INT */ \
  { NULL }, /* OMPI_OP_TYPE_2INT */ \
  { NULL }, /* OMPI_OP_TYPE_SHORT_INT */ \
  { NULL }  /* OMPI_OP_TYPE_LONG_DOUBLE_INT */


/*
 * MPI_OP_NULL
 * All types
 */
ompi_op_t ompi_mpi_op_null = {
    { NULL, 0 },

    "MPI_OP_NULL",
    (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | OMPI_OP_FLAGS_COMMUTE),
    { C_INTEGER_NULL,
      FORTRAN_INTEGER_NULL,
      FLOATING_POINT_NULL,
      LOGICAL_NULL,
      COMPLEX_NULL,
      BYTE_NULL,
      TWOLOC_NULL }
};


/*
 * MPI_OP_MAX
 * C integer, Fortran integer, Floating point
 */
ompi_op_t ompi_mpi_op_max = {
    { NULL, 0 },

    "MPI_OP_MAX",
    (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | OMPI_OP_FLAGS_COMMUTE),
    { C_INTEGER(max),
      FORTRAN_INTEGER(max),
      FLOATING_POINT(max),
      LOGICAL_NULL,
      COMPLEX_NULL,
      BYTE_NULL,
      TWOLOC_NULL }
};


/*
 * MPI_OP_MIN
 */
ompi_op_t ompi_mpi_op_min = {
    { NULL, 0 },

    "MPI_OP_MIN",
    (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | OMPI_OP_FLAGS_COMMUTE),
    { C_INTEGER(min),
      FORTRAN_INTEGER(min),
      FLOATING_POINT(min),
      LOGICAL_NULL,
      COMPLEX_NULL,
      BYTE_NULL,
      TWOLOC_NULL }
};


/*
 * MPI_OP_SUM
 */
ompi_op_t ompi_mpi_op_sum = {
    { NULL, 0 },

    "MPI_OP_SUM",
    (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | OMPI_OP_FLAGS_COMMUTE),
    { C_INTEGER(sum),
      FORTRAN_INTEGER(sum),
      FLOATING_POINT(sum),
      LOGICAL_NULL,
      COMPLEX(sum),
      BYTE_NULL,
      TWOLOC_NULL }
};


/*
 * MPI_OP_PROD
 */
ompi_op_t ompi_mpi_op_prod = {
    { NULL, 0 },

    "MPI_OP_PROD",
    (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | OMPI_OP_FLAGS_COMMUTE),
    { C_INTEGER(prod),
      FORTRAN_INTEGER(prod),
      FLOATING_POINT(prod),
      LOGICAL_NULL,
      COMPLEX(prod),
      BYTE_NULL,
      TWOLOC_NULL }
};


/*
 * MPI_OP_LAND
 */
ompi_op_t ompi_mpi_op_land = {
    { NULL, 0 },

    "MPI_OP_LAND",
    (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | OMPI_OP_FLAGS_COMMUTE),
    { C_INTEGER(land),
      FORTRAN_INTEGER_NULL,
      FLOATING_POINT_NULL,
      LOGICAL(land),
      COMPLEX_NULL,
      BYTE_NULL,
      TWOLOC_NULL }
};


/*
 * MPI_OP_BAND
 */
ompi_op_t ompi_mpi_op_band = {
    { NULL, 0 },

    "MPI_OP_BAND",
    (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | OMPI_OP_FLAGS_COMMUTE),
    { C_INTEGER(band),
      FORTRAN_INTEGER(band),
      FLOATING_POINT_NULL,
      LOGICAL_NULL,
      COMPLEX_NULL,
      BYTE(band),
      TWOLOC_NULL }
};


/*
 * MPI_OP_LOR
 */
ompi_op_t ompi_mpi_op_lor = {
    { NULL, 0 },

    "MPI_OP_LOR",
    (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | OMPI_OP_FLAGS_COMMUTE),
    { C_INTEGER(lor),
      FORTRAN_INTEGER_NULL,
      FLOATING_POINT_NULL,
      LOGICAL(lor),
      COMPLEX_NULL,
      BYTE_NULL,
      TWOLOC_NULL }
};


/*
 * MPI_OP_BOR
 */
ompi_op_t ompi_mpi_op_bor = {
    { NULL, 0 },

    "MPI_OP_BOR",
    (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | OMPI_OP_FLAGS_COMMUTE),
    { C_INTEGER(bor),
      FORTRAN_INTEGER(bor),
      FLOATING_POINT_NULL,
      LOGICAL_NULL,
      COMPLEX_NULL,
      BYTE(bor),
      TWOLOC_NULL }
};


/*
 * MPI_OP_LXOR
 */
ompi_op_t ompi_mpi_op_lxor = {
    { NULL, 0 },

    "MPI_OP_LXOR",
    (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | OMPI_OP_FLAGS_COMMUTE),
    { C_INTEGER(lxor),
      FORTRAN_INTEGER_NULL,
      FLOATING_POINT_NULL,
      LOGICAL(lxor),
      COMPLEX_NULL,
      BYTE_NULL,
      TWOLOC_NULL }
};


/*
 * MPI_OP_BXOR
 */
ompi_op_t ompi_mpi_op_bxor = {
    { NULL, 0 },

    "MPI_OP_BXOR",
    (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | OMPI_OP_FLAGS_COMMUTE),
    { C_INTEGER(bxor),
      FORTRAN_INTEGER(bxor),
      FLOATING_POINT_NULL,
      LOGICAL_NULL,
      COMPLEX_NULL,
      BYTE(bxor),
      TWOLOC_NULL }
};


/*
 * MPI_OP_MAXLOC
 */
ompi_op_t ompi_mpi_op_maxloc = {
    { NULL, 0 },

    "MPI_OP_MAXLOC",
    (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | OMPI_OP_FLAGS_COMMUTE),
    { C_INTEGER_NULL,
      FORTRAN_INTEGER_NULL,
      FLOATING_POINT_NULL,
      LOGICAL_NULL,
      COMPLEX_NULL,
      BYTE_NULL,
      TWOLOC(maxloc) }
};


/*
 * MPI_OP_MINLOC
 */
ompi_op_t ompi_mpi_op_minloc = {
    { NULL, 0 },

    "MPI_OP_MINLOC",
    (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | OMPI_OP_FLAGS_COMMUTE),
    { C_INTEGER_NULL,
      FORTRAN_INTEGER_NULL,
      FLOATING_POINT_NULL,
      LOGICAL_NULL,
      COMPLEX_NULL,
      BYTE_NULL,
      TWOLOC(minloc) }
};

/*
 * MPI_OP_REPLACE
 */
ompi_op_t ompi_mpi_op_replace = {
    { NULL, 0 },

    "MPI_OP_REPLACE",
    (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | OMPI_OP_FLAGS_COMMUTE),
    { C_INTEGER(replace),
      FORTRAN_INTEGER(replace),
      FLOATING_POINT(replace),
      LOGICAL(replace),
      COMPLEX(replace),
      BYTE(replace),
      TWOLOC_NULL }
};

/*
 * Map from ddt->id to position in op function pointer array
 */
int ompi_op_ddt_map[DT_MAX_PREDEFINED];


/*
 * Initialize OMPI op infrastructure
 */
int ompi_op_init(void)
{
  int i;

  /* initialize ompi_op_f_to_c_table */

  ompi_op_f_to_c_table = OBJ_NEW(ompi_pointer_array_t);
  if (NULL == ompi_op_f_to_c_table){
    return OMPI_ERROR;
  }

  /* Fill in the ddt.id->op_position map */

  for (i = 0; i < DT_MAX_PREDEFINED; ++i)
    ompi_op_ddt_map[i] = -1;

  ompi_op_ddt_map[DT_BYTE] = OMPI_OP_TYPE_BYTE;
  ompi_op_ddt_map[DT_SHORT] = OMPI_OP_TYPE_SHORT;
  ompi_op_ddt_map[DT_UNSIGNED_SHORT] = OMPI_OP_TYPE_UNSIGNED_SHORT;
  ompi_op_ddt_map[DT_INT] = OMPI_OP_TYPE_INT;
  ompi_op_ddt_map[DT_UNSIGNED_INT] = OMPI_OP_TYPE_UNSIGNED;
  ompi_op_ddt_map[DT_LONG] = OMPI_OP_TYPE_LONG;
  ompi_op_ddt_map[DT_UNSIGNED_LONG] = OMPI_OP_TYPE_UNSIGNED_LONG;
  ompi_op_ddt_map[DT_FLOAT] = OMPI_OP_TYPE_FLOAT;
  ompi_op_ddt_map[DT_DOUBLE] = OMPI_OP_TYPE_DOUBLE;
  ompi_op_ddt_map[DT_LONG_DOUBLE] = OMPI_OP_TYPE_LONG_DOUBLE;
  ompi_op_ddt_map[DT_COMPLEX_FLOAT] = OMPI_OP_TYPE_COMPLEX;
  ompi_op_ddt_map[DT_LOGIC] = OMPI_OP_TYPE_LOGICAL;
  ompi_op_ddt_map[DT_FLOAT_INT] = OMPI_OP_TYPE_FLOAT_INT;
  ompi_op_ddt_map[DT_DOUBLE_INT] = OMPI_OP_TYPE_DOUBLE_INT;
  ompi_op_ddt_map[DT_LONG_INT] = OMPI_OP_TYPE_LONG_INT;
  ompi_op_ddt_map[DT_2INT] = OMPI_OP_TYPE_2INT;
  ompi_op_ddt_map[DT_SHORT_INT] = OMPI_OP_TYPE_SHORT_INT;
  ompi_op_ddt_map[DT_INTEGER] = OMPI_OP_TYPE_INTEGER;
  ompi_op_ddt_map[DT_REAL] = OMPI_OP_TYPE_REAL;
  ompi_op_ddt_map[DT_DBLPREC] = OMPI_OP_TYPE_DOUBLE_PRECISION;
  ompi_op_ddt_map[DT_2REAL] = OMPI_OP_TYPE_2REAL;
  ompi_op_ddt_map[DT_2DBLPREC] = OMPI_OP_TYPE_2DOUBLE_PRECISION;
  ompi_op_ddt_map[DT_2INTEGER] = OMPI_OP_TYPE_2INTEGER;
  ompi_op_ddt_map[DT_LONG_DOUBLE_INT] = OMPI_OP_TYPE_LONG_DOUBLE_INT;
  ompi_op_ddt_map[DT_WCHAR] = OMPI_OP_TYPE_WCHAR;

  /* Create the intrinsic ops */

  if (add_intrinsic(&ompi_mpi_op_null, OMPI_OP_FORTRAN_NULL) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_max, OMPI_OP_FORTRAN_MAX) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_min, OMPI_OP_FORTRAN_MIN) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_sum, OMPI_OP_FORTRAN_SUM) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_prod, OMPI_OP_FORTRAN_PROD) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_land, OMPI_OP_FORTRAN_LAND) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_band, OMPI_OP_FORTRAN_BAND) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_lor, OMPI_OP_FORTRAN_LOR) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_bor, OMPI_OP_FORTRAN_BOR) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_lxor, OMPI_OP_FORTRAN_LXOR) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_bxor, OMPI_OP_FORTRAN_BXOR) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_maxloc, 
                    OMPI_OP_FORTRAN_MAXLOC) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_minloc, 
                    OMPI_OP_FORTRAN_MINLOC) != OMPI_SUCCESS) {
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


/*
 * Create a new MPI_Op
 */
ompi_op_t *ompi_op_create(bool commute,
                        ompi_op_fortran_handler_fn_t *func)
{
  int i;
  ompi_op_t *new_op;

  /* Create a new object and ensure that it's valid */

  new_op = OBJ_NEW(ompi_op_t);
  if (NULL != new_op) {
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

      new_op->o_flags = OMPI_OP_FLAGS_ASSOC;
      if (commute) {
        new_op->o_flags |= OMPI_OP_FLAGS_COMMUTE;
      }
      new_op->o_func[0].fort_fn = func;
      for (i = 1; i < OMPI_OP_TYPE_MAX; ++i) {
        new_op->o_func[i].fort_fn = NULL;
      }
    }
  }

  /* All done */

  return new_op;
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

  ret_val = ompi_pointer_array_add(ompi_op_f_to_c_table, new_op);
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
