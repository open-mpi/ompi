/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#include "ompi/constants.h"
#include "ompi/op/op.h"
#include "ompi/op/op_predefined.h"
#include "ompi/class/ompi_pointer_array.h"
#include "ompi/datatype/datatype_internal.h"


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
OBJ_CLASS_INSTANCE(ompi_op_t, opal_object_t, 
                   ompi_op_construct, ompi_op_destruct);


/*
 * Helpful defines, because there's soooo many names!
 *
 * **NOTE** These #define's are strictly ordered!  A series of macros
 * are built up to assemble a list of function names (or NULLs) that
 * are put into the intrinsict ompi_op_t's in the middle of this
 * file.  The order of these function names is critical, and must be
 * the same as the OMPI_OP_TYPE_* enums in op.h (i.e., the enum's
 * starting with OMPI_OP_TYPE_UNSIGNED_CHAR -- at the time of this writing,
 * this is op.h:78).
 */

/** C integer ***********************************************************/

#ifdef HAVE_LONG_LONG
#define C_INTEGER_LONG_LONG(name) \
  { ompi_mpi_op_##name##_long_long_int },  /* OMPI_OP_TYPE_LONG_LONG_INT */ \
  { ompi_mpi_op_##name##_unsigned_long_long } /* OMPI_OP_TYPE_UNSIGNED_LONG_LONG */
#else
#define C_INTEGER_LONG_LONG(name) \
  { NULL }, /* OMPI_OP_TYPE_LONG_LONG_INT */ \
  { NULL }  /* OMPI_OP_TYPE_UNSIGNED_LONG_LONG */
#endif

#define C_INTEGER(name) \
  { ompi_mpi_op_##name##_unsigned_char },  /* OMPI_OP_TYPE_UNSIGNED_CHAR */ \
  { ompi_mpi_op_##name##_signed_char },    /* OMPI_OP_TYPE_SIGNED_CHAR */ \
  { ompi_mpi_op_##name##_int },            /* OMPI_OP_TYPE_INT */ \
  { ompi_mpi_op_##name##_long },           /* OMPI_OP_TYPE_LONG */ \
  { ompi_mpi_op_##name##_short },          /* OMPI_OP_TYPE_SHORT */ \
  { ompi_mpi_op_##name##_unsigned_short }, /* OMPI_OP_TYPE_UNSIGNED_SHORT */ \
  { ompi_mpi_op_##name##_unsigned },       /* OMPI_OP_TYPE_UNSIGNED */ \
  { ompi_mpi_op_##name##_unsigned_long },  /* OMPI_OP_TYPE_UNSIGNED_LONG */ \
  C_INTEGER_LONG_LONG(name)
#define C_INTEGER_NULL \
  { NULL }, /* OMPI_OP_TYPE_UNSIGNED_CHAR */ \
  { NULL }, /* OMPI_OP_TYPE_SIGNED_CHAR */ \
  { NULL }, /* OMPI_OP_TYPE_INT */ \
  { NULL }, /* OMPI_OP_TYPE_LONG */ \
  { NULL }, /* OMPI_OP_TYPE_SHORT */ \
  { NULL }, /* OMPI_OP_TYPE_UNSIGNED_SHORT */ \
  { NULL }, /* OMPI_OP_TYPE_UNSIGNED */ \
  { NULL }, /* OMPI_OP_TYPE_UNSIGNED_LONG */ \
  { NULL }, /* OMPI_OP_TYPE_LONG_LONG_ING */ \
  { NULL }  /* OMPI_OP_TYPE_UNSIGNED_LONG_LONG */

/** All the Fortran integers ********************************************/

#if OMPI_HAVE_FORTRAN_INTEGER
#define FORTRAN_INTEGER_PLAIN(name) { ompi_mpi_op_##name##_fortran_integer }
#else
#define FORTRAN_INTEGER_PLAIN(name) { NULL }
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
#define FORTRAN_INTEGER1(name) { ompi_mpi_op_##name##_fortran_integer1 }
#else
#define FORTRAN_INTEGER1(name) { NULL }
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
#define FORTRAN_INTEGER2(name) { ompi_mpi_op_##name##_fortran_integer2 }
#else
#define FORTRAN_INTEGER2(name) { NULL }
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
#define FORTRAN_INTEGER4(name) { ompi_mpi_op_##name##_fortran_integer4 }
#else
#define FORTRAN_INTEGER4(name) { NULL }
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
#define FORTRAN_INTEGER8(name) { ompi_mpi_op_##name##_fortran_integer8 }
#else
#define FORTRAN_INTEGER8(name) { NULL }
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
#define FORTRAN_INTEGER16(name) { ompi_mpi_op_##name##_fortran_integer16 }
#else
#define FORTRAN_INTEGER16(name) { NULL }
#endif
#define FORTRAN_INTEGER(name) \
  FORTRAN_INTEGER_PLAIN(name),      /* OMPI_OP_TYPE_INTEGER */ \
  FORTRAN_INTEGER1(name),           /* OMPI_OP_TYPE_INTEGER1 */ \
  FORTRAN_INTEGER2(name),           /* OMPI_OP_TYPE_INTEGER2 */ \
  FORTRAN_INTEGER4(name),           /* OMPI_OP_TYPE_INTEGER4 */ \
  FORTRAN_INTEGER8(name),           /* OMPI_OP_TYPE_INTEGER8 */ \
  FORTRAN_INTEGER16(name)           /* OMPI_OP_TYPE_INTEGER16 */
#define FORTRAN_INTEGER_NULL \
  { NULL },  /* OMPI_OP_TYPE_INTEGER */ \
  { NULL },  /* OMPI_OP_TYPE_INTEGER1 */ \
  { NULL },  /* OMPI_OP_TYPE_INTEGER2 */ \
  { NULL },  /* OMPI_OP_TYPE_INTEGER4 */ \
  { NULL },  /* OMPI_OP_TYPE_INTEGER8 */ \
  { NULL }  /* OMPI_OP_TYPE_INTEGER16 */

/** All the Fortran reals ***********************************************/

#if OMPI_HAVE_FORTRAN_REAL
#define FLOATING_POINT_FORTRAN_REAL_PLAIN(name) { ompi_mpi_op_##name##_fortran_real }
#else
#define FLOATING_POINT_FORTRAN_REAL_PLAIN(name) { NULL }
#endif
#if OMPI_HAVE_FORTRAN_REAL4
#define FLOATING_POINT_FORTRAN_REAL4(name) { ompi_mpi_op_##name##_fortran_real4 }
#else
#define FLOATING_POINT_FORTRAN_REAL4(name) { NULL }
#endif
#if OMPI_HAVE_FORTRAN_REAL8
#define FLOATING_POINT_FORTRAN_REAL8(name) { ompi_mpi_op_##name##_fortran_real8 }
#else
#define FLOATING_POINT_FORTRAN_REAL8(name) { NULL }
#endif
#if OMPI_HAVE_FORTRAN_REAL16
#define FLOATING_POINT_FORTRAN_REAL16(name) { ompi_mpi_op_##name##_fortran_real16 }
#else
#define FLOATING_POINT_FORTRAN_REAL16(name) { NULL }
#endif

#define FLOATING_POINT_FORTRAN_REAL(name) \
  FLOATING_POINT_FORTRAN_REAL_PLAIN(name),      /* OMPI_OP_TYPE_REAL */ \
  FLOATING_POINT_FORTRAN_REAL4(name),           /* OMPI_OP_TYPE_REAL4 */ \
  FLOATING_POINT_FORTRAN_REAL8(name),           /* OMPI_OP_TYPE_REAL8 */ \
  FLOATING_POINT_FORTRAN_REAL16(name)           /* OMPI_OP_TYPE_REAL16 */

/** Fortran double precision ********************************************/

#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
#define FLOATING_POINT_FORTRAN_DOUBLE_PRECISION(name) \
    { ompi_mpi_op_##name##_fortran_double_precision }
#else
#define FLOATING_POINT_FORTRAN_DOUBLE_PRECISION(name) { NULL }
#endif

/** Floating point, including all the Fortran reals *********************/

#define FLOATING_POINT(name) \
  { ompi_mpi_op_##name##_float },                    /* OMPI_OP_TYPE_FLOAT */\
  { ompi_mpi_op_##name##_double },                   /* OMPI_OP_TYPE_DOUBLE */\
  FLOATING_POINT_FORTRAN_REAL(name),                 /* OMPI_OP_TYPE_REAL */ \
  FLOATING_POINT_FORTRAN_DOUBLE_PRECISION(name),     /* OMPI_OP_TYPE_DOUBLE_PRECISION */ \
  { ompi_mpi_op_##name##_long_double }               /* OMPI_OP_TYPE_LONG_DOUBLE */
#define FLOATING_POINT_NULL \
  { NULL }, /* OMPI_OP_TYPE_FLOAT */ \
  { NULL }, /* OMPI_OP_TYPE_DOUBLE */ \
  { NULL }, /* OMPI_OP_TYPE_REAL */ \
  { NULL }, /* OMPI_OP_TYPE_REAL4 */ \
  { NULL }, /* OMPI_OP_TYPE_REAL8 */ \
  { NULL }, /* OMPI_OP_TYPE_REAL16 */ \
  { NULL }, /* OMPI_OP_TYPE_DOUBLE_PRECISION */ \
  { NULL }  /* OMPI_OP_TYPE_LONG_DOUBLE */

/** Fortran logical *****************************************************/

#if OMPI_HAVE_FORTRAN_LOGICAL
#define FORTRAN_LOGICAL(name) \
  { ompi_mpi_op_##name##_fortran_logical }  /* OMPI_OP_TYPE_LOGICAL */
#else
#define FORTRAN_LOGICAL(name) { NULL }
#endif
#define LOGICAL(name) \
  FORTRAN_LOGICAL(name), \
  { ompi_mpi_op_##name##_bool }  /* OMPI_OP_TYPE_BOOL */

#define LOGICAL_NULL \
  { NULL },  /* OMPI_OP_TYPE_LOGICAL */ \
  { NULL }   /* OMPI_OP_TYPE_BOOL */

/** Fortran complex *****************************************************/

#if OMPI_HAVE_FORTRAN_REAL && OMPI_HAVE_FORTRAN_COMPLEX
#define COMPLEX_PLAIN(name) { ompi_mpi_op_##name##_fortran_complex }
#else
#define COMPLEX_PLAIN(name) { NULL }
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION && OMPI_HAVE_FORTRAN_COMPLEX
#define COMPLEX_DOUBLE(name) { ompi_mpi_op_##name##_fortran_double_complex }
#else
#define COMPLEX_DOUBLE(name) { NULL }
#endif
#if OMPI_HAVE_FORTRAN_REAL4 && OMPI_HAVE_FORTRAN_COMPLEX8
#define COMPLEX8(name) { ompi_mpi_op_##name##_fortran_complex8 }
#else
#define COMPLEX8(name) { NULL }
#endif
#if OMPI_HAVE_FORTRAN_REAL8 && OMPI_HAVE_FORTRAN_COMPLEX16
#define COMPLEX16(name) { ompi_mpi_op_##name##_fortran_complex16 }
#else
#define COMPLEX16(name) { NULL }
#endif
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_HAVE_FORTRAN_COMPLEX32
#define COMPLEX32(name) { ompi_mpi_op_##name##_fortran_complex32 }
#else
#define COMPLEX32(name) { NULL }
#endif

#define COMPLEX(name) \
  COMPLEX_PLAIN(name),  /* OMPI_OP_TYPE_COMPLEX */ \
  COMPLEX_DOUBLE(name), /* OMPI_OP_TYPE_DOUBLE_COMPLEX */ \
  COMPLEX8(name),       /* OMPI_OP_TYPE_COMPLEX8 */ \
  COMPLEX16(name),      /* OMPI_OP_TYPE_COMPLEX16 */ \
  COMPLEX32(name)       /* OMPI_OP_TYPE_COMPLEX32 */
#define COMPLEX_NULL \
  { NULL },  /* OMPI_OP_TYPE_COMPLEX */ \
  { NULL },  /* OMPI_OP_TYPE_DOUBLE_COMPLEX */ \
  { NULL },  /* OMPI_OP_TYPE_COMPLEX8 */ \
  { NULL },  /* OMPI_OP_TYPE_COMPLEX16 */ \
  { NULL }   /* OMPI_OP_TYPE_COMPLEX32 */

/** Byte ****************************************************************/

#define BYTE(name) \
  { ompi_mpi_op_##name##_byte }  /* OMPI_OP_TYPE_BYTE */
#define BYTE_NULL \
  { NULL }  /* OMPI_OP_TYPE_BYTE */

/** Fortran complex *****************************************************/
/** Fortran "2" types ***************************************************/

#if OMPI_HAVE_FORTRAN_REAL
#define TWOLOC_FORTRAN_2REAL(name) { ompi_mpi_op_##name##_2real }
#else
#define TWOLOC_FORTRAN_2REAL(name) { NULL }
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
#define TWOLOC_FORTRAN_2DOUBLE_PRECISION(name) { ompi_mpi_op_##name##_2double_precision }
#else
#define TWOLOC_FORTRAN_2DOUBLE_PRECISION(name) { NULL }
#endif
#if OMPI_HAVE_FORTRAN_INTEGER
#define TWOLOC_FORTRAN_2INTEGER(name) { ompi_mpi_op_##name##_2integer }
#else
#define TWOLOC_FORTRAN_2INTEGER(name) { NULL }
#endif

/** All "2" types *******************************************************/

#define TWOLOC(name) \
  TWOLOC_FORTRAN_2REAL(name),                 /* OMPI_OP_TYPE_2REAL */ \
  TWOLOC_FORTRAN_2DOUBLE_PRECISION(name),     /* OMPI_OP_TYPE_2DOUBLE_PRECISION */ \
  TWOLOC_FORTRAN_2INTEGER(name),              /* OMPI_OP_TYPE_2INTEGER */ \
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
#define FLAGS_NO_FLOAT \
    (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | OMPI_OP_FLAGS_COMMUTE)
#define FLAGS \
    (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | \
     OMPI_OP_FLAGS_FLOAT_ASSOC | OMPI_OP_FLAGS_COMMUTE)

ompi_op_t ompi_mpi_op_null = {
    { NULL, 0 },

    "MPI_OP_NULL",
    FLAGS,
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

    "MPI_MAX",
    FLAGS,
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

    "MPI_MIN",
    FLAGS,
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

    "MPI_SUM",
    FLAGS_NO_FLOAT,
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

    "MPI_PROD",
    FLAGS_NO_FLOAT,
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

    "MPI_LAND",
    FLAGS,
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

    "MPI_BAND",
    FLAGS,
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

    "MPI_LOR",
    FLAGS,
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

    "MPI_BOR",
    FLAGS,
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

    "MPI_LXOR",
    FLAGS,
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

    "MPI_BXOR",
    FLAGS,
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

    "MPI_MAXLOC",
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

    "MPI_MINLOC",
    FLAGS,
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
 * (MPI_ACCUMULATE is handled differently than the other reductions,
 * so just zero out its function impementations here to ensure that
 * users don't invoke MPI_REPLACE with any reduction operations other
 * than ACCUMULATE)
 */
ompi_op_t ompi_mpi_op_replace = {
    { NULL, 0 },

    "MPI_REPLACE",
    FLAGS,
    { C_INTEGER_NULL,
      FORTRAN_INTEGER_NULL,
      FLOATING_POINT_NULL,
      LOGICAL_NULL,
      COMPLEX_NULL,
      BYTE_NULL,
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

  for (i = 0; i < DT_MAX_PREDEFINED; ++i) {
    ompi_op_ddt_map[i] = -1;
  }

  ompi_op_ddt_map[DT_UNSIGNED_CHAR] = OMPI_OP_TYPE_UNSIGNED_CHAR;
  ompi_op_ddt_map[DT_SIGNED_CHAR] = OMPI_OP_TYPE_SIGNED_CHAR;
  ompi_op_ddt_map[DT_BYTE] = OMPI_OP_TYPE_BYTE;
  ompi_op_ddt_map[DT_SHORT] = OMPI_OP_TYPE_SHORT;
  ompi_op_ddt_map[DT_UNSIGNED_SHORT] = OMPI_OP_TYPE_UNSIGNED_SHORT;
  ompi_op_ddt_map[DT_INT] = OMPI_OP_TYPE_INT;
  ompi_op_ddt_map[DT_UNSIGNED_INT] = OMPI_OP_TYPE_UNSIGNED;
  ompi_op_ddt_map[DT_LONG] = OMPI_OP_TYPE_LONG;
  ompi_op_ddt_map[DT_UNSIGNED_LONG] = OMPI_OP_TYPE_UNSIGNED_LONG;
  ompi_op_ddt_map[DT_LONG_LONG_INT] = OMPI_OP_TYPE_LONG_LONG_INT;
  ompi_op_ddt_map[DT_UNSIGNED_LONG_LONG] = OMPI_OP_TYPE_UNSIGNED_LONG_LONG;
  ompi_op_ddt_map[DT_FLOAT] = OMPI_OP_TYPE_FLOAT;
  ompi_op_ddt_map[DT_DOUBLE] = OMPI_OP_TYPE_DOUBLE;
  ompi_op_ddt_map[DT_LONG_DOUBLE] = OMPI_OP_TYPE_LONG_DOUBLE;
  ompi_op_ddt_map[DT_COMPLEX_FLOAT] = OMPI_OP_TYPE_COMPLEX;
  ompi_op_ddt_map[DT_COMPLEX_DOUBLE] = OMPI_OP_TYPE_DOUBLE_COMPLEX;
  ompi_op_ddt_map[DT_LOGIC] = OMPI_OP_TYPE_LOGICAL;
  ompi_op_ddt_map[DT_CXX_BOOL] = OMPI_OP_TYPE_BOOL;
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
                    OMPI_OP_FORTRAN_MINLOC) != OMPI_SUCCESS ||
      add_intrinsic(&ompi_mpi_op_replace, 
                    OMPI_OP_FORTRAN_REPLACE) != OMPI_SUCCESS) {
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
  /* clean up the intrinsic ops */

  OBJ_DESTRUCT(&ompi_mpi_op_minloc);
  OBJ_DESTRUCT(&ompi_mpi_op_maxloc);
  OBJ_DESTRUCT(&ompi_mpi_op_bxor);
  OBJ_DESTRUCT(&ompi_mpi_op_lxor);
  OBJ_DESTRUCT(&ompi_mpi_op_bor);
  OBJ_DESTRUCT(&ompi_mpi_op_lor);
  OBJ_DESTRUCT(&ompi_mpi_op_band);
  OBJ_DESTRUCT(&ompi_mpi_op_land);
  OBJ_DESTRUCT(&ompi_mpi_op_prod);
  OBJ_DESTRUCT(&ompi_mpi_op_sum);
  OBJ_DESTRUCT(&ompi_mpi_op_min);
  OBJ_DESTRUCT(&ompi_mpi_op_max);
  OBJ_DESTRUCT(&ompi_mpi_op_null);
        
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
         will override the o_flags field directly.  We cast the
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


void ompi_op_set_cxx_callback(ompi_op_t *op, MPI_User_function *fn)
{
    op->o_flags |= OMPI_OP_FLAGS_CXX_FUNC;
    op->o_func[1].c_fn = fn;
}


/**************************************************************************
 *
 * Static functions
 *
 **************************************************************************/

static int add_intrinsic(ompi_op_t *op, int fort_handle)
{
  /* Add the op to the table */

  OBJ_CONSTRUCT(op, ompi_op_t);
  if (op->o_f_to_c_index != fort_handle) {
      return OMPI_ERROR;
  }

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
