/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"
#include <complex.h>

#include "orte/runtime/orte_globals.h"

#include "opal/datatype/opal_datatype_internal.h"
#include "opal/class/opal_pointer_array.h"

#include "oshmem/constants.h"
#include "oshmem/op/op.h"

/*
 * Table for op handle conversion
 */
opal_pointer_array_t oshmem_op_array;

/*
 * Class information
 */
static void oshmem_op_construct(oshmem_op_t *object);
static void oshmem_op_destruct(oshmem_op_t *object);

/*
 * Class instance
 */
OBJ_CLASS_INSTANCE(oshmem_op_t,
                   opal_object_t,
                   oshmem_op_construct,
                   oshmem_op_destruct);

/*
 * Intrinsic Operation objects
 */
/* Bitwise AND */
oshmem_op_t* oshmem_op_and_short = NULL;
oshmem_op_t* oshmem_op_and_int = NULL;
oshmem_op_t* oshmem_op_and_long = NULL;
oshmem_op_t* oshmem_op_and_longlong = NULL;
oshmem_op_t* oshmem_op_and_fint2 = NULL;
oshmem_op_t* oshmem_op_and_fint4 = NULL;
oshmem_op_t* oshmem_op_and_fint8 = NULL;
oshmem_op_t* oshmem_op_and_int16 = NULL;
oshmem_op_t* oshmem_op_and_int32 = NULL;
oshmem_op_t* oshmem_op_and_int64 = NULL;

/* Bitwise OR */
oshmem_op_t* oshmem_op_or_short = NULL;
oshmem_op_t* oshmem_op_or_int = NULL;
oshmem_op_t* oshmem_op_or_long = NULL;
oshmem_op_t* oshmem_op_or_longlong = NULL;
oshmem_op_t* oshmem_op_or_fint2 = NULL;
oshmem_op_t* oshmem_op_or_fint4 = NULL;
oshmem_op_t* oshmem_op_or_fint8 = NULL;
oshmem_op_t* oshmem_op_or_int16 = NULL;
oshmem_op_t* oshmem_op_or_int32 = NULL;
oshmem_op_t* oshmem_op_or_int64 = NULL;

/* Bitwise XOR */
oshmem_op_t* oshmem_op_xor_short = NULL;
oshmem_op_t* oshmem_op_xor_int = NULL;
oshmem_op_t* oshmem_op_xor_long = NULL;
oshmem_op_t* oshmem_op_xor_longlong = NULL;
oshmem_op_t* oshmem_op_xor_fint2 = NULL;
oshmem_op_t* oshmem_op_xor_fint4 = NULL;
oshmem_op_t* oshmem_op_xor_fint8 = NULL;
oshmem_op_t* oshmem_op_xor_int16 = NULL;
oshmem_op_t* oshmem_op_xor_int32 = NULL;
oshmem_op_t* oshmem_op_xor_int64 = NULL;

/* MAX */
oshmem_op_t* oshmem_op_max_short = NULL;
oshmem_op_t* oshmem_op_max_int = NULL;
oshmem_op_t* oshmem_op_max_long = NULL;
oshmem_op_t* oshmem_op_max_longlong = NULL;
oshmem_op_t* oshmem_op_max_float = NULL;
oshmem_op_t* oshmem_op_max_double = NULL;
oshmem_op_t* oshmem_op_max_longdouble = NULL;
oshmem_op_t* oshmem_op_max_fint2 = NULL;
oshmem_op_t* oshmem_op_max_fint4 = NULL;
oshmem_op_t* oshmem_op_max_fint8 = NULL;
oshmem_op_t* oshmem_op_max_freal4 = NULL;
oshmem_op_t* oshmem_op_max_freal8 = NULL;
oshmem_op_t* oshmem_op_max_freal16 = NULL;
oshmem_op_t* oshmem_op_max_int16 = NULL;
oshmem_op_t* oshmem_op_max_int32 = NULL;
oshmem_op_t* oshmem_op_max_int64 = NULL;

/* MIN */
oshmem_op_t* oshmem_op_min_short = NULL;
oshmem_op_t* oshmem_op_min_int = NULL;
oshmem_op_t* oshmem_op_min_long = NULL;
oshmem_op_t* oshmem_op_min_longlong = NULL;
oshmem_op_t* oshmem_op_min_float = NULL;
oshmem_op_t* oshmem_op_min_double = NULL;
oshmem_op_t* oshmem_op_min_longdouble = NULL;
oshmem_op_t* oshmem_op_min_fint2 = NULL;
oshmem_op_t* oshmem_op_min_fint4 = NULL;
oshmem_op_t* oshmem_op_min_fint8 = NULL;
oshmem_op_t* oshmem_op_min_freal4 = NULL;
oshmem_op_t* oshmem_op_min_freal8 = NULL;
oshmem_op_t* oshmem_op_min_freal16 = NULL;
oshmem_op_t* oshmem_op_min_int16 = NULL;
oshmem_op_t* oshmem_op_min_int32 = NULL;
oshmem_op_t* oshmem_op_min_int64 = NULL;

/* SUM */
oshmem_op_t* oshmem_op_sum_short = NULL;
oshmem_op_t* oshmem_op_sum_int = NULL;
oshmem_op_t* oshmem_op_sum_long = NULL;
oshmem_op_t* oshmem_op_sum_longlong = NULL;
oshmem_op_t* oshmem_op_sum_float = NULL;
oshmem_op_t* oshmem_op_sum_double = NULL;
oshmem_op_t* oshmem_op_sum_longdouble = NULL;
oshmem_op_t* oshmem_op_sum_complexf = NULL;
oshmem_op_t* oshmem_op_sum_complexd = NULL;
oshmem_op_t* oshmem_op_sum_fint2 = NULL;
oshmem_op_t* oshmem_op_sum_fint4 = NULL;
oshmem_op_t* oshmem_op_sum_fint8 = NULL;
oshmem_op_t* oshmem_op_sum_freal4 = NULL;
oshmem_op_t* oshmem_op_sum_freal8 = NULL;
oshmem_op_t* oshmem_op_sum_freal16 = NULL;
oshmem_op_t* oshmem_op_sum_int16 = NULL;
oshmem_op_t* oshmem_op_sum_int32 = NULL;
oshmem_op_t* oshmem_op_sum_int64 = NULL;

/* PROD */
oshmem_op_t* oshmem_op_prod_short = NULL;
oshmem_op_t* oshmem_op_prod_int = NULL;
oshmem_op_t* oshmem_op_prod_long = NULL;
oshmem_op_t* oshmem_op_prod_longlong = NULL;
oshmem_op_t* oshmem_op_prod_float = NULL;
oshmem_op_t* oshmem_op_prod_double = NULL;
oshmem_op_t* oshmem_op_prod_longdouble = NULL;
oshmem_op_t* oshmem_op_prod_complexf = NULL;
oshmem_op_t* oshmem_op_prod_complexd = NULL;
oshmem_op_t* oshmem_op_prod_fint2 = NULL;
oshmem_op_t* oshmem_op_prod_fint4 = NULL;
oshmem_op_t* oshmem_op_prod_fint8 = NULL;
oshmem_op_t* oshmem_op_prod_freal4 = NULL;
oshmem_op_t* oshmem_op_prod_freal8 = NULL;
oshmem_op_t* oshmem_op_prod_freal16 = NULL;
oshmem_op_t* oshmem_op_prod_int16 = NULL;
oshmem_op_t* oshmem_op_prod_int32 = NULL;
oshmem_op_t* oshmem_op_prod_int64 = NULL;

#define FUNC_OP_CREATE(name, type_name, type, calc)  \
    void oshmem_op_##name##_##type_name##_func(void *in, void *out, int count); \
    void oshmem_op_##name##_##type_name##_func(void *in, void *out, int count) \
    {                                                                       \
        int i;                                                              \
        type *a = (type *) in;                                              \
        type *b = (type *) out;                                             \
        for (i = 0; i < count; ++i) {                                       \
            *(b) = calc(*(b), *(a));                                        \
            ++b;                                                            \
            ++a;                                                            \
        }                                                                   \
    }

#define OBJ_OP_CREATE(name, type_name, type, op_id, dt_id)  \
    oshmem_op_##name##_##type_name = OBJ_NEW(oshmem_op_t);                                      \
    if (oshmem_op_##name##_##type_name)                                                         \
    {                                                                                           \
        oshmem_op_##name##_##type_name->op = op_id;                                             \
        oshmem_op_##name##_##type_name->dt = dt_id;                                             \
        oshmem_op_##name##_##type_name->dt_size = sizeof(type);                                 \
        oshmem_op_##name##_##type_name->o_func.c_fn = oshmem_op_##name##_##type_name##_func;    \
    }                                                                                           \

/* Bitwise AND */
#define __and_op(a, b) ((a) & (b))
FUNC_OP_CREATE(and, short, short, __and_op)
FUNC_OP_CREATE(and, int, int, __and_op)
FUNC_OP_CREATE(and, long, long, __and_op)
FUNC_OP_CREATE(and, longlong, long long, __and_op)
FUNC_OP_CREATE(and, fint2, ompi_fortran_integer4_t, __and_op)
FUNC_OP_CREATE(and, fint4, ompi_fortran_integer4_t, __and_op)
FUNC_OP_CREATE(and, fint8, ompi_fortran_integer8_t, __and_op)
FUNC_OP_CREATE(and, int16, int16_t, __and_op)
FUNC_OP_CREATE(and, int32, int32_t, __and_op)
FUNC_OP_CREATE(and, int64, int64_t, __and_op)

/* Bitwise OR */
#define __or_op(a, b) ((a) | (b))
FUNC_OP_CREATE(or, short, short, __or_op)
FUNC_OP_CREATE(or, int, int, __or_op)
FUNC_OP_CREATE(or, long, long, __or_op)
FUNC_OP_CREATE(or, longlong, long long, __or_op)
FUNC_OP_CREATE(or, fint2, ompi_fortran_integer2_t, __or_op)
FUNC_OP_CREATE(or, fint4, ompi_fortran_integer4_t, __or_op)
FUNC_OP_CREATE(or, fint8, ompi_fortran_integer8_t, __or_op)
FUNC_OP_CREATE(or, int16, int16_t, __or_op)
FUNC_OP_CREATE(or, int32, int32_t, __or_op)
FUNC_OP_CREATE(or, int64, int64_t, __or_op)

/* Bitwise XOR */
#define __xor_op(a, b) ((a) ^ (b))
FUNC_OP_CREATE(xor, short, short, __xor_op)
FUNC_OP_CREATE(xor, int, int, __xor_op)
FUNC_OP_CREATE(xor, long, long, __xor_op)
FUNC_OP_CREATE(xor, longlong, long long, __xor_op)
FUNC_OP_CREATE(xor, fint2, ompi_fortran_integer4_t, __xor_op)
FUNC_OP_CREATE(xor, fint4, ompi_fortran_integer4_t, __xor_op)
FUNC_OP_CREATE(xor, fint8, ompi_fortran_integer8_t, __xor_op)
FUNC_OP_CREATE(xor, int16, int16_t, __xor_op)
FUNC_OP_CREATE(xor, int32, int32_t, __xor_op)
FUNC_OP_CREATE(xor, int64, int64_t, __xor_op)

/* MAX */
#define __max_op(a, b) ((a) > (b) ? (a) : (b))
FUNC_OP_CREATE(max, short, short, __max_op)
FUNC_OP_CREATE(max, int, int, __max_op)
FUNC_OP_CREATE(max, long, long, __max_op)
FUNC_OP_CREATE(max, longlong, long long, __max_op)
FUNC_OP_CREATE(max, float, float, __max_op)
FUNC_OP_CREATE(max, double, double, __max_op)
FUNC_OP_CREATE(max, longdouble, long double, __max_op)
FUNC_OP_CREATE(max, fint2, ompi_fortran_integer4_t, __max_op)
FUNC_OP_CREATE(max, fint4, ompi_fortran_integer4_t, __max_op)
FUNC_OP_CREATE(max, fint8, ompi_fortran_integer8_t, __max_op)
FUNC_OP_CREATE(max, freal4, ompi_fortran_real4_t, __max_op)
FUNC_OP_CREATE(max, freal8, ompi_fortran_real8_t, __max_op)
#if OMPI_HAVE_FORTRAN_REAL16
FUNC_OP_CREATE(max, freal16, ompi_fortran_real16_t, __max_op)
#endif
FUNC_OP_CREATE(max, int16, int16_t, __max_op)
FUNC_OP_CREATE(max, int32, int32_t, __max_op)
FUNC_OP_CREATE(max, int64, int64_t, __max_op)

/* MIN */
#define __min_op(a, b) ((a) < (b) ? (a) : (b))
FUNC_OP_CREATE(min, short, short, __min_op)
FUNC_OP_CREATE(min, int, int, __min_op)
FUNC_OP_CREATE(min, long, long, __min_op)
FUNC_OP_CREATE(min, longlong, long long, __min_op)
FUNC_OP_CREATE(min, float, float, __min_op)
FUNC_OP_CREATE(min, double, double, __min_op)
FUNC_OP_CREATE(min, longdouble, long double, __min_op)
FUNC_OP_CREATE(min, fint2, ompi_fortran_integer4_t, __min_op)
FUNC_OP_CREATE(min, fint4, ompi_fortran_integer4_t, __min_op)
FUNC_OP_CREATE(min, fint8, ompi_fortran_integer8_t, __min_op)
FUNC_OP_CREATE(min, freal4, ompi_fortran_real4_t, __min_op)
FUNC_OP_CREATE(min, freal8, ompi_fortran_real8_t, __min_op)
#if OMPI_HAVE_FORTRAN_REAL16
FUNC_OP_CREATE(min, freal16, ompi_fortran_real16_t, __min_op)
#endif
FUNC_OP_CREATE(min, int16, int16_t, __min_op)
FUNC_OP_CREATE(min, int32, int32_t, __min_op)
FUNC_OP_CREATE(min, int64, int64_t, __min_op)

/* SUM */
#define __sum_op(a, b) ((a) + (b))
FUNC_OP_CREATE(sum, short, short, __sum_op)
FUNC_OP_CREATE(sum, int, int, __sum_op)
FUNC_OP_CREATE(sum, long, long, __sum_op)
FUNC_OP_CREATE(sum, longlong, long long, __sum_op)
FUNC_OP_CREATE(sum, float, float, __sum_op)
FUNC_OP_CREATE(sum, double, double, __sum_op)
FUNC_OP_CREATE(sum, longdouble, long double, __sum_op)
FUNC_OP_CREATE(sum, complexf, float complex, __sum_op)
FUNC_OP_CREATE(sum, complexd, double complex, __sum_op)
FUNC_OP_CREATE(sum, fint2, ompi_fortran_integer4_t, __sum_op)
FUNC_OP_CREATE(sum, fint4, ompi_fortran_integer4_t, __sum_op)
FUNC_OP_CREATE(sum, fint8, ompi_fortran_integer8_t, __sum_op)
FUNC_OP_CREATE(sum, freal4, ompi_fortran_real4_t, __sum_op)
FUNC_OP_CREATE(sum, freal8, ompi_fortran_real8_t, __sum_op)
#if OMPI_HAVE_FORTRAN_REAL16
FUNC_OP_CREATE(sum, freal16, ompi_fortran_real16_t, __sum_op)
#endif
FUNC_OP_CREATE(sum, int16, int16_t, __sum_op)
FUNC_OP_CREATE(sum, int32, int32_t, __sum_op)
FUNC_OP_CREATE(sum, int64, int64_t, __sum_op)

/* PROD */
#define __prod_op(a, b) ((a) * (b))
FUNC_OP_CREATE(prod, short, short, __prod_op)
FUNC_OP_CREATE(prod, int, int, __prod_op)
FUNC_OP_CREATE(prod, long, long, __prod_op)
FUNC_OP_CREATE(prod, longlong, long long, __prod_op)
FUNC_OP_CREATE(prod, float, float, __prod_op)
FUNC_OP_CREATE(prod, double, double, __prod_op)
FUNC_OP_CREATE(prod, longdouble, long double, __prod_op)
FUNC_OP_CREATE(prod, complexf, float complex, __prod_op)
FUNC_OP_CREATE(prod, complexd, double complex, __prod_op)
FUNC_OP_CREATE(prod, fint2, ompi_fortran_integer2_t, __prod_op)
FUNC_OP_CREATE(prod, fint4, ompi_fortran_integer4_t, __prod_op)
FUNC_OP_CREATE(prod, fint8, ompi_fortran_integer8_t, __prod_op)
FUNC_OP_CREATE(prod, freal4, ompi_fortran_real4_t, __prod_op)
FUNC_OP_CREATE(prod, freal8, ompi_fortran_real8_t, __prod_op)
#if OMPI_HAVE_FORTRAN_REAL16
FUNC_OP_CREATE(prod, freal16, ompi_fortran_real16_t, __prod_op)
#endif
FUNC_OP_CREATE(prod, int16, int16_t, __prod_op)
FUNC_OP_CREATE(prod, int32, int32_t, __prod_op)
FUNC_OP_CREATE(prod, int64, int64_t, __prod_op)

int oshmem_op_init(void)
{

    /* Setup operation array */
    OBJ_CONSTRUCT(&oshmem_op_array, opal_pointer_array_t);
    if (OPAL_SUCCESS
            != opal_pointer_array_init(&oshmem_op_array,
                                       0,
                                       ORTE_GLOBAL_ARRAY_MAX_SIZE,
                                       1)) {
        return OSHMEM_ERROR;
    }

    /* Bitwise AND */
    OBJ_OP_CREATE(and, short, short, OSHMEM_OP_AND, OSHMEM_OP_TYPE_SHORT);
    OBJ_OP_CREATE(and, int, int, OSHMEM_OP_AND, OSHMEM_OP_TYPE_INT);
    OBJ_OP_CREATE(and, long, long, OSHMEM_OP_AND, OSHMEM_OP_TYPE_LONG);
    OBJ_OP_CREATE(and, longlong, long long, OSHMEM_OP_AND, OSHMEM_OP_TYPE_LLONG);
    OBJ_OP_CREATE(and, fint2, ompi_fortran_integer2_t, OSHMEM_OP_AND, OSHMEM_OP_TYPE_FINT2);
    OBJ_OP_CREATE(and, fint4, ompi_fortran_integer4_t, OSHMEM_OP_AND, OSHMEM_OP_TYPE_FINT4);
    OBJ_OP_CREATE(and, fint8, ompi_fortran_integer8_t, OSHMEM_OP_AND, OSHMEM_OP_TYPE_FINT8);
    OBJ_OP_CREATE(and, int16, int16_t, OSHMEM_OP_AND, OSHMEM_OP_TYPE_INT16_T);
    OBJ_OP_CREATE(and, int32, int32_t, OSHMEM_OP_AND, OSHMEM_OP_TYPE_INT32_T);
    OBJ_OP_CREATE(and, int64, int64_t, OSHMEM_OP_AND, OSHMEM_OP_TYPE_INT64_T);

    /* Bitwise OR */
    OBJ_OP_CREATE(or, short, short, OSHMEM_OP_OR, OSHMEM_OP_TYPE_SHORT);
    OBJ_OP_CREATE(or, int, int, OSHMEM_OP_OR, OSHMEM_OP_TYPE_INT);
    OBJ_OP_CREATE(or, long, long, OSHMEM_OP_OR, OSHMEM_OP_TYPE_LONG);
    OBJ_OP_CREATE(or, longlong, long long, OSHMEM_OP_OR, OSHMEM_OP_TYPE_LLONG);
    OBJ_OP_CREATE(or, fint2, ompi_fortran_integer2_t, OSHMEM_OP_OR, OSHMEM_OP_TYPE_FINT2);
    OBJ_OP_CREATE(or, fint4, ompi_fortran_integer4_t, OSHMEM_OP_OR, OSHMEM_OP_TYPE_FINT4);
    OBJ_OP_CREATE(or, fint8, ompi_fortran_integer8_t, OSHMEM_OP_OR, OSHMEM_OP_TYPE_FINT8);
    OBJ_OP_CREATE(or, int16, int16_t, OSHMEM_OP_OR, OSHMEM_OP_TYPE_INT16_T);
    OBJ_OP_CREATE(or, int32, int32_t, OSHMEM_OP_OR, OSHMEM_OP_TYPE_INT32_T);
    OBJ_OP_CREATE(or, int64, int64_t, OSHMEM_OP_OR, OSHMEM_OP_TYPE_INT64_T);

    /* Bitwise XOR */
    OBJ_OP_CREATE(xor, short, short, OSHMEM_OP_XOR, OSHMEM_OP_TYPE_SHORT);
    OBJ_OP_CREATE(xor, int, int, OSHMEM_OP_XOR, OSHMEM_OP_TYPE_INT);
    OBJ_OP_CREATE(xor, long, long, OSHMEM_OP_XOR, OSHMEM_OP_TYPE_LONG);
    OBJ_OP_CREATE(xor, longlong, long long, OSHMEM_OP_XOR, OSHMEM_OP_TYPE_LLONG);
    OBJ_OP_CREATE(xor, fint2, ompi_fortran_integer2_t, OSHMEM_OP_XOR, OSHMEM_OP_TYPE_FINT2);
    OBJ_OP_CREATE(xor, fint4, ompi_fortran_integer4_t, OSHMEM_OP_XOR, OSHMEM_OP_TYPE_FINT4);
    OBJ_OP_CREATE(xor, fint8, ompi_fortran_integer8_t, OSHMEM_OP_XOR, OSHMEM_OP_TYPE_FINT8);
    OBJ_OP_CREATE(xor, int16, int16_t, OSHMEM_OP_XOR, OSHMEM_OP_TYPE_INT16_T);
    OBJ_OP_CREATE(xor, int32, int32_t, OSHMEM_OP_XOR, OSHMEM_OP_TYPE_INT32_T);
    OBJ_OP_CREATE(xor, int64, int64_t, OSHMEM_OP_XOR, OSHMEM_OP_TYPE_INT64_T);

    /* MAX */
    OBJ_OP_CREATE(max, short, short, OSHMEM_OP_MAX, OSHMEM_OP_TYPE_SHORT);
    OBJ_OP_CREATE(max, int, int, OSHMEM_OP_MAX, OSHMEM_OP_TYPE_INT);
    OBJ_OP_CREATE(max, long, long, OSHMEM_OP_MAX, OSHMEM_OP_TYPE_LONG);
    OBJ_OP_CREATE(max, longlong, long long, OSHMEM_OP_MAX, OSHMEM_OP_TYPE_LLONG);
    OBJ_OP_CREATE(max, float, float, OSHMEM_OP_MAX, OSHMEM_OP_TYPE_FLOAT);
    OBJ_OP_CREATE(max, double, double, OSHMEM_OP_MAX, OSHMEM_OP_TYPE_DOUBLE);
    OBJ_OP_CREATE(max, longdouble, long double, OSHMEM_OP_MAX, OSHMEM_OP_TYPE_LDOUBLE);
    OBJ_OP_CREATE(max, fint2, ompi_fortran_integer2_t, OSHMEM_OP_MAX, OSHMEM_OP_TYPE_FINT2);
    OBJ_OP_CREATE(max, fint4, ompi_fortran_integer4_t, OSHMEM_OP_MAX, OSHMEM_OP_TYPE_FINT4);
    OBJ_OP_CREATE(max, fint8, ompi_fortran_integer8_t, OSHMEM_OP_MAX, OSHMEM_OP_TYPE_FINT8);
    OBJ_OP_CREATE(max, freal4, ompi_fortran_real4_t, OSHMEM_OP_MAX, OSHMEM_OP_TYPE_FREAL4);
    OBJ_OP_CREATE(max, freal8, ompi_fortran_real8_t, OSHMEM_OP_MAX, OSHMEM_OP_TYPE_FREAL8);
#if OMPI_HAVE_FORTRAN_REAL16
    OBJ_OP_CREATE(max, freal16, ompi_fortran_real16_t, OSHMEM_OP_MAX, OSHMEM_OP_TYPE_FREAL16);
#endif
    OBJ_OP_CREATE(max, int16, int16_t, OSHMEM_OP_MAX, OSHMEM_OP_TYPE_INT16_T);
    OBJ_OP_CREATE(max, int32, int32_t, OSHMEM_OP_MAX, OSHMEM_OP_TYPE_INT32_T);
    OBJ_OP_CREATE(max, int64, int64_t, OSHMEM_OP_MAX, OSHMEM_OP_TYPE_INT64_T);

    /* MIN */
    OBJ_OP_CREATE(min, short, short, OSHMEM_OP_MIN, OSHMEM_OP_TYPE_SHORT);
    OBJ_OP_CREATE(min, int, int, OSHMEM_OP_MIN, OSHMEM_OP_TYPE_INT);
    OBJ_OP_CREATE(min, long, long, OSHMEM_OP_MIN, OSHMEM_OP_TYPE_LONG);
    OBJ_OP_CREATE(min, longlong, long long, OSHMEM_OP_MIN, OSHMEM_OP_TYPE_LLONG);
    OBJ_OP_CREATE(min, float, float, OSHMEM_OP_MIN, OSHMEM_OP_TYPE_FLOAT);
    OBJ_OP_CREATE(min, double, double, OSHMEM_OP_MIN, OSHMEM_OP_TYPE_DOUBLE);
    OBJ_OP_CREATE(min, longdouble, long double, OSHMEM_OP_MIN, OSHMEM_OP_TYPE_LDOUBLE);
    OBJ_OP_CREATE(min, fint2, ompi_fortran_integer2_t, OSHMEM_OP_MIN, OSHMEM_OP_TYPE_FINT2);
    OBJ_OP_CREATE(min, fint4, ompi_fortran_integer4_t, OSHMEM_OP_MIN, OSHMEM_OP_TYPE_FINT4);
    OBJ_OP_CREATE(min, fint8, ompi_fortran_integer8_t, OSHMEM_OP_MIN, OSHMEM_OP_TYPE_FINT8);
    OBJ_OP_CREATE(min, freal4, ompi_fortran_real4_t, OSHMEM_OP_MIN, OSHMEM_OP_TYPE_FREAL4);
    OBJ_OP_CREATE(min, freal8, ompi_fortran_real8_t, OSHMEM_OP_MIN, OSHMEM_OP_TYPE_FREAL8);
#if OMPI_HAVE_FORTRAN_REAL16
    OBJ_OP_CREATE(min, freal16, ompi_fortran_real16_t, OSHMEM_OP_MIN, OSHMEM_OP_TYPE_FREAL16);
#endif
    OBJ_OP_CREATE(min, int16, int16_t, OSHMEM_OP_MIN, OSHMEM_OP_TYPE_INT16_T);
    OBJ_OP_CREATE(min, int32, int32_t, OSHMEM_OP_MIN, OSHMEM_OP_TYPE_INT32_T);
    OBJ_OP_CREATE(min, int64, int64_t, OSHMEM_OP_MIN, OSHMEM_OP_TYPE_INT64_T);

    /* SUM */
    OBJ_OP_CREATE(sum, short, short, OSHMEM_OP_SUM, OSHMEM_OP_TYPE_SHORT);
    OBJ_OP_CREATE(sum, int, int, OSHMEM_OP_SUM, OSHMEM_OP_TYPE_INT);
    OBJ_OP_CREATE(sum, long, long, OSHMEM_OP_SUM, OSHMEM_OP_TYPE_LONG);
    OBJ_OP_CREATE(sum, longlong, long long, OSHMEM_OP_SUM, OSHMEM_OP_TYPE_LLONG);
    OBJ_OP_CREATE(sum, float, float, OSHMEM_OP_SUM, OSHMEM_OP_TYPE_FLOAT);
    OBJ_OP_CREATE(sum, double, double, OSHMEM_OP_SUM, OSHMEM_OP_TYPE_DOUBLE);
    OBJ_OP_CREATE(sum, longdouble, long double, OSHMEM_OP_SUM, OSHMEM_OP_TYPE_LDOUBLE);
    OBJ_OP_CREATE(sum, complexf, float complex, OSHMEM_OP_SUM, OSHMEM_OP_TYPE_FCOMPLEX);
    OBJ_OP_CREATE(sum, complexd, double complex, OSHMEM_OP_SUM, OSHMEM_OP_TYPE_DCOMPLEX);
    OBJ_OP_CREATE(sum, fint2, ompi_fortran_integer2_t, OSHMEM_OP_SUM, OSHMEM_OP_TYPE_FINT2);
    OBJ_OP_CREATE(sum, fint4, ompi_fortran_integer4_t, OSHMEM_OP_SUM, OSHMEM_OP_TYPE_FINT4);
    OBJ_OP_CREATE(sum, fint8, ompi_fortran_integer8_t, OSHMEM_OP_SUM, OSHMEM_OP_TYPE_FINT8);
    OBJ_OP_CREATE(sum, freal4, ompi_fortran_real4_t, OSHMEM_OP_SUM, OSHMEM_OP_TYPE_FREAL4);
    OBJ_OP_CREATE(sum, freal8, ompi_fortran_real8_t, OSHMEM_OP_SUM, OSHMEM_OP_TYPE_FREAL8);
#if OMPI_HAVE_FORTRAN_REAL16
    OBJ_OP_CREATE(sum, freal16, ompi_fortran_real16_t, OSHMEM_OP_SUM, OSHMEM_OP_TYPE_FREAL16);
#endif
    OBJ_OP_CREATE(sum, int16, int16_t, OSHMEM_OP_SUM, OSHMEM_OP_TYPE_INT16_T);
    OBJ_OP_CREATE(sum, int32, int32_t, OSHMEM_OP_SUM, OSHMEM_OP_TYPE_INT32_T);
    OBJ_OP_CREATE(sum, int64, int64_t, OSHMEM_OP_SUM, OSHMEM_OP_TYPE_INT64_T);

    /* PROD */
    OBJ_OP_CREATE(prod, short, short, OSHMEM_OP_PROD, OSHMEM_OP_TYPE_SHORT);
    OBJ_OP_CREATE(prod, int, int, OSHMEM_OP_PROD, OSHMEM_OP_TYPE_INT);
    OBJ_OP_CREATE(prod, long, long, OSHMEM_OP_PROD, OSHMEM_OP_TYPE_LONG);
    OBJ_OP_CREATE(prod, longlong, long long, OSHMEM_OP_PROD, OSHMEM_OP_TYPE_LLONG);
    OBJ_OP_CREATE(prod, float, float, OSHMEM_OP_PROD, OSHMEM_OP_TYPE_FLOAT);
    OBJ_OP_CREATE(prod, double, double, OSHMEM_OP_PROD, OSHMEM_OP_TYPE_DOUBLE);
    OBJ_OP_CREATE(prod, longdouble, long double, OSHMEM_OP_PROD, OSHMEM_OP_TYPE_LDOUBLE);
    OBJ_OP_CREATE(prod, complexf, float complex, OSHMEM_OP_PROD, OSHMEM_OP_TYPE_FCOMPLEX);
    OBJ_OP_CREATE(prod, complexd, double complex, OSHMEM_OP_PROD, OSHMEM_OP_TYPE_DCOMPLEX);
    OBJ_OP_CREATE(prod, fint2, ompi_fortran_integer2_t, OSHMEM_OP_PROD, OSHMEM_OP_TYPE_FINT2);
    OBJ_OP_CREATE(prod, fint4, ompi_fortran_integer4_t, OSHMEM_OP_PROD, OSHMEM_OP_TYPE_FINT4);
    OBJ_OP_CREATE(prod, fint8, ompi_fortran_integer8_t, OSHMEM_OP_PROD, OSHMEM_OP_TYPE_FINT8);
    OBJ_OP_CREATE(prod, freal4, ompi_fortran_real4_t, OSHMEM_OP_PROD, OSHMEM_OP_TYPE_FREAL4);
    OBJ_OP_CREATE(prod, freal8, ompi_fortran_real8_t, OSHMEM_OP_PROD, OSHMEM_OP_TYPE_FREAL8);
#if OMPI_HAVE_FORTRAN_REAL16
    OBJ_OP_CREATE(prod, freal16, ompi_fortran_real16_t, OSHMEM_OP_PROD, OSHMEM_OP_TYPE_FREAL16);
#endif
    OBJ_OP_CREATE(prod, int16, int16_t, OSHMEM_OP_PROD, OSHMEM_OP_TYPE_INT16_T);
    OBJ_OP_CREATE(prod, int32, int32_t, OSHMEM_OP_PROD, OSHMEM_OP_TYPE_INT32_T);
    OBJ_OP_CREATE(prod, int64, int64_t, OSHMEM_OP_PROD, OSHMEM_OP_TYPE_INT64_T);

    return OSHMEM_SUCCESS;
}

int oshmem_op_finalize(void)
{
    int max, i;
    oshmem_op_t *op;

    /* Check whether we have some left */
    max = opal_pointer_array_get_size(&oshmem_op_array);
    for (i = 0; i < max; i++) {
        op = (oshmem_op_t *) opal_pointer_array_get_item(&oshmem_op_array, i);
        if (NULL != op) {
            OBJ_RELEASE(op);
        }
    }

    OBJ_DESTRUCT(&oshmem_op_array);

    return OSHMEM_SUCCESS;
}

/**************************************************************************
 *
 * Static functions
 *
 **************************************************************************/

/*
 * Op constructor
 */
static void oshmem_op_construct(oshmem_op_t *object)
{
    object->id = opal_pointer_array_add(&oshmem_op_array, object);
}

/*
 * Op destructor
 */
static void oshmem_op_destruct(oshmem_op_t *object)
{
    if (NULL != opal_pointer_array_get_item(&oshmem_op_array, object->id)) {
        opal_pointer_array_set_item(&oshmem_op_array, object->id, NULL );
    }
}
