/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2017      ARM, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"
#include "ompi/mca/bml/base/base.h"
#include "opal/datatype/opal_convertor.h"
#include "orte/include/orte/types.h"
#include "orte/runtime/orte_globals.h"
#include "oshmem/proc/proc.h"
#include "oshmem/mca/spml/base/base.h"
#include "opal/mca/btl/btl.h"

#define SPML_BASE_DO_CMP(_res, _addr, _op, _val) \
    switch((_op)) { \
        case SHMEM_CMP_EQ: \
            _res = *(_addr) == (_val) ? 1 : 0; \
            break; \
        case SHMEM_CMP_NE: \
            _res = *(_addr) != (_val) ? 1 : 0; \
            break; \
        case SHMEM_CMP_GT: \
            _res =  *(_addr) > (_val) ? 1 : 0; \
            break; \
        case SHMEM_CMP_LE: \
            _res = *(_addr) <= (_val) ? 1 : 0; \
            break; \
        case SHMEM_CMP_LT: \
            _res = *(_addr) < (_val) ?  1 : 0; \
            break; \
        case SHMEM_CMP_GE: \
            _res = *(_addr) >= (_val) ? 1 : 0; \
            break; \
    }

#define SPML_BASE_DO_WAIT(_res, _addr, _op, _val)  \
    do {                                           \
        SPML_BASE_DO_CMP(_res, _addr, _op, _val);  \
        if (_res == 0) {                           \
            opal_progress();                       \
        }                                          \
    } while (_res == 0);

/**
 * Check on a variable given in addr to see it is not equal to value.
 */
int mca_spml_base_test(void* addr, int cmp, void* value, int datatype, int *out_value)
{
    volatile int *int_addr;
    volatile long *long_addr;
    volatile short *short_addr;
    volatile long long *longlong_addr;
    volatile int32_t *int32_addr;
    volatile int64_t *int64_addr;

    int int_value;
    long long_value;
    short short_value;
    long long longlong_value;
    int32_t int32_value;
    int64_t int64_value;

    ompi_fortran_integer_t *fint_addr, fint_value;
    ompi_fortran_integer4_t *fint4_addr, fint4_value;
    ompi_fortran_integer8_t *fint8_addr, fint8_value;

    switch (datatype) {

    /* Int */
    case SHMEM_INT:
        int_value = *(int*) value;
        int_addr = (int*) addr;
        SPML_BASE_DO_CMP((*out_value), int_addr, cmp, int_value);
        break;

        /* Short */
    case SHMEM_SHORT:
        short_value = *(short*) value;
        short_addr = (short*) addr;
        SPML_BASE_DO_CMP((*out_value), short_addr, cmp, short_value);
        break;

        /* Long */
    case SHMEM_LONG:
        long_value = *(long*) value;
        long_addr = (long*) addr;
        SPML_BASE_DO_CMP((*out_value), long_addr, cmp, long_value);
        break;

        /* Long-Long */
    case SHMEM_LLONG:
        longlong_value = *(long long*) value;
        longlong_addr = (long long*) addr;
        SPML_BASE_DO_CMP((*out_value), longlong_addr, cmp, longlong_value);
        break;

       /* Int32_t */
    case SHMEM_INT32_T:
        int32_value = *(int32_t*) value;
        int32_addr = (int32_t*) addr;
        SPML_BASE_DO_CMP((*out_value), int32_addr, cmp, int32_value);
        break;

       /* Int64_t */
    case SHMEM_INT64_T:
        int64_value = *(int64_t*) value;
        int64_addr = (int64_t*) addr;
        SPML_BASE_DO_CMP((*out_value), int64_addr, cmp, int64_value);
        break;

        /*C equivalent of Fortran integer type */
    case SHMEM_FINT:
        fint_value = *(ompi_fortran_integer_t *) value;
        fint_addr = (ompi_fortran_integer_t *) addr;
        SPML_BASE_DO_CMP((*out_value), fint_addr, cmp, fint_value);
        break;

        /*C equivalent of Fortran int4 type*/
    case SHMEM_FINT4:
        fint4_value = *(ompi_fortran_integer4_t *) value;
        fint4_addr = (ompi_fortran_integer4_t *) addr;
        SPML_BASE_DO_CMP((*out_value), fint4_addr, cmp, fint4_value);
        break;

        /*C equivalent of Fortran int8 type*/
    case SHMEM_FINT8:
        fint8_value = *(ompi_fortran_integer8_t *) value;
        fint8_addr = (ompi_fortran_integer8_t *) addr;
        SPML_BASE_DO_CMP((*out_value), fint8_addr, cmp, fint8_value);
        break;
    }

    return OSHMEM_SUCCESS;
}

int mca_spml_base_wait(void* addr, int cmp, void* value, int datatype)
{
    volatile int *int_addr;
    volatile long *long_addr;
    volatile short *short_addr;
    volatile long long *longlong_addr;
    volatile int32_t *int32_addr;
    volatile int64_t *int64_addr;

    int int_value;
    long long_value;
    short short_value;
    long long longlong_value;
    int32_t int32_value;
    int64_t int64_value;

    ompi_fortran_integer_t *fint_addr, fint_value;
    ompi_fortran_integer4_t *fint4_addr, fint4_value;
    ompi_fortran_integer8_t *fint8_addr, fint8_value;

    int res = 0;

    switch (datatype) {

    /* Int */
    case SHMEM_INT:
        int_value = *(int*) value;
        int_addr = (int*) addr;
        SPML_BASE_DO_WAIT(res, int_addr, cmp, int_value);
        break;

        /* Short */
    case SHMEM_SHORT:
        short_value = *(short*) value;
        short_addr = (short*) addr;
        SPML_BASE_DO_WAIT(res, short_addr, cmp, short_value);
        break;

        /* Long */
    case SHMEM_LONG:
        long_value = *(long*) value;
        long_addr = (long*) addr;
        SPML_BASE_DO_WAIT(res, long_addr, cmp, long_value);
        break;

        /* Long-Long */
    case SHMEM_LLONG:
        longlong_value = *(long long*) value;
        longlong_addr = (long long*) addr;
        SPML_BASE_DO_WAIT(res, longlong_addr, cmp, longlong_value);
        break;

       /* Int32_t */
    case SHMEM_INT32_T:
        int32_value = *(int32_t*) value;
        int32_addr = (int32_t*) addr;
        SPML_BASE_DO_WAIT(res, int32_addr, cmp, int32_value);
        break;

       /* Int64_t */
    case SHMEM_INT64_T:
        int64_value = *(int64_t*) value;
        int64_addr = (int64_t*) addr;
        SPML_BASE_DO_WAIT(res, int64_addr, cmp, int64_value);
        break;

        /*C equivalent of Fortran integer type */
    case SHMEM_FINT:
        fint_value = *(ompi_fortran_integer_t *) value;
        fint_addr = (ompi_fortran_integer_t *) addr;
        SPML_BASE_DO_WAIT(res, fint_addr, cmp, fint_value);
        break;

        /*C equivalent of Fortran int4 type*/
    case SHMEM_FINT4:
        fint4_value = *(ompi_fortran_integer4_t *) value;
        fint4_addr = (ompi_fortran_integer4_t *) addr;
        SPML_BASE_DO_WAIT(res, fint4_addr, cmp, fint4_value);
        break;

        /*C equivalent of Fortran int8 type*/
    case SHMEM_FINT8:
        fint8_value = *(ompi_fortran_integer8_t *) value;
        fint8_addr = (ompi_fortran_integer8_t *) addr;
        SPML_BASE_DO_WAIT(res, fint8_addr, cmp, fint8_value);
        break;
    }

    return OSHMEM_SUCCESS;
}


/**
 * Waits for completion of a non-blocking put or get issued by the calling PE.
 * This function waits for completion of a single non-blocking transfer issued by
 * shmem_put_nb() or shmem_get_nb() (or related functions) when called with the
 * address of a completion handle.
 * Completion of the call to shmem_wait_nb() ensures that a non-blocking transfer has
 * completed. The source buffer may then be reused.
 */
int mca_spml_base_wait_nb(void* handle)
{
    MCA_SPML_CALL(quiet(oshmem_ctx_default));

    return OSHMEM_SUCCESS;
}

int mca_spml_base_oob_get_mkeys(shmem_ctx_t ctx, int pe, uint32_t segno, sshmem_mkey_t *mkeys)
{
    return OSHMEM_ERROR;
}

void mca_spml_base_rmkey_unpack(shmem_ctx_t ctx, sshmem_mkey_t *mkey, uint32_t segno, int pe, int tr_id)
{
}

void mca_spml_base_rmkey_free(sshmem_mkey_t *mkey)
{
}

void *mca_spml_base_rmkey_ptr(const void *dst_addr, sshmem_mkey_t *mkey, int pe)
{
    return NULL;
}

int mca_spml_base_put_nb(void *dst_addr, size_t size,
                         void *src_addr, int dst, void **handle)
{
    return OSHMEM_ERROR;
}

int mca_spml_base_get_nb(void *dst_addr, size_t size,
                         void *src_addr, int src, void **handle)
{
    return OSHMEM_ERROR;
}

void mca_spml_base_memuse_hook(void *addr, size_t length)
{
}

int mca_spml_base_put_all_nb(void *target, const void *source,
                             size_t size, long *counter)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}
