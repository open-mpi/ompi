/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2017      ARM, Inc. All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"
#include "ompi/mca/bml/base/base.h"
#include "opal/datatype/opal_convertor.h"
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

#define SPML_BASE_TEST_CASE(_type, _shmem_type, _addr, _value, _cmp, _out_value) \
    case _shmem_type: \
        { \
            _type typed_value = *(const _type*)_value; \
            const _type *typed_addr = (const _type*)_addr; \
            SPML_BASE_DO_CMP((*_out_value), typed_addr , _cmp, typed_value); \
        } \
        break;

/**
 * Check on a variable given in addr to see it is not equal to value.
 */
int mca_spml_base_test(void* addr, int cmp, void* value, int datatype, int *out_value)
{
    switch (datatype) {
        SPML_BASE_TEST_CASE(int, SHMEM_INT, addr, value, cmp, out_value);
        SPML_BASE_TEST_CASE(unsigned int, SHMEM_UINT, addr, value, cmp, out_value);
        SPML_BASE_TEST_CASE(long, SHMEM_LONG, addr, value, cmp, out_value);
        SPML_BASE_TEST_CASE(unsigned long, SHMEM_ULONG, addr, value, cmp, out_value);
        SPML_BASE_TEST_CASE(short, SHMEM_SHORT, addr, value, cmp, out_value);
        SPML_BASE_TEST_CASE(unsigned short, SHMEM_USHORT, addr, value, cmp, out_value);
        SPML_BASE_TEST_CASE(long long, SHMEM_LLONG, addr, value, cmp, out_value);
        SPML_BASE_TEST_CASE(unsigned long long, SHMEM_ULLONG, addr, value, cmp, out_value);
        SPML_BASE_TEST_CASE(int32_t, SHMEM_INT32_T, addr, value, cmp, out_value);
        SPML_BASE_TEST_CASE(uint32_t, SHMEM_UINT32_T, addr, value, cmp, out_value);
        SPML_BASE_TEST_CASE(int64_t, SHMEM_INT64_T, addr, value, cmp, out_value);
        SPML_BASE_TEST_CASE(uint64_t, SHMEM_UINT64_T, addr, value, cmp, out_value);
        SPML_BASE_TEST_CASE(size_t, SHMEM_SIZE_T, addr, value, cmp, out_value);
        SPML_BASE_TEST_CASE(ptrdiff_t, SHMEM_PTRDIFF_T, addr, value, cmp, out_value);
        SPML_BASE_TEST_CASE(ompi_fortran_integer_t, SHMEM_FINT, addr, value, cmp, out_value);
        SPML_BASE_TEST_CASE(ompi_fortran_integer4_t, SHMEM_FINT4, addr, value, cmp, out_value);
        SPML_BASE_TEST_CASE(ompi_fortran_integer8_t, SHMEM_FINT8, addr, value, cmp, out_value);
    }

    return OSHMEM_SUCCESS;
}

#define SPML_BASE_WAIT_CASE(_type, _shmem_type, _addr, _value, _cmp, _res) \
    case _shmem_type: \
        { \
            _type typed_value = *(const _type*)_value; \
            const _type *typed_addr = (const _type*)_addr; \
            SPML_BASE_DO_WAIT(_res, typed_addr, _cmp, typed_value); \
        } \
        break;

int mca_spml_base_wait(void* addr, int cmp, void* value, int datatype)
{
    int res = 0;

    switch (datatype) {
        SPML_BASE_WAIT_CASE(int, SHMEM_INT, addr, value, cmp, res);
        SPML_BASE_WAIT_CASE(unsigned int, SHMEM_UINT, addr, value, cmp, res);
        SPML_BASE_WAIT_CASE(long, SHMEM_LONG, addr, value, cmp, res);
        SPML_BASE_WAIT_CASE(unsigned long, SHMEM_ULONG, addr, value, cmp, res);
        SPML_BASE_WAIT_CASE(short, SHMEM_SHORT, addr, value, cmp, res);
        SPML_BASE_WAIT_CASE(unsigned short, SHMEM_USHORT, addr, value, cmp, res);
        SPML_BASE_WAIT_CASE(long long, SHMEM_LLONG, addr, value, cmp, res);
        SPML_BASE_WAIT_CASE(unsigned long long, SHMEM_ULLONG, addr, value, cmp, res);
        SPML_BASE_WAIT_CASE(int32_t, SHMEM_INT32_T, addr, value, cmp, res);
        SPML_BASE_WAIT_CASE(uint32_t, SHMEM_UINT32_T, addr, value, cmp, res);
        SPML_BASE_WAIT_CASE(int64_t, SHMEM_INT64_T, addr, value, cmp, res);
        SPML_BASE_WAIT_CASE(uint64_t, SHMEM_UINT64_T, addr, value, cmp, res);
        SPML_BASE_WAIT_CASE(size_t, SHMEM_SIZE_T, addr, value, cmp, res);
        SPML_BASE_WAIT_CASE(ptrdiff_t, SHMEM_PTRDIFF_T, addr, value, cmp, res);
        SPML_BASE_WAIT_CASE(ompi_fortran_integer_t, SHMEM_FINT, addr, value, cmp, res);
        SPML_BASE_WAIT_CASE(ompi_fortran_integer4_t, SHMEM_FINT4, addr, value, cmp, res);
        SPML_BASE_WAIT_CASE(ompi_fortran_integer8_t, SHMEM_FINT8, addr, value, cmp, res);
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

void mca_spml_base_rmkey_free(sshmem_mkey_t *mkey, int pe)
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
