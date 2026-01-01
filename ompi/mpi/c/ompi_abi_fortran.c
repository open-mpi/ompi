/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
 * Copyright (c) 2024-2025 Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/info/info.h"
#include "ompi/instance/instance.h"
#include "ompi/mpi/c/bindings.h"
#include "opal/util/bit_ops.h"
#include "opal/include/opal_stdint.h"

static ompi_info_t *abi_fortran_info_from_user = NULL;

typedef struct {
    int64_t true_value;
    int64_t false_value;
#ifdef HAVE_INT128_T
    int128_t true_value128;
    int128_t false_value128;
#else
    int64_t true_value128;
    int64_t false_value128;
#endif
    bool is_set;
    bool using_int128;
} ompi_abi_fortran_user_bool_info_t;

static ompi_abi_fortran_user_bool_info_t user_logicals[5];

static int countbits32(unsigned int n) {
    int count = 0;
    while (n) {
        n &= (n - 1); // Clear the least significant bit set
        count++;
    }
    return count;
}
static int ompi_abi_fortran_finalize(void) 
{
    if (NULL != abi_fortran_info_from_user) {
        ompi_info_free(&abi_fortran_info_from_user);
        abi_fortran_info_from_user = NULL;
    }
    return OMPI_SUCCESS;
}
        
int ompi_abi_get_fortran_info(ompi_info_t **info)
{
    ompi_info_t *newinfo = NULL;
    int ret = MPI_SUCCESS;
#if OMPI_BUILD_FORTRAN_BINDINGS
    char tmp[16];
    const char true_str[]="true";
    const char false_str[]="false";
    const char *cptr;

    newinfo = ompi_info_allocate ();
    if (NULL == (*info)) {
        return MPI_ERR_NO_MEM;
    }

#if OMPI_SIZEOF_FORTRAN_LOGICAL
    snprintf(tmp, 16, "%d", OMPI_SIZEOF_FORTRAN_LOGICAL);
    ret = opal_info_set(&newinfo->super, "mpi_logical_size", tmp);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }
#endif

#if OMPI_SIZEOF_FORTRAN_INTEGER
    snprintf(tmp, 16, "%d", OMPI_SIZEOF_FORTRAN_INTEGER);
    ret = opal_info_set(&newinfo->super, "mpi_integer_size", tmp);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }
#endif

#if OMPI_SIZEOF_FORTRAN_REAL
    snprintf(tmp, 16, "%d", OMPI_SIZEOF_FORTRAN_REAL);
    ret = opal_info_set(&newinfo->super, "mpi_real_size", tmp);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }
#endif

#if OMPI_SIZEOF_FORTRAN_DOUBLE_PRECISION
    snprintf(tmp, 16, "%d", OMPI_SIZEOF_FORTRAN_DOUBLE_PRECISION);
    ret = opal_info_set(&newinfo->super, "mpi_double_precision_size", tmp);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }
#endif

#define OMPI_DATATYPE_SET_SUPPORTED(type, TYPE) do { \
    cptr = (OMPI_DATATYPE_##TYPE != OMPI_DATATYPE_MPI_UNAVAILABLE) ? true_str : false_str; \
    ret = opal_info_set(&newinfo->super, #type "_supported", cptr); \
    if (OPAL_SUCCESS != ret) goto err_cleanup; } while (0)

    OMPI_DATATYPE_SET_SUPPORTED(mpi_logical1, MPI_LOGICAL1);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_logical2, MPI_LOGICAL2);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_logical4, MPI_LOGICAL4);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_logical8, MPI_LOGICAL8);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_logical16, MPI_LOGICAL16);

    OMPI_DATATYPE_SET_SUPPORTED(mpi_integer1, MPI_INTEGER1);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_integer2, MPI_INTEGER2);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_integer4, MPI_INTEGER4);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_integer8, MPI_INTEGER8);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_integer16, MPI_INTEGER16);

    OMPI_DATATYPE_SET_SUPPORTED(mpi_real2, MPI_REAL2);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_real4, MPI_REAL4);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_real8, MPI_REAL8);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_real16, MPI_REAL16);

    OMPI_DATATYPE_SET_SUPPORTED(mpi_complex4, MPI_COMPLEX4);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_complex8, MPI_COMPLEX8);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_complex16, MPI_COMPLEX16);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_complex32, MPI_COMPLEX32);

    OMPI_DATATYPE_SET_SUPPORTED(mpi_double_complex, MPI_DOUBLE_COMPLEX);

#undef OMPI_DATATYPE_SET_SUPPORTED
    
    *info = newinfo;

#else

    if(NULL != abi_fortran_info_from_user) {
        ret = ompi_info_dup(abi_fortran_info_from_user, &newinfo);
        if (OPAL_SUCCESS != ret) {
            goto err_cleanup;
        }
        *info = newinfo;
    } else {
        *info = MPI_INFO_NULL;
    }

#endif /*  OMPI_BUILD_FORTRAN_BINDINGS */


    return ret;

err_cleanup:
    if (NULL != newinfo) {
        ompi_info_free(&newinfo);
    }

    return ret;
}

int ompi_abi_set_fortran_info(ompi_info_t *info)
{
    int ret = MPI_SUCCESS;
    static bool already_called = false;   
        
    /*
     * according to MPI 5.0 standard this function can only be called once.
     */
    if (true == already_called) {         
        return MPI_ERR_ABI;
    }

    already_called = true;

    /* 
     * If OMPI was built with fortran enabled, just tell the app
     * no to any of this setting fortran info stuff.
     */
#if OMPI_BUILD_FORTRAN_BINDINGS
    ret = MPI_ERR_ABI;
#endif

    /*
     * If no fortran bindings, dup user supplied fortran info.  For now just to
     * be able to return whatever the user provided to
     * subsequent calls to MPI_Abi_get_fortran_info.
     * Perhaps someday this info can be used to handle
     * Fortran MPI datatypes even when OMPI is not configured
     * with Fortran support,but that will be a heavily lift.
     */
    if (MPI_SUCCESS == ret) {
        ret = ompi_info_dup(info, &abi_fortran_info_from_user);
        if (MPI_SUCCESS == ret) {
            ompi_mpi_instance_append_finalize (ompi_abi_fortran_finalize);
        }
    }

    return ret;

}

int ompi_abi_get_fortran_booleans(int logical_size, void *logical_true, void *logical_false, int *is_set)
{
    int ret = MPI_SUCCESS;

#if  OMPI_HAVE_FORTRAN_LOGICAL
    bool unavailable = false;
    bool use_int8_t = false, use_int16_t = false, use_int32_t = false, use_int64_t = false;
    /*
     * handle special case of "default" LOGICAL size.
     */
    if (OMPI_SIZEOF_FORTRAN_LOGICAL == logical_size) {
        switch(OMPI_SIZEOF_FORTRAN_LOGICAL) {
            case 1:
                use_int8_t = true;
                break;
            case 2:
                use_int16_t = true;
                break;
            case 4:
                use_int32_t = true;
                break;
            case 8:
                use_int64_t = true;
                break;
            default:
                unavailable = true;
                break;
        }
    } else {
    switch (logical_size) {
        case OMPI_SIZEOF_FORTRAN_LOGICAL1:
            switch (OMPI_DATATYPE_MPI_LOGICAL1) {
            case OMPI_DATATYPE_MPI_INT8_T:
                use_int8_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT16_T:
                use_int16_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT32_T:
                use_int32_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT64_T:
                use_int64_t = true;
                break;
            default:
                unavailable = true;
                break;
             }
        case OMPI_SIZEOF_FORTRAN_LOGICAL2:
            switch (OMPI_DATATYPE_MPI_LOGICAL2) {
            case OMPI_DATATYPE_MPI_INT8_T:
                use_int8_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT16_T:
                use_int16_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT32_T:
                use_int32_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT64_T:
                use_int64_t = true;
                break;
            default:
                unavailable = true;
                break;
            }
        case OMPI_SIZEOF_FORTRAN_LOGICAL4:
            switch (OMPI_DATATYPE_MPI_LOGICAL4) {
            case OMPI_DATATYPE_MPI_INT8_T:
                use_int8_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT16_T:
                use_int16_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT32_T:
                use_int32_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT64_T:
                use_int64_t = true;
                break;
            default:
                unavailable = true;
                break;
            }
        case OMPI_SIZEOF_FORTRAN_LOGICAL8:
            switch (OMPI_DATATYPE_MPI_LOGICAL8) {
            case OMPI_DATATYPE_MPI_INT8_T:
                use_int8_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT16_T:
                use_int16_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT32_T:
                use_int32_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT64_T:
                use_int64_t = true;
                break;
            default:
                unavailable = true;
                break;
            }
        case OMPI_SIZEOF_FORTRAN_LOGICAL16:
            switch (OMPI_DATATYPE_MPI_LOGICAL16) {
            case OMPI_DATATYPE_MPI_INT8_T:
                use_int8_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT16_T:
                use_int16_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT32_T:
                use_int32_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT64_T:
                use_int64_t = true;
                break;
            default:
                unavailable = true;
                break;
            }
        default:
            unavailable = true;
        }
    }

    if (true == unavailable) {
        *is_set = 0;
    } else {
        *is_set = 1;
        if (true == use_int8_t) {
            *(int8_t *)logical_true = (int8_t)OMPI_FORTRAN_VALUE_TRUE;
            *(int8_t *)logical_false = (int8_t)OMPI_FORTRAN_VALUE_FALSE;
        } else if (true == use_int16_t) {
            *(int16_t *)logical_true = (int16_t)OMPI_FORTRAN_VALUE_TRUE;
            *(int16_t *)logical_false = (int16_t)OMPI_FORTRAN_VALUE_FALSE;
        } else if (true == use_int32_t) {
            *(int32_t *)logical_true = (int32_t)OMPI_FORTRAN_VALUE_TRUE;
            *(int32_t *)logical_false = (int32_t)OMPI_FORTRAN_VALUE_FALSE;
        } else if (true == use_int64_t) {
            *(int64_t *)logical_true = (int64_t)OMPI_FORTRAN_VALUE_TRUE;
            *(int64_t *)logical_false = (int64_t)OMPI_FORTRAN_VALUE_FALSE;
        }
    }

#else

/*
 * OMPI wasn't built with fortran bindings support so 
 * see if the user set something with MPI_ABI_set_fortran_boleans
 */
    int logical_size_pow2;

    /* check logical size to be pow2 */
    
    if(countbits32((unsigned int)logical_size) > 1) {
        return MPI_ERR_ARG;
    }

    logical_size_pow2 = opal_hibit(logical_size, 0);

    if (4 > logical_size_pow2) {
        return MPI_ERR_ARG;
    }

    if (false == user_logicals[logical_size_pow2].is_set) {
        *is_set = 0;
    } else { 
        switch(logical_size_pow2) {
        case 0:
            *(int8_t *)logical_true = (int8_t)user_logicals[logical_size_pow2].true_value;
            *(int8_t *)logical_false = (int8_t)user_logicals[logical_size_pow2].false_value;
            *is_set = 1;
            break;
        case 1:
            *(int16_t *)logical_true = (int16_t)user_logicals[logical_size_pow2].true_value;
            *(int16_t *)logical_false = (int16_t)user_logicals[logical_size_pow2].false_value;
            *is_set = 1;
            break;
        case 2:
            *(int32_t *)logical_true = (int32_t)user_logicals[logical_size_pow2].true_value;
            *(int32_t *)logical_false = (int32_t)user_logicals[logical_size_pow2].false_value;
            *is_set = 1;
            break;
        case 3:
            *(int64_t *)logical_true = (int64_t)user_logicals[logical_size_pow2].true_value;
            *(int64_t *)logical_false = (int64_t)user_logicals[logical_size_pow2].false_value;
            *is_set = 1;
            break;
#ifdef HAVE_INT128_T
        case 4:
            *(int128_t *)logical_true = (int128_t)user_logicals[logical_size_pow2].true_value128;
            *(int128_t *)logical_false = (int128_t)user_logicals[logical_size_pow2].false_value128;
            *is_set = 1;
            break;
#endif
        default:
            ret = MPI_ERR_ARG;
            break;
        }
    }

#endif  /* OMPI_HAVE_FORTRAN_LOGICAL */
    return ret;
}

int ompi_abi_set_fortran_booleans(int logical_size, void *logical_true, void *logical_false)
{
    int ret=MPI_SUCCESS;
    static bool already_called = false;

    /*
     * according to MPI 5.0 standard this function can only be called once.
     */
    if (true == already_called) {
        return MPI_ERR_ABI;
    }

    already_called = true;

    int logical_size_pow2;
    int64_t logical_true64 = 0;
    int64_t logical_false64 = 0;
#ifdef HAVING_INT128_T
    int128_t logical_true128 = 0;
    int128_t logical_false128 = 0;
#else
    int64_t logical_true128 = 0;
    int64_t logical_false128 = 0;
#endif
    bool using128 = false;

    /* check logical size to be pow2 */

    if(countbits32((unsigned int)logical_size) > 1) {
        return MPI_ERR_ARG;
    }

    logical_size_pow2 = opal_hibit(logical_size, 0);

    switch(logical_size_pow2) {
    case 0:
        logical_true64 = (int64_t)*(int8_t *)logical_true;
        logical_false64 = (int64_t)*(int8_t *)logical_false;
        break;
    case 1:
        logical_true64 = (int64_t)*(int16_t *)logical_true;
        logical_false64 = (int64_t)*(int16_t *)logical_false;
        break;
    case 2:
        logical_true64 = (int64_t)*(int32_t *)logical_true;
        logical_false64 = (int64_t)*(int32_t *)logical_false;
        break;
    case 3:
        logical_true64 = (int64_t)*(int64_t *)logical_true;
        logical_false64 = (int64_t)*(int64_t *)logical_false;
        break;
#ifdef HAVE_INT128_T
    case 4:
        logical_true128 = (int128_t)*(int128_t *)logical_true;
        logical_false128 = (int128_t)*(int128_t *)logical_false;
        using128 = true;
        break;
#endif
    default:
        ret = MPI_ERR_ARG;
        break;
    }

#ifndef HAVE_INT128_T
    assert(false == using128);
#endif

    if (true == using128) {
        user_logicals[logical_size_pow2].true_value128 = logical_true128;
        user_logicals[logical_size_pow2].false_value128 = logical_false128;
        user_logicals[logical_size_pow2].is_set = true;
        user_logicals[logical_size_pow2].using_int128 = true;
    } else { 
        user_logicals[logical_size_pow2].true_value = logical_true64;
        user_logicals[logical_size_pow2].false_value = logical_false64;
        user_logicals[logical_size_pow2].is_set = true;
    }

    return ret;
}

