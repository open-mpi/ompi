/* THIS FILE WAS AUTOMATICALLY GENERATED. DO NOT EDIT BY HAND. */
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

static ompi_info_t *abi_fortran_info_from_user = NULL;

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

#if OMPI_SIZEOF_FORTRAN_LOGICAL1
    cptr = true_str;
#else
    cptr = false_str;
#endif
    ret = opal_info_set(&newinfo->super, "mpi_logical1_supported", cptr);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }

#if OMPI_SIZEOF_FORTRAN_LOGICAL2
    cptr = true_str;
#else
    cptr = false_str;
#endif
    ret = opal_info_set(&newinfo->super, "mpi_logical2_supported", cptr);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }

#if OMPI_SIZEOF_FORTRAN_LOGICAL4
    cptr = true_str;
#else
    cptr = false_str;
#endif
    ret = opal_info_set(&newinfo->super, "mpi_logical4_supported", cptr);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }

#if OMPI_SIZEOF_FORTRAN_LOGICAL8
    cptr = true_str;
#else
    cptr = false_str;
#endif
    ret = opal_info_set(&newinfo->super, "mpi_logical8_supported", cptr);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }

#if 0
/* TODO need to bolt in fortran logical16 data type */
#if OMPI_SIZEOF_FORTRAN_LOGICAL16
    cptr = true_str;
#else
    cptr = false_str;
#endif
#endif
    ret = opal_info_set(&newinfo->super, "mpi_logical16_supported", "false");
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }

#if OMPI_SIZEOF_FORTRAN_INTEGER1
    cptr = true_str;
#else
    cptr = false_str;
#endif
    ret = opal_info_set(&newinfo->super, "mpi_integer1_supported", cptr);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }
    
#if OMPI_SIZEOF_FORTRAN_INTEGER2
    cptr = true_str;
#else
    cptr = false_str;
#endif
    ret = opal_info_set(&newinfo->super, "mpi_integer2_supported", cptr);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }
    
#if OMPI_SIZEOF_FORTRAN_INTEGER4
    cptr = true_str;
#else
    cptr = false_str;
#endif
    ret = opal_info_set(&newinfo->super, "mpi_integer4_supported", cptr);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }
    
#if OMPI_SIZEOF_FORTRAN_INTEGER8
    cptr = true_str;
#else
    cptr = false_str;
#endif
    ret = opal_info_set(&newinfo->super, "mpi_integer8_supported", cptr);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }
    
#if OMPI_SIZEOF_FORTRAN_INTEGER16
    cptr = true_str;
#else
    cptr = false_str;
#endif
    ret = opal_info_set(&newinfo->super, "mpi_integer16_supported", cptr);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }

#if OMPI_SIZEOF_FORTRAN_REAL4
    cptr = true_str;
#else
    cptr = false_str;
#endif
    ret = opal_info_set(&newinfo->super, "mpi_real2_supported", cptr);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }
    
#if OMPI_SIZEOF_FORTRAN_REAL4
    cptr = true_str;
#else
    cptr = false_str;
#endif
    ret = opal_info_set(&newinfo->super, "mpi_real4_supported", cptr);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }
    
#if OMPI_SIZEOF_FORTRAN_REAL8
    cptr = true_str;
#else
    cptr = false_str;
#endif
    ret = opal_info_set(&newinfo->super, "mpi_real8_supported", cptr);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }
    
#if OMPI_SIZEOF_FORTRAN_REAL16
    cptr = true_str;
#else
    cptr = false_str;
#endif
    ret = opal_info_set(&newinfo->super, "mpi_real16_supported", cptr);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }

#if OMPI_SIZEOF_FORTRAN_COMPLEX4
    cptr = true_str;
#else
    cptr = false_str;
#endif
    ret = opal_info_set(&newinfo->super, "mpi_complex4_supported", cptr);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }
    
#if OMPI_SIZEOF_FORTRAN_COMPLEX8
    cptr = true_str;
#else
    cptr = false_str;
#endif
    ret = opal_info_set(&newinfo->super, "mpi_complex8_supported", cptr);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }
    
#if OMPI_SIZEOF_FORTRAN_COMPLEX16
    cptr = true_str;
#else
    cptr = false_str;
#endif
    ret = opal_info_set(&newinfo->super, "mpi_complex16_supported", cptr);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }

#if OMPI_SIZEOF_FORTRAN_COMPLEX32
    cptr = true_str;
#else
    cptr = false_str;
#endif
    ret = opal_info_set(&newinfo->super, "mpi_complex32_supported", cptr);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }

#if OMPI_SIZEOF_FORTRAN_DOUBLE_COMPLEX
    cptr = true_str;
#else
    cptr = false_str;
#endif
    ret = opal_info_set(&newinfo->super, "mpi_double_complex_supported", cptr);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }

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
    int ret;

    /* 
     * If OMPI was built with fortran enabled, just tell the app
     * no to any of this setting fortran info stuff.
     */
#if OMPI_BUILD_FORTRAN_BINDINGS
    ret = MPI_ERR_ABI;
#else

    /*
     * dup user supplied fortran info.  For now just to
     * be able to return whatever the user provided to
     * subsequent calls to MPI_Abi_get_fortran_info.
     * Perhaps someday this info can be used to handle
     * Fortran MPI datatypes even when OMPI is not configured
     * with Fortran support,but that will be a heavily lift.
     */
    ret = ompi_info_dup(info, &abi_fortran_info_from_user);
    if (MPI_SUCCESS == ret) {
        ompi_mpi_instance_append_finalize (ompi_abi_fortran_finalize);
    }
#endif
    return ret;

}

int ompi_abi_get_fortran_booleans(int logical_size, void *logical_true, void *logical_false, int *is_set)
{
#if  OMPI_HAVE_FORTRAN_LOGICAL
    bool unavailable = false;
    bool use_int8_t = false, use_int16_t = false, use_int32_t = false, use_int64_t = false;
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
    default:
        unavailable = true;
    }

    if (true == unavailable) {
        *is_set = 0;
    } else {
        *is_set = 1;
        if (true == use_int8_t) {
            int8_t *true_ptr = (int8_t *)logical_true;
            int8_t *false_ptr = (int8_t *)logical_false;
            *true_ptr = (int8_t)OMPI_FORTRAN_VALUE_TRUE;
            *false_ptr = (int8_t)OMPI_FORTRAN_VALUE_FALSE;
        } else if (true == use_int16_t) {
            int16_t *true_ptr = (int16_t *)logical_true;
            int16_t *false_ptr = (int16_t *)logical_false;
            *true_ptr = (int16_t)OMPI_FORTRAN_VALUE_TRUE;
            *false_ptr = (int16_t)OMPI_FORTRAN_VALUE_FALSE;
        } else if (true == use_int32_t) {
            int32_t *true_ptr = (int32_t *)logical_true;
            int32_t *false_ptr = (int32_t *)logical_false;
            *true_ptr = (int32_t)OMPI_FORTRAN_VALUE_TRUE;
            *false_ptr = (int32_t)OMPI_FORTRAN_VALUE_FALSE;
        } else if (true == use_int64_t) {
            int64_t *true_ptr = (int64_t *)logical_true;
            int64_t *false_ptr = (int64_t *)logical_false;
            *true_ptr = (int64_t)OMPI_FORTRAN_VALUE_TRUE;
            *false_ptr = (int64_t)OMPI_FORTRAN_VALUE_FALSE;
        }
    }
#else
    *is_set = 0;
#endif  /* OMPI_HAVE_FORTRAN_LOGICAL */
    return MPI_SUCCESS;
}
