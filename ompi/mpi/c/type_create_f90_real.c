/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2009 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights
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
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"

#include <float.h>

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_create_f90_real = PMPI_Type_create_f90_real
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_create_f90_real";


int MPI_Type_create_f90_real(int p, int r, MPI_Datatype *newtype)
{
    uint64_t key;

    OPAL_CR_NOOP_PROGRESS();

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        /* Note: These functions accept negative integers for the p and r
         * arguments.  This is because for the SELECTED_REAL_KIND,
         * negative numbers are equivalent to zero values.  See section
         * 13.14.95 of the Fortran 95 standard. */

        if ((MPI_UNDEFINED == p && MPI_UNDEFINED == r)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
        }
    }

    /* if the user does not care about p or r set them to 0 so the
     * test associate with them will always succeed.
     */
    if( MPI_UNDEFINED == p ) p = 0;
    if( MPI_UNDEFINED == r ) r = 0;

    /**
     * With respect to the MPI standard, MPI-2.0 Sect. 10.2.5, MPI_TYPE_CREATE_F90_xxxx,
     * page 295, line 47 we handle this nicely by caching the values in a hash table.
     * However, as the value of might not always make sense, a little bit of optimization
     * might be a good idea. Therefore, first we try to see if we can handle the value
     * with some kind of default value, and if it's the case then we look into the
     * cache.
     */

    if( (LDBL_DIG < p) || (LDBL_MAX_10_EXP < r) )    *newtype = &ompi_mpi_datatype_null.dt;
    else if( (DBL_DIG < p) || (DBL_MAX_10_EXP < r) ) *newtype = &ompi_mpi_long_double.dt;
    else if( (FLT_DIG < p) || (FLT_MAX_10_EXP < r) ) *newtype = &ompi_mpi_double.dt;
    else                                             *newtype = &ompi_mpi_float.dt;

    if( *newtype != &ompi_mpi_datatype_null.dt ) {
        ompi_datatype_t* datatype;
        const int* a_i[2] = {&r, &p};
        int rc;

        key = (((uint64_t)p) << 32) | ((uint64_t)r);
        if( OPAL_SUCCESS == opal_hash_table_get_value_uint64( &ompi_mpi_f90_real_hashtable,
                                                              key, (void**)newtype ) ) {
            return MPI_SUCCESS;
        }
        /* Create the duplicate type corresponding to selected type, then
         * set the argument to be a COMBINER with the correct value of r
         * and add it to the hash table. */
        if (OMPI_SUCCESS != ompi_datatype_duplicate( *newtype, &datatype)) {
            OMPI_ERRHANDLER_RETURN (MPI_ERR_INTERN, MPI_COMM_WORLD,
                                    MPI_ERR_INTERN, FUNC_NAME );
        }
        /* Make sure the user is not allowed to free this datatype as specified
         * in the MPI standard.
         */
        datatype->super.flags |= OMPI_DATATYPE_FLAG_PREDEFINED;
        /* Mark the datatype as a special F90 convenience type */
        snprintf(datatype->name, MPI_MAX_OBJECT_NAME, "COMBINER %s",
                 (*newtype)->name);

        ompi_datatype_set_args( datatype, 2, a_i, 0, NULL, 0, NULL, MPI_COMBINER_F90_REAL );

        rc = opal_hash_table_set_value_uint64( &ompi_mpi_f90_real_hashtable, key, datatype );
        if (OMPI_SUCCESS != rc) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, rc, FUNC_NAME);
        }
        *newtype = datatype;
        return MPI_SUCCESS;
    }

    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
}
