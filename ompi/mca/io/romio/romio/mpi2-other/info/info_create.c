/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_Info_create = PMPI_Info_create
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_Info_create MPI_Info_create
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_Info_create as PMPI_Info_create
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_Info_create - Creates a new info object

Output Parameters:
. info - info object (handle)

.N fortran
@*/
int MPI_Info_create(MPI_Info *info)
{
    int error_code;

    MPIR_MPIOInit(&error_code);
    if (error_code != MPI_SUCCESS) goto fn_exit;

    *info = (MPI_Info) ADIOI_Malloc(sizeof(struct MPIR_Info));
    (*info)->cookie = MPIR_INFO_COOKIE;
    (*info)->key = 0;
    (*info)->value = 0;
    (*info)->next = 0;
    /* this is the first structure in this linked list. it is 
       always kept empty. new (key,value) pairs are added after it. */

fn_exit:
    return MPI_SUCCESS;
}
