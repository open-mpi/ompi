/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "lfc/lam_list.h"
#include "info/info.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_dup = PMPI_Info_dup
#endif

/**
 *   MPI_Info_dup - Duplicate an 'MPI_Info' object
 *
 *   @param info source info object (handle)
 *   @param newinfo pointer to the new info object (handle)
 *
 *   @retval MPI_SUCCESS
 *   @retval MPI_ERR_ARG
 *   @retval MPI_ERR_SYSRESOURCE
 *
 *   Not only will the (key, value) pairs be duplicated, the order of keys
 *   will be the same in 'newinfo' as it is in 'info'.
 *   When an info object is no longer being used, it should be freed with
 *   'MPI_Info_free'.
 */
int MPI_Info_dup(MPI_Info info, MPI_Info *newinfo) {
    int err;
    /**
     * Here we need to do 2 things
     * 1. Create a newinfo object using MPI_Info_create
     * 2. Fetch all the values from info and copy them to 
     *    newinfo using MPI_Info_set
     * The new implementation facilitates traversal in many ways.
     * I have chosen to get the number of elements on the list 
     * and copy them to newinfo one by one
     */

    if (NULL == info){
        printf ("Invalid MPI_Info handle passed\n");
        return MPI_ERR_ARG;
    }

    err = MPI_Info_create(newinfo);
    if (MPI_SUCCESS != err) {
        printf ("Creation of newinfo falied\n");
        return err;
    }
    /*
     * Now to actually duplicate all the values
     */
    err = lam_info_dup (info, newinfo);

    if (err == MPI_ERR_SYSRESOURCE) {
        printf ("Resources are not sufficient to finish dup'ing\n");
        return err;
    }  
        
    return MPI_SUCCESS;
}
