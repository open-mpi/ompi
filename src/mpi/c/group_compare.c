/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "group/group.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_compare = PMPI_Group_compare
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Group_compare(MPI_Group group1, MPI_Group group2, int *result) {

    /* local variables */
    int return_value, proc1, proc2, similar, identical, match ;
    lam_group_t *group1_pointer, *group2_pointer;
    lam_proc_t *proc1_pointer, *proc2_pointer;


    /* initialization */
    return_value=MPI_SUCCESS;

    /* check for errors */
    if( MPI_PARAM_CHECK ) {
        if( ( MPI_GROUP_NULL == group1 ) || ( MPI_GROUP_NULL == group2 ) ||
                (NULL == group1) || (NULL==group2) ){
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                        "MPI_Group_compare");
        }
    }

    /* check for same groups */
    if( group1 == group2 ) {
        *result=MPI_IDENT;
        return return_value;
    }

    /* check to see if either is MPI_GROUP_NULL or MPI_GROUP_EMPTY */
    if( ( MPI_GROUP_EMPTY == group1 ) || ( MPI_GROUP_EMPTY == group2 ) ) {
        *result=MPI_UNEQUAL;
        return return_value;
    }

    /* get group pointers */
    group1_pointer = (lam_group_t *)group1;
    group2_pointer = (lam_group_t *)group2;

    /* compare sizes */
    if( group1_pointer->grp_proc_count != group2_pointer->grp_proc_count ) {
        /* if not same size - return */
        *result=MPI_UNEQUAL;
        return return_value;
    }

    /* check for similarity */
    /* loop over group1 processes */
    similar=1;
    identical=1;
    for(proc1=0 ; proc1 < group1_pointer->grp_proc_count ; proc1++ ) {
        proc1_pointer=group1_pointer->grp_proc_pointers[proc1];
        /* loop over group2 processes to find "match" */
        match=-1;
        for(proc2=0 ; proc2 < group2_pointer->grp_proc_count ; proc2++ ) {
            proc2_pointer=group1_pointer->grp_proc_pointers[proc2];
            if( proc1_pointer == proc2_pointer ) {
                if(proc1 != proc2 ) {
                    identical=0;
                }
                match=proc2;
                break;
            }
        } /* end proc2 loop */
        if( match== -1 ) {
            similar=false;
            break;
        }
    } /* end proc1 loop */

    /* set comparison result */
    if( identical ) {
        *result=MPI_UNEQUAL;
    } else if( similar ) {
        *result=MPI_SIMILAR;
    } else {
        *result=MPI_IDENT;
    }

    return return_value;
}
