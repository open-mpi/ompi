/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "runtime/runtime.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_compare = PMPI_Comm_compare
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Comm_compare(MPI_Comm comm1, MPI_Comm comm2, int *result) {

    /* local variables */
    ompi_communicator_t *comp1, *comp2;
    ompi_group_t *grp1, *grp2;
    int size1, size2, rsize1, rsize2;
    int lresult, rresult;
    int sameranks = 1;
    int sameorder = 1;
    int i, j;
    int found = 0;
    
    if ( MPI_PARAM_CHECK ) {
        if ( ompi_mpi_finalized )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                         "MPI_Comm_compare");

        if ( MPI_COMM_NULL == comm1 || MPI_COMM_NULL == comm2 || 
             ompi_comm_invalid ( comm1 ) || ompi_comm_invalid (comm2) )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                         "MPI_Comm_compare");  

        if ( NULL == result )
            return OMPI_ERRHANDLER_INVOKE(comm1, MPI_ERR_ARG, 
                                         "MPI_Comm_compare");
    }
    
    comp1 = (ompi_communicator_t *) comm1;
    comp2 = (ompi_communicator_t *) comm2;

    /* compare sizes of local and remote groups */
    size1 = ompi_comm_size (comp1);
    size2 = ompi_comm_size (comp1);
    rsize1 = ompi_comm_remote_size (comp1);
    rsize2 = ompi_comm_remote_size (comp1);

    if ( size1 != size2 || rsize1 != rsize2 ) {
        *result = MPI_UNEQUAL;
        return MPI_SUCCESS;
    }
        
    /* Compare local groups */
    /* we need to check whether the communicators contain
       the same processes and in the same order */
    grp1 = (ompi_group_t *)comp1->c_local_group;
    grp2 = (ompi_group_t *)comp2->c_local_group;
    for ( i = 0; i < size1; i++ ) {
        if ( grp1->grp_proc_pointers[i] != grp2->grp_proc_pointers[i]) {
            sameorder = 0;
            break;
        }
    }
    
    for ( i = 0; i < size1; i++ ) {
        found = 0;
        for ( j = 0; j < size2; j++ ) {
            if ( grp1->grp_proc_pointers[i] == grp2->grp_proc_pointers[j]) {
                found = 1;
                break;
            }
        }
        if ( !found  ) {
            sameranks = 0;
            break;
        }
    }
    
    if ( sameranks && sameorder )
        lresult = MPI_SIMILAR;
    else if ( sameranks && !sameorder )
        lresult = MPI_CONGRUENT;
    else
        lresult = MPI_UNEQUAL;


    if ( rsize1 > 0 ) {        
        /* Compare remote groups for inter-communicators */
        /* we need to check whether the communicators contain
           the same processes and in the same order */
        sameranks = sameorder = 1;
        rresult = MPI_SIMILAR;

        grp1 = (ompi_group_t *)comp1->c_remote_group;
        grp2 = (ompi_group_t *)comp2->c_remote_group;
        for ( i = 0; i < rsize1; i++ ) {
            if ( grp1->grp_proc_pointers[i] != grp2->grp_proc_pointers[i]) {
                sameorder = 0;
                break;
            }
        }

        for ( i = 0; i < size1; i++ ) {
            found = 0;
            for ( j = 0; j < size2; j++ ) {
                if ( grp1->grp_proc_pointers[i] == grp2->grp_proc_pointers[j]) {
                    found = 1;
                    break;
                }
            }
            if ( !found  ) {
                sameranks = 0;
                break;
            }
        }
        
        if ( sameranks && sameorder )
            rresult = MPI_SIMILAR;
        else if ( sameranks && !sameorder )
            rresult = MPI_CONGRUENT;
        else
            rresult = MPI_UNEQUAL;
    }

    /* determine final results */
    if ( MPI_SIMILAR == rresult ) {
        *result = lresult;
    }
    else if ( MPI_CONGRUENT == rresult ) {
        if ( MPI_SIMILAR == lresult )
            *result = MPI_CONGRUENT;
        else
            *result = MPI_SIMILAR;
    }
    else if ( MPI_UNEQUAL == rresult ) 
        *result = MPI_UNEQUAL;

    return MPI_SUCCESS;
}
