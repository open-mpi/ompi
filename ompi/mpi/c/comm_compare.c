/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_compare = PMPI_Comm_compare
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Comm_compare";


int MPI_Comm_compare(MPI_Comm comm1, MPI_Comm comm2, int *result) {

    /* local variables */
    ompi_communicator_t *comp1, *comp2;
    ompi_group_t *group1, *group2;
    int size1, size2, rsize1, rsize2;
    int lresult, rresult=MPI_CONGRUENT;
    int sameranks=1;
    int sameorder=1;
    int i, j;
    int found = 0;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if (ompi_comm_invalid(comm1) || ompi_comm_invalid(comm2)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);
        }

        if ( NULL == result ) {
            return OMPI_ERRHANDLER_INVOKE(comm1, MPI_ERR_ARG, 
                                          FUNC_NAME);
        }
    }
    
    comp1 = (ompi_communicator_t *) comm1;
    comp2 = (ompi_communicator_t *) comm2;

    if ( comp1->c_contextid == comp2->c_contextid ) {
        *result = MPI_IDENT;
        return MPI_SUCCESS;
    }

    if ( MPI_COMM_NULL == comm1 || MPI_COMM_NULL == comm2 ) {
	*result = MPI_UNEQUAL;
	return MPI_SUCCESS;
    }

    /* compare sizes of local and remote groups */
    size1 = ompi_comm_size (comp1);
    size2 = ompi_comm_size (comp2);
    rsize1 = ompi_comm_remote_size (comp1);
    rsize2 = ompi_comm_remote_size (comp2);

    if ( size1 != size2 || rsize1 != rsize2 ) {
        *result = MPI_UNEQUAL;
        return MPI_SUCCESS;
    }
        
    /* Compare local groups */
    /* we need to check whether the communicators contain
       the same processes and in the same order */
   group1 = (ompi_group_t *)comp1->c_local_group;
   group2 = (ompi_group_t *)comp2->c_local_group;
    for ( i = 0; i < size1; i++ ) {
        if ( group1->grp_proc_pointers[i] != group2->grp_proc_pointers[i]) {
            sameorder = 0;
            break;
        }
    }
    
    for ( i = 0; i < size1; i++ ) {
        found = 0;
        for ( j = 0; j < size2; j++ ) {
            if ( group1->grp_proc_pointers[i] == group2->grp_proc_pointers[j]) {
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
        lresult = MPI_CONGRUENT;
    else if ( sameranks && !sameorder )
        lresult = MPI_SIMILAR;
    else
        lresult = MPI_UNEQUAL;


    if ( rsize1 > 0 ) {        
        /* Compare remote groups for inter-communicators */
        /* we need to check whether the communicators contain
           the same processes and in the same order */
        sameranks = sameorder = 1;

        group1 = (ompi_group_t *)comp1->c_remote_group;
        group2 = (ompi_group_t *)comp2->c_remote_group;
        for ( i = 0; i < rsize1; i++ ) {
            if ( group1->grp_proc_pointers[i] != group2->grp_proc_pointers[i]) {
                sameorder = 0;
                break;
            }
        }

        for ( i = 0; i < rsize1; i++ ) {
            found = 0;
            for ( j = 0; j < rsize2; j++ ) {
                if ( group1->grp_proc_pointers[i] == group2->grp_proc_pointers[j]) {
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
            rresult = MPI_CONGRUENT;
        else if ( sameranks && !sameorder )
            rresult = MPI_SIMILAR;
        else
            rresult = MPI_UNEQUAL;
    }

    /* determine final results */
    if ( MPI_CONGRUENT == rresult ) {
        *result = lresult;
    }
    else if ( MPI_SIMILAR == rresult ) {
        if ( MPI_SIMILAR == lresult || MPI_CONGRUENT == lresult ) {
            *result = MPI_SIMILAR;
	}
	else 
	    *result = MPI_UNEQUAL;
    }
    else if ( MPI_UNEQUAL == rresult ) 
        *result = MPI_UNEQUAL;

    return MPI_SUCCESS;
}
