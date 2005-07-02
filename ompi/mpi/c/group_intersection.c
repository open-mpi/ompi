/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "group/group.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Group_intersection = PMPI_Group_intersection
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Group_intersection";


int MPI_Group_intersection(MPI_Group group1, MPI_Group group2,
        MPI_Group *new_group) 
{
    /* local variables */
    int my_group_rank;
    int group_size,proc1,proc2,cnt;
    ompi_group_t *group1_pointer, *group2_pointer, *new_group_pointer;
    ompi_proc_t *proc1_pointer, *proc2_pointer, *my_proc_pointer = NULL;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        /* verify that groups are valid */
        if ( (MPI_GROUP_NULL == group1) || (MPI_GROUP_NULL == group2) ||
             ( NULL == group1) || (NULL == group2) ) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                                      FUNC_NAME);
        }
    }

    group1_pointer=(ompi_group_t *)group1;
    group2_pointer=(ompi_group_t *)group2;

    /*
     * form intersection
     */

    /* figure out how large the intersection is */
    group_size = 0;
    /* loop over group1 members */
    for (proc1 = 0; proc1 < group1_pointer->grp_proc_count; proc1++) {
        proc1_pointer=group1_pointer->grp_proc_pointers[proc1];
        /* check to see if this proc is in group2 */
    
        for (proc2 = 0; proc2 < group2_pointer->grp_proc_count; proc2++) {
            proc2_pointer=group2_pointer->grp_proc_pointers[proc2];
            if( proc1_pointer == proc2_pointer ) {
                group_size++;
                break;
            }
        }  /* end proc2 loop */
    }  /* end proc1 loop */

    if ( 0 == group_size ) {
	*new_group = MPI_GROUP_EMPTY;
	OBJ_RETAIN(MPI_GROUP_EMPTY);
	return MPI_SUCCESS;
    }


    /* fill in new group */
    new_group_pointer=ompi_group_allocate(group_size);
    if( NULL == new_group_pointer ) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                                      FUNC_NAME);
    }

    cnt = 0;

    /* loop over group1 members */
    for (proc1 = 0; proc1 < group1_pointer->grp_proc_count; proc1++) {
        proc1_pointer=group1_pointer->grp_proc_pointers[proc1];
        /* check to see if this proc is in group2 */
        for (proc2 = 0; proc2 < group2_pointer->grp_proc_count; proc2++) {
            proc2_pointer=group2_pointer->grp_proc_pointers[proc2];
            if( proc1_pointer == proc2_pointer ) {
                new_group_pointer->grp_proc_pointers[cnt]=proc1_pointer;
                cnt++;
                break;
            }
        }  /* end proc2 loop */
    }  /* end proc1 loop */

    /* increment proc reference counters */
    ompi_group_increment_proc_count(new_group_pointer);

    /* find my rank */
    my_group_rank=group1_pointer->grp_my_rank;
    if ( MPI_UNDEFINED != my_group_rank ) {
	my_proc_pointer=group1_pointer->grp_proc_pointers[my_group_rank];
    }
    else {
	my_group_rank=group2_pointer->grp_my_rank;
	if ( MPI_UNDEFINED != my_group_rank  ) {
	    my_proc_pointer=group2_pointer->grp_proc_pointers[my_group_rank];
	}
    }

    if ( MPI_UNDEFINED == my_group_rank ) {
	new_group_pointer->grp_my_rank = MPI_UNDEFINED;
    }
    else {
	ompi_set_group_rank(new_group_pointer,my_proc_pointer);
    }

    *new_group = (MPI_Group)new_group_pointer;

    return MPI_SUCCESS;
}
