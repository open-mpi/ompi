/*
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
#pragma weak MPI_Group_excl = PMPI_Group_excl
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Group_excl";


int MPI_Group_excl(MPI_Group group, int n, int *ranks,
                   MPI_Group *new_group) {

    /* local variables */
    int return_value,proc,i_excl,found,excl_proc,cnt,my_group_rank;
    ompi_group_t *group_pointer, *new_group_pointer;
    ompi_proc_t *my_proc_pointer;

    return_value = MPI_SUCCESS;
    group_pointer = (ompi_group_t *)group;

    if( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        /* verify that group is valid group */
        if ( (MPI_GROUP_NULL == group)  || (NULL == group) || 
                (NULL == ranks) ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                                          FUNC_NAME);
        }

        /* check that new group is no larger than old group */
        if ( n > group_pointer->grp_proc_count) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                                          FUNC_NAME);
        }

    }  /* end if( MPI_CHECK_ARGS) */

    if ( n == group_pointer->grp_proc_count ) {
        *new_group = MPI_GROUP_EMPTY;
        OBJ_RETAIN(MPI_GROUP_EMPTY);
        return MPI_SUCCESS;
    }

    /*
     * pull out elements
     */

    /* get new group struct */
    new_group_pointer=ompi_group_allocate(group_pointer->grp_proc_count-n);
    if( NULL == new_group_pointer ) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                                      FUNC_NAME);
    }

    /* put group elements in the list */
    cnt=0;
    for (proc = 0; proc < group_pointer->grp_proc_count; proc++) {
        found=0;
        /* check to see if this proc is in the exclusion list */
        for( i_excl=0 ; i_excl < n ; ++i_excl ) {
            excl_proc=ranks[i_excl];
            /* check to see if this proc is within range */
            if( ( 0 > excl_proc ) ||
                (excl_proc >= group_pointer->grp_proc_count)){
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_RANK,
                                              FUNC_NAME);
                }
            if(excl_proc == proc ){
                found=1;
                break;
            }

        } /* end i_excl loop */
        if( !found ) {
            new_group_pointer->grp_proc_pointers[cnt] =
                group_pointer->grp_proc_pointers[proc];
            cnt++;
        }

    }   /* end proc loop */

    /* increment proc reference counters */
    ompi_group_increment_proc_count(new_group_pointer);

    /* find my rank */
    my_group_rank=group_pointer->grp_my_rank;
    my_proc_pointer=group_pointer->grp_proc_pointers[my_group_rank];
    ompi_set_group_rank(new_group_pointer,my_proc_pointer);

    *new_group = (MPI_Group)new_group_pointer;

    return MPI_SUCCESS;
}
