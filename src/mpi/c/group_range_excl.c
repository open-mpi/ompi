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
#pragma weak MPI_Group_range_excl = PMPI_Group_range_excl
#endif

int MPI_Group_range_excl(MPI_Group group, int n_triplets, int ranges[][3],
                         MPI_Group *new_group) {

    /* local variables */
    int new_group_size, proc, first_rank, last_rank;
	int stride, triplet, index, *elements_int_list, my_group_rank;
    lam_group_t *group_pointer, *new_group_pointer;
    lam_proc_t *my_proc_pointer;

    group_pointer=(lam_group_t *)group;

    /* can't act on NULL group */
    if( MPI_PARAM_CHECK ) {
        if ( (MPI_GROUP_NULL == group) || (NULL == group) ) {
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                        "MPI_Group_range_excl");
        }
    }

    /*
     * pull out elements
     */
    elements_int_list = (int *) 
        malloc(sizeof(int) * group_pointer->grp_proc_count);
    if (NULL == elements_int_list) {
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_OTHER,
                "MPI_Group_range_excl - II");
    }
    for (proc = 0; proc < group_pointer->grp_proc_count; proc++) {
        elements_int_list[proc] = -1;
    }

    /* loop over triplet */
    new_group_size = 0;
    for (triplet = 0; triplet < n_triplets; triplet++) {
        first_rank = ranges[triplet][0];
        last_rank = ranges[triplet][1];
        stride = ranges[triplet][2];

        /* check for error conditions */
        if ((0 > first_rank) || (first_rank > group_pointer->grp_proc_count)) {
            free(elements_int_list);
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_RANK,
                "MPI_Group_range_excl - III");
        }
        if ((0 > last_rank) || (last_rank > group_pointer->grp_proc_count)) {
            free(elements_int_list);
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_RANK,
                "MPI_Group_range_excl - IV");
        }
        if (stride == 0) {
            free(elements_int_list);
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_RANK,
                "MPI_Group_range_excl - V");
        }
        if (first_rank < last_rank) {
            if (stride < 0) {
                free(elements_int_list);
                return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_RANK,
                "MPI_Group_range_excl - VI");
            }

            /* positive stride */
            index = first_rank;
            while (index <= last_rank) {
                /* make sure rank has not already been selected */
                if (elements_int_list[index] != -1) {
                    free(elements_int_list);
                    return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_RANK,
                            "MPI_Group_range_excl - VII");
                }
                elements_int_list[index] = new_group_size;
                index += stride;
                new_group_size++;
            }			/* end while loop */

        } else if (first_rank > last_rank) {

            if (stride > 0) {
                free(elements_int_list);
                return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_RANK,
                        "MPI_Group_range_excl - VIII");
            }
            /* negative stride */
            index = first_rank;
            while (index >= last_rank) {
                /* make sure rank has not already been selected */
                if (elements_int_list[index] != -1) {
                    free(elements_int_list);
                    return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_RANK,
                            "MPI_Group_range_excl - IX");
                }
                elements_int_list[index] = new_group_size;
                index += stride;
                new_group_size++;
            }			// end while loop

        } else {
            /* first_rank == last_rank */
            index = first_rank;
            if (elements_int_list[index] != -1) {
                free(elements_int_list);
                return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_RANK,
                        "MPI_Group_range_excl - X");
            }
            elements_int_list[index] = new_group_size;
            new_group_size++;
        }
    }  /* end triplet loop */

    /* we have counted the procs to exclude from the list */
    new_group_size=group_pointer->grp_proc_count-new_group_size;

    /* allocate a new lam_group_t structure */
    new_group_pointer=lam_group_allocate(new_group_size);
    if( NULL == new_group_pointer ) {
        free(elements_int_list);
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                "MPI_Group_range_excl - XI");
    }

    /* fill in group list */
    index=0;
    for (proc = 0; proc < group_pointer->grp_proc_count; proc++) {
        /* if value == -1, include in the list */
        if (0 < elements_int_list[proc] ) {
            new_group_pointer->grp_proc_pointers[index] =
                            group_pointer->grp_proc_pointers[proc];
            index++;
        }
    } /* end of proc loop */

    /* increment proc reference counters */
    lam_group_increment_proc_count(new_group_pointer);

    free(elements_int_list);

    /* find my rank */
    my_group_rank=group_pointer->grp_my_rank;
    my_proc_pointer=group_pointer->grp_proc_pointers[my_group_rank];
    lam_set_group_rank(new_group_pointer,my_proc_pointer);
   
    *new_group = (MPI_Group)new_group_pointer;

    return MPI_SUCCESS;
}
