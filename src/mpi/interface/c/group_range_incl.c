/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "mpi/group/group.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_range_incl = PMPI_Group_range_incl
#endif

int MPI_Group_range_incl(MPI_Group group, int n_triplets, int ranges[][3],
                         MPI_Group *new_group) {
    /* local variables */
    int return_value, new_group_size, proc, first_rank, last_rank;
    int stride, triplet, index, *elements_int_list, my_group_rank;
    lam_group_t *group_pointer, *new_group_pointer;
    lam_proc_t *my_proc_pointer;

    return_value = MPI_SUCCESS;
    group_pointer=(lam_group_t *)group;

    /* can't act on NULL group */
    if( MPI_PARAM_CHECK ) {
        if ( MPI_GROUP_NULL == group ) {
            return MPI_ERR_GROUP;
        }
    }

    /*
     * pull out elements
     */
    elements_int_list =
        (int *) malloc(sizeof(int) * group_pointer->grp_proc_count);
    if (NULL == elements_int_list) {
        return MPI_ERR_OTHER;
    }
    for (proc = 0; proc < group_pointer->grp_proc_count; proc++) {
        elements_int_list[proc] = -1;
    }

    /* loop over triplet */
    new_group_size = 0;
    for (triplet = 0; triplet < n_triplets; triplet++) 
    {
        first_rank = ranges[triplet][0];
        last_rank = ranges[triplet][1];
        stride = ranges[triplet][2];
        if(( 0 > first_rank ) || (first_rank > group_pointer->grp_proc_count)) {
            free(elements_int_list);
            return MPI_ERR_RANK;
        }
        if((0 > last_rank) || (last_rank > group_pointer->grp_proc_count)) {
            free(elements_int_list);
            return MPI_ERR_RANK;
        }
        if (stride == 0) {
            free(elements_int_list);
            return MPI_ERR_RANK;
        }
        if (first_rank < last_rank) {

            if (stride < 0) {
                free(elements_int_list);
                return MPI_ERR_RANK;
            }
            
            /* positive stride */
            index = first_rank;
            while (index <= last_rank) {
                /* make sure rank has not already been selected */
                if (elements_int_list[index] != -1) {
                    free(elements_int_list);
                    return MPI_ERR_RANK;
                }
                elements_int_list[index] = new_group_size;
                index += stride;
                new_group_size++;
            }                   /* end while loop */

        } else if (first_rank > last_rank) {

            if (stride > 0){
                free(elements_int_list);
                return MPI_ERR_RANK;
            }
                
            /* negative stride */
            index = first_rank;
            while (index >= last_rank) {
                /* make sure rank has not already been selected */
                if (elements_int_list[index] != -1) {
                    free(elements_int_list);
                    return MPI_ERR_RANK;
                }
                elements_int_list[index] = new_group_size;
                index += stride;
                new_group_size++;
            }                   // end while loop

        } else {                // first_rank == last_rank

            index = first_rank;
            if (elements_int_list[index] != -1) {
                free(elements_int_list);
                return MPI_ERR_RANK;
            }
            elements_int_list[index] = new_group_size;
            new_group_size++;
        }
    }

    /* check for empty group */
    if( 0 == new_group_size ) {
        *new_group = MPI_GROUP_EMPTY;
        free(elements_int_list);
        return MPI_SUCCESS;
    }

    /* allocate a new lam_group_t structure */
    new_group_pointer=group_allocate(new_group_size);
    if( NULL == new_group_pointer ) {
        free(elements_int_list);
        return MPI_ERR_GROUP;
    }

    /* fill in group list */
    index=0;
    for (proc = 0; proc < group_pointer->grp_proc_count; proc++) {
        /* if value >= 0, include in the list */
        if (0 <= elements_int_list[proc] ) {
            new_group_pointer->grp_proc_pointers[index] =
                group_pointer->grp_proc_pointers[proc];
            index++;
        }
    } /* end of proc loop */

    free(elements_int_list);

    /* find my rank */
    my_group_rank=group_pointer->grp_my_rank;
    my_proc_pointer=group_pointer->grp_proc_pointers[my_group_rank];
    lam_set_group_rank(new_group_pointer,my_proc_pointer);
   
    *new_group = (MPI_Group)new_group_pointer;

    return MPI_SUCCESS;
}
