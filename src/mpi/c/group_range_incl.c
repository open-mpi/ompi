/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "group/group.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Group_range_incl = PMPI_Group_range_incl
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Group_range_incl";


OMPI_EXPORT
int MPI_Group_range_incl(MPI_Group group, int n_triplets, int ranges[][3],
                         MPI_Group *new_group) 
{
    /* local variables */
    int new_group_size, proc, first_rank, last_rank;
    int stride, triplet, index, *elements_int_list, my_group_rank;
    ompi_group_t *group_pointer, *new_group_pointer;
    ompi_proc_t *my_proc_pointer;

    group_pointer=(ompi_group_t *)group;

    /* can't act on NULL group */
    if( MPI_PARAM_CHECK ) {
      OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( (MPI_GROUP_NULL == group) || (NULL == group) ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                                          FUNC_NAME);
        }
    }

    /*
     * pull out elements
     */
    elements_int_list =
        (int *) malloc(sizeof(int) * group_pointer->grp_proc_count);
    if (NULL == elements_int_list) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_OTHER,
                                      FUNC_NAME);
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
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_RANK,
                                          FUNC_NAME);
        }
        if((0 > last_rank) || (last_rank > group_pointer->grp_proc_count)) {
            free(elements_int_list);
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_RANK,
                    "MPI_Group_range_incl - IV");
        }
        if (stride == 0) {
            free(elements_int_list);
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_RANK,
                    "MPI_Group_range_incl - V");
        }
        if (first_rank < last_rank) {

            if (stride < 0) {
                free(elements_int_list);
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_RANK,
                        "MPI_Group_range_incl - VI");
            }
            
            /* positive stride */
            index = first_rank;
            while (index <= last_rank) {
                /* make sure rank has not already been selected */
                if (elements_int_list[index] != -1) {
                    free(elements_int_list);
                    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_RANK,
                            "MPI_Group_range_incl - VII");
                }
                elements_int_list[index] = new_group_size;
                index += stride;
                new_group_size++;
            }                   /* end while loop */

        } else if (first_rank > last_rank) {

            if (stride > 0){
                free(elements_int_list);
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_RANK,
                        "MPI_Group_range_incl - VIII");
            }
                
            /* negative stride */
            index = first_rank;
            while (index >= last_rank) {
                /* make sure rank has not already been selected */
                if (elements_int_list[index] != -1) {
                    free(elements_int_list);
                    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_RANK,
                            "MPI_Group_range_incl - IX");
                }
                elements_int_list[index] = new_group_size;
                index += stride;
                new_group_size++;
            }                   /* end while loop */

        } else {                /* first_rank == last_rank */

            index = first_rank;
            if (elements_int_list[index] != -1) {
                free(elements_int_list);
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_RANK,
                        "MPI_Group_range_incl - X");
            }
            elements_int_list[index] = new_group_size;
            new_group_size++;
        }
    }

    if ( 0 == new_group_size ) {
        *new_group = MPI_GROUP_EMPTY;
        OBJ_RETAIN(MPI_GROUP_EMPTY);
        return MPI_SUCCESS;
    }

    /* allocate a new ompi_group_t structure */
    new_group_pointer=ompi_group_allocate(new_group_size);
    if( NULL == new_group_pointer ) {
        free(elements_int_list);
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                "MPI_Group_range_incl - X");
    }

    /* fill in group list */
    for (proc = 0; proc < group_pointer->grp_proc_count; proc++) {
        /* if value >= 0, include in the list */
        if (0 <= elements_int_list[proc] ) {
            new_group_pointer->grp_proc_pointers[elements_int_list[proc]] =
                group_pointer->grp_proc_pointers[proc];
        }
    } /* end of proc loop */

    /* increment proc reference counters */
    ompi_group_increment_proc_count(new_group_pointer);

    free(elements_int_list);

    /* find my rank */
    my_group_rank=group_pointer->grp_my_rank;
    my_proc_pointer=group_pointer->grp_proc_pointers[my_group_rank];
    ompi_set_group_rank(new_group_pointer,my_proc_pointer);
   
    *new_group = (MPI_Group)new_group_pointer;

    return MPI_SUCCESS;
}
