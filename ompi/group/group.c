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
 * Copyright (c) 2006      University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/group/group.h"
#include "ompi/constants.h"
#include "mpi.h"
#include "ompi/proc/proc.h"

int ompi_group_free ( ompi_group_t **group )
{
    ompi_group_t *l_group;

    l_group = (ompi_group_t *) *group;
    ompi_group_decrement_proc_count (l_group);
    OBJ_RELEASE(l_group);

    *group = MPI_GROUP_NULL;
    return OMPI_SUCCESS;
}

int ompi_group_translate_ranks ( ompi_group_t *group1, 
                                 int n_ranks, int *ranks1,
                                 ompi_group_t *group2, 
                                 int *ranks2) 
{
    int rank, proc, proc2;
    struct ompi_proc_t *proc1_pointer, *proc2_pointer;
    

    if ( MPI_GROUP_EMPTY == group1 || MPI_GROUP_EMPTY == group2 ) {
	for (proc = 0; proc < n_ranks ; proc++) {
	    ranks2[proc] = MPI_UNDEFINED;
	}
	return MPI_SUCCESS;
    }
    /* loop over all ranks */
    for (proc = 0; proc < n_ranks; proc++) {
        rank=ranks1[proc];
	if ( MPI_PROC_NULL == rank) {
	    ranks2[proc] = MPI_PROC_NULL;
	}
	else {
	    proc1_pointer=group1->grp_proc_pointers[rank];
	    /* initialize to no "match" */
	    ranks2[proc] = MPI_UNDEFINED;
	    for (proc2 = 0; proc2 < group2->grp_proc_count; proc2++) 
	    {
		proc2_pointer=group2->grp_proc_pointers[proc2];
		if ( proc1_pointer == proc2_pointer) {
		    ranks2[proc] = proc2;
		    break;
		}
	    }  /* end proc2 loop */
	}
    } /* end proc loop */

    return MPI_SUCCESS;
}

int ompi_group_union (ompi_group_t* group1, ompi_group_t* group2, ompi_group_t **new_group) 
{
    /* local variables */
    int new_group_size, proc1, proc2, found_in_group;
    int my_group_rank, cnt;
    ompi_group_t *group1_pointer, *group2_pointer, *new_group_pointer;
    ompi_proc_t *proc1_pointer, *proc2_pointer, *my_proc_pointer = NULL;

    group1_pointer = (ompi_group_t *) group1;
    group2_pointer = (ompi_group_t *) group2;

    /*
     * form union
     */

    /* get new group size */
    new_group_size = group1_pointer->grp_proc_count;

    /* check group2 elements to see if they need to be included in the list */
    for (proc2 = 0; proc2 < group2_pointer->grp_proc_count; proc2++) {
        proc2_pointer = group2_pointer->grp_proc_pointers[proc2];

        /* check to see if this proc2 is alread in the group */
        found_in_group = 0;
        for (proc1 = 0; proc1 < group1_pointer->grp_proc_count; proc1++) {
            proc1_pointer = group1_pointer->grp_proc_pointers[proc1];
            if (proc1_pointer == proc2_pointer) {
                /* proc2 is in group1 - don't double count */
                found_in_group = 1;
                break;
            }
        }                       /* end proc1 loop */

        if (found_in_group)
            continue;

        new_group_size++;
    }                           /* end proc loop */

    if ( 0 == new_group_size ) {
	*new_group = MPI_GROUP_EMPTY;
	OBJ_RETAIN(MPI_GROUP_EMPTY);
	return MPI_SUCCESS;
    }

    /* get new group struct */
    new_group_pointer = ompi_group_allocate(new_group_size);
    if (NULL == new_group_pointer) {
        return MPI_ERR_GROUP;
    }

    /* fill in the new group list */

    /* put group1 elements in the list */
    for (proc1 = 0; proc1 < group1_pointer->grp_proc_count; proc1++) {
        new_group_pointer->grp_proc_pointers[proc1] =
            group1_pointer->grp_proc_pointers[proc1];
    }
    cnt = group1_pointer->grp_proc_count;

    /* check group2 elements to see if they need to be included in the list */
    for (proc2 = 0; proc2 < group2_pointer->grp_proc_count; proc2++) {
        proc2_pointer = group2_pointer->grp_proc_pointers[proc2];

        /* check to see if this proc2 is alread in the group */
        found_in_group = 0;
        for (proc1 = 0; proc1 < group1_pointer->grp_proc_count; proc1++) {
            proc1_pointer = group1_pointer->grp_proc_pointers[proc1];
            if (proc1_pointer == proc2_pointer) {
                /* proc2 is in group1 - don't double count */
                found_in_group = 1;
                break;
            }
        }                       /* end proc1 loop */

        if (found_in_group)
            continue;

        new_group_pointer->grp_proc_pointers[cnt] =
            group2_pointer->grp_proc_pointers[proc2];
        cnt++;
    }                           /* end proc loop */

    /* increment proc reference counters */
    ompi_group_increment_proc_count(new_group_pointer);

    /* find my rank */
    my_group_rank = group1_pointer->grp_my_rank;
    if (MPI_UNDEFINED == my_group_rank) {
        my_group_rank = group2_pointer->grp_my_rank;
	if ( MPI_UNDEFINED != my_group_rank) {
	    my_proc_pointer = group2_pointer->grp_proc_pointers[my_group_rank];
	}
    } else {
        my_proc_pointer = group1_pointer->grp_proc_pointers[my_group_rank];
    }

    if ( MPI_UNDEFINED == my_group_rank ) {
	new_group_pointer->grp_my_rank = MPI_UNDEFINED;
    }
    else {
	ompi_set_group_rank(new_group_pointer, my_proc_pointer);
    }

    *new_group = (MPI_Group) new_group_pointer;


    return OMPI_SUCCESS;
}

int ompi_group_incl(ompi_group_t* group, int n, int *ranks, ompi_group_t **new_group) 
{
    /* local variables */
    int proc,my_group_rank;
    ompi_group_t *group_pointer, *new_group_pointer;
    ompi_proc_t *my_proc_pointer;

    group_pointer = (ompi_group_t *)group;

    /* get new group struct */
    new_group_pointer=ompi_group_allocate(n);
    if( NULL == new_group_pointer ) {
      return MPI_ERR_GROUP;
    }

    /* put group elements in the list */
    for (proc = 0; proc < n; proc++) {
        new_group_pointer->grp_proc_pointers[proc] =
            group_pointer->grp_proc_pointers[ranks[proc]];
	
    }                           /* end proc loop */

    /* increment proc reference counters */
    ompi_group_increment_proc_count(new_group_pointer);

    /* find my rank */
    my_group_rank=group_pointer->grp_my_rank;
    my_proc_pointer=group_pointer->grp_proc_pointers[my_group_rank];
    ompi_set_group_rank(new_group_pointer,my_proc_pointer);

    *new_group = (MPI_Group)new_group_pointer;

    return OMPI_SUCCESS;
}

int ompi_group_excl(ompi_group_t* group, int n, int *ranks,
                   ompi_group_t **new_group) 
{

    /* local variables */
    int return_value,proc,i_excl,found,cnt,my_group_rank;
    ompi_group_t *group_pointer, *new_group_pointer;
    ompi_proc_t *my_proc_pointer;

    return_value = MPI_SUCCESS;
    group_pointer = (ompi_group_t *)group;


    /*
     * pull out elements
     */

    /* get new group struct */
    new_group_pointer=ompi_group_allocate(group_pointer->grp_proc_count-n);
    if( NULL == new_group_pointer ) {
	return MPI_ERR_GROUP;
    }

    /* put group elements in the list */
    cnt=0;
    for (proc = 0; proc < group_pointer->grp_proc_count; proc++) {
        found=0;
        /* check to see if this proc is in the exclusion list */
        for( i_excl=0 ; i_excl < n ; ++i_excl ) {
            if ( ranks[i_excl] == proc ){
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

    return OMPI_SUCCESS;
}


int ompi_group_range_incl(ompi_group_t* group, int n_triplets, int ranges[][3],
                         ompi_group_t **new_group) 
{
    /* local variables */
    int new_group_size, proc, first_rank, last_rank;
    int stride, triplet, index, *elements_int_list, my_group_rank;
    ompi_group_t *group_pointer, *new_group_pointer;
    ompi_proc_t *my_proc_pointer;

    group_pointer=(ompi_group_t *)group;

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

        if (first_rank < last_rank) {
            /* positive stride */
            index = first_rank;
            while (index <= last_rank) {
                elements_int_list[index] = new_group_size;
                index += stride;
                new_group_size++;
            }                   /* end while loop */	    
        } 
	else if (first_rank > last_rank) {
            /* negative stride */
            index = first_rank;
            while (index >= last_rank) {
                elements_int_list[index] = new_group_size;
                index += stride;
                new_group_size++;
            }                   /* end while loop */

        } else {                /* first_rank == last_rank */

            index = first_rank;
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
	return MPI_ERR_GROUP;
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

    return OMPI_SUCCESS;
}


int ompi_group_range_excl(ompi_group_t* group, int n_triplets, int ranges[][3],
                         ompi_group_t **new_group) 
{
    /* local variables */
    int new_group_size, proc, first_rank, last_rank;
    int stride, triplet, index, *elements_int_list, my_group_rank;
    ompi_group_t *group_pointer, *new_group_pointer;
    ompi_proc_t *my_proc_pointer;

    group_pointer=(ompi_group_t *)group;

    /*
     * pull out elements
     */
    elements_int_list = (int *) 
        malloc(sizeof(int) * group_pointer->grp_proc_count);
    if (NULL == elements_int_list) {
      return MPI_ERR_OTHER;
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

        if (first_rank < last_rank) {
            /* positive stride */
            for (index = first_rank; index <= last_rank; index += stride) {
                elements_int_list[index] = new_group_size;
                new_group_size++;
            }

        } else if (first_rank > last_rank) {
            /* negative stride */
            for (index = first_rank; index >= last_rank; index += stride) {
                elements_int_list[index] = new_group_size;
                new_group_size++;
            }

        } else {
            /* first_rank == last_rank */
            index = first_rank;
            elements_int_list[index] = new_group_size;
            new_group_size++;
        }
    }  /* end triplet loop */

    /* we have counted the procs to exclude from the list */
    new_group_size=group_pointer->grp_proc_count-new_group_size;

    if ( 0 == new_group_size ) {
        *new_group = MPI_GROUP_EMPTY;
        OBJ_RETAIN(MPI_GROUP_EMPTY);
        return MPI_SUCCESS;
    }

    /* allocate a new ompi_group_t structure */
    new_group_pointer=ompi_group_allocate(new_group_size);
    if( NULL == new_group_pointer ) {
        free(elements_int_list);
	return MPI_ERR_GROUP;
    }

    /* fill in group list */
    index=0;
    for (proc = 0; proc < group_pointer->grp_proc_count; proc++) {
        /* if value == -1, include in the list */
        if (0 > elements_int_list[proc] ) {
            new_group_pointer->grp_proc_pointers[index] =
                            group_pointer->grp_proc_pointers[proc];
            index++;
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

    return OMPI_SUCCESS;
}


int ompi_group_intersection(ompi_group_t* group1,ompi_group_t* group2,
        ompi_group_t **new_group) 
{
    /* local variables */
    int my_group_rank;
    int group_size,proc1,proc2,cnt;
    ompi_group_t *group1_pointer, *group2_pointer, *new_group_pointer;
    ompi_proc_t *proc1_pointer, *proc2_pointer, *my_proc_pointer = NULL;

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
      return MPI_ERR_GROUP;
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

    return OMPI_SUCCESS;
}

int ompi_group_difference(ompi_group_t* group1, ompi_group_t* group2,
                         ompi_group_t **new_group) {

    /* local varibles */
    int new_group_size, proc1, proc2, found_in_group2, cnt;
    int my_group_rank;
    ompi_group_t *group1_pointer, *group2_pointer, *new_group_pointer;
    ompi_proc_t *proc1_pointer, *proc2_pointer, *my_proc_pointer = NULL;

   
    group1_pointer=(ompi_group_t *)group1;
    group2_pointer=(ompi_group_t *)group2;

    /*
     * form union
     */

    /* get new group size */
    new_group_size=0;

    /* loop over group1 members */
    for( proc1=0; proc1 < group1_pointer->grp_proc_count; proc1++ ) {
        proc1_pointer=group1_pointer->grp_proc_pointers[proc1];
        /* check to see if this proc is in group2 */
        found_in_group2=0;
        for( proc2=0 ; proc2 < group2_pointer->grp_proc_count ; proc2++ ) {
            proc2_pointer=group2_pointer->grp_proc_pointers[proc2];
            if( proc1_pointer == proc2_pointer ) {
                found_in_group2=true;
                break;
            }
        }  /* end proc1 loop */
        if(found_in_group2)
            continue;
        new_group_size++;
    }  /* end proc loop */

    if ( 0 == new_group_size ) {
	*new_group = MPI_GROUP_EMPTY;
	OBJ_RETAIN(MPI_GROUP_EMPTY);
	return MPI_SUCCESS;
    }

    /* allocate a new ompi_group_t structure */
    new_group_pointer=ompi_group_allocate(new_group_size);
    if( NULL == new_group_pointer ) {
      return MPI_ERR_GROUP;
    }

    /* fill in group list */
    cnt=0;
    /* loop over group1 members */
    for( proc1=0; proc1 < group1_pointer->grp_proc_count; proc1++ ) {
        proc1_pointer=group1_pointer->grp_proc_pointers[proc1];
        /* check to see if this proc is in group2 */
        found_in_group2=0;
        for( proc2=0 ; proc2 < group2_pointer->grp_proc_count ; proc2++ ) {
            proc2_pointer=group2_pointer->grp_proc_pointers[proc2];
            if( proc1_pointer == proc2_pointer ) {
                found_in_group2=true;
                break;
            }
        }  /* end proc1 loop */
        if(found_in_group2)
            continue;

        new_group_pointer->grp_proc_pointers[cnt] =
            group1_pointer->grp_proc_pointers[proc1];

        cnt++;
    }  /* end proc loop */

    /* increment proc reference counters */
    ompi_group_increment_proc_count(new_group_pointer);

    /* find my rank */
    my_group_rank=group1_pointer->grp_my_rank;
    if ( MPI_UNDEFINED != my_group_rank ) {
	my_proc_pointer=group1_pointer->grp_proc_pointers[my_group_rank];
    }
    else {
	my_group_rank=group2_pointer->grp_my_rank;
	if ( MPI_UNDEFINED != my_group_rank ) {
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

    return OMPI_SUCCESS;
}

