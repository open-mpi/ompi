/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2006-2007 University of Houston. All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc. All rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/group/group.h"
#include "ompi/constants.h"
#include "ompi/proc/proc.h"
#include "mpi.h"

#include <math.h>

static struct ompi_proc_t *ompi_group_dense_lookup_raw (ompi_group_t *group, const int peer_id)
{
    if (OPAL_UNLIKELY((intptr_t) group->grp_proc_pointers[peer_id] < 0)) {
        ompi_proc_t *proc =
            (ompi_proc_t *) ompi_proc_lookup (ompi_proc_sentinel_to_name ((intptr_t) group->grp_proc_pointers[peer_id]));
        if (NULL != proc) {
            /* replace sentinel value with an actual ompi_proc_t */
            group->grp_proc_pointers[peer_id] = proc;
            /* retain the proc */
            OBJ_RETAIN(group->grp_proc_pointers[peer_id]);
        }
    }

    return group->grp_proc_pointers[peer_id];
}

ompi_proc_t *ompi_group_get_proc_ptr_raw (ompi_group_t *group, int rank)
{
#if OMPI_GROUP_SPARSE
    do {
        if (OMPI_GROUP_IS_DENSE(group)) {
            return ompi_group_dense_lookup_raw (group, peer_id);
        }
        int ranks1 = rank;
        ompi_group_translate_ranks (group, 1, &ranks1, group->grp_parent_group_ptr, &rank);
        group = group->grp_parent_group_ptr;
    } while (1);
#else
    return ompi_group_dense_lookup_raw (group, rank);
#endif
}

int ompi_group_calc_plist ( int n , const int *ranks ) {
    return sizeof(char *) * n ;
}

int ompi_group_incl_plist(ompi_group_t* group, int n, const int *ranks,
                          ompi_group_t **new_group)
{
    /* local variables */
    int my_group_rank;
    ompi_group_t *group_pointer, *new_group_pointer;
    ompi_proc_t *my_proc_pointer;

    group_pointer = (ompi_group_t *)group;

    if ( 0 == n ) {
        *new_group = MPI_GROUP_EMPTY;
        OBJ_RETAIN(MPI_GROUP_EMPTY);
        return OMPI_SUCCESS;
    }

    /* get new group struct */
    new_group_pointer=ompi_group_allocate(n);
    if( NULL == new_group_pointer ) {
        return MPI_ERR_GROUP;
    }

    /* put group elements in the list */
    for (int proc = 0; proc < n; proc++) {
        new_group_pointer->grp_proc_pointers[proc] =
            ompi_group_get_proc_ptr_raw (group_pointer, ranks[proc]);
    }                           /* end proc loop */

    /* increment proc reference counters */
    ompi_group_increment_proc_count(new_group_pointer);

    /* find my rank */
    my_group_rank=group_pointer->grp_my_rank;
    if (MPI_UNDEFINED != my_group_rank) {
        my_proc_pointer=ompi_group_peer_lookup (group_pointer,my_group_rank);
        ompi_set_group_rank(new_group_pointer,my_proc_pointer);
    }
    else {
        new_group_pointer->grp_my_rank = MPI_UNDEFINED;
    }

    *new_group = (MPI_Group)new_group_pointer;

    return OMPI_SUCCESS;
}

/*
 * Group Union has to use the dense format since we don't support
 * two parent groups in the group structure and maintain functions
 */
int ompi_group_union (ompi_group_t* group1, ompi_group_t* group2,
                      ompi_group_t **new_group)
{
    /* local variables */
    int new_group_size, my_group_rank, cnt;
    ompi_group_t *new_group_pointer;
    ompi_proc_t *proc1_pointer, *proc2_pointer, *my_proc_pointer = NULL;

    /*
     * form union
     */

    /* get new group size */
    new_group_size = group1->grp_proc_count;

    /* check group2 elements to see if they need to be included in the list */
    for (int proc2 = 0; proc2 < group2->grp_proc_count; ++proc2) {
        bool found_in_group = false;

        proc2_pointer = ompi_group_get_proc_ptr_raw (group2, proc2);

        /* check to see if this proc2 is alread in the group */
        for (int proc1 = 0; proc1 < group1->grp_proc_count; ++proc1) {
            proc1_pointer = ompi_group_get_proc_ptr_raw (group1, proc1);

            if (proc1_pointer == proc2_pointer) {
                /* proc2 is in group1 - don't double count */
                found_in_group = true;
                break;
            }
        }                       /* end proc1 loop */

        if (found_in_group) {
            continue;
        }

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
    for (int proc1 = 0; proc1 < group1->grp_proc_count; ++proc1) {
        new_group_pointer->grp_proc_pointers[proc1] =
            ompi_group_get_proc_ptr_raw (group1, proc1);
    }
    cnt = group1->grp_proc_count;

    /* check group2 elements to see if they need to be included in the list */
    for (int proc2 = 0; proc2 < group2->grp_proc_count; ++proc2) {
        bool found_in_group = false;

        proc2_pointer = ompi_group_get_proc_ptr_raw (group2, proc2);

        /* check to see if this proc2 is alread in the group */
        for (int proc1 = 0; proc1 < group1->grp_proc_count; ++proc1) {
            proc1_pointer = ompi_group_get_proc_ptr_raw (group1, proc1);

            if (proc1_pointer == proc2_pointer) {
                /* proc2 is in group1 - don't double count */
                found_in_group = true;
                break;
            }
        }                       /* end proc1 loop */

        if (found_in_group) {
            continue;
        }

        new_group_pointer->grp_proc_pointers[cnt++] = proc2_pointer;
    }                           /* end proc loop */

    /* increment proc reference counters */
    ompi_group_increment_proc_count(new_group_pointer);

    /* find my rank */
    my_group_rank = group1->grp_my_rank;
    if (MPI_UNDEFINED == my_group_rank) {
        my_group_rank = group2->grp_my_rank;
        if ( MPI_UNDEFINED != my_group_rank) {
            my_proc_pointer = ompi_group_peer_lookup(group2,my_group_rank);
        }
    } else {
        my_proc_pointer = ompi_group_peer_lookup(group1,my_group_rank);
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

/*
 * Group Difference has to use the dense format since we don't support
 * two parent groups in the group structure and maintain functions
 */
int ompi_group_difference(ompi_group_t* group1, ompi_group_t* group2,
                          ompi_group_t **new_group) {

    /* local varibles */
    int new_group_size, my_group_rank;
    ompi_group_t *new_group_pointer;
    ompi_proc_t *proc1_pointer, *proc2_pointer, *my_proc_pointer = NULL;

    /*
     * form union
     */

    /* get new group size */
    new_group_size = group1->grp_proc_count;

    /* loop over group1 members */
    for (int proc1 = 0 ; proc1 < group1->grp_proc_count ; ++proc1) {
        proc1_pointer = ompi_group_get_proc_ptr_raw (group1, proc1);

        /* check to see if this proc is in group2 */
        for (int proc2 = 0 ; proc2 < group2->grp_proc_count ; ++proc2) {
            proc2_pointer = ompi_group_get_proc_ptr_raw (group2, proc2);
            if( proc1_pointer == proc2_pointer ) {
                --new_group_size;
                break;
            }
        }  /* end proc1 loop */
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
    /* loop over group1 members */
    for (int proc1 = 0, cnt = 0 ; proc1 < group1->grp_proc_count ; ++proc1) {
        bool found_in_group2 = false;

        proc1_pointer = ompi_group_get_proc_ptr_raw (group1, proc1);
        /* check to see if this proc is in group2 */
        for (int proc2 = 0 ; proc2 < group2->grp_proc_count ; ++proc2) {
            proc2_pointer = ompi_group_get_proc_ptr_raw (group2, proc2);
            if( proc1_pointer == proc2_pointer ) {
                found_in_group2 = true;
                break;
            }
        }  /* end proc1 loop */

        if (found_in_group2) {
            continue;
        }

        new_group_pointer->grp_proc_pointers[cnt++] = proc1_pointer;
    }  /* end proc loop */

    /* increment proc reference counters */
    ompi_group_increment_proc_count(new_group_pointer);

    /* find my rank */
    my_group_rank=group1->grp_my_rank;
    if ( MPI_UNDEFINED != my_group_rank ) {
        my_proc_pointer = ompi_group_peer_lookup(group1,my_group_rank);
    }
    else {
        my_group_rank=group2->grp_my_rank;
        if ( MPI_UNDEFINED != my_group_rank ) {
            my_proc_pointer = ompi_group_peer_lookup(group2,my_group_rank);
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
