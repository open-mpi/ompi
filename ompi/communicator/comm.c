/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2023 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2022 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2011 University of Houston. All rights reserved.
 * Copyright (c) 2007-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Inria.  All rights reserved.
 * Copyright (c) 2011-2013 Universite Bordeaux 1
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2012-2016 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2017-2022 IBM Corporation.  All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2018-2022 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2023      Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include <stdio.h>

#include "ompi/constants.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/pmix/pmix-internal.h"
#include "opal/util/string_copy.h"

#include "ompi/proc/proc.h"
#include "opal/mca/threads/mutex.h"
#include "opal/util/bit_ops.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "ompi/mca/topo/topo.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/dpm/dpm.h"

#include "ompi/attribute/attribute.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/request/request.h"

#include "ompi/runtime/params.h"

struct ompi_comm_split_type_hw_guided_t {
    const char *info_value;
    int split_type;
};
typedef struct ompi_comm_split_type_hw_guided_t ompi_comm_split_type_hw_guided_t;

/*
 * The ompi_comm_split_unguided function uses this array to determine the next
 * topology to test for a MPI_COMM_TYPE_HW_UNGUIDED communicator split. Therefore,
 * the order in this array must be from largest topology class to smallest.
 */
static const ompi_comm_split_type_hw_guided_t ompi_comm_split_type_hw_guided_support[] = {
    {.info_value = "cluster",  .split_type = OMPI_COMM_TYPE_CLUSTER},
    {.info_value = "cu",       .split_type = OMPI_COMM_TYPE_CU},
    {.info_value = "host",     .split_type = OMPI_COMM_TYPE_HOST},
    {.info_value = "mpi_shared_memory", .split_type = MPI_COMM_TYPE_SHARED},
    {.info_value = "board",    .split_type = OMPI_COMM_TYPE_BOARD},
    {.info_value = "numanode", .split_type = OMPI_COMM_TYPE_NUMA},
    {.info_value = "socket",   .split_type = OMPI_COMM_TYPE_SOCKET},
    {.info_value = "l3cache",  .split_type = OMPI_COMM_TYPE_L3CACHE},
    {.info_value = "l2cache",  .split_type = OMPI_COMM_TYPE_L2CACHE},
    {.info_value = "l1cache",  .split_type = OMPI_COMM_TYPE_L1CACHE},
    {.info_value = "core",     .split_type = OMPI_COMM_TYPE_CORE},
    {.info_value = "hwthread", .split_type = OMPI_COMM_TYPE_HWTHREAD},
    {.info_value = NULL},
};

static const char * ompi_comm_split_type_to_str(int split_type) {
    for (int i = 0; NULL != ompi_comm_split_type_hw_guided_support[i].info_value; ++i) {
        if (split_type == ompi_comm_split_type_hw_guided_support[i].split_type) {
            return ompi_comm_split_type_hw_guided_support[i].info_value;
        }
    }
    if (MPI_COMM_TYPE_HW_GUIDED == split_type) {
        return "MPI_COMM_TYPE_HW_GUIDED";
    }
    else if (MPI_COMM_TYPE_HW_UNGUIDED == split_type) {
        return "MPI_COMM_TYPE_HW_UNGUIDED";
    }
    return "Unknown";
}

/*
** sort-function for MPI_Comm_split
*/
static int rankkeycompare(const void *, const void *);

/**
 * to fill the rest of the stuff for the communicator
 */
static int ompi_comm_fill_rest (ompi_communicator_t *comm,
                                int num_procs,
                                ompi_proc_t **proc_pointers,
                                int my_rank,
                                ompi_errhandler_t *errh );
/*
** typedef for the allgather_intra required in comm_split.
** the reason for introducing this abstraction is, that
** for Comm_split for inter-coms, we do not have this
** functions, so we need to emulate it.
*/
typedef int ompi_comm_allgatherfct (void* inbuf, int incount, MPI_Datatype intype,
                                    void* outbuf, int outcount, MPI_Datatype outtype,
                                    ompi_communicator_t *comm,
                                    mca_coll_base_module_t *data);

static int ompi_comm_allgather_emulate_intra (void* inbuf, int incount, MPI_Datatype intype,
                                              void* outbuf, int outcount,
                                              MPI_Datatype outtype,
                                              ompi_communicator_t *comm,
                                              mca_coll_base_module_t *data);

static int ompi_comm_copy_topo (ompi_communicator_t *oldcomm,
                                ompi_communicator_t *newcomm);

/* idup with local group and info. the local group support is provided to support ompi_comm_set_nb */
static int ompi_comm_idup_internal (ompi_communicator_t *comm, ompi_group_t *group, ompi_group_t *remote_group,
                                    opal_info_t *info, ompi_communicator_t **newcomm, ompi_request_t **req);


static int ompi_comm_get_rprocs (ompi_communicator_t *local_comm, ompi_communicator_t *bridge_comm,
                                 int local_leader, int remote_leader, int tag, int rsize,
                                 ompi_proc_t ***rprocs);

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/*
 * This is the function setting all elements of a communicator.
 * All other routines are just used to determine these elements.
 */

int ompi_comm_set ( ompi_communicator_t **ncomm,
                    ompi_communicator_t *oldcomm,
                    int local_size,
                    int *local_ranks,
                    int remote_size,
                    int *remote_ranks,
                    opal_hash_table_t *attr,
                    ompi_errhandler_t *errh,
                    ompi_group_t *local_group,
                    ompi_group_t *remote_group,
                    uint32_t flags)
{
    ompi_request_t *req;
    int rc;

    rc = ompi_comm_set_nb (ncomm, oldcomm, local_size, local_ranks, remote_size, remote_ranks,
                           attr, errh, local_group, remote_group, flags, &req);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    if (NULL != req) {
        rc = ompi_request_wait( &req, MPI_STATUS_IGNORE);
    }

    return rc;
}

static int ompi_comm_set_simple (ompi_communicator_t **ncomm, ompi_errhandler_t *errhandler,
                                 ompi_group_t *local_group)
{
    return ompi_comm_set (ncomm, NULL, local_group->grp_proc_count, NULL, 0, NULL, NULL, errhandler,
			  local_group, NULL, 0);
}


/*
 * if remote_group == &ompi_mpi_group_null, then the new communicator
 * is forced to be an inter communicator.
 */
int ompi_comm_set_nb (ompi_communicator_t **ncomm, ompi_communicator_t *oldcomm, int local_size,
                      int *local_ranks, int remote_size, int *remote_ranks, opal_hash_table_t *attr,
                      ompi_errhandler_t *errh, ompi_group_t *local_group, ompi_group_t *remote_group,
                      uint32_t flags, ompi_request_t **req)
{
    bool copy_topocomponent = !!(flags & OMPI_COMM_SET_FLAG_COPY_TOPOLOGY);
    bool dup_comm = !(flags & OMPI_COMM_SET_FLAG_LOCAL_COMM_NODUP);
    ompi_communicator_t *newcomm = NULL;
    int ret;

    if (NULL != local_group) {
        local_size = ompi_group_size (local_group);
    }

    if ( (NULL != remote_group) && (&ompi_mpi_group_null.group != remote_group) ) {
        remote_size = ompi_group_size (remote_group);
    }

    *req = NULL;

    /* ompi_comm_allocate */
    newcomm = OBJ_NEW(ompi_communicator_t);
    if (NULL == newcomm) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    newcomm->c_name = (char*) malloc (OPAL_MAX_OBJECT_NAME);
    if (NULL == newcomm->c_name) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    newcomm->c_name[0]    = '\0';
    newcomm->super.s_info = NULL;
    /* fill in the inscribing hyper-cube dimensions */
    newcomm->c_cube_dim = opal_cube_dim(local_size);

    if (NULL == local_group) {
        /* determine how the list of local_rank can be stored most
           efficiently */
        ret = ompi_group_incl(oldcomm->c_local_group, local_size,
                              local_ranks, &newcomm->c_local_group);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            return ret;
        }
    } else {
        newcomm->c_local_group = local_group;
        OBJ_RETAIN(newcomm->c_local_group);
    }
    newcomm->c_my_rank = newcomm->c_local_group->grp_my_rank;
    newcomm->c_assertions = 0;

    /* Set remote group and duplicate the local comm, if applicable */
    if ((NULL == remote_group) && (NULL != remote_ranks)) {
        /* determine how the list of local_rank can be stored most
           efficiently */
        ret = ompi_group_incl(oldcomm->c_remote_group, remote_size,
                              remote_ranks, &newcomm->c_remote_group);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            return ret;
        }
        remote_group = newcomm->c_remote_group;
    }
    if ( NULL != remote_group ) {
        ompi_communicator_t *old_localcomm;

        if (&ompi_mpi_group_null.group == remote_group) {
            ret = ompi_group_incl(oldcomm->c_remote_group, remote_size,
                                  remote_ranks, &newcomm->c_remote_group);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                return ret;
            }
        } else {
            newcomm->c_remote_group = remote_group;
            OBJ_RETAIN(newcomm->c_remote_group);
        }

        newcomm->c_flags |= OMPI_COMM_INTER;

        if (dup_comm) {
            old_localcomm = OMPI_COMM_IS_INTRA(oldcomm) ? oldcomm : oldcomm->c_local_comm;

            /* NTH: use internal idup function that takes a local group argument */
            ompi_comm_idup_internal (old_localcomm, newcomm->c_local_group, NULL, NULL,
                                     &newcomm->c_local_comm, req);
        } else {
            /* take ownership of the old communicator (it must be an intracommunicator) */
            assert (OMPI_COMM_IS_INTRA(oldcomm));
            newcomm->c_local_comm = oldcomm;
        }
    } else {
        newcomm->c_remote_group = newcomm->c_local_group;
        OBJ_RETAIN(newcomm->c_remote_group);
    }

    /* Check how many different jobids are represented in this communicator.
       Necessary for the disconnect of dynamic communicators. */

    if ( 0 < local_size && (OMPI_COMM_IS_INTRA(newcomm) || 0 <remote_size) ) {
        ompi_dpm_mark_dyncomm (newcomm);
    }

    /* Set error handler */
    newcomm->error_handler = errh;
    OBJ_RETAIN ( newcomm->error_handler );

    /* Set Topology, if required and if available */
    if (NULL != oldcomm && copy_topocomponent && (NULL != oldcomm->c_topo) ) {
        /**
         * The MPI standard is pretty clear on this, the topology information
         * behave as info keys, and is copied only on MPI_Comm_dup.
         */
        if (OMPI_SUCCESS != (ret = ompi_comm_copy_topo(oldcomm, newcomm))) {
            ompi_comm_free(&newcomm);
            return ret;
        }
    }

    /* Copy attributes and call according copy functions, if required */
    if (NULL != oldcomm && NULL != oldcomm->c_keyhash) {
        if (NULL != attr) {
            ompi_attr_hash_init(&newcomm->c_keyhash);
            if (OMPI_SUCCESS != (ret = ompi_attr_copy_all (COMM_ATTR, oldcomm,
                                                           newcomm, attr,
                                                           newcomm->c_keyhash))) {
                ompi_comm_free(&newcomm);
                return ret;
            }
        }
    }

    if (NULL != oldcomm) {
        newcomm->instance = oldcomm->instance;
    }

    *ncomm = newcomm;
    return (OMPI_SUCCESS);
}


/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/*
** Counterpart to MPI_Comm_group. To be used within OMPI functions.
*/
int ompi_comm_group ( ompi_communicator_t* comm, ompi_group_t **group )
{
    /* increment reference counters for the group */
    OBJ_RETAIN(comm->c_local_group);

    *group = comm->c_local_group;
    return OMPI_SUCCESS;
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/*
** Counterpart to MPI_Comm_create. To be used within OMPI.
*/
int ompi_comm_create_w_info (ompi_communicator_t *comm, ompi_group_t *group, opal_info_t *info,
                             ompi_communicator_t **newcomm)
{
    ompi_communicator_t *newcomp = NULL;
    int rsize;
    int mode,i,j;
    int *allranks=NULL;
    int *rranks=NULL;
    int rc = OMPI_SUCCESS;
    ompi_group_t *remote_group = NULL;

    /* silence clang warning. newcomm should never be NULL */
    if (OPAL_UNLIKELY(NULL == newcomm)) {
        return OMPI_ERR_BAD_PARAM;
    }

    if ( OMPI_COMM_IS_INTER(comm) ) {
        int tsize;
        remote_group = &ompi_mpi_group_null.group;

        tsize = ompi_comm_remote_size(comm);
        allranks = (int *) malloc ( tsize * sizeof(int));
        if ( NULL == allranks ) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }

        rc = comm->c_coll->coll_allgather ( &(group->grp_my_rank),
                                           1, MPI_INT, allranks,
                                           1, MPI_INT, comm,
                                           comm->c_coll->coll_allgather_module);
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }

        /* Count number of procs in future remote group */
        for (rsize=0, i = 0; i < tsize; i++) {
            if ( MPI_UNDEFINED != allranks[i] ) {
                rsize++;
            }
        }

        /* If any of those groups is empty, we have to return
           MPI_COMM_NULL */
        if ( 0 == rsize || 0 == group->grp_proc_count ) {
            newcomp = MPI_COMM_NULL;
            rc = OMPI_SUCCESS;
            goto exit;
        }

        /* Set proc-pointers for remote group */
        rranks = (int *) malloc ( rsize * sizeof(int));
        if ( NULL == rranks ) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }

        for ( j = 0, i = 0; i < tsize; i++ ) {
            if ( MPI_UNDEFINED != allranks[i] ) {
                rranks[j] = i;
                j++;
            }
        }
        mode = OMPI_COMM_CID_INTER;

    } else {
        rsize  = 0;
        rranks = NULL;
        mode   = OMPI_COMM_CID_INTRA;
    }

    rc = ompi_comm_set ( &newcomp,                 /* new comm */
                         comm,                     /* old comm */
                         0,                        /* local array size */
                         NULL,                     /* local_ranks */
                         rsize,                    /* remote_size */
                         rranks,                   /* remote_ranks */
                         NULL,                     /* attrs */
                         comm->error_handler,      /* error handler */
                         group,                    /* local group */
                         remote_group,             /* remote group */
                         0);                       /* flags */

    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid (newcomp, comm, NULL, NULL, NULL, false, mode);
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    /* Copy info if there is one. */
    newcomp->super.s_info = OBJ_NEW(opal_info_t);
    if (info) {
        opal_info_dup(info, &(newcomp->super.s_info));
    }

    /* Set name for debugging purposes */
    snprintf(newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI COMMUNICATOR %s CREATE FROM %s",
	     ompi_comm_print_cid (newcomp), ompi_comm_print_cid (comm));

    /* Activate the communicator and init coll-component */
    rc = ompi_comm_activate (&newcomp, comm, NULL, NULL, NULL, false, mode);
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }


    /* Check whether we are part of the new comm.
       If not, we have to free the structure again.
       However, we could not avoid the comm_nextcid step, since
       all processes of the original comm have to participate in
       that function call. Additionally, all errhandler stuff etc.
       has to be set to make ompi_comm_free happy */
    if ( MPI_UNDEFINED == newcomp->c_local_group->grp_my_rank ) {
        ompi_comm_free ( &newcomp );
    }

 exit:
    if ( NULL != allranks ) {
        free ( allranks );
    }
    if ( NULL != rranks ) {
        free ( rranks );
    }

    *newcomm = newcomp;
    return ( rc );
}

int ompi_comm_create ( ompi_communicator_t *comm, ompi_group_t *group,
                       ompi_communicator_t **newcomm )
{
    return ompi_comm_create_w_info (comm, group, NULL, newcomm);
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/

int ompi_comm_split_with_info( ompi_communicator_t* comm, int color, int key,
                               opal_info_t *info,
                               ompi_communicator_t **newcomm, bool pass_on_topo )
{
    int myinfo[2];
    int size, my_size;
    int my_rsize=0;
    int mode;
    int rsize;
    int i, loc;
    int inter;
    int *results=NULL, *sorted=NULL;
    int *rresults=NULL, *rsorted=NULL;
    int rc=OMPI_SUCCESS;
    ompi_communicator_t *newcomp = NULL;
    int *lranks=NULL, *rranks=NULL;
    ompi_group_t * local_group=NULL, *remote_group=NULL;

    ompi_comm_allgatherfct *allgatherfct=NULL;

    /* Step 1: determine all the information for the local group */
    /* --------------------------------------------------------- */

    /* sort according to color and rank. Gather information from everyone */
    myinfo[0] = color;
    myinfo[1] = key;

    size     = ompi_comm_size ( comm );
    inter    = OMPI_COMM_IS_INTER(comm);
    if ( inter ) {
        allgatherfct = (ompi_comm_allgatherfct *)ompi_comm_allgather_emulate_intra;
    } else {
        allgatherfct = (ompi_comm_allgatherfct *)comm->c_coll->coll_allgather;
    }

    results  = (int*) malloc ( 2 * size * sizeof(int));
    if ( NULL == results ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    rc = allgatherfct( myinfo, 2, MPI_INT, results, 2, MPI_INT, comm, comm->c_coll->coll_allgather_module );
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    /* how many have the same color like me */
    for ( my_size = 0, i=0; i < size; i++) {
        if ( results[(2*i)+0] == color) {
            my_size++;
        }
    }

    /* silence clang warning. my_size should never be 0 here */
    if (OPAL_UNLIKELY(0 == my_size)) {
        rc = OMPI_ERR_BAD_PARAM;
        goto exit;
    }

    sorted = (int *) calloc (my_size * 2, sizeof (int));
    if ( NULL == sorted) {
        rc =  OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    /* ok we can now fill this info */
    for( loc = 0, i = 0; i < size; i++ ) {
        if ( results[(2*i)+0] == color) {
            sorted[(2*loc)+0] = i;                 /* copy org rank */
            sorted[(2*loc)+1] = results[(2*i)+1];  /* copy key */
            loc++;
        }
    }

    /* the new array needs to be sorted so that it is in 'key' order */
    /* if two keys are equal then it is sorted in original rank order! */
    if(my_size>1){
        qsort ((int*)sorted, my_size, sizeof(int)*2, rankkeycompare);
    }

    /* put group elements in a list */
    lranks = (int *) malloc ( my_size * sizeof(int));
    if ( NULL == lranks ) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }
    for (i = 0; i < my_size; i++) {
        lranks[i] = sorted[i*2];
    }

    /* Step 2: determine all the information for the remote group */
    /* --------------------------------------------------------- */
    if ( inter ) {
        remote_group = &ompi_mpi_group_null.group;
        rsize    = comm->c_remote_group->grp_proc_count;
        rresults = (int *) malloc ( rsize * 2 * sizeof(int));
        if ( NULL == rresults ) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }

        /* this is an allgather on an inter-communicator */
        rc = comm->c_coll->coll_allgather( myinfo, 2, MPI_INT, rresults, 2,
                                          MPI_INT, comm,
                                          comm->c_coll->coll_allgather_module);
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }

        /* how many have the same color like me */
        for ( my_rsize = 0, i=0; i < rsize; i++) {
            if ( rresults[(2*i)+0] == color) {
                my_rsize++;
            }
        }

        if (my_rsize > 0) {
            rsorted = (int *) calloc (my_rsize * 2, sizeof (int));
            if ( NULL == rsorted) {
                rc = OMPI_ERR_OUT_OF_RESOURCE;
                goto exit;
            }

            /* ok we can now fill this info */
            for( loc = 0, i = 0; i < rsize; i++ ) {
                if ( rresults[(2*i)+0] == color) {
                    rsorted[(2*loc)+0] = i;                  /* org rank */
                    rsorted[(2*loc)+1] = rresults[(2*i)+1];  /* key */
                    loc++;
                }
            }

            /* the new array needs to be sorted so that it is in 'key' order */
            /* if two keys are equal then it is sorted in original rank order! */
            if (my_rsize > 1) {
                qsort ((int*)rsorted, my_rsize, sizeof(int)*2, rankkeycompare);
            }

            /* put group elements in a list */
            rranks = (int *) malloc ( my_rsize * sizeof(int));
            if ( NULL ==  rranks) {
                rc = OMPI_ERR_OUT_OF_RESOURCE;
                goto exit;
            }

            for (i = 0; i < my_rsize; i++) {
                rranks[i] = rsorted[i*2];
            }
        }

        rc = ompi_group_incl(comm->c_local_group, my_size, lranks, &local_group);
        if (OMPI_SUCCESS != rc) {
            goto exit;
        }

        mode = OMPI_COMM_CID_INTER;
    } else {
        rranks = NULL;
        mode      = OMPI_COMM_CID_INTRA;
    }

    /* Step 3: set up the communicator                           */
    /* --------------------------------------------------------- */
    /* Create the communicator finally */

    rc = ompi_comm_set ( &newcomp,           /* new comm */
                         comm,               /* old comm */
                         my_size,            /* local_size */
                         lranks,             /* local_ranks */
                         my_rsize,           /* remote_size */
                         rranks,             /* remote_ranks */
                         NULL,               /* attrs */
                         comm->error_handler,/* error handler */
                         local_group,        /* local group */
                         remote_group,       /* remote group */
                         pass_on_topo ? OMPI_COMM_SET_FLAG_COPY_TOPOLOGY : 0); /* flags */

    if ( OMPI_SUCCESS != rc  ) {
        goto exit;
    }

    if ( inter ) {
        OBJ_RELEASE(local_group);
        if (NULL != newcomp->c_local_comm) {
            snprintf(newcomp->c_local_comm->c_name, MPI_MAX_OBJECT_NAME,
                     "MPI COMM %s SPLIT FROM %s", ompi_comm_print_cid (newcomp),
		     ompi_comm_print_cid (comm));
        }
    }

    /* set the rank to MPI_UNDEFINED. This prevents this process from interfering
     * in ompi_comm_nextcid() and the collective module selection in ompi_comm_activate()
     * for a communicator that will be freed anyway.
     */
    if ( MPI_UNDEFINED == color || (inter && my_rsize==0)) {
        newcomp->c_local_group->grp_my_rank = MPI_UNDEFINED;
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid (newcomp, comm, NULL, NULL, NULL, false, mode);
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    /* Set name for debugging purposes */
    snprintf(newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI COMM %s SPLIT FROM %s",
	     ompi_comm_print_cid (newcomp), ompi_comm_print_cid (comm));

    /* Copy info if there is one */
    if (info) {
        newcomp->super.s_info = OBJ_NEW(opal_info_t);
        opal_info_dup(info, &(newcomp->super.s_info));
    }

    /* Activate the communicator and init coll-component */
    rc = ompi_comm_activate (&newcomp, comm, NULL, NULL, NULL, false, mode);

    /* MPI-4 §7.4.4 requires us to remove all unknown keys from the info object */
    if (NULL != newcomp->super.s_info) {
        opal_info_remove_unreferenced(newcomp->super.s_info);
    }

 exit:
    free ( results );
    free ( sorted );
    free ( rresults );
    free ( rsorted );
    free ( lranks );
    free ( rranks );

    /* Step 4: if we are not part of the comm, free the struct   */
    /* --------------------------------------------------------- */
    if (inter && my_rsize == 0) {
        color = MPI_UNDEFINED;
    }
    if ( NULL != newcomp && MPI_UNDEFINED == color ) {
        ompi_comm_free ( &newcomp );
    }

    *newcomm = newcomp;
    return rc;
}


/*
** Counterpart to MPI_Comm_split. To be used within OMPI (e.g. MPI_Cart_sub).
*/
int ompi_comm_split( ompi_communicator_t* comm, int color, int key,
                     ompi_communicator_t **newcomm, bool pass_on_topo )
{
    return ompi_comm_split_with_info(comm, color, key, NULL, newcomm, pass_on_topo);
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/*
 * Produces an array of ranks that will be part of the local/remote group in the
 * new communicator. The results array will be modified by this call.
 */
static int ompi_comm_split_type_get_part (ompi_group_t *group, const int split_type, int **ranks_out, int *rank_size) {
    int size = ompi_group_size (group);
    int my_size = 0;
    int *ranks;
    int ret;

    ranks = malloc (size * sizeof (int));
    if (OPAL_UNLIKELY(NULL == ranks)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (int i = 0 ; i < size ; ++i) {
        ompi_proc_t *proc = ompi_group_get_proc_ptr_raw (group, i);
        uint16_t locality, *u16ptr;
        int include = false;

        if (ompi_proc_is_sentinel (proc)) {
            opal_process_name_t proc_name = ompi_proc_sentinel_to_name ((uintptr_t) proc);

            if (split_type <= OMPI_COMM_TYPE_HOST) {
                /* local ranks should never be represented by sentinel procs. ideally we
                 * should be able to use OPAL_MODEX_RECV_VALUE_OPTIONAL but it does have
                 * some overhead. update this to use the optional recv if that is ever fixed. */
                continue;
            }

            u16ptr = &locality;

            OPAL_MODEX_RECV_VALUE_OPTIONAL(ret, PMIX_LOCALITY, &proc_name, &u16ptr, PMIX_UINT16);
            if (OPAL_SUCCESS != ret) {
                continue;
            }
        } else {
            locality = proc->super.proc_flags;
        }

        switch (split_type) {
        case OMPI_COMM_TYPE_HWTHREAD:
            include = OPAL_PROC_ON_LOCAL_HWTHREAD(locality);
            break;
        case OMPI_COMM_TYPE_CORE:
            include = OPAL_PROC_ON_LOCAL_CORE(locality);
            break;
        case OMPI_COMM_TYPE_L1CACHE:
            include = OPAL_PROC_ON_LOCAL_L1CACHE(locality);
            break;
        case OMPI_COMM_TYPE_L2CACHE:
            include = OPAL_PROC_ON_LOCAL_L2CACHE(locality);
            break;
        case OMPI_COMM_TYPE_L3CACHE:
            include = OPAL_PROC_ON_LOCAL_L3CACHE(locality);
            break;
        case OMPI_COMM_TYPE_SOCKET:
            include = OPAL_PROC_ON_LOCAL_SOCKET(locality);
            break;
        case OMPI_COMM_TYPE_NUMA:
            include = OPAL_PROC_ON_LOCAL_NUMA(locality);
            break;
        case MPI_COMM_TYPE_SHARED:
            include = OPAL_PROC_ON_LOCAL_NODE(locality);
            break;
        case OMPI_COMM_TYPE_BOARD:
            include = OPAL_PROC_ON_LOCAL_BOARD(locality);
            break;
        case OMPI_COMM_TYPE_HOST:
            include = OPAL_PROC_ON_LOCAL_HOST(locality);
            break;
        case OMPI_COMM_TYPE_CU:
            include = OPAL_PROC_ON_LOCAL_CU(locality);
            break;
        case OMPI_COMM_TYPE_CLUSTER:
            include = OPAL_PROC_ON_LOCAL_CLUSTER(locality);
            break;
        case MPI_COMM_TYPE_HW_GUIDED:
        case MPI_COMM_TYPE_HW_UNGUIDED:
            /*
             * MPI_COMM_TYPE_HW_(UN)GUIDED handled in calling function.
             * We should not get here as the split type will be changed
             * at a higher level.
             */
            opal_show_help("help-comm.txt",
                           "unexpected-split-type",
                           true,
                           ompi_comm_split_type_to_str(split_type),
                           split_type);
            free (ranks);
            return OMPI_ERR_BAD_PARAM;
        }

        if (include) {
            ranks[my_size++] = i;
        }
    }

    *rank_size = my_size;

    /* silence a clang warning about a 0-byte malloc. my_size will never be 0 here */
    if (OPAL_UNLIKELY(0 == my_size)) {
        free (ranks);
        return OMPI_SUCCESS;
    }

    /* shrink the rank array */
    int *tmp = realloc (ranks, my_size * sizeof (int));
    if (OPAL_LIKELY(NULL != tmp)) {
        ranks = tmp;
    }

    *ranks_out = ranks;

    return OMPI_SUCCESS;
}

static int ompi_comm_split_verify (ompi_communicator_t *comm, int split_type, int key, bool *need_split)
{
    int rank = ompi_comm_rank (comm);
    int size = ompi_comm_size (comm);
    int *results;
    int rc;

    if (*need_split) {
        return OMPI_SUCCESS;
    }

    results = malloc (2 * sizeof (int) * size);
    if (OPAL_UNLIKELY(NULL == results)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    *need_split = false;

    results[rank * 2] = split_type;
    results[rank * 2 + 1] = key;

    rc = comm->c_coll->coll_allgather (MPI_IN_PLACE, 2, MPI_INT, results, 2, MPI_INT, comm,
                                      comm->c_coll->coll_allgather_module);
    if (OMPI_SUCCESS != rc) {
        free (results);
        return rc;
    }

    for (int i = 0 ; i < size ; ++i) {
        if (MPI_UNDEFINED == results[i * 2] || (i >= 1 && results[i * 2 + 1] < results[i * 2 - 1])) {
            *need_split = true;
            break;
        }
    }

    free (results);

    return OMPI_SUCCESS;
}

/**
 * ompi_comm_split_type_core: Perform common processing for a MPI_Comm_type_split
 *                            function call.
 *
 * comm(in) i            : input communicator
 * global_split_type(in) : Split type defined by all ranks in the communicator
 * local_split_type(in)  : Split type defined by this rank in the communicator
 *                         (might be MPI_UNDEFINED)
 * key(in)               : original key from the user
 * need_split            : If a split is needed to separate ranks supplying
 *                         MPI_UNDEFINED from others
 * no_reorder(in)        : Did all of the ranks specify the same key (optimization)
 * no_undefined(in)      : None of the ranks specified MPI_UNDEFINED (optimization)
 * info(out)             : info guiding the split operation
 * newcomm(out)          : Pointer to the newly created communicator, or pointer to MPI_COMM_NULL
 *                         if no communicator created.
 */
static int ompi_comm_split_type_core(ompi_communicator_t *comm,
                                     int global_split_type, int local_split_type,
                                     int key, bool need_split, bool no_reorder,
                                     bool no_undefined, opal_info_t *info,
                                     ompi_communicator_t **newcomm)
{
    int *lranks = NULL, *rranks = NULL;
    ompi_communicator_t *newcomp = MPI_COMM_NULL;
    int my_size, my_rsize = 0, mode;
    int rc;
    int inter = OMPI_COMM_IS_INTER(comm);

    /* Perform the rest of the processing for a communicator split.
     *
     * Step 1: Build potential communicator groups. If any ranks will not be part of
     * the ultimate communicator we will drop them later. This saves doing an extra
     * allgather on the whole communicator. By using ompi_comm_split() later only
     * if needed we 1) optimized the common case (no MPI_UNDEFINED and no reorder),
     * and 2) limit the allgather to a smaller set of peers in the uncommon case. */
    /* --------------------------------------------------------- */

    /* allowed splitting types:
       CLUSTER
       CU
       HOST
       BOARD
       NODE
       NUMA
       SOCKET
       L3CACHE
       L2CACHE
       L1CACHE
       CORE
       HWTHREAD
       Even though HWTHREAD/CORE etc. is overkill they are here for consistency.
       They will most likely return a communicator which is equal to MPI_COMM_SELF
       Unless oversubscribing.
    */

    /* how many ranks are potentially participating and on my node? */
    rc = ompi_comm_split_type_get_part (comm->c_local_group, global_split_type, &lranks, &my_size);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    /* Step 2: determine all the information for the remote group */
    /* --------------------------------------------------------- */
    if (inter) {
        rc = ompi_comm_split_type_get_part (comm->c_remote_group, global_split_type, &rranks, &my_rsize);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            free (lranks);
            return rc;
        }
    }

    /* set the CID allgather mode to the appropriate one for the communicator */
    mode = inter ? OMPI_COMM_CID_INTER : OMPI_COMM_CID_INTRA;

    /* Step 3: set up the communicator                           */
    /* --------------------------------------------------------- */
    /* Create the communicator finally */

    rc = ompi_comm_set (&newcomp, comm, my_size, lranks, my_rsize,
                        rranks, NULL, comm->error_handler, NULL, NULL, 0);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
       goto exit;
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid (newcomp, comm, NULL, NULL, NULL, false, mode);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        goto exit;
    }

    ompi_comm_assert_subscribe (newcomp, OMPI_COMM_ASSERT_LAZY_BARRIER);
    ompi_comm_assert_subscribe (newcomp, OMPI_COMM_ASSERT_ACTIVE_POLL);
    if (info) {
        opal_infosubscribe_change_info(&newcomp->super, info);
    }

    /* Activate the communicator and init coll-component */
    rc = ompi_comm_activate (&newcomp, comm, NULL, NULL, NULL, false, mode);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        goto exit;
    }

    /* Step 4: Check if we need to remove or reorder ranks in the communicator */
    if (!(no_reorder && no_undefined)) {
        rc = ompi_comm_split_verify (newcomp, local_split_type, key, &need_split);

        if (inter) {
            /* verify that no local ranks need to be removed or reordered */
            rc = ompi_comm_split_verify (newcomp->c_local_comm, local_split_type, key,
                                         &need_split);
        }
    }

    if (!need_split) {

        /* common case. no reordering and no MPI_UNDEFINED */
        *newcomm = newcomp;

        /* Set name for debugging purposes */
        snprintf(newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI COMM %s SPLIT_TYPE FROM %s",
        ompi_comm_print_cid (newcomp), ompi_comm_print_cid (comm));
        goto exit;
    }

    /* MPI-4 §7.4.4 requires us to remove all unknown keys from the info object */
    opal_info_remove_unreferenced(newcomp->super.s_info);

    /* TODO: there probably is better way to handle this case without throwing away the
     * intermediate communicator. */
    rc = ompi_comm_split (newcomp, local_split_type, key, newcomm, false);
    /* get rid of the intermediate communicator */
    ompi_comm_free (&newcomp);

 exit:
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc && MPI_COMM_NULL != newcomp)) {
        ompi_comm_free (&newcomp);
        *newcomm = MPI_COMM_NULL;
    }

    free (lranks);
    free (rranks);

    return rc;
}

/**
 * ompi_comm_split_unguided: Process an MPI_Comm_split_type function call where the
 *                           split is performed at the next lower topology level where
 *                           the new communicator would contain fewer ranks than the
 *                           input communicator.
 *
 * comm(in)         : Input communicator
 * split_type(in)   : Communicator split type for this task, may be MPI_UNDEFINED
 * key(in)          : Original key from the user
 * need_split(in)   : If a split is needed to separate ranks supplying MPI_UNDEFINED
 *                    from others
 * no_reorder(in)   : Did all of the ranks specify the same key (optimization)
 * no_undefined(in) : None of the ranks specified MPI_UNDEFINED (optimization)
 * info(out)        : Info object to update with selected topology class
 * newcomm(out)     : Pointer to newly created communicator, or pointer to
 *                    MPI_COMM_NULL if no communicator generated.
 */
static int ompi_comm_split_unguided(ompi_communicator_t *comm, int split_type, int key,
                                    bool need_split, bool no_reorder,
                                    bool no_undefined, struct opal_info_t *info,
                                    ompi_communicator_t** newcomm)
{
    int i, rc;
    struct opal_info_t *split_info = NULL;
    int new_size, original_size;
    ompi_communicator_t *unguided_comm = NULL;

    if (1 == ompi_comm_size(comm)) {
        /* Communicator cannot be split any smaller */
        *newcomm = MPI_COMM_NULL;
        return OMPI_SUCCESS;
    }

    /*
     * Split out the MPI_UNDEFINED
     */
    rc = ompi_comm_split(comm,
                         MPI_UNDEFINED == split_type ? MPI_UNDEFINED : 0,
                         key,
                         &unguided_comm, false);
    if (OMPI_SUCCESS != rc) {
        *newcomm = MPI_COMM_NULL;
        return rc;
    }
    if (MPI_UNDEFINED == split_type) {
        ompi_comm_free(&unguided_comm);
        *newcomm = MPI_COMM_NULL;
        return OMPI_SUCCESS;
    }

    /*
     * Attempt to split the communicator based on topology class by iteratively
     * calling ompi_comm_split_type specifying the split type as
     * MPI_COMM_TYPE_HW_GUIDED using the next lower topology class until a
     * split results in a smaller size communicator than the input communicator.
     * The search starts with OMPI_COMM_TYPE_CU since that is the highest possible
     * topology class where the communicator size can be smaller than MPI_COMM_WORLD.
     */
    original_size = ompi_comm_size(unguided_comm);
    split_info = OBJ_NEW(opal_info_t);
    i = 1;
    while (NULL != ompi_comm_split_type_hw_guided_support[i].info_value) {
        /* MPI_COMM_TYPE_HW_GUIDED splits require mpi_hw_resource_type to be set */
        opal_info_set(split_info, "mpi_hw_resource_type",
                      ompi_comm_split_type_hw_guided_support[i].info_value);

        rc = ompi_comm_split_type_core(unguided_comm,
                                       ompi_comm_split_type_hw_guided_support[i].split_type,
                                       ompi_comm_split_type_hw_guided_support[i].split_type,
                                       key, need_split, no_reorder, no_undefined,
                                       split_info, newcomm);
        if (OMPI_SUCCESS != rc) {
            break;
        }

        // Everyone else will get a new communicator.
        // Check the size to see if we need to iterate again.
        new_size = ompi_comm_size(*newcomm);
        if (new_size < original_size) {
            /* If a valid info object was passed, set the selected topology */
            if (NULL != info) {
                opal_info_set(info, "mpi_hw_resource_type", 
                              ompi_comm_split_type_hw_guided_support[i].info_value);
            }
            ompi_comm_free(&unguided_comm);
            OBJ_RELEASE(split_info);
            return OMPI_SUCCESS;
        }

        // Free the new communicator, and prepare for the next iteration.
        ompi_comm_free(newcomm);
        i = i + 1;
    }

    ompi_comm_free(&unguided_comm);
    OBJ_RELEASE(split_info);
    *newcomm = MPI_COMM_NULL;
    return rc;
}

/*
 * ompi_comm_split_type: Performs a communicator split. This function performs initial
 *                       processing to set up a  MPI_COMM_TYPE_HW_GUIDED split and
 *                       validation of input parameters.
 * comm(in)              : Input communicator
 * split_type(in)        : Split type to be performed by this rank, may be MPI_UNDEFINED
 * key(in)               : Original key from the user
 * info(in/out)          : Info guiding the split operation
 * newcomm(out)          : Pointer to the newly created communicator, or pointer to MPI_COMM_NULL
 *                         if no communicator created.
 */                         
int ompi_comm_split_type (ompi_communicator_t *comm, int split_type, int key,
                          opal_info_t *info, ompi_communicator_t **newcomm)
{
    bool need_split = false, no_reorder = false, no_undefined = false;
    int inter;
    int global_split_type, global_orig_split_type, ok[2], tmp[6];
    int rc;
    int orig_split_type = split_type;
    int flag;
    opal_cstring_t *value = NULL;

    /* silence clang warning. newcomm should never be NULL */
    if (OPAL_UNLIKELY(NULL == newcomm)) {
        return OMPI_ERR_BAD_PARAM;
    }

    inter = OMPI_COMM_IS_INTER(comm);

    /* Step 0: Convert MPI_COMM_TYPE_HW_GUIDED to the internal type */
    if (MPI_COMM_TYPE_HW_GUIDED == split_type) {
        opal_info_get(info, "mpi_hw_resource_type", &value, &flag);
        /* If key is not in the 'info', then return MPI_COMM_NULL.
         * This is caught at the MPI interface level, but it doesn't hurt to
         * check it again.
         */
        if (!flag) {
            *newcomm = MPI_COMM_NULL;
            return OMPI_SUCCESS;
        }

        /* Verify the value associated with the "mpi_hw_resource_type" key
         * - is supported, and
         * - is the same value at all ranks
         *
         * If not supported, then return MPI_COMM_NULL.
         * If not the same at all ranks, throw an error.
         */
        flag = 0;
        for (int i = 0; NULL != ompi_comm_split_type_hw_guided_support[i].info_value; ++i) {
            if (0 == strncasecmp(value->string,
                                 ompi_comm_split_type_hw_guided_support[i].info_value,
                                 strlen(ompi_comm_split_type_hw_guided_support[i].info_value))) {
                split_type = ompi_comm_split_type_hw_guided_support[i].split_type;
                flag = 1;
                break;
            }
        }
        /* If not supported, then return MPI_COMM_NULL. */
        if (0 == flag) {
            *newcomm = MPI_COMM_NULL;
            return OMPI_SUCCESS;
        }
    }

    /* Step 1: verify all ranks have supplied the same value for split type. All split types
     * must be the same or MPI_UNDEFINED (which is negative). */
    tmp[0] = orig_split_type;
    tmp[1] = -orig_split_type;
    tmp[2] = key;
    tmp[3] = -key;
    /* For MPI_COMM_TYPE_HW_GUIDED, verify all ranks have supplied the same
     * split_type (represented by orig_split_type) and info 'value' (represented by split_type).
     *
     * For split_type != MPI_COMM_TYPE_HW_GUIDED then orig_split_type == split_type.
     */
    tmp[4] = split_type;
    tmp[5] = -split_type;

    rc = comm->c_coll->coll_allreduce (MPI_IN_PLACE, &tmp, 6, MPI_INT, MPI_MAX, comm,
                                      comm->c_coll->coll_allreduce_module);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    global_orig_split_type = tmp[0];
    global_split_type = tmp[4];

    if (tmp[0] != -tmp[1] || tmp[4] != -tmp[5] || inter) {
        /* at least one rank supplied a different split type check if our split_type is ok */
        ok[0] = (MPI_UNDEFINED == orig_split_type) || global_orig_split_type == orig_split_type;
        ok[1] = (MPI_UNDEFINED == orig_split_type) || global_split_type == split_type;

        rc = comm->c_coll->coll_allreduce (MPI_IN_PLACE, &ok, 2, MPI_INT, MPI_MIN, comm,
                                          comm->c_coll->coll_allreduce_module);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            return rc;
        }

        if (inter) {
            /* need an extra allreduce to ensure that all ranks have the same result */
            rc = comm->c_coll->coll_allreduce (MPI_IN_PLACE, &ok, 2, MPI_INT, MPI_MIN, comm,
                                              comm->c_coll->coll_allreduce_module);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                return rc;
            }
        }

        if (OPAL_UNLIKELY(!ok[0] || !ok[1])) {
            if (0 == ompi_comm_rank(comm)) {
                opal_info_get(info, "mpi_hw_resource_type", &value, &flag);
                if (!flag) {
                    value = NULL;
                }
                opal_show_help("help-comm.txt",
                               "mismatched-split_type-values",
                               true,
                               ompi_comm_split_type_to_str(orig_split_type),
                               orig_split_type,
                               NULL == value ? "" : value->string);
            }
            return OMPI_ERR_BAD_PARAM;
        }

        need_split = tmp[0] == -tmp[1];
    } else {
        /* intracommunicator and all ranks specified the same split type */
        no_undefined = true;
        /* check if all ranks specified the same key */
        no_reorder = tmp[2] == -tmp[3];
    }

    if (MPI_UNDEFINED == global_orig_split_type) {
        /* short-circut. every rank provided MPI_UNDEFINED */
        *newcomm = MPI_COMM_NULL;
        return OMPI_SUCCESS;
    }

    if (MPI_COMM_TYPE_HW_UNGUIDED == global_orig_split_type) {
        /* Handle MPI_COMM_TYPE_HW_UNGUIDED communicator split. */
        return ompi_comm_split_unguided( comm, split_type,
                                         key, need_split, no_reorder,
                                         no_undefined, info, newcomm );
    } else {
        return ompi_comm_split_type_core( comm, global_split_type, split_type,
                                          key, need_split, no_reorder,
                                          no_undefined, info, newcomm);
    }
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
int ompi_comm_dup ( ompi_communicator_t * comm, ompi_communicator_t **newcomm )
{
    return ompi_comm_dup_with_info (comm, NULL, newcomm);
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
int ompi_comm_dup_with_info ( ompi_communicator_t * comm, opal_info_t *info, ompi_communicator_t **newcomm )
{
    ompi_communicator_t *newcomp = NULL;
    ompi_group_t *remote_group = NULL;
    int mode = OMPI_COMM_CID_INTRA, rc = OMPI_SUCCESS;

    if ( OMPI_COMM_IS_INTER ( comm ) ){
        mode   = OMPI_COMM_CID_INTER;
        remote_group = comm->c_remote_group;
    }

    *newcomm = MPI_COMM_NULL;

    rc =  ompi_comm_set ( &newcomp,                               /* new comm */
                          comm,                                   /* old comm */
                          0,                                      /* local array size */
                          NULL,                                   /* local_procs*/
                          0,                                      /* remote array size */
                          NULL,                                   /* remote_procs */
                          comm->c_keyhash,                        /* attrs */
                          comm->error_handler,                    /* error handler */
                          comm->c_local_group,                    /* local group */
                          remote_group,                           /* remote group */
                          OMPI_COMM_SET_FLAG_COPY_TOPOLOGY);      /* flags */
    if ( OMPI_SUCCESS != rc) {
        return rc;
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid (newcomp, comm, NULL, NULL, NULL, false, mode);
    if ( OMPI_SUCCESS != rc ) {
        OBJ_RELEASE(newcomp);
        return rc;
    }

    /* Set name for debugging purposes */
    snprintf(newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI COMM %s DUP FROM %s",
	     ompi_comm_print_cid (newcomp), ompi_comm_print_cid (comm));

    // Copy info if there is one.
    ompi_comm_assert_subscribe (newcomp, OMPI_COMM_ASSERT_LAZY_BARRIER);
    ompi_comm_assert_subscribe (newcomp, OMPI_COMM_ASSERT_ACTIVE_POLL);
    if (info) {
        opal_infosubscribe_change_info(&newcomp->super, info);
    }

    /* activate communicator and init coll-module */
    rc = ompi_comm_activate (&newcomp, comm, NULL, NULL, NULL, false, mode);
    if ( OMPI_SUCCESS != rc ) {
        OBJ_RELEASE(newcomp);
        return rc;
    }

    /* MPI-4 §7.4.4 requires us to remove all unknown keys from the info object */
    opal_info_remove_unreferenced(newcomp->super.s_info);

    *newcomm = newcomp;
    return MPI_SUCCESS;
}

struct ompi_comm_idup_with_info_context_t {
    opal_object_t super;
    ompi_communicator_t *comm;
    ompi_communicator_t *newcomp;
};

typedef struct ompi_comm_idup_with_info_context_t ompi_comm_idup_with_info_context_t;
OBJ_CLASS_INSTANCE(ompi_comm_idup_with_info_context_t, opal_object_t, NULL, NULL);

static int ompi_comm_idup_with_info_activate (ompi_comm_request_t *request);
static int ompi_comm_idup_with_info_finish (ompi_comm_request_t *request);
static int ompi_comm_idup_getcid (ompi_comm_request_t *request);

int ompi_comm_idup (ompi_communicator_t *comm, ompi_communicator_t **newcomm, ompi_request_t **req)
{
    return ompi_comm_idup_with_info (comm, NULL, newcomm, req);
}

int ompi_comm_idup_with_info (ompi_communicator_t *comm, opal_info_t *info, ompi_communicator_t **newcomm, ompi_request_t **req)
{
    return ompi_comm_idup_internal (comm, comm->c_local_group, comm->c_remote_group, info, newcomm, req);
}

/* NTH: we need a way to idup with a smaller local group so this function takes a local group */
static int ompi_comm_idup_internal (ompi_communicator_t *comm, ompi_group_t *group, ompi_group_t *remote_group,
                                    opal_info_t *info, ompi_communicator_t **newcomm, ompi_request_t **req)
{
    ompi_comm_idup_with_info_context_t *context;
    ompi_comm_request_t *request;
    ompi_request_t *subreq[1];
    int rc;

    *newcomm = MPI_COMM_NULL;

    if (!OMPI_COMM_IS_INTER (comm)){
        remote_group = NULL;
    }

    request = ompi_comm_request_get ();
    if (NULL == request) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    context = OBJ_NEW(ompi_comm_idup_with_info_context_t);
    if (NULL == context) {
        ompi_comm_request_return (request);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    context->comm    = comm;

    request->context = &context->super;
    request->super.req_mpi_object.comm = comm;

    rc =  ompi_comm_set_nb (&context->newcomp,                      /* new comm */
                            comm,                                   /* old comm */
                            0,                                      /* local array size */
                            NULL,                                   /* local_procs */
                            0,                                      /* remote array size */
                            NULL,                                   /* remote_procs */
                            comm->c_keyhash,                        /* attrs */
                            comm->error_handler,                    /* error handler */
                            group,                                  /* local group */
                            remote_group,                           /* remote group */
                            OMPI_COMM_SET_FLAG_COPY_TOPOLOGY,       /* flags */
                            subreq);                                /* new subrequest */
    if (OMPI_SUCCESS != rc) {
        ompi_comm_request_return (request);
        return rc;
    }

    // Copy info if there is one.
    {
        ompi_communicator_t *newcomp = context->newcomp;
        newcomp->super.s_info = OBJ_NEW(opal_info_t);
        if (info) {
            opal_info_dup(info, &(newcomp->super.s_info));
        }
    }

    ompi_comm_request_schedule_append (request, ompi_comm_idup_getcid, subreq, subreq[0] ? 1 : 0);

    /* assign the newcomm now */
    *newcomm = context->newcomp;

    /* kick off the request */
    ompi_comm_request_start (request);
    *req = &request->super;

    return OMPI_SUCCESS;
}

static int ompi_comm_idup_getcid (ompi_comm_request_t *request)
{
    ompi_comm_idup_with_info_context_t *context =
        (ompi_comm_idup_with_info_context_t *) request->context;
    ompi_request_t *subreq[1];
    int rc, mode;

    if (OMPI_COMM_IS_INTER(context->comm)){
        mode  = OMPI_COMM_CID_INTER;
    } else {
        mode  = OMPI_COMM_CID_INTRA;
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid_nb (context->newcomp, context->comm, NULL, NULL,
                               NULL, false, mode, subreq);
    if (OMPI_SUCCESS != rc) {
        ompi_comm_request_return (request);
        OBJ_RELEASE(context->newcomp);
        return rc;
    }

    ompi_comm_request_schedule_append (request, ompi_comm_idup_with_info_activate, subreq, 1);

    return OMPI_SUCCESS;
}

static int ompi_comm_idup_with_info_activate (ompi_comm_request_t *request)
{
    ompi_comm_idup_with_info_context_t *context =
        (ompi_comm_idup_with_info_context_t *) request->context;
    ompi_request_t *subreq[1];
    int rc, mode;

    if (OMPI_COMM_IS_INTER(context->comm)){
        mode  = OMPI_COMM_CID_INTER;
    } else {
        mode  = OMPI_COMM_CID_INTRA;
    }

    /* Set name for debugging purposes */
    snprintf(context->newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI COMM %s DUP FROM %s",
	     ompi_comm_print_cid (context->newcomp), ompi_comm_print_cid (context->comm));

    /* activate communicator and init coll-module */
    rc = ompi_comm_activate_nb (&context->newcomp, context->comm, NULL, NULL, NULL, false, mode, subreq);
    if ( OMPI_SUCCESS != rc ) {
        OBJ_RELEASE(context->newcomp);
        return rc;
    }

    ompi_comm_request_schedule_append (request, ompi_comm_idup_with_info_finish, subreq, 1);

    return OMPI_SUCCESS;
}

static int ompi_comm_idup_with_info_finish (ompi_comm_request_t *request)
{
    ompi_comm_idup_with_info_context_t *context =
        (ompi_comm_idup_with_info_context_t *) request->context;
    /* MPI-4 §7.4.4 requires us to remove all unknown keys from the info object */
    opal_info_remove_unreferenced(context->newcomp->super.s_info);

    /* done */
    return MPI_SUCCESS;
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
int ompi_comm_create_group (ompi_communicator_t *comm, ompi_group_t *group, int tag, ompi_communicator_t **newcomm)
{
    ompi_communicator_t *newcomp = NULL;
    int mode = OMPI_COMM_CID_GROUP, rc = OMPI_SUCCESS;

    *newcomm = MPI_COMM_NULL;

    rc =  ompi_comm_set ( &newcomp,                               /* new comm */
                          comm,                                   /* old comm */
                          group->grp_proc_count,                  /* local_size */
                          NULL,                                   /* local_procs*/
                          0,                                      /* remote_size */
                          NULL,                                   /* remote_procs */
                          comm->c_keyhash,                        /* attrs */
                          comm->error_handler,                    /* error handler */
                          group,                                  /* local group */
                          NULL,                                   /* remote group */
                          OMPI_COMM_SET_FLAG_COPY_TOPOLOGY);      /* flags */
    if ( OMPI_SUCCESS != rc) {
        return rc;
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid (newcomp, comm, NULL, &tag, NULL, false, mode);
    if ( OMPI_SUCCESS != rc ) {
        OBJ_RELEASE(newcomp);
        return rc;
    }

    /* Set name for debugging purposes */
    snprintf(newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI COMM %s GROUP FROM %s",
	     ompi_comm_print_cid (newcomp), ompi_comm_print_cid (comm));

    /* activate communicator and init coll-module */
    rc = ompi_comm_activate (&newcomp, comm, NULL, &tag, NULL, false, mode);
    if ( OMPI_SUCCESS != rc ) {
        OBJ_RELEASE(newcomp);
        return rc;
    }

    *newcomm = newcomp;
    return MPI_SUCCESS;
}

int ompi_comm_create_from_group (ompi_group_t *group, const char *tag, opal_info_t *info,
                                 ompi_errhandler_t *errhandler, ompi_communicator_t **newcomm)
{
    ompi_communicator_t *newcomp = NULL;
    int rc;

    *newcomm = MPI_COMM_NULL;

    rc = ompi_comm_set_simple (&newcomp, errhandler, group);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid (newcomp, NULL, NULL, (void *) tag, NULL, false,
                            OMPI_COMM_CID_GROUP_NEW);
    if ( OMPI_SUCCESS != rc ) {
        return rc;
    }

    /* Set name for debugging purposes */
    snprintf(newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI COMM %s FROM GROUP",
	     ompi_comm_print_cid (newcomp));

    newcomp->super.s_info = OBJ_NEW(opal_info_t);
    if (NULL == newcomp->super.s_info) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* activate communicator and init coll-module. use the group allreduce implementation as
     * no collective module has yet been selected. the tag does not matter as any tag will
     * be unique on the new communicator. */
    rc = ompi_comm_activate (&newcomp, newcomp, NULL, &(int) {0xfeed}, NULL,
                             false, OMPI_COMM_CID_GROUP);
    if ( OMPI_SUCCESS != rc ) {
        return rc;
    }

    newcomp->instance = group->grp_instance;

    /*
     * setup predefined keyvals - see MPI Standard for predefined keyvals cached on 
     * communicators created via MPI_Comm_from_group or MPI_Intercomm_create_from_groups
     */
    ompi_attr_hash_init(&newcomp->c_keyhash);
    ompi_attr_set_int(COMM_ATTR,
                      newcomp,
                      &newcomp->c_keyhash,
                      MPI_TAG_UB, mca_pml.pml_max_tag,
                      true);

    *newcomm = newcomp;
    return MPI_SUCCESS;
}

int ompi_intercomm_create (ompi_communicator_t *local_comm, int local_leader, ompi_communicator_t *bridge_comm,
                           int remote_leader, int tag, ompi_communicator_t **newintercomm)
{
    int local_size = 0, local_rank = 0, lleader = 0, rleader = 0, rc, rsize = 0;
    struct ompi_proc_t **rprocs;
    ompi_communicator_t *newcomp;
    ompi_group_t *new_group_pointer;

    *newintercomm = MPI_COMM_NULL;

    local_size = ompi_comm_size ( local_comm );
    local_rank = ompi_comm_rank ( local_comm );
    lleader = local_leader;
    rleader = remote_leader;

    if ( MPI_PARAM_CHECK ) {
        if ( (0 > local_leader) || (local_leader >= local_size) ) {
            return OMPI_ERR_BAD_PARAM;
        }

        /* remember that the remote_leader and bridge_comm arguments
           just have to be valid at the local_leader */
        if ( local_rank == local_leader ) {
            if (ompi_comm_invalid (bridge_comm) || (bridge_comm->c_flags & OMPI_COMM_INTER)) {
                return MPI_ERR_COMM;
            }

            if ((remote_leader < 0) || (remote_leader >= ompi_comm_size(bridge_comm))) {
                return OMPI_ERR_BAD_PARAM;
            }
        } /* if ( local_rank == local_leader ) */
    }

    if (local_rank == local_leader) {
        MPI_Request req;

        /* local leader exchange group sizes lists */
        rc = MCA_PML_CALL(irecv (&rsize, 1, MPI_INT, rleader, tag, bridge_comm, &req));
        if ( rc != MPI_SUCCESS ) {
            return rc;
        }
        rc = MCA_PML_CALL(send (&local_size, 1, MPI_INT, rleader, tag,
                                MCA_PML_BASE_SEND_STANDARD, bridge_comm));
        if ( rc != MPI_SUCCESS ) {
            return rc;
        }
        rc = ompi_request_wait (&req, MPI_STATUS_IGNORE);
        if ( rc != MPI_SUCCESS ) {
            return rc;
        }
    }

    /* bcast size and list of remote processes to all processes in local_comm */
    rc = local_comm->c_coll->coll_bcast (&rsize, 1, MPI_INT, lleader, local_comm,
                                         local_comm->c_coll->coll_bcast_module);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    rc = ompi_comm_get_rprocs (local_comm, bridge_comm, lleader, remote_leader, tag, rsize, &rprocs);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    /* put group elements in the list */
    new_group_pointer = ompi_group_allocate_plist_w_procs (local_comm->c_local_group, rprocs, rsize);
    if (OPAL_UNLIKELY(NULL == new_group_pointer)) {
        free (rprocs);
        return MPI_ERR_GROUP;
    }

    if (MPI_PARAM_CHECK) {
        bool overlap = ompi_group_overlap (local_comm->c_local_group, new_group_pointer);
        if (overlap && MPI_THREAD_MULTIPLE != ompi_mpi_thread_provided) {
            ompi_group_free (&new_group_pointer);
            return OMPI_ERR_BAD_PARAM;
        }
    }

    rc = ompi_comm_set (&newcomp,                                     /* new comm */
                        local_comm,                                   /* old comm */
                        local_comm->c_local_group->grp_proc_count,    /* local_size */
                        NULL,                                         /* local_procs*/
                        rsize,                                        /* remote_size */
                        NULL,                                         /* remote_procs */
                        NULL,                                         /* attrs */
                        local_comm->error_handler,                    /* error handler*/
                        local_comm->c_local_group,                    /* local group */
                        new_group_pointer,                            /* remote group */
                        0);                                           /* flags */

    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        ompi_group_free (&new_group_pointer);
        return rc;
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid (newcomp, local_comm, bridge_comm, &lleader,
                            &rleader, false, OMPI_COMM_CID_INTRA_BRIDGE);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        ompi_comm_free (&newcomp);
        return rc;
    }

    /* activate comm and init coll-module */
    rc = ompi_comm_activate (&newcomp, local_comm, bridge_comm, &lleader, &rleader,
                             false, OMPI_COMM_CID_INTRA_BRIDGE);
    if ( MPI_SUCCESS != rc ) {
        ompi_comm_free (&newcomp);
        return rc;
    }

    *newintercomm = newcomp;

    return OMPI_SUCCESS;
}

int ompi_intercomm_create_from_groups (ompi_group_t *local_group, int local_leader,
                                       ompi_group_t *remote_group, int remote_leader, const char *tag,
                                       opal_info_t *info, ompi_errhandler_t *errhandler,
                                       ompi_communicator_t **newintercomm)
{
    ompi_communicator_t *newcomp = NULL, *local_comm, *leader_comm = MPI_COMM_NULL;
    ompi_comm_extended_cid_block_t new_block;
    bool i_am_leader = local_leader == local_group->grp_my_rank;
    ompi_proc_t **rprocs;
    uint64_t data[4];
    int leader_comm_remote_leader;
    char *sub_tag = NULL;
    size_t rsize;
    int rc;

    *newintercomm = MPI_COMM_NULL;

    /* create a local communicator first. create a unique tag for this communicator */
    opal_asprintf (&sub_tag, "%s-OMPIi-%s", tag, OPAL_NAME_PRINT(ompi_group_get_proc_name (local_group, local_leader)));
    if (OPAL_UNLIKELY(NULL == sub_tag)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    rc = ompi_comm_create_from_group (local_group, sub_tag, info, errhandler, &local_comm);
    free (sub_tag);
    sub_tag = NULL;
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    if (i_am_leader) {
        /* create a bridge communicator for the leaders (so we can use the existing collectives
         * for activation). there are probably more efficient ways to do this but for intercommunicator
         * creation is not considered a performance critical operation. */
        ompi_proc_t **leader_procs, *my_proc;
        ompi_group_t *leader_group;

        leader_procs = calloc (2, sizeof (*leader_procs));

        my_proc = leader_procs[0] = ompi_group_get_proc_ptr (local_group, local_leader, true);
        leader_procs[1] = ompi_group_get_proc_ptr (remote_group, remote_leader, true);

        if (leader_procs[0] != leader_procs[1]) {
            /* NTH: they are definitely different (can the ever be the same) */
            if (leader_procs[0]->super.proc_name.jobid > leader_procs[1]->super.proc_name.jobid ||
                (leader_procs[0]->super.proc_name.jobid == leader_procs[1]->super.proc_name.jobid &&
                 leader_procs[0]->super.proc_name.vpid > leader_procs[1]->super.proc_name.vpid)) {
                ompi_proc_t *tmp = leader_procs[0];
                leader_procs[0] = leader_procs[1];
                leader_procs[1] = tmp;
            }

            /* create a unique tag for allocating the leader communicator. we can eliminate this step
             * if we take a CID from the newly allocated block belonging to local_comm. this is
             * a note to make this change at a later time. */
            opal_asprintf (&sub_tag, "%s-OMPIi-LC", tag);
            if (OPAL_UNLIKELY(NULL == sub_tag)) {
                ompi_comm_free (&local_comm);
                free(leader_procs);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            leader_group = ompi_group_allocate_plist_w_procs (NULL, leader_procs, 2);
            ompi_set_group_rank (leader_group, my_proc);
            if (OPAL_UNLIKELY(NULL == leader_group)) {
                free (sub_tag);
                free(leader_procs);
                ompi_comm_free (&local_comm);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            /* remote leader is whichever rank I am not */
            leader_comm_remote_leader = !(leader_group->grp_my_rank);

            rc = ompi_comm_create_from_group (leader_group, sub_tag, info, errhandler, &leader_comm);
            OBJ_RELEASE(leader_group);
            free (sub_tag);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                free(leader_procs);
                ompi_comm_free (&local_comm);
                return rc;
            }

            /* grab a CID for the intercomm while we are at it */
            ompi_comm_extended_cid_block_new (&leader_comm->c_contextidb, &new_block, false);

            data[0] = remote_group->grp_proc_count;
            /* store the relevant new_block data */
            data[1] = new_block.block_cid.cid_base;
            data[2] = new_block.block_cid.cid_sub.u64;
            data[3] = new_block.block_level;
        } else {
            free (leader_procs);
        }
    }

    /* bcast size and list of remote processes to all processes in local_comm */
    rc = local_comm->c_coll->coll_bcast (data, 4, MPI_UINT64_T, local_leader, local_comm,
                                         local_comm->c_coll->coll_bcast_module);
    rsize = data[0];
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        ompi_comm_free (&local_comm);
        return rc;
    }

    /* using 0 for the tag because we control both local_comm and leader_comm */
    rc = ompi_comm_get_rprocs (local_comm, leader_comm, local_leader, leader_comm_remote_leader, 0, rsize, &rprocs);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        ompi_comm_free (&local_comm);
        return rc;
    }

    if (!i_am_leader) {
        /* create a new group containing the remote processes for non-leader ranks */
        remote_group = ompi_group_allocate_plist_w_procs (local_group, rprocs, rsize);
        if (OPAL_UNLIKELY(NULL == remote_group)) {
            free (rprocs);
            ompi_comm_free (&local_comm);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    } else {
        OBJ_RETAIN(remote_group);
    }

    rc = ompi_comm_set (&newcomp, local_comm, local_group->grp_proc_count, NULL, remote_group->grp_proc_count,
                        NULL, NULL, errhandler, local_group, remote_group, OMPI_COMM_SET_FLAG_LOCAL_COMM_NODUP);
    OBJ_RELEASE(remote_group);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        ompi_comm_free (&local_comm);
        return rc;
    }

    /* will be using a communicator ID derived from the bridge communicator to save some time */
    new_block.block_cid.cid_base = data[1];
    new_block.block_cid.cid_sub.u64 = data[2];
    new_block.block_nextsub = 0;
    new_block.block_nexttag = 0;
    new_block.block_level = (int8_t) data[3];

    rc = ompi_comm_nextcid (newcomp, NULL, NULL, (void *) tag, &new_block, false, OMPI_COMM_CID_GROUP_NEW);
    if ( OMPI_SUCCESS != rc ) {
        OBJ_RELEASE(newcomp);
        return rc;
    }

    /* Set name for debugging purposes */
    snprintf(newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI INTERCOMM %s FROM GROUP", ompi_comm_print_cid (newcomp));

    // Copy info if there is one.
    newcomp->super.s_info = OBJ_NEW(opal_info_t);
    if (info) {
        opal_info_dup(info, &(newcomp->super.s_info));
    }

    /* activate communicator and init coll-module */
    rc = ompi_comm_activate (&newcomp, local_comm, leader_comm, &local_leader, &leader_comm_remote_leader,
                             false, OMPI_COMM_CID_INTRA_BRIDGE);
    if (MPI_COMM_NULL != leader_comm) {
        ompi_comm_free (&leader_comm);
    }

    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        ompi_comm_free (&newcomp);
        return rc;
    }

    *newintercomm = newcomp;

    return MPI_SUCCESS;
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
int ompi_comm_compare(ompi_communicator_t *comm1, ompi_communicator_t *comm2, int *result) {
    /* local variables */
    ompi_communicator_t *comp1, *comp2;
    int size1, size2, rsize1, rsize2;
    int lresult, rresult=MPI_CONGRUENT;
    int cmp_result;

    if (comm1->instance != comm2->instance) {
        return OMPI_ERR_BAD_PARAM;
    }

    comp1 = (ompi_communicator_t *) comm1;
    comp2 = (ompi_communicator_t *) comm2;

    if (ompi_comm_compare_cids(comp1,comp2)) {
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
    ompi_group_compare((ompi_group_t *)comp1->c_local_group,
                       (ompi_group_t *)comp2->c_local_group,
                       &cmp_result);

    /* MPI_IDENT resulting from the group comparison is
     * MPI_CONGRUENT for communicators.
     * All others results are the same.
     */
    if( MPI_IDENT == cmp_result ) {
        lresult = MPI_CONGRUENT;
    } else {
        lresult = cmp_result;
    }


    if ( rsize1 > 0 ) {
        /* Compare remote groups for inter-communicators */
        ompi_group_compare((ompi_group_t *)comp1->c_remote_group,
                           (ompi_group_t *)comp2->c_remote_group,
                           &cmp_result);

        /* MPI_IDENT resulting from the group comparison is
         * MPI_CONGRUENT for communicators.
         * All others results are the same.
         */
        if( MPI_IDENT == cmp_result ) {
            rresult = MPI_CONGRUENT;
        } else {
            rresult = cmp_result;
        }
    }

    /* determine final results */
    if ( MPI_CONGRUENT == rresult ) {
        *result = lresult;
    }
    else if ( MPI_SIMILAR == rresult ) {
        if ( MPI_SIMILAR == lresult || MPI_CONGRUENT == lresult ) {
            *result = MPI_SIMILAR;
        }
        else {
            *result = MPI_UNEQUAL;
        }
    }
    else if ( MPI_UNEQUAL == rresult ) {
        *result = MPI_UNEQUAL;
    }

    return OMPI_SUCCESS;
}
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
int ompi_comm_set_name (ompi_communicator_t *comm, const char *name )
{

    OPAL_THREAD_LOCK(&(comm->c_lock));
    opal_string_copy(comm->c_name, name, MPI_MAX_OBJECT_NAME);
    comm->c_flags |= OMPI_COMM_NAMEISSET;
    OPAL_THREAD_UNLOCK(&(comm->c_lock));

    return OMPI_SUCCESS;
}
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/*
 * Implementation of MPI_Allgather for the local_group in an inter-comm.
 * The algorithm consists of two steps:
 * 1. an inter-gather to rank 0 in remote group
 * 2. an inter-bcast from rank 0 in remote_group.
 */

static int ompi_comm_allgather_emulate_intra( void *inbuf, int incount,
                                              MPI_Datatype intype, void* outbuf,
                                              int outcount, MPI_Datatype outtype,
                                              ompi_communicator_t *comm,
                                              mca_coll_base_module_t *data)
{
    int rank, size, rsize, i, rc;
    int *tmpbuf=NULL;
    MPI_Request *req=NULL, sendreq;

    rsize = ompi_comm_remote_size(comm);
    size  = ompi_comm_size(comm);
    rank  = ompi_comm_rank(comm);

    /* silence clang warning about 0-byte malloc. neither of these values can
     * be 0 here */
    if (OPAL_UNLIKELY(0 == rsize || 0 == outcount)) {
        return OMPI_ERR_BAD_PARAM;
    }

    /* Step 1: the gather-step */
    if ( 0 == rank ) {
        tmpbuf = (int *) malloc (rsize*outcount*sizeof(int));
        if ( NULL == tmpbuf ) {
            return (OMPI_ERR_OUT_OF_RESOURCE);
        }
        req = (MPI_Request *)malloc (rsize*outcount*sizeof(MPI_Request));
        if ( NULL == req ) {
            free ( tmpbuf );
            return (OMPI_ERR_OUT_OF_RESOURCE);
        }

        for ( i=0; i<rsize; i++) {
            rc = MCA_PML_CALL(irecv( &tmpbuf[outcount*i], outcount, outtype, i,
                                     OMPI_COMM_ALLGATHER_TAG, comm, &req[i] ));
            if ( OMPI_SUCCESS != rc ) {
                goto exit;
            }
        }
    }
    rc = MCA_PML_CALL(isend( inbuf, incount, intype, 0, OMPI_COMM_ALLGATHER_TAG,
                             MCA_PML_BASE_SEND_STANDARD, comm, &sendreq ));
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    if ( 0 == rank ) {
        rc = ompi_request_wait_all( rsize, req, MPI_STATUSES_IGNORE);
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }
    }

    rc = ompi_request_wait( &sendreq, MPI_STATUS_IGNORE);
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    /* Step 2: the inter-bcast step */
    rc = MCA_PML_CALL(irecv (outbuf, size*outcount, outtype, 0,
                             OMPI_COMM_ALLGATHER_TAG, comm, &sendreq));
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    if ( 0 == rank ) {
        for ( i=0; i < rsize; i++ ){
            rc = MCA_PML_CALL(send (tmpbuf, rsize*outcount, outtype, i,
                                    OMPI_COMM_ALLGATHER_TAG,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
            if ( OMPI_SUCCESS != rc ) {
                goto exit;
            }
        }
    }

    rc = ompi_request_wait( &sendreq, MPI_STATUS_IGNORE );

 exit:
    if ( NULL != req ) {
        free ( req );
    }
    if ( NULL != tmpbuf ) {
        free ( tmpbuf );
    }

    return (rc);
}
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/*
** Counterpart to MPI_Comm_free. To be used within OMPI.
** The freeing of all attached objects (groups, errhandlers
** etc. ) has moved to the destructor.
*/
int ompi_comm_free( ompi_communicator_t **comm )
{
    int ret;
    int cid = (*comm)->c_index;
    int is_extra_retain = OMPI_COMM_IS_EXTRA_RETAIN(*comm);

    /* Release attributes.  We do this now instead of during the
       communicator destructor for 2 reasons:

       1. The destructor will only NOT be called immediately during
       ompi_comm_free() if the reference count is still greater
       than zero at that point, meaning that there are ongoing
       communications.  However, pending communications will never
       need attributes, so it's safe to release them directly here.

       2. Releasing attributes in ompi_comm_free() enables us to check
       the return status of the attribute delete functions.  At
       least one interpretation of the MPI standard (i.e., the one
       of the Intel test suite) is that if any of the attribute
       deletion functions fail, then MPI_COMM_FREE /
       MPI_COMM_DISCONNECT should also fail.  We can't do that if
       we delay releasing the attributes -- we need to release the
       attributes right away so that we can report the error right
       away. */
    if (NULL != (*comm)->c_keyhash) {
        ret = ompi_attr_delete_all(COMM_ATTR, *comm, (*comm)->c_keyhash);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        OBJ_RELEASE((*comm)->c_keyhash);
    }

    if ( OMPI_COMM_IS_INTER(*comm) ) {
        if ( ! OMPI_COMM_IS_INTRINSIC((*comm)->c_local_comm)) {
            ompi_comm_free (&(*comm)->c_local_comm);
        }
    }

    /* Special case: if we are freeing the parent handle, then we need
       to set our internal handle to the parent to be equal to
       COMM_NULL.  This is according to MPI-2:88-89. */

    if (*comm == ompi_mpi_comm_parent && comm != &ompi_mpi_comm_parent) {
        ompi_mpi_comm_parent = &ompi_mpi_comm_null.comm;
    }

    if (NULL != ((*comm)->super.s_info)) {
        OBJ_RELEASE((*comm)->super.s_info);
    }

    /* Release the communicator */
    if ( OMPI_COMM_IS_DYNAMIC (*comm) ) {
        ompi_comm_num_dyncomm --;
    }
    OBJ_RELEASE( (*comm) );

    if ( is_extra_retain) {
        /* This communicator has been marked as an "extra retain"
         * communicator. This can happen if a communicator creates
         * 'dependent' subcommunicators (e.g. for inter
         * communicators or when using hierarch collective
         * module *and* the cid of the dependent communicator
         * turned out to be lower than of the parent one.
         * In that case, the reference counter has been increased
         * by one more, in order to handle the scenario,
         * that the user did not free the communicator.
         * Note, that if we enter this routine, we can
         * decrease the counter by one more therefore. However,
         * in ompi_comm_finalize, we only used OBJ_RELEASE instead
         * of ompi_comm_free(), and the increased reference counter
         * makes sure that the pointer to the dependent communicator
         * still contains a valid object.
         */
        ompi_communicator_t *tmpcomm = (ompi_communicator_t *) opal_pointer_array_get_item(&ompi_mpi_communicators, cid);
        if ( NULL != tmpcomm ){
            ompi_comm_free(&tmpcomm);
        }
    }

    *comm = MPI_COMM_NULL;
    return OMPI_SUCCESS;
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/**
 * This is a short-hand routine used in intercomm_create.
 * The routine makes sure, that all processes have afterwards
 * a list of ompi_proc_t pointers for the remote group.
 */
int ompi_comm_get_rprocs (ompi_communicator_t *local_comm, ompi_communicator_t *bridge_comm,
			  int local_leader, int remote_leader, int tag, int rsize, ompi_proc_t ***prprocs)
{
    MPI_Request req;
    int rc = OMPI_SUCCESS;
    int local_rank, local_size;
    ompi_proc_t **rprocs=NULL;
    size_t size_len;
    int int_len=0, rlen;
    pmix_data_buffer_t *sbuf=NULL, *rbuf=NULL;
    char *sendbuf=NULL;
    char *recvbuf;
    ompi_proc_t **proc_list = NULL;
    int i;

    local_rank = ompi_comm_rank (local_comm);
    local_size = ompi_comm_size (local_comm);

    if (local_rank == local_leader) {
        PMIX_DATA_BUFFER_CREATE(sbuf);
        if (NULL == sbuf) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto err_exit;
        }
        if(OMPI_GROUP_IS_DENSE(local_comm->c_local_group)) {
            rc = ompi_proc_pack(local_comm->c_local_group->grp_proc_pointers,
                                local_size, sbuf);
        }
        /* get the proc list for the sparse implementations */
        else {
            proc_list = (ompi_proc_t **) calloc (local_comm->c_local_group->grp_proc_count,
                                                 sizeof (ompi_proc_t *));
            for(i=0 ; i<local_comm->c_local_group->grp_proc_count ; i++)
                proc_list[i] = ompi_group_peer_lookup(local_comm->c_local_group,i);
            rc = ompi_proc_pack (proc_list, local_size, sbuf);
        }
        if ( OMPI_SUCCESS != rc ) {
            goto err_exit;
        }
        PMIX_DATA_BUFFER_UNLOAD(sbuf, sendbuf, size_len);
        if (NULL == sendbuf) {
            rc = OMPI_ERR_UNPACK_FAILURE;
            goto err_exit;
        }

        /* send the remote_leader the length of the buffer */
        rc = MCA_PML_CALL(irecv (&rlen, 1, MPI_INT, remote_leader, tag,
                                 bridge_comm, &req ));
        if ( OMPI_SUCCESS != rc ) {
            goto err_exit;
        }
        int_len = (int)size_len;

        rc = MCA_PML_CALL(send (&int_len, 1, MPI_INT, remote_leader, tag,
                                MCA_PML_BASE_SEND_STANDARD, bridge_comm ));
        if ( OMPI_SUCCESS != rc ) {
            rlen = 0;  /* complete the recv and then the collectives */
        }
        rc = ompi_request_wait( &req, MPI_STATUS_IGNORE );
        if ( OMPI_SUCCESS != rc ) {
            rlen = 0;  /* participate in the collective and then done */
        }
    }

    /* broadcast buffer length to all processes in local_comm */
    rc = local_comm->c_coll->coll_bcast( &rlen, 1, MPI_INT,
                                        local_leader, local_comm,
                                        local_comm->c_coll->coll_bcast_module );
    if ( OMPI_SUCCESS != rc ) {
#if OPAL_ENABLE_FT_MPI
        if ( local_rank != local_leader ) {
            goto err_exit;
        }
        /* the leaders must go on in order to avoid deadlocks */
#else
        goto err_exit;
#endif  /* OPAL_ENABLE_FT_MPI */
    }

    /* Allocate temporary buffer */
    recvbuf = (char *)malloc(rlen);
    if ( NULL == recvbuf ) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto err_exit;
    }

    if ( local_rank == local_leader ) {
        /* local leader exchange name lists */
        rc = MCA_PML_CALL(irecv (recvbuf, rlen, MPI_BYTE, remote_leader, tag,
                                 bridge_comm, &req ));
        if ( OMPI_SUCCESS != rc ) {
            goto err_exit;
        }
        rc = MCA_PML_CALL(send(sendbuf, int_len, MPI_BYTE, remote_leader, tag,
                               MCA_PML_BASE_SEND_STANDARD, bridge_comm ));
#if OPAL_ENABLE_FT_MPI
        /* let it flow even if there are errors */
        if ( OMPI_SUCCESS != rc && MPI_ERR_PROC_FAILED != rc && MPI_ERR_REVOKED != rc ) {
#else
        if ( OMPI_SUCCESS != rc ) {
#endif
            goto err_exit;
        }

        rc = ompi_request_wait( &req, MPI_STATUS_IGNORE );
#if OPAL_ENABLE_FT_MPI
        /* let it flow even if there are errors */
        if ( OMPI_SUCCESS != rc && MPI_ERR_PROC_FAILED != rc && MPI_ERR_REVOKED != rc ) {
#else
        if ( OMPI_SUCCESS != rc ) {
#endif
            goto err_exit;
        }
        PMIX_DATA_BUFFER_RELEASE(sbuf);
    }

    /* broadcast name list to all processes in local_comm */
    rc = local_comm->c_coll->coll_bcast( recvbuf, rlen, MPI_BYTE,
                                        local_leader, local_comm,
                                        local_comm->c_coll->coll_bcast_module);
    if ( OMPI_SUCCESS != rc ) {
        goto err_exit;
    }

    PMIX_DATA_BUFFER_CREATE(rbuf);
    if (NULL == rbuf) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto err_exit;
    }

    PMIX_DATA_BUFFER_LOAD(rbuf, recvbuf, rlen);

    /* decode the names into a proc-list -- will never add a new proc
       as the result of this operation, so no need to get the newprocs
       list or call PML add_procs(). */
    rc = ompi_proc_unpack(rbuf, rsize, &rprocs, NULL, NULL);
    PMIX_DATA_BUFFER_RELEASE(rbuf);
    if (OMPI_SUCCESS != rc) {
        goto err_exit;
    }

    /* set the locality of the remote procs */
    for (i=0; i < rsize; i++) {
        /* get the locality information - all RTEs are required
         * to provide this information at startup */
        uint16_t *u16ptr, u16;
        u16ptr = &u16;
        OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_LOCALITY, &rprocs[i]->super.proc_name, &u16ptr, PMIX_UINT16);
        if (OPAL_SUCCESS == rc) {
            rprocs[i]->super.proc_flags = u16;
        } else {
            rprocs[i]->super.proc_flags = OPAL_PROC_NON_LOCAL;
        }
    }

    /* And now add the information into the database */
    if (OMPI_SUCCESS != (rc = MCA_PML_CALL(add_procs(rprocs, rsize)))) {
        goto err_exit;
    }

 err_exit:
    /* rprocs isn't freed unless we have an error,
       since it is used in the communicator */
    if ( OMPI_SUCCESS != rc ) {
        OMPI_ERROR_LOG(rc);
        opal_output(0, "%d: Error in ompi_get_rprocs\n", local_rank);
        if ( NULL != rprocs ) {
            free ( rprocs );
            rprocs=NULL;
        }
    }
    /* make sure the buffers have been released */
    if (NULL != sbuf) {
        PMIX_DATA_BUFFER_RELEASE(sbuf);
    }
    if (NULL != rbuf) {
        PMIX_DATA_BUFFER_RELEASE(rbuf);
    }
    if ( NULL != proc_list ) {
        free ( proc_list );
    }
    if (NULL != sendbuf) {
        free ( sendbuf );
    }

    *prprocs = rprocs;
    return rc;
}
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
int ompi_comm_determine_first ( ompi_communicator_t *intercomm, int high )
{
    int flag, rhigh;
    int rank, rsize;
    int *rcounts;
    int *rdisps;
    int scount=0;
    int rc;

    rank = ompi_comm_rank        (intercomm);
    rsize= ompi_comm_remote_size (intercomm);

    /* silence clang warnings. rsize can not be 0 here */
    if (OPAL_UNLIKELY(0 == rsize)) {
        return OMPI_ERR_BAD_PARAM;
    }

    rdisps  = (int *) calloc ( rsize, sizeof(int));
    if ( NULL == rdisps ){
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    rcounts = (int *) calloc ( rsize, sizeof(int));
    if ( NULL == rcounts ){
        free (rdisps);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    rcounts[0] = 1;
    if ( 0 == rank ) {
        scount = 1;
    }

    rc = intercomm->c_coll->coll_allgatherv(&high, scount, MPI_INT,
                                           &rhigh, rcounts, rdisps,
                                           MPI_INT, intercomm,
                                           intercomm->c_coll->coll_allgatherv_module);
    if ( NULL != rdisps ) {
        free ( rdisps );
    }
    if ( NULL != rcounts ) {
        free ( rcounts );
    }

    if ( rc != OMPI_SUCCESS ) {
        return rc;
    }

    /* This is the logic for determining who is first, who is second */
    if ( high && !rhigh ) {
        flag = false;
    }
    else if ( !high && rhigh ) {
        flag = true;
    }
    else {
        flag = ompi_comm_determine_first_auto(intercomm);
    }

    return flag;
}

int ompi_comm_determine_first_auto ( ompi_communicator_t* intercomm )
{
    ompi_proc_t *ourproc, *theirproc;
    ompi_rte_cmp_bitmask_t mask;
    int rc;

    ourproc   = ompi_group_peer_lookup(intercomm->c_local_group,0);
    theirproc = ompi_group_peer_lookup(intercomm->c_remote_group,0);

    mask = OMPI_RTE_CMP_JOBID | OMPI_RTE_CMP_VPID;
    rc = ompi_rte_compare_name_fields(mask, (const ompi_process_name_t*)&(ourproc->super.proc_name),
                                            (const ompi_process_name_t*)&(theirproc->super.proc_name));
    return (rc > 0);
}

/********************************************************************************/
/********************************************************************************/
/********************************************************************************/
int ompi_comm_dump ( ompi_communicator_t *comm )
{
    opal_output(0, "Dumping information for comm_cid %s\n", ompi_comm_print_cid (comm));
    opal_output(0,"  f2c index:%d cube_dim: %d\n", comm->c_f_to_c_index,
                comm->c_cube_dim);
    opal_output(0,"  Local group: size = %d my_rank = %d\n",
                comm->c_local_group->grp_proc_count,
                comm->c_local_group->grp_my_rank );

    opal_output(0,"  Communicator is:");
    /* Display flags */
    if ( OMPI_COMM_IS_INTER(comm) )
        opal_output(0," inter-comm,");
    if ( OMPI_COMM_IS_CART(comm))
        opal_output(0," topo-cart");
    else if ( OMPI_COMM_IS_GRAPH(comm))
        opal_output(0," topo-graph");
    else if ( OMPI_COMM_IS_DIST_GRAPH(comm))
        opal_output(0," topo-dist-graph");
     opal_output(0,"\n");

    if (OMPI_COMM_IS_INTER(comm)) {
        opal_output(0,"  Remote group size:%d\n", comm->c_remote_group->grp_proc_count);
    }
    return OMPI_SUCCESS;
}
/********************************************************************************/
/********************************************************************************/
/********************************************************************************/
/* static functions */
/*
** rankkeygidcompare() compares a tuple of (rank,key,gid) producing
** sorted lists that match the rules needed for a MPI_Comm_split
*/
static int rankkeycompare (const void *p, const void *q)
{
    int *a, *b;

    /* ranks at [0] key at [1] */
    /* i.e. we cast and just compare the keys and then the original ranks.. */
    a = (int*)p;
    b = (int*)q;

    /* simple tests are those where the keys are different */
    if (a[1] < b[1]) {
        return (-1);
    }
    if (a[1] > b[1]) {
        return (1);
    }

    /* ok, if the keys are the same then we check the original ranks */
    if (a[1] == b[1]) {
        if (a[0] < b[0]) {
            return (-1);
        }
        if (a[0] == b[0]) {
            return (0);
        }
        if (a[0] > b[0]) {
            return (1);
        }
    }
    return ( 0 );
}


/***********************************************************************
 * Counterpart of MPI_Cart/Graph_create. This will be called from the
 * top level MPI. The condition for INTER communicator is already
 * checked by the time this has been invoked. This function should do
 * somewhat the same things which ompi_comm_create does. It will
 * however select a component for topology and then call the
 * cart_create on that component so that it can re-arrange the proc
 * structure as required (if the reorder flag is true). It will then
 * use this proc structure to create the communicator using
 * ompi_comm_set.
 */

/**
 * Take an almost complete communicator and reserve the CID as well
 * as activate it (initialize the collective and the topologies).
 */
int ompi_comm_enable(ompi_communicator_t *old_comm,
                     ompi_communicator_t *new_comm,
                     int new_rank,
                     int num_procs,
                     ompi_proc_t** topo_procs)
{
    int ret = OMPI_SUCCESS;

    /* set the rank information before calling nextcid */
    new_comm->c_local_group->grp_my_rank = new_rank;
    new_comm->c_my_rank = new_rank;

    /* Determine context id. It is identical to f_2_c_handle */
    ret = ompi_comm_nextcid (new_comm, old_comm, NULL, NULL, NULL, false,
                             OMPI_COMM_CID_INTRA);
    if (OMPI_SUCCESS != ret) {
        /* something wrong happened while setting the communicator */
        goto complete_and_return;
    }

    /* Now, the topology module has been selected and the group
     * which has the topology information has been created. All we
     * need to do now is to fill the rest of the information into the
     * communicator. The following steps are not just similar to
     * ompi_comm_set, but are actually the same */

    ret = ompi_comm_fill_rest(new_comm,                /* the communicator */
                              num_procs,               /* local size */
                              topo_procs,              /* process structure */
                              new_rank,                /* rank of the process */
                              old_comm->error_handler); /* error handler */

    if (OMPI_SUCCESS != ret) {
        /* something wrong happened while setting the communicator */
        goto complete_and_return;
    }

    ret = ompi_comm_activate (&new_comm, old_comm, NULL, NULL, NULL, false,
                              OMPI_COMM_CID_INTRA);
    if (OMPI_SUCCESS != ret) {
        /* something wrong happened while setting the communicator */
        goto complete_and_return;
    }

 complete_and_return:
    return ret;
}

static int ompi_comm_fill_rest(ompi_communicator_t *comm,
                               int num_procs,
                               ompi_proc_t **proc_pointers,
                               int my_rank,
                               ompi_errhandler_t *errh)
{
    ompi_group_t *new_group;

    new_group = ompi_group_allocate_plist_w_procs(comm->c_local_group, proc_pointers, num_procs);

    /* properly decrement the ref counts on the groups.
       We are doing this because this function is sort of a redo
       of what is done in comm.c. No need to decrement the ref
       count on the proc pointers
       This is just a quick fix, and will be looking for a
       better solution */
    if (comm->c_local_group) {
        OBJ_RELEASE( comm->c_local_group );
    }

    if (comm->c_remote_group) {
        OBJ_RELEASE( comm->c_remote_group );
    }

    /* allocate a group structure for the new communicator */
    comm->c_local_group = new_group;

    /* set the remote group to be the same as local group */
    comm->c_remote_group = comm->c_local_group;
    OBJ_RETAIN( comm->c_remote_group );

    /* set the rank information */
    comm->c_local_group->grp_my_rank = my_rank;
    comm->c_my_rank = my_rank;

    if( MPI_UNDEFINED != my_rank ) {
        /* verify whether to set the flag, that this comm
           contains process from more than one jobid. */
        ompi_dpm_mark_dyncomm (comm);
    }

    /* set the error handler */
    comm->error_handler = errh;
    OBJ_RETAIN (comm->error_handler);

    /* set name for debugging purposes */
    /* there is no cid at this stage ... make this right and make edgars
     * code call this function and remove dupli cde
     */
    snprintf (comm->c_name, MPI_MAX_OBJECT_NAME, "MPI_COMMUNICATOR %s",
	      ompi_comm_print_cid (comm));

    /* determine the cube dimensions */
    comm->c_cube_dim = opal_cube_dim(comm->c_local_group->grp_proc_count);

    return OMPI_SUCCESS;
}

static int ompi_comm_copy_topo(ompi_communicator_t *oldcomm,
                               ompi_communicator_t *newcomm)
{
    if( NULL == oldcomm->c_topo )
        return OMPI_ERR_NOT_FOUND;

    newcomm->c_topo = oldcomm->c_topo;
    OBJ_RETAIN(newcomm->c_topo);
    newcomm->c_flags |= newcomm->c_topo->type;
    return OMPI_SUCCESS;
}

char *ompi_comm_print_cid (const ompi_communicator_t *comm)
{
#if OPAL_HAVE_THREAD_LOCAL
    static opal_thread_local char cid_buffer[2][20];
    static opal_thread_local int cid_buffer_index = 0;
#else
    /* no thread local == you get what you get. upgrade your compiler */
    static char cid_buffer[2][20];
    static int cid_buffer_index = 0;
#endif
    int bindex = cid_buffer_index;

    if (mca_pml_base_supports_extended_cid () && !OMPI_COMM_IS_GLOBAL_INDEX(comm)) {
	snprintf (cid_buffer[bindex], sizeof (cid_buffer[0]), "0x%" PRIx64 "%08" PRIx64,
	    comm->c_contextid.cid_base,
	    comm->c_contextid.cid_sub.u64);
    } else {
	snprintf (cid_buffer[bindex], sizeof (cid_buffer[0]), "%d", comm->c_index);
    }

    cid_buffer_index = cid_buffer_index ? 0 : 1;

    return cid_buffer[bindex];
}
