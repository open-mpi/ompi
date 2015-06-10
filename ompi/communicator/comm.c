/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2011 University of Houston. All rights reserved.
 * Copyright (c) 2007-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Inria.  All rights reserved.
 * Copyright (c) 2011-2013 Universite Bordeaux 1
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2012-2014 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
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
#include "opal/mca/dstore/dstore.h"
#include "opal/dss/dss.h"

#include "ompi/proc/proc.h"
#include "opal/threads/mutex.h"
#include "opal/util/bit_ops.h"
#include "opal/util/output.h"
#include "ompi/mca/topo/topo.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/mca/dpm/dpm.h"

#include "ompi/attribute/attribute.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/request/request.h"

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
static int ompi_comm_idup_internal (ompi_communicator_t *comm, ompi_group_t *group, ompi_info_t *info,
                                    ompi_communicator_t **newcomm, ompi_request_t **req);


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
                    bool copy_topocomponent, 
                    ompi_group_t *local_group, 
                    ompi_group_t *remote_group )
{
    ompi_request_t *req;
    int rc;

    rc = ompi_comm_set_nb (ncomm, oldcomm, local_size, local_ranks, remote_size, remote_ranks,
                           attr, errh, copy_topocomponent, local_group, remote_group, &req);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    if (NULL != req) {
        ompi_request_wait( &req, MPI_STATUS_IGNORE);
    }

    return OMPI_SUCCESS;
}

/*
 * if remote_group == &ompi_mpi_group_null, then the new communicator
 * is forced to be an inter communicator.
 */
int ompi_comm_set_nb ( ompi_communicator_t **ncomm,
                       ompi_communicator_t *oldcomm,
                       int local_size,
                       int *local_ranks,
                       int remote_size,
                       int *remote_ranks,
                       opal_hash_table_t *attr,
                       ompi_errhandler_t *errh,
                       bool copy_topocomponent,
                       ompi_group_t *local_group,
                       ompi_group_t *remote_group,
                       ompi_request_t **req )
{
    ompi_communicator_t *newcomm = NULL;
    int ret;

    *req = NULL;

    /* ompi_comm_allocate */
    newcomm = OBJ_NEW(ompi_communicator_t);
    /* fill in the inscribing hyper-cube dimensions */
    newcomm->c_cube_dim = opal_cube_dim(local_size);
    newcomm->c_id_available   = MPI_UNDEFINED;
    newcomm->c_id_start_index = MPI_UNDEFINED;

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
        ompi_group_increment_proc_count(newcomm->c_local_group);
    }
    newcomm->c_my_rank = newcomm->c_local_group->grp_my_rank;
    
    /* Set remote group and duplicate the local comm, if applicable */
    if (0 < remote_size) {
        if (NULL == remote_group || &ompi_mpi_group_null.group == remote_group) {
            ret = ompi_group_incl(oldcomm->c_remote_group, remote_size, 
                                  remote_ranks, &newcomm->c_remote_group);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                return ret;
            }
        } else {
            newcomm->c_remote_group = remote_group;
            OBJ_RETAIN(newcomm->c_remote_group);
            ompi_group_increment_proc_count(newcomm->c_remote_group);
        }
    }
    if (0 < remote_size || &ompi_mpi_group_null.group == remote_group) {
        newcomm->c_flags |= OMPI_COMM_INTER;
        if ( OMPI_COMM_IS_INTRA(oldcomm) ) {
            ompi_comm_idup(oldcomm, &newcomm->c_local_comm, req);
        } else if (NULL == local_group) {
            ompi_comm_idup(oldcomm->c_local_comm, &newcomm->c_local_comm, req);
        } else {
            /* NTH: use internal idup function that takes a local group argument */
            ompi_comm_idup_internal (oldcomm->c_local_comm, local_group, NULL,
                                     &newcomm->c_local_comm, req);
        }
    } else { 
        newcomm->c_remote_group = newcomm->c_local_group;
        OBJ_RETAIN(newcomm->c_remote_group);
    }
    
    /* Check how many different jobids are represented in this communicator.
       Necessary for the disconnect of dynamic communicators. */

    if ( 0 < local_size && (OMPI_COMM_IS_INTRA(newcomm) || 0 <remote_size) ) {
        ompi_dpm.mark_dyncomm (newcomm);
    }

    /* Set error handler */
    newcomm->error_handler = errh;
    OBJ_RETAIN ( newcomm->error_handler );
    
    /* Set Topology, if required and if available */
    if ( copy_topocomponent && (NULL != oldcomm->c_topo) ) {
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
    if (NULL != oldcomm->c_keyhash) {
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

    /* increase also the reference counter for the procs */
    ompi_group_increment_proc_count(comm->c_local_group);

    *group = comm->c_local_group;
    return OMPI_SUCCESS;
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/*
** Counterpart to MPI_Comm_create. To be used within OMPI.
*/
int ompi_comm_create ( ompi_communicator_t *comm, ompi_group_t *group, 
                       ompi_communicator_t **newcomm )
{
    ompi_communicator_t *newcomp = NULL;
    int rsize , lsize;
    int mode,i,j;
    int *allranks=NULL;
    int *rranks=NULL;
    int rc = OMPI_SUCCESS;
    
    /* silence clang warning. newcomm should never be NULL */
    if (OPAL_UNLIKELY(NULL == newcomm)) {
        return OMPI_ERR_BAD_PARAM;
    }

    lsize = group->grp_proc_count;
 
    if ( OMPI_COMM_IS_INTER(comm) ) {
        int tsize;
 
        tsize = ompi_comm_remote_size(comm);
        allranks = (int *) malloc ( tsize * sizeof(int));
        if ( NULL == allranks ) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }
        
        rc = comm->c_coll.coll_allgather ( &(group->grp_my_rank), 
                                           1, MPI_INT, allranks, 
                                           1, MPI_INT, comm,
                                           comm->c_coll.coll_allgather_module);
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
                         lsize,                    /* local_size */
                         NULL,                     /* local_ranks */
                         rsize,                    /* remote_size */
                         rranks,                   /* remote_ranks */
                         NULL,                     /* attrs */
                         comm->error_handler,      /* error handler */
                         false,                    /* dont copy the topo */
                         group,                    /* local group */
                         NULL                      /* remote group */
                         );
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    if ( NULL == newcomm ) {
        rc =  MPI_ERR_INTERN;
        goto exit;
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid ( newcomp,  /* new communicator */ 
                             comm,     /* old comm */
                             NULL,     /* bridge comm */
                             NULL,     /* local leader */
                             NULL,     /* remote_leader */
                             mode,     /* mode */
                             -1 );     /* send first */
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    /* Set name for debugging purposes */
    snprintf(newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI COMMUNICATOR %d CREATE FROM %d", 
             newcomp->c_contextid, comm->c_contextid );

    /* Activate the communicator and init coll-component */
    rc = ompi_comm_activate( &newcomp, /* new communicator */ 
                             comm, 
                             NULL, 
                             NULL, 
                             NULL,
                             mode, 
                             -1 );  
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


/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/*
** Counterpart to MPI_Comm_split. To be used within OMPI (e.g. MPI_Cart_sub).
*/
int ompi_comm_split( ompi_communicator_t* comm, int color, int key, 
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
    ompi_group_t * local_group=NULL, * remote_group=NULL;
    
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
        allgatherfct = (ompi_comm_allgatherfct *)comm->c_coll.coll_allgather;
    }

    results  = (int*) malloc ( 2 * size * sizeof(int));
    if ( NULL == results ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    rc = allgatherfct( myinfo, 2, MPI_INT, results, 2, MPI_INT, comm, comm->c_coll.coll_allgather_module );
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
        rc = comm->c_coll.coll_allgather( myinfo, 2, MPI_INT, rresults, 2, 
                                          MPI_INT, comm,
                                          comm->c_coll.coll_allgather_module);
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

        ompi_group_incl(comm->c_local_group, my_size, lranks, &local_group);
        ompi_group_increment_proc_count(local_group);

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
                         pass_on_topo,
                         local_group,       /* local group */
                         remote_group       /* remote group */
                         );

    if ( NULL == newcomp ) {
        rc =  MPI_ERR_INTERN;
        goto exit;
    }
    if ( OMPI_SUCCESS != rc  ) {
        goto exit;
    }

    if ( inter ) {
        ompi_group_decrement_proc_count (local_group);
        OBJ_RELEASE(local_group);
        if (NULL != newcomp->c_local_comm) {
            snprintf(newcomp->c_local_comm->c_name, MPI_MAX_OBJECT_NAME,
                     "MPI COMMUNICATOR %d SPLIT FROM %d",
                     newcomp->c_local_comm->c_contextid,
                     comm->c_local_comm->c_contextid );
        }
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid ( newcomp,  /* new communicator */ 
                             comm,     /* old comm */
                             NULL,     /* bridge comm */
                             NULL,     /* local leader */
                             NULL,     /* remote_leader */
                             mode,     /* mode */
                             -1 );     /* send first, doesn't matter */
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    /* Set name for debugging purposes */
    snprintf(newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI COMMUNICATOR %d SPLIT FROM %d", 
             newcomp->c_contextid, comm->c_contextid );

    /* set the rank to MPI_UNDEFINED. This prevents in comm_activate
     * the collective module selection for a communicator that will
     * be freed anyway.
     */
    if ( MPI_UNDEFINED == color || (inter && my_rsize==0)) {
        newcomp->c_local_group->grp_my_rank = MPI_UNDEFINED;
    }


    /* Activate the communicator and init coll-component */
    rc = ompi_comm_activate( &newcomp, /* new communicator */ 
                             comm, 
                             NULL,
                             NULL, 
                             NULL, 
                             mode, 
                             -1 );  
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

 exit:
    if ( NULL != results ) {
        free ( results );
    }
    if ( NULL != sorted  ) {
        free ( sorted );
    }
    if ( NULL != rresults) {
        free ( rresults );
    }
    if ( NULL != rsorted ) {
        free ( rsorted );
    }
    if ( NULL != lranks   ) {
        free ( lranks );
    }
    if ( NULL != rranks  ) {
        free ( rranks );
    }

    /* Step 4: if we are not part of the comm, free the struct   */
    /* --------------------------------------------------------- */
    if (inter && my_rsize == 0) {
        color = MPI_UNDEFINED;
    }
    if ( NULL != newcomp && MPI_UNDEFINED == color ) {
        ompi_comm_free ( &newcomp );
    }

    *newcomm = newcomp;
    return ( rc );
}


/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
int
ompi_comm_split_type(ompi_communicator_t *comm, 
                     int split_type, int key,
                     ompi_info_t *info,
                     ompi_communicator_t** newcomm)
{
    int myinfo[2];
    int size, my_size;
    int my_rsize;
    int mode;
    int rsize;
    int i, loc, found;
    int inter;
    int *results=NULL, *sorted=NULL; 
    int *rresults=NULL, *rsorted=NULL; 
    int rc=OMPI_SUCCESS;
    ompi_communicator_t *newcomp = NULL;
    int *lranks=NULL, *rranks=NULL;
    
    ompi_comm_allgatherfct *allgatherfct=NULL;

    /* silence clang warning. newcomm should never be NULL */
    if (OPAL_UNLIKELY(NULL == newcomm)) {
        return OMPI_ERR_BAD_PARAM;
    }

    /* Step 1: determine all the information for the local group */
    /* --------------------------------------------------------- */

    /* sort according to participation and rank. Gather information from everyone */
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
    myinfo[0] = 0; // default to no type splitting (also if non-recognized split-type)
    switch ( split_type ) {
    case OMPI_COMM_TYPE_HWTHREAD:
        myinfo[0] = 1; break;
    case OMPI_COMM_TYPE_CORE:
        myinfo[0] = 2; break;
    case OMPI_COMM_TYPE_L1CACHE:
        myinfo[0] = 3; break;
    case OMPI_COMM_TYPE_L2CACHE:
        myinfo[0] = 4; break;
    case OMPI_COMM_TYPE_L3CACHE:
        myinfo[0] = 5; break;
    case OMPI_COMM_TYPE_SOCKET:
        myinfo[0] = 6; break;
    case OMPI_COMM_TYPE_NUMA:
        myinfo[0] = 7; break;
    //case MPI_COMM_TYPE_SHARED: // the standard implemented type
    case OMPI_COMM_TYPE_NODE:
        myinfo[0] = 8; break;
    case OMPI_COMM_TYPE_BOARD:
        myinfo[0] = 9; break;
    case OMPI_COMM_TYPE_HOST:
        myinfo[0] = 10; break;
    case OMPI_COMM_TYPE_CU:
        myinfo[0] = 11; break;
    case OMPI_COMM_TYPE_CLUSTER:
        myinfo[0] = 12; break;
    }
    myinfo[1] = key;

    size     = ompi_comm_size ( comm );
    inter    = OMPI_COMM_IS_INTER(comm);
    if ( inter ) {
        allgatherfct = (ompi_comm_allgatherfct *)ompi_comm_allgather_emulate_intra;
    } else {
        allgatherfct = (ompi_comm_allgatherfct *)comm->c_coll.coll_allgather;
    }

    results  = (int*) malloc ( 2 * size * sizeof(int));
    if ( NULL == results ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    rc = allgatherfct( myinfo, 2, MPI_INT, results, 2, MPI_INT, comm, comm->c_coll.coll_allgather_module );
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    /* check that all processors have been called with the same value */
    for ( i=0; i < size; i++) {
        if ( results[2*i] != myinfo[0] ) {
            rc = OMPI_ERR_BAD_PARAM;
            goto exit;
        }
    }
    
    /* how many are participating and on my node? */
    for ( my_size = 0, i=0; i < size; i++) {
        if        ( results[2*i] == 1 ) {
            if (OPAL_PROC_ON_LOCAL_HWTHREAD(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                my_size++;
            }
        } else if ( results[2*i] == 2 ) {
            if (OPAL_PROC_ON_LOCAL_CORE(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                my_size++;
            }
        } else if ( results[2*i] == 3 ) {
            if (OPAL_PROC_ON_LOCAL_L1CACHE(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                my_size++;
            }
        } else if ( results[2*i] == 4 ) {
            if (OPAL_PROC_ON_LOCAL_L2CACHE(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                my_size++;
            }
        } else if ( results[2*i] == 5 ) {
            if (OPAL_PROC_ON_LOCAL_L3CACHE(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                my_size++;
            }
        } else if ( results[2*i] == 6 ) {
            if (OPAL_PROC_ON_LOCAL_SOCKET(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                my_size++;
            }
        } else if ( results[2*i] == 7 ) {
            if (OPAL_PROC_ON_LOCAL_NUMA(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                my_size++;
            }
        } else if ( results[2*i] == 8 ) {
            if (OPAL_PROC_ON_LOCAL_NODE(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                my_size++;
            }
        } else if ( results[2*i] == 9 ) {
            if (OPAL_PROC_ON_LOCAL_BOARD(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                my_size++;
            }
        } else if ( results[2*i] == 10 ) {
            if (OPAL_PROC_ON_LOCAL_HOST(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                my_size++;
            }
        } else if ( results[2*i] == 11 ) {
            if (OPAL_PROC_ON_LOCAL_CU(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                my_size++;
            }
        } else if ( results[2*i] == 12 ) {
            if (OPAL_PROC_ON_LOCAL_CLUSTER(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                my_size++;
            }
        }
    }

    /* silence a clang warning about a 0-byte malloc. my_size can not be 0 here */
    if (OPAL_UNLIKELY(0 == my_size)) {
        rc = OMPI_ERR_BAD_PARAM;
        goto exit;
    }

    sorted = (int *) malloc ( sizeof( int ) * my_size * 2);
    if ( NULL == sorted) {
        rc =  OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }
    
    /* ok we can now fill this info */
    for( loc = 0, i = 0; i < size; i++ ) {
        found = 0;
        if        ( results[2*i] == 1 ) {
            if (OPAL_PROC_ON_LOCAL_HWTHREAD(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                found = 1;
            }
        } else if ( results[2*i] == 2 ) {
            if (OPAL_PROC_ON_LOCAL_CORE(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                found = 1;
            }
        } else if ( results[2*i] == 3 ) {
            if (OPAL_PROC_ON_LOCAL_L1CACHE(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                found = 1;
            }
        } else if ( results[2*i] == 4 ) {
            if (OPAL_PROC_ON_LOCAL_L2CACHE(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                found = 1;
            }
        } else if ( results[2*i] == 5 ) {
            if (OPAL_PROC_ON_LOCAL_L3CACHE(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                found = 1;
            }
        } else if ( results[2*i] == 6 ) {
            if (OPAL_PROC_ON_LOCAL_SOCKET(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                found = 1;
            }
        } else if ( results[2*i] == 7 ) {
            if (OPAL_PROC_ON_LOCAL_NUMA(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                found = 1;
            }
        } else if ( results[2*i] == 8 ) {
            if (OPAL_PROC_ON_LOCAL_NODE(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                found = 1;
            }
        } else if ( results[2*i] == 9 ) {
            if (OPAL_PROC_ON_LOCAL_BOARD(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                found = 1;
            }
        } else if ( results[2*i] == 10 ) {
            if (OPAL_PROC_ON_LOCAL_HOST(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                found = 1;
            }
        } else if ( results[2*i] == 11 ) {
            if (OPAL_PROC_ON_LOCAL_CU(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                found = 1;
            }
        } else if ( results[2*i] == 12 ) {
            if (OPAL_PROC_ON_LOCAL_CLUSTER(ompi_group_peer_lookup(comm->c_local_group, i)->super.proc_flags)) {
                found = 1;
            }
        }

        /* we have found and occupied the index (i) */
        if ( found == 1 ) {
            sorted[2*loc  ] = i;               /* copy org rank */
            sorted[2*loc+1] = results[2*i+1];  /* copy key */
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
        rsize    = comm->c_remote_group->grp_proc_count;
        rresults = (int *) malloc ( rsize * 2 * sizeof(int));
        if ( NULL == rresults ) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }

        /* this is an allgather on an inter-communicator */
        rc = comm->c_coll.coll_allgather( myinfo, 2, MPI_INT, rresults, 2, 
                                          MPI_INT, comm,
                                          comm->c_coll.coll_allgather_module);
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }

        /* how many are participating and on my node? */
        for ( my_rsize = 0, i=0; i < rsize; i++) {
            if        ( rresults[2*i] == 1 ) {
                if (OPAL_PROC_ON_LOCAL_HWTHREAD(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                    my_rsize++;
                }
            } else if ( rresults[2*i] == 2 ) {
                if (OPAL_PROC_ON_LOCAL_CORE(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                    my_rsize++;
                }
            } else if ( rresults[2*i] == 3 ) {
                if (OPAL_PROC_ON_LOCAL_L1CACHE(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                    my_rsize++;
                }
            } else if ( rresults[2*i] == 4 ) {
                if (OPAL_PROC_ON_LOCAL_L2CACHE(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                    my_rsize++;
                }
            } else if ( rresults[2*i] == 5 ) {
                if (OPAL_PROC_ON_LOCAL_L3CACHE(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                    my_rsize++;
                }
            } else if ( rresults[2*i] == 6 ) {
                if (OPAL_PROC_ON_LOCAL_SOCKET(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                    my_rsize++;
                }
            } else if ( rresults[2*i] == 7 ) {
                if (OPAL_PROC_ON_LOCAL_NUMA(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                    my_rsize++;
                }
            } else if ( rresults[2*i] == 8 ) {
                if (OPAL_PROC_ON_LOCAL_NODE(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                    my_rsize++;
                }
            } else if ( rresults[2*i] == 9 ) {
                if (OPAL_PROC_ON_LOCAL_BOARD(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                    my_rsize++;
                }
            } else if ( rresults[2*i] == 10 ) {
                if (OPAL_PROC_ON_LOCAL_HOST(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                    my_rsize++;
                }
            } else if ( rresults[2*i] == 11 ) {
                if (OPAL_PROC_ON_LOCAL_CU(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                    my_rsize++;
                }
            } else if ( rresults[2*i] == 12 ) {
                if (OPAL_PROC_ON_LOCAL_CLUSTER(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                    my_rsize++;
                }
            }
        }

        if (my_rsize > 0) {
            rsorted = (int *) malloc ( sizeof( int ) * my_rsize * 2);
            if ( NULL == rsorted) {
                rc = OMPI_ERR_OUT_OF_RESOURCE;
                goto exit;
            }
        
            /* ok we can now fill this info */
            for( loc = 0, i = 0; i < rsize; i++ ) {
                found = 0;
                if        ( rresults[2*i] == 1 ) {
                    if (OPAL_PROC_ON_LOCAL_HWTHREAD(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                        found = 1;
                    }
                } else if ( rresults[2*i] == 2 ) {
                    if (OPAL_PROC_ON_LOCAL_CORE(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                        found = 1;
                    }
                } else if ( rresults[2*i] == 3 ) {
                    if (OPAL_PROC_ON_LOCAL_L1CACHE(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                        found = 1;
                    }
                } else if ( rresults[2*i] == 4 ) {
                    if (OPAL_PROC_ON_LOCAL_L2CACHE(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                        found = 1;
                    }
                } else if ( rresults[2*i] == 5 ) {
                    if (OPAL_PROC_ON_LOCAL_L3CACHE(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                        found = 1;
                    }
                } else if ( rresults[2*i] == 6 ) {
                    if (OPAL_PROC_ON_LOCAL_SOCKET(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                        found = 1;
                    }
                } else if ( rresults[2*i] == 7 ) {
                    if (OPAL_PROC_ON_LOCAL_NUMA(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                        found = 1;
                    }
                } else if ( rresults[2*i] == 8 ) {
                    if (OPAL_PROC_ON_LOCAL_NODE(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                        found = 1;
                    }
                } else if ( rresults[2*i] == 9 ) {
                    if (OPAL_PROC_ON_LOCAL_BOARD(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                        found = 1;
                    }
                } else if ( rresults[2*i] == 10 ) {
                    if (OPAL_PROC_ON_LOCAL_HOST(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                        found = 1;
                    }
                } else if ( rresults[2*i] == 11 ) {
                    if (OPAL_PROC_ON_LOCAL_CU(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                        found = 1;
                    }
                } else if ( rresults[2*i] == 12 ) {
                    if (OPAL_PROC_ON_LOCAL_CLUSTER(ompi_group_peer_lookup(comm->c_remote_group, i)->super.proc_flags)) {
                        found = 1;
                    }
                }

                if ( found == 1 ) {
                    rsorted[2*loc  ] = i;                /* org rank */
                    rsorted[2*loc+1] = rresults[2*i+1];  /* key */
                    loc++;
                }
            }
        
            /* the new array needs to be sorted so that it is in 'key' order */
            /* if two keys are equal then it is sorted in original rank order! */
            if(my_rsize > 1) {
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

        mode = OMPI_COMM_CID_INTER;
    } else {
        my_rsize  = 0;
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
                         false,              /* don't copy the topo */
                         NULL,               /* local group */
                         NULL );             /* remote group */

    if ( NULL == newcomm ) {
        rc =  MPI_ERR_INTERN;
        goto exit;
    }
    if ( OMPI_SUCCESS != rc  ) {
        goto exit;
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid ( newcomp,  /* new communicator */ 
                             comm,     /* old comm */
                             NULL,     /* bridge comm */
                             NULL,     /* local leader */
                             NULL,     /* remote_leader */
                             mode,     /* mode */
                             -1 );     /* send first, doesn't matter */
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    /* Set name for debugging purposes */
    snprintf(newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI COMMUNICATOR %d SPLIT_TYPE FROM %d", 
             newcomp->c_contextid, comm->c_contextid );

    /* set the rank to MPI_UNDEFINED. This prevents in comm_activate
     * the collective module selection for a communicator that will
     * be freed anyway.
     */
    if ( MPI_UNDEFINED == split_type ) {
        newcomp->c_local_group->grp_my_rank = MPI_UNDEFINED;
    }


    /* Activate the communicator and init coll-component */
    rc = ompi_comm_activate( &newcomp, /* new communicator */ 
                             comm, 
                             NULL,
                             NULL, 
                             NULL, 
                             mode, 
                             -1 );  
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

 exit:
    if ( NULL != results ) {
        free ( results );
    }
    if ( NULL != sorted  ) {
        free ( sorted );
    }
    if ( NULL != rresults) {
        free ( rresults );
    }
    if ( NULL != rsorted ) {
        free ( rsorted );
    }
    if ( NULL != lranks   ) {
        free ( lranks );
    }
    if ( NULL != rranks  ) {
        free ( rranks );
    }

    /* Step 4: if we are not part of the comm, free the struct   */
    /* --------------------------------------------------------- */
    if ( NULL != newcomp && MPI_UNDEFINED == split_type ) {
        ompi_comm_free ( &newcomp );
    }

    *newcomm = newcomp;
    return ( rc );
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
int ompi_comm_dup_with_info ( ompi_communicator_t * comm, ompi_info_t *info, ompi_communicator_t **newcomm )
{
    ompi_communicator_t *newcomp = NULL;
    int rsize = 0, mode = OMPI_COMM_CID_INTRA, rc = OMPI_SUCCESS;

    if ( OMPI_COMM_IS_INTER ( comm ) ){
        rsize  = comm->c_remote_group->grp_proc_count;
        mode   = OMPI_COMM_CID_INTER;
    }
    
    *newcomm = MPI_COMM_NULL;

    rc =  ompi_comm_set ( &newcomp,                               /* new comm */
                          comm,                                   /* old comm */
                          comm->c_local_group->grp_proc_count,    /* local_size */
                          NULL,                                   /* local_procs*/
                          rsize,                                  /* remote_size */
                          NULL,                                   /* remote_procs */
                          comm->c_keyhash,                        /* attrs */
                          comm->error_handler,                    /* error handler */
                          true,                                   /* copy the topo */
                          comm->c_local_group,                    /* local group */
                          comm->c_remote_group );                 /* remote group */
    if ( NULL == newcomp ) {
        rc =  MPI_ERR_INTERN;
        return rc;
    }
    if ( MPI_SUCCESS != rc) { 
        return rc;
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid ( newcomp,  /* new communicator */ 
                             comm,     /* old comm */
                             NULL,     /* bridge comm */
                             NULL,     /* local leader */
                             NULL,     /* remote_leader */
                             mode,     /* mode */
                             -1 );     /* send_first */
    if ( OMPI_SUCCESS != rc ) {
        return rc;
    }

    /* Set name for debugging purposes */
    snprintf(newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI COMMUNICATOR %d DUP FROM %d", 
             newcomp->c_contextid, comm->c_contextid );

    /* activate communicator and init coll-module */
    rc = ompi_comm_activate( &newcomp, /* new communicator */ 
                             comm,
                             NULL, 
                             NULL, 
                             NULL, 
                             mode, 
                             -1 );  
    if ( OMPI_SUCCESS != rc ) {
        return rc;
    }
    
    *newcomm = newcomp;
    return MPI_SUCCESS;
}

struct ompi_comm_idup_with_info_context {
    ompi_communicator_t *comm;
    ompi_communicator_t *newcomp;
};

static int ompi_comm_idup_with_info_activate (ompi_comm_request_t *request);
static int ompi_comm_idup_with_info_finish (ompi_comm_request_t *request);
static int ompi_comm_idup_getcid (ompi_comm_request_t *request);

int ompi_comm_idup (ompi_communicator_t *comm, ompi_communicator_t **newcomm, ompi_request_t **req)
{
    return ompi_comm_idup_with_info (comm, NULL, newcomm, req);
}

int ompi_comm_idup_with_info (ompi_communicator_t *comm, ompi_info_t *info, ompi_communicator_t **newcomm, ompi_request_t **req)
{
    return ompi_comm_idup_internal (comm, comm->c_local_group, info, newcomm, req);
}

/* NTH: we need a way to idup with a smaller local group so this function takes a local group */
static int ompi_comm_idup_internal (ompi_communicator_t *comm, ompi_group_t *group, ompi_info_t *info,
                                    ompi_communicator_t **newcomm, ompi_request_t **req)
{
    struct ompi_comm_idup_with_info_context *context;
    ompi_comm_request_t *request;
    ompi_request_t *subreq[1];
    int rsize = 0, rc;

    *newcomm = MPI_COMM_NULL;

    request = ompi_comm_request_get ();
    if (NULL == request) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    context = calloc (1, sizeof (*context));
    if (NULL == context) {
        ompi_comm_request_return (request);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    context->comm    = comm;

    request->context = context;

    rc =  ompi_comm_set_nb (&context->newcomp,                      /* new comm */
                            comm,                                   /* old comm */
                            group->grp_proc_count,                  /* local_size */
                            NULL,                                   /* local_procs */
                            rsize,                                  /* remote_size */
                            NULL,                                   /* remote_procs */
                            comm->c_keyhash,                        /* attrs */
                            comm->error_handler,                    /* error handler */
                            true,                                   /* copy the topo */
                            group,                                  /* local group */
                            comm->c_remote_group,                   /* remote group */
                            subreq);                                /* new subrequest */
    if (NULL == context->newcomp) {
        ompi_comm_request_return (request);
        return rc;
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
    struct ompi_comm_idup_with_info_context *context =
        (struct ompi_comm_idup_with_info_context *) request->context;
    ompi_request_t *subreq[1];
    int rc, mode;

    if (OMPI_COMM_IS_INTER(context->comm)){
        mode  = OMPI_COMM_CID_INTER;
    } else {
        mode  = OMPI_COMM_CID_INTRA;
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid_nb (context->newcomp,  /* new communicator */
                               context->comm,     /* old comm */
                               NULL,              /* bridge comm */
                               mode,              /* mode */
                               subreq);           /* new subrequest */
    if (OMPI_SUCCESS != rc) {
        ompi_comm_request_return (request);
        return rc;
    }

    ompi_comm_request_schedule_append (request, ompi_comm_idup_with_info_activate, subreq, 1);

    return OMPI_SUCCESS;
}

static int ompi_comm_idup_with_info_activate (ompi_comm_request_t *request)
{
    struct ompi_comm_idup_with_info_context *context =
        (struct ompi_comm_idup_with_info_context *) request->context;
    ompi_request_t *subreq[1];
    int rc, mode;

    if (OMPI_COMM_IS_INTER(context->comm)){
        mode  = OMPI_COMM_CID_INTER;
    } else {
        mode  = OMPI_COMM_CID_INTRA;
    }

    /* Set name for debugging purposes */
    snprintf(context->newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI COMMUNICATOR %d DUP FROM %d",
             context->newcomp->c_contextid, context->comm->c_contextid );

    /* activate communicator and init coll-module */
    rc = ompi_comm_activate_nb (&context->newcomp, context->comm, NULL, mode, subreq);
    if ( OMPI_SUCCESS != rc ) {
        return rc;
    }

    ompi_comm_request_schedule_append (request, ompi_comm_idup_with_info_finish, subreq, 1);

    return OMPI_SUCCESS;
}

static int ompi_comm_idup_with_info_finish (ompi_comm_request_t *request)
{
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
                          true,                                   /* copy the topo */
                          group,                                  /* local group */
                          NULL);                                  /* remote group */
    if ( NULL == newcomp ) {
        rc =  MPI_ERR_INTERN;
        return rc;
    }
    if ( MPI_SUCCESS != rc) {
        return rc;
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid ( newcomp,  /* new communicator */
                             comm,     /* old comm */
                             newcomp,  /* bridge comm (used to pass the group into the group allreduce) */
                             &tag,     /* user defined tag */
                             NULL,     /* remote_leader */
                             mode,     /* mode */
                             -1 );     /* send_first */
    if ( OMPI_SUCCESS != rc ) {
        return rc;
    }

    /* Set name for debugging purposes */
    snprintf(newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI COMMUNICATOR %d GROUP FROM %d",
             newcomp->c_contextid, comm->c_contextid );

    /* activate communicator and init coll-module */
    rc = ompi_comm_activate( &newcomp, /* new communicator */
                             comm,
                             newcomp,
                             &tag,
                             NULL,
                             mode,
                             -1 );
    if ( OMPI_SUCCESS != rc ) {
        return rc;
    }

    *newcomm = newcomp;
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

#ifdef USE_MUTEX_FOR_COMMS
    OPAL_THREAD_LOCK(&(comm->c_lock));
#endif
    memset(comm->c_name, 0, MPI_MAX_OBJECT_NAME);
    strncpy(comm->c_name, name, MPI_MAX_OBJECT_NAME);
    comm->c_name[MPI_MAX_OBJECT_NAME - 1] = 0;
    comm->c_flags |= OMPI_COMM_NAMEISSET;
#ifdef USE_MUTEX_FOR_COMMS
    OPAL_THREAD_UNLOCK(&(comm->c_lock));
#endif

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
    int cid = (*comm)->c_contextid;
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
ompi_proc_t **ompi_comm_get_rprocs ( ompi_communicator_t *local_comm, 
                                     ompi_communicator_t *bridge_comm, 
                                     int local_leader,
                                     int remote_leader,
                                     int tag,
                                     int rsize)
{

    MPI_Request req;
    int rc;
    int local_rank, local_size;
    ompi_proc_t **rprocs=NULL;
    int32_t size_len;
    int int_len=0, rlen;
    opal_buffer_t *sbuf=NULL, *rbuf=NULL;
    void *sendbuf=NULL;
    char *recvbuf;
    ompi_proc_t **proc_list=NULL;
    int i;
    opal_list_t myvals;
    opal_value_t *kv;

    local_rank = ompi_comm_rank (local_comm);
    local_size = ompi_comm_size (local_comm);

    if (local_rank == local_leader) {
        sbuf = OBJ_NEW(opal_buffer_t);
        if (NULL == sbuf) {
            rc = OMPI_ERROR;
            goto err_exit;
        }
        if(OMPI_GROUP_IS_DENSE(local_comm->c_local_group)) {
            rc = ompi_proc_pack(local_comm->c_local_group->grp_proc_pointers,
                                local_size, true, sbuf);
        }
        /* get the proc list for the sparse implementations */
        else {
            proc_list = (ompi_proc_t **) calloc (local_comm->c_local_group->grp_proc_count,
                                                 sizeof (ompi_proc_t *));
            for(i=0 ; i<local_comm->c_local_group->grp_proc_count ; i++)
                proc_list[i] = ompi_group_peer_lookup(local_comm->c_local_group,i);
            rc = ompi_proc_pack (proc_list, local_size, true, sbuf);
        }
        if ( OMPI_SUCCESS != rc ) {
            goto err_exit;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.unload(sbuf, &sendbuf, &size_len))) {
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
            goto err_exit;
        }
        rc = ompi_request_wait( &req, MPI_STATUS_IGNORE );
        if ( OMPI_SUCCESS != rc ) {
            goto err_exit;
        }
    }

    /* broadcast buffer length to all processes in local_comm */
    rc = local_comm->c_coll.coll_bcast( &rlen, 1, MPI_INT,
                                        local_leader, local_comm,
                                        local_comm->c_coll.coll_bcast_module );
    if ( OMPI_SUCCESS != rc ) {
        goto err_exit;
    }

    /* Allocate temporary buffer */
    recvbuf = (char *)malloc(rlen);
    if ( NULL == recvbuf ) {
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
        if ( OMPI_SUCCESS != rc ) {
            goto err_exit;
        }
        rc = ompi_request_wait( &req, MPI_STATUS_IGNORE );
        if ( OMPI_SUCCESS != rc ) {
            goto err_exit;
        }
    }

    /* broadcast name list to all proceses in local_comm */
    rc = local_comm->c_coll.coll_bcast( recvbuf, rlen, MPI_BYTE,
                                        local_leader, local_comm,
                                        local_comm->c_coll.coll_bcast_module);
    if ( OMPI_SUCCESS != rc ) {
        goto err_exit;
    }

    rbuf = OBJ_NEW(opal_buffer_t);
    if (NULL == rbuf) {
        rc = OMPI_ERROR;
        goto err_exit;
    }

    if (OMPI_SUCCESS != (rc = opal_dss.load(rbuf, recvbuf, rlen))) {
        goto err_exit;
    }

    /* decode the names into a proc-list */
    rc = ompi_proc_unpack(rbuf, rsize, &rprocs, true, NULL, NULL);
    OBJ_RELEASE(rbuf);
    if (OMPI_SUCCESS != rc) {
        OMPI_ERROR_LOG(rc);
        goto err_exit;
    }

    /* set the locality of the remote procs */
    for (i=0; i < rsize; i++) {
        /* get the locality information - do not use modex recv for
         * this request as that will automatically cause the hostname
         * to be loaded as well. All RTEs are required to provide this
         * information at startup for procs on our node. Thus, not
         * finding the info indicates that the proc is non-local.
         */
        OBJ_CONSTRUCT(&myvals, opal_list_t);
        if (OMPI_SUCCESS != opal_dstore.fetch(opal_dstore_internal,
                                              &rprocs[i]->super.proc_name,
                                              OPAL_DSTORE_LOCALITY, &myvals)) {
            rprocs[i]->super.proc_flags = OPAL_PROC_NON_LOCAL;
        } else {
            kv = (opal_value_t*)opal_list_get_first(&myvals);
            rprocs[i]->super.proc_flags = kv->data.uint16;
        }
        OPAL_LIST_DESTRUCT(&myvals);
    }

    /* And now add the information into the database */
    if (OMPI_SUCCESS != (rc = MCA_PML_CALL(add_procs(rprocs, rsize)))) {
        OMPI_ERROR_LOG(rc);
        goto err_exit;
    }

 err_exit:
    /* rprocs isn't freed unless we have an error,
       since it is used in the communicator */
    if ( OMPI_SUCCESS != rc ) {
        opal_output(0, "%d: Error in ompi_get_rprocs\n", local_rank);
        if ( NULL != rprocs ) {
            free ( rprocs );
            rprocs=NULL;
        }
    }
    /* make sure the buffers have been released */
    if (NULL != sbuf) {
        OBJ_RELEASE(sbuf);
    }
    if (NULL != rbuf) {
        OBJ_RELEASE(rbuf);
    }
    if ( NULL != proc_list ) {
        free ( proc_list );
    }
    if (NULL != sendbuf) {
        free ( sendbuf );
    }

    return rprocs;
}
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/**
 * This routine verifies, whether local_group and remote group are overlapping
 * in intercomm_create
 */
int ompi_comm_overlapping_groups (int size, ompi_proc_t **lprocs,
                                  int rsize, ompi_proc_t ** rprocs)

{
    int rc=OMPI_SUCCESS;
    int i,j;

    for (i=0; i<size; i++) {
        for ( j=0; j<rsize; j++) {
            if ( lprocs[i] == rprocs[j] ) {
                rc = MPI_ERR_COMM;
                return rc;
            }
        }
    }

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
    ompi_proc_t *ourproc, *theirproc;
    ompi_rte_cmp_bitmask_t mask;

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
    
    rc = intercomm->c_coll.coll_allgatherv(&high, scount, MPI_INT,
                                           &rhigh, rcounts, rdisps,
                                           MPI_INT, intercomm,
                                           intercomm->c_coll.coll_allgatherv_module);
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
        ourproc   = ompi_group_peer_lookup(intercomm->c_local_group,0);
        theirproc = ompi_group_peer_lookup(intercomm->c_remote_group,0);

        mask = OMPI_RTE_CMP_JOBID | OMPI_RTE_CMP_VPID;
        rc = ompi_rte_compare_name_fields(mask, (const orte_process_name_t*)&(ourproc->super.proc_name),
                                                (const orte_process_name_t*)&(theirproc->super.proc_name));
        if ( 0 > rc ) {
            flag = true;
        }
        else {
            flag = false;
        }
    }

    return flag;
}
/********************************************************************************/
/********************************************************************************/
/********************************************************************************/
int ompi_comm_dump ( ompi_communicator_t *comm )
{
    opal_output(0, "Dumping information for comm_cid %d\n", comm->c_contextid);
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

    /* Determine context id. It is identical to f_2_c_handle */
    ret = ompi_comm_nextcid ( new_comm, /* new communicator */
                              old_comm, /* old comm */
                              NULL,     /* bridge comm */
                              NULL,     /* local leader */
                              NULL,     /* remote_leader */
                              OMPI_COMM_CID_INTRA,   /* mode */
                              -1 );     /* send first, doesn't matter */
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

    ret = ompi_comm_activate( &new_comm,  /* new communicator */
                              old_comm,   /* old comm */
                              NULL,       /* bridge comm */
                              NULL,       /* local leader */
                              NULL,       /* remote_leader */
                              OMPI_COMM_CID_INTRA,   /* mode */
                              -1 );       /* send first, doesn't matter */


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
    /* properly decrement the ref counts on the groups.
       We are doing this because this function is sort of a redo 
       of what is done in comm.c. No need to decrement the ref 
       count on the proc pointers 
       This is just a quick fix, and will be looking for a 
       better solution */
    OBJ_RELEASE( comm->c_local_group );
    /* silence clang warning about a NULL pointer dereference */
    assert (NULL != comm->c_local_group);
    OBJ_RELEASE( comm->c_local_group );

    /* allocate a group structure for the new communicator */
    comm->c_local_group = ompi_group_allocate(num_procs);
    
    /* free the malloced  proc pointers */
    free(comm->c_local_group->grp_proc_pointers);
    
    /* set the group information */
    comm->c_local_group->grp_proc_pointers = proc_pointers;

    /* set the remote group to be the same as local group */
    comm->c_remote_group = comm->c_local_group;
    OBJ_RETAIN( comm->c_remote_group );

    /* retain these proc pointers */
    ompi_group_increment_proc_count(comm->c_local_group);
        
    /* set the rank information */
    comm->c_local_group->grp_my_rank = my_rank;
    comm->c_my_rank = my_rank;

    if( MPI_UNDEFINED != my_rank ) {
        /* verify whether to set the flag, that this comm
           contains process from more than one jobid. */
        ompi_dpm.mark_dyncomm (comm);
    }

    /* set the error handler */
    comm->error_handler = errh;
    OBJ_RETAIN (comm->error_handler);

    /* set name for debugging purposes */
    /* there is no cid at this stage ... make this right and make edgars
     * code call this function and remove dupli cde
     */  
    snprintf (comm->c_name, MPI_MAX_OBJECT_NAME, "MPI_COMMUNICATOR %d",
              comm->c_contextid);

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
