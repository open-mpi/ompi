/*
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include <stdio.h>
#include "mpi.h"

#include "communicator/communicator.h"
#include "proc/proc.h"
#include "include/constants.h"
#include "mca/pcm/pcm.h"
#include "mca/pml/pml.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"

#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_comm.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "mca/ptl/base/ptl_base_header.h"
#include "mca/ptl/base/ptl_base_match.h"

/*
** sort-function for MPI_Comm_split 
*/
static int rankkeycompare(const void *, const void *);


/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/* 
 * This is the function setting all elements of a communicator.
 * All other routines are just used to determine these elements.
 */   
ompi_communicator_t * ompi_comm_set ( int mode,
                                    ompi_communicator_t* oldcomm,
                                    ompi_communicator_t* bridgecomm,
                                    int local_size, 
                                    ompi_proc_t **local_procs,
                                    int remote_size,
                                    ompi_proc_t **remote_procs,
                                    ompi_hash_table_t *attr,
                                    ompi_errhandler_t *errh,
                                    mca_base_module_t *collmodule,
                                    mca_base_module_t *topomodule, 
                                    int local_leader,
                                    int remote_leader
                                    )
{
    ompi_communicator_t *newcomm;
    ompi_proc_t *my_gpointer;
    int my_grank;
    int rc;

    /* Allocate comm structure */
    newcomm = ompi_comm_allocate ( local_size, remote_size );
    
    /* Set local_group information */
    memcpy ( newcomm->c_local_group->grp_proc_pointers, 
             local_procs, local_size * sizeof(ompi_proc_t *));
    ompi_group_increment_proc_count(newcomm->c_local_group);
        
    /* determine my rank */
    my_grank    = oldcomm->c_local_group->grp_my_rank;             
    my_gpointer = oldcomm->c_local_group->grp_proc_pointers[my_grank];
    ompi_set_group_rank(newcomm->c_local_group, my_gpointer);
    newcomm->c_my_rank = newcomm->c_local_group->grp_my_rank;

    /* Set remote group, if applicable */
    if ( 0 < remote_size) {        
        memcpy ( newcomm->c_remote_group->grp_proc_pointers, 
                 remote_procs, remote_size * sizeof(ompi_proc_t *));
        ompi_group_increment_proc_count(newcomm->c_remote_group);
        newcomm->c_flags |= OMPI_COMM_INTER;
    }

    /* Determine context id. It is identical to f_2_c_handle */
#ifdef HAVE_COLLECTIVES
    newcomm->c_contextid = ompi_comm_nextcid ( oldcomm, 
                                              bridgecomm, 
                                              local_leader, 
                                              remote_leader, 
                                              mode );    
#else
    /* just for now */
    newcomm->c_contextid = oldcomm->c_contextid + 10;
    ompi_pointer_array_set_item ( &ompi_mpi_communicators, newcomm->c_contextid,
                                 newcomm );
#endif
    newcomm->c_f_to_c_index = newcomm->c_contextid;

    /* Set error handler */
    newcomm->error_handler = errh;
    OBJ_RETAIN ( newcomm->error_handler );

    /* Set name for debugging purposes */
    snprintf(newcomm->c_name, MPI_MAX_OBJECT_NAME, "MPI COMMUNICATOR %d", 
             newcomm->c_contextid);

    /* Determine cube_dim */
    newcomm->c_cube_dim = ompi_cube_dim(newcomm->c_local_group->grp_proc_count);

    /* Set Topology, if required */
    if ( NULL != topomodule ) {
        if (OMPI_COMM_IS_CART ( oldcomm ) )
            newcomm->c_flags |= OMPI_COMM_CART;
        if (OMPI_COMM_IS_GRAPH ( oldcomm ) ) 
            newcomm->c_flags |= OMPI_COMM_GRAPH;
        
        /* set the topo-module */
    }

    /* Copy attributes and call according copy functions, 
       if required */
    if ( attr != NULL ) {
    }

    /* Initialize the PML stuff in the newcomm  */
    if ( OMPI_ERROR == mca_pml.pml_add_comm(newcomm) ) {
        goto err_exit;
    }

    /* Initialize the coll modules */
    /* Let the collectives modules fight over who will do
       collective on this new comm.  */
    if (OMPI_ERROR == mca_coll_base_init_comm(newcomm)) {
	goto err_exit;
    }

    /* ******* VPS: Remove this later -- need to be in coll module ******  */
    /* VPS: Cache the reqs for bcast */
    newcomm->bcast_lin_reqs =
	malloc (mca_coll_base_bcast_collmaxlin * sizeof(ompi_request_t*));
    if (NULL ==  newcomm->bcast_lin_reqs) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto err_exit;
    }
    newcomm->bcast_log_reqs = 
	malloc (mca_coll_base_bcast_collmaxdim * sizeof(ompi_request_t*));
    if (NULL ==  newcomm->bcast_log_reqs) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
	goto err_exit;
    }

 err_exit:
    /* Free whatever has been allocated, print an appropriate
       error message and return a null pointer */

    return ( newcomm );
}



/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/*
** Counterpart to MPI_Comm_group. To be used within OMPI functions.
*/
int ompi_comm_group ( ompi_communicator_t* comm, ompi_group_t **group )
{
    /* increment proc reference counters */
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
int ompi_comm_create ( ompi_communicator_t *comm, ompi_group_t *group, 
                      ompi_communicator_t **newcomm )
{
    ompi_communicator_t *newcomp;
    int rsize;
    int mode;
    int *allranks=NULL;
    ompi_proc_t **rprocs=NULL;
    int rc = OMPI_SUCCESS;
    
    if ( OMPI_COMM_IS_INTER(comm) ) {
#ifdef HAVE_COMM_CREATE_FOR_INTERCOMMS
        int tsize, i, j;

        tsize = comm->c_remote_group->grp_proc_count;
        allranks = (int *) malloc ( tsize * sizeof(int));
        if ( NULL == allranks ) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }
        
        rc = comm->c_coll.coll_allgather_inter ( &group->grp_my_rank, 
                                                 1, MPI_INT, allranks, 
                                                 1, MPI_INT, comm );
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }

        /* Count number of procs in future remote group */
        for (rsize=0, i = 0; i < tsize; i++) {
            if ( MPI_PROC_NULL != allranks[i] ) {
                rsize++;
            }
        }

        /* If any of those groups is empty, we have to return
           MPI_COMM_NULL */
        if ( 0 == rsize || 0 == group->grp_proc_count ) {
            *newcomm = MPI_COMM_NULL;
            rc = OMPI_SUCCESS;
            goto exit;
        }

        /* Set proc-pointers for remote group */
        rprocs = (ompi_proc_t **) malloc ( sizeof(ompi_proc_t *) * rsize);
        if ( NULL == rprocs ) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }
        for ( j = 0, i = 0; i < rsize; i++ ) {
            if ( MPI_PROC_NULL != allranks[i] ) {
                rprocs[j] = comm->c_remote_group->grp_proc_pointers[i];
                j++;
            }
        }                                           
        mode = OMPI_COMM_INTER_INTER;
#else
        return ( MPI_ERR_COMM );
#endif
    }
    else {
        rsize  = 0;
        rprocs = NULL;
        mode = OMPI_COMM_INTRA_INTRA;
    }

    newcomp = ompi_comm_set ( mode,                     /* mode */
                             comm,                     /* old comm */
                             NULL,                     /* bridge comm */
                             group->grp_proc_count,    /* local_size */
                             group->grp_proc_pointers, /* local_procs*/
                             rsize,                    /* remote_size */
                             rprocs,                   /* remote_procs */
                             NULL,                     /* attrs */
                             comm->error_handler,      /* error handler */
                             NULL,                     /* coll module */
                             NULL,                     /* topo module */
                             MPI_UNDEFINED,            /* local leader */
                             MPI_UNDEFINED             /* remote leader */
                             );

    if ( newcomp == MPI_COMM_NULL ) {
        rc = MPI_ERR_INTERN;
        goto exit;
    }

    /* Check whether we are part of the new comm.
       If not, we have to free the structure again.
       However, we could not avoid the comm_nextcid step, since
       all processes of the original comm have to participate in
       that function call. Additionally, all errhandler stuff etc.
       has to be set to make ompi_comm_free happy */
    if ( MPI_PROC_NULL == newcomp->c_local_group->grp_my_rank ) {
        ompi_comm_free ( &newcomp );
    }

 exit:
    if ( NULL != allranks ) {
        free ( allranks );
    }
    if ( NULL != rprocs ) {
        free ( rprocs );
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
int ompi_comm_split ( ompi_communicator_t* comm, int color, int key, 
                     ompi_communicator_t **newcomm )
{
    int myinfo[2];
    int size, my_size;
    int my_rsize;
    int mode;
#ifdef HAVE_COMM_SPLIT_FOR_INTERCOMMS
    int rsize;
#endif
    int i, loc;
    int *results=NULL, *sorted=NULL; 
    int *rresults=NULL, *rsorted=NULL; 
    int rc=OMPI_SUCCESS;
    ompi_proc_t **procs=NULL, **rprocs=NULL;
    ompi_communicator_t *newcomp;
    
    /* Step 1: determine all the information for the local group */
    /* --------------------------------------------------------- */

    /* sort according to color and rank. Gather information from everyone */
    myinfo[0] = color;
    myinfo[1] = key;

    size      = ompi_comm_size ( comm );
    results   = (int*) malloc ( 2 * size * sizeof(int));
    if ( NULL == results ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    rc = comm->c_coll.coll_allgather_intra ( myinfo, 2, MPI_INT,
                                             results, 2, MPI_INT, comm );
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }
        
    /* how many have the same color like me */
    for ( my_size = 0, i=0; i < size; i++) {
        if ( results[(2*i)+0] == color) my_size++;
    }

    sorted = (int *) malloc ( sizeof( int ) * my_size * 2);
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
    procs = (ompi_proc_t **) malloc ( sizeof(ompi_proc_t *) * my_size);
    if ( NULL == procs ) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }
    for (i = 0; i < my_size; i++) {
        procs[i] = comm->c_local_group->grp_proc_pointers[sorted[i*2]];
    }  
            
    /* Step 2: determine all the information for the remote group */
    /* --------------------------------------------------------- */
    if ( OMPI_COMM_IS_INTER(comm) ) {
#ifdef HAVE_COMM_SPLIT_FOR_INTERCOMMS
        rsize    = comm->c_remote_group->grp_proc_count;
        rresults = (int *) malloc ( rsize * 2 * sizeof(int));
        if ( NULL == rresults ) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }

        rc = comm->c_coll.coll_allgather_inter ( myinfo, 2, MPI_INT,
                                                 rresults, 2, MPI_INT,
                                                 comm );
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }

        /* how many have the same color like me */
        for ( my_rsize = 0, i=0; i < rsize; i++) {
            if ( rresults[(2*i)+0] == color) my_rsize++;
        }
        rsorted = (int *) malloc ( sizeof( int ) * my_rsize * 2);
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
        if(my_rsize>1) {
            qsort ((int*)rsorted, my_rsize, sizeof(int)*2, rankkeycompare);
        }

        /* put group elements in a list */
        rprocs = (ompi_proc_t **) malloc ( sizeof(ompi_proc_t *) * my_rsize);
        if ( NULL == procs ) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }
        for (i = 0; i < my_rsize; i++) {
            rprocs[i] = comm->c_remote_group->grp_proc_pointers[rsorted[i*2]];
        }  

        mode = OMPI_COMM_INTER_INTER;
#else
        /* creating an inter-communicator using MPI_Comm_create will
           be supported soon, but not in this version */
        rc = MPI_ERR_COMM;
        goto exit;
#endif
    }
    else {
        my_rsize  = 0;
        rprocs = NULL;
        mode   = OMPI_COMM_INTRA_INTRA;
    }
    
    
    /* Step 3: set up the communicator                           */
    /* --------------------------------------------------------- */
    /* Create the communicator finally */
    newcomp = ompi_comm_set ( mode,               /* mode */
                             comm,               /* old comm */
                             NULL,               /* bridge comm */
                             my_size,            /* local_size */
                             procs,              /* local_procs*/
                             my_rsize,           /* remote_size */
                             rprocs,             /* remote_procs */
                             NULL,               /* attrs */
                             comm->error_handler,/* error handler */
                             NULL,               /* coll module */
                             NULL,               /* topo module */
                             MPI_UNDEFINED,      /* local leader */
                             MPI_UNDEFINED       /* remote leader */
                             );
    if ( MPI_COMM_NULL == newcomp ) rc = MPI_ERR_INTERN;

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
    if ( NULL != procs   ) {
        free ( procs );
    }
    if ( NULL != rprocs  ) {
        free ( rprocs );
    }


    /* Step 4: if we are not part of the comm, free the struct   */
    /* --------------------------------------------------------- */
    if ( MPI_UNDEFINED == color ) {
        ompi_comm_free ( &newcomp );
    }

    *newcomm = newcomp;
    return ( rc );
}


/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/*
** Counterpart to MPI_Comm_free. To be used within OMPI.
** The freeing of all attached objects (groups, errhandlers
** etc. ) has moved to the destructor. 
*/
int ompi_comm_free ( ompi_communicator_t **comm )
{

#if 0
    /* Release attributes */
    ompi_attr_delete_all ( COMM_ATTR, comm );
#endif

    /* Release the communicator */
    OBJ_RELEASE ( comm );

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
    int local_rank, local_size;
    ompi_proc_t **rprocs;
    ompi_job_handle_t job;
    uint32_t *rvpids=NULL, *vpids=NULL;
    int rc, i;
    
    local_rank = ompi_comm_rank ( local_comm );

    vpids  = (uint32_t *) malloc ( local_size * sizeof(uint32_t));
    rvpids = (uint32_t *) malloc ( rsize * sizeof(uint32_t));
    rprocs = (ompi_proc_t **) malloc ( rsize * sizeof(ompi_proc_t *));
    if ( NULL == vpids || NULL == rvpids || NULL == rprocs ) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto err_exit;
    }
    
    if ( local_rank == local_leader ) {
        MPI_Request req;
        MPI_Status status;
        /* generate vpid list */
        for ( i = 0; i < local_size; i++ ){
            vpids[i] = (uint32_t) local_comm->c_local_group->grp_proc_pointers[i]->proc_vpid;
        }
        
        
        /* local leader exchange group sizes and vpid lists */
        rc = mca_pml.pml_irecv (rvpids, rsize, MPI_UNSIGNED, remote_leader, tag,
                                bridge_comm, &req );
        if ( rc != MPI_SUCCESS ) {
            goto err_exit;
        }
        rc = mca_pml.pml_send (vpids, local_size, MPI_UNSIGNED, remote_leader, tag, 
                               MCA_PML_BASE_SEND_STANDARD, bridge_comm );
        if ( rc != MPI_SUCCESS ) {
            goto err_exit;
        }
        rc = mca_pml.pml_wait ( 1, &req, NULL, &status);
        if ( rc != MPI_SUCCESS ) {
            goto err_exit;
        }
    }

    rc = local_comm->c_coll.coll_bcast_intra ( rvpids, rsize, MPI_UNSIGNED, 
                                         local_leader, local_comm );
    if ( rc != MPI_SUCCESS ) {
        goto err_exit;
    }
    
    /* determine according proc-list */
    if(NULL == (job = mca_pcm.pcm_handle_get())) {
        return NULL;
    }

    for ( i = 0; i < rsize; i++ ) {
        rprocs[i] = ompi_proc_find ( job, rvpids[i] );
    }

 err_exit:
    if ( NULL != vpids ) {
        free ( vpids );
    }
    if ( NULL != rvpids) {
        free ( rvpids );
    }
    /* rprocs has to be freed in the level above (i.e. intercomm_create ) */

return rprocs;
}
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
int ompi_comm_determine_first ( ompi_communicator_t *intercomm, int high )
{
    int flag, rhigh;
    int local_rank, rc;
    ompi_proc_t *lvpid, *rvpid;

    lvpid = intercomm->c_local_group->grp_proc_pointers[0];
    rvpid = intercomm->c_remote_group->grp_proc_pointers[0];
    local_rank = ompi_comm_rank ( intercomm );

    /*
     * determine maximal high value over the intercomm
     */
    if ( lvpid->proc_vpid > rvpid->proc_vpid ) {
        if ( 0 == local_rank  ) {
            rc = intercomm->c_coll.coll_bcast_inter ( &high, 1, MPI_INT, MPI_ROOT,
                                                      intercomm );
        }
        else {
            rc = intercomm->c_coll.coll_bcast_inter ( &high, 1, MPI_INT, MPI_PROC_NULL,
                                                      intercomm );
        }
        if ( rc != MPI_SUCCESS ) {
            return rc;
        }

        rc = intercomm->c_coll.coll_bcast_inter ( &rhigh, 1, MPI_INT, 0, intercomm );
        if ( rc != MPI_SUCCESS ) {
            return rc;
        }
    }
    else {
        rc = intercomm->c_coll.coll_bcast_inter ( &rhigh, 1, MPI_INT, 0, intercomm );
        if ( rc != MPI_SUCCESS ) {
            return rc;
        }

        if ( 0 == local_rank  ) {
            rc = intercomm->c_coll.coll_bcast_inter ( &high, 1, MPI_INT, MPI_ROOT,
                                                      intercomm );
        }
        else { 
            rc = intercomm->c_coll.coll_bcast_inter ( &high, 1, MPI_INT, MPI_PROC_NULL,
                                                      intercomm );
        }
        if ( rc != MPI_SUCCESS ) {
            return rc;
        }

    }


    /* This is the logic for determining who is first, who is second */
    if ( high && !rhigh ) {
        flag = true;
    }
    else if ( !high && rhigh ) {
        flag = false;
    }
    else {
        
        if ( lvpid > rvpid ) {
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
    mca_pml_ptl_comm_t *pml_comm;
    mca_ptl_sequence_t *seq;
    ompi_list_t *list;
    int i;

    printf("Dumping information for comm_cid %d\n", comm->c_contextid);
    printf("  f2c index:%d cube_dim: %d\n", comm->c_f_to_c_index,  
           comm->c_cube_dim);
    printf("  Local group: size = %d my_rank = %d\n", 
           comm->c_local_group->grp_proc_count, 
           comm->c_local_group->grp_my_rank );

    printf("  Communicator is:");
    /* Display flags */
    if ( OMPI_COMM_IS_INTER(comm) )
        printf(" inter-comm,");
    if ( OMPI_COMM_IS_CART(comm))
        printf(" topo-cart,");
    if ( OMPI_COMM_IS_GRAPH(comm))
        printf(" topo-graph");
    printf("\n");

    if (OMPI_COMM_IS_INTER(comm)) {
        printf("  Remote group size:%d\n", comm->c_remote_group->grp_proc_count);
    }


    /* Dump the c_pml_comm->c_unexpexted_frags for each process */
    pml_comm = (mca_pml_ptl_comm_t *)comm->c_pml_comm;
    seq      = (mca_ptl_sequence_t *) pml_comm->c_frags_cant_match;
    for ( i = 0; i < comm->c_local_group->grp_proc_count; i++ ){
        list = (ompi_list_t *)seq+i;
        printf("%d: head->list_next:%p head->list_prev:%p"
               "    tail->list_next:%p tail->list_next:%p\n",
               i,
               (char*)list->ompi_list_head.ompi_list_next,
               (char*)list->ompi_list_head.ompi_list_prev, 
               (char*)list->ompi_list_tail.ompi_list_next, 
               (char*)list->ompi_list_tail.ompi_list_prev );
        fflush(stdout);
    }
    
    return MPI_SUCCESS;
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

