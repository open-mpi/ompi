/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 */

#include "ompi_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include "opal/mca/hwloc/hwloc.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/dss/dss_internal.h"
#include "opal/class/opal_object.h"

#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/mca/grpcomm/grpcomm.h"

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "sbgp_basesmsocket.h"

#include "ompi/mca/common/commpatterns/common_coll_ops.h"


/*
 * Public string showing the coll ompi_sm V2 component version number
 */
const char *mca_sbgp_basesmsocket_component_version_string =
    "Open MPI sbgp - basesmsocket collective MCA component version " OMPI_VERSION;


/*
 * Local functions
 */

static int basesmsocket_open(void);
static int basesmsocket_close(void);
static mca_sbgp_base_module_t *mca_sbgp_basesmsocket_select_procs(struct ompi_proc_t ** procs,
        int n_procs_in,
        struct ompi_communicator_t *comm,
        char *key,
        void *output_data
        );
static int mca_sbgp_basesmsocket_init_query(bool enable_progress_threads,
        bool enable_mpi_threads);
/*----end local functions ----*/

static inline int mca_sbgp_basesmsocket_param_register_int(
        const char* param_name, int default_value)
{
    int id = mca_base_param_register_int("sbgp", "basesmsocket", param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_sbgp_basesmsocket_component_t mca_sbgp_basesmsocket_component = {

    /* First, fill in the super */

    {
        /* First, the mca_component_t struct containing meta
           information about the component itself */

        {
            MCA_SBGP_BASE_VERSION_2_0_0,

            /* Component name and version */

            "basesmsocket",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,

            /* Component open and close functions */

            basesmsocket_open,
            basesmsocket_close,
        },

    mca_sbgp_basesmsocket_init_query,
    mca_sbgp_basesmsocket_select_procs,

    /* (default) priority */
    0
    }

};

/*
 * Open the component
 */
static int basesmsocket_open(void)
{

    /* local variables */
    mca_sbgp_basesmsocket_component_t *cs = &mca_sbgp_basesmsocket_component;

    /* set component priority */
    cs->super.priority=
        mca_sbgp_basesmsocket_param_register_int("priority",90);

    return OMPI_SUCCESS;
}

/*
 * Close the component
 */
static int basesmsocket_close(void)
{
    return OMPI_SUCCESS;
}

/* query to see if the component is available for use, and can
 * satisfy the thread and progress requirements
 */
int mca_sbgp_basesmsocket_init_query(bool enable_progress_threads,
        bool enable_mpi_threads)
{
    /* at this stage there is no reason to disaulify this component */

    /* done */
    return OMPI_SUCCESS;
}

static int mca_sbgp_map_to_logical_socket_id(int *socket)
{
    int ret = OMPI_SUCCESS;
    hwloc_obj_t obj;
    hwloc_obj_t first_pu_object;
    hwloc_bitmap_t good;
    int pu_os_index = -1, my_logical_socket_id = -1;
    int this_pus_logical_socket_id = -1;

    *socket = my_logical_socket_id;

    /* bozo check */
    if (NULL == opal_hwloc_topology) {
        return OPAL_ERR_NOT_INITIALIZED;
    }

    good = hwloc_bitmap_alloc();
    if (NULL == good) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* get this process' CPU binding */
    if( 0 !=  hwloc_get_cpubind(opal_hwloc_topology,good, 0)){
        /* report some error */
        BASESMSOCKET_VERBOSE(10, ("The global variable opal_hwloc_topology appears not to have been initialized\n"));
        return OMPI_ERROR;
    }

    /* find the first logical PU object in the hwloc tree */
    first_pu_object = hwloc_get_obj_by_type(opal_hwloc_topology, HWLOC_OBJ_PU, 0);


    /* get the next bit in the bitmap (note: if pu_os_index == -1, then the 
     * first bit is returned 
     */
     /* traverse the hwloc tree */
     while( -1 != (pu_os_index = hwloc_bitmap_next(good, pu_os_index) ) ) {
         /* Traverse all PUs in the machine in logical order, in the simple case 
          * there should only be a single PU that this process is bound to, right?
          *
          */
          for( obj = first_pu_object; obj != NULL; obj = obj->next_cousin ) {/* WTF is a "next_cousin" ? */ 
              /* is this PU the same as the bit I pulled off the mask? */
              if( obj->os_index == (unsigned int) pu_os_index) {
                  /* Then I found it, break out of for loop */
                  break;
              }
          }

          if( NULL != obj) {
              /* if we found the PU, then go upward in the tree
               * looking for the enclosing socket 
               */
               while( (NULL != obj) && ( HWLOC_OBJ_SOCKET != obj->type) ){
                   obj = obj->parent;
               }

               if( NULL == obj ) {
                   /* then we couldn't find an enclosing socket, report this */
               } else {
                   /* We found the enclosing socket */
                   if( -1 == my_logical_socket_id ){
                       /* this is the first PU that I'm bound to */
                       this_pus_logical_socket_id = obj->logical_index;
                       my_logical_socket_id = this_pus_logical_socket_id;
                   } else {
                       /* this is not the first PU that I'm bound to. 
                        * Seems I'm bound to more than a single PU. Question
                        * is, am I bound to the same socket?? 
                        */
                       /* in order to get rid of the compiler warning, I had to cast 
                        * "this_pus_logical_socket_id", at a glance this seems ok, 
                        * but if subgrouping problems arise, maybe look here. I shall 
                        * tag this line with the "mark of the beast" for grepability
                        * 666
                        */  
                        if( (unsigned int) this_pus_logical_socket_id != obj->logical_index ){
                            /* 666 */
                            /* Then we're bound to more than one socket...fail */
                            this_pus_logical_socket_id = -1;
                            my_logical_socket_id = -1;
                            break;
                        }
                   }
               }

          }

          /* end while */
     }
     *socket = my_logical_socket_id;

     return ret;

}


/* This routine is used to find the list of procs that run on the
** same host as the calling process.
*/

static mca_sbgp_base_module_t *mca_sbgp_basesmsocket_select_procs(struct ompi_proc_t ** procs,
    int n_procs_in,
    struct ompi_communicator_t *comm,
    char *key,
    void *output_data
    )
{
    /* local variables */
    mca_sbgp_basesmsocket_module_t *module;
    int ret;
    int my_socket_index;
    int proc, cnt, local, n_local_peers, my_index, my_rank;
    ompi_proc_t* my_proc;
    int *local_ranks_in_comm=NULL;
    int *socket_info=NULL, my_socket_info;
    int  i_cnt, lp_cnt, my_local_index, comm_size=ompi_comm_size(comm);

    /* initialize data */
    output_data=NULL;
    my_rank=ompi_comm_rank(comm);
    my_proc=ompi_comm_peer_lookup(comm,my_rank);
    for( proc=0 ; proc < n_procs_in ; proc++) {
        if( procs[proc]==my_proc){
            my_index=proc;
        }
    }

    /*create a new module*/
    module=OBJ_NEW(mca_sbgp_basesmsocket_module_t);
    if (!module ) {
        return NULL;
    }
    module->super.group_size=0;
    module->super.group_comm = comm;
    module->super.group_list = NULL;
    module->super.group_net = OMPI_SBGP_SOCKET;

    /* test to see if process is bound */
    if( OPAL_BIND_TO_NONE == OPAL_GET_BINDING_POLICY(opal_hwloc_binding_policy) ) {

        /* pa affinity not set, so socket index will be set to -1 */
        my_socket_index=-1;
        /*debug print*/
        /* */
        BASESMSOCKET_VERBOSE(10, ("[%d] FAILED to set basesmsocket group, processes are not bound!!!\n",my_rank));
        /*end debug*/
        goto NoLocalPeers;
    } else {

        my_socket_index=-1;
        /* this should find my logical socket id which is the socket id we want 
         * physical socket ids are not necessarily unique, logical ones, as defined
         * by the hwloc API are unique. 
         */
        if( OMPI_SUCCESS != mca_sbgp_map_to_logical_socket_id(&my_socket_index)){
            BASESMSOCKET_VERBOSE(10, ("[%d] FAILED to set basesmsocket group !!!\n",my_rank));

            goto NoLocalPeers;
        }
    }

    /* Debug prints */
    /*
       {
       fprintf(stderr,"Number of processors per node: %d\n",num_processors);
       fprintf(stderr,"I am rank %d and my socket index is %d\n and my core index is %d\n",my_rank,my_socket_index,core_index);
       fprintf(stderr,"n_proc_in = %d\n",n_procs_in);
       fprintf(stderr,"\n");
       fflush(stderr);
       }
       end debug prints */


    /*get my socket index*/
    cnt=0;
    for( proc=0 ; proc < n_procs_in ; proc++) {
        local=OPAL_PROC_ON_LOCAL_NODE(procs[proc]->proc_flags);
        if( local ) {
            cnt++;
        }
    }
     /*debug print */
    /*
    fprintf(stderr,"Number of local processors %d\n",cnt);
    end debug print*/

    /* if no other local procs found skip to end */
    if( 1 >= cnt ) {
      goto NoLocalPeers;
    }



    /* allocate structure to hold the list of local ranks */
    local_ranks_in_comm=(int *)malloc(sizeof(int)*cnt);
    if(NULL == local_ranks_in_comm ){
        goto Error;
    }
    /* figure out which ranks from the input communicator - comm - will
     * particiapte in the local socket determination.
     */

    n_local_peers=0;
    i_cnt=0;
    for( proc = 0; proc < n_procs_in; proc++){
        local = OPAL_PROC_ON_LOCAL_NODE(procs[proc]->proc_flags);
        if ( local ) {

            /* set the rank within the on-host ranks - this will be used for tha
             * allgather
             */
            if( my_proc == procs[proc] ) {
                my_local_index=n_local_peers;
            }
            /* find the rank of the current proc in comm.  We take advantage
             * of the fact that ranks in a group have the same relative
             * ordering as they do within the communicator.
             */
            for( lp_cnt=proc; lp_cnt < comm_size ; lp_cnt++ ) {
                if(procs[proc] == ompi_comm_peer_lookup(comm,lp_cnt) ){
                    local_ranks_in_comm[i_cnt]=lp_cnt;
                    /* lp_cnt has alrady been checked */
                    i_cnt++;
                    /* found the corresponding rank in comm, so don't need
                     * to search any more */
                    break;
                }
                /*i_cnt++;*/
                /*fprintf(stderr,"QQQ i_cnt %d \n",i_cnt);*/
            }
            n_local_peers++;
        }
        }
        /*fprintf(stderr,"YYY n_local_peers %d\n",n_local_peers);*/
        socket_info=(int *)malloc(sizeof(int)*n_local_peers);
        /*fprintf(stderr,"XXX got socket info\n");*/
        if(NULL == socket_info ){
            goto Error;
        }

        my_socket_info=my_socket_index;

        /* Allgather data over the communicator */
        ret=comm_allgather_pml(&my_socket_info, socket_info, 1,
                MPI_INT, my_local_index, n_local_peers, local_ranks_in_comm,comm);
        if (OMPI_SUCCESS != ret ) {
            BASESMSOCKET_VERBOSE(10, ("comm_allgather_pml returned error %d\n",ret));
            return NULL;
        }


        /*allocate memory to the group_list probably an overestimation
          of the necessary resources */
        module->super.group_list=(int *)malloc(sizeof(int)*cnt);
        if(NULL == module->super.group_list){
            goto Error;
        }

        /* figure out who is sharing the same socket */
        cnt=0;
        for (proc = 0; proc < n_local_peers; proc++) {
            int rem_rank=local_ranks_in_comm[proc];
            int rem_socket_index=socket_info[proc];

            /*Populate the list*/
            if (rem_socket_index == my_socket_index) {
                module->super.group_list[cnt]=rem_rank;
                cnt++;
            }
        }

        module->super.group_size=cnt;

    /*debug print*/
    
    {
        int ii;
        fprintf(stderr,"Ranks per socket: %d\n",cnt);
        fprintf(stderr,"Socket %d owns ranks: ", my_socket_index);
        for (ii=0; ii < cnt; ii++)
            fprintf(stderr,"%d ",module->super.group_list[ii]);
        fprintf(stderr,"\n");
        fflush(stderr);
    }

   /* end debug*/


    /*Free resources*/
    free(local_ranks_in_comm);
    free(socket_info);

    /*Return the module*/
    return (mca_sbgp_base_module_t *) module;


NoLocalPeers:
    /* nothing to store, so just free the module and return */
    /*fprintf(stderr,"No local socket peers\n");*/
    /*free(module);*/
    if(socket_info) {
        free(socket_info);
        socket_info=NULL;
    }
    if(local_ranks_in_comm){
        free(local_ranks_in_comm);
    }
    OBJ_RELEASE(module);
    return NULL;

Error:
    /*clean up*/
    if( NULL != module->super.group_list){
        free(module->super.group_list);
        module->super.group_list=NULL;
    }
    if(socket_info) {
        free(socket_info);
        socket_info=NULL;
    }
    if(local_ranks_in_comm){
        free(local_ranks_in_comm);
    }
    OBJ_RELEASE(module);
    return NULL;


}



#if 0
static int mca_sbgp_map_to_socket_core(int processor_id, int *socket, int *core)
{
    int ret = OPAL_ERR_NOT_FOUND;
    hwloc_obj_t obj;
    hwloc_topology_t *t;
    hwloc_bitmap_t good;

    /* bozo check */
    if (NULL == opal_hwloc_topology) {
        return OPAL_ERR_NOT_INITIALIZED;
    }
    t = &opal_hwloc_topology;

    good = hwloc_bitmap_alloc();
    if (NULL == good) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Iterate through every core and find one that contains the
       processor_id.  Then find the corresponding socket. */
    for (obj = hwloc_get_next_obj_by_type(*t, HWLOC_OBJ_CORE, NULL);
            NULL != obj;
            obj = hwloc_get_next_obj_by_type(*t, HWLOC_OBJ_CORE, obj)) {
        hwloc_bitmap_and(good, obj->online_cpuset,
                obj->allowed_cpuset);

        /* Does this core contain the processor_id in question? */
        if (hwloc_bitmap_isset(good, processor_id)) {
            *core = obj->os_index;

            /* Go upward from the core object until we find its parent
               socket. */
            while (HWLOC_OBJ_SOCKET != obj->type) {
                if (NULL == obj->parent) {
                    /* If we get to the root without finding a socket,
                       er..  Hmm.  Error! */
                    ret = OPAL_ERR_NOT_FOUND;
                    goto out;
                }
                obj = obj->parent;
            }
            *socket = obj->os_index;
            ret = OPAL_SUCCESS;
            goto out;
        }
    }

    /* If we didn't even find the right core, we didn't find it.  Fall
       through. */
    ret = OPAL_ERR_NOT_FOUND;

out:
    hwloc_bitmap_free(good);
    return ret;
}
#endif

#if 0
static mca_sbgp_base_module_t *mca_sbgp_basesmsocket_select_procs(struct ompi_proc_t ** procs,
    int n_procs_in,
    struct ompi_communicator_t *comm,
    char *key,
    void *output_data
    )
{
    /* local variables */
    mca_sbgp_basesmsocket_module_t *module;
    /*
    opal_buffer_t* sbuffer = OBJ_NEW(opal_buffer_t);
    opal_buffer_t* rbuffer = OBJ_NEW(opal_buffer_t);
    */
    opal_paffinity_base_cpu_set_t my_cpu_set;
    bool bound;
    int ret;
    int num_processors;
    int socket_tmp;
    int my_socket_index;
    int core_index=-1;
    int proc, cnt, local, n_local_peers, my_index, my_rank;
    ompi_proc_t* my_proc;
    int *local_ranks_in_comm=NULL;
    int *socket_info=NULL, my_socket_info;
    int  i_cnt, lp_cnt, my_local_index, comm_size=ompi_comm_size(comm);

    /* initialize data */
    output_data=NULL;
    my_rank=ompi_comm_rank(comm);
    my_proc=ompi_comm_peer_lookup(comm,my_rank);
    for( proc=0 ; proc < n_procs_in ; proc++) {
        if( procs[proc]==my_proc){
            my_index=proc;
        }
    }

    /*create a new module*/
    module=OBJ_NEW(mca_sbgp_basesmsocket_module_t);
    if (!module ) {
        return NULL;
    }
    module->super.group_size=0;
    module->super.group_comm = comm;
    module->super.group_list = NULL;
    module->super.group_net = OMPI_SBGP_SOCKET;

/*
    ** get my process affinity information
    ** */

    /* get the number of processors on this node */

    ret=opal_paffinity_base_get_processor_info(&num_processors);

    /* get process affinity mask */
    OPAL_PAFFINITY_CPU_ZERO(my_cpu_set);
    ret=opal_paffinity_base_get(&my_cpu_set);
    OPAL_PAFFINITY_PROCESS_IS_BOUND(my_cpu_set,&bound);

     /*debug process affinity*/
    /*
    {
        ret=opal_paffinity_base_get_socket_info(&num_socket);
        fprintf(stderr,"Number of sockets %d\n",num_socket);
        fprintf(stderr,"Test if rank %d is bound %d\n", my_rank, bound);
        fprintf(stderr,"return from opal_paffinity_base_get: %d\n\n",ret);
        fprintf(stderr,"bitmask elements: ");
        unsigned int long  jj;
        for(jj=0; jj < OPAL_PAFFINITY_BITMASK_NUM_ELEMENTS; jj++)
                 fprintf(stderr," %d ",my_cpu_set.bitmask[jj]);
        fprintf(stderr,"\n");
        fflush(stderr);
    }
    end debug process affinity*/

    if( !bound ) {

        /* pa affinity not set, so socket index will be set to -1 */
        my_socket_index=-1;
        /*debug print*/
        /* */
        fprintf(stderr,"[%d]FAILED to set basesmsocket group !!!\n",my_rank);
        fflush(stderr);
        /*end debug*/
        goto NoLocalPeers;
    } else {

        my_socket_index=-1;
        /* loop over number of processors */
        for ( proc=0 ; proc < num_processors ; proc++ ) {
            if (OPAL_PAFFINITY_CPU_ISSET(proc,my_cpu_set)) {
        ret=opal_paffinity_base_get_map_to_socket_core(proc,&socket_tmp,&core_index);
        if( my_socket_index != socket_tmp ) {
                        my_socket_index=socket_tmp;
                        break;
        }
        }
    } /* end of proc loop */
    }

    /* Debug prints */
       /*
    {
     fprintf(stderr,"Number of processors per node: %d\n",num_processors);
     fprintf(stderr,"I am rank %d and my socket index is %d\n and my core index is %d\n",my_rank,my_socket_index,core_index);
     fprintf(stderr,"n_proc_in = %d\n",n_procs_in);
     fprintf(stderr,"\n");
     fflush(stderr);
    }
       end debug prints */


    /*get my socket index*/
    cnt=0;
    for( proc=0 ; proc < n_procs_in ; proc++) {
    local=OPAL_PROC_ON_LOCAL_NODE(procs[proc]->proc_flags);
    if( local ) {
      cnt++;
    }
    }
     /*debug print */
    /*
    fprintf(stderr,"Number of local processors %d\n",cnt);
    end debug print*/

    /* if no other local procs found skip to end */
    if( 1 >= cnt ) {
      goto NoLocalPeers;
    }


#if 0
    int *local_ranks_in_comm;
    int32_t *socket_info, *my_socket_info;
    int  my_local_index;
#endif
    /* allocate structure to hold the list of local ranks */
    local_ranks_in_comm=(int *)malloc(sizeof(int)*cnt);
    if(NULL == local_ranks_in_comm ){
    goto Error;
    }
    /* figure out which ranks from the input communicator - comm - will
     * particiapte in the local socket determination.
     */

    n_local_peers=0;
    i_cnt=0;
    for( proc = 0; proc < n_procs_in; proc++){
        local = OPAL_PROC_ON_LOCAL_NODE(procs[proc]->proc_flags);
        if ( local ) {

            /* set the rank within the on-host ranks - this will be used for tha
             * allgather
             */
            if( my_proc == procs[proc] ) {
                my_local_index=n_local_peers;
            }
            /* find the rank of the current proc in comm.  We take advantage
             * of the fact that ranks in a group have the same relative
             * ordering as they do within the communicator.
             */
#if 1
            /*for( lp_cnt=i_cnt; lp_cnt < comm_size ; lp_cnt++ ) {*/
            for( lp_cnt=proc; lp_cnt < comm_size ; lp_cnt++ ) {
                if(procs[proc] == ompi_comm_peer_lookup(comm,lp_cnt) ){
                    local_ranks_in_comm[i_cnt]=lp_cnt;
                    /* lp_cnt has alrady been checked */
                    i_cnt++;
                    /* found the corresponding rank in comm, so don't need
                     * to search any more */
                    break;
                }
                /*i_cnt++;*/
                /*fprintf(stderr,"QQQ i_cnt %d \n",i_cnt);*/
            }
#endif
            n_local_peers++;
        }
    }
    /*fprintf(stderr,"YYY n_local_peers %d\n",n_local_peers);*/
    socket_info=(int *)malloc(sizeof(int)*n_local_peers);
    /*fprintf(stderr,"XXX got socket info\n");*/
    if(NULL == socket_info ){
    goto Error;
    }

    my_socket_info=my_socket_index;

    /* Allgather data over the communicator */
    ret=comm_allgather_pml(&my_socket_info, socket_info, 1,
            MPI_INT, my_local_index, n_local_peers, local_ranks_in_comm,comm);
    if (OMPI_SUCCESS != ret ) {
        fprintf(stderr," comm_allgather_pml returned error %d \n", ret);
        fflush(stderr);
        return NULL;
    }


    /*allocate memory to the group_list probably an overestimation
      of the necessary resources */
    module->super.group_list=(int *)malloc(sizeof(int)*cnt);
    if(NULL == module->super.group_list){
    goto Error;
    }

    /* figure out who is sharing the same socket */
    cnt=0;
    for (proc = 0; proc < n_local_peers; proc++) {
               int rem_rank=local_ranks_in_comm[proc];
               int rem_socket_index=socket_info[proc];

        /*Populate the list*/
        if (rem_socket_index == my_socket_index) {
                module->super.group_list[cnt]=rem_rank;
                cnt++;
        }
    }

    module->super.group_size=cnt;

    /*debug print*/
    /*
    {
        int ii;
        fprintf(stderr,"Ranks per socket: %d\n",cnt);
        fprintf(stderr,"Socket %d owns ranks: ", my_socket_index);
        for (ii=0; ii < cnt; ii++)
            fprintf(stderr,"%d ",module->super.group_list[ii]);
        fprintf(stderr,"\n");
        fflush(stderr);
    }

    {
        cpu_set_t set;
        unsigned int len = sizeof(set);
        int i;
        unsigned long mask = 0;
        CPU_ZERO(&set);
        if (sched_getaffinity(0, len, &set) < 0) {
            perror("sched_getaffinity");
            return -1;
        }
        for (i = 0; i < CPU_SETSIZE; i++) {
            int cpu = CPU_ISSET(i, &set);
            if (cpu) {
                mask |= 1<< i;
            }
        }
        opal_output(0,"%d: my affinity mask is: %08lx\n", my_local_index,mask);
    }


    end debug*/


    /*Free resources*/
    free(local_ranks_in_comm);
    free(socket_info);

    /*Return the module*/
    return (mca_sbgp_base_module_t *) module;


NoLocalPeers:
    /* nothing to store, so just free the module and return */
    /*fprintf(stderr,"No local socket peers\n");*/
    /*free(module);*/
    if(socket_info) {
        free(socket_info);
        socket_info=NULL;
    }
    if(local_ranks_in_comm){
        free(local_ranks_in_comm);
    }
    OBJ_RELEASE(module);
    return NULL;

Error:
    /*clean up*/
    if( NULL != module->super.group_list){
        free(module->super.group_list);
        module->super.group_list=NULL;
    }
    if(socket_info) {
        free(socket_info);
        socket_info=NULL;
    }
    if(local_ranks_in_comm){
        free(local_ranks_in_comm);
    }
    OBJ_RELEASE(module);
    return NULL;


}
#endif
