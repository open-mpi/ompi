/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * Most of the description of the data layout is in the
 * coll_sm_module.c file.
 */

#include "ompi_config.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "opal/util/show_help.h"
#include "coll_sm2.h"
#include "ompi/mca/coll/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/sys_info.h"


/*
 * Local functions
 */
static int sm2_module_enable(struct mca_coll_base_module_1_1_0_t *module,
        struct ompi_communicator_t *comm);

/*
 * Local functions
 */

static bool have_local_peers(ompi_group_t *group, size_t size)
{
    size_t i;
    ompi_proc_t *proc;

    for (i = 0; i < size; ++i) {
        proc = ompi_group_peer_lookup(group,i);
        if (0 == (proc->proc_flags & OMPI_PROC_FLAG_LOCAL)) {
            return false;
        }
    }
    return true;
}

/*
 * Create mmaped shared file
 */

static int allocate_shared_file(size_t size, char *file_name, 
        struct ompi_communicator_t *comm, char **sm_backing_file)
{
    int fd = -1;
    int group_size,my_rank;

    bool i_create_shared_file=false;
    size_t p;
    int rc=0, sm_file_inited=0;
    struct iovec iov[2]; 
    int sm_file_created;
    ompi_proc_t **comm_proc_list;

    /* get the list of procs */
    comm_proc_list=comm->c_local_group->grp_proc_pointers;

    group_size=ompi_comm_size(comm);
    my_rank=ompi_comm_rank(comm);

    /* determine who will actually create the file */
    if( my_rank == 0 ) {
        i_create_shared_file=true;
    }

    /* open the backing file. */
    if( i_create_shared_file ) {
        /* process initializing the file */
        fd = open(file_name, O_CREAT|O_RDWR, 0600);
        if (fd < 0) {
            opal_output(0,"mca_common_sm_mmap_init: open %s failed with errno=%d\n",                      
                        file_name, errno);
            goto file_opened;
        }
        /* map the file and initialize segment state */
        *sm_backing_file = (char *)
            mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
        if( (void*)-1 == sm_backing_file ) {
            opal_output(0, "mca_common_sm_mmap_init: mmap failed with errno=%d\n",
                    errno);
            goto file_opened;
        }

        /* truncate the file to the requested size */
        if(ftruncate(fd, size) != 0) {
            opal_output(0, 
                    "mca_common_sm_mmap_init: ftruncate failed with errno=%d\n",
                    errno);
            goto file_opened;
        }

        /* if we got this far, the file has been initialized correctly */
        sm_file_inited=1;

        file_opened:

        /* signal the rest of the local procs that the backing file
         * has been created - not very scalable, but for small shared
         * memory nodes is adequate for now
         */
        for(p=1 ; p < group_size ; p++ ) {
            sm_file_created=OMPI_RML_TAG_COLL_SM2_BACK_FILE_CREATED;
            iov[0].iov_base=&sm_file_created;
            iov[0].iov_len=sizeof(sm_file_created);
            iov[1].iov_base=&sm_file_inited;
            iov[1].iov_len=sizeof(sm_file_inited);
            rc=orte_rml.send(&(comm_proc_list[p]->proc_name),iov,2,
                OMPI_RML_TAG_COLL_SM2_BACK_FILE_CREATED,0);
            if( rc < 0 ) {
                opal_output(0,
                    "allocate_shared_file: orte_rml.send failed to %lu with errno=%d\n",
                    (unsigned long)p, errno);
                goto return_error;
            }
        }
        if ( 0 == sm_file_inited ) {
            /* error - the sm backing file did not get opened correctly */
            goto return_error;
        }
    } else {
        /* all other procs wait for the file to be initialized
           before using the backing file */
        iov[0].iov_base=&sm_file_created;
        iov[0].iov_len=sizeof(sm_file_created);
        iov[1].iov_base=&sm_file_inited;
        iov[1].iov_len=sizeof(sm_file_inited);
        rc=orte_rml.recv(&(comm_proc_list[0]->proc_name),iov,2,
              OMPI_RML_TAG_COLL_SM2_BACK_FILE_CREATED,0);
        if( rc < 0 ) {
            opal_output(0, "allocate_shared_file: orte_rml.recv failed from %ld with errno=%d\n",            
                        0L, errno);
            goto return_error;
        }
        /* check to see if file inited correctly */
        if( 0 == sm_file_inited ) {
            goto return_error;
        }

        /* open backing file */
        fd = open(file_name, O_RDWR, 0600);
            if (fd < 0) {
            opal_output(0,"mca_common_sm_mmap_init: open %s failed with errno=%d\n",                      
                        file_name, errno);
            goto return_error;
        }

        /* map the file and initialize segment state */
        *sm_backing_file = (char *)
            mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
        if( (void*)-1 == sm_backing_file ) {
            opal_output(0, "mca_common_sm_mmap_init: mmap failed with errno=%d\n",
                    errno);
            goto return_error;
        }
    
    } 

    /* enable access by other processes on this host */
    close(fd);

    return OMPI_SUCCESS;

  return_error:
    if( -1 != fd ) {
        close(fd);
    }

    if( NULL != sm_backing_file ) munmap((void*) sm_backing_file,size);

    return OMPI_ERROR;

}

/* setup an n-array tree */

static int setup_nary_tree(int tree_order, int my_rank, int num_nodes,
        tree_node_t *my_node)
{
    /* local variables */
    int n_levels, result;
    int my_level_in_tree, cnt, parent_cnt;
    int lvl,cum_cnt, my_rank_in_my_level,n_lvls_in_tree;
    int start_index,end_index;

    /* sanity check */
    if( 1 >= tree_order ) {
        goto Error;
    }

    my_node->my_rank=my_rank;
    my_node->tree_size=num_nodes;

    /* figure out number of levels in tree */
    n_levels=0;
    result=num_nodes-1;
    while (0 < result ) {
        result/=tree_order;
        n_levels++;
    };

    /* figure out who my children and parents are */
    my_level_in_tree=-1;
    result=my_rank;
    /* cnt - number of ranks in given level */
    cnt=1;
    /* parent_cnt - cummulative count of ranks */
    parent_cnt=0;
    while( 0 <= result ) {
        result-=cnt;
        cnt*=tree_order;
        my_level_in_tree++;
    };
    /* int my_level_in_tree, n_children, n_parents; */

    if( 0 == my_rank ) {
        my_node->n_parents=0;
        my_node->parent_rank=-1;
        my_rank_in_my_level=0;
    } else {
        my_node->n_parents=1;
        cnt=1;
        cum_cnt=0;
        for (lvl = 0 ; lvl < my_level_in_tree ; lvl ++ ) {
            /* cummulative count up to this level */
            cum_cnt+=cnt;
            /* number of ranks in this level */
            cnt*=tree_order;
        }
        my_rank_in_my_level=my_rank-cum_cnt;
        /* tree_order consecutive ranks have the same parent */
        my_node->parent_rank=cum_cnt-cnt/tree_order+my_rank_in_my_level/tree_order;
    }
    
    /* figure out number of levels in the tree */
    n_lvls_in_tree=0;
    result=num_nodes;
    /* cnt - number of ranks in given level */
    cnt=1;
    /* parent_cnt - cummulative count of ranks */
    parent_cnt=0;
    while( 0 < result ) {
        result-=cnt;
        cnt*=tree_order;
        n_lvls_in_tree++;
    };

    /* get list of children */
    if( my_level_in_tree == (n_lvls_in_tree -1 ) ) {
        /* last level has no children */
        my_node->n_children=0;
    } else {
        cum_cnt=0;
        cnt=1;
        for( lvl=0 ; lvl <= my_level_in_tree ; lvl++ ) {
            cum_cnt+=cnt;
            cnt*=tree_order;
        }
        start_index=cum_cnt+my_rank_in_my_level*tree_order;
        end_index=start_index+tree_order-1;

        /* don't go out of bounds at the end of the list */
        if( end_index >= num_nodes ) {
            end_index = num_nodes-1;
        }

        if( start_index <= (num_nodes-1) ) {
            my_node->n_children=end_index-start_index+1;
        } else {
            my_node->n_children=0;
        }

        my_node->children_ranks=NULL;
        if( 0 < my_node->n_children ) {
            my_node->children_ranks=
                (int *)malloc( sizeof(int)*my_node->n_children);
            if( NULL == my_node->children_ranks) {
                goto Error;
            }
            for (lvl= start_index ; lvl <= end_index ; lvl++ ) {
                my_node->children_ranks[lvl-start_index]=lvl;
            }
        }
    }

    /* successful return */
    return OMPI_SUCCESS;

Error:

    /* error return */
    return OMPI_ERROR;
}

/* initialize barrier structures */
static int init_sm2_barrier(struct ompi_communicator_t *comm,
        mca_coll_sm2_component_t *component,
        mca_coll_sm2_module_t *module) {

    /*local variables */
    int comm_size, my_rank, tree_order, rc;
    /*debug */
    int i,j;
    /* end debug */

    /* get order of fan-in and fan-out tree */
    tree_order=component->order_barrier_tree;

    /* get communicator size */
    comm_size=ompi_comm_size(comm);

    /* get rank within communictor */
    my_rank=ompi_comm_rank(comm);

    /* initialize fan-in/fan-out tree */
    rc=setup_nary_tree(tree_order, my_rank, comm_size,
        &(module->barrier_tree));
    if( OMPI_SUCCESS != rc ) {
        goto Error;
    }
    /* debug */
    fprintf(stderr," CCCC my rank %d n_parents  %d parent %d\n",
            my_rank,module->barrier_tree.n_parents,
            module->barrier_tree.parent_rank);
    fprintf(stderr," CCCC my rank %d n_children %d :: ",
            my_rank,module->barrier_tree.n_children);
    for (i=0 ; i < module->barrier_tree.n_children; i++ ) {
        fprintf(stderr," %d ",module->barrier_tree.children_ranks[i]);
    }
    fprintf(stderr," \n");
    fflush(stderr);
    /* end debug */

    /* Allocate barrier control structures - allocating one barrier structure
     * per memory bank.  Allocating two shared memory regions per bank. */
    module->barrier_request=(mca_coll_sm2_nb_request_process_shared_mem_t *)
        malloc(sizeof(mca_coll_sm2_nb_request_process_shared_mem_t) * 
                component->sm2_num_mem_banks);
    if( NULL == module->barrier_request ){
        rc=OMPI_ERROR;
        goto Error;
    }

    module->nb_barrier_tag=0;
    /* initialize barrier control structures */
    for(i=0 ; i < component->sm2_num_mem_banks ; i++ ) {

        module->barrier_request[i].tag=0;
        module->barrier_request[i].sm_index=0;
        module->barrier_request[i].sm2_barrier_phase=NB_BARRIER_INACTIVE;

        /* set the base address of each barrier's shared memory regions */
        for( j =0 ; j < 2 ; j++ ) {
            module->barrier_request[i].barrier_base_address[j]=
                (mca_coll_sm2_nb_request_process_shared_mem_t *)
                (module->shared_memory_region + j*
                 module->sm2_size_management_region_per_proc *
                 module->barrier_tree.tree_size);
        }
    }

    /* set pointer to the collective operation buffers */
    module->collective_buffer_region=module->shared_memory_region+
        module->sm2_size_management_region_per_proc*
        module->barrier_tree.tree_size;

    /* set the pointer to the request that needs to be completed first */
    module->current_request_index=0;

    /* return - successful */
    return OMPI_SUCCESS;

Error:
    return rc;
}

static void                
mca_coll_sm2_module_construct(mca_coll_sm2_module_t *module)
{
}

static void                
mca_coll_sm2_module_destruct(mca_coll_sm2_module_t *module)
{
    int ret;
    /* remove shared memory backing file */
    if( module->shared_memory_region) {
        ret=munmap(module->shared_memory_region,
                module->size_sm2_backing_file);
        /* this is cleanup, no recovery will be done */
    }
}


/* query to see if the module is available for use on the given
 * communicator, and if so, what it's priority is.  This is where
 * the backing shared-memory file is created.
 */
struct mca_coll_base_module_1_1_0_t *
mca_coll_sm2_comm_query(struct ompi_communicator_t *comm, int *priority)
{
    /* local variables */
    mca_coll_sm2_module_t *sm_module;
    int group_size,ret;
    size_t alignment,size,size_tot,size_tot_per_proc_per_seg;
    size_t tot_size_per_bank,size_tot_per_segment;
    size_t tot_size_mem_banks;
    size_t ctl_memory_per_proc_per_segment;
    size_t mem_management_per_proc_per_block;
    size_t mem_management_per_proc;
    size_t mem_management_total;
    size_t size_sm2_backing_file;
    size_t len;

    /*
     * This is activated only for intra-communicators
     */
    if (OMPI_COMM_IS_INTER(comm) ) {
        return NULL;
    }

    /*
     * Use only if more than on proc in the communicator
     */
    if (1 == ompi_comm_size(comm) ) {
        return NULL;
    }

    /* check to see if all procs are on the same node, and therefore
     *   can communicate using shared memory
     */
    if ( !have_local_peers(comm->c_local_group, ompi_comm_size(comm))) {
        return NULL;
    }

    /* Get our priority */
    *priority = mca_coll_sm2_component.sm2_priority;

    /* allocate and initialize an sm-v2  module */
    sm_module = OBJ_NEW(mca_coll_sm2_module_t);

    sm_module->super.coll_module_enable = sm2_module_enable;
    sm_module->super.ft_event        = NULL;
    sm_module->super.coll_allgather  = NULL;
    sm_module->super.coll_allgatherv = NULL;
    sm_module->super.coll_allreduce  = NULL;
    sm_module->super.coll_alltoall   = NULL;
    sm_module->super.coll_alltoallv  = NULL;
    sm_module->super.coll_alltoallw  = NULL;
    sm_module->super.coll_barrier    = NULL;
    sm_module->super.coll_bcast      = NULL;
    sm_module->super.coll_exscan     = NULL;
    sm_module->super.coll_gather     = NULL;
    sm_module->super.coll_gatherv    = NULL;
    sm_module->super.coll_reduce     = NULL;
    sm_module->super.coll_reduce_scatter = NULL;
    sm_module->super.coll_scan       = NULL;
    sm_module->super.coll_scatter    = NULL;
    sm_module->super.coll_scatterv   = NULL;

    /* 
     * create backing file 
     */

    /*
     * set group size
     */
    group_size=ompi_comm_size(comm);

    sm_module->module_comm=comm;

    /*
     * set memory region parameters 
     */
    sm_module->sm2_module_num_memory_banks=
        mca_coll_sm2_component.sm2_num_mem_banks;
    sm_module->sm2_module_num_regions_per_bank=
        mca_coll_sm2_component.sm2_num_regions_per_bank;

    /* 
     * get control region size 
     */ 
    /* just enough place for one flag per process */
    ctl_memory_per_proc_per_segment=sizeof(long long);
    if( mca_coll_sm2_component.sm2_ctl_size_per_proc > ctl_memory_per_proc_per_segment )
        ctl_memory_per_proc_per_segment=mca_coll_sm2_component.sm2_ctl_size_per_proc;
    ctl_memory_per_proc_per_segment=ctl_memory_per_proc_per_segment * group_size ;
   
    /* pad this up to the alignment needed by the data segment, as the
     * that data segment will directly follow the control segment in
     * memory.
     */
    alignment=mca_coll_sm2_component.sm2_data_alignment;
    ctl_memory_per_proc_per_segment=
        (alignment + ctl_memory_per_proc_per_segment -1) / alignment;
    ctl_memory_per_proc_per_segment*=alignment;
    mca_coll_sm2_component.sm2_ctl_size_allocated=ctl_memory_per_proc_per_segment;

    /* get data region size - allocation happens on a page granularity, with
     * a minimum of a page allocated per proc, so adjust to this
     */
    size=mca_coll_sm2_component.sm2_data_seg_size;
    if( size > mca_coll_sm2_component.sm2_max_data_seg_size )
        size=mca_coll_sm2_component.sm2_max_data_seg_size;
    size_tot_per_proc_per_seg=size+ mca_coll_sm2_component.sm2_ctl_size_per_proc;
    if( size_tot_per_proc_per_seg < getpagesize())
        size_tot_per_proc_per_seg=getpagesize();
    /* round this up to the nearest integer page-size multiple */
    size_tot_per_proc_per_seg= ( size_tot_per_proc_per_seg + getpagesize() - 1)/
        getpagesize();
    size_tot_per_proc_per_seg*=getpagesize();

    /* compute segment memory needed */
    size_tot_per_segment=group_size * size_tot_per_proc_per_seg ;

    sm_module->segement_size_per_process=size_tot_per_proc_per_seg;
    sm_module->segment_size=size_tot_per_segment;

    /* compute memory per bank */
    tot_size_per_bank=size_tot_per_segment*mca_coll_sm2_component.sm2_num_regions_per_bank;

    /* compute total memory in the memory banks */
    tot_size_mem_banks=tot_size_per_bank*mca_coll_sm2_component.sm2_num_mem_banks;

    /* compute the amount of memory needed for the anynchromous barriers used to
     *   manage the memory resources.
     */
    /* for each bank, 2 sets of barrier buffers */
    mem_management_per_proc_per_block= 2 * CACHE_LINE_SIZE ;
    /* add in number of banks */
    mem_management_per_proc= mem_management_per_proc_per_block *
        mca_coll_sm2_component.sm2_num_mem_banks;
    /* round up to page multiples */
    mem_management_per_proc=(mem_management_per_proc +
            getpagesize() -1 ) / getpagesize();
    mem_management_per_proc*=getpagesize();

    /* size of memory region, per process, for memory bank management */
    sm_module->sm2_size_management_region_per_proc=
        mem_management_per_proc;

    /* total memory management required */
    mem_management_total=mem_management_per_proc * group_size;

    /* set the total number of working buffers */
    sm_module->sm2_module_num_buffers=
        mca_coll_sm2_component.sm2_num_regions_per_bank *
        mca_coll_sm2_component.sm2_num_mem_banks;

    /* total size of backing file - this assumes the mmap allocation
     *   occurs on page boundaries, and that all segments are paged
     *   aligned 
     */
    size_sm2_backing_file=mem_management_total+tot_size_mem_banks;
    sm_module->size_sm2_backing_file=size_sm2_backing_file;

    /* set file name */
    len=asprintf(&(sm_module->coll_sm2_file_name),
            "%s"OPAL_PATH_SEP"sm_coll_v2%s_%0d",orte_process_info.job_session_dir,
            orte_system_info.nodename,ompi_comm_get_cid(comm));
    if( 0 > len ) {
        goto CLEANUP;
    }

    /* allocate backing file */
    ret=allocate_shared_file(size_sm2_backing_file,
            sm_module->coll_sm2_file_name, comm,
            &(sm_module->shared_memory_region));
    if( MPI_SUCCESS != ret ) {
        goto CLEANUP;
    }

    /* intialize barrier structures */
    ret=init_sm2_barrier(comm, &mca_coll_sm2_component, 
        sm_module);
    if( MPI_SUCCESS != ret ) {
        goto CLEANUP;
    }

    /* initialize local counters */
    sm_module->sm2_allocated_buffer_index=-1;
    sm_module->sm2_freed_buffer_index=-1;
    /* NOTE: need to fix this if we have only one memory bank */
    sm_module->sm2_first_buffer_index_next_bank=
        sm_module->sm2_module_num_regions_per_bank;
    sm_module->sm2_last_buffer_index_this_bank=
         sm_module->sm2_module_num_regions_per_bank-1;
    if(sm_module->sm2_module_num_memory_banks > 1 ) {
        sm_module->sm2_first_buffer_index_next_bank=
            mca_coll_sm2_component.sm2_num_regions_per_bank;
    } else {
        sm_module->sm2_first_buffer_index_next_bank=0;
    }

    /* set pointers */

    /* touch pages to apply memory affinity - Note: do we really need this or will
     * the algorithms do this */

    /* return */
    return &(sm_module->super);


CLEANUP:
    OBJ_RELEASE(sm_module);

    if( NULL == sm_module->coll_sm2_file_name ) {
        free(sm_module->coll_sm2_file_name);
    }

    return NULL;
}

/*
 * Init module on the communicator
 */
static int
sm2_module_enable(struct mca_coll_base_module_1_1_0_t *module,
                         struct ompi_communicator_t *comm)
{
    /* local variables */
    char output_buffer[2*MPI_MAX_OBJECT_NAME];

    memset(&output_buffer[0],0,sizeof(output_buffer));
    snprintf(output_buffer,sizeof(output_buffer),"%s (cid %d)", comm->c_name,
                       comm->c_contextid);
    opal_output_verbose(10, mca_coll_base_output,
            "coll:sm2:enable: new communicator: %s", output_buffer);

    /* All done */
    return OMPI_SUCCESS;
}

/* allocate working buffer */
char *alloc_sm2_shared_buffer(mca_coll_sm2_module_t *module)
{
    /* local variables */
    int rc,buffer_index, memory_bank_index;
    char *return_buffer;
    mca_coll_sm2_nb_request_process_private_mem_t *request;

    /* check to see if need to progress the current nb-barrier, which
     * is being used for recycling the shared buffers.
     */
    if( NB_BARRIER_INACTIVE != 
            module->barrier_request[module->current_request_index].
            sm2_barrier_phase ) {
        rc=mca_coll_sm2_nbbarrier_intra_progress(module->module_comm,
                &(module->barrier_request[module->current_request_index]),
                (struct mca_coll_base_module_1_1_0_t *)module);
        if( OMPI_SUCCESS != rc ) {
            return NULL;
        }
        /* if barrier is completed, transition it to inactive, and point to
         * the request object for then next bank
         */
        if ( NB_BARRIER_DONE == 
                module->barrier_request[module->current_request_index].
          sm2_barrier_phase ) {
            /* set request to inactive */
            module->barrier_request[module->current_request_index]. 
                sm2_barrier_phase=NB_BARRIER_INACTIVE;
            /* move pointer to next request that needs to be completed */
            module->current_request_index++;
            /* wrap around */
            if( module->current_request_index == 
                    module->sm2_module_num_memory_banks ) {
                module->current_request_index=0;
            }
        }
    }

    /* get next buffer index */
    module->sm2_allocated_buffer_index++;

    /* check for wrap-around */
    if(  module->sm2_allocated_buffer_index == module->sm2_module_num_buffers ) {
        module->sm2_allocated_buffer_index=0;
    }

    /* do I need to complete non-blocking barrier ?  The barrier will
     * be initiated when a process is done with the buffer */
    if( module->sm2_allocated_buffer_index ==
            module->sm2_first_buffer_index_next_bank) {
        /* 
         * complete non-blocking barrier, so this memory bank will
         *   be available for use.
         */
        memory_bank_index= module->sm2_allocated_buffer_index /
                module->sm2_module_num_regions_per_bank;
        request=&(module->barrier_request[memory_bank_index]);
        while ( NB_BARRIER_DONE != request->sm2_barrier_phase ) {
            rc=mca_coll_sm2_nbbarrier_intra_progress(module->module_comm,
                    request,
                    (struct mca_coll_base_module_1_1_0_t *)module);
            if( OMPI_SUCCESS != rc ) {
                return NULL;
            }
            /* set the reqeust to inactive, and point current_request_index
             *  to the request for the next memory bank
             */
            /* set request to inactive */
            request->sm2_barrier_phase==NB_BARRIER_INACTIVE;
            /* move pointer to next request that needs to be completed */
            module->current_request_index=memory_bank_index+1;
            /* wrap around */
            if( module->current_request_index == 
                    module->sm2_module_num_memory_banks ) {
                module->current_request_index=0;
            }
        }

        /* re-set counter for next bank */
        module->sm2_first_buffer_index_next_bank +=
            module->sm2_module_num_regions_per_bank;
        if( module->sm2_first_buffer_index_next_bank == 
                module->sm2_module_num_memory_banks ) {
            module->sm2_module_num_memory_banks=0;
        }
    }

    buffer_index=module->sm2_allocated_buffer_index;

    /* get base address of return buffer */
    return_buffer=module->collective_buffer_region+
        buffer_index*module->segment_size;

    /* return */
    return return_buffer;

}

/* free working buffer - it is assumed that buffers are released in
 * the order they are allocated.  We can assume this because each
 * communiator will have only one outstanding collective at a given
 * time, and we ensure that operations are completed in order. */
int free_sm2_shared_buffer(mca_coll_sm2_module_t *module)
{
    /* local variables */
    int rc,buffer_index,memory_bank_index;
    mca_coll_sm2_nb_request_process_private_mem_t *request;

    /* check to see if need to progress the current nb-barrier, which
     * is being used for recycling the shared buffers.
     */
    if( NB_BARRIER_INACTIVE != 
            module->barrier_request[module->current_request_index].
            sm2_barrier_phase ) {
        rc=mca_coll_sm2_nbbarrier_intra_progress(module->module_comm,
                &(module->barrier_request[module->current_request_index]),
                (struct mca_coll_base_module_1_1_0_t *)module);
        if( OMPI_SUCCESS != rc ) {
            return NULL;
        }
        /* if barrier is completed, transition it to inactive, and point to
         * the request object for then next bank
         */
        if ( NB_BARRIER_DONE == 
                module->barrier_request[module->current_request_index].
          sm2_barrier_phase ) {
            /* set request to inactive */
            module->barrier_request[module->current_request_index]. 
                sm2_barrier_phase=NB_BARRIER_INACTIVE;
            /* move pointer to next request that needs to be completed */
            module->current_request_index++;
            /* wrap around */
            if( module->current_request_index == 
                    module->sm2_module_num_memory_banks ) {
                module->current_request_index=0;
            }
        }
    }  /* done with progress */

    /* get next buffer index */
    module->sm2_freed_buffer_index++;

    /* do I need to initiate non-blocking barrier */
    if( module->sm2_freed_buffer_index ==
            module->sm2_last_buffer_index_this_bank) {

        /* complete non-blocking barrier */
        memory_bank_index= module->sm2_freed_buffer_index /
                module->sm2_module_num_regions_per_bank;
        request=&(module->barrier_request[memory_bank_index]);
        rc=mca_coll_sm2_nbbarrier_intra(module->module_comm,
                request,module);
        if( OMPI_SUCCESS !=rc ) {
            return rc;
        }
        /* check to see if barrier completed, and reset
         *   current_request_index
         */
        if( NB_BARRIER_DONE == request->sm2_barrier_phase ) {
            /* set request to inactive */
            request->sm2_barrier_phase=NB_BARRIER_INACTIVE;
            /* move pointer to next request that needs to be completed */
            module->current_request_index++;
            /* wrap around */
            if( module->current_request_index == 
                    module->sm2_module_num_memory_banks ) {
                module->current_request_index=0;
            }

        }

        /* need to use buffer out of next memory bank */
        module->sm2_last_buffer_index_this_bank +=
            module->sm2_module_num_regions_per_bank;

        /* wrap around */
        if( module->sm2_last_buffer_index_this_bank >= 
                module->sm2_module_num_memory_banks ) {
            module->sm2_last_buffer_index_this_bank=
                module->sm2_module_num_regions_per_bank-1;
        }
    }

    /* check for wrap-around */
    if(  module->sm2_freed_buffer_index == module->sm2_module_num_buffers ) {
        module->sm2_freed_buffer_index=0;
    }

    /* return */
    return OMPI_SUCCESS;
}


OBJ_CLASS_INSTANCE(mca_coll_sm2_module_t,
                   mca_coll_base_module_1_1_0_t,
                   mca_coll_sm2_module_construct,
                   mca_coll_sm2_module_destruct);
