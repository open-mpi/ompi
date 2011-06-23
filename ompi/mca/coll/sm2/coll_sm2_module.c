/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
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
#include <errno.h>

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "opal/util/output.h"
#include "coll_sm2.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/dpm/dpm.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "opal/mca/paffinity/base/base.h"
#include "orte/mca/grpcomm/grpcomm.h"

/*
 * Local functions
 */
static int sm2_module_enable(mca_coll_base_module_t *module,
        struct ompi_communicator_t *comm);

#if 0
/* debug */
extern int debug_print;
extern int my_debug_rank;
extern int my_debug_comm_size;
extern void debug_module(void);
extern int last_root;
extern int node_type;
long long free_buff_free_index=-1;
static mca_coll_sm2_module_t *module_dbg;
static int blocking_cnt=0;
void debug_module(void) {
 int i,j,k;
 char *ptr;
 int barrier_index,index;
 long long br_tag;

 mca_coll_sm2_nb_request_process_shared_mem_t * ctl_ptr;
   /* control regions */
   if ( 0 == my_debug_rank ) {
       for( i=0 ; i < 2 ; i++ ) {
           for( j=0 ; j < 2 ; j++ ) {
              fprintf(stderr," bank %d index %d \n", i,j);
                  for( k=0 ; k < my_debug_comm_size ; k++ ) {
                      ctl_ptr=module_dbg->barrier_request[i].barrier_base_address[j];
                      ctl_ptr=(mca_coll_sm2_nb_request_process_shared_mem_t *) (
                          (char *)ctl_ptr+k*module_dbg->sm2_size_management_region_per_proc
                      );
                      fprintf(stderr," bank %d index %d flag %lld \n",
                         i,j,ctl_ptr->flag);
                  }
           }
       }
    }
    /* data regions */

    fprintf(stderr," my_debug_rank %d current index %d freed index %d coll_tag %lld debug stat %d blocking_cnt %d last_root %d free_buff_free_index %lld node_type %d \n",
         my_debug_rank,
         module_dbg->sm2_allocated_buffer_index,module_dbg->sm2_freed_buffer_index,
         module_dbg->collective_tag,
         module_dbg->blocked_on_barrier,blocking_cnt,last_root,
         free_buff_free_index,node_type);

    barrier_index=(module_dbg->num_nb_barriers_completed%
            module_dbg->sm2_module_num_memory_banks);
    index=module_dbg->barrier_request[barrier_index].sm_index;
    fprintf(stderr," my_debug_rank %d  started %lld completed %lld bank %d index %d br_tag %lld \n",
            my_debug_rank,
            module_dbg->num_nb_barriers_started,
            module_dbg->num_nb_barriers_completed,
            barrier_index,index,
            module_dbg->barrier_request[barrier_index].tag);
    fprintf(stderr," my_debug_rank %d barrier_bank_cntr %lld ",
            my_debug_rank,module_dbg->barrier_bank_cntr);
    for( i=0 ; i < BARRIER_BANK_LIST_SIZE ; i++ )
        fprintf(stderr,"%2d",module_dbg->barrier_bank_list[i]);
    fprintf(stderr," \n");
    if( 0 == my_debug_rank ) {
        for( i=0 ; i < module_dbg->sm2_module_num_buffers ; i++ ) {
            for( j=0 ; j < my_debug_comm_size ; j++ ) {
                fprintf(stderr," buffer index %d tag %lld ptr %p \n",
                   i,
                   module_dbg->sm_buffer_descriptor[i].proc_memory[j].control_region->flag,
                   module_dbg->sm_buffer_descriptor[i].proc_memory[j].control_region);
            }
        }
    }

    fflush(stderr);
    return;
 
}
/* end debug */
#endif

/*
 * Local functions
 */
static void                
mca_coll_sm2_module_construct(mca_coll_sm2_module_t *module)
{
}

static void                
mca_coll_sm2_module_destruct(mca_coll_sm2_module_t *module)
{
    int i,ret;
    /* free the mmaped shared file */
    if( module->shared_memory_region) {
        ret=munmap(module->shared_memory_region,
                module->size_sm2_backing_file);
        /* this is cleanup, no recovery will be done */
    }

    /* free list of children in the barrier-tree */
    if( NULL != module->sm_buffer_mgmt_barrier_tree.children_ranks ) {
        free(module->sm_buffer_mgmt_barrier_tree.children_ranks);
    }

    /* free non-blocking barrier request objects */
    if( NULL != module->barrier_request ) {
        free(module->barrier_request);
    }

    /* free reduction tree */
    if( NULL != module->reduction_tree ) {
        for( i=0 ; i < module->comm_size ; i++ ) {
            if( NULL != module->reduction_tree[i].children_ranks) {
                free(module->reduction_tree[i].children_ranks);
            }
        }
        free(module->reduction_tree);
    }

    /* free fan-out read tree */
    if( NULL != module->fanout_read_tree ) {
        for( i=0 ; i < module->comm_size ; i++ ) {
            if( NULL != module->fanout_read_tree[i].children_ranks) {
                free(module->fanout_read_tree[i].children_ranks);
            }
        }
        free(module->fanout_read_tree);
    }


    /* done */
}

static bool have_local_peers(ompi_group_t *group, size_t size)
{
    size_t i;
    ompi_proc_t *proc;

    for (i = 0; i < size; ++i) {
        proc = ompi_group_peer_lookup(group,i);
        if (!OPAL_PROC_ON_LOCAL_NODE(proc->proc_flags)) {
            return false;
        }
    }
    return true;
}

/*
 * Create mmaped shared file
 */

static int allocate_shared_file(size_t size, char **file_name, 
        struct ompi_communicator_t *comm, char **sm_backing_file)
{
    int fd = -1;
    int group_size,my_rank;
    int unique_comm_id;
    size_t len;
    char *f_name;

    bool i_create_shared_file=false;
    ssize_t p;
    int rc=0, sm_file_inited=0;
    struct iovec iov[3]; 
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
        /* 
         * set file name 
         */

        /* generate id that will be different for non-overlapping
         *   communicators.
         */
        unique_comm_id=(int)getpid();
        len=asprintf(&f_name,
                "%s"OPAL_PATH_SEP"sm_coll_v2_%0d_%0d",orte_process_info.job_session_dir,
                ompi_comm_get_cid(comm),unique_comm_id);
        if( 0 > len ) {
            return OMPI_ERROR;
        }
        *file_name=f_name;

        /* process initializing the file */
        fd = open(*file_name, O_CREAT|O_RDWR, 0600);
        if (fd < 0) {
            opal_output(0,"mca_common_sm_mmap_init: open %s len %ld failed with errno=%d\n",                      
                        *file_name, len, errno);
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
            iov[2].iov_base=&unique_comm_id;
            iov[2].iov_len=sizeof(unique_comm_id);
            rc=orte_rml.send(&(comm_proc_list[p]->proc_name),iov,3,
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
        iov[2].iov_base=&unique_comm_id;
        iov[2].iov_len=sizeof(unique_comm_id);
        rc=orte_rml.recv(&(comm_proc_list[0]->proc_name),iov,3,
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
        /* set file name - we need the unique id for non-overlapping
         *   communicators, that could have the same communicator id
         */
        len=asprintf(&f_name,
                "%s"OPAL_PATH_SEP"sm_coll_v2_%0d_%0d",orte_process_info.job_session_dir,
                ompi_comm_get_cid(comm),unique_comm_id);
        if( 0 > len ) {
            return OMPI_ERROR;
        }
        *file_name=f_name;

        /* open backing file */
        fd = open(*file_name, O_RDWR, 0600);
            if (fd < 0) {
            opal_output(0,"mca_common_sm_mmap_init: open %s len %ld failed with errno=%d\n",                      
                        *file_name, len, errno);
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

static 
int barrier( struct ompi_communicator_t *comm ,
        tree_node_t *multinomial_tree)
{
    int group_size,my_rank,n_children,child,n_parents,my_fanout_parent;
    int child_rank, dummy;
    tree_node_t *my_node;
    int rc=0;
    struct iovec iov; 
    ompi_proc_t **comm_proc_list;

    /* get the list of procs */
    comm_proc_list=comm->c_local_group->grp_proc_pointers;

    group_size=ompi_comm_size(comm);
    my_rank=ompi_comm_rank(comm);
    my_node=&(multinomial_tree[my_rank]);
    n_children=my_node->n_children;
    n_parents=my_node->n_parents;
    my_fanout_parent=my_node->parent_rank;

    /* 
     * fan in 
     */
    /* receive from the children */
    for( child=0 ; child < n_children ; child++ ) {
        child_rank=my_node->children_ranks[child];
        iov.iov_base=&dummy;
        iov.iov_len=sizeof(dummy);
        rc=orte_rml.recv(&(comm_proc_list[child_rank]->proc_name),&iov,1,
                OMPI_RML_TAG_COLL_SM2_BACK_FILE_CREATED,0);
        if( rc < 0 ) {
            opal_output(0,
                    "sm barrier fan-in:  orte_rml.recv failed to %lu with errno=%d\n",
                    (unsigned long)child_rank, errno);
            goto return_error;
        }
    }
    /* send to parent */
    if( 0 < n_parents ) {
        iov.iov_base=&dummy;
        iov.iov_len=sizeof(dummy);
        rc=orte_rml.send(&(comm_proc_list[my_fanout_parent]->proc_name),&iov,1,
                OMPI_RML_TAG_COLL_SM2_BACK_FILE_CREATED,0);
        if( rc < 0 ) {
            opal_output(0,
                    "sm barrier fan-in: orte_rml.send failed to %lu with errno=%d\n",
                    (unsigned long)my_fanout_parent, errno);
            goto return_error;
        }
    }

    /*
     * Fan out
     */
    /* receive from parent */
    if( 0 < n_parents ) {
        iov.iov_base=&dummy;
        iov.iov_len=sizeof(dummy);
        rc=orte_rml.recv(&(comm_proc_list[my_fanout_parent]->proc_name),&iov,1,
                OMPI_RML_TAG_COLL_SM2_BACK_FILE_CREATED,0);
        if( rc < 0 ) {
            opal_output(0,
                    "sm barrier fan-out: orte_rml.recv failed to %lu with errno=%d\n",
                    (unsigned long)my_fanout_parent, errno);
            goto return_error;
        }
    }

    /* send to children */
    for( child=0 ; child < n_children ; child++ ) {
        child_rank=my_node->children_ranks[child];
        iov.iov_base=&dummy;
        iov.iov_len=sizeof(dummy);
        rc=orte_rml.send(&(comm_proc_list[child_rank]->proc_name),&iov,1,
                OMPI_RML_TAG_COLL_SM2_BACK_FILE_CREATED,0);
        if( rc < 0 ) {
            opal_output(0,
                    "sm barrier fan-out:  orte_rml.send failed to %lu with errno=%d\n",
                    (unsigned long)child_rank, errno);
            goto return_error;
        }
    }

    return OMPI_SUCCESS;

  return_error:

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

    my_node->children_ranks=(int *)NULL;

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
    /* set node type */
    if( 0 == my_node->n_parents ) {
        my_node->my_node_type=ROOT_NODE;
    } else if ( 0 == my_node->n_children ) {
        my_node->my_node_type=LEAF_NODE;
    } else {
        my_node->my_node_type=INTERIOR_NODE;
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
    int i,j,k,comm_size, my_rank, tree_order, rc;
    mca_coll_sm2_nb_request_process_shared_mem_t *sm_address;

    /* get order of fan-in and fan-out tree */
    tree_order=component->order_barrier_tree;

    /* get communicator size */
    comm_size=ompi_comm_size(comm);

    /* get rank within communictor */
    my_rank=ompi_comm_rank(comm);

    /* initialize fan-in/fan-out tree */
    rc=setup_nary_tree(tree_order, my_rank, comm_size,
        &(module->sm_buffer_mgmt_barrier_tree));
    if( OMPI_SUCCESS != rc ) {
        goto Error;
    }

    /* Allocate barrier control structures - allocating one barrier structure
     * per memory bank.  Allocating two shared memory regions per bank. */
    module->barrier_request=(mca_coll_sm2_nb_request_process_private_mem_t *)
        malloc(sizeof(mca_coll_sm2_nb_request_process_private_mem_t) * 
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
                (module->shared_memory_region + 
                 /* there are 2 barrier structs per bank */
                 (2*i+j)*opal_cache_line_size);
            /* initialize per-process flags */
            for(k=0 ; k < comm_size ; k++ ) {
                sm_address=(mca_coll_sm2_nb_request_process_shared_mem_t *)
                    ((char *)
                     (module->barrier_request[i].barrier_base_address[j])+
                     k*module->sm2_size_management_region_per_proc);
                sm_address->flag=0;
            }
        }
    }

    module->num_nb_barriers_started=0;
    module->num_nb_barriers_completed=0;

    /* set pointer to the collective operation buffers */
    module->collective_buffer_region=module->shared_memory_region+
        module->sm2_size_management_region_per_proc*
        module->sm_buffer_mgmt_barrier_tree.tree_size;

    /* set the pointer to the request that needs to be completed first */
    module->current_request_index=0;

    /* set starting collective tag */
    module->collective_tag=1;

    /* return - successful */
    return OMPI_SUCCESS;

Error:
    return rc;
}



/* query to see if the module is available for use on the given
 * communicator, and if so, what it's priority is.  This is where
 * the backing shared-memory file is created.
 */
mca_coll_base_module_t *
mca_coll_sm2_comm_query(struct ompi_communicator_t *comm, int *priority)
{
    /* local variables */
    mca_coll_sm2_module_t *sm_module;
    int i,j,group_size,ret,proc;
    int my_socket_index,num_procs,socket,socket_tmp,core,n_sockets,cnt;
    size_t alignment,size;
    size_t tot_size_mem_banks;
    size_t ctl_memory_per_proc_per_segment;
    size_t mem_management_per_proc_per_block;
    size_t mem_management_per_proc;
    size_t mem_management_total;
    size_t size_sm2_backing_file;
    size_t size_buff_ctl_per_proc,size_data_buff_per_proc;

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
    sm_module->super.coll_allreduce  = mca_coll_sm2_allreduce_intra;
    sm_module->super.coll_alltoall   = NULL;
    sm_module->super.coll_alltoallv  = NULL;
    sm_module->super.coll_alltoallw  = NULL;
    sm_module->super.coll_barrier    = mca_coll_sm2_barrier_intra;
    sm_module->super.coll_bcast      = mca_coll_sm2_bcast_intra;
    sm_module->super.coll_exscan     = NULL;
    sm_module->super.coll_gather     = NULL;
    sm_module->super.coll_gatherv    = NULL;
    sm_module->super.coll_reduce     = mca_coll_sm2_reduce_intra;
    sm_module->super.coll_reduce_scatter = NULL;
    sm_module->super.coll_scan       = NULL;
    sm_module->super.coll_scatter    = NULL;
    sm_module->super.coll_scatterv   = NULL;

    /* 
     * set up specific function to be used 
     */

    /* barrier */
    sm_module->barrier_functions[FANIN_FAN_OUT_BARRIER_FN]=
        mca_coll_sm2_barrier_intra_fanin_fanout;
    sm_module->barrier_functions[RECURSIVE_DOUBLING_BARRIER_FN]=
        mca_coll_sm2_barrier_intra_fanin_fanout;
    if( ( 0 <= mca_coll_sm2_component.force_barrier ) &&
            ( N_BARRIER_FNS > mca_coll_sm2_component.force_barrier ) ) {
        /* set user specifed function */
        mca_coll_base_module_barrier_fn_t tmp_fn=
            sm_module->barrier_functions[mca_coll_sm2_component.force_barrier];
        sm_module->barrier_functions[FANIN_FAN_OUT_BARRIER_FN]=tmp_fn;
        sm_module->barrier_functions[RECURSIVE_DOUBLING_BARRIER_FN]=tmp_fn;
    }

    /* reduce */
    sm_module->list_reduce_functions[FANIN_REDUCE_FN]=
        mca_coll_sm2_reduce_intra_fanin;
    sm_module->list_reduce_functions[REDUCE_SCATTER_GATHER_FN]=
        mca_coll_sm2_reduce_intra_reducescatter_gather;
    sm_module->reduce_functions[SHORT_DATA_FN_REDUCE]=
        sm_module->list_reduce_functions[FANIN_REDUCE_FN];
    sm_module->reduce_functions[LONG_DATA_FN_REDUCE]=
        sm_module->list_reduce_functions[REDUCE_SCATTER_GATHER_FN];
    if( ( 0 <= mca_coll_sm2_component.force_reduce ) &&
            ( N_REDUCE_FNS > mca_coll_sm2_component.force_reduce ) ) {
        /* set user specifed function */
        mca_coll_base_module_reduce_fn_t tmp_fn=sm_module->
            list_reduce_functions[mca_coll_sm2_component.force_reduce];
        sm_module->reduce_functions[SHORT_DATA_FN_REDUCE]=tmp_fn;
        sm_module->reduce_functions[LONG_DATA_FN_REDUCE]=tmp_fn;
    }

    /* allreduce */
    sm_module->list_allreduce_functions[FANIN_FANOUT_ALLREDUCE_FN]=
        mca_coll_sm2_allreduce_intra_fanin_fanout;
    sm_module->list_allreduce_functions[REDUCE_SCATTER_ALLGATHER_FN]=
        mca_coll_sm2_allreduce_intra_reducescatter_allgather;
    sm_module->allreduce_functions[SHORT_DATA_FN_ALLREDUCE]=
        sm_module->list_allreduce_functions[FANIN_FANOUT_ALLREDUCE_FN];
    sm_module->allreduce_functions[LONG_DATA_FN_ALLREDUCE]=
        sm_module->list_allreduce_functions[REDUCE_SCATTER_ALLGATHER_FN];
    if( ( 0 <= mca_coll_sm2_component.force_allreduce ) &&
            ( N_ALLREDUCE_FNS > mca_coll_sm2_component.force_allreduce ) ) {
        /* set user specifed function */
        mca_coll_base_module_allreduce_fn_t tmp_fn=sm_module->
            list_allreduce_functions[mca_coll_sm2_component.force_allreduce];
        sm_module->allreduce_functions[SHORT_DATA_FN_ALLREDUCE]=tmp_fn;
        sm_module->allreduce_functions[LONG_DATA_FN_ALLREDUCE]=tmp_fn;
    }

    /*
     * Some initialization
     */
    sm_module->reduction_tree=NULL;
    sm_module->fanout_read_tree=NULL;

    /* 
     * create backing file 
     */

    /*
     * set group size
     */
    group_size=ompi_comm_size(comm);

    sm_module->module_comm=comm;
    sm_module->comm_size=group_size;
    sm_module->n_poll_loops=mca_coll_sm2_component.n_poll_loops;

    /*
     * set memory region parameters 
     */
    sm_module->sm2_module_num_memory_banks=
        mca_coll_sm2_component.sm2_num_mem_banks;
    sm_module->sm2_module_num_regions_per_bank=
        mca_coll_sm2_component.sm2_num_regions_per_bank;
    sm_module->sm2_module_num_buffers=
        mca_coll_sm2_component.sm2_num_regions_per_bank *
        mca_coll_sm2_component.sm2_num_mem_banks;


    /* allocate the array of memory descriptors used to describe the
     *   shared memory buffers.  This structure resides in process
     *   private memory, but describes the shared memory.
     */
    sm_module->sm_buffer_descriptor=(sm_work_buffer_t *)malloc(
            sizeof(sm_work_buffer_t)*sm_module->sm2_module_num_buffers);
    if( NULL == sm_module->sm_buffer_descriptor ) {
        goto CLEANUP;
    }

#if 0  /* data buffers and management buffers are allocated in a single
        * contigous region */
    /*
     *  Now figure out how much memory to allocate for use as
     *  working memory for the shared memory collectives.
     */
    /* 
     * get control region size 
     */ 
    /* just enough place for two flags per process */
    ctl_memory_per_proc_per_segment=2*sizeof(long long);
    if( mca_coll_sm2_component.sm2_ctl_size_per_proc > ctl_memory_per_proc_per_segment )
        ctl_memory_per_proc_per_segment=mca_coll_sm2_component.sm2_ctl_size_per_proc;
   
    /* pad this up to the alignment needed by the data segment, as the
     * that data segment will directly follow the control segment in
     * memory.
     */
    alignment=mca_coll_sm2_component.sm2_data_alignment;
    ctl_memory_per_proc_per_segment=
        (alignment + ctl_memory_per_proc_per_segment -1) / alignment;
    ctl_memory_per_proc_per_segment*=alignment;
    mca_coll_sm2_component.sm2_ctl_size_allocated=ctl_memory_per_proc_per_segment;
    sm_module->ctl_memory_per_proc_per_segment=ctl_memory_per_proc_per_segment;

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
    sm_module->data_memory_per_proc_per_segment=size_tot_per_proc_per_seg-
        ctl_memory_per_proc_per_segment;

    /* compute memory per bank */
    tot_size_per_bank=size_tot_per_segment*mca_coll_sm2_component.sm2_num_regions_per_bank;

    /* compute total memory in the memory banks */
    tot_size_mem_banks=tot_size_per_bank*mca_coll_sm2_component.sm2_num_mem_banks;
    sm_module->data_memory_per_proc_per_segment=size_tot_per_proc_per_seg-
        ctl_memory_per_proc_per_segment;

#endif

    /* management structures are allocated is a one segment, and data buffers
     * in a separate segment
     */
    /*
     *  Now figure out how much memory to allocate for use as
     *  working memory for the shared memory collectives.
     */
    /* 
     * get control region size 
     */ 
    /* just enough place for two flags per process */
    ctl_memory_per_proc_per_segment=2*sizeof(long long);
    if( mca_coll_sm2_component.sm2_ctl_size_per_proc > ctl_memory_per_proc_per_segment )
        ctl_memory_per_proc_per_segment=mca_coll_sm2_component.sm2_ctl_size_per_proc;
   
    /* pad this up to the alignment needed by the data segment, as the
     * that data segment will directly follow the control segment in
     * memory.
     */
    alignment=mca_coll_sm2_component.sm2_data_alignment;
    ctl_memory_per_proc_per_segment=
        (alignment + ctl_memory_per_proc_per_segment -1) / alignment;
    ctl_memory_per_proc_per_segment*=alignment;
    mca_coll_sm2_component.sm2_ctl_size_allocated=ctl_memory_per_proc_per_segment;
    sm_module->ctl_memory_per_proc_per_segment=ctl_memory_per_proc_per_segment;

    /* get data region size - allocation happens on a page granularity, with
     * a minimum of a page allocated per proc, so adjust to this
     */
    size=mca_coll_sm2_component.sm2_data_seg_size;
    if( size < getpagesize() )
        size=getpagesize();
    if( size > mca_coll_sm2_component.sm2_max_data_seg_size )
        size=mca_coll_sm2_component.sm2_max_data_seg_size;
    size= ( size + getpagesize() - 1)/getpagesize();
    size*=getpagesize();
    sm_module->segment_size=size*group_size;
    size_data_buff_per_proc=size;

    /* compute size of management region - per proc */
     size_buff_ctl_per_proc=
         ctl_memory_per_proc_per_segment*sm_module->sm2_module_num_buffers;
    size_buff_ctl_per_proc= ( size_buff_ctl_per_proc + getpagesize() - 1)/
        getpagesize();
    size_buff_ctl_per_proc*=getpagesize();

    tot_size_mem_banks=
        /* size of buffer conrol region */
        size_buff_ctl_per_proc*group_size+
        /* size of data buffers */
        size*sm_module->sm2_module_num_buffers*group_size;
    sm_module->size_of_collective_buffer_region=tot_size_mem_banks;
    sm_module->data_memory_per_proc_per_segment=size;


    /* 
     * compute the amount of memory needed for the anynchromous barriers used to
     *   manage the memory resources.
     */
    /* for each bank, 2 sets of barrier buffers */
    mem_management_per_proc_per_block= 2 * opal_cache_line_size ;
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
    sm_module->size_mem_banks_ctl_region=mem_management_total;

    /*
     * Memory for blocking collectives - need two sets of memory
     *   regions for this.
     */
    /* size per proc */
    size=2*sizeof(mca_coll_sm2_nb_request_process_shared_mem_t);
    /* page align */
    size=(size +
            getpagesize() -1 ) / getpagesize();
    size*=getpagesize();
    sm_module->per_proc_size_of_blocking_barrier_region=size;
    sm_module->size_of_blocking_barrier_region=size*group_size;


    /* total size of backing file - this assumes the mmap allocation
     *   occurs on page boundaries, and that all segments are paged
     *   aligned 
     */
    size_sm2_backing_file=sm_module->size_mem_banks_ctl_region+
        sm_module->size_of_collective_buffer_region+
        sm_module->size_of_blocking_barrier_region;
    sm_module->size_sm2_backing_file=size_sm2_backing_file;

    /* set file name */
    /*
    len=asprintf(&(sm_module->coll_sm2_file_name),
            "%s"OPAL_PATH_SEP"sm_coll_v2%s_%0d\0",orte_process_info.job_session_dir,
            orte_process_info.nodename,ompi_comm_get_cid(comm));
    if( 0 > len ) {
        goto CLEANUP;
    }
    */

    /* allocate backing file */
    ret=allocate_shared_file(size_sm2_backing_file,
            &(sm_module->coll_sm2_file_name), comm,
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

    /* initialize reduction tree */
    sm_module->reduction_tree=(tree_node_t *) malloc(
            sizeof(tree_node_t )*group_size);
    if( NULL == sm_module->reduction_tree ) {
        goto CLEANUP;
    }
    
    ret=setup_multinomial_tree(mca_coll_sm2_component.order_reduction_tree,
            group_size,sm_module->reduction_tree);
    if( MPI_SUCCESS != ret ) {
        goto CLEANUP;
    }

    /* initialize fan-out read tree */
    sm_module->fanout_read_tree=(tree_node_t *) malloc(
            sizeof(tree_node_t )*group_size);
    if( NULL == sm_module->fanout_read_tree ) {
        goto CLEANUP;
    }
    
    ret=setup_multinomial_tree(mca_coll_sm2_component.order_fanout_read_tree,
            group_size,sm_module->fanout_read_tree);
    if( MPI_SUCCESS != ret ) {
        goto CLEANUP;
    }

    /* initialize recursive doubling tree */
    ret=setup_recursive_doubling_tree_node(group_size, ompi_comm_rank(comm),
            &(sm_module->recursive_doubling_tree));
    if( MPI_SUCCESS != ret ) {
        goto CLEANUP;
    }
    
    /* initialize local counters */
    sm_module->sm2_allocated_buffer_index=-1;
    sm_module->sm2_freed_buffer_index=-1;

    /* setup shared memory memory descriptors */
    for( i=0 ; i < sm_module->sm2_module_num_buffers ; i++ ) {

        char *base_buffer;
        volatile mca_coll_sm2_nb_request_process_shared_mem_t *ctl_ptr;

        /* set the base address for this working buffer */
        base_buffer= sm_module->collective_buffer_region+
            /* offset past control data structures */
            size_buff_ctl_per_proc*group_size +
            i*sm_module->segment_size;
        sm_module->sm_buffer_descriptor[i].base_segment_address=base_buffer;

        /* allocate array to keep data on each segment in the buffer.
         *   One segment per process in the group.
         */
        sm_module->sm_buffer_descriptor[i].proc_memory=
            (sm_memory_region_desc_t *)malloc(sizeof(sm_memory_region_desc_t)*
                                            group_size);
        if( NULL == sm_module->sm_buffer_descriptor[i].proc_memory ) {
            goto CLEANUP;
        }

        /* set bank index */
        sm_module->sm_buffer_descriptor[i].bank_index=
            i/sm_module->sm2_module_num_regions_per_bank;
        sm_module->sm_buffer_descriptor[i].index_first_buffer_in_bank=
            sm_module->sm_buffer_descriptor[i].bank_index *
            sm_module->sm2_module_num_regions_per_bank;
        sm_module->sm_buffer_descriptor[i].index_last_buffer_in_bank=
            ((sm_module->sm_buffer_descriptor[i].bank_index+1) *
            sm_module->sm2_module_num_regions_per_bank)-1;

        for(j=0 ; j < group_size ; j++ ) {
            ctl_ptr=(volatile mca_coll_sm2_nb_request_process_shared_mem_t *)
                (base_buffer+j* sm_module->segement_size_per_process);
            sm_module->sm_buffer_descriptor[i].proc_memory[j].control_region=
                (volatile mca_coll_sm2_nb_request_process_shared_mem_t *)
                /* offset to temp space */
                (sm_module->collective_buffer_region+
                /* offset to the per-proc control region */
                size_buff_ctl_per_proc*j+
                /* offset to control structure for the i'th buffer */
                ctl_memory_per_proc_per_segment*i);
            sm_module->sm_buffer_descriptor[i].proc_memory[j].data_segment=
                (char *)base_buffer+
                /* offset to data segment for the j'th proc */
                j*size_data_buff_per_proc;
            /* initialize the control region */
            sm_module->sm_buffer_descriptor[i].proc_memory[j].control_region->
                flag=0;
            sm_module->sm_buffer_descriptor[i].proc_memory[j].control_region->flag=0;
        }

    }

    /* allocate process private scratch space */
    sm_module->scratch_space=(int *)malloc(sizeof(int)*group_size);
    if( NULL == sm_module->scratch_space) {
        goto CLEANUP;
    }

    /* 
     * setup blocking barrier data structures 
     */
    sm_module->sm_blocking_barrier_region=
        sm_module->shared_memory_region+
        sm_module->size_mem_banks_ctl_region+
        sm_module->size_of_collective_buffer_region;

    sm_module->index_blocking_barrier_memory_bank=0;

    sm_module->ctl_blocking_barrier=
        (volatile mca_coll_sm2_nb_request_process_shared_mem_t ***)
        malloc(2*sizeof(mca_coll_sm2_nb_request_process_shared_mem_t **));
    if( NULL == sm_module->ctl_blocking_barrier ) {
        goto CLEANUP;
    }
    sm_module->ctl_blocking_barrier[0]=
        (mca_coll_sm2_nb_request_process_shared_mem_t **)
        malloc(group_size*sizeof(mca_coll_sm2_nb_request_process_shared_mem_t *));
    if( NULL == sm_module->ctl_blocking_barrier[0]) {
        goto CLEANUP;
    }
    sm_module->ctl_blocking_barrier[1]=
        (mca_coll_sm2_nb_request_process_shared_mem_t **)
        malloc(group_size*sizeof(mca_coll_sm2_nb_request_process_shared_mem_t *));
    if( NULL == sm_module->ctl_blocking_barrier[1]) {
        goto CLEANUP;
    }

    for( j= 0 ; j < 2 ; j++ ) {
        for( i=0 ; i < group_size ; i++ ) {
            sm_module->ctl_blocking_barrier[j][i]=
                (mca_coll_sm2_nb_request_process_shared_mem_t * ) 
                (
                sm_module->sm_blocking_barrier_region+
                j*sizeof(mca_coll_sm2_nb_request_process_shared_mem_t)+
                i*sm_module->per_proc_size_of_blocking_barrier_region )
                ;
            sm_module->ctl_blocking_barrier[j][i]->flag=0;
        }
    }

    /* set the switch-over parameter */
    sm_module->short_message_size=mca_coll_sm2_component.short_message_size;


    /* 
    ** set up process affinity information 
    ** */
    {
        opal_buffer_t* sbuffer = OBJ_NEW(opal_buffer_t);
        opal_buffer_t* rbuffer = OBJ_NEW(opal_buffer_t);
        opal_paffinity_base_cpu_set_t my_cpu_set;
        opal_list_t peers;
        orte_namelist_t *peer;
        int my_rank=ompi_comm_rank(comm);
        uint32_t dummy;
        /* use socket layout based collectives, only if explicitly discovered
        ** that we can */
        sm_module->have_socket_information=0;

        /* get the number of processors on this node */        
        ret=opal_paffinity_base_get_processor_info(&num_procs);

        /* get process affinity mask */
        OPAL_PAFFINITY_CPU_ZERO(my_cpu_set);
        ret=opal_paffinity_base_get(&my_cpu_set);
        if( OPAL_ERR_NOT_FOUND == OPAL_SOS_GET_ERROR_CODE(ret) ) {

            /* pa affinity not set, so socket index will be set to -1 */
            my_socket_index=-1;
        } else {

            my_socket_index=-1;
            /* loop over number of processors */
            for ( proc=0 ; proc < num_procs ; proc++ ) {
                if (OPAL_PAFFINITY_CPU_ISSET(proc,my_cpu_set)) {
                    opal_paffinity_base_get_map_to_socket_core(i,&socket_tmp,&core);
                    if( (-1) == socket ){
                        /* socket not set yet */
                        my_socket_index=socket_tmp;
                    } else {
                        /* the algorithms assume that procs are local to one
                        ** socket only */
                        if( my_socket_index != socket_tmp ) {
                            my_socket_index=-1;
                            break;
                        }
                    }
                }
            }

            /* get every one elses information */
            
        }
        /* prepare list of ranks */
        OBJ_CONSTRUCT(&peers, opal_list_t);
        for (i = 0; i < size; i++) {
            peer = OBJ_NEW(orte_namelist_t);
            peer->name.jobid = comm->c_local_group->grp_proc_pointers[i]->proc_name.jobid;
            peer->name.vpid =  comm->c_local_group->grp_proc_pointers[i]->proc_name.vpid;
            peer->name.epoch = comm->c_local_group->grp_proc_pointers[i]->proc_name.epoch;
            opal_list_append(&peers, &peer->item);
        }
        /* prepare send data */
        if (NULL == sbuffer || NULL == rbuffer) {
            fprintf(stderr," Can't allocte memory for sbuffer or rbuffer \n");
            fflush(stderr);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* Pack my rank , I need it because allgather doesnot work as expected */
        ret = opal_dss.pack(sbuffer, &my_rank, 1, OPAL_UINT32);
        if (ORTE_SUCCESS != ret) {
            fprintf(stderr," pack returned error %d for my_rank \n",ret);
            fflush(stderr);
            return ret;
        }

        /* Pack socket index */
        ret = opal_dss.pack(sbuffer, my_socket_index, 1, OPAL_UINT32);
        if (ORTE_SUCCESS != ret) {
            fprintf(stderr," pack returned error %d for my_socket_index \n",ret);
            fflush(stderr);
            return ret;
        }
        /* Allgather data over the comunicator */
        if (ORTE_SUCCESS != (ret = orte_grpcomm.allgather_list(&peers, sbuffer,  rbuffer))) {
            fprintf(stderr," orte_grpcomm.allgather_list returned error %d \n",ret);
            fflush(stderr);
            return ret;
        }

        /* 
        ** note !!!! - not sure why this is here, but will leave if for now
        */
        ret = opal_dss.unpack(rbuffer, &dummy, &cnt, ORTE_STD_CNTR);
        OPAL_OUTPUT_VERBOSE((10, mca_coll_base_output,"Get dummy value %d \n", dummy));
        if (ORTE_SUCCESS != ret) {
            fprintf(stderr," unpack returned error %d for dummy \n",ret);
            fflush(stderr);
            return OMPI_ERROR;
        }

        sm_module->have_socket_information=1;
        /* allocte memory to store socket information per process */
        sm_module->socket_index=(int *)malloc(sizeof(int)*ompi_comm_size(comm));
        if ( NULL == sm_module->socket_index) {
            goto DONE_WITH_SOCKET_SETUP;
        }
        for (proc = 0; proc < ompi_comm_size(comm); proc++) {
            uint32_t rem_socket_index;
            uint32_t rem_rank;

            /* note !!!! need to store the data for manipulation */
            /* unpack rank*/
            ret = opal_dss.unpack(rbuffer, &rem_rank, &cnt, OPAL_UINT32);
            if (ORTE_SUCCESS != ret) {
                fprintf(stderr," unpack returned error %d for rem_rank \n",ret);
                fflush(stderr);
                return OMPI_ERROR;
            }

            /* unpack socket index */
            ret = opal_dss.unpack(rbuffer, &rem_socket_index, &cnt, OPAL_UINT32);
            if (ORTE_SUCCESS != ret) {
                fprintf(stderr," unpack returned error %d for rem_socket_index \n",ret);
                fflush(stderr);
                return OMPI_ERROR;
            }

            sm_module->socket_index[rem_rank]=rem_socket_index;
            if( (-1) == rem_socket_index ) {
                sm_module->have_socket_information=0;
                free(sm_module->socket_index);
                sm_module->socket_index=NULL;
                goto DONE_WITH_SOCKET_SETUP;
            }

        }
        /* need to generate the required data for the collective algorithms */

        /* figure out how many sokcets are used */
        /* allocte memory to store socket information per process */
        sm_module->n_procs_per_socket=(int *)malloc(sizeof(int)*num_procs);
        if ( NULL == sm_module->socket_index) {
            goto DONE_WITH_SOCKET_SETUP;
        }
        /* initialize counters */
        for (proc = 0; proc < num_procs; proc++) {
            sm_module->n_procs_per_socket[proc]=0;
        }
        /* count how many procs are associated with a given socket */
        for (proc = 0; proc < ompi_comm_size(comm); proc++) {
            sm_module->n_procs_per_socket[sm_module->socket_index[proc]]++;
        }
        n_sockets=0;
        for (proc = 0; proc < num_procs; proc++) {
            if( 0 < sm_module->n_procs_per_socket[proc]) {
                n_sockets++;
            }
        }
        if( n_sockets == ompi_comm_size(comm) ) {
            /* only one proc per socket - no extra level of hierarchy */
            if( NULL !=  sm_module->socket_index ) {
                free(sm_module->socket_index);
                sm_module->socket_index=NULL;
            }
            if( NULL !=  sm_module->n_procs_per_socket ) {
                free(sm_module->n_procs_per_socket);
                sm_module->n_procs_per_socket=NULL;
            }
            sm_module->have_socket_information=0;
            goto DONE_WITH_SOCKET_SETUP;
        }
        /* group procs by socket - for rooted operations want to access the
        ** root directly, rather than through the intermediate designated
        ** socket "leader" */
        sm_module->sockets_in_use=(int *)malloc(sizeof(int)*n_sockets);
        if ( NULL == sm_module->sockets_in_use) {
            goto DONE_WITH_SOCKET_SETUP;
        }
        cnt=0;
        for (proc = 0; proc < num_procs; proc++) {
            if( 0 < sm_module->n_procs_per_socket[proc] ) {
                /* this is the group I belong to */
                if(sm_module->socket_index[ompi_comm_rank(comm)]==proc) {
                    sm_module->my_socket_group=cnt;
                }
                sm_module->sockets_in_use[cnt]=proc;
                cnt++;
            }
        }
        sm_module->list_of_ranks_per_socket=(int **)malloc(sizeof(int *)*n_sockets);
        if ( NULL == sm_module->list_of_ranks_per_socket) {
            goto DONE_WITH_SOCKET_SETUP;
        }
        for (j = 0; j < n_sockets; j++) {
            socket=sm_module->sockets_in_use[j];
            cnt=sm_module->n_procs_per_socket[socket];
            sm_module->list_of_ranks_per_socket[j]=(int *)malloc(sizeof(int)*cnt);
            if ( NULL == sm_module->list_of_ranks_per_socket) {
                goto DONE_WITH_SOCKET_SETUP;
            }
            cnt=0;
            for (i = 0; i < ompi_comm_size(comm); i++) {
                if( socket == sm_module->socket_index[i] ) {
                    sm_module->list_of_ranks_per_socket[j][cnt]=i;
                    cnt++;
                }
            }
        }

DONE_WITH_SOCKET_SETUP:
        /* free resources */
        OBJ_RELEASE(peer);
        OBJ_RELEASE(sbuffer);
        OBJ_RELEASE(rbuffer);
    }

    /* touch pages to apply memory affinity - Note: do we really need this or will
     * the algorithms do this */

    /* make sure all procs are done with setup - need to avoid initializing
     *  shared memory regions already in use
     */
    ret=barrier(comm,sm_module->reduction_tree);
    if( MPI_SUCCESS != ret ) {
        goto CLEANUP;
    }

    /* return */
    return &(sm_module->super);


CLEANUP:

    if( NULL != sm_module->coll_sm2_file_name ) {
        free(sm_module->coll_sm2_file_name);
        sm_module->coll_sm2_file_name=NULL;
    }

    if( NULL != sm_module->reduction_tree ) {
        free(sm_module->coll_sm2_file_name);
        sm_module->coll_sm2_file_name=NULL;
    }

    if( NULL != sm_module->sm_buffer_descriptor ) {
        for(i=0 ; i < group_size ; i++ ) {
            if(NULL != sm_module->sm_buffer_descriptor[i].proc_memory) {
                free(sm_module->sm_buffer_descriptor[i].proc_memory);
                sm_module->sm_buffer_descriptor[i].proc_memory=NULL;
            }
        }
        free(sm_module->sm_buffer_descriptor);
        sm_module->sm_buffer_descriptor=NULL;
    }

    if(sm_module->scratch_space) {
        free(sm_module->scratch_space);
        sm_module->scratch_space=NULL;
    }

    for( i= 0 ; i < group_size ; i++ ) {
        if( NULL != sm_module->ctl_blocking_barrier[0][i] ) {
            free( sm_module->ctl_blocking_barrier[0][i]);
             sm_module->ctl_blocking_barrier[0][i]=NULL;
        }
        if( NULL != sm_module->ctl_blocking_barrier[1][i] ) {
            free( sm_module->ctl_blocking_barrier[1][i]);
             sm_module->ctl_blocking_barrier[1][i]=NULL;
        }
    }
    if( NULL !=  sm_module->ctl_blocking_barrier ) {
        free(sm_module->ctl_blocking_barrier);
         sm_module->ctl_blocking_barrier=NULL;
    }
    if( NULL !=  sm_module->socket_index ) {
        free(sm_module->socket_index);
         sm_module->socket_index=NULL;
    }
    if( NULL !=  sm_module->n_procs_per_socket ) {
        free(sm_module->n_procs_per_socket);
         sm_module->n_procs_per_socket=NULL;
    }
    if( NULL !=  sm_module->sockets_in_use ) {
        for (j = 0; j < n_sockets; j++) {
            if( NULL !=  sm_module->sockets_in_use[j] ) {
                free(sm_module->sockets_in_use[j]);
                sm_module->sockets_in_use[j]=NULL;
            }
        free(sm_module->sockets_in_use);
        sm_module->sockets_in_use=NULL;
        }
    }

    OBJ_RELEASE(sm_module);

    return NULL;
}

/*
 * Init module on the communicator
 */
static int
sm2_module_enable(mca_coll_base_module_t *module,
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

/* progress barrier */
static
int progress_nb_barrier(mca_coll_sm2_module_t *module)
{
    int rc,barrier_index;

    if( module->num_nb_barriers_started != 
            module->num_nb_barriers_completed ) {
        /* is there anything to progress ? */
        /* get index of barrier structure to progress.  The one to progress
         *  is the one right after the last competed nb barrier.  No need
         *  to subtract 1 for the index, as the number completed is the index
         *  of the next one to complete.
         */
        barrier_index=(module->num_nb_barriers_completed%
                module->sm2_module_num_memory_banks);
    
        rc=mca_coll_sm2_nbbarrier_intra_progress(module->module_comm,
                &(module->barrier_request[barrier_index]),
                (mca_coll_base_module_t *)module);
        if( OMPI_SUCCESS != rc ) {
            return rc;
        }
    
        /* if barrier is completed, transition it to inactive, and point to
         * the request object for then next bank
         */
        if ( NB_BARRIER_DONE == 
                module->barrier_request[barrier_index].sm2_barrier_phase ) {
    
            /* set request to inactive */
            module->barrier_request[barrier_index].sm2_barrier_phase=
                NB_BARRIER_INACTIVE;
            module->num_nb_barriers_completed++;
            /* change pointer to the shared data structure to use next time */
            module->barrier_request[barrier_index].sm_index^=1;
        }
    }

    return OMPI_SUCCESS;
}

/* allocate working buffer */
sm_work_buffer_t *alloc_sm2_shared_buffer(mca_coll_sm2_module_t *module)
{
    /* local variables */
    int rc,buffer_index;

    /* progress active barrier */
    rc=progress_nb_barrier(module);
    if( OMPI_SUCCESS != rc ) {
        return NULL;
    }

    /* get next buffer index */
    module->sm2_allocated_buffer_index++;

    /* check for wrap-around */
    if( module->sm2_allocated_buffer_index == module->sm2_module_num_buffers ) {
        module->sm2_allocated_buffer_index=0;
    }

    /* If this is the first buffer in the bank, see if the barrier
     *   needs to be completed
     */
    buffer_index=module->sm2_allocated_buffer_index;
    if( buffer_index == 
            module->sm_buffer_descriptor[buffer_index].
            index_first_buffer_in_bank ) {
        /* are there incomplete barriers ? */
        int num_incomlete_barriers=module->num_nb_barriers_started -
            module->num_nb_barriers_completed;

        /* only complete the one we want to use.  If there are less than
         * module->sm2_module_num_memory_banks active banks, not need to
         * worry about completion, as completion is ordered.
         */
        while( num_incomlete_barriers == module->sm2_module_num_memory_banks ) {
            rc=progress_nb_barrier(module);
            if( OMPI_SUCCESS != rc ) {
                return NULL;
            }
            num_incomlete_barriers=module->num_nb_barriers_started -
                module->num_nb_barriers_completed;
        }

    } /* end pooling waiting to be able to use the memory bank */


    return &(module->sm_buffer_descriptor[buffer_index]);

}

/* free working buffer - it is assumed that buffers are released in
 * the order they are allocated.  We can assume this because each
 * communiator will have only one outstanding collective at a given
 * time, and we ensure that operations are completed in order. */
int free_sm2_shared_buffer(mca_coll_sm2_module_t *module)
{
    /* local variables */
    int rc,buffer_index;
    mca_coll_sm2_nb_request_process_private_mem_t *request;

    /* progress active barrier */
    rc=progress_nb_barrier(module);
    if( OMPI_SUCCESS != rc ) {
        return rc;
    }

    /* get next buffer index */
    module->sm2_freed_buffer_index++;
    /* check for wrap-around */
    if(  module->sm2_freed_buffer_index == module->sm2_module_num_buffers ) {
        module->sm2_freed_buffer_index=0;
    }

    buffer_index=module->sm2_freed_buffer_index;
    if( buffer_index == 
            module->sm_buffer_descriptor[buffer_index].
            index_last_buffer_in_bank ) {
        int barrier_index=module->
            sm_buffer_descriptor[buffer_index].bank_index;

        /* start non-blocking barrier */
        request=&(module->barrier_request[barrier_index]);
        rc=mca_coll_sm2_nbbarrier_intra(module->module_comm,
                request,(mca_coll_base_module_t *)module);
        if( OMPI_SUCCESS !=rc ) {
            return rc;
        }
        module->num_nb_barriers_started++;
        /* the mca_coll_sm2_nbbarrier_intra never completes the barrier,
         * so no need to check.  This is needed for order completion.
         */
    }


    /* return */
    return OMPI_SUCCESS;
}


OBJ_CLASS_INSTANCE(mca_coll_sm2_module_t,
                   mca_coll_base_module_t,
                   mca_coll_sm2_module_construct,
                   mca_coll_sm2_module_destruct);
