/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
#include "ompi_config.h"
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif  /* HAVE_SYS_MMAN_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>  /* for mkfifo */
#endif  /* HAVE_SYS_STAT_H */

#include "ompi/constants.h"
#include "opal/sys/cache.h"
#include "opal/event/event.h"
#include "opal/util/if.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "ompi/mca/pml/pml.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/common/sm/common_sm_mmap.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "btl_sm.h"
#include "btl_sm_frag.h"
#include "btl_sm_fifo.h"



/*
 * Shared Memory (SM) component instance. 
 */

mca_btl_sm_component_t mca_btl_sm_component = {
    {  /* super is being filled in */
        /* First, the mca_base_component_t struct containing meta information
          about the component itself */
        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */
            MCA_BTL_BASE_VERSION_1_0_1,
            "sm", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_sm_component_open,  /* component open */
            mca_btl_sm_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */
        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        mca_btl_sm_component_init,  
        mca_btl_sm_component_progress,
    }  /* end super */
};


/*
 * utility routines for parameter registration
 */

static inline char* mca_btl_sm_param_register_string(
    const char* param_name, 
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("btl","sm",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}
                                                                                                                            
static inline int mca_btl_sm_param_register_int(
    const char* param_name, 
    int default_value)
{
    int id = mca_base_param_register_int("btl","sm",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}


/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_btl_sm_component_open(void)
{
    /* register SM component parameters */
    mca_btl_sm_component.sm_free_list_num =
        mca_btl_sm_param_register_int("free_list_num", 8);
    mca_btl_sm_component.sm_free_list_max =
        mca_btl_sm_param_register_int("free_list_max", -1);
    mca_btl_sm_component.sm_free_list_inc =
        mca_btl_sm_param_register_int("free_list_inc", 64);
    mca_btl_sm_component.sm_max_procs =
        mca_btl_sm_param_register_int("max_procs", -1);
    mca_btl_sm_component.sm_extra_procs =
        mca_btl_sm_param_register_int("sm_extra_procs", -1);
    mca_btl_sm_component.sm_mpool_name =
        mca_btl_sm_param_register_string("mpool", "sm");
    mca_btl_sm_component.size_of_cb_queue =
        mca_btl_sm_param_register_int("size_of_cb_queue", 128);
    mca_btl_sm_component.cb_lazy_free_freq =
        mca_btl_sm_param_register_int("cb_lazy_free_freq", 120);
    mca_btl_sm_component.cb_max_num =
        mca_btl_sm_param_register_int("cb_max_num", -1);
    /* make sure that queue size and lazy free frequency are consistent -
     * want to make sure that slots are freed at a rate they can be
     * reused, w/o allocating extra new circular buffer fifo arrays */
    if( (float)(mca_btl_sm_component.cb_lazy_free_freq) >=
            0.95*(float)(mca_btl_sm_component.size_of_cb_queue) ) {
        /* upper limit */
        mca_btl_sm_component.cb_lazy_free_freq=
            (int)(0.95*(float)(mca_btl_sm_component.size_of_cb_queue));
        /* lower limit */
        if( 0>= mca_btl_sm_component.cb_lazy_free_freq ) {
            mca_btl_sm_component.cb_lazy_free_freq=1;
        }
    }

    /* default number of extra procs to allow for future growth */
    mca_btl_sm_component.sm_extra_procs =
        mca_btl_sm_param_register_int("sm_extra_procs", 2);

    mca_btl_sm.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_HIGH-1;
    mca_btl_sm.super.btl_eager_limit = 4*1024;
    mca_btl_sm.super.btl_min_send_size = 32*1024;
    mca_btl_sm.super.btl_max_send_size = 32*1024;
    mca_btl_sm.super.btl_rdma_pipeline_send_length = 32*1024;
    mca_btl_sm.super.btl_rdma_pipeline_frag_size = 32*1024;
    mca_btl_sm.super.btl_min_rdma_pipeline_size = 0;
    mca_btl_sm.super.btl_flags = MCA_BTL_FLAGS_SEND;
    mca_btl_sm.super.btl_bandwidth = 900;
    mca_btl_sm.super.btl_latency = 100;

    mca_btl_base_param_register(&mca_btl_sm_component.super.btl_version, 
            &mca_btl_sm.super);
    mca_btl_sm_component.max_frag_size = mca_btl_sm.super.btl_max_send_size;
    mca_btl_sm_component.eager_limit = mca_btl_sm.super.btl_eager_limit;

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_frags1, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_frags2, ompi_free_list_t);
    return OMPI_SUCCESS;
}


/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_btl_sm_component_close(void)
{
    int return_value = OMPI_SUCCESS;

    OBJ_DESTRUCT(&mca_btl_sm_component.sm_lock);
    /**
     * We don't have to destroy the fragment lists. They are allocated
     * directly into the mmapped file, they will auto-magically dissapear
     * when the file get unmapped.
     */
    /*OBJ_DESTRUCT(&mca_btl_sm_component.sm_frags1);*/
    /*OBJ_DESTRUCT(&mca_btl_sm_component.sm_frags2);*/

    /* unmap the shared memory control structure */
    if(mca_btl_sm_component.mmap_file != NULL) {
        return_value = mca_common_sm_mmap_fini( mca_btl_sm_component.mmap_file );
        if( OMPI_SUCCESS != return_value ) {
            return_value=OMPI_ERROR;
            opal_output(0," munmap failed :: file - %s :: errno - %d \n",
                    mca_btl_sm_component.mmap_file->map_addr,
                    errno);
            goto CLEANUP;
        }
    
        /* unlink file, so that it will be deleted when all references
         * to it are gone - no error checking, since we want all procs
         * to call this, so that in an abnormal termination scenario,
         * this file will still get cleaned up */
        unlink(mca_btl_sm_component.mmap_file->map_path);
        OBJ_RELEASE(mca_btl_sm_component.mmap_file);
    }

#if OMPI_ENABLE_PROGRESS_THREADS == 1
    /* close/cleanup fifo create for event notification */
    if(mca_btl_sm_component.sm_fifo_fd > 0) {
        /* write a done message down the pipe */
        unsigned char cmd = DONE;
        if( write(mca_btl_sm_component.sm_fifo_fd,&cmd,sizeof(cmd)) != 
                sizeof(cmd)){
            opal_output(0, "mca_btl_sm_component_close: write fifo failed: errno=%d\n",
                    errno);
        }
        opal_thread_join(&mca_btl_sm_component.sm_fifo_thread, NULL);
        close(mca_btl_sm_component.sm_fifo_fd);
        unlink(mca_btl_sm_component.sm_fifo_path);
    }
#endif


CLEANUP:

    /* return */
    return return_value;
}

/*
 *  SM component initialization
 */
mca_btl_base_module_t** mca_btl_sm_component_init(
    int *num_btls, 
    bool enable_progress_threads,
    bool enable_mpi_threads)
{
    mca_btl_base_module_t **btls = NULL;

    *num_btls = 0;

    /* lookup/create shared memory pool only when used */
    mca_btl_sm_component.sm_mpool = NULL;
    mca_btl_sm_component.sm_mpool_base = NULL;

#if OMPI_ENABLE_PROGRESS_THREADS == 1
    /* create a named pipe to receive events  */
    sprintf( mca_btl_sm_component.sm_fifo_path, 
             "%s"OPAL_PATH_SEP"sm_fifo.%lu", orte_process_info.job_session_dir,
             (unsigned long)orte_process_info.my_name->vpid );
    if(mkfifo(mca_btl_sm_component.sm_fifo_path, 0660) < 0) {
        opal_output(0, "mca_btl_sm_component_init: mkfifo failed with errno=%d\n",errno);
        return NULL;
    }
    mca_btl_sm_component.sm_fifo_fd = open(mca_btl_sm_component.sm_fifo_path, O_RDWR);
    if(mca_btl_sm_component.sm_fifo_fd < 0) {
        opal_output(0, "mca_btl_sm_component_init: open(%s) failed with errno=%d\n",
            mca_btl_sm_component.sm_fifo_path, errno);
        return NULL;
    }

    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_fifo_thread, opal_thread_t);
    mca_btl_sm_component.sm_fifo_thread.t_run = (opal_thread_fn_t) mca_btl_sm_component_event_thread;
    opal_thread_start(&mca_btl_sm_component.sm_fifo_thread);
#endif

    /* allocate the Shared Memory BTL */
    *num_btls = 1;
    btls = (mca_btl_base_module_t**)malloc((*num_btls)*sizeof(mca_btl_base_module_t*));
    if (NULL == btls) {
        return NULL;
    }

    /* get pointer to the btls */
    btls[0] = (mca_btl_base_module_t *)(&(mca_btl_sm));

    /* initialize some PTL data */
    /* start with no SM procs */
    mca_btl_sm_component.num_smp_procs = 0;
    mca_btl_sm_component.my_smp_rank   = -1;  /* not defined */

    /* set flag indicating btl not inited */
    mca_btl_sm.btl_inited=false;

    return btls;
}


/*
 *  SM component progress.
 */

#if OMPI_ENABLE_PROGRESS_THREADS == 1
void mca_btl_sm_component_event_thread(opal_object_t* thread)
{
    while(1) {
        unsigned char cmd;
        if(read(mca_btl_sm_component.sm_fifo_fd, &cmd, sizeof(cmd)) != sizeof(cmd)) {
            /* error condition */
            return;
        }
        if( DONE == cmd ){
            /* return when done message received */
            return;
        } 
        mca_btl_sm_component_progress();
    }
}
#endif

int mca_btl_sm_component_progress(void)
{
    /* local variables */
    unsigned int peer_smp_rank ;
    mca_btl_sm_frag_t *frag;
    ompi_fifo_t *fifo = NULL;
    mca_btl_sm_hdr_t *hdr;
    int my_smp_rank=mca_btl_sm_component.my_smp_rank;
    int proc;
    int rc = 0;

    /* send progress is made by the PML */

    /* 
     * receive progress 
     */

    /* poll each fifo */
    for(proc = 0; proc < mca_btl_sm_component.num_smp_procs - 1; proc++) {
        peer_smp_rank = mca_btl_sm_component.list_smp_procs[proc];
        fifo = &(mca_btl_sm_component.fifo[my_smp_rank][peer_smp_rank]);

        /* if fifo is not yet setup - continue - not data has been sent*/
        if(OMPI_CB_FREE == fifo->tail){
            continue;
        }

        /* aquire thread lock */
        if(opal_using_threads()) {
            opal_atomic_lock(fifo->tail_lock);
        }

        hdr = (mca_btl_sm_hdr_t *)ompi_fifo_read_from_tail(fifo);

        /* release thread lock */
        if(opal_using_threads()) {
            opal_atomic_unlock(fifo->tail_lock);
        }

        if(OMPI_CB_FREE == hdr) {
            continue;
        }

        /* dispatch fragment by type */
        switch(((uintptr_t)hdr) & MCA_BTL_SM_FRAG_TYPE_MASK) {
            case MCA_BTL_SM_FRAG_ACK:
            {
                int status = (uintptr_t)hdr & MCA_BTL_SM_FRAG_STATUS_MASK;
                frag = (mca_btl_sm_frag_t *)((char*)((uintptr_t)hdr &
                            (~(MCA_BTL_SM_FRAG_TYPE_MASK |
                            MCA_BTL_SM_FRAG_STATUS_MASK))));
                /* completion callback */
                frag->base.des_cbfunc(&mca_btl_sm.super, frag->endpoint,
                        &frag->base, status?OMPI_ERROR:OMPI_SUCCESS);
                break;
            }
            case MCA_BTL_SM_FRAG_SEND:
            {
                mca_btl_sm_recv_reg_t* reg;
                /* change the address from address relative to the shared
                * memory address, to a true virtual address */
                hdr = (mca_btl_sm_hdr_t *)((char *)hdr +
                        mca_btl_sm_component.sm_offset[peer_smp_rank]);
                /* recv upcall */
                reg = mca_btl_sm.sm_reg + hdr->tag;
                MCA_BTL_SM_FRAG_ALLOC(frag, rc);
                frag->segment.seg_addr.pval = ((char*)hdr) +
                    sizeof(mca_btl_sm_hdr_t);
                frag->segment.seg_len = hdr->len;
                reg->cbfunc(&mca_btl_sm.super, hdr->tag, &frag->base,
                        reg->cbdata);
                MCA_BTL_SM_FRAG_RETURN(frag);
                MCA_BTL_SM_FIFO_WRITE(
                        mca_btl_sm_component.sm_peers[peer_smp_rank],
                        my_smp_rank, peer_smp_rank, hdr->frag, rc);
                if(OMPI_SUCCESS != rc)
                    goto err;
                break;
            }
            default:
                /* unknown */
                hdr = (mca_btl_sm_hdr_t*)((uintptr_t)hdr->frag |
                        MCA_BTL_SM_FRAG_STATUS_MASK);
                MCA_BTL_SM_FIFO_WRITE(
                        mca_btl_sm_component.sm_peers[peer_smp_rank],
                        my_smp_rank, peer_smp_rank, hdr, rc);
                if(OMPI_SUCCESS != rc)
                    goto err;
                break;
        }
        rc++;
    }
    return rc;
err:
    BTL_ERROR(("SM faild to send message due to shortage of shared memory.\n"));
    mca_btl_sm.error_cb(&mca_btl_sm.super, MCA_BTL_ERROR_FLAGS_FATAL);
    return rc;
}
