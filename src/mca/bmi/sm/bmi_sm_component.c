/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>  /* for mkfifo */

#include "include/constants.h"
#include "include/sys/cache.h"
#include "event/event.h"
#include "util/if.h"
#include "util/argv.h"
#include "util/output.h"
#include "util/sys_info.h"
#include "util/proc_info.h"
#include "mca/pml/pml.h"
#include "mca/base/mca_base_param.h"
#include "mca/base/mca_base_module_exchange.h"
#include "mca/mpool/base/base.h"
#include "mca/common/sm/common_sm_mmap.h"
#include "bmi_sm.h"
#include "bmi_sm_frag.h"
#include "bmi_sm_fifo.h"


/*
 * Local utility functions.
 */

static int mca_bmi_sm_component_exchange(void);

/*
 * Shared Memory (SM) component instance. 
 */

mca_bmi_sm_component_t mca_bmi_sm_component = {
    {  /* super is being filled in */
        /* First, the mca_base_component_t struct containing meta information
          about the component itself */
        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */
            MCA_BMI_BASE_VERSION_1_0_0,
            "sm", /* MCA component name */
            1,  /* MCA component major version */
            0,  /* MCA component minor version */
            0,  /* MCA component release version */
            mca_bmi_sm_component_open,  /* component open */
            mca_bmi_sm_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */
        {
            /* Whether the component is checkpointable or not */
            false
        },

        mca_bmi_sm_component_init,  
        mca_bmi_sm_component_progress,
    }  /* end super */
};


/*
 * utility routines for parameter registration
 */

static inline char* mca_bmi_sm_param_register_string(
    const char* param_name, 
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("ptl","sm",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}
                                                                                                                            
static inline int mca_bmi_sm_param_register_int(
    const char* param_name, 
    int default_value)
{
    int id = mca_base_param_register_int("ptl","sm",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}


/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_bmi_sm_component_open(void)
{
    /* register SM component parameters */
    mca_bmi_sm_component.sm_free_list_num =
        mca_bmi_sm_param_register_int("free_list_num", 256);
    mca_bmi_sm_component.sm_free_list_max =
        mca_bmi_sm_param_register_int("free_list_max", -1);
    mca_bmi_sm_component.sm_free_list_inc =
        mca_bmi_sm_param_register_int("free_list_inc", 256);
    mca_bmi_sm_component.sm_max_procs =
        mca_bmi_sm_param_register_int("max_procs", -1);
    mca_bmi_sm_component.sm_extra_procs =
        mca_bmi_sm_param_register_int("sm_extra_procs", -1);
    mca_bmi_sm_component.sm_mpool_name =
        mca_bmi_sm_param_register_string("mpool", "sm");
    mca_bmi_sm_component.first_fragment_size =
        mca_bmi_sm_param_register_int("first_fragment_size", 1024);
    mca_bmi_sm_component.max_fragment_size =
        mca_bmi_sm_param_register_int("max_fragment_size", 8*1024);
    mca_bmi_sm_component.fragment_alignment =
        mca_bmi_sm_param_register_int("fragment_alignment",
                CACHE_LINE_SIZE);
    mca_bmi_sm_component.size_of_cb_queue =
        mca_bmi_sm_param_register_int("size_of_cb_queue", 128);
    mca_bmi_sm_component.cb_lazy_free_freq =
        mca_bmi_sm_param_register_int("cb_lazy_free_freq", 120);
    /* make sure that queue size and lazy free frequency are consistent -
     * want to make sure that slots are freed at a rate they can be
     * reused, w/o allocating extra new circular buffer fifo arrays */
    if( (float)(mca_bmi_sm_component.cb_lazy_free_freq) >=
            0.95*(float)(mca_bmi_sm_component.size_of_cb_queue) ) {
        /* upper limit */
        mca_bmi_sm_component.cb_lazy_free_freq=
            (int)(0.95*(float)(mca_bmi_sm_component.size_of_cb_queue));
        /* lower limit */
        if( 0>= mca_bmi_sm_component.cb_lazy_free_freq ) {
            mca_bmi_sm_component.cb_lazy_free_freq=1;
        }
    }

    /* default number of extra procs to allow for future growth */
    mca_bmi_sm_component.sm_extra_procs =
        mca_bmi_sm_param_register_int("sm_extra_procs", 2);

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_bmi_sm_component.sm_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&mca_bmi_sm_component.sm_frags1, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_bmi_sm_component.sm_frags2, ompi_free_list_t);
    return OMPI_SUCCESS;
}


/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_bmi_sm_component_close(void)
{
    int return_value=OMPI_SUCCESS;

    OBJ_DESTRUCT(&mca_bmi_sm_component.sm_lock);
    OBJ_DESTRUCT(&mca_bmi_sm_component.sm_frags1);
    OBJ_DESTRUCT(&mca_bmi_sm_component.sm_frags2);

    /* unmap the shared memory control structure */
    if(mca_bmi_sm_component.mmap_file != NULL) {
        return_value=munmap(mca_bmi_sm_component.mmap_file->map_addr,
                mca_bmi_sm_component.mmap_file->map_size);
        if(-1 == return_value) {
            return_value=OMPI_ERROR;
            ompi_output(0," munmap failed :: file - %s :: errno - %d \n",
                    mca_bmi_sm_component.mmap_file->map_addr,
                    errno);
            goto CLEANUP;
        }
    
        /* unlink file, so that it will be deleted when all references
         * to it are gone - no error checking, since we want all procs
         * to call this, so that in an abnormal termination scanario,
         * this file will still get cleaned up */
        unlink(mca_bmi_sm_component.mmap_file->map_path);
    }

#if OMPI_ENABLE_PROGRESS_THREADS == 1
    /* close/cleanup fifo create for event notification */
    if(mca_bmi_sm_component.sm_fifo_fd > 0) {
        /* write a done message down the pipe */
        unsigned char cmd = DONE;
        if( write(mca_bmi_sm_component.sm_fifo_fd,&cmd,sizeof(cmd)) != 
                sizeof(cmd)){
            ompi_output(0, "mca_bmi_sm_component_close: write fifo failed: errno=%d\n",
                    errno);
        }
        ompi_thread_join(&mca_bmi_sm_component.sm_fifo_thread, NULL);
        close(mca_bmi_sm_component.sm_fifo_fd);
        unlink(mca_bmi_sm_component.sm_fifo_path);
    }
#endif


CLEANUP:

    /* return */
    return return_value;
}


/*
 *  SM component initialization
 */
mca_bmi_base_module_t** mca_bmi_sm_component_init(
    int *num_ptls, 
    bool enable_progress_threads,
    bool enable_mpi_threads)
{
    mca_bmi_base_module_t **ptls = NULL;
    int i;

    *num_ptls = 0;

    /* lookup/create shared memory pool only when used */
    mca_bmi_sm_component.sm_mpool = NULL;
    mca_bmi_sm_component.sm_mpool_base = NULL;

    /* publish shared memory parameters with the MCA framework */
    if (OMPI_SUCCESS != mca_bmi_sm_component_exchange()) {
        return NULL;
    }

#if OMPI_ENABLE_PROGRESS_THREADS == 1
    /* create a named pipe to receive events  */
    sprintf(mca_bmi_sm_component.sm_fifo_path, 
        "%s/sm_fifo.%d", orte_process_info.job_session_dir,
         orte_process_info.my_name->vpid);
    if(mkfifo(mca_bmi_sm_component.sm_fifo_path, 0660) < 0) {
        ompi_output(0, "mca_bmi_sm_component_init: mkfifo failed with errno=%d\n",errno);
        return NULL;
    }
    mca_bmi_sm_component.sm_fifo_fd = open(mca_bmi_sm_component.sm_fifo_path, O_RDWR);
    if(mca_bmi_sm_component.sm_fifo_fd < 0) {
        ompi_output(0, "mca_bmi_sm_component_init: open(%s) failed with errno=%d\n",
            mca_bmi_sm_component.sm_fifo_path, errno);
        return NULL;
    }

    OBJ_CONSTRUCT(&mca_bmi_sm_component.sm_fifo_thread, ompi_thread_t);
    mca_bmi_sm_component.sm_fifo_thread.t_run = (ompi_thread_fn_t) mca_bmi_sm_component_event_thread;
    ompi_thread_start(&mca_bmi_sm_component.sm_fifo_thread);
#endif

    /* allocate the Shared Memory PTL */
    *num_ptls = 2;
    ptls = malloc((*num_ptls)*sizeof(mca_bmi_base_module_t*));
    if (NULL == ptls) {
        return NULL;
    }

    /* get pointer to the ptls */
    ptls[0] = (mca_bmi_base_module_t *)(&(mca_bmi_sm[0]));
    ptls[1] = (mca_bmi_base_module_t *)(&(mca_bmi_sm[1]));

    /* set scheduling parameters */
    for( i=0 ; i < 2 ; i++ ) {
        mca_bmi_sm[i].super.bmi_first_frag_size=mca_bmi_sm_component.first_fragment_size;
        mca_bmi_sm[i].super.bmi_min_frag_size=mca_bmi_sm_component.max_fragment_size;
        mca_bmi_sm[i].super.bmi_max_frag_size=mca_bmi_sm_component.max_fragment_size;
        mca_bmi_sm[i].super.bmi_exclusivity=100;  /* always use this ptl */
        mca_bmi_sm[i].super.bmi_latency=100;      /* lowest latency */
        mca_bmi_sm[i].super.bmi_bandwidth=900; /* not really used now since exclusivity is set to 100 */
    }

    /* initialize some PTL data */
    /* start with no SM procs */
    mca_bmi_sm_component.num_smp_procs = 0;
    mca_bmi_sm_component.my_smp_rank   = 0xFFFFFFFF;  /* not defined */

    /* set flag indicating ptl not inited */
    mca_bmi_sm[0].bmi_inited=false;
    mca_bmi_sm[1].bmi_inited=false;

    return ptls;
}


/*
 *  SM component progress.
 */

#if OMPI_ENABLE_PROGRESS_THREADS == 1
void mca_bmi_sm_component_event_thread(ompi_object_t* thread)
{
    while(1) {
        unsigned char cmd;
        if(read(mca_bmi_sm_component.sm_fifo_fd, &cmd, sizeof(cmd)) != sizeof(cmd)) {
            /* error condition */
            return;
        }
        if( DONE == cmd ){
            /* return when done message received */
            return;
        } 
        mca_bmi_sm_component_progress(0);
    }
}
#endif


int mca_bmi_sm_component_progress(void)
{
    /* local variables */
    unsigned int peer_smp_rank ;
    mca_bmi_sm_frag_t *frag;
    ompi_fifo_t *fifo = NULL;
    int my_smp_rank=mca_bmi_sm_component.my_smp_rank;
    int proc;
    int rc = 0;

    /* send progress is made by the PML */

    /* 
     * receive progress 
     */

    /* poll each fifo */

    /* loop over fifo's - procs with same base shared memory 
     * virtual address as this process */
    for( proc=0 ; proc < mca_bmi_sm_component.num_smp_procs_same_base_addr
            ; proc++ ) 
    {
        peer_smp_rank= mca_bmi_sm_component.list_smp_procs_same_base_addr[proc];
        fifo=&(mca_bmi_sm_component.fifo[peer_smp_rank][my_smp_rank]);

        /* if fifo is not yet setup - continue - not data has been sent*/
        if(OMPI_CB_FREE == fifo->tail){
            continue;
        }

        /* aquire thread lock */
        if( ompi_using_threads() ) {
            ompi_atomic_lock( &(fifo->tail_lock) );
        }

        /* get pointer - pass in offset to change queue pointer
         * addressing from that of the sender */
        frag = (mca_bmi_sm_frag_t *)
	    ompi_fifo_read_from_tail_same_base_addr( fifo );
        if( OMPI_CB_FREE == frag ) {
            /* release thread lock */
            if( ompi_using_threads() ) {
                ompi_atomic_unlock(&(fifo->tail_lock));
            }
            continue;
        }

        /* release thread lock */
        if( ompi_using_threads() ) {
            ompi_atomic_unlock(&(fifo->tail_lock));
        }

        /* dispatch fragment by type */
        switch(frag->type) {
            case MCA_BMI_SM_FRAG_ACK:
            {
                /* completion callback */
                frag->base.des_cbfunc(&mca_bmi_sm[0].super, frag->endpoint, &frag->base, frag->rc);
                break;
            }
            case MCA_BMI_SM_FRAG_SEND:
            {
                /* recv upcall */
                mca_bmi_sm_registration_t* reg = mca_bmi_sm[0].sm_reg + frag->tag;
                reg->cbfunc(&mca_bmi_sm[0].super,frag->tag,&frag->base,reg->cbdata);
                frag->type = MCA_BMI_SM_FRAG_ACK;
                MCA_BMI_SM_FIFO_WRITE(my_smp_rank,peer_smp_rank,frag,rc);
                if(OMPI_SUCCESS != rc)
                    return rc;
                break;
            }
            default:
            {
                /* unknown */
                frag->rc = OMPI_ERROR;
                frag->type = MCA_BMI_SM_FRAG_ACK;
                MCA_BMI_SM_FIFO_WRITE(my_smp_rank,peer_smp_rank,frag,rc);
                if(OMPI_SUCCESS != rc)
                    return rc;
                break;
            }
        }
        rc++;
    }  /* end peer_local_smp_rank loop */

#if 0
    /* loop over fifo's - procs with different base shared memory 
     * virtual address as this process */
    for( proc=0 ; proc < mca_bmi_sm_component.num_smp_procs_different_base_addr
            ; proc++ ) 
    {
        peer_smp_rank= mca_bmi_sm_component.list_smp_procs_different_base_addr[proc];
        fifo=&(mca_bmi_sm_component.fifo[peer_smp_rank][my_smp_rank]);

        /* if fifo is not yet setup - continue - not data has been sent*/
        if(OMPI_CB_FREE == fifo->tail){
            continue;
        }

        /* aquire thread lock */
        if( ompi_using_threads() ) {
            ompi_atomic_lock(&(fifo->tail_lock));
        }

        /* get pointer - pass in offset to change queue pointer
         * addressing from that of the sender */
        frag=(mca_bmi_sm_frag_t *)ompi_fifo_read_from_tail( fifo,
                mca_bmi_sm_component.sm_offset[peer_smp_rank]);
        if( OMPI_CB_FREE == frag ) {
            /* release thread lock */
            if( ompi_using_threads() ) {
                ompi_atomic_unlock(&(fifo->tail_lock));
            }
            continue;
        }

        /* release thread lock */
        if( ompi_using_threads() ) {
            ompi_atomic_unlock(&(fifo->tail_lock));
        }

        /* change the address from address relative to the shared
         * memory address, to a true virtual address */
        frag = (mca_bmi_sm_frag_t *)( (char *)frag+
                mca_bmi_sm_component.sm_offset[peer_smp_rank]);

        rc++;
    }  /* end peer_local_smp_rank loop */
#endif
    return rc;
}


/*
 *
 */

static int mca_bmi_sm_component_exchange()
{
    mca_bmi_sm_exchange_t mca_bmi_sm_setup_info;
    size_t len,size;
    char *ptr;
    int rc;

    /* determine length of host name */
    len=strlen(orte_system_info.nodename);
    /* check if string is zero length or there is an error */
    if( 0 >= len) {
        return OMPI_ERROR;
    }
    /* check if string is too long */
    if( MCA_BMI_SM_MAX_HOSTNAME_LEN < (len+1) ){
        return OMPI_ERROR;
    }

    /* copy string into structure that will be used to send data around */
    ptr=NULL;
    ptr=strncpy(&(mca_bmi_sm_setup_info.host_name[0]),
            orte_system_info.nodename, len);
    if( NULL == ptr ) {
        return OMPI_ERROR;
    }
    mca_bmi_sm_setup_info.host_name[len]='\0';


    /* exchange setup information */
    size=sizeof(mca_bmi_sm_exchange_t);
    rc =  mca_base_modex_send(&mca_bmi_sm_component.super.bmi_version, 
            &mca_bmi_sm_setup_info, size);
    
    return OMPI_SUCCESS;
}

