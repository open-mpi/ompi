/*
 * $HEADER$
 */

/* Open MPI includes */
#include "include/constants.h"
#include "event/event.h"
#include "util/if.h"
#include "util/argv.h"
#include "util/output.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/base/mca_base_param.h"
#include "mca/base/mca_base_module_exchange.h"

/* IB ptl includes */
#include "ptl_ib.h"
#include "ptl_ib_priv.h"


mca_ptl_ib_module_1_0_0_t mca_ptl_ib_module = {
    {
        /* First, the mca_base_module_t struct containing meta information
           about the module itself */

        {
            /* Indicate that we are a pml v1.0.0 module (which also implies a
               specific MCA version) */

            MCA_PTL_BASE_VERSION_1_0_0,

            "ib", /* MCA module name */
            1,  /* MCA module major version */
            0,  /* MCA module minor version */
            0,  /* MCA module release version */
            mca_ptl_ib_module_open,  /* module open */
            mca_ptl_ib_module_close  /* module close */
        },

        /* Next the MCA v1.0.0 module meta data */

        {
            /* Whether the module is checkpointable or not */

            false
        },

        mca_ptl_ib_module_init,  
        mca_ptl_ib_module_control,
        mca_ptl_ib_module_progress,
    }
};

/*
 * functions for receiving event callbacks
 */

static void mca_ptl_ib_module_recv_handler(int, short, void*);


/*
 * utility routines for parameter registration
 */

static inline char* mca_ptl_ib_param_register_string(
        const char* param_name, 
        const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("ptl","ib",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static inline int mca_ptl_ib_param_register_int(
        const char* param_name, 
        int default_value)
{
    int id = mca_base_param_register_int("ptl","ib",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/*
 *  Called by MCA framework to open the module, registers
 *  module parameters.
 */

int mca_ptl_ib_module_open(void)
{
    D_PRINT("Opening InfiniBand module ...\n");
    /* register super module parameters */
    mca_ptl_ib.super.ptl_exclusivity =
        mca_ptl_ib_param_register_int ("exclusivity", 0);
    mca_ptl_ib.super.ptl_first_frag_size =
        mca_ptl_ib_param_register_int ("first_frag_size",
                (2048 - sizeof(mca_ptl_base_header_t))/*magic*/);
    mca_ptl_ib.super.ptl_min_frag_size =
        mca_ptl_ib_param_register_int ("min_frag_size",
                (2048 - sizeof(mca_ptl_base_header_t))/*magic*/);
    mca_ptl_ib.super.ptl_max_frag_size =
        mca_ptl_ib_param_register_int ("max_frag_size", 2<<30);

    /* register IB module parameters */
    mca_ptl_ib_module.ib_free_list_num =
        mca_ptl_ib_param_register_int ("free_list_num", 32);
    mca_ptl_ib_module.ib_free_list_max =
        mca_ptl_ib_param_register_int ("free_list_max", 1024);
    mca_ptl_ib_module.ib_free_list_inc =
        mca_ptl_ib_param_register_int ("free_list_inc", 32);

    return OMPI_SUCCESS;
}

/*
 * module cleanup - sanity checking of queue lengths
 */

int mca_ptl_ib_module_close(void)
{
    fprintf(stderr,"[%s][%d]\n", __FILE__, __LINE__);
    /* Stub */
    return OMPI_SUCCESS;
}

/*
 *  Register IB module addressing information. The MCA framework
 *  will make this available to all peers. 
 */

static int mca_ptl_ib_module_send(void)
{
    int i, rc, size;
    mca_ptl_ib_ud_addr_t* ud_qp_addr = NULL;

    size = sizeof(mca_ptl_ib_ud_addr_t) * mca_ptl_ib_module.ib_num_ptls;

    ud_qp_addr = (mca_ptl_ib_ud_addr_t*) malloc(size);

    if(NULL == ud_qp_addr) {
        return OMPI_ERROR;
    }

    for(i = 0; i < mca_ptl_ib_module.ib_num_ptls; i++) {
        mca_ptl_ib_t* ptl = mca_ptl_ib_module.ib_ptls[i];
        ud_qp_addr[i].ud_qp = ptl->ud_qp_hndl;
        ud_qp_addr[i].lid = ptl->port.lid;
    }

    D_PRINT("ud_qp_addr[0].ud_qp = %d\n",(int)ud_qp_addr[0].ud_qp);
    D_PRINT("ud_qp_addr[0].lid = %d\n", (int)ud_qp_addr[0].lid);

    rc =  mca_base_modex_send(&mca_ptl_ib_module.super.ptlm_version, 
            ud_qp_addr, size);

    free(ud_qp_addr);

    return rc;
}

/*
 *  IB module initialization:
 *  (1) read interface list from kernel and compare against module parameters
 *      then create a PTL instance for selected interfaces
 *  (2) setup IB listen socket for incoming connection attempts
 *  (3) register PTL parameters with the MCA
 */
mca_ptl_t** mca_ptl_ib_module_init(int *num_ptls, 
        bool *allow_multi_user_threads,
        bool *have_hidden_threads)
{
    mca_ptl_t **ptls;
    int i, ret;

    uint32_t  num_hcas;
    mca_ptl_ib_t* ptl_ib = NULL;
    *num_ptls = 0;

    *allow_multi_user_threads = true;
    *have_hidden_threads = OMPI_HAVE_THREADS;

    D_PRINT("IB Module Init\n");

    /* need to set ompi_using_threads() as ompi_event_init() 
     * will spawn a thread if supported */
    if(OMPI_HAVE_THREADS) {
        ompi_set_using_threads(true);
    }

    if((ret = ompi_event_init()) != OMPI_SUCCESS) {
        ompi_output(0, "mca_ptl_ib_module_init: "
                "unable to initialize event dispatch thread: %d\n", ret);
        return NULL;
    }

    ret = mca_ptl_ib_get_num_hcas(&num_hcas);

    D_PRINT("Number of HCAs found: %d\n", num_hcas);

    if ((0 == num_hcas) || (OMPI_SUCCESS != ret)) {
        return NULL;
    }

    /* HACK: To avoid confusion, right now open only 
     * one IB PTL */

    /*mca_ptl_ib_module.ib_num_hcas = num_hcas;*/
    mca_ptl_ib_module.ib_num_hcas = 1;

    /* Number of InfiniBand PTLs is equal to
     * number of physical HCAs. Is this always the
     * case, or under some conditions, there can be
     * multiple PTLs for one HCA? */
    mca_ptl_ib_module.ib_num_ptls = 
        mca_ptl_ib_module.ib_num_hcas;

    /* Not sure what max_ptls does */
    mca_ptl_ib_module.ib_max_ptls = 
        mca_ptl_ib_module.ib_num_hcas;

    D_PRINT("num_hcas: %d, num_ptls: %d, max_ptls: %d\n",
           mca_ptl_ib_module.ib_num_hcas,
          mca_ptl_ib_module.ib_num_ptls,
         mca_ptl_ib_module.ib_max_ptls); 

    ptl_ib = (mca_ptl_ib_t*) malloc(sizeof(mca_ptl_ib_t) * 
            mca_ptl_ib_module.ib_num_ptls);
    if(NULL == ptl_ib) {
        return NULL;
    }

    /* Zero out the PTL struct memory region */
    memset((void*)ptl_ib, 0, sizeof(mca_ptl_ib_t) *
            mca_ptl_ib_module.ib_num_ptls);

    /* Copy the function pointers to the IB ptls */
    for(i = 0; i< mca_ptl_ib_module.ib_num_ptls; i++) {
        memcpy((void*)&ptl_ib[i], 
                &mca_ptl_ib, 
                sizeof(mca_ptl_ib));
    }

    D_PRINT("About to initialize IB ptls ...\n");

    /* For each ptl, do this */
    for(i = 0; i < mca_ptl_ib_module.ib_num_ptls; i++) {

        if(mca_ptl_ib_get_hca_id(i, &ptl_ib[i].hca_id) 
                != OMPI_SUCCESS) {
            return NULL;
        }

        D_PRINT("hca_id: %s\n", ptl_ib[i].hca_id);

        if(mca_ptl_ib_get_hca_hndl(ptl_ib[i].hca_id, &ptl_ib[i].nic)
                != OMPI_SUCCESS) {
            return NULL;
        }

        D_PRINT("hca_hndl: %d\n", ptl_ib[i].nic);

        /* Each HCA uses only port 1. Need to change
         * this so that each ptl can choose different
         * ports */

        if(mca_ptl_ib_query_hca_prop(ptl_ib[i].nic, &ptl_ib[i].port)
                != OMPI_SUCCESS) {
            return NULL;
        }

        D_PRINT("LID: %d\n", ptl_ib[i].port.lid);

        if(mca_ptl_ib_alloc_pd(ptl_ib[i].nic, &ptl_ib[i].ptag)
                != OMPI_SUCCESS) {
            return NULL;
        }

        D_PRINT("Protection Domain: %d\n", (int)ptl_ib[i].ptag);

        if(mca_ptl_ib_create_cq(ptl_ib[i].nic, &ptl_ib[i].cq_hndl)
                != OMPI_SUCCESS) {
            return NULL;
        }

        D_PRINT("CQ handle: %d\n", (int)ptl_ib[i].cq_hndl);

        if(mca_ptl_ib_ud_cq_init(ptl_ib[i].nic, &ptl_ib[i].ud_scq_hndl,
                    &ptl_ib[i].ud_rcq_hndl)
                != OMPI_SUCCESS) {
            return NULL;
        }

        if(mca_ptl_ib_ud_qp_init(ptl_ib[i].nic, ptl_ib[i].ud_rcq_hndl,
                    ptl_ib[i].ud_scq_hndl, ptl_ib[i].ptag,
                    ptl_ib[i].ud_qp_hndl, ptl_ib[i].ud_qp_prop)
                != OMPI_SUCCESS) {
            return NULL;
        }

        /* Attach asynchronous handler */
        if(mca_ptl_ib_set_async_handler(ptl_ib[i].nic, 
                    ptl_ib[i].async_handler) 
                != OMPI_SUCCESS) {
            return NULL;
        }

        /* Prepare the UD buffers for communication:
         *
         * 1. register
         * 2. fill up descriptors
         */
        if(mca_ptl_ib_prep_ud_bufs(ptl_ib[i].nic, ptl_ib[i].ud_buf) 
                != OMPI_SUCCESS) {
            return NULL;
        }

        /* Post the UD recv descriptors */
        if(mca_ptl_ib_post_ud_recv(ptl_ib[i].nic, ptl_ib[i].ud_qp_hndl, 
                    ptl_ib[i].ud_buf)
                != OMPI_SUCCESS) {
            return NULL;
        }

        if(mca_ptl_ib_get_comp_ev_hndl(&ptl_ib[i].ud_comp_ev_handler)
                != OMPI_SUCCESS) {
            return NULL;
        }

        /* Set the completion event handler for the UD recv queue */
        if(mca_ptl_ib_set_comp_ev_hndl(ptl_ib[i].nic, 
                    ptl_ib[i].ud_rcq_hndl,
                    ptl_ib[i].ud_comp_ev_handler, 
                    (void*)NULL, &ptl_ib[i].ud_comp_ev_hndl) 
                != OMPI_SUCCESS) {
            return NULL;
        }

        /* Request for interrupts on the UD recv queue */
        if(mca_ptl_ib_req_comp_notif(ptl_ib[i].nic, ptl_ib[i].ud_rcq_hndl)
                != OMPI_SUCCESS) {
            return NULL;
        }
    }

    /* Allocate list of IB ptl pointers */
    mca_ptl_ib_module.ib_ptls = (struct mca_ptl_ib_t**) 
        malloc(mca_ptl_ib_module.ib_num_ptls * 
                sizeof(struct mca_ptl_ib_t*));
    if(NULL == mca_ptl_ib_module.ib_ptls) {
        return NULL;
    }

    /* Set the pointers for all IB ptls */
    for(i = 0; i < mca_ptl_ib_module.ib_num_ptls; i++) {
        mca_ptl_ib_module.ib_ptls[i] = &ptl_ib[i];
    }

    if(mca_ptl_ib_module_send() != OMPI_SUCCESS) {
        return NULL;
    }

    /* Allocate list of MCA ptl pointers */
    ptls = (mca_ptl_t**) malloc(mca_ptl_ib_module.ib_num_ptls * 
            sizeof(mca_ptl_t*));
    if(NULL == ptls) {
        return NULL;
    }

    memcpy(ptls, mca_ptl_ib_module.ib_ptls, 
            mca_ptl_ib_module.ib_num_ptls * 
            sizeof(mca_ptl_ib_t*));

    *num_ptls = mca_ptl_ib_module.ib_num_ptls;

    return ptls;
}

/*
 *  IB module control
 */

int mca_ptl_ib_module_control(int param, void* value, size_t size)
{
    /* Stub */
    fprintf(stderr,"[%s][%d]\n", __FILE__, __LINE__);
    return OMPI_SUCCESS;
}


/*
 *  IB module progress.
 */

int mca_ptl_ib_module_progress(mca_ptl_tstamp_t tstamp)
{
    /* Stub */
    fprintf(stderr,"[%s][%d]\n", __FILE__, __LINE__);
    return OMPI_SUCCESS;
}
