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


mca_ptl_ib_component_t mca_ptl_ib_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */

            MCA_PTL_BASE_VERSION_1_0_0,

            "ib", /* MCA component name */
            1,  /* MCA component major version */
            0,  /* MCA component minor version */
            0,  /* MCA component release version */
            mca_ptl_ib_component_open,  /* component open */
            mca_ptl_ib_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */

        {
            /* Whether the component is checkpointable or not */

            false
        },

        mca_ptl_ib_component_init,  
        mca_ptl_ib_component_control,
        mca_ptl_ib_component_progress,
    }
};

/*
 * functions for receiving event callbacks
 */

static void mca_ptl_ib_component_recv_handler(int, short, void*);


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
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_ptl_ib_component_open(void)
{
    D_PRINT("Opening InfiniBand component ...\n");

    OBJ_CONSTRUCT(&mca_ptl_ib_component.ib_procs, ompi_list_t);
    
    /* register super component parameters */
    mca_ptl_ib_module.super.ptl_exclusivity =
        mca_ptl_ib_param_register_int ("exclusivity", 0);
    mca_ptl_ib_module.super.ptl_first_frag_size =
        mca_ptl_ib_param_register_int ("first_frag_size",
                (2048 - sizeof(mca_ptl_base_header_t))/*magic*/);
    mca_ptl_ib_module.super.ptl_min_frag_size =
        mca_ptl_ib_param_register_int ("min_frag_size",
                (2048 - sizeof(mca_ptl_base_header_t))/*magic*/);
    mca_ptl_ib_module.super.ptl_max_frag_size =
        mca_ptl_ib_param_register_int ("max_frag_size", 2<<30);

    /* register IB component parameters */
    mca_ptl_ib_component.ib_free_list_num =
        mca_ptl_ib_param_register_int ("free_list_num", 32);
    mca_ptl_ib_component.ib_free_list_max =
        mca_ptl_ib_param_register_int ("free_list_max", 1024);
    mca_ptl_ib_component.ib_free_list_inc =
        mca_ptl_ib_param_register_int ("free_list_inc", 32);

    return OMPI_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_ptl_ib_component_close(void)
{
    fprintf(stderr,"[%s][%d]\n", __FILE__, __LINE__);
    /* Stub */
    return OMPI_SUCCESS;
}

/*
 *  Register IB component addressing information. The MCA framework
 *  will make this available to all peers. 
 */

static int mca_ptl_ib_component_send(void)
{
    int i, rc, size;
    mca_ptl_ib_ud_addr_t* ud_qp_addr = NULL;

    size = sizeof(mca_ptl_ib_ud_addr_t) * mca_ptl_ib_component.ib_num_ptl_modules;

    ud_qp_addr = (mca_ptl_ib_ud_addr_t*) malloc(size);

    if(NULL == ud_qp_addr) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for(i = 0; i < mca_ptl_ib_component.ib_num_ptl_modules; i++) {
        mca_ptl_ib_module_t* ptl = mca_ptl_ib_component.ib_ptl_modules[i];
        ud_qp_addr[i].ud_qp = ptl->ud_qp_hndl;
        ud_qp_addr[i].lid = ptl->port.lid;
    }

    D_PRINT("ud_qp_addr[0].ud_qp = %d\n",(int)ud_qp_addr[0].ud_qp);
    D_PRINT("ud_qp_addr[0].lid = %d\n", (int)ud_qp_addr[0].lid);

    rc =  mca_base_modex_send(&mca_ptl_ib_component.super.ptlm_version, 
            ud_qp_addr, size);

    if(OMPI_SUCCESS != rc) {
        D_PRINT("mca_base_modex_send didn't succeed : %d\n", rc);
    }

    free(ud_qp_addr);

    return rc;
}

/*
 *  IB component initialization:
 *  (1) read interface list from kernel and compare against component parameters
 *      then create a PTL instance for selected interfaces
 *  (2) setup IB listen socket for incoming connection attempts
 *  (3) register PTL parameters with the MCA
 */
mca_ptl_base_module_t** mca_ptl_ib_component_init(int *num_ptl_modules, 
        bool *allow_multi_user_threads,
        bool *have_hidden_threads)
{
    mca_ptl_base_module_t **modules;
    int i, ret;

    uint32_t  num_hcas;
    mca_ptl_ib_module_t* ib_modules = NULL;
    *num_ptl_modules = 0;

    *allow_multi_user_threads = true;
    *have_hidden_threads = OMPI_HAVE_THREADS;

    D_PRINT("IB Component Init\n");

    /* need to set ompi_using_threads() as ompi_event_init() 
     * will spawn a thread if supported */
    if(OMPI_HAVE_THREADS) {
        ompi_set_using_threads(true);
    }

    if((ret = ompi_event_init()) != OMPI_SUCCESS) {
        ompi_output(0, "mca_ptl_ib_component_init: "
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

    /*mca_ptl_ib_component.ib_num_hcas = num_hcas;*/
    mca_ptl_ib_component.ib_num_hcas = 1;

    /* Number of InfiniBand PTLs is equal to
     * number of physical HCAs. Is this always the
     * case, or under some conditions, there can be
     * multiple PTLs for one HCA? */
    mca_ptl_ib_component.ib_num_ptl_modules = 
        mca_ptl_ib_component.ib_num_hcas;

    /* Not sure what max_ptl_modules does */
    mca_ptl_ib_component.ib_max_ptl_modules = 
        mca_ptl_ib_component.ib_num_hcas;

    D_PRINT("num_hcas: %d, num_ptl_modules: %d, max_ptl_modules: %d\n",
           mca_ptl_ib_component.ib_num_hcas,
          mca_ptl_ib_component.ib_num_ptl_modules,
         mca_ptl_ib_component.ib_max_ptl_modules); 

    ib_modules = (mca_ptl_ib_module_t*) malloc(sizeof(mca_ptl_ib_module_t) * 
            mca_ptl_ib_component.ib_num_ptl_modules);
    if(NULL == ib_modules) {
        return NULL;
    }

    /* Zero out the PTL struct memory region */
    memset((void*)ib_modules, 0, sizeof(mca_ptl_ib_module_t) *
            mca_ptl_ib_component.ib_num_ptl_modules);

    /* Copy the function pointers to the IB modules */
    for(i = 0; i < mca_ptl_ib_component.ib_num_ptl_modules; i++) {
        memcpy((void*)&ib_modules[i], 
                &mca_ptl_ib_module, 
                sizeof(mca_ptl_ib_module));
    }

    D_PRINT("About to initialize IB modules ...\n");

    /* For each ptl, do this */
    for(i = 0; i < mca_ptl_ib_component.ib_num_ptl_modules; i++) {

        if(mca_ptl_ib_get_hca_id(i, &ib_modules[i].hca_id) 
                != OMPI_SUCCESS) {
            return NULL;
        }

        D_PRINT("hca_id: %s\n", ib_modules[i].hca_id);

        if(mca_ptl_ib_get_hca_hndl(ib_modules[i].hca_id, &ib_modules[i].nic)
                != OMPI_SUCCESS) {
            return NULL;
        }

        D_PRINT("hca_hndl: %d\n", ib_modules[i].nic);

        /* Each HCA uses only port 1. Need to change
         * this so that each ptl can choose different
         * ports */

        if(mca_ptl_ib_query_hca_prop(ib_modules[i].nic, &ib_modules[i].port)
                != OMPI_SUCCESS) {
            return NULL;
        }

        D_PRINT("LID: %d\n", ib_modules[i].port.lid);

        if(mca_ptl_ib_alloc_pd(ib_modules[i].nic, &ib_modules[i].ptag)
                != OMPI_SUCCESS) {
            return NULL;
        }

        D_PRINT("Protection Domain: %d\n", (int)ib_modules[i].ptag);

        if(mca_ptl_ib_create_cq(ib_modules[i].nic, &ib_modules[i].cq_hndl)
                != OMPI_SUCCESS) {
            return NULL;
        }

        D_PRINT("CQ handle: %d\n", (int)ib_modules[i].cq_hndl);

        if(mca_ptl_ib_ud_cq_init(ib_modules[i].nic, &ib_modules[i].ud_scq_hndl,
                    &ib_modules[i].ud_rcq_hndl)
                != OMPI_SUCCESS) {
            return NULL;
        }

        if(mca_ptl_ib_ud_qp_init(ib_modules[i].nic, ib_modules[i].ud_rcq_hndl,
                    ib_modules[i].ud_scq_hndl, ib_modules[i].ptag,
                    &ib_modules[i].ud_qp_hndl, &ib_modules[i].ud_qp_prop)
                != OMPI_SUCCESS) {
            return NULL;
        }

        /* Attach asynchronous handler */
        if(mca_ptl_ib_set_async_handler(ib_modules[i].nic, 
                    &ib_modules[i].async_handler) 
                != OMPI_SUCCESS) {
            return NULL;
        }

        /* Prepare the UD buffers for communication:
         *
         * 1. register
         * 2. fill up descriptors
         */
        ib_modules[i].ud_buf = NULL;

        if(mca_ptl_ib_prep_ud_bufs(ib_modules[i].nic, &ib_modules[i].ud_buf) 
                != OMPI_SUCCESS) {
            return NULL;
        }

        /* Post the UD recv descriptors */
        if(mca_ptl_ib_post_ud_recv(ib_modules[i].nic, ib_modules[i].ud_qp_hndl, 
                    ib_modules[i].ud_buf)
                != OMPI_SUCCESS) {
            return NULL;
        }

        if(mca_ptl_ib_get_comp_ev_hndl(&ib_modules[i].ud_comp_ev_handler)
                != OMPI_SUCCESS) {
            return NULL;
        }

        /* Set the completion event handler for the UD recv queue */
        if(mca_ptl_ib_set_comp_ev_hndl(ib_modules[i].nic, 
                    ib_modules[i].ud_rcq_hndl,
                    ib_modules[i].ud_comp_ev_handler, 
                    (void*)NULL, &ib_modules[i].ud_comp_ev_hndl) 
                != OMPI_SUCCESS) {
            return NULL;
        }

        /* Request for interrupts on the UD recv queue */
        if(mca_ptl_ib_req_comp_notif(ib_modules[i].nic, ib_modules[i].ud_rcq_hndl)
                != OMPI_SUCCESS) {
            return NULL;
        }
    }

    /* Allocate list of IB ptl pointers */
    mca_ptl_ib_component.ib_ptl_modules = (struct mca_ptl_ib_module_t**) 
        malloc(mca_ptl_ib_component.ib_num_ptl_modules * 
                sizeof(struct mca_ptl_ib_module_t*));
    if(NULL == mca_ptl_ib_component.ib_ptl_modules) {
        return NULL;
    }

    /* Set the pointers for all IB ptls */
    for(i = 0; i < mca_ptl_ib_component.ib_num_ptl_modules; i++) {
        mca_ptl_ib_component.ib_ptl_modules[i] = &ib_modules[i];
    }

    if(mca_ptl_ib_component_send() != OMPI_SUCCESS) {
        return NULL;
    }

    /* Allocate list of MCA ptl pointers */
    modules = (mca_ptl_base_module_t**) malloc(mca_ptl_ib_component.ib_num_ptl_modules * 
            sizeof(mca_ptl_base_module_t*));
    if(NULL == modules) {
        return NULL;
    }

    memcpy(modules, mca_ptl_ib_component.ib_ptl_modules, 
            mca_ptl_ib_component.ib_num_ptl_modules * 
            sizeof(mca_ptl_ib_module_t*));

    *num_ptl_modules = mca_ptl_ib_component.ib_num_ptl_modules;

    return modules;
}

/*
 *  IB component control
 */

int mca_ptl_ib_component_control(int param, void* value, size_t size)
{
    /* Stub */
    D_PRINT("Stub\n");
    return OMPI_SUCCESS;
}


/*
 *  IB component progress.
 */

int mca_ptl_ib_component_progress(mca_ptl_tstamp_t tstamp)
{
    /* Stub */
    D_PRINT("Stub\n");
    return OMPI_SUCCESS;
}
