/*
 * $HEADER$
 */

/* Open MPI includes */
#include "ompi_config.h"
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
    /* register super component parameters */
    mca_ptl_ib_module.super.ptl_exclusivity =
        mca_ptl_ib_param_register_int ("exclusivity", 0);

    mca_ptl_ib_module.super.ptl_first_frag_size =
        mca_ptl_ib_param_register_int ("first_frag_size",
                (MCA_PTL_IB_FIRST_FRAG_SIZE
                 - sizeof(mca_ptl_base_header_t)));

    mca_ptl_ib_module.super.ptl_min_frag_size =
        mca_ptl_ib_param_register_int ("min_frag_size",
                (MCA_PTL_IB_FIRST_FRAG_SIZE 
                 - sizeof(mca_ptl_base_header_t)));

    mca_ptl_ib_module.super.ptl_max_frag_size =
        mca_ptl_ib_param_register_int ("max_frag_size", 2<<30);

    /* register IB component parameters */
    mca_ptl_ib_component.ib_free_list_num =
        mca_ptl_ib_param_register_int ("free_list_num", 64);
    mca_ptl_ib_component.ib_free_list_max =
        mca_ptl_ib_param_register_int ("free_list_max", 1024);
    mca_ptl_ib_component.ib_free_list_inc =
        mca_ptl_ib_param_register_int ("free_list_inc", 32);

    OBJ_CONSTRUCT(&mca_ptl_ib_component.ib_procs, ompi_list_t);

    OBJ_CONSTRUCT (&mca_ptl_ib_component.ib_recv_frags, ompi_free_list_t);

    return OMPI_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_ptl_ib_component_close(void)
{
    D_PRINT("");
    /* Stub */
    return OMPI_SUCCESS;
}

/*
 *  Register IB component addressing information. The MCA framework
 *  will make this available to all peers. 
 */

static int mca_ptl_ib_component_send(void)
{
    return OMPI_SUCCESS;
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

    /* Initialize Receive fragments */
    ompi_free_list_init (&(mca_ptl_ib_component.ib_recv_frags),
            sizeof (mca_ptl_ib_recv_frag_t),
            OBJ_CLASS (mca_ptl_ib_recv_frag_t),
            mca_ptl_ib_component.ib_free_list_num,
            mca_ptl_ib_component.ib_free_list_max,
            mca_ptl_ib_component.ib_free_list_inc, NULL);


    ret = mca_ptl_ib_get_num_hcas(&num_hcas);

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

    /* Allocate space for number of modules available
     * to this component */
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

    /* For each module, Initialize! */
    for(i = 0; i < mca_ptl_ib_component.ib_num_ptl_modules; i++) {

        /* Allocate space for the state of the IB module */
        ib_modules[i].ib_state = malloc(sizeof(mca_ptl_ib_state_t));

        if(NULL == ib_modules[i].ib_state) {
            return NULL;
        }
        
        if(mca_ptl_ib_init_module(ib_modules[i].ib_state, i)
                != OMPI_SUCCESS) {
            return NULL;
        }

        /* Find a better place for this */
        OBJ_CONSTRUCT(&(ib_modules[i].send_free), ompi_free_list_t);

        A_PRINT("Free list addr : %p", &ib_modules[i].send_free);

        OBJ_CONSTRUCT(&(ib_modules[i].recv_free), ompi_free_list_t);

        ompi_free_list_init(&(ib_modules[i].send_free),
                sizeof(mca_ptl_ib_send_frag_t),
                OBJ_CLASS(mca_ptl_ib_send_frag_t),
                mca_ptl_ib_component.ib_free_list_num,
                mca_ptl_ib_component.ib_free_list_max,
                mca_ptl_ib_component.ib_free_list_inc,
                NULL);

        /* Initialize the send descriptors */
        if(mca_ptl_ib_register_send_frags((mca_ptl_base_module_t *) &ib_modules[i])
                != OMPI_SUCCESS) {

            return NULL;
        }

        DUMP_IB_STATE(ib_modules[i].ib_state);
    }

    /* Post OOB receives */
    mca_ptl_ib_post_oob_recv_nb();

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
    modules = (mca_ptl_base_module_t**) 
        malloc(mca_ptl_ib_component.ib_num_ptl_modules * 
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
    return OMPI_SUCCESS;
}


/*
 *  IB component progress.
 */

int mca_ptl_ib_component_progress(mca_ptl_tstamp_t tstamp)
{
    int i, num_procs, num_modules;
    ompi_list_item_t *item;
    mca_ptl_ib_peer_t *peer;
    mca_ptl_ib_proc_t *proc;
    mca_ptl_ib_module_t *module;
    int comp_type = IB_COMP_NOTHING;
    void* comp_addr;

    num_procs = ompi_list_get_size(&(mca_ptl_ib_component.ib_procs));

    /* Traverse the list of procs associated with the
     * IB component */

    item = ompi_list_get_first(&(mca_ptl_ib_component.ib_procs));

    for(i = 0; i < num_procs; 
            item = ompi_list_get_next(item), i++) {

        proc = (mca_ptl_ib_proc_t *) item;

        /* We only have one peer per proc right now */
        peer = (mca_ptl_ib_peer_t *) proc->proc_peers[0];

        if(!ompi_list_is_empty(&(peer->pending_send_frags))) {

            mca_ptl_ib_progress_send_frags(peer);
        }
    }

    /* Poll for completions */

    num_modules = mca_ptl_ib_component.ib_num_ptl_modules;

    for(i = 0; i < num_modules; i++) {
        
        module = mca_ptl_ib_component.ib_ptl_modules[i];

        mca_ptl_ib_drain_network(module->ib_state->nic,
                    module->ib_state->cq_hndl,
                    &comp_type, &comp_addr);

        /* Handle n/w completions */

        switch(comp_type) {
            case IB_COMP_SEND :
                D_PRINT("Caught a send completion");

                /* Process a completed send */
                mca_ptl_ib_process_send_comp(
                        (mca_ptl_base_module_t *) module,
                        comp_addr);

                break;
            case IB_COMP_RECV :
                D_PRINT("Caught a recv completion");

                /* Process incoming receives */
                mca_ptl_ib_process_recv((mca_ptl_base_module_t *)module, 
                        comp_addr);
                /* Re post recv buffers */
                mca_ptl_ib_buffer_repost(module->ib_state->nic,
                        comp_addr);
                
                break;
            case IB_COMP_RDMA_W :

                mca_ptl_ib_process_rdma_w_comp(
                        (mca_ptl_base_module_t *) module,
                        comp_addr);

                break;
            case IB_COMP_NOTHING:
                break;
            default:
                ompi_output(0, "Errorneous network completion");
                break;
        }
    }

    return OMPI_SUCCESS;
}
