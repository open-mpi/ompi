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

    OBJ_CONSTRUCT(&mca_ptl_ib_component.ib_procs, ompi_list_t);

    OBJ_CONSTRUCT (&mca_ptl_ib_component.ib_recv_frags, ompi_free_list_t);

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
#if 0
    VAPI_ret_t ret;
    VAPI_wc_desc_t comp;
    char* env = NULL;

    if((env = getenv("POLL")) != NULL) {

        while(1) {
            ret = VAPI_poll_cq(mca_ptl_ib_module.nic, mca_ptl_ib_module.ud_rcq_hndl,
                    &comp);
            if(VAPI_OK == ret) {
                fprintf(stderr,"Something arrived!\n");
            }
        }
    }
#endif
    return OMPI_SUCCESS;
}


/*
 *  IB component progress.
 */

int mca_ptl_ib_component_progress(mca_ptl_tstamp_t tstamp)
{
#if 0
    VAPI_ret_t ret;
    VAPI_wc_desc_t comp;

    mca_ptl_base_header_t *header;
    mca_ptl_ib_recv_buf_t *recv_buf;
    mca_ptl_ib_send_buf_t *send_buf;
    mca_pml_base_request_t *req;

    D_PRINT("Checking completions ... \n");

    ret = VAPI_poll_cq(mca_ptl_ib_component.ib_ptl_modules[0]->nic, 
            mca_ptl_ib_component.ib_ptl_modules[0]->cq_hndl,
            &comp);
    if(VAPI_OK == ret) {
        if(comp.status != VAPI_SUCCESS) {
            fprintf(stderr,"Got error : %s, Vendor code : %d\n",
                    VAPI_wc_status_sym(comp.status),
                    comp.vendor_err_syndrome);

        }
        if(VAPI_CQE_SQ_SEND_DATA == comp.opcode) {
            D_PRINT("Send completion, id:%d\n",
                    comp.id);
            send_buf = (mca_ptl_ib_send_buf_t*) (unsigned int)comp.id;
            header = (mca_ptl_base_header_t*) send_buf->buf;

            req = (mca_pml_base_request_t *) send_buf->req;

            mca_ptl_ib_component.ib_ptl_modules[0]->super.ptl_send_progress(
                    mca_ptl_ib_component.ib_ptl_modules[0], 
                    req,
                    header->hdr_frag.hdr_frag_length);
        }
        else if(VAPI_CQE_RQ_SEND_DATA == comp.opcode) {
            D_PRINT("Received message completion len = %d, id : %d\n",
                    comp.byte_len, comp.id);

            recv_buf = (mca_ptl_ib_recv_buf_t*) (unsigned int)comp.id;
            header = (mca_ptl_base_header_t*) recv_buf->buf;

            switch(header->hdr_common.hdr_type) {
                case MCA_PTL_HDR_TYPE_MATCH:
                    D_PRINT("Header type match\n");

                    mca_ptl_ib_frag(mca_ptl_ib_component.ib_ptl_modules[0],
                            header);
                    break;
                case MCA_PTL_HDR_TYPE_FRAG:
                    D_PRINT("Header type frag\n");
                    break;
                default :
                    D_PRINT("Header, what header?\n");
                    break;
            }
        }
        else {
            D_PRINT("Got Unknown completion! Opcode : %d\n", 
                    comp.opcode);
        }
    }
#endif

    return OMPI_SUCCESS;
}
