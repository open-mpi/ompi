/*
 * Copyright (c) 2015-2017 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/base.h"
#include "opal/util/argv.h"
#include "opal/util/net.h"
#include "opal/util/output.h"
#include "opal/mca/backtrace/backtrace.h"
#include "opal/mca/event/event.h"

#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/rml/rml.h"
#include "orte/mca/state/state.h"
#endif
#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "rml_ofi.h"


static int rml_ofi_component_open(void);
static int rml_ofi_component_close(void);
static int rml_ofi_component_register(void);

static int rml_ofi_component_init(void);
static orte_rml_base_module_t* open_conduit(opal_list_t *attributes);
static orte_rml_pathway_t* query_transports(void);

/**
 * component definition
 */
orte_rml_component_t mca_rml_ofi_component = {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

    .base = {
        ORTE_RML_BASE_VERSION_3_0_0,

        .mca_component_name = "ofi",
        MCA_BASE_MAKE_VERSION(component, ORTE_MAJOR_VERSION, ORTE_MINOR_VERSION,
                              ORTE_RELEASE_VERSION),
        .mca_open_component = rml_ofi_component_open,
        .mca_close_component = rml_ofi_component_close,
        .mca_register_component_params = rml_ofi_component_register
     },
    .data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    .priority = 10,
    .open_conduit = open_conduit,
    .query_transports = query_transports,
    .close_conduit = NULL
};

/* Local variables */
orte_rml_ofi_module_t orte_rml_ofi = {
    .api = {
    .component = (struct orte_rml_component_t*)&mca_rml_ofi_component,
    .ping = NULL,
    .send_nb = orte_rml_ofi_send_nb,
    .send_buffer_nb = orte_rml_ofi_send_buffer_nb,
    .purge = NULL
    }
};

/* Local variables */
static bool init_done = false;
static char *ofi_transports_supported = NULL;
static char *initial_ofi_transports_supported = NULL;
static bool ofi_desired = false;
static bool routing_desired = false;

/* return true if user override for choice of ofi provider */
bool user_override(void)
{
  if( 0 == strcmp(initial_ofi_transports_supported, ofi_transports_supported ) )
     return false;
  else
     return true;
}

static int
rml_ofi_component_open(void)
{
     /* Initialise endpoint and all queues */

    orte_rml_ofi.fi_info_list = NULL;
    orte_rml_ofi.min_ofi_recv_buf_sz = MIN_MULTI_BUF_SIZE;
    orte_rml_ofi.cur_msgid = 1;
    orte_rml_ofi.cur_transport_id = RML_OFI_PROV_ID_INVALID;
    orte_rml_ofi.ofi_prov_open_num = 0;
    OBJ_CONSTRUCT(&orte_rml_ofi.peers, opal_hash_table_t);
    opal_hash_table_init(&orte_rml_ofi.peers, 128);
    OBJ_CONSTRUCT(&orte_rml_ofi.recv_msg_queue_list, opal_list_t);

    for( uint8_t ofi_prov_id=0; ofi_prov_id < MAX_OFI_PROVIDERS ; ofi_prov_id++) {
        orte_rml_ofi.ofi_prov[ofi_prov_id].fabric =  NULL;
        orte_rml_ofi.ofi_prov[ofi_prov_id].domain =  NULL;
        orte_rml_ofi.ofi_prov[ofi_prov_id].av     =  NULL;
        orte_rml_ofi.ofi_prov[ofi_prov_id].cq     =  NULL;
        orte_rml_ofi.ofi_prov[ofi_prov_id].ep     =  NULL;
        orte_rml_ofi.ofi_prov[ofi_prov_id].ep_name[0] = 0;
        orte_rml_ofi.ofi_prov[ofi_prov_id].epnamelen = 0;
        orte_rml_ofi.ofi_prov[ofi_prov_id].mr_multi_recv = NULL;
        orte_rml_ofi.ofi_prov[ofi_prov_id].rxbuf = NULL;
        orte_rml_ofi.ofi_prov[ofi_prov_id].rxbuf_size = 0;
        orte_rml_ofi.ofi_prov[ofi_prov_id].progress_ev_active = false;
        orte_rml_ofi.ofi_prov[ofi_prov_id].ofi_prov_id = RML_OFI_PROV_ID_INVALID;
    }

    opal_output_verbose(10,orte_rml_base_framework.framework_output," from %s:%d rml_ofi_component_open()",__FILE__,__LINE__);

    if (!ORTE_PROC_IS_HNP && !ORTE_PROC_IS_DAEMON) {
        return ORTE_ERROR;
    }
    if (!ofi_desired) {
        return ORTE_ERROR;
    }
    return ORTE_SUCCESS;
}


void free_ofi_prov_resources( int ofi_prov_id)
{

    int ret=0;
    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                       " %s - free_ofi_prov_resources() begin. OFI ofi_prov_id- %d",
                       ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),ofi_prov_id);
    if (orte_rml_ofi.ofi_prov[ofi_prov_id].ep) {
        opal_output_verbose(10,orte_rml_base_framework.framework_output,
                       " %s - close ep",ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
         CLOSE_FID(orte_rml_ofi.ofi_prov[ofi_prov_id].ep);
    }
    if (orte_rml_ofi.ofi_prov[ofi_prov_id].mr_multi_recv) {
        opal_output_verbose(10,orte_rml_base_framework.framework_output,
                       " %s - close mr_multi_recv",ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
         CLOSE_FID(orte_rml_ofi.ofi_prov[ofi_prov_id].mr_multi_recv);
    }
    if (orte_rml_ofi.ofi_prov[ofi_prov_id].cq) {
        opal_output_verbose(10,orte_rml_base_framework.framework_output,
                       " %s - close cq",ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
         CLOSE_FID(orte_rml_ofi.ofi_prov[ofi_prov_id].cq);
    }
    if (orte_rml_ofi.ofi_prov[ofi_prov_id].av) {
         CLOSE_FID(orte_rml_ofi.ofi_prov[ofi_prov_id].av);
    }
    if (orte_rml_ofi.ofi_prov[ofi_prov_id].domain) {
        opal_output_verbose(10,orte_rml_base_framework.framework_output,
                       " %s - close domain",ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
         CLOSE_FID(orte_rml_ofi.ofi_prov[ofi_prov_id].domain);
    }
    if (orte_rml_ofi.ofi_prov[ofi_prov_id].fabric) {
        opal_output_verbose(10,orte_rml_base_framework.framework_output,
                       " %s - close fabric",ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
         fi_close((fid_t)orte_rml_ofi.ofi_prov[ofi_prov_id].fabric);
    }
    if (orte_rml_ofi.ofi_prov[ofi_prov_id].rxbuf) {
         free(orte_rml_ofi.ofi_prov[ofi_prov_id].rxbuf);
    }

    orte_rml_ofi.ofi_prov[ofi_prov_id].fabric =  NULL;
    orte_rml_ofi.ofi_prov[ofi_prov_id].domain =  NULL;
    orte_rml_ofi.ofi_prov[ofi_prov_id].av     =  NULL;
    orte_rml_ofi.ofi_prov[ofi_prov_id].cq     =  NULL;
    orte_rml_ofi.ofi_prov[ofi_prov_id].ep     =  NULL;
    orte_rml_ofi.ofi_prov[ofi_prov_id].ep_name[0] = 0;
    orte_rml_ofi.ofi_prov[ofi_prov_id].epnamelen = 0;
    orte_rml_ofi.ofi_prov[ofi_prov_id].rxbuf = NULL;
    orte_rml_ofi.ofi_prov[ofi_prov_id].rxbuf_size = 0;
    orte_rml_ofi.ofi_prov[ofi_prov_id].fabric_info = NULL;
    orte_rml_ofi.ofi_prov[ofi_prov_id].mr_multi_recv = NULL;
    orte_rml_ofi.ofi_prov[ofi_prov_id].ofi_prov_id = RML_OFI_PROV_ID_INVALID;


    if( orte_rml_ofi.ofi_prov[ofi_prov_id].progress_ev_active) {
        opal_output_verbose(10,orte_rml_base_framework.framework_output,
                            " %s - deleting progress event",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        opal_event_del( &orte_rml_ofi.ofi_prov[ofi_prov_id].progress_event);
    }

    return;
}


static int
rml_ofi_component_close(void)
{

    int rc;
    opal_object_t *value;
    uint64_t key;
    void *node;
    uint8_t ofi_prov_id;

    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                            " %s - rml_ofi_component_close() -begin, total open OFI providers = %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),orte_rml_ofi.ofi_prov_open_num);

    if (orte_rml_ofi.fi_info_list) {
        (void) fi_freeinfo(orte_rml_ofi.fi_info_list);
    }

    /* Close endpoint and all queues */
    for (ofi_prov_id=0; ofi_prov_id < orte_rml_ofi.ofi_prov_open_num; ofi_prov_id++) {
         free_ofi_prov_resources(ofi_prov_id);
    }

    /* release all peers from the hash table */
    rc = opal_hash_table_get_first_key_uint64(&orte_rml_ofi.peers, &key,
                                              (void **)&value, &node);
    while (OPAL_SUCCESS == rc) {
        if (NULL != value) {
          OBJ_RELEASE(value);
        }
        rc = opal_hash_table_get_next_key_uint64 (&orte_rml_ofi.peers, &key,
                                                  (void **) &value, node, &node);
    }
    OBJ_DESTRUCT(&orte_rml_ofi.peers);
    OPAL_LIST_DESTRUCT(&orte_rml_ofi.recv_msg_queue_list);

    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                        " %s - rml_ofi_component_close() end",ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    return ORTE_SUCCESS;
}

static int rml_ofi_component_register(void)
{
    mca_base_component_t *component = &mca_rml_ofi_component.base;

    initial_ofi_transports_supported = "fabric,ethernet";
    ofi_transports_supported = strdup(initial_ofi_transports_supported);
    mca_base_component_var_register(component, "transports",
                                    "Comma-delimited list of transports to support (default=\"fabric,ethernet\"",
                                    MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                    OPAL_INFO_LVL_2,
                                    MCA_BASE_VAR_SCOPE_LOCAL,
                                    &ofi_transports_supported);


    ofi_desired = false;
    mca_base_component_var_register(component, "desired",
                                    "Use OFI for coll conduit",
                                    MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                    OPAL_INFO_LVL_2,
                                    MCA_BASE_VAR_SCOPE_LOCAL,
                                    &ofi_desired);

    routing_desired = false;
    mca_base_component_var_register(component, "routing",
                                    "Route OFI messages",
                                    MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                    OPAL_INFO_LVL_2,
                                    MCA_BASE_VAR_SCOPE_LOCAL,
                                    &routing_desired);

    return ORTE_SUCCESS;
}

void print_provider_info (struct fi_info *cur_fi )
{
    //Display all the details in the fi_info structure
    opal_output_verbose(1,orte_rml_base_framework.framework_output,
                        " %s - Print_provider_info() ",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                        " Provider name  : %s",cur_fi->fabric_attr->prov_name);
    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                        " Protocol       : %s",fi_tostr(&cur_fi->ep_attr->protocol,FI_TYPE_PROTOCOL));
    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                        " EP Type        : %s",fi_tostr(&cur_fi->ep_attr->type,FI_TYPE_EP_TYPE));
    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                        " address_format : %s",fi_tostr(&cur_fi->addr_format,FI_TYPE_ADDR_FORMAT));
}

void print_provider_list_info (struct fi_info *fi )
{
    struct fi_info *cur_fi = fi;
    int fi_count = 0;
    //Display all the details in the fi_info structure
    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                        " %s - Print_provider_list_info() ",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    while( NULL != cur_fi )   {
        fi_count++;
        opal_output_verbose(10,orte_rml_base_framework.framework_output,
                            " %d.\n",fi_count);
        print_provider_info( cur_fi);
        cur_fi = cur_fi->next;
    }
    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                        "Total # of providers supported is %d\n",fi_count);
}

/*
 * This returns all the supported transports in the system that support endpoint type RDM (reliable datagram)
 * The providers returned is a list of type opal_valut_t holding opal_list_t
 */
static orte_rml_pathway_t* query_transports(void)
{

    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                        "%s:%d OFI Query Interface not implemented",__FILE__,__LINE__);
    return NULL;
}


/**
    ofi_prov [in]: the ofi ofi_prov_id that triggered the progress fn
 **/
static int orte_rml_ofi_progress(ofi_transport_ofi_prov_t* prov)
{
    ssize_t ret;
    int count=0;    /* number of messages read and processed */
    struct fi_cq_data_entry wc = { 0 };
    struct fi_cq_err_entry error = { 0 };
    orte_rml_ofi_request_t *ofi_req;

    opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s orte_rml_ofi_progress called for OFI ofi_provid %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         prov->ofi_prov_id);
    /**
    * Read the work completions from the CQ.
    * From the completion's op_context, we get the associated OFI request.
    * Call the request's callback.
    */
    while (true) {
        /* Read the cq - that triggered the libevent to call this progress fn. */
        ret = fi_cq_read(prov->cq, (void *)&wc, 1);
        if (0 < ret) {
            opal_output_verbose(15, orte_rml_base_framework.framework_output,
                         "%s cq read for OFI ofi_provid %d - wc.flags = %llx",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         prov->ofi_prov_id, (long long unsigned int)wc.flags);
            count++;
            // check the flags to see if this is a send-completion or receive
            if ( wc.flags & FI_SEND )
            {
                opal_output_verbose(15, orte_rml_base_framework.framework_output,
                       "%s Send completion received on OFI provider id %d",
                       ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                       prov->ofi_prov_id);
                if (NULL != wc.op_context) {
                    /* get the context from the wc and call the message handler */
                    ofi_req = TO_OFI_REQ(wc.op_context);
                    assert(ofi_req);
                    ret = orte_rml_ofi_send_callback(&wc, ofi_req);
                    if (ORTE_SUCCESS != ret) {
                        opal_output(orte_rml_base_framework.framework_output,
                        "Error returned by OFI send callback handler when a send completion was received on OFI prov: %zd",
                         ret);
                    }
                }
            } else if ( (wc.flags & FI_RECV) && (wc.flags & FI_MULTI_RECV) ) {
                    opal_output_verbose(15, orte_rml_base_framework.framework_output,
                         "%s Received message on OFI ofi_prov_id %d - but buffer is consumed, need to repost",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         prov->ofi_prov_id);
                         // reposting buffer
                         ret = fi_recv(orte_rml_ofi.ofi_prov[prov->ofi_prov_id].ep,
                          orte_rml_ofi.ofi_prov[prov->ofi_prov_id].rxbuf,
                          orte_rml_ofi.ofi_prov[prov->ofi_prov_id].rxbuf_size,
                          fi_mr_desc(orte_rml_ofi.ofi_prov[prov->ofi_prov_id].mr_multi_recv),
                          0,&(prov->rx_ctx1));
                        // call the receive message handler that will call the rml_base
                        ret = orte_rml_ofi_recv_handler(&wc, prov->ofi_prov_id);
                        if (ORTE_SUCCESS != ret) {
                            opal_output(orte_rml_base_framework.framework_output,
                            "Error returned by OFI Recv handler when handling the received message on the prov: %zd",
                             ret);
                        }
            } else if ( wc.flags & FI_RECV )  {
                    opal_output_verbose(15, orte_rml_base_framework.framework_output,
                         "%s Received message on OFI provider id %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         prov->ofi_prov_id);
                        // call the receive message handler that will call the rml_base
                        ret = orte_rml_ofi_recv_handler(&wc, prov->ofi_prov_id);
                        if (ORTE_SUCCESS != ret) {
                            opal_output(orte_rml_base_framework.framework_output,
                            "Error returned by OFI Recv handler when handling the received message on the OFI prov: %zd",
                             ret);
                        }
            } else if ( wc.flags & FI_MULTI_RECV ) {
                    opal_output_verbose(15, orte_rml_base_framework.framework_output,
                         "%s Received buffer overrun message on OFI provider id %d - need to repost",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         prov->ofi_prov_id);
                        // reposting buffer
                        ret = fi_recv(orte_rml_ofi.ofi_prov[prov->ofi_prov_id].ep,
                          orte_rml_ofi.ofi_prov[prov->ofi_prov_id].rxbuf,
                          orte_rml_ofi.ofi_prov[prov->ofi_prov_id].rxbuf_size,
                          fi_mr_desc(orte_rml_ofi.ofi_prov[prov->ofi_prov_id].mr_multi_recv),
                          0,&(prov->rx_ctx1));
                        if (ORTE_SUCCESS != ret) {
                            opal_output(orte_rml_base_framework.framework_output,
                            "Error returned by OFI when reposting buffer on the OFI prov: %zd",
                             ret);
                        }
            }else {
                opal_output_verbose(1,orte_rml_base_framework.framework_output,
                        "CQ has unhandled completion event with FLAG wc.flags = 0x%llx",
                         (long long unsigned int)wc.flags);
            }
        } else if (ret == -FI_EAVAIL) {
            /**
            * An error occured and is being reported via the CQ.
            * Read the error and forward it to the upper layer.
            */
            opal_output_verbose(1, orte_rml_base_framework.framework_output,
                         "%s cq_read for OFI provider id %d  returned error 0x%zx <%s>",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         prov->ofi_prov_id, ret,
                         fi_strerror((int) -ret) );
            ret = fi_cq_readerr(prov->cq,&error,0);
            if (0 > ret) {
                opal_output_verbose(1,orte_rml_base_framework.framework_output,
                                "Error returned from fi_cq_readerr: %zd", ret);
            }
            assert(error.op_context);
            /* get the context from wc and call the error handler */
            ofi_req = TO_OFI_REQ(error.op_context);
            assert(ofi_req);
            ret = orte_rml_ofi_error_callback(&error, ofi_req);
            if (ORTE_SUCCESS != ret) {
               opal_output_verbose(1,orte_rml_base_framework.framework_output,
               "Error returned by request error callback: %zd",
               ret);
            }
            break;
        } else if (ret == -FI_EAGAIN){
            /**
             * The CQ is empty. Return.
             */
            opal_output_verbose(1, orte_rml_base_framework.framework_output,
                         "%s Empty cq for OFI provider id %d,exiting from ofi_progress()",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         prov->ofi_prov_id );
            break;
        } else {
            opal_output_verbose(1, orte_rml_base_framework.framework_output,
                         "%s cq_read for OFI provider id %d  returned error 0x%zx <%s>",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         prov->ofi_prov_id, ret,
                         fi_strerror((int) -ret) );
            break;
        }
    }
    return count;
}


/*
 * call the ofi_progress() fn to read the cq
 *
 */
int cq_progress_handler(int sd, short flags, void *cbdata)
{
    ofi_transport_ofi_prov_t* prov = (ofi_transport_ofi_prov_t*)cbdata;
    int count;

    opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s cq_progress_handler called for OFI Provider id %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         prov->ofi_prov_id);

      /* call the progress fn to read the cq and process the message
        *    for the ofi provider */
        count = orte_rml_ofi_progress(prov);
        return count;
}


/*
 * Returns the number of ofi-providers available
 */
static int rml_ofi_component_init(void)
{
    int ret, fi_version;
    struct fi_info *hints, *fabric_info;
    struct fi_cq_attr cq_attr = {0};
    struct fi_av_attr av_attr = {0};
    uint8_t cur_ofi_prov;
    opal_buffer_t modex, entry, *eptr;

    opal_output_verbose(10,orte_rml_base_framework.framework_output,
                        "%s - Entering rml_ofi_component_init()",ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));


    if (init_done) {
         return orte_rml_ofi.ofi_prov_open_num;
    }


    /**
     * Hints to filter providers
     * See man fi_getinfo for a list of all filters
     * mode:  Select capabilities MTL is prepared to support.
     *        In this case, MTL will pass in context into communication calls
     * ep_type:  reliable datagram operation
     * caps:     Capabilities required from the provider.
     *           Tag matching is specified to implement MPI semantics.
     * msg_order: Guarantee that messages with same tag are ordered.
     */

    hints = fi_allocinfo();
    if (!hints) {
        opal_output_verbose(1, orte_rml_base_framework.framework_output,
                            "%s:%d: Could not allocate fi_info\n",
                            __FILE__, __LINE__);
        return orte_rml_ofi.ofi_prov_open_num;
    }

    /**
     * Refine filter for additional capabilities
     * endpoint type : Reliable datagram
     * threading:  Disable locking
     * control_progress:  enable async progress
     */
     hints->mode               = FI_CONTEXT;
     hints->ep_attr->type      = FI_EP_RDM;      /* Reliable datagram         */

     hints->domain_attr->threading        = FI_THREAD_UNSPEC;
     hints->domain_attr->control_progress = FI_PROGRESS_AUTO;
     hints->domain_attr->data_progress = FI_PROGRESS_AUTO;
     hints->domain_attr->av_type          = FI_AV_MAP;

    /**
     * FI_VERSION provides binary backward and forward compatibility support
     * Specify the version of OFI is coded to, the provider will select struct
     * layouts that are compatible with this version.
     */
     fi_version = FI_VERSION(1, 3);

    /**
     * fi_getinfo:  returns information about fabric  services for reaching a
     * remote node or service.  this does not necessarily allocate resources.
     * Pass NULL for name/service because we want a list of providers supported.
     */
     ret = fi_getinfo(fi_version,    /* OFI version requested                    */
                       NULL,          /* Optional name or fabric to resolve       */
                       NULL,          /* Optional service name or port to request */
                       0ULL,          /* Optional flag                            */
                       hints,        /* In: Hints to filter providers            */
                       &orte_rml_ofi.fi_info_list);   /* Out: List of matching providers          */
     if (0 != ret) {
        opal_output_verbose(1, orte_rml_base_framework.framework_output,
                            "%s:%d: fi_getinfo failed: %s\n",
                            __FILE__, __LINE__, fi_strerror(-ret));
        fi_freeinfo(hints);
        return ORTE_ERROR;
    }

    /* added for debug purpose - Print the provider info
    print_transports_query();
    print_provider_list_info(orte_rml_ofi.fi_info_list);
    */

    /* create a buffer for constructing our modex blob */
    OBJ_CONSTRUCT(&modex, opal_buffer_t);

    /** create the OFI objects for each transport in the system
    *   (fi_info_list) and store it in the ofi_prov array **/
    orte_rml_ofi.ofi_prov_open_num = 0;   // start the ofi_prov_id from 0
    for(fabric_info = orte_rml_ofi.fi_info_list;
        NULL != fabric_info && orte_rml_ofi.ofi_prov_open_num < MAX_OFI_PROVIDERS;
        fabric_info = fabric_info->next)
    {
        opal_output_verbose(10,orte_rml_base_framework.framework_output,
             "%s:%d beginning to add endpoint for OFI_provider_id=%d ",__FILE__,__LINE__,
             orte_rml_ofi.ofi_prov_open_num);
        print_provider_info(fabric_info);
        cur_ofi_prov = orte_rml_ofi.ofi_prov_open_num;
        orte_rml_ofi.ofi_prov[cur_ofi_prov].ofi_prov_id = orte_rml_ofi.ofi_prov_open_num ;
        orte_rml_ofi.ofi_prov[cur_ofi_prov].fabric_info = fabric_info;

        // set FI_MULTI_RECV flag for all recv operations
        fabric_info->rx_attr->op_flags = FI_MULTI_RECV;
        /**
        * Open fabric
        * The getinfo struct returns a fabric attribute struct that can be used to
        * instantiate the virtual or physical network. This opens a "fabric
        * provider". See man fi_fabric for details.
        */

        ret = fi_fabric(fabric_info->fabric_attr,        /* In:  Fabric attributes             */
                         &orte_rml_ofi.ofi_prov[cur_ofi_prov].fabric,  /* Out: Fabric handle */
                         NULL);                          /* Optional context for fabric events */
        if (0 != ret) {
            opal_output_verbose(1, orte_rml_base_framework.framework_output,
                            "%s:%d: fi_fabric failed: %s\n",
                            __FILE__, __LINE__, fi_strerror(-ret));
                orte_rml_ofi.ofi_prov[cur_ofi_prov].fabric = NULL;
                /* abort this current transport, but check if next transport can be opened */
                continue;
        }


        /**
        * Create the access domain, which is the physical or virtual network or
        * hardware port/collection of ports.  Returns a domain object that can be
        * used to create endpoints.  See man fi_domain for details.
        */
        ret = fi_domain(orte_rml_ofi.ofi_prov[cur_ofi_prov].fabric,   /* In:  Fabric object */
                         fabric_info,                                   /* In:  Provider          */
                         &orte_rml_ofi.ofi_prov[cur_ofi_prov].domain, /* Out: Domain oject  */
                          NULL);                                        /* Optional context for domain events */
        if (0 != ret) {
            opal_output_verbose(1, orte_rml_base_framework.framework_output,
                                "%s:%d: fi_domain failed: %s\n",
                                __FILE__, __LINE__, fi_strerror(-ret));
            orte_rml_ofi.ofi_prov[cur_ofi_prov].domain = NULL;
            /* abort this current transport, but check if next transport can be opened */
            continue;
        }

        /**
        * Create a transport level communication endpoint.  To use the endpoint,
        * it must be bound to completion counters or event queues and enabled,
        * and the resources consumed by it, such as address vectors, counters,
        * completion queues, etc.
        * see man fi_endpoint for more details.
        */
        ret = fi_endpoint(orte_rml_ofi.ofi_prov[cur_ofi_prov].domain, /* In:  Domain object   */
                            fabric_info,                                /* In:  Provider        */
                            &orte_rml_ofi.ofi_prov[cur_ofi_prov].ep,  /* Out: Endpoint object */
                          NULL);                                        /* Optional context     */
        if (0 != ret) {
            opal_output_verbose(1, orte_rml_base_framework.framework_output,
                                "%s:%d: fi_endpoint failed: %s\n",
                                __FILE__, __LINE__, fi_strerror(-ret));
            free_ofi_prov_resources(cur_ofi_prov);
            /* abort this current transport, but check if next transport can be opened */
            continue;
        }

        /**
        * Save the maximum inject size.
        */
         //orte_rml_ofi.max_inject_size = prov->tx_attr->inject_size;

        /**
        * Create the objects that will be bound to the endpoint.
        * The objects include:
        *     - completion queue for events
        *     - address vector of other endpoint addresses
        *     - dynamic memory-spanning memory region
        */
        cq_attr.format = FI_CQ_FORMAT_DATA;
        cq_attr.wait_obj = FI_WAIT_FD;
        cq_attr.wait_cond = FI_CQ_COND_NONE;
        ret = fi_cq_open(orte_rml_ofi.ofi_prov[cur_ofi_prov].domain,
                         &cq_attr, &orte_rml_ofi.ofi_prov[cur_ofi_prov].cq, NULL);
        if (ret) {
            opal_output_verbose(1, orte_rml_base_framework.framework_output,
                            "%s:%d: fi_cq_open failed: %s\n",
                            __FILE__, __LINE__, fi_strerror(-ret));
            free_ofi_prov_resources(cur_ofi_prov);
            /* abort this current transport, but check if next transport can be opened */
            continue;
        }

        /**
        * The remote fi_addr will be stored in the ofi_endpoint struct.
        * So, we use the AV in "map" mode.
        */
        av_attr.type = FI_AV_MAP;
        ret = fi_av_open(orte_rml_ofi.ofi_prov[cur_ofi_prov].domain,
                         &av_attr, &orte_rml_ofi.ofi_prov[cur_ofi_prov].av, NULL);
        if (ret) {
            opal_output_verbose(1, orte_rml_base_framework.framework_output,
                                "%s:%d: fi_av_open failed: %s\n",
                                __FILE__, __LINE__, fi_strerror(-ret));
            free_ofi_prov_resources(cur_ofi_prov);
            /* abort this current transport, but check if next transport can be opened */
            continue;
        }

        /**
        * Bind the CQ and AV to the endpoint object.
        */
        ret = fi_ep_bind(orte_rml_ofi.ofi_prov[cur_ofi_prov].ep,
                        (fid_t)orte_rml_ofi.ofi_prov[cur_ofi_prov].cq,
                        FI_SEND | FI_RECV);
        if (0 != ret) {
            opal_output_verbose(1, orte_rml_base_framework.framework_output,
                                "%s:%d: fi_bind CQ-EP failed: %s\n",
                                __FILE__, __LINE__, fi_strerror(-ret));
            free_ofi_prov_resources(cur_ofi_prov);
            /* abort this current transport, but check if next transport can be opened */
            continue;
        }

        ret = fi_ep_bind(orte_rml_ofi.ofi_prov[cur_ofi_prov].ep,
                        (fid_t)orte_rml_ofi.ofi_prov[cur_ofi_prov].av,
                        0);
        if (0 != ret) {
            opal_output_verbose(1, orte_rml_base_framework.framework_output,
                                "%s:%d: fi_bind AV-EP failed: %s\n",
                                __FILE__, __LINE__, fi_strerror(-ret));
            free_ofi_prov_resources(cur_ofi_prov);
            /* abort this current transport, but check if next transport can be opened */
            continue;
        }

        /**
        * Enable the endpoint for communication
        * This commits the bind operations.
        */
        ret = fi_enable(orte_rml_ofi.ofi_prov[cur_ofi_prov].ep);
        if (0 != ret) {
            opal_output_verbose(1, orte_rml_base_framework.framework_output,
                                "%s:%d: fi_enable failed: %s\n",
                                __FILE__, __LINE__, fi_strerror(-ret));
            free_ofi_prov_resources(cur_ofi_prov);
            /* abort this current transport, but check if next transport can be opened */
            continue;
        }
        opal_output_verbose(10,orte_rml_base_framework.framework_output,
                        "%s:%d ep enabled for ofi_prov_id - %d ",__FILE__,__LINE__,
                        orte_rml_ofi.ofi_prov[cur_ofi_prov].ofi_prov_id);


        /**
        * Get our address and publish it with modex.
        **/
        orte_rml_ofi.ofi_prov[cur_ofi_prov].epnamelen = sizeof (orte_rml_ofi.ofi_prov[cur_ofi_prov].ep_name);
        ret = fi_getname((fid_t)orte_rml_ofi.ofi_prov[cur_ofi_prov].ep,
                        &orte_rml_ofi.ofi_prov[cur_ofi_prov].ep_name[0],
                        &orte_rml_ofi.ofi_prov[cur_ofi_prov].epnamelen);
        if (ret) {
                    opal_output_verbose(1, orte_rml_base_framework.framework_output,
                                "%s:%d: fi_getname failed: %s\n",
                                __FILE__, __LINE__, fi_strerror(-ret));
            free_ofi_prov_resources(cur_ofi_prov);
            /* abort this current transport, but check if next transport can be opened */
              continue;
        }

        /* create the modex entry for this provider */
        OBJ_CONSTRUCT(&entry, opal_buffer_t);
        /* pack the provider's name */
        if (OPAL_SUCCESS != (ret = opal_dss.pack(&entry, &(orte_rml_ofi.ofi_prov[cur_ofi_prov].fabric_info->fabric_attr->prov_name), 1, OPAL_STRING))) {
            OBJ_DESTRUCT(&entry);
            free_ofi_prov_resources(cur_ofi_prov);
            continue;
        }
        /* pack the provider's local index */
        if (OPAL_SUCCESS != (ret = opal_dss.pack(&entry, &cur_ofi_prov, 1, OPAL_UINT8))) {
            OBJ_DESTRUCT(&entry);
            free_ofi_prov_resources(cur_ofi_prov);
            continue;
        }
        /* pack the size of the provider's connection blob */
        if (OPAL_SUCCESS != (ret = opal_dss.pack(&entry, &orte_rml_ofi.ofi_prov[cur_ofi_prov].epnamelen, 1, OPAL_SIZE))) {
            OBJ_DESTRUCT(&entry);
            free_ofi_prov_resources(cur_ofi_prov);
            continue;
        }
        /* pack the blob itself */
        if (OPAL_SUCCESS != (ret = opal_dss.pack(&entry, orte_rml_ofi.ofi_prov[cur_ofi_prov].ep_name,
                                                 orte_rml_ofi.ofi_prov[cur_ofi_prov].epnamelen, OPAL_BYTE))) {
            OBJ_DESTRUCT(&entry);
            free_ofi_prov_resources(cur_ofi_prov);
            continue;
        }
        /* add this entry to the overall modex object */
        eptr = &entry;
        if (OPAL_SUCCESS != (ret = opal_dss.pack(&modex, &eptr, 1, OPAL_BUFFER))) {
            OBJ_DESTRUCT(&entry);
            free_ofi_prov_resources(cur_ofi_prov);
            continue;
        }
        OBJ_DESTRUCT(&entry);

        /*print debug information on opal_modex_string */
        switch ( orte_rml_ofi.ofi_prov[cur_ofi_prov].fabric_info->addr_format) {
            case  FI_SOCKADDR_IN :
                opal_output_verbose(1,orte_rml_base_framework.framework_output,
                                    "%s:%d In FI_SOCKADDR_IN.  ",__FILE__,__LINE__);
                /*  Address is of type sockaddr_in (IPv4) */
                opal_output_verbose(1,orte_rml_base_framework.framework_output,
                                    "%s sending Opal modex string for ofi prov_id %d, epnamelen = %lu  ",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    cur_ofi_prov, orte_rml_ofi.ofi_prov[cur_ofi_prov].epnamelen);
                /*[debug] - print the sockaddr - port and s_addr */
                struct sockaddr_in* ep_sockaddr = (struct sockaddr_in*)orte_rml_ofi.ofi_prov[cur_ofi_prov].ep_name;
                opal_output_verbose(1,orte_rml_base_framework.framework_output,
                                    "%s port = 0x%x, InternetAddr = 0x%s  ",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    ntohs(ep_sockaddr->sin_port), inet_ntoa(ep_sockaddr->sin_addr));
                break;
        }

        /**
        * Set the ANY_SRC address.
        */
        orte_rml_ofi.any_addr = FI_ADDR_UNSPEC;

        /**
        *  Allocate tx,rx buffers and Post a multi-RECV buffer for each endpoint
        **/
        //[TODO later]  For now not considering ep_attr prefix_size (add this later)
        orte_rml_ofi.ofi_prov[cur_ofi_prov].rxbuf_size = MIN_MULTI_BUF_SIZE * MULTI_BUF_SIZE_FACTOR;
        orte_rml_ofi.ofi_prov[cur_ofi_prov].rxbuf = malloc(orte_rml_ofi.ofi_prov[cur_ofi_prov].rxbuf_size);

        ret = fi_mr_reg(orte_rml_ofi.ofi_prov[cur_ofi_prov].domain,
                        orte_rml_ofi.ofi_prov[cur_ofi_prov].rxbuf,
                        orte_rml_ofi.ofi_prov[cur_ofi_prov].rxbuf_size,
                        FI_RECV, 0, 0, 0, &orte_rml_ofi.ofi_prov[cur_ofi_prov].mr_multi_recv,
                        &orte_rml_ofi.ofi_prov[cur_ofi_prov].rx_ctx1);
        if (ret) {
                    opal_output_verbose(1, orte_rml_base_framework.framework_output,
                                "%s:%d: fi_mr_reg failed: %s\n",
                                __FILE__, __LINE__, fi_strerror(-ret));
            free_ofi_prov_resources(cur_ofi_prov);
            /* abort this current transport, but check if next transport can be opened */
              continue;
        }

        ret = fi_setopt(&orte_rml_ofi.ofi_prov[cur_ofi_prov].ep->fid, FI_OPT_ENDPOINT, FI_OPT_MIN_MULTI_RECV,
                        &orte_rml_ofi.min_ofi_recv_buf_sz, sizeof(orte_rml_ofi.min_ofi_recv_buf_sz) );
        if (ret) {
                    opal_output_verbose(1, orte_rml_base_framework.framework_output,
                                "%s:%d: fi_setopt failed: %s\n",
                                __FILE__, __LINE__, fi_strerror(-ret));
            free_ofi_prov_resources(cur_ofi_prov);
            /* abort this current transport, but check if next transport can be opened */
              continue;
        }

        ret = fi_recv(orte_rml_ofi.ofi_prov[cur_ofi_prov].ep,
                      orte_rml_ofi.ofi_prov[cur_ofi_prov].rxbuf,
                      orte_rml_ofi.ofi_prov[cur_ofi_prov].rxbuf_size,
                      fi_mr_desc(orte_rml_ofi.ofi_prov[cur_ofi_prov].mr_multi_recv),
                      0,&orte_rml_ofi.ofi_prov[cur_ofi_prov].rx_ctx1);
        if (ret) {
                    opal_output_verbose(1, orte_rml_base_framework.framework_output,
                                "%s:%d: fi_recv failed: %s\n",
                                __FILE__, __LINE__, fi_strerror(-ret));
            free_ofi_prov_resources(cur_ofi_prov);
            /* abort this current transport, but check if next transport can be opened */
              continue;
        }
        /**
        *  get the fd  and register the progress fn
        **/
        ret = fi_control(&orte_rml_ofi.ofi_prov[cur_ofi_prov].cq->fid, FI_GETWAIT,
                        (void *) &orte_rml_ofi.ofi_prov[cur_ofi_prov].fd);
        if (0 != ret) {
            opal_output_verbose(1, orte_rml_base_framework.framework_output,
                                "%s:%d: fi_control failed to get fd: %s\n",
                                __FILE__, __LINE__, fi_strerror(-ret));
            free_ofi_prov_resources(cur_ofi_prov);
            /* abort this current transport, but check if next transport can be opened */
              continue;
        }

        /* - create the event  that will wait on the fd*/
        /* use the opal_event_set to do a libevent set on the fd
        *  so when something is available to read, the cq_porgress_handler
        *  will be called */
        opal_event_set(orte_event_base,
                   &orte_rml_ofi.ofi_prov[cur_ofi_prov].progress_event,
                   orte_rml_ofi.ofi_prov[cur_ofi_prov].fd,
                   OPAL_EV_READ|OPAL_EV_PERSIST,
                   cq_progress_handler,
                   &orte_rml_ofi.ofi_prov[cur_ofi_prov]);
        opal_event_add(&orte_rml_ofi.ofi_prov[cur_ofi_prov].progress_event, 0);
        orte_rml_ofi.ofi_prov[cur_ofi_prov].progress_ev_active = true;

        /** update the number of ofi_provs in the ofi_prov[] array **/
        opal_output_verbose(10,orte_rml_base_framework.framework_output,
             "%s:%d ofi_prov id - %d created ",__FILE__,__LINE__,orte_rml_ofi.ofi_prov_open_num);
        orte_rml_ofi.ofi_prov_open_num++;
    }
    if (fabric_info != NULL &&  orte_rml_ofi.ofi_prov_open_num >= MAX_OFI_PROVIDERS ) {
        opal_output_verbose(1,orte_rml_base_framework.framework_output,
             "%s:%d fi_getinfo list not fully parsed as MAX_OFI_PROVIDERS - %d reached ",__FILE__,__LINE__,orte_rml_ofi.ofi_prov_open_num);
    }

    /**
     * Free providers info since it's not needed anymore.
    */
    fi_freeinfo(hints);
    hints = NULL;
    /* check if at least one ofi_prov was successfully opened */
    if (0 < orte_rml_ofi.ofi_prov_open_num) {
        uint8_t *data;
        int32_t sz;

        opal_output_verbose(10,orte_rml_base_framework.framework_output,
                    "%s:%d ofi providers openened=%d returning orte_rml_ofi.api",
                    __FILE__,__LINE__,orte_rml_ofi.ofi_prov_open_num);

        OBJ_CONSTRUCT(&orte_rml_ofi.recv_msg_queue_list,opal_list_t);
        /* post the modex object */
        opal_output_verbose(1, orte_rml_base_framework.framework_output,
                            "%s calling OPAL_MODEX_SEND_STRING for RML/OFI ",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        ret = opal_dss.unload(&modex, (void**)(&data), &sz);
        OBJ_DESTRUCT(&modex);
        if (OPAL_SUCCESS != ret) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        OPAL_MODEX_SEND_STRING(ret, OPAL_PMIX_GLOBAL,
                               "rml.ofi", data, sz);
        free(data);
        if (OPAL_SUCCESS != ret) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    } else {
        opal_output_verbose(1,orte_rml_base_framework.framework_output,
                    "%s:%d Failed to open any OFI Providers",__FILE__,__LINE__);
    }

    return orte_rml_ofi.ofi_prov_open_num;
}

/* return : the ofi_prov_id that corresponds to the transport requested by the attributes
            if transport is not found RML_OFI_PROV_ID_INVALID is returned.
    @[in]attributes  : the attributes passed in to open_conduit reg the transport requested
*/
int get_ofi_prov_id(opal_list_t *attributes)
{
    int ofi_prov_id = RML_OFI_PROV_ID_INVALID, prov_num=0;
    char **providers = NULL, *provider;
    struct fi_info *cur_fi;
    char *comp_attrib = NULL;
    char **comps;
    int i;
    bool choose_fabric= false;

    /* check the list of attributes in below order
     * Attribute should have ORTE_RML_TRANSPORT_ATTRIB key
     * with values "ethernet" or "fabric". "fabric" is higher priority.
     * (or)  ORTE_RML_OFI_PROV_NAME key with values "socket" or "OPA"
     * if both above attributes are missing return failure
     */
    //if (orte_get_attribute(attributes, ORTE_RML_TRANSPORT_ATTRIB, (void**)&transport, OPAL_STRING) )    {
 
    if (orte_get_attribute(attributes, ORTE_RML_TRANSPORT_TYPE, (void**)&comp_attrib, OPAL_STRING) &&
        NULL != comp_attrib) {
        comps = opal_argv_split(comp_attrib, ',');
        for (i=0; NULL != comps[i]; i++) {
            if (NULL != strstr(ofi_transports_supported, comps[i])) {
                if (0 == strcmp(comps[i], "ethernet")) {
                    opal_output_verbose(20,orte_rml_base_framework.framework_output,
                        "%s - user requested opening conduit using OFI ethernet/sockets provider",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                    opal_argv_append_nosize(&providers, "sockets");
                } else if (0 == strcmp(comps[i], "fabric")) {
                    opal_output_verbose(20,orte_rml_base_framework.framework_output,
                        "%s - user requested opening conduit using OFI fabric provider",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                    opal_argv_prepend_nosize(&providers, "fabric"); /* fabric is higher priority so prepend it */
               }
           }
       }
    }
    /* if from the transport we don't know which provider we want, then check for the ORTE_RML_OFI_PROV_NAME_ATTRIB */
    if (NULL == providers) {
        if (orte_get_attribute(attributes, ORTE_RML_PROVIDER_ATTRIB, (void**)&provider, OPAL_STRING)) {
            opal_argv_append_nosize(&providers, provider);
        } else {
            ofi_prov_id = RML_OFI_PROV_ID_INVALID;
        }
    }
    if (NULL != providers) {
        /* go down the list of preferences in order */
        for (i=0; NULL != providers[i] && RML_OFI_PROV_ID_INVALID == ofi_prov_id; i++) {
            // if generic transport "fabric" is requested then choose first available non-socket provider
            if (0 == strcmp(providers[i],"fabric")) 
                choose_fabric=true;
            else
                choose_fabric=false;
            // loop the orte_rml_ofi.ofi_provs[] and see if someone matches
            for (prov_num = 0; prov_num < orte_rml_ofi.ofi_prov_open_num; prov_num++ ) {
                cur_fi = orte_rml_ofi.ofi_prov[prov_num].fabric_info;
                if (choose_fabric) {
                   opal_output_verbose(20,orte_rml_base_framework.framework_output,
                       "%s - get_ofi_prov_id() -> comparing sockets != %s to choose first available fabric provider",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        cur_fi->fabric_attr->prov_name);
                   if (0 != strcmp("sockets", cur_fi->fabric_attr->prov_name)) {
                       ofi_prov_id = prov_num;
                       opal_output_verbose(20,orte_rml_base_framework.framework_output,
                                           "%s - Choosing provider %s",
                                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                            cur_fi->fabric_attr->prov_name);
                       break;
                   }
                } else {
                   opal_output_verbose(20,orte_rml_base_framework.framework_output,
                       "%s - get_ofi_prov_id() -> comparing %s = %s ",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            providers[i], cur_fi->fabric_attr->prov_name);
                    if (0 == strcmp(providers[i], cur_fi->fabric_attr->prov_name)) {
                        ofi_prov_id = prov_num;
                        opal_output_verbose(20,orte_rml_base_framework.framework_output,                                                                    "%s - Choosing provider %s",
                                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                            cur_fi->fabric_attr->prov_name);
                        break;
                    }
                }
            }
        }
    }
    opal_output_verbose(20,orte_rml_base_framework.framework_output,
                    "%s - get_ofi_prov_id(), returning ofi_prov_id=%d ",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),ofi_prov_id);
    return ofi_prov_id;
}

/*
 * Allocate a new module and initialise ofi_prov information
 * for the requested provider and return the module *
 */
static orte_rml_base_module_t* make_module( int ofi_prov_id)
{
    orte_rml_ofi_module_t *mod = NULL;

    opal_output_verbose(20,orte_rml_base_framework.framework_output,
                    "%s - rml_ofi make_module() begin ",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    if (RML_OFI_PROV_ID_INVALID == ofi_prov_id) {
        opal_output_verbose(20,orte_rml_base_framework.framework_output,
                    "%s - open_conduit did not select any ofi provider, returning NULL ",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        return NULL;
    }


    /* create a new module   */
    mod = (orte_rml_ofi_module_t*)calloc(1,sizeof(orte_rml_ofi_module_t));
    if (NULL == mod) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }
    /* copy the APIs over to it and the OFI provider information */
    memcpy(mod, &orte_rml_ofi, sizeof(orte_rml_ofi_module_t));
    /*  setup the remaining data locations in mod, associate conduit with ofi provider selected*/
    mod->cur_transport_id = ofi_prov_id;
    /* set the routed module */
    if (routing_desired) {
        mod->api.routed = orte_routed.assign_module(NULL);
    } else {
        mod->api.routed = orte_routed.assign_module("direct");
    }
    if (NULL == mod->api.routed) {
        /* we can't work */
        opal_output_verbose(1,orte_rml_base_framework.framework_output,
                    "%s - Failed to get%srouted support, disqualifying ourselves",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    routing_desired ? " " : " direct ");
        free(mod);
        return NULL;
    }
    return (orte_rml_base_module_t*)mod;
}


/* Order of attributes honoring          *
* ORTE_RML_INCLUDE_COMP_ATTRIB           *
* ORTE_RML_EXCLUDE_COMP_ATTRIB           *
* ORTE_RML_TRANSPORT_ATTRIB              *
* ORTE_RML_PROVIDER_ATTRIB               */
static orte_rml_base_module_t* open_conduit(opal_list_t *attributes)
{
    char *comp_attrib = NULL;
    char **comps;
    int i;
    orte_attribute_t *attr;

    opal_output_verbose(20,orte_rml_base_framework.framework_output,
                    "%s - Entering rml_ofi_open_conduit()",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* Open all ofi endpoints */
    if (!init_done) {
        rml_ofi_component_init();
        init_done = true;
    }

    /* check if atleast 1 ofi provider is initialised */
    if ( 0 >= orte_rml_ofi.ofi_prov_open_num) {
        opal_output_verbose(20,orte_rml_base_framework.framework_output,
                    "%s - Init did not open any Ofi endpoints, returning NULL",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        return NULL;
    }

    /* someone may require this specific component, so look for "ofi" */
    if (orte_get_attribute(attributes, ORTE_RML_INCLUDE_COMP_ATTRIB, (void**)&comp_attrib, OPAL_STRING) &&
        NULL != comp_attrib) {
        /* they specified specific components - could be multiple */
        comps = opal_argv_split(comp_attrib, ',');
        for (i=0; NULL != comps[i]; i++) {
            if (0 == strcmp(comps[i], "ofi")) {
                /* we are a candidate,  */
                opal_argv_free(comps);
                return make_module(get_ofi_prov_id(attributes));
            }
        }
        /* we are not a candidate */
        opal_argv_free(comps);
        return NULL;
    } else if (orte_get_attribute(attributes, ORTE_RML_EXCLUDE_COMP_ATTRIB, (void**)&comp_attrib, OPAL_STRING) &&
               NULL != comp_attrib) {
        /* see if we are on the list */
        comps = opal_argv_split(comp_attrib, ',');
        for (i=0; NULL != comps[i]; i++) {
            if (0 == strcmp(comps[i], "ofi")) {
                /* we cannot be a candidate */
                opal_argv_free(comps);
                return NULL;
            }
        }
    }

    if (orte_get_attribute(attributes, ORTE_RML_TRANSPORT_TYPE, (void**)&comp_attrib, OPAL_STRING) &&
        NULL != comp_attrib) {
                opal_output_verbose(20,orte_rml_base_framework.framework_output,
                    "%s - ORTE_RML_TRANSPORT_TYPE = %s ",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), comp_attrib);
        comps = opal_argv_split(comp_attrib, ',');
        for (i=0; NULL != comps[i]; i++) {
            if (NULL != strstr(ofi_transports_supported, comps[i])) {
                /* we are a candidate,  */
                opal_output_verbose(20,orte_rml_base_framework.framework_output,
                    "%s - Opening conduit using OFI.. ",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                opal_argv_free(comps);
                return make_module(get_ofi_prov_id(attributes));
            }
        }
        opal_argv_free(comps);
    }

    /* Alternatively, check the attributes to see if we qualify - we only handle
     * "pt2pt" */
    OPAL_LIST_FOREACH(attr, attributes, orte_attribute_t) {
        /* [TODO] add any additional attributes check here */

    }
    opal_output_verbose(20,orte_rml_base_framework.framework_output,
                    "%s - ofi is not a candidate as per attributes, returning NULL",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    /* if we get here, we cannot handle it */
    return NULL;
}

static void pr_cons(orte_rml_ofi_peer_t *ptr)
{
    ptr->ofi_prov_name = NULL;
    ptr->ofi_ep = NULL;
    ptr->ofi_ep_len = 0;
    ptr->src_prov_id = RML_OFI_PROV_ID_INVALID;
}

static void pr_des(orte_rml_ofi_peer_t *ptr)
{
    if ( NULL != ptr->ofi_prov_name)
        free(ptr->ofi_prov_name);
    if ( 0 < ptr->ofi_ep_len)
        free( ptr->ofi_ep);
}

OBJ_CLASS_INSTANCE(orte_rml_ofi_peer_t,
                   opal_object_t,
                   pr_cons, pr_des);
