/*
 * Copyright (c) 2015      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_RML_OFI_RML_OFI_H
#define MCA_RML_OFI_RML_OFI_H

#include "orte_config.h"

#include "opal/dss/dss_types.h"
#include "opal/mca/event/event.h"
#include "opal/mca/pmix/pmix.h"
#include "orte/mca/rml/base/base.h"

#include <rdma/fabric.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_errno.h>
#include <rdma/fi_tagged.h>

#include "rml_ofi_request.h"


/** RML/OFI key values  **/
/* (char*)  ofi socket address (type IN) of the node process is running on */
#define OPAL_RML_OFI_FI_SOCKADDR_IN                "rml.ofi.fisockaddrin"
/* (char*)  ofi socket address (type PSM) of the node process is running on */
#define OPAL_RML_OFI_FI_ADDR_PSMX                  "rml.ofi.fiaddrpsmx"

// MULTI_BUF_SIZE_FACTOR defines how large the multi recv buffer will be.
// In order to use FI_MULTI_RECV feature efficiently, we need to have a
// large recv buffer so that we don't need to repost the buffer often to
// get the remaining data when the buffer is full
#define MULTI_BUF_SIZE_FACTOR 128
#define MIN_MULTI_BUF_SIZE (1024 * 1024)

#define CLOSE_FID(fd)                                                               \
    do {                                                                            \
        int _ret = 0;                                                               \
        if (0 != (fd)) {                                                            \
            _ret = fi_close(&(fd)->fid);                                            \
            fd = NULL;                                                              \
            if (0 != _ret) {                                                        \
                opal_output_verbose(10,orte_rml_base_framework.framework_output,    \
                                    " %s - fi_close failed with error- %d",         \
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),ret);        \
            }                                                                       \
        }                                                                           \
    } while (0);


#define RML_OFI_RETRY_UNTIL_DONE(FUNC)         \
    do {                                       \
        do {                                   \
            ret = FUNC;                        \
            if(OPAL_LIKELY(0 == ret)) {break;} \
        } while(-FI_EAGAIN == ret);            \
    } while(0);

BEGIN_C_DECLS

struct orte_rml_ofi_module_t;

/** This structure will hold the ep and all ofi objects for each transport
and also the corresponding fi_info
**/
typedef struct {

    /** OFI conduit ID **/
    uint8_t conduit_id;

    /** fi_info for this transport */
    struct fi_info *fabric_info;

    /** Fabric Domain handle */
    struct fid_fabric *fabric;

    /** Access Domain handle */
    struct fid_domain *domain;

    /** Address vector handle */
    struct fid_av *av;

    /** Completion queue handle */
    struct fid_cq *cq;

    /** Endpoint to communicate on */
    struct fid_ep *ep;

    /** Endpoint name */
    char ep_name[FI_NAME_MAX];

    /** Endpoint name length */
    size_t epnamelen;

    /** OFI memory region */
    struct fid_mr *mr_multi_recv;

    /** buffer for tx and rx */
    void *rxbuf;

    uint64_t rxbuf_size;

    /* event,fd associated with the cq */
    int fd;

    /*event associated with progress fn */
    opal_event_t progress_event;
    bool progress_ev_active;

    struct fi_context rx_ctx1;

   /* module associated with this conduit_id returned to rml
      from open_conduit call */
   struct orte_rml_ofi_module_t *ofi_module;

} ofi_transport_conduit_t;


 struct orte_rml_ofi_module_t {
    orte_rml_base_module_t api;

    /** current ofi transport id the component is using, this will be initialised
     ** in the open_conduit() call **/
    int  cur_transport_id;

    /** Fabric info structure of all supported transports in system **/
    struct fi_info *fi_info_list;

   /** OFI ep and corr fi_info for all the transports (conduit) **/
    ofi_transport_conduit_t ofi_conduits[MAX_CONDUIT];

    size_t min_ofi_recv_buf_sz;

    /** "Any source" address */
    fi_addr_t any_addr;

    /** number of conduits currently opened **/
    uint8_t conduit_open_num;

    /** Unique message id for every message that is fragmented to be sent over OFI **/
    uint32_t    cur_msgid;

    opal_list_t     recv_msg_queue_list;
    opal_list_t     queued_routing_messages;
    opal_event_t    *timer_event;
    struct timeval  timeout;
} ;
typedef struct orte_rml_ofi_module_t orte_rml_ofi_module_t;


ORTE_MODULE_DECLSPEC extern orte_rml_component_t mca_rml_ofi_component;

int orte_rml_ofi_send_buffer_nb(struct orte_rml_base_module_t *mod,
                                orte_process_name_t* peer,
                                struct opal_buffer_t* buffer,
                                orte_rml_tag_t tag,
                                orte_rml_buffer_callback_fn_t cbfunc,
                                void* cbdata);
int orte_rml_ofi_send_nb(struct orte_rml_base_module_t *mod,
                         orte_process_name_t* peer,
                         struct iovec* iov,
                         int count,
                         orte_rml_tag_t tag,
                         orte_rml_callback_fn_t cbfunc,
                         void* cbdata);

/****************** INTERNAL OFI Functions*************/
void free_conduit_resources( int conduit_id);
void print_provider_list_info (struct fi_info *fi );

/** Send callback */
int orte_rml_ofi_send_callback(struct fi_cq_data_entry *wc,
                          orte_rml_ofi_request_t*);

/** Error callback */
int orte_rml_ofi_error_callback(struct fi_cq_err_entry *error,
                           orte_rml_ofi_request_t*);

/* OFI Recv handler */
int orte_rml_ofi_recv_handler(struct fi_cq_data_entry *wc, uint8_t conduit_id);

END_C_DECLS

#endif
