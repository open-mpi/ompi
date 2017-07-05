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

/** the maximum open OFI ofi_prov - assuming system will have no more than 20 transports*/
#define MAX_OFI_PROVIDERS  40
#define RML_OFI_PROV_ID_INVALID 0xFF

/** RML/OFI key values  **/
/* (char*)  ofi socket address (type IN) of the node process is running on */
#define OPAL_RML_OFI_FI_SOCKADDR_IN     "rml.ofi.fisockaddrin"
/* (char*)  ofi socket address (type PSM) of the node process is running on */
#define OPAL_RML_OFI_FI_ADDR_PSMX       "rml.ofi.fiaddrpsmx"

// MULTI_BUF_SIZE_FACTOR defines how large the multi recv buffer will be.
// In order to use FI_MULTI_RECV feature efficiently, we need to have a
// large recv buffer so that we don't need to repost the buffer often to
// get the remaining data when the buffer is full
#define MULTI_BUF_SIZE_FACTOR 128
#define MIN_MULTI_BUF_SIZE (1024 * 1024)

#define OFIADDR    "ofiaddr"

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

    /** ofi provider ID **/
    uint8_t ofi_prov_id;

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

} ofi_transport_ofi_prov_t;


 struct orte_rml_ofi_module_t {
    orte_rml_base_module_t api;

    /** current ofi transport id the component is using, this will be initialised
     ** in the open_ofi_prov() call **/
    int  cur_transport_id;

    /** Fabric info structure of all supported transports in system **/
    struct fi_info *fi_info_list;

   /** OFI ep and corr fi_info for all the transports (ofi_providers) **/
    ofi_transport_ofi_prov_t ofi_prov[MAX_OFI_PROVIDERS];

    size_t min_ofi_recv_buf_sz;

    /** "Any source" address */
    fi_addr_t any_addr;

    /** number of ofi providers currently opened **/
    uint8_t ofi_prov_open_num;

    /** Unique message id for every message that is fragmented to be sent over OFI **/
    uint32_t    cur_msgid;

    /* hashtable stores the peer addresses */
    opal_hash_table_t   peers;

    opal_list_t     recv_msg_queue_list;
    opal_list_t     queued_routing_messages;
    opal_event_t    *timer_event;
    struct timeval  timeout;
} ;
typedef struct orte_rml_ofi_module_t orte_rml_ofi_module_t;

/* For every first send initiated to new peer 
 * select the peer provider, peer ep-addr, 
 * local provider and populate in orte_rml_ofi_peer_t instance.
 * Insert this in hash table.
 * */
typedef struct {
    opal_object_t super;
    char*   ofi_prov_name;   /* peer (dest) provider chosen */
    void*   ofi_ep;          /* peer (dest) ep chosen */
    size_t  ofi_ep_len;      /* peer (dest) ep length */
    uint8_t src_prov_id;     /* index of the local (src) provider used for this peer */
} orte_rml_ofi_peer_t;
OBJ_CLASS_DECLARATION(orte_rml_ofi_peer_t);

ORTE_MODULE_DECLSPEC extern orte_rml_component_t mca_rml_ofi_component;
extern orte_rml_ofi_module_t orte_rml_ofi;

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
void free_ofi_prov_resources( int ofi_prov_id);
void print_provider_list_info (struct fi_info *fi );
void print_provider_info (struct fi_info *cur_fi );
int cq_progress_handler(int sd, short flags, void *cbdata);
int get_ofi_prov_id( opal_list_t *attributes);

/** Send callback */
int orte_rml_ofi_send_callback(struct fi_cq_data_entry *wc,
                          orte_rml_ofi_request_t*);

/** Error callback */
int orte_rml_ofi_error_callback(struct fi_cq_err_entry *error,
                           orte_rml_ofi_request_t*);

/* OFI Recv handler */
int orte_rml_ofi_recv_handler(struct fi_cq_data_entry *wc, uint8_t ofi_prov_id);

bool user_override(void);
END_C_DECLS

#endif
