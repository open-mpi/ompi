/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(BTL_UCT_TYPES_H)
#define BTL_UCT_TYPES_H

#include "opal/mca/btl/btl.h"

/* forward declarations */
struct mca_btl_uct_module_t;
struct mca_btl_base_endpoint_t;
struct mca_btl_uct_base_frag_t;

/* TL endpoint flags */
/** connection data was received */
#define MCA_BTL_UCT_ENDPOINT_FLAG_CONN_REC       0x1
/** remote endpoint read */
#define MCA_BTL_UCT_ENDPOINT_FLAG_CONN_REM_READY 0x2
/** connection was established */
#define MCA_BTL_UCT_ENDPOINT_FLAG_CONN_READY     0x4

/* AM tags */
/** BTL fragment */
#define MCA_BTL_UCT_FRAG         0x0d
/** connection request */
#define MCA_BTL_UCT_CONNECT_RDMA 0x0e

/** maximum number of modules supported by the btl component */
#define MCA_BTL_UCT_MAX_MODULES 16
/** maximum number of UCT workers */
#define MCA_BTL_UCT_MAX_WORKERS 64

/**
 * @brief MODEx data
 */
struct mca_btl_uct_modex_t {
    /** number of modules whose data is stored in this modex */
    int32_t module_count;

    /** variable length modex data */
    uint8_t data[];
};

typedef struct mca_btl_uct_modex_t mca_btl_uct_modex_t;

/**
 * @brief BTL UCT memory domain structure
 *
 * Each BTL module supports a single memory domain. Each memory domain has
 * one or more transport layers.
 */
struct mca_btl_uct_md_t {
    /** make this an opal object */
    opal_object_t super;

    /** UCT memory domain handle */
    uct_md_h uct_md;
};

typedef struct mca_btl_uct_md_t mca_btl_uct_md_t;

OBJ_CLASS_DECLARATION(mca_btl_uct_md_t);


/**
 * @brief Connection request structure
 */
struct mca_btl_uct_conn_req_t {
    /** name of the requesting process */
    opal_process_name_t proc_name;

    /** request type: 0 == endpoint data, 1 == endpoint data + remote ready */
    int type;

    /** context id that should be connected */
    int context_id;

    /** transport index that should be connected */
    int tl_index;

    /** endpoint address data */
    uint8_t ep_addr[];
};

typedef struct mca_btl_uct_conn_req_t mca_btl_uct_conn_req_t;

/**
 * @brief Transport endpoint stucture
 */
struct mca_btl_uct_tl_endpoint_t {
    /** current flags (connected, requested, etc) */
    volatile int32_t flags;

    /** UCT endpoint handle */
    uct_ep_h uct_ep;
};

typedef struct mca_btl_uct_tl_endpoint_t mca_btl_uct_tl_endpoint_t;

/**
 * @brief Structure to keep track of connection endpoints
 */
struct mca_btl_uct_connection_ep_t {
    /** opal base object */
    opal_object_t super;

    /** UCT endpoint used for connection */
    uct_ep_h uct_ep;
};

typedef struct mca_btl_uct_connection_ep_t mca_btl_uct_connection_ep_t;

OBJ_CLASS_DECLARATION(mca_btl_uct_connection_ep_t);

/**
 * @brief Context for UCT device interface
 *
 * This structure uses atomic locks to protect the UCT worker (which is not thread safe).
 * In order to make device access fast pthread mutexes are not used. To deal with recursion
 * (unavoidable with active messages) we implement an atomic lock using C11 atomics (or
 * pthread thread-specific values with older compilers).
 */
struct mca_btl_uct_device_context_t {
    /** index of this context */
    int context_id;

    /** btl module this context is associated with */
    struct mca_btl_uct_module_t *uct_btl;

    /** mutex for protecting the UCT worker */
    opal_recursive_mutex_t mutex;

    /** UCT worker handle */
    uct_worker_h uct_worker;

    /** UCT interface handle */
    uct_iface_h uct_iface;

    /** interface attributes */
    uct_iface_attr_t uct_iface_attr;

    /** RDMA completions */
    opal_free_list_t rdma_completions;

    /** complete fragments and rdma operations. this fifo is used to avoid making
     * callbacks while holding the device lock. */
    opal_fifo_t completion_fifo;

    /** progress is enabled on this context */
    bool progress_enabled;

    /** context is in AM callback */
    volatile bool in_am_callback;
};

typedef struct mca_btl_uct_device_context_t mca_btl_uct_device_context_t;

/**
 * @brief Header for all BTL UCT active messages
 */
union mca_btl_uct_am_header_t {
    /** active message header data */
    struct mca_btl_uct_am_header_data_t {
	/** callback tag */
        mca_btl_base_tag_t tag;

	/** padding */
        uint8_t            padding[7];
    } data;

    /** header value. this is 64-bits to support using this with uct_ep_am_short */
    uint64_t value;
};

typedef union mca_btl_uct_am_header_t mca_btl_uct_am_header_t;

/**
 * @brief structure to keep track of btl callback
 *
 * This structuere is passed to various uct functions. It
 * does the translation between the uct callback and the
 * btl callback.
 */
struct mca_btl_uct_uct_completion_t {
    /** allocated from a free list */
    opal_free_list_item_t super;

    /** uct completion structure */
    uct_completion_t uct_comp;

    /** AM completion context */
    struct mca_btl_uct_base_frag_t *frag;

    /** btl module associated with the callback */
    struct mca_btl_base_module_t *btl;

    /** btl endpoint associated with the callback */
    struct mca_btl_base_endpoint_t *endpoint;

    /** local address */
    void *local_address;

    /** local registration handle */
    mca_btl_base_registration_handle_t *local_handle;

    /** user callback function */
    mca_btl_base_rdma_completion_fn_t cbfunc;

    /** user callback context */
    void *cbcontext;

    /** user callback data */
    void *cbdata;

    /** device context */
    mca_btl_uct_device_context_t *dev_context;

    /** status */
    int status;
};

typedef struct mca_btl_uct_uct_completion_t mca_btl_uct_uct_completion_t;

OBJ_CLASS_DECLARATION(mca_btl_uct_uct_completion_t);

/**
 * @brief Base fragment structure
 */
struct mca_btl_uct_base_frag_t {
    /** btl base fragment */
    mca_btl_base_descriptor_t base;

    /** segments (used with the base fragment) */
    mca_btl_base_segment_t segments[2];

    /** module this fragment is associated with */
    struct mca_btl_uct_module_t *btl;

    /* tl context */
    mca_btl_uct_device_context_t *context;

    /** is this frag ready to send (only used when pending) */
    bool ready;

    /** endpoint this fragment is associated with */
    struct mca_btl_base_endpoint_t *endpoint;

    /** free list this fragment was allocated from */
    opal_free_list_t *free_list;

    /** fragment btl/uct header */
    mca_btl_uct_am_header_t header;

    /** pre-filled UCT io vector */
    uct_iov_t uct_iov;

    /** completion structure */
    mca_btl_uct_uct_completion_t comp;
};

typedef struct mca_btl_uct_base_frag_t mca_btl_uct_base_frag_t;

OBJ_CLASS_DECLARATION(mca_btl_uct_base_frag_t);

struct mca_btl_base_endpoint_t {
    /** opal base class */
    opal_object_t super;

    /** endpoint proc */
    opal_proc_t *ep_proc;

    /** mutex to protect this structure */
    opal_recursive_mutex_t ep_lock;

    /** cached connection endpoint */
    mca_btl_uct_connection_ep_t *conn_ep;

    /** endpoints into UCT for this BTL endpoint */
    mca_btl_uct_tl_endpoint_t uct_eps[][2];
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t mca_btl_uct_endpoint_t;
OBJ_CLASS_DECLARATION(mca_btl_uct_endpoint_t);

/**
 * @brief BTL UCT abstraction of a UCT transport layer
 */
struct mca_btl_uct_tl_t {
    /** make this an opal object */
    opal_list_item_t super;

    /** relative priority 0 == highest */
    int priority;

    /** memory domain associated with this tl */
    mca_btl_uct_md_t *uct_md;

    /** lock protecting tl structures */
    opal_mutex_t tl_lock;

    /** tl configuration (used for creating device contexts) */
    uct_iface_config_t *uct_tl_config;

    /** name of this tl (used for creating device contexts) */
    char *uct_tl_name;

    /** device name for this tl (used for creating device contexts) */
    char *uct_dev_name;

    /** maxiumum number of device contexts that can be created */
    int max_device_contexts;

    /** array of device contexts */
    mca_btl_uct_device_context_t **uct_dev_contexts;

    /** tl index. this is used to differentiate (if there is any difference)
     * between rdma and am endpoints */
    int tl_index;
};

typedef struct mca_btl_uct_tl_t mca_btl_uct_tl_t;
OBJ_CLASS_DECLARATION(mca_btl_uct_tl_t);

#define MCA_BTL_UCT_TL_ATTR(tl, context_id) (tl)->uct_dev_contexts[(context_id)]->uct_iface_attr

struct mca_btl_uct_pending_connection_request_t {
    opal_list_item_t super;
    uint8_t request_data[];
};

typedef struct mca_btl_uct_pending_connection_request_t mca_btl_uct_pending_connection_request_t;
OBJ_CLASS_DECLARATION(mca_btl_uct_pending_connection_request_t);

#endif /* !defined(BTL_UCT_TYPES_H) */
