/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2025      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <uct/api/uct.h>

#if !defined(BTL_UCT_TYPES_H)
#    define BTL_UCT_TYPES_H

#    include "opal/mca/btl/btl.h"

#include "opal/class/opal_fifo.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_object.h"
#include "opal/mca/timer/base/base.h"

/* forward declarations */
struct mca_btl_uct_module_t;
struct mca_btl_base_endpoint_t;
struct mca_btl_uct_base_frag_t;
struct mca_btl_uct_tl_t;

/* TL endpoint flags */
/** connection data was received */
#    define MCA_BTL_UCT_ENDPOINT_FLAG_CONN_REC 0x1
/** remote endpoint read */
#    define MCA_BTL_UCT_ENDPOINT_FLAG_CONN_REM_READY 0x2
/** local UCT endpoint connected */
#    define MCA_BTL_UCT_ENDPOINT_FLAG_EP_CONNECTED 0x4
/** connection was established */
#    define MCA_BTL_UCT_ENDPOINT_FLAG_CONN_READY 0x8

/* AM tags */
/** BTL fragment */
#    define MCA_BTL_UCT_FRAG 0x0d
/** connection request */
#    define MCA_BTL_UCT_CONNECT_RDMA 0x0e

/** maximum number of modules supported by the btl component */
#    define MCA_BTL_UCT_MAX_MODULES 16
/** maximum number of UCT workers */
#    define MCA_BTL_UCT_MAX_WORKERS 64

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
    opal_list_item_t super;

    /** if true none of the tls in this domain will be used
     * for communication */
    bool connection_only_domain;

    /** name of the memory domain backing this module */
    char *md_name;

    /** list of mca_btl_uct_tl_t's for this memory domain */
    opal_list_t tls;

    /** UCT memory domain handle */
    uct_md_h uct_md;

    /** memory domain attributes */
    uct_md_attr_t md_attr;

#if UCT_API >= UCT_VERSION(1, 7)
    uct_component_h uct_component;
#endif
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

    /** module that is being connected (local index to the receiver) */
    int module_index;

    /** endpoint address data */
    uint8_t ep_addr[];
};

typedef struct mca_btl_uct_conn_req_t mca_btl_uct_conn_req_t;

/**
 * @brief Transport endpoint structure
 */
struct mca_btl_uct_tl_endpoint_t {
    /** current flags (connected, requested, etc) */
    opal_atomic_int32_t flags;

    /** UCT endpoint handle */
    uct_ep_h uct_ep;

    /** Time of last connection message. */
    opal_timer_t last_connection_req;
};

typedef struct mca_btl_uct_tl_endpoint_t mca_btl_uct_tl_endpoint_t;

/**
 * @brief Structure to keep track of connection endpoints
 */
struct mca_btl_uct_connection_ep_t {
    /** opal base object */
    opal_object_t super;

    struct mca_btl_uct_tl_t *tl;

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

    /** RDMA completions */
    opal_free_list_t rdma_completions;

    /** complete fragments and rdma operations. this fifo is used to avoid making
     * callbacks while holding the device lock. */
    opal_fifo_t completion_fifo;

    /** progress is enabled on this context */
    bool progress_enabled;

    /** communication AM handler is installed */
    bool am_handler_installed;

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
        uint8_t padding[7];
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
    uct_iov_t uct_iov[3];

    /** how many iov entries are filled */
    int uct_iov_count;

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
    opal_mutex_t ep_lock;

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

    /** memory domain associated with this tl (no reference) */
    mca_btl_uct_md_t *uct_md;

    /** lock protecting tl structures */
    opal_mutex_t tl_lock;

    /** tl configuration (used for creating device contexts) */
    uct_iface_config_t *uct_tl_config;

    /** name of this tl (used for creating device contexts) */
    char *uct_tl_name;

    /** device name for this tl (used for creating device contexts) */
    char *uct_dev_name;

    /** UCT device type from the tl description */
    uct_device_type_t dev_type;

    /** maximum number of device contexts that can be created */
    int max_device_contexts;

    /** array of device contexts */
    mca_btl_uct_device_context_t *uct_dev_contexts[MCA_BTL_UCT_MAX_WORKERS];

    /** tl index. this is used to differentiate (if there is any difference)
     * between rdma and am endpoints */
    int tl_index;

    /** interface attributes */
    uct_iface_attr_t uct_iface_attr;

    /** async context */
    ucs_async_context_t *ucs_async;

    /** pending connection requests */
    opal_fifo_t pending_connection_reqs;
};

typedef struct mca_btl_uct_tl_t mca_btl_uct_tl_t;
OBJ_CLASS_DECLARATION(mca_btl_uct_tl_t);

struct mca_btl_uct_pending_connection_request_t {
    opal_list_item_t super;
    uint8_t request_data[];
};

typedef struct mca_btl_uct_pending_connection_request_t mca_btl_uct_pending_connection_request_t;
OBJ_CLASS_DECLARATION(mca_btl_uct_pending_connection_request_t);

/**
 * @brief parsed include/exclude list
 *
 */
struct mca_btl_uct_include_list_t {
    opal_object_t super;

    /** argv-style (NULL terminated) array of strings */
    char **list;
    /** is an inclusive list (vs exclusive) */
    bool include;
};
typedef struct mca_btl_uct_include_list_t mca_btl_uct_include_list_t;
OBJ_CLASS_DECLARATION(mca_btl_uct_include_list_t);

struct mca_btl_uct_tl_modex_t {
    /** total size of this modex */
    uint16_t size;
    char tl_name[UCT_TL_NAME_MAX];
    uint8_t data[];
} __opal_attribute_packed__;
typedef struct mca_btl_uct_tl_modex_t mca_btl_uct_tl_modex_t;

struct mca_btl_uct_md_modex_t {
    /** total size of this modex */
    uint16_t size;
    uint16_t module_index;
    char md_name[UCT_MD_NAME_MAX];
    uint8_t data[];
} __opal_attribute_packed__;
typedef struct mca_btl_uct_md_modex_t mca_btl_uct_md_modex_t;

#endif /* !defined(BTL_UCT_TYPES_H) */
