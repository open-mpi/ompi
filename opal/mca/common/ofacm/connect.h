/*
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009 Mellanox Technogies, Inc.  All rights reserved.
 *
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * This interface is designed to hide the back-end details of how IB
 * RC connections are made from the rest of the openib BTL.  There are
 * module-like instances of the implemented functionality (dlopen and
 * friends are not used, but all the functionality is accessed through
 * struct's of function pointers, so you can swap between multiple
 * different implementations at run time, just like real components).
 * Hence, these entities are referred to as "Connect
 * Pseudo-Components" (CPCs).
 *
 * The CPCs are referenced by their names (e.g., "oob", "rdma_cm").
 *
 * CPCs are split into components and modules, similar to all other
 * MCA frameworks in this code base.
 *
 * Before diving into the CPC interface, let's discuss some
 * terminology and mappings of data structures:
 *
 * - a BTL module represents a network port (in the case of the openib
 *   BTL, a LID)
 * - a CPC module represents one way to make connections to a BTL module
 * - hence, a BTL module has potentially multiple CPC modules
 *   associated with it
 * - an endpoint represnts a connection between a local BTL module and
 *   a remote BTL module (in the openib BTL, because of BSRQ, an
 *   endpoint can contain multiple QPs)
 * - when an endpoint is created, one of the CPC modules associated
 *   with the local BTL is selected and associated with the endpoint
 *   (obviously, it is a CPC module that is common between the local
 *   and remote BTL modules)
 * - endpoints may be created and destroyed during the MPI job
 * - endpoints are created lazily, during the first communication
 *   between two peers
 * - endpoints are destroyed when two MPI processes become
 *   disconnected (e.g., MPI-2 dynamics or MPI_FINALIZE)
 * - hence, BTL modules and CPC modules outlive endpoints.
 *   Specifically, BTL modules and CPC modules live from MPI_INIT to
 *   MPI_FINALIZE. endpoints come and go as MPI semantics demand it.
 * - therefore, CPC modules need to cache information on endpoints that
 *   are specific to that connection.
 *
 * Component interface:
 *
 * - component_register(): The openib BTL's component_open() function
 * calls the connect_base_register() function, which scans all
 * compiled-in CPC's.  If they have component_register() functions,
 * they are called (component_register() functions are only allowed to
 * register MCA parameters).
 *
 * NOTE: The connect_base_register() function will process the
 * btl_openib_cpc_include and btl_openib_cpc_exclude MCA parameters
 * and automatically include/exclude CPCs as relevant.  If a CPC is
 * excluded, none of its other interface functions will be invoked for
 * the duration of the process.
 *
 * - component_init(): The openib BTL's component_init() function
 * calls connect_base_init(), which will invoke this query function on
 * each CPC to see if it wants to run at all.  CPCs can gracefully
 * remove themselves from consideration in this process by returning
 * OPAL_ERR_NOT_SUPPORTED.
 *
 * - component_query(): The openib BTL's init_one_port() calls the
 * connect_base_select_for_local_port() function, which, for each LID
 * on that port, calls the component_query() function on every
 * available CPC on that LID.  This function is intended to see if a
 * CPC can run on a sepcific openib BTL module (i.e., LID).  If it
 * can, the CPC is supposed to create a CPC module that is specific to
 * that BTL/LID and return it.  If it cannot, it should return
 * OPAL_ERR_NOT_SUPPORTED and be gracefully skipped for this
 * OpenFabrics port.
 *
 * component_finalize(): The openib BTL's component_close() function
 * calls connect_base_finalize(), which, in turn, calls the
 * component_finalize() function on all available CPCs.  Note that all
 * CPC modules will have been finalized by this point; the CPC
 * component_finalize() function is a chance for the CPC to clean up
 * any component-specific resources.
 *
 * Module interface:
 *
 * cbm_component member: A pointer pointing to the single, global
 * instance of the CPC component.  This member is used for creating a
 * unique index representing the modules' component so that it can be
 * shared with remote peer processes.
 *
 * cbm_priority member: An integer between 0 and 100, inclusive,
 * representing the priority of this CPC.
 *
 * cbm_modex_message member: A pointer to a blob buffer that will be
 * included in the modex message for this port for this CPC (it is
 * assumed that this blob is a) only understandable by the
 * corresponding CPC in the peer process, and b) contains specific
 * addressing/contact information for *this* port's CPC module).
 *
 * cbm_modex_message_len member: The length of the cbm_modex_message
 * blob, in bytes.
 *
 * cbm_endpoint_init(): Called during endpoint creation, allowing a
 * CPC module to cache information on the endpoint.  A pointer to the
 * endpoint's CPC module is already cached on the endpoint.
 *
 * cbm_start_connect(): initiate a connection to a remote peer.  The
 * CPC is responsible for setting itself up for asyncronous operation
 * for progressing the outgoing connection request.
 *
 * cbm_endpoint_finalize(): Called during the endpoint destrouction,
 * allowing the CPC module to destroy anything that it cached on the
 * endpoint.
 *
 * cbm_finalize(): shut down all asynchronous handling and clean up
 * any state that was setup for this CPC module/BTL.  Some CPCs setup
 * asynchronous support on a per-HCA/NIC basis (vs. per-port/LID).  It
 * is the reponsibility of the CPC to figure out such issues (e.g.,
 * via reference counting) -- there is no notification from the
 * upper-level BTL about when an entire HCA/NIC is no longer being
 * used.  There is only this function, which tells when a specific
 * CPC/BTL module is no longer being used.
 *
 * cbm_uses_cts: a bool that indicates whether the CPC will use the
 * CTS protocol or not.
 *   - if true: the CPC will post the fragment on
 *     endpoint->endpoint_cts_frag as a receive buffer and will *not*
 *     call opal_btl_openib_post_recvs().
 *   - if false: the CPC will call opal_btl_openib_post_recvs() before
 *     calling opal_btl_openib_cpc_complete().
 *
 * There are two functions in the main openib BTL that the CPC may
 * call:
 *
 * - opal_btl_openib_post_recvs(endpoint): once a QP is locally
 * connected to the remote side (but we don't know if the remote side
 * is connected to us yet), this function is invoked to post buffers
 * on the QP, setup credits for the endpoint, etc.  This function is
 * *only* invoked if the CPC's cbm_uses_cts is false.
 *
 * - opal_btl_openib_cpc_complete(endpoint): once that a CPC knows
 * that a QP is connected on *both* sides, this function is invoked to
 * tell the main openib BTL "ok, you can use this connection now."
 * (e.g., the main openib BTL will either invoke the CTS protocol or
 * start sending out fragments that were queued while the connection
 * was establishing, etc.).
 */
#ifndef OPAL_COMMON_OFACM_CONNECT_H
#define OPAL_COMMON_OFACM_CONNECT_H

/* System includes */
#include <infiniband/verbs.h>

#include "opal/threads/mutex.h"
#include "opal/class/opal_list.h"
#include "opal/util/proc.h"

BEGIN_C_DECLS

#define BCF_MAX_NAME 64

/**
 * Must forward declare these structs to avoid include file loops.
 */

/**
 * This is struct is defined below
 */
struct opal_common_ofacm_base_module_t;

/* special capabilities */
#define OPAL_COMMON_OFACM_XRC_ONLY        1
#define OPAL_COMMON_OFACM_IWARP_ONLY      1 << 1

/**
 * State of OFACM connection.
 */

typedef enum {
    /* Defines the state in which this BTL instance
     * has started the process of connection */
    MCA_COMMON_OFACM_CONNECTING,

    /* Waiting for ack from endpoint */
    MCA_COMMON_OFACM_CONNECT_ACK,

    /*Waiting for final connection ACK from endpoint */
    MCA_COMMON_OFACM_WAITING_ACK,

    /* Connected ... both sender & receiver have
     * buffers associated with this connection */
    MCA_COMMON_OFACM_CONNECTED,

    /* Connection is closed, there are no resources
     * associated with this */
    MCA_COMMON_OFACM_CLOSED,

    /* Maximum number of retries have been used.
     * Report failure on send to upper layer */
    MCA_COMMON_OFACM_FAILED,

    /* We found is useful to have one more
     * state that maybe utilized for user needs */
    MCA_COMMON_OFACM_USER_CUSTOM
} opal_common_ofacm_connection_state_t;

typedef enum {
    MCA_COMMON_OFACM_BTL = 0,
    MCA_COMMON_OFACM_COLL = 100
} opal_common_ofacm_type;

typedef struct opal_common_ofacm_base_dev_desc_t {
    struct ibv_device* ib_dev;          /* device */
    struct ibv_context* ib_dev_context; /* device context */
    int capabilities;                   /* Special capabilities like: XRC, Iwarp, etc.. */
} opal_common_ofacm_base_dev_desc_t;

/* QPs configuration container that should be filled by
 * upper layer, for example - btl */
typedef struct opal_common_ofacm_base_qp_config_t {
    int num_qps;
    int num_srqs;
    struct ibv_qp_init_attr *init_attr;
    struct ibv_qp_attr *attr;
    uint32_t *srq_num;
    uint32_t *init_attr_mask;
    uint32_t *rtr_attr_mask;
    uint32_t *rts_attr_mask;
} opal_common_ofacm_base_qp_config_t;

/* QP base data */
typedef struct opal_common_ofacm_base_qp_t {
    struct ibv_qp *lcl_qp;
    size_t ib_inline_max; /**< max size of IB inline send */
    uint32_t lcl_psn;
    int32_t  sd_wqe;      /**< number of available send wqe entries */
    int users;
    opal_mutex_t lock;
} opal_common_ofacm_base_qp_t;

/* Remote QP info */
typedef struct opal_common_ofacm_base_rem_qp_info_t {
    uint32_t                    rem_qp_num;
    /* Remote QP number */
    uint32_t                    rem_psn;
    /* Remote processes port sequence number */
} opal_common_ofacm_base_rem_qp_info_t;

/* Remote SRQ info */
typedef struct opal_common_ofacm_base_rem_srq_info_t {
    /* Remote SRQ number */
    uint32_t                    rem_srq_num;
} opal_common_ofacm_base_rem_srq_info_t;

/* Remote connection context */
typedef struct opal_common_ofacm_base_remote_connection_context_t {
    opal_object_t               super;
    /* Local identifier of the remote process */
    uint16_t                    rem_lid;
    /* subnet id of remote process */
    uint64_t                    rem_subnet_id;
    /* MTU of remote process */
    uint32_t                    rem_mtu; /* TBD: not sure that i need this one */
    /* index of remote endpoint in endpoint array */
    uint32_t                    rem_index; /* TBD: the index we use as immidiate data */
    /* Remote QPs */
    opal_common_ofacm_base_rem_qp_info_t *rem_qps;
    /* Remote xrc_srq info, used only with XRC connections */
    opal_common_ofacm_base_rem_srq_info_t *rem_srqs;
} opal_common_ofacm_base_remote_connection_context_t;

typedef struct opal_common_ofacm_base_proc_t {
    opal_list_item_t super;
    opal_proc_t *proc_opal; /* target proc */
    opal_list_t all_contexts; /* list of all contexts connected to
                                 this endpoint*/
} opal_common_ofacm_base_proc_t;
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_common_ofacm_base_proc_t);

/* Connection call back function that is called on connection setup */
typedef void (*opal_common_ofacm_base_context_connect_cb_fn_t)(void *);

/* Connection call back function that is called on context error */
typedef void (*opal_common_ofacm_base_context_error_cb_fn_t)(void *);
/* Prepare recive call back function that is when recv side should be prepared,
 * for example recv packet prepost */
typedef int (*opal_common_ofacm_base_context_prepare_recv_cb_fn_t)(void *);

/* Basic connection context
 * ========================
 * The initial connection contxet is created during endpoint initialazation call.
 * Each CPC will return opal_common_ofacm_base_local_connection_context_t that
 * is based on CPC connection context.
 *
 * As Input for context creation user must provide:
 * ================================================
 * number of QPs
 * qp init atributes
 * qp standart attribute
 * pointer to protection domain
 * pointer to user context (for example pointer to endpoint in case of btl)
 */
typedef struct opal_common_ofacm_base_local_connection_context_t {
    opal_list_item_t super;
    struct opal_common_ofacm_base_proc_t *proc;         /* target proc */
    struct opal_common_ofacm_base_module_t *cpc;        /* Pointer to context cpc */
    opal_common_ofacm_connection_state_t state;         /* Connection context status */
    uint64_t subnet_id;                                 /* caching subnet_id */
    int cpc_type;                                       /* connection manager family: openib, coll, etc..*/
    uint16_t lid;                                       /* caching lid */
    uint16_t rem_lid;                                   /* remote lid */
    uint8_t num_of_qps;                                 /* Number of qps that we want to open */
    struct opal_common_ofacm_base_qp_t *qps;            /* qps data */
    uint8_t num_of_srqs;                                /* Number of qps that we want to open */
    uint32_t *srq_num;                                  /* srq numbers for recv on this context */
    struct ibv_qp_init_attr *init_attr;                 /* list of initial attr for each qp   */
    struct ibv_qp_attr *attr;                           /* qp attributes                      */
    struct ibv_pd* ib_pd;                               /* protection domain                  */
    uint32_t *custom_init_attr_mask;                    /* in additional to standard attr_mask we want allow to user
                                                           specify special custom masks for init */
    uint32_t *custom_rtr_attr_mask;                     /* in additional to standard attr_mask we want allow to user
                                                          specify special custom masks for rtr */
    uint32_t *custom_rts_attr_mask;                     /* in additional to standard attr_mask we want allow to user
                                                          specify special custom masks for rts */
    void *user_context;                                 /* back pointer to endpoint */
    opal_common_ofacm_base_context_connect_cb_fn_t connect_cb; /* Connection callback function */
    opal_common_ofacm_base_context_error_cb_fn_t error_cb; /* Error callback function */
    opal_common_ofacm_base_context_prepare_recv_cb_fn_t prepare_recv_cb; /* Prepare recv side
                                                                            (prepost) callback function */
    /* TBD: Need to check when we can update the index. I think during endpoint creation we do not
     * have it. It mean that BTL should some how to update it later ...*/
    int32_t index;                                      /* user context index */
    bool initiator;                                     /* initiator of connection ? */
    opal_common_ofacm_base_remote_connection_context_t remote_info; /* data about remote side of this
                                                                       connection*/
    uint32_t xrc_recv_qp_num ;                          /* in xrc we will use it as recv qp */

    opal_mutex_t context_lock;                          /* protection */
} opal_common_ofacm_base_local_connection_context_t;
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_common_ofacm_base_local_connection_context_t);
/* Constructor and destructor are located in common_ofacm_base.c */

/************************************************************************/

/**
 * Function to register MCA params in the connect functions.  It
 * returns no value, so it cannot fail.
 */
typedef void (*opal_common_ofacm_base_component_register_fn_t)(void);

/**
 * This function is invoked once by the openib BTL component during
 * startup.  It is intended to have CPC component-wide startup.
 *
 * Return value:
 *
 * - OPAL_SUCCESS: this CPC component will be used in selection during
 *   this process.
 *
 * - OPAL_ERR_NOT_SUPPORTED: this CPC component will be silently
 *   ignored in this process.
 *
 * - Other OPAL_ERR_* values: the error will be propagated upwards,
 *   likely causing a fatal error (and/or the openib BTL component
 *   being ignored).
 */
typedef int (*opal_common_ofacm_base_component_init_fn_t)(void);

/**
 * Query the CPC to see if it wants to run on a specific port (i.e., a
 * specific BTL module).  If the component init function previously
 * returned OPAL_SUCCESS, this function is invoked once per BTL module
 * creation (i.e., for each port found by an MPI process).  If this
 * CPC wants to be used on this BTL module, it returns a CPC module
 * that is specific to this BTL module.
 *
 * The BTL module in question is passed to the function; all of its
 * attributes can be used to query to see if it's eligible for this
 * CPC.
 *
 * If it is eligible, the CPC is responsible for creating a
 * corresponding CPC module, filling in all the relevant fields on the
 * modules, and for setting itself up to run (per above) and returning
 * a CPC module (this is effectively the "module_init" function).
 * Note that the module priority must be between 0 and 100
 * (inclusive).  When multiple CPCs are eligible for a single module,
 * the CPC with the highest priority will be used.
 *
 * Return value:
 *
 * - OPAL_SUCCESS if this CPC is eligible for and was able to be setup
 * for this BTL module.  It is assumed that the CPC is now completely
 * setup to run on this openib module (per description above).
 *
 * - OPAL_ERR_NOT_SUPPORTED if this CPC cannot support this BTL
 * module.  This is not an error; it's just the CPC saying "sorry, I
 * cannot support this BTL module."
 *
 * - Other OPAL_ERR_* code: an error occurred.
 */
typedef int (*opal_common_ofacm_base_func_component_query_t)
    (struct opal_common_ofacm_base_dev_desc_t *dev,
     struct opal_common_ofacm_base_module_t **cpc);

/**
 * This function is invoked once by the openib BTL component during
 * shutdown.  It is intended to have CPC component-wide shutdown.
 */
typedef int (*opal_common_ofacm_base_component_finalize_fn_t)(void);

/**
 * CPC component struct
 */
typedef struct opal_common_ofacm_base_component_t {
    /** Name of this set of connection functions */
    char cbc_name[BCF_MAX_NAME];

    /** Register function.  Can be NULL. */
    opal_common_ofacm_base_component_register_fn_t cbc_register;

    /** CPC component init function.  Can be NULL. */
    opal_common_ofacm_base_component_init_fn_t cbc_init;

    /** Query the CPC component to get a CPC module corresponding to
        an openib BTL module.  Cannot be NULL. */
    opal_common_ofacm_base_func_component_query_t cbc_query;

    /** CPC component finalize function.  Can be NULL. */
    opal_common_ofacm_base_component_finalize_fn_t cbc_finalize;
    /** All connection contexts that are using this CPC **/
    opal_list_t all_procs;
} opal_common_ofacm_base_component_t;

/************************************************************************/

/**
 * Function called when an endpoint has been created and has been
 * associated with a CPC.
 */
typedef opal_common_ofacm_base_local_connection_context_t*
    (*opal_common_ofacm_base_module_endpoint_init_fn_t)
    (opal_proc_t *proc,
     opal_common_ofacm_base_qp_config_t *qp_config, struct ibv_pd *pd,
     uint64_t subnet_id, int cpc_type, uint16_t lid, uint16_t rem_lid,
     int32_t user_context_index, void *user_context,
     struct opal_common_ofacm_base_module_t *cpc,
     opal_common_ofacm_base_context_connect_cb_fn_t connect_cb,
     opal_common_ofacm_base_context_error_cb_fn_t error_cb,
     opal_common_ofacm_base_context_prepare_recv_cb_fn_t prepare_recv_cb);

/**
 * Function to initiate a connection to a remote process.
 */
typedef int (*opal_common_ofacm_base_module_start_connect_fn_t)
    (struct opal_common_ofacm_base_local_connection_context_t *context);

/**
 * Function called when an endpoint is being destroyed.
 */
typedef int (*opal_common_ofacm_base_module_endpoint_finalize_fn_t)
     (struct opal_common_ofacm_base_local_connection_context_t *context);

/**
 * Function to finalize the CPC module.  It is called once when the
 * CPC module's corresponding openib BTL module is being finalized.
 */
typedef int (*opal_common_ofacm_base_module_finalize_fn_t)(void);

/**
 * Error callback that is called by cpc module on error.
 * The callback should be set on upper layer (for example BTL)
 */
typedef int (*opal_common_ofacm_base_module_error_cb_fn_t)(void *);
/**
 * Meta data about a CPC module.  This is in a standalone struct
 * because it is used in both the CPC module struct and the
 * openib_btl_proc_t struct to hold information received from the
 * modex.
 */
typedef struct opal_common_ofacm_base_module_data_t {
    /** Pointer back to the component.  Used by the base and openib
        btl to calculate this module's index for the modex. */
    opal_common_ofacm_base_component_t *cbm_component;

    /** Priority of the CPC module (must be >=0 and <=100) */
    uint8_t cbm_priority;

    /** Blob that the CPC wants to include in the openib modex message
        for a specific port, or NULL if the CPC does not want to
        include a message in the modex.  */
    void *cbm_modex_message;

    /** Length of the cbm_modex_message blob (0 if
        cbm_modex_message==NULL).  The message is intended to be short
        (because the size of the modex broadcast is a function of
        sum(cbm_modex_message_len[i]) for
        i=(0...total_num_ports_in_MPI_job) -- e.g., IBCM imposes its
        own [very short] limits (per IBTA volume 1, chapter 12). */
    uint8_t cbm_modex_message_len;
} opal_common_ofacm_base_module_data_t;

/**
 * Struct for holding CPC module and associated meta data
 */
typedef struct opal_common_ofacm_base_module_t {
    /** Meta data about the module */
    opal_common_ofacm_base_module_data_t data;

    /** Endpoint initialization function */
    opal_common_ofacm_base_module_endpoint_init_fn_t cbm_endpoint_init;

    /** Connect function */
    opal_common_ofacm_base_module_start_connect_fn_t cbm_start_connect;

    /** Endpoint finalization function */
    opal_common_ofacm_base_module_endpoint_finalize_fn_t cbm_endpoint_finalize;

    /** Finalize the cpc module */
    opal_common_ofacm_base_module_finalize_fn_t cbm_finalize;

    /** Whether this module will use the CTS protocol or not.  This
        directly states whether this module will call
        mca_btl_openib_endpoint_post_recvs() or not: true = this
        module will *not* call _post_recvs() and instead will post the
        receive buffer provided at endpoint->endpoint_cts_frag on qp
        0. */
    bool cbm_uses_cts;
} opal_common_ofacm_base_module_t;

END_C_DECLS

#endif
