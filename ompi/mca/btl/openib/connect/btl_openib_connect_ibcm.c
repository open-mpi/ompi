/*
 * Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2008-2009 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/*
 * TO-DO:
 *
 * - audit control values passed to req_send()
 * - More show_help() throughout 
 * - error handling in case of broken connection is not good; need to
 *   notify btl module safely
 */

/*
 * For a description of how IBCM works, see chapter 12 of the vol 1 of
 * the IBTA doc.  The general connection scheme described in chapter
 * is a UD-based state-transition protcol that utilizes on a 3 way
 * handshake:
 *
 * 1. active side creates local side of the QP and sends connect
 *    request to remote peer.
 * 2. passive side receives the request, makes its local side of QP,
 *    posts receive buffers, transitions the QP to RTR, and sends back
 *    a reply.
 * 3. active side gets reply, transitions its QP to both RTR and RTS,
 *    and sends back a "ready to use" (RTU) message.
 * 4. passive side gets the RTU, transitions its QP to RTS.
 *
 * It looks like this (time flowing in the down direction):
 *
 *         active     passive
 *              |     |
 *      request |---->|
 *              |     |
 *              |<----| reply
 *              |     |
 *          RTU |---->|
 *              |     |
 *
 * The fact that it's based on UD allows the IBCM service to sit on a
 * single, well-advertised location that any remote peer can contact
 * (don't need to setup a per-peer RC QP to talk to it).  But the
 * drawback is that UD fragments can be lost, reordered, etc.
 *
 * The OMPI openib BTL has a few additional requirements:
 *
 * - both peers must have IBCM available and on the same subnet
 * - because of BSRQ, each MPI-level connection to another peer
 *   actually consists of N QPs (where N>=1)
 * - MPI might initiate connections from both directions
 *   "simultaneously"; need to unambigously resolve which one "wins"
 *   in a distributed fashion
 * - MPI may have more than one process per node
 *
 * Note that the IBCM request/reply/RTU messages allow for "private"
 * data to be included, so we can include pointers to relevant data
 * structures to make matching simpler.  The request message includes
 * a pointer that is relevant in the active process context that is
 * echoed back in the reply message.  Similarly, the reply message
 * includes a pointer that is relevant in the passive process context
 * that is echoed back in the RTU message.  This allows active side
 * receipt of the reply and the passive side receipt of the RTU to
 * find their matches easily.  The phase that has to do the most
 * amount of work is the passive side when receiving an incoming
 * connection request.  It has to do a lot of work to find a matching
 * endpoint with wich to accept the request.
 *
 * The IBCM software in OFED has some points that are worth bearing,
 * as they influenced the design and implementation of this CPC:
 *
 * - IBCM is actually quite intelligent about timeouts and
 *   retransmissions; it will try to retransmit each request/reply/RTU
 *   several times before reporting the failure up to the ULP (i.e.,
 *   this CPC).
 * - IBCM also nicely detects most obvious duplicates.  For example,
 *   say the active side sends a request, and the passive side sends a
 *   reply.  But the reply gets lost, so the active side sends the
 *   request again.  The passive side will recognize it as a duplicate
 *   because the corresponding CM ID will be in a state indicating
 *   that it sent the reply
 * - One point that was not clear about IBCM to me when I read cm.h is
 *   that there is a function called ib_cm_notify() that is used to
 *   tell IBCM (among other things) when the first message arrives on
 *   a QP when the RTU has not yet been received.  This can happen, of
 *   course, since IBCM traffic is UD.
 * - Also, note that IBCM "listener" IDs are per DEVICE, not per port.  
 * - CM ID's are persistent throughout the life of a QP.  If you
 *   destroy a CM ID (ib_cm_destroy_id), the IBCM system will tear
 *   down the connection.  So the CM ID you get when receiving a
 *   request message is the same one you'll have all throughout the
 *   life of the QP that you are creating.  Likewise, the CM ID you
 *   explicitly create when *sending* a connect request will exist for
 *   the entire lifetime of the QP that is being created.
 * - In short CM IDs are mapped 1:1 to QPs.
 * - There is a formal disconnect protocol in IBCM that contains a
 *   multi-part handshake.  When you destroy a CM ID
 *   (ib_cm_destroy_id), the disconnect protocol is enacted in the
 *   IBCM kernel (similar to invoking "close()" on a TCP socket -- the
 *   kernel handles the multi-way handshake).
 *
 * With that background, here's how we use the IBCM in the openib BTL
 * IBCM CPC.  Note that we actually have to add a *4th* leg in the
 * handshake (beyond the request, reply, RTU) :-( -- more details
 * below.
 *
 * openib BTL modules have one or more CPC modules.  Each CPC module
 * comes from a different CPC component.  One CPC module is bound to
 * exactly one openib BTL module.  The CPC base glue will find a pair
 * of matching CPC modules and choose them to make the QP(s).  One or
 * more endpoints will be bound to a BTL module, depending on the LMC.
 * There is only one CPC module per BTL module, so multiple endpoints
 * may share a CPC module.  However, QPs are made on a per-endpoint
 * basis, so the CPC caches info on the endpoint, and destroys this
 * info when the endpoint is destroyed (e.g., we destroy the CM IDs in
 * the endpoint CPC destructor -- this fires the IBCM disconnect
 * protocol, but it's handled entirely within the IBCM kernel; we
 * don't have to worry about single threaded progress or deadlock
 * because all progress can happen asynchronously within the kernel).
 *
 * The IBCM CPC adds LID and GUID information into the MPI modex.  So
 * every process does not need to do any lookup on how to use the IBCM
 * system to make a connection to its peer; it can use the LID and GID
 * information to create a path record (vs. querying the SM to get a
 * patch record for that peer) and use that to make the IBCM
 * connection.
 *
 * The IBCM listener service ID is the PID of the MPI process.  This
 * is how we distinguish between multiple MPI processes listening via
 * IBCM on the same host.
 *
 * Since IBCM listners are per DEVICE (vs. per port), we maintain a list
 * of existing listeners.  When a new query comes in, we check the
 * list to see if the DEVICE connected to this BTL module already has a
 * listener.  If it does, we OBJ_RETAIN it and move on.  Otherwise, we
 * create a new one.  When CPC modules are destroyed, we simply
 * OBJ_RELEASE the listener object; the destructor takes care of all
 * the actual cleanup.
 *
 * Note that DEVICEs are capable of having multiple GIDs.  OMPI defaults
 * to using the 0th GID, but the MCA param
 * btl_openib_connect_ibcm_gid_index allows the user to choose a
 * different one.
 *
 * This CPC uses the openib FD monitoring service to listen for
 * incoming IBCM activity on the listener CM IDs.  Activity on these
 * FDs will trigger a callback which launches the dispatcher to handle
 * the incoming event.  There's efforts to make this activity thread
 * safe -- in the case where the IBCM "listener" is in the FD service,
 * which is blocking in its own separate thread.  In recognition of
 * this, the callbacks from the from the FD service is careful about
 * accessing data that the main thread/upper-level openib BTL needs to
 * access, and re-invoking callbacks through the FD service so that
 * the execute in the main thread (vs. running in the FD service
 * thread).  It dual compiles so that when OMPI is compiled in a
 * single-threaded mode, the FD service simply uses the main libevent
 * fd progress engine.  This single-threaded mode has been tested well
 * and appears to work nicely.  The multi-threaded mode has only been
 * lightly tested; we were hampered by other thread safety bugs and
 * couldn't fully test this mode.
 *
 * To avoid some race conditions and complex code for when two MPI
 * process peers initiate connections "simultaneously", we only allow
 * IBCM connections to be made in one direction.  That is, a QP will
 * only be created if the IBCM request flows from a process with the
 * lower GUID to the process with the higher GUID (if two processes
 * are on the same host / have the same GUID, their PIDs are compared
 * instead).  However, since OMPI can operate in a single-threaded
 * mode, if the MPI sender is the "wrong" one (e.g., it has a higher
 * GUID), then it can starve waiting for the other MPI peer to
 * initiate a send.  So we use an alternate scheme in this scenario:
 *
 * 1. The "wrong" process will send a single IBCM connection request
 *    to its peer on a bogus QP that was created just for this
 *    request.  
 * 2. The receiver will get the request, detect that it came
 *    in from the "wrong" direction, and reject it (IBCM has an
 *    explicit provision for rejecting incoming connections).
 * 3. The receiver will then turn around and re-initiate the
 *    connection in the "right" direction.
 * 4. The originator will receive the reject message and destroy the
 *    bogus QP.
 * 5. The originator will then receive the new request message(s) from
 *    the receiver and the connection will proceed in the "right"
 *    direction.
 *
 * In this way, the initial connection request's only purpose is to
 * wake up the receiver so that it can re-initiate the connection in
 * the "right" direction.  The reason for this is to be able to easily
 * detect and handle "simultaneous" connections from both directions.
 * If processes A and B both initiate connections simultaneously, when
 * A receives B's request, it can see that a) it's coming from the
 * "wrong" direction and b) that there's already a connection in
 * progress in the "right" direction.  Regardless, B will eventually
 * get the connection request from the "right" direction and all
 * proceeds as expected.
 *
 * Without this kind of protocol, there would have had to have been
 * complicated buffers / credit negotiation with the upper-level
 * openib BTL (e.g., make some QP's, post some receives, then
 * potentially move those buffers to a different QP, ...etc.).  Yuck.
 *
 * Note that since an endpoint represents 1 or more QPs, we do a few
 * actions "in blocks" because it's cheaper / easier to do a bunch of
 * things at once rather than for each callback for that QP.  For
 * example:
 *
 * - create all CM IDs and send connection requests (in the right
 *   direction only) for mca_btl_openib_component.num_qps.
 * - upon receipt of the first connection request for an endpoint, if
 *   the QPs have not already been created (they will have been
 *   created if we're initiating in the "wrong" direction -- because
 *   start_connect() will have created them already), create all
 *   num_qps QPs.  Also post receive buffers on all QPs.
 *
 * We wholly reply on the IBCM system for all retransmissions and
 * duplicate filtering of IBCM requests, replies, and RTUs.  If IBCM
 * reports a timeout error up to OMPI, we abort the connection.  Lists
 * are maintained of pending IBCM requests and replies solely for
 * error handling; request/reply timeouts are reported via CM ID.  We
 * can cross-reference this CM ID to the endpoint that it was trying
 * to connect via these lists.
 *
 * Note that there is a race condition: because UD is unordered, the
 * first message may arrive on the QP before the RTU has arrived.
 * This will cause an IBV_EVENT_COMM_EST event to be raised, which
 * would then be picked up by the async event handler in the
 * upper-level openib BTL, which would be Bad.  :-(
 *
 * To fix this race, we have to do the following:
 *
 * - Have the active side send the RTU.  This is necessary to
 *   transition the IBCM system on the active side to the "connected"
 *   state.
 * - Have the active side IBCM CPC send a 0 byte message on the new
 *   QP.  Since the QP is RC, it's guaranteed to get there (or die
 *   trying).  So we don't have to play all the UD games.
 * - If the RTU is received first on the passive side, do the normal
 *   RTU processing.
 * - If the 0 byte message is received first on the passive side, call
 *   ib_cm_notify() with the COMM_EST event, which will also do the
 *   normal RTU processing.  If the RTU is received later, the IBCM
 *   system on the passive side will know that it's effectively a
 *   duplicate, and therefore can be ignored.
 */

#include "ompi_config.h"

#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#include <infiniband/cm.h>

#include "opal/util/output.h"
#include "opal/util/error.h"
#include "opal/mca/event/event.h"
#include "opal_stdint.h"

#include "orte/util/show_help.h"

#include "btl_openib_endpoint.h"
#include "btl_openib_proc.h"
#include "btl_openib_fd.h"
#include "connect/connect.h"

/* JMS to be removed: see #1264 */
#undef event

/*--------------------------------------------------------------------*/

/*
 * Message that this CPC includes in the modex.  Filed are laid out in
 * order to avoid holes.
 */
typedef struct {
    /** The GUID of the port; used to locate the source endpoint when
        an IB CM request arrives */
    uint64_t mm_port_guid;
    /** The service ID that we're listening on */
    uint64_t mm_service_id;
    /** The LID that we're sitting on; it also identifies the source
        endpoint when an IB CM request arrives */
    uint16_t mm_lid;
    /** The port number of this port, also used to locate the source
        endpoint when an IB CM request arrives */
    uint8_t mm_port_num;
} modex_msg_t;

/*
 * Forward reference to a struct defined below
 */
struct ibcm_request_t;

/*
 * Message (private data) that is sent with the IB CM connect request
 */
typedef struct {
    struct ibcm_request_t *ireqd_request;
    uint64_t ireqd_pid;
    uint32_t ireqd_ep_index;
    uint8_t ireqd_qp_index;
} ibcm_req_data_t;

/*
 * Forward reference to a struct defined below
 */
struct ibcm_reply_t;

/*
 * Message (private data) that is sent with the IB CM reply
 */
typedef struct {
    struct ibcm_request_t *irepd_request;
    struct ibcm_reply_t *irepd_reply;
    uint32_t irepd_ep_index;
    uint8_t irepd_qp_index;
} ibcm_rep_data_t;

/*
 * Message (private data) that is sent with the IB CM RTU
 */
typedef struct {
    struct ibcm_reply_t *irtud_reply;
    uint8_t irtud_qp_index;
} ibcm_rtu_data_t;

/*
 * Specific reasons for connection rejection
 */
typedef enum {
    REJ_ALREADY_CONNECTED,
    REJ_QP_ALREADY_CONNECTED,
    REJ_WRONG_DIRECTION,
    REJ_PEER_NOT_FOUND,
    REJ_PASSIVE_SIDE_ERROR,
    REJ_MAX
} ibcm_reject_reason_t;

/*
 * Need to maintain a list of IB CM listening handles since they are
 * per *DEVICE*, and openib BTL modules are per *LID*.
 */
typedef struct {
    opal_list_item_t super;

    /* IB device context that was used to create this IB CM context */
    struct ibv_context *ib_context;
    /* The actual CM device handle */
    struct ib_cm_device *cm_device;
    /* The listening handle */
    struct ib_cm_id *listen_cm_id;
    /* Connection identifier parametres */
    struct ib_cm_attr_param param;

    /* List of ibcm_module_t's that use this handle */
    opal_list_t ibcm_modules;
} ibcm_listen_cm_id_t;

static void ibcm_listen_cm_id_constructor(ibcm_listen_cm_id_t *h);
static void ibcm_listen_cm_id_destructor(ibcm_listen_cm_id_t *h);
static OBJ_CLASS_INSTANCE(ibcm_listen_cm_id_t, opal_list_item_t, 
                          ibcm_listen_cm_id_constructor,
                          ibcm_listen_cm_id_destructor);

/*
 * Generic base type for holding an ib_cm_id.  Used for base classes
 * of requests, replies, and RTUs.
 */
typedef struct {
    opal_list_item_t super;

    /* The active handle, representing an active CM ID */
    struct ib_cm_id *cm_id;
} ibcm_base_cm_id_t;

static OBJ_CLASS_INSTANCE(ibcm_base_cm_id_t, opal_list_item_t, NULL, NULL);

/*
 * Need to maintain a list of pending CM ID requests (for error
 * handling if the requests timeout).  Need to use the struct name
 * here because it was forward referenced, above.
 */
typedef struct ibcm_request_t {
    ibcm_base_cm_id_t super;

    /* Request */
    struct ib_cm_req_param cm_req;

    /* Path record */
    struct ibv_sa_path_rec path_rec;

    /* Private data sent with the request */
    ibcm_req_data_t private_data;

    /* Endpoint for this request */
    mca_btl_openib_endpoint_t *endpoint;
} ibcm_request_t;

static void ibcm_request_cm_id_constructor(ibcm_request_t *h);
static OBJ_CLASS_INSTANCE(ibcm_request_t, ibcm_base_cm_id_t,
                          ibcm_request_cm_id_constructor, NULL);

/*
 * Need to maintain a list of pending CM ID replies (for error
 * handling if the replies timeout).  Need to use a struct name here
 * because it was forward referenced, above.
 */
typedef struct ibcm_reply_t {
    ibcm_base_cm_id_t super;

    /* Reply */
    struct ib_cm_rep_param cm_rep;

    /* Private data sent with the reply */
    ibcm_rep_data_t private_data;

    /* Endpoint for this reply */
    mca_btl_openib_endpoint_t *endpoint;
} ibcm_reply_t;

static void ibcm_reply_cm_id_constructor(ibcm_reply_t *h);
static OBJ_CLASS_INSTANCE(ibcm_reply_t, ibcm_base_cm_id_t,
                          ibcm_reply_cm_id_constructor, NULL);

/*
 * The IBCM module (i.e., the base module plus more meta data required
 * by this CPC)
 */
typedef struct {
    ompi_btl_openib_connect_base_module_t cpc;

    /* IB CM listen CM ID */
    ibcm_listen_cm_id_t *cmh;

    /* The associated BTL */
    struct mca_btl_openib_module_t *btl;
} ibcm_module_t;

/*
 * List item container for ibcm_module_t
 */
typedef struct {
    opal_list_item_t super;
    ibcm_module_t *ibcm_module;
} ibcm_module_list_item_t;

static OBJ_CLASS_INSTANCE(ibcm_module_list_item_t, opal_list_item_t, 
                          NULL, NULL);

/*
 * Flags for per-endpoint IBCM data
 */
enum {
    CFLAGS_ONGOING = 1,
    CFLAGS_COMPLETED = 2
};

/*
 * Per-endpoint IBCM data
 */
typedef struct {
    /* Pointer to the base */
    ompi_btl_openib_connect_base_module_t *ie_cpc;
    /* Back pointer to the endpoint */
    struct mca_btl_base_endpoint_t *ie_endpoint;

    /* Array of active CM ID's */
    ibcm_base_cm_id_t **ie_cm_id_cache;
    /* Length of ie_request_cm_cache array */
    int ie_cm_id_cache_size;

    /* Used for sending a CM request that we know will be rejected
       (i.e., if inititing in the "wrong" direction) */
    struct ibv_qp *ie_bogus_qp;

    /* Whether we've created all the qp's or not */
    bool ie_qps_created;
    /* Whether all the receive buffers have been posted to the qp's or
       not */
    bool ie_recv_buffers_posted;
    /* IPC between threads in the ibcm CPC */
    volatile uint32_t ie_connection_flags;
    /* How many qp's are left to connect */
    int ie_qps_to_connect;
    /* Lock for IPC between threads in the ibcm CPC */
    opal_mutex_t ie_lock;
} ibcm_endpoint_t;

/*
 * Info passed to start_connect() when it's invoked via an incoming
 * IBCM request
 */
typedef struct {
    ompi_btl_openib_connect_base_module_t *cscd_cpc;
    struct mca_btl_base_endpoint_t *cscd_endpoint;
} callback_start_connect_data_t;

/*--------------------------------------------------------------------*/

static void ibcm_component_register(void);
static int ibcm_component_query(mca_btl_openib_module_t *btl, 
                                ompi_btl_openib_connect_base_module_t **cpc);
static int ibcm_component_finalize(void);

static int ibcm_endpoint_init(struct mca_btl_base_endpoint_t *endpoint);
static int ibcm_module_start_connect(ompi_btl_openib_connect_base_module_t *cpc,
                                     mca_btl_base_endpoint_t *endpoint);
static int ibcm_endpoint_finalize(struct mca_btl_base_endpoint_t *endpoint);
static int ibcm_module_finalize(mca_btl_openib_module_t *btl,
                                ompi_btl_openib_connect_base_module_t *cpc);

static void *ibcm_event_dispatch(int fd, int flags, void *context);

/*--------------------------------------------------------------------*/

static bool initialized = false;
static int ibcm_priority = 40;
static int ibcm_gid_table_index = 0;
static opal_list_t ibcm_cm_listeners;
static opal_list_t ibcm_pending_requests;
static opal_list_t ibcm_pending_replies;

/*******************************************************************
 * Component
 *******************************************************************/

ompi_btl_openib_connect_base_component_t ompi_btl_openib_connect_ibcm = {
    "ibcm",
    ibcm_component_register,
    NULL,
    ibcm_component_query,
    ibcm_component_finalize
};

/*--------------------------------------------------------------------*/

#define ENABLE_TIMERS (OPAL_ENABLE_DEBUG && 0)

#if ENABLE_TIMERS 
#include MCA_timer_IMPLEMENTATION_HEADER

enum {
    QUERY,
    START_CONNECT,
    QP_TO_RTR, 
    QP_TO_RTS,
    REQUEST_RECEIVED,
    REPLY_RECEIVED,
    RTU_RECEIVED,
    REJECT_RECEIVED,
    CM_GET_EVENT,
    CM_ACK_EVENT,
    MAXTIMER
};
static double timer_tmp[MAXTIMER];
static double timers[MAXTIMER];

static void print_timers(void)
{
    opal_output(0, "ibcm timer[QUERY] = %g", timers[QUERY]);
    opal_output(0, "ibcm timer[START_CONNECT] = %g", timers[START_CONNECT]);
    opal_output(0, "ibcm timer[QP_TO_RTR] = %g", timers[QP_TO_RTR]);
    opal_output(0, "ibcm timer[QP_TO_RTS] = %g", timers[QP_TO_RTS]);
    opal_output(0, "ibcm timer[REQUEST_RECEIVED] = %g", timers[REQUEST_RECEIVED]);
    opal_output(0, "ibcm timer[REPLY_RECEIVED] = %g", timers[REPLY_RECEIVED]);
    opal_output(0, "ibcm timer[RTU_RECEIVED] = %g", timers[RTU_RECEIVED]);
    opal_output(0, "ibcm timer[REJECT_RECEIVED] = %g", timers[REJECT_RECEIVED]);
    opal_output(0, "ibcm timer[CM_GET_EVENT] = %g", timers[CM_GET_EVENT]);
    opal_output(0, "ibcm timer[CM_ACK_EVENT] = %g", timers[CM_ACK_EVENT]);
}

static inline double gettime(void) __opal_attribute_always_inline__;
static inline double gettime(void)
{
    double wtime;
#if OPAL_TIMER_USEC_NATIVE
    wtime = ((double) opal_timer_base_get_usec()) / 1000000.0;
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_usec / 1000000.0;
#endif
    return wtime;
}

#define TIMER_START(x) timer_tmp[(x)] = gettime();
#define TIMER_STOP(x) timers[(x)] = (gettime() - timer_tmp[(x)]);
#else
#define TIMER_START(x)
#define TIMER_STOP(x)
#endif

/*--------------------------------------------------------------------*/

static void ibcm_component_register(void)
{
    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                           "connect_ibcm_priority",
                           "The selection method priority for ibcm",
                           false, false, ibcm_priority, &ibcm_priority);
    if (ibcm_priority > 100) {
        ibcm_priority = 100;
    } else if (ibcm_priority < -1) {
        ibcm_priority = 0;
    }

    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                           "connect_ibcm_gid_index",
                           "GID table index to use to obtain each port's GUID",
                           false, false, ibcm_gid_table_index, 
                           &ibcm_gid_table_index);
    if (ibcm_gid_table_index < 0) {
        ibcm_gid_table_index = 0;
    }
}

/*--------------------------------------------------------------------*/

/* The IB_CM_ASSIGN_SERVICE_ID value passed to ib_cm_listen function asks, 
 * from IBCM , to assign service_id.
 * The value was taken from IBCM kernel level 
 */
#ifndef IB_CM_ASSIGN_SERVICE_ID
#define IB_CM_ASSIGN_SERVICE_ID hton64(0x0200000000000000ULL)
#endif

static int ibcm_component_query(mca_btl_openib_module_t *btl, 
                                ompi_btl_openib_connect_base_module_t **cpc)
{
    int rc;
    modex_msg_t *msg;
    ibcm_module_t *m = NULL;
    opal_list_item_t *item;
    ibcm_listen_cm_id_t *cmh;
    ibcm_module_list_item_t *imli;
    union ibv_gid gid;

    if (!mca_btl_openib_component.cpc_explicitly_defined) {
        BTL_VERBOSE(("ibcm CPC is experimental feature and it is disabled by default"));
        rc = OMPI_ERR_NOT_SUPPORTED;
        goto error;
    }

    /* If we do not have struct ibv_device.transport_device, then
       we're in an old version of OFED that is IB only (i.e., no
       iWarp), so we can safely assume that we can use this CPC. */
#if defined(HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE)
    if (BTL_OPENIB_CONNECT_BASE_CHECK_IF_NOT_IB(btl)) {
        BTL_VERBOSE(("ibcm CPC only supported on InfiniBand; skipped on %s:%d",
                     ibv_get_device_name(btl->device->ib_dev),
                     openib_btl->port_num));
        rc = OMPI_ERR_NOT_SUPPORTED;
        goto error;
    }
#endif

    /* IBCM is not supported if we have any XRC QPs */
    if (mca_btl_openib_component.num_xrc_qps > 0) {
        BTL_VERBOSE(("ibcm CPC not supported with XRC receive queues, please try xoob CPC; skipped on %s:%d",
                     ibv_get_device_name(btl->device->ib_dev),
                     openib_btl->port_num));
        rc = OMPI_ERR_NOT_SUPPORTED;
        goto error;
    }

    /* Do some setup only once -- the first time this query function
       is invoked */
    if (!initialized) {
        OBJ_CONSTRUCT(&ibcm_cm_listeners, opal_list_t);
        OBJ_CONSTRUCT(&ibcm_pending_requests, opal_list_t);
        OBJ_CONSTRUCT(&ibcm_pending_replies, opal_list_t);
        initialized = true;

#if ENABLE_TIMERS
        {
            int i;
            for (i = 0; i < MAXTIMER; ++i) {
                timer_tmp[i] = timers[i] = 0.0;
            }
        }
#endif
    }

    TIMER_START(QUERY);

    /* Allocate the module struct.  Use calloc so that it's safe to
       finalize the module if something goes wrong. */
    m = calloc(1, sizeof(*m) + sizeof(*msg));
    if (NULL == m) {
        BTL_ERROR(("malloc failed!"));
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto error;
    }
    msg = (modex_msg_t*) (m + 1);
    BTL_VERBOSE(("created cpc module %p for btl %p",
                 (void*)m, (void*)btl));

    /* See if we've already got an IB CM listener for this device */
    for (item = opal_list_get_first(&ibcm_cm_listeners);
         item != opal_list_get_end(&ibcm_cm_listeners);
         item = opal_list_get_next(item)) {
        cmh = (ibcm_listen_cm_id_t*) item;
        if (cmh->ib_context == btl->device->ib_dev_context) {
            OBJ_RETAIN(cmh);
            break;
        }
    }
    /* If we got to the end of the list without finding a match, setup
       IB CM to start listening for connect requests.  Use our PID as
       the service ID. */
    if (opal_list_get_end(&ibcm_cm_listeners) == item) {
        char *filename;

        cmh = OBJ_NEW(ibcm_listen_cm_id_t);
        if (NULL == cmh) {
            BTL_ERROR(("malloc failed!"));
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto error;
        }

        /* libibcm <= v1.0.2 will print out a message to stderr if it
           can't find the /dev/infiniband/ucmX device.  So check for
           this file first; if it's not there, don't even bother
           calling ib_cm_open_device().  The "+6" accounts for
           "uverbs". */
        asprintf(&filename, "/dev/infiniband/ucm%s",
                 btl->device->ib_dev_context->device->dev_name + 6);
	rc = open(filename, O_RDWR);
        if (rc < 0) {
            /* We can't open the device for some reason (can't read,
               can't write, doesn't exist, ...etc.); IBCM is not setup
               on this node. */
            BTL_ERROR(("failed to open IB CM device: %s", filename));
            free(filename);
            rc = OMPI_ERR_NOT_SUPPORTED;
            goto error;
        }
        close(rc);
        free(filename);

        cmh->ib_context = btl->device->ib_dev_context;
        cmh->cm_device = ib_cm_open_device(btl->device->ib_dev_context);
        if (NULL == cmh->cm_device) {
            /* If we fail to open the IB CM device, it's not an error
               -- it's likely that IBCM simply isn't supported on this
               platform.  So print an optional message and return
               ERR_NOT_SUPPORTED (i.e., gracefully fail). */
            OBJ_RELEASE(cmh);
            BTL_ERROR(("failed to open IB CM device"));
            rc = OMPI_ERR_NOT_SUPPORTED;
            goto error;
        }
        OPAL_OUTPUT((-1, "opened ibcm device 0x%" PRIx64 " (%s:%d)",
                     (uint64_t) cmh->cm_device, 
                     ibv_get_device_name(cmh->ib_context->device),
                     openib_btl->port_num));

        if (0 != (rc = ib_cm_create_id(cmh->cm_device, 
                                       &cmh->listen_cm_id, NULL))) {
            /* Same rationale as above */
            OBJ_RELEASE(cmh);
            BTL_ERROR(("failed to ib_cm_create_id: rc=%d, errno=%d", rc, errno));
            rc = OMPI_ERR_NOT_SUPPORTED;
            goto error;
        }

        if (0 != (rc = ib_cm_listen(cmh->listen_cm_id, IB_CM_ASSIGN_SERVICE_ID, 0))) {
            /* Same rationale as above */ 
            OBJ_RELEASE(cmh); 
            BTL_ERROR(("failed to ib_cm_listen : rc=%d, errno=%d", rc, errno)); 
            rc = OMPI_ERR_NOT_SUPPORTED; 
            goto error; 
        } 

        if (0 != (rc = ib_cm_attr_id(cmh->listen_cm_id, &(cmh->param)))) {
            OBJ_RELEASE(cmh);
            BTL_ERROR(("failed to ib_cm_attr_id: rc=%d, errno=%d", rc, errno));
            rc = OMPI_ERR_NOT_SUPPORTED;
            goto error;
        }

        opal_list_append(&ibcm_cm_listeners, &(cmh->super));
    }
    m->cmh = cmh;
    imli = OBJ_NEW(ibcm_module_list_item_t);
    if (NULL == imli) {
        OBJ_RELEASE(cmh);
        BTL_ERROR(("malloc failed!"));
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto error;
    }
    imli->ibcm_module = m;
    opal_list_append(&(cmh->ibcm_modules), &(imli->super));

    /* IB CM initialized properly.  So fill in the rest of the CPC
       module. */
    m->btl = btl;
    m->cpc.data.cbm_component = &ompi_btl_openib_connect_ibcm;
    m->cpc.data.cbm_priority = ibcm_priority;
    m->cpc.data.cbm_modex_message = msg;

    /* Note that the LID is already included in the main modex message
       -- it is not ibcm-specific.  Also, don't assume that the port
       GUID is node_guid+port_number (e.g., QLogic DEVICEs use a
       different formula).  Query for the Nth GID (N = MCA param) on
       the port. */
    if (ibcm_gid_table_index > btl->ib_port_attr.gid_tbl_len) {
        BTL_ERROR(("desired GID table index (%d) is larger than the actual table size (%d) on %s:%d",
                     ibcm_gid_table_index,
                     btl->ib_port_attr.gid_tbl_len,
                     ibv_get_device_name(btl->device->ib_dev),
                     btl->port_num));
        rc = OMPI_ERR_UNREACH;
        goto error;
    }
    rc = ibv_query_gid(btl->device->ib_dev_context, btl->port_num, ibcm_gid_table_index, 
                       &gid);
    if (0 != rc) {
        BTL_ERROR(("system error (ibv_query_gid failed)"));
        rc = OMPI_ERR_UNREACH;
        goto error;
    }
    msg->mm_port_guid = ntoh64(gid.global.interface_id);
    msg->mm_lid = btl->lid;
    msg->mm_port_num = btl->port_num;
    msg->mm_service_id = cmh->param.service_id;
    m->cpc.data.cbm_modex_message_len = sizeof(*msg);

    m->cpc.cbm_endpoint_init = ibcm_endpoint_init;
    m->cpc.cbm_start_connect = ibcm_module_start_connect;
    m->cpc.cbm_endpoint_finalize = ibcm_endpoint_finalize;
    m->cpc.cbm_finalize = ibcm_module_finalize;
    /* Setting uses_cts=true also guarantees that we'll only be
       selected if QP 0 is PP */
    m->cpc.cbm_uses_cts = true;

    /* Start monitoring the fd associated with the cm_device */
    ompi_btl_openib_fd_monitor(cmh->cm_device->fd, OPAL_EV_READ,
                               ibcm_event_dispatch, cmh);

    /* All done */
    *cpc = (ompi_btl_openib_connect_base_module_t *) m;
    BTL_VERBOSE(("available for use on %s:%d",
                 ibv_get_device_name(btl->device->ib_dev),
                 btl->port_num));
    TIMER_STOP(QUERY);
    return OMPI_SUCCESS;

 error:
    ibcm_module_finalize(btl, (ompi_btl_openib_connect_base_module_t *) m);
    if (OMPI_ERR_NOT_SUPPORTED == OPAL_SOS_GET_ERROR_CODE(rc)) {
        BTL_VERBOSE(("unavailable for use on %s:%d; skipped",
                     ibv_get_device_name(btl->device->ib_dev),
                     btl->port_num));
    } else {
        BTL_VERBOSE(("unavailable for use on %s:%d; fatal error %d (%s)",
                     ibv_get_device_name(btl->device->ib_dev), 
                     btl->port_num, rc, 
                     opal_strerror(rc)));
    }
    return rc;
}

/*******************************************************************
 * Module
 *******************************************************************/

/* Returns max inlne size for qp #N */
static uint32_t max_inline_size(int qp, mca_btl_openib_device_t *device)
{
    if (mca_btl_openib_component.qp_infos[qp].size <= device->max_inline_data) {
        /* If qp message size is smaller than max_inline_data,
         * we should enable inline messages */
        return mca_btl_openib_component.qp_infos[qp].size;
    } else if (mca_btl_openib_component.rdma_qp == qp || 0 == qp) {
        /* If qp message size is bigger that max_inline_data, we
         * should enable inline messages only for RDMA QP (for PUT/GET
         * fin messages) and for the first qp */
        return device->max_inline_data;
    }
    /* Otherway it is no reason for inline */
    return 0;
}

/*
 * Create the local side of one qp.  The remote side will be connected
 * later.
 */
static int qp_create_one(mca_btl_base_endpoint_t* endpoint, int qp, 
                         struct ibv_srq *srq, uint32_t max_recv_wr, 
                         uint32_t max_send_wr)
{
    mca_btl_openib_module_t *openib_btl = endpoint->endpoint_btl;
    struct ibv_qp *my_qp;
    struct ibv_qp_init_attr init_attr;
    size_t req_inline;

    memset(&init_attr, 0, sizeof(init_attr));

    init_attr.qp_type = IBV_QPT_RC;
    init_attr.send_cq = openib_btl->device->ib_cq[BTL_OPENIB_LP_CQ];
    init_attr.recv_cq = openib_btl->device->ib_cq[qp_cq_prio(qp)];
    init_attr.srq = srq;
    init_attr.cap.max_inline_data = req_inline = 
        max_inline_size(qp, openib_btl->device);
    init_attr.cap.max_send_sge = 1;
    init_attr.cap.max_recv_sge = 1; /* we do not use SG list */
    if(BTL_OPENIB_QP_TYPE_PP(qp)) {
        /* Add one for the CTS receive frag that will be posted */
        init_attr.cap.max_recv_wr = max_recv_wr + 1;
    } else {
        init_attr.cap.max_recv_wr = 0;
    }
    init_attr.cap.max_send_wr = max_send_wr;

    my_qp = ibv_create_qp(openib_btl->device->ib_pd, &init_attr); 
    if (NULL == my_qp) { 
        BTL_ERROR(("error creating qp errno says %s", strerror(errno))); 
        return OMPI_ERROR; 
    }
    endpoint->qps[qp].qp->lcl_qp = my_qp;
    if (init_attr.cap.max_inline_data < req_inline) {
        endpoint->qps[qp].ib_inline_max = init_attr.cap.max_inline_data;
        orte_show_help("help-mpi-btl-openib-cpc-base.txt",
                       "inline truncated", orte_process_info.nodename,
                       ibv_get_device_name(openib_btl->device->ib_dev),
                       openib_btl->port_num,
                       req_inline, init_attr.cap.max_inline_data);
    } else {
        endpoint->qps[qp].ib_inline_max = req_inline;
    }
    /* Setup meta data on the endpoint */
    endpoint->qps[qp].qp->lcl_psn = lrand48() & 0xffffff;
    endpoint->qps[qp].credit_frag = NULL;

    return OMPI_SUCCESS;
}


/*
 * Create the local side of all the qp's.  The remote sides will be
 * connected later.
 */
static int qp_create_all(mca_btl_base_endpoint_t* endpoint,
                         ibcm_module_t *m)
{
    int qp, rc, pp_qp_num = 0;
    int32_t rd_rsv_total = 0;
    ibcm_endpoint_t *ie = (ibcm_endpoint_t*) endpoint->endpoint_local_cpc_data;

    for (qp = 0; qp < mca_btl_openib_component.num_qps; ++qp) {
        if (BTL_OPENIB_QP_TYPE_PP(qp)) {
            rd_rsv_total +=
                mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv;
            pp_qp_num++;
        }
    }

    /* if there is no pp QPs we still need reserved WQE for eager rdma flow
     * control */
    if (0 == pp_qp_num && true == endpoint->use_eager_rdma) {
        pp_qp_num = 1;
    }

    for (qp = 0; qp < mca_btl_openib_component.num_qps; ++qp) { 
        struct ibv_srq *srq = NULL;
        uint32_t max_recv_wr, max_send_wr;
        int32_t rd_rsv, rd_num_credits;

        /* QP used for SW flow control need some additional recourses */
        if (qp == mca_btl_openib_component.credits_qp) {
            rd_rsv = rd_rsv_total;
            rd_num_credits = pp_qp_num;
        } else {
            rd_rsv = rd_num_credits = 0;
        }

        if (BTL_OPENIB_QP_TYPE_PP(qp)) {
            max_recv_wr = mca_btl_openib_component.qp_infos[qp].rd_num + 
                rd_rsv;
            max_send_wr = mca_btl_openib_component.qp_infos[qp].rd_num +
                rd_num_credits;
        } else {
            srq = endpoint->endpoint_btl->qps[qp].u.srq_qp.srq;
            /* no receives are posted to SRQ qp */
            max_recv_wr = 0;
            max_send_wr = mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max
                + rd_num_credits;
        }

        /* Go create the actual qp */
        rc = qp_create_one(endpoint, qp, srq, max_recv_wr, max_send_wr);
        if (OMPI_SUCCESS != rc) {
            return rc;
        }
    }

    /* All done! */
    ie->ie_qps_created = true;
    return OMPI_SUCCESS;
}


/*
 * Fill in a path record for a peer.  This could be replaced with
 * calls to rdma_resolve_addr() / rdma_resolve_route(), but for now,
 * we fill it in with data we got from the modex, etc.
 */
static int fill_path_record(ibcm_module_t *m,
                            mca_btl_base_endpoint_t *endpoint,
                            struct ibv_sa_path_rec *path_rec)
{
    modex_msg_t *remote_msg = 
        (modex_msg_t*) endpoint->endpoint_remote_cpc_data->cbm_modex_message;
    modex_msg_t *local_msg = 
        (modex_msg_t*) m->cpc.data.cbm_modex_message;

    /* Global attributes */
    path_rec->dgid.global.subnet_prefix = 
        path_rec->sgid.global.subnet_prefix = 
        hton64(m->btl->port_info.subnet_id);
    path_rec->dgid.global.interface_id = hton64(remote_msg->mm_port_guid);
    path_rec->sgid.global.interface_id = hton64(local_msg->mm_port_guid);
    path_rec->dlid = htons(endpoint->rem_info.rem_lid);
    path_rec->slid = htons(m->btl->port_info.lid);

    /* Several remarks below are from e-mail exchanges with Sean
       Hefty.  We probably don't need all of these items, but I'm
       going to include them anyway (along with comments explaining
       why we don't need them) just for the sake of someone who is
       going to look at this code in the future. */
    /* 0 = IB traffic */
    path_rec->raw_traffic = 0;

    /* This is QoS stuff, which we're not using -- so just set to 0 */
    path_rec->flow_label = 0;
    path_rec->hop_limit = 0;
    path_rec->traffic_class = 0;

    /* IBCM currently only supports reversible paths */
    path_rec->reversible = 0x1000000;

    /* These are only used for SA queries, so set numb_path 1 to and
       others to 2 (from Sean) */
    path_rec->numb_path = 1;
    path_rec->mtu_selector = 2;
    path_rec->rate_selector = 2;
    path_rec->packet_life_time_selector = 2;

    /* Indicates which path record to use -- use the 0th one */
    path_rec->preference = 0;

    /* If the user specified a pkey, use it.  If not, use the first
       pkey on this port */
    path_rec->pkey = mca_btl_openib_component.ib_pkey_val;
    if (0 == path_rec->pkey) {
        uint16_t pkey;
        ibv_query_pkey(endpoint->endpoint_btl->device->ib_dev_context, 
                       endpoint->endpoint_btl->port_num, 0, &pkey);
        path_rec->pkey = ntohs(pkey);
    }

    path_rec->packet_life_time = mca_btl_openib_component.ib_timeout;
    path_rec->packet_life_time = 0;
    path_rec->sl = mca_btl_openib_component.ib_service_level;
    path_rec->mtu = endpoint->rem_info.rem_mtu;

    /* The rate is actually of type enum ibv_rate.  Figure it out from
       the bandwidth that we calculated for the btl. */
    switch (m->btl->super.btl_bandwidth) {
    case 2000:
        path_rec->rate = IBV_RATE_2_5_GBPS; break;
    case 4000:
        path_rec->rate = IBV_RATE_5_GBPS; break;
    case 8000:
        path_rec->rate = IBV_RATE_10_GBPS; break;
    case 16000:
        path_rec->rate = IBV_RATE_20_GBPS; break;
    case 24000:
        path_rec->rate = IBV_RATE_30_GBPS; break;
    case 32000:
        path_rec->rate = IBV_RATE_40_GBPS; break;
    case 48000:
        path_rec->rate = IBV_RATE_60_GBPS; break;
    case 64000:
        path_rec->rate = IBV_RATE_80_GBPS; break;
    case 96000:
        path_rec->rate = IBV_RATE_120_GBPS; break;
    default:
        /* Shouldn't happen */
        path_rec->rate = IBV_RATE_MAX; break;
    }

    BTL_VERBOSE(("Got src/dest subnet id: 0x%" PRIx64 " / 0x%" PRIx64,
                 path_rec->sgid.global.subnet_prefix,
                 path_rec->dgid.global.subnet_prefix));
    BTL_VERBOSE(("Got src/dest interface id: 0x%" PRIx64 " / 0x%" PRIx64, 
                 path_rec->sgid.global.interface_id,
                 path_rec->dgid.global.interface_id));
    BTL_VERBOSE(("Got src/dest lid: 0x%x / 0x%x", 
                 path_rec->slid, path_rec->dlid));
    BTL_VERBOSE(("Got raw_traffic: %d", path_rec->raw_traffic));

    BTL_VERBOSE(("Got flow_label: %d", path_rec->flow_label));
    BTL_VERBOSE(("Got hop_limit: %d", path_rec->hop_limit));
    BTL_VERBOSE(("Got traffic_class: %d", path_rec->traffic_class));
    BTL_VERBOSE(("Got reversible: 0x%x", path_rec->reversible));
    BTL_VERBOSE(("Got numb_path: %d", path_rec->numb_path));
    BTL_VERBOSE(("Got pkey: 0x%x", path_rec->pkey));

    BTL_VERBOSE(("Got sl: %d", path_rec->sl));
    BTL_VERBOSE(("Got mtu_selector: %d", path_rec->mtu_selector));
    BTL_VERBOSE(("Got mtu: %d", path_rec->mtu));
    BTL_VERBOSE(("Got rate_selector: %d", path_rec->rate_selector));
    BTL_VERBOSE(("Got rate: %d", path_rec->rate));
    BTL_VERBOSE(("Got packet_life_time_selector: %d", path_rec->packet_life_time_selector));
    BTL_VERBOSE(("Got packet lifetime: 0x%x", path_rec->packet_life_time));
    BTL_VERBOSE(("Got preference: %d", path_rec->preference));

    return OMPI_SUCCESS;
}

static int ibcm_endpoint_init(struct mca_btl_base_endpoint_t *endpoint)
{
    ibcm_endpoint_t *ie = endpoint->endpoint_local_cpc_data = 
        calloc(1, sizeof(ibcm_endpoint_t));
    if (NULL == ie) {
        BTL_ERROR(("malloc failed!"));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    BTL_VERBOSE(("endpoint %p / %p", (void*)endpoint, (void*)ie));
    ie->ie_cpc = endpoint->endpoint_local_cpc;
    ie->ie_endpoint = endpoint;
    ie->ie_qps_created = 
        ie->ie_recv_buffers_posted = false;
    ie->ie_qps_to_connect = mca_btl_openib_component.num_qps;

    return OMPI_SUCCESS;
}

/* To avoid all kinds of nasty race conditions (because IBCM is based
 * on UD, in which the ordering of messages is not guaranteed), we
 * only allow connections to be made in one direction.  So use a
 * simple (arbitrary) test to decide which direction is allowed to
 * initiate the connection: the process with the lower GUID wins.  If
 * the GUIDs are the same (i.e., the MPI procs are on the same node),
 * then compare PIDs.
 */
static bool i_initiate(ibcm_module_t *m,
                       mca_btl_openib_endpoint_t *endpoint)
{
    modex_msg_t *msg = 
        (modex_msg_t*) endpoint->endpoint_remote_cpc_data->cbm_modex_message;
    uint64_t my_port_guid = ntoh64(m->btl->device->ib_dev_attr.node_guid) + 
        m->btl->port_num;
    uint64_t service_id = m->cmh->param.service_id;
    
    BTL_VERBOSE(("i_initiate: my guid (%0" PRIx64 "), msg guid (%0" PRIx64 ")",
                 my_port_guid, msg->mm_port_guid));
    BTL_VERBOSE(("i_initiate: my service id (%d), msg service id (%d)",
                 service_id, msg->mm_service_id));

    return
        (my_port_guid == msg->mm_port_guid &&
         service_id < msg->mm_service_id) ? true : 
        (my_port_guid < msg->mm_port_guid) ? true : false;
}

/*
 * Allocate a CM request structure and initialize some common fields
 * (that are independent of the specific QP, etc.)
 */
static ibcm_request_t *alloc_request(ibcm_module_t *m, modex_msg_t *msg,
                                     struct ibv_sa_path_rec *path_rec,
                                     mca_btl_base_endpoint_t *endpoint)
{
    struct ib_cm_req_param *cm_req;
    ibcm_request_t *req = OBJ_NEW(ibcm_request_t);
    BTL_VERBOSE(("allocated cached req id: 0x%" PRIx64, (void*)req));
        
    if (NULL == req) {
        return NULL;
    }
    
    /* Create this CM ID */
    if (0 != ib_cm_create_id(m->cmh->cm_device,
                             &(req->super.cm_id),
                             NULL)) {
        BTL_VERBOSE(("failed to create active device id"));
        OBJ_RELEASE(req);
        return NULL;
    }
    BTL_VERBOSE(("created CM ID 0x%" PRIx64, &(req->super.cm_id)));
    
    /* This data is constant for all the QP's */
    req->path_rec = *path_rec;
    req->endpoint = endpoint;
    
    cm_req = &(req->cm_req);
    cm_req->qp_type = IBV_QPT_RC;
    cm_req->alternate_path = NULL;
    cm_req->service_id = msg->mm_service_id;
    cm_req->responder_resources = 10;
    cm_req->initiator_depth = 10;
    cm_req->retry_count = mca_btl_openib_component.ib_retry_count;
    cm_req->peer_to_peer = 0;
    /* JMS what does this do? */
    cm_req->flow_control = 0;
    /* JMS what's the units? */
    cm_req->remote_cm_response_timeout = 20;
    cm_req->local_cm_response_timeout = 20;
    cm_req->max_cm_retries = 5;
    
    req->private_data.ireqd_pid = m->cmh->param.service_id;
    req->private_data.ireqd_ep_index = endpoint->index;

    return req;
}


static void print_req(struct ib_cm_req_param *cm_req)
{
    BTL_VERBOSE(("cm_req->primary_path: 0x%" PRIx64, cm_req->primary_path));
    BTL_VERBOSE(("cm_req->alternate_path: 0x%" PRIx64, cm_req->alternate_path));
    BTL_VERBOSE(("cm_req->service_id: 0x016%" PRIx64, cm_req->service_id));
    BTL_VERBOSE(("cm_req->qp_num: %d", cm_req->qp_num));
    BTL_VERBOSE(("cm_req->qp_type: %d", cm_req->qp_type));
    BTL_VERBOSE(("cm_req->starting_psn: %d", cm_req->starting_psn));
    BTL_VERBOSE(("cm_req->private_data: %" PRIx64, cm_req->private_data));
    BTL_VERBOSE(("cm_req->private_data_len: %d", cm_req->private_data_len));
    BTL_VERBOSE(("cm_req->peer_to_peer: %d", cm_req->peer_to_peer));
    BTL_VERBOSE(("cm_req->responder_resources: %d", cm_req->responder_resources));
    BTL_VERBOSE(("cm_req->initiator_depth: %d", cm_req->initiator_depth));
    BTL_VERBOSE(("cm_req->remote_cm_response_timeout: %d", cm_req->remote_cm_response_timeout));
    BTL_VERBOSE(("cm_req->flow_control: %d", cm_req->flow_control));
    BTL_VERBOSE(("cm_req->local_cm_response_timeout: %d", cm_req->local_cm_response_timeout));
    BTL_VERBOSE(("cm_req->retry_count: %d", cm_req->retry_count));
    BTL_VERBOSE(("cm_req->rnr_retry_count: %d", cm_req->rnr_retry_count));
    BTL_VERBOSE(("cm_req->max_cm_retries: %d", cm_req->max_cm_retries));
    BTL_VERBOSE(("cm_req->srq: %d", cm_req->srq));
}
 
static int ibcm_module_start_connect(ompi_btl_openib_connect_base_module_t *cpc,
                                     mca_btl_base_endpoint_t *endpoint)
{
    int i, rc;
    ibcm_module_t *m = (ibcm_module_t *) cpc;
    ibcm_endpoint_t *ie = 
        (ibcm_endpoint_t *) endpoint->endpoint_local_cpc_data;
    modex_msg_t *msg = 
        (modex_msg_t*) endpoint->endpoint_remote_cpc_data->cbm_modex_message;
    struct ibv_sa_path_rec path_rec;
    bool do_initiate;

    TIMER_START(START_CONNECT);
    BTL_VERBOSE(("endpoint %p (lid %d, ep index %d)", 
                 (void*)endpoint, endpoint->endpoint_btl->port_info.lid,
                 endpoint->index));

    /* Has an incoming request already initiated the connect sequence
       on this endpoint?  If so, just exit successfully -- the
       incoming request process will eventually complete
       successfully. */
    opal_mutex_lock(&ie->ie_lock);
    if (0 != ie->ie_connection_flags) {
        opal_mutex_unlock(&ie->ie_lock);
        BTL_VERBOSE(("already ongoing %p", (void*)endpoint));
        return OMPI_SUCCESS;
    }
    ie->ie_connection_flags = CFLAGS_ONGOING;
    opal_mutex_unlock(&ie->ie_lock);

    /* Set the endpoint state to "connecting" (this function runs in
       the main MPI thread; not the service thread, so we can set the
       endpoint_state here with no memory barriers). */
    endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;

    /* Fill in the path record for this peer */
    if (OMPI_SUCCESS != fill_path_record(m, endpoint, &path_rec)) {
        BTL_VERBOSE(("================ start connect failed!!!"));
        rc = OMPI_ERR_NOT_FOUND;
        goto err;
    }
        
    /* If we're not the initiator, make a bogus QP (must be done
       before we make all the other QPs) */

    do_initiate = i_initiate(m, endpoint);
    if (!do_initiate) {
        rc = qp_create_one(endpoint, 0, NULL, 1, 1);
        if (OMPI_SUCCESS != rc) {
            goto err;
        }
        ie->ie_bogus_qp = endpoint->qps[0].qp->lcl_qp;
    }
        
    /* Make the local side of all the QP's */
    if (OMPI_SUCCESS != (rc = qp_create_all(endpoint, m))) {
        goto err;
    }

    /* Check initiation direction (see comment above i_initiate()
       function): 

       - if this is the side that is not supposed to initiate, then
         send a single bogus request that we expect to be rejected.
         The purpose of the request is to wake up the other side to
         force *them* to initiate the connection.

       - if this is the real initiation side, we create all the QPs
         and send off as many connect requests as is appropriate

       Note that there are completel separate code paths for these two
       cases.  Having separate paths for these options makes the code
       *much* cleaner / easier to read at the expense of some
       duplication.  Trying to integrate the two results in oodles of
       special cases that just isn't worth it. */

    if (do_initiate) {
        ie->ie_cm_id_cache_size = mca_btl_openib_component.num_qps;
        ie->ie_cm_id_cache = calloc(ie->ie_cm_id_cache_size,
                                    sizeof(ibcm_base_cm_id_t*));
        if (NULL == ie->ie_cm_id_cache) {
            BTL_ERROR(("malloc failed!"));
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto err;
        }

        for (i = 0; i < mca_btl_openib_component.num_qps; ++i) {
            ibcm_request_t *req;
            struct ib_cm_req_param *cm_req;

            /* Allocate a CM ID cache object */
            ie->ie_cm_id_cache[i] = OBJ_NEW(ibcm_base_cm_id_t);
            if (NULL == ie->ie_cm_id_cache[i]) {
                BTL_ERROR(("malloc failed!"));
                rc = OMPI_ERR_OUT_OF_RESOURCE;
                goto err;
            }

            /* Initialize the request-common fields */
            req = alloc_request(m, msg, &path_rec, endpoint);
            if (NULL == req) {
                BTL_ERROR(("malloc failed!"));
                rc = OMPI_ERR_OUT_OF_RESOURCE;
                goto err;
            }
            ie->ie_cm_id_cache[i]->cm_id = req->super.cm_id;

            /* On PP QPs we have SW flow control, no need for rnr
               retries. Setting it to zero helps to catch bugs */
            cm_req = &(req->cm_req);
            cm_req->rnr_retry_count = BTL_OPENIB_QP_TYPE_PP(i) ? 0 :
                mca_btl_openib_component.ib_rnr_retry;
            cm_req->srq = BTL_OPENIB_QP_TYPE_SRQ(i);
            cm_req->qp_num = endpoint->qps[i].qp->lcl_qp->qp_num;
            cm_req->starting_psn = endpoint->qps[i].qp->lcl_psn;
            BTL_VERBOSE(("sending my qpn %d, psn %d", 
                         cm_req->qp_num, cm_req->starting_psn));
            
            req->private_data.ireqd_request = req;
            req->private_data.ireqd_qp_index = i;
            
            /* Send the request */
            BTL_VERBOSE(("sending connect request %d of %d (id %p)",
                        i, mca_btl_openib_component.num_qps,
                         (void*)req->super.cm_id));
            if (mca_btl_base_verbose > 0) {
                print_req(cm_req);
            }
            if (0 != (rc = ib_cm_send_req(req->super.cm_id, cm_req))) {
                BTL_VERBOSE(("Got nonzero return from ib_cm_send_req: %d, errno %d", rc, errno));
                rc = OMPI_ERR_UNREACH;
                goto err;
            }

            /* Save the request on the global "pending requests" list */
            opal_list_append(&ibcm_pending_requests, &(req->super.super));
        }
    }

    /* The case where we're sending the request and expecting it to be
       rejected */
    else {
        ibcm_request_t *req;
        struct ib_cm_req_param *cm_req;

        /* Initialize the request-common fields */
        req = alloc_request(m, msg, &path_rec, endpoint);
        if (NULL == req) {
            BTL_ERROR(("malloc failed!"));
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto err;
        }
        cm_req = &(req->cm_req);

        /* Setup one request to be sent (and eventually rejected) */
        cm_req->rnr_retry_count = mca_btl_openib_component.ib_rnr_retry;
        cm_req->srq = 0;
        cm_req->qp_num = ie->ie_bogus_qp->qp_num;
        cm_req->starting_psn = 0;
        BTL_VERBOSE(("sending BOGUS qpn %d, psn %d (id %p)", 
                    cm_req->qp_num, cm_req->starting_psn,
                     (void*)req->super.cm_id));

        req->private_data.ireqd_request = req;
        req->private_data.ireqd_qp_index = 0;
        if (mca_btl_base_verbose > 0) {
            print_req(cm_req);
        }

        /* Send the request */
        if (0 != (rc = ib_cm_send_req(req->super.cm_id, cm_req))) {
            BTL_VERBOSE(("Got nonzero return from ib_cm_send_req: %d", rc));
            rc = OMPI_ERR_UNREACH;
            goto err;
        }

        /* Save the request on the global "pending requests" list */
        opal_list_append(&ibcm_pending_requests, &(req->super.super));
    }

    opal_progress_event_users_increment();
    BTL_VERBOSE(("connect request send successfully"));
    TIMER_STOP(START_CONNECT);
    return OMPI_SUCCESS;

 err:
    BTL_VERBOSE(("error!"));
    if (NULL != ie && NULL != ie->ie_cm_id_cache) {
        free(ie->ie_cm_id_cache);
        ie->ie_cm_id_cache = NULL;
        ie->ie_cm_id_cache_size = 0;
    }
    return rc;
}

/*--------------------------------------------------------------------*/

/*
 * Callback from when we stop monitoring the cm_device fd
 */
static void *callback_unlock(int fd, int flags, void *context)
{
    volatile int *barrier = (volatile int *) context;
    OPAL_OUTPUT((-1, "ibcm unlocking main thread"));
    *barrier = 1;
    return NULL;
}

/*--------------------------------------------------------------------*/

static void ibcm_listen_cm_id_constructor(ibcm_listen_cm_id_t *cmh)
{
    OBJ_CONSTRUCT(&(cmh->ibcm_modules), opal_list_t);
}

static void ibcm_listen_cm_id_destructor(ibcm_listen_cm_id_t *cmh)
{
    volatile int barrier = 0;
    opal_list_item_t *item;

    /* Remove all the ibcm module items */
    for (item = opal_list_remove_first(&(cmh->ibcm_modules));
         NULL != item; 
         item = opal_list_remove_first(&(cmh->ibcm_modules))) {
        OBJ_RELEASE(item);
    }

    /* Remove this handle from the ibcm_cm_listeners list */
    for (item = opal_list_get_first(&ibcm_cm_listeners);
         item != opal_list_get_end(&ibcm_cm_listeners);
         item = opal_list_get_next(item)) {
        if (item == &(cmh->super)) {
            opal_list_remove_item(&ibcm_cm_listeners, item);
            break;
        }
    }

    /* If this handle wasn't in the ibcm_cm_listeners list, then this
       handle was destroyed before it was used and we're done.
       Otherwise, there's some more cleanup to do. */
    if (item != opal_list_get_end(&ibcm_cm_listeners)) {

        /* Stop monitoring the cm_device's fd (wait for it to be
           released from the monitoring entity) */
        ompi_btl_openib_fd_unmonitor(cmh->cm_device->fd, 
                                     callback_unlock,

                                     (void*) &barrier);
        /* JMS debug code while figuring out the IBCM problem */
#if 0
        while (0 == barrier) {
            sched_yield();
        }
#else
        {
            time_t t = time(NULL);
            OPAL_OUTPUT((-1, "main thread waiting for ibcm barrier"));
            while (0 == barrier) {
                sched_yield();
                if (time(NULL) - t > 5) {
                    OPAL_OUTPUT((-1, "main thread been looping for a long time..."));
                    break;
                }
            }
        }
#endif

        /* Destroy the listener */
        if (NULL != cmh->listen_cm_id) {
            OPAL_OUTPUT((-1, "destryoing ibcm listener 0x%" PRIx64,
                         (uint64_t) cmh->listen_cm_id));
            ib_cm_destroy_id(cmh->listen_cm_id);
        }

        /* Close the CM device */
        if (NULL != cmh->cm_device) {
            OPAL_OUTPUT((-1, "closing ibcm device 0x%" PRIx64 " (%s)",
                         (uint64_t) cmh->cm_device, 
                         ibv_get_device_name(cmh->ib_context->device)));
            ib_cm_close_device(cmh->cm_device);
        }
    }
}

/*--------------------------------------------------------------------*/

static void ibcm_request_cm_id_constructor(ibcm_request_t *h)
{
    memset(&(h->cm_req), 0, sizeof(h->cm_req));
    memset(&(h->path_rec), 0, sizeof(h->path_rec));
    memset(&(h->private_data), 0, sizeof(h->private_data));

    h->cm_req.primary_path = &(h->path_rec);
    h->cm_req.private_data = &(h->private_data);
    h->cm_req.private_data_len = sizeof(h->private_data);
}

/*--------------------------------------------------------------------*/

static void ibcm_reply_cm_id_constructor(ibcm_reply_t *h)
{
    memset(&(h->cm_rep), 0, sizeof(h->cm_rep));
    memset(&(h->private_data), 0, sizeof(h->private_data));

    h->cm_rep.private_data = &(h->private_data);
    h->cm_rep.private_data_len = sizeof(h->private_data);
}

/*--------------------------------------------------------------------*/

static int ibcm_endpoint_finalize(struct mca_btl_base_endpoint_t *endpoint)
{
    ibcm_endpoint_t *ie =
        (ibcm_endpoint_t *) endpoint->endpoint_local_cpc_data;
    BTL_VERBOSE(("endpoint %p", (void*)endpoint));
    
    /* Free the stuff we allocated in ibcm_module_init */
    if (NULL != ie) {
        int i;
        for (i = 0; i < ie->ie_cm_id_cache_size; ++i) {
            if (NULL != ie->ie_cm_id_cache[i]) {
                BTL_VERBOSE(("Endpoint %p (%p), destroying ID %d (%p)",
                            (void*)endpoint,
                            (void*)ie,
                             i, (void*)&(ie->ie_cm_id_cache[i]->cm_id)));
                ib_cm_destroy_id(ie->ie_cm_id_cache[i]->cm_id);
                OBJ_RELEASE(ie->ie_cm_id_cache[i]);
            }
        }

        if (ie->ie_cm_id_cache_size > 0) {
            free(ie->ie_cm_id_cache);
        }
        free(ie);
        endpoint->endpoint_local_cpc_data = NULL;
    }

    BTL_VERBOSE(("endpoint finalize done: %p", (void*)endpoint));
    return OMPI_SUCCESS;
}

/*--------------------------------------------------------------------*/

static int ibcm_module_finalize(mca_btl_openib_module_t *btl,
                                ompi_btl_openib_connect_base_module_t *cpc)
{
    ibcm_module_t *m = (ibcm_module_t *) cpc;

    /* If we previously successfully initialized, then release
       everything */
    if (NULL != m && NULL != m->cmh) {
        OBJ_RELEASE(m->cmh);
    }
    
    return OMPI_SUCCESS;
}

/*--------------------------------------------------------------------*/

static int ibcm_component_finalize(void)
{
#if ENABLE_TIMERS
    print_timers();
#endif
    return OMPI_SUCCESS;
}

/*--------------------------------------------------------------------*/

/*
 * We have received information about the remote peer's QP; move the
 * local QP through INIT to RTR.
 */
static int qp_to_rtr(int qp_index, struct ib_cm_id *cm_id,
                     mca_btl_openib_endpoint_t *endpoint)
{
    int attr_mask;
    struct ibv_qp_attr attr;
    struct ibv_qp *qp = endpoint->qps[qp_index].qp->lcl_qp;
    mca_btl_openib_module_t *btl = endpoint->endpoint_btl;
    enum ibv_mtu mtu;

    TIMER_START(QP_TO_RTR);

    /* IB CM does not negotiate the MTU for us, so we have to figure
       it out ourselves.  Luckly, we know what the MTU is of the other
       port (from its modex message), so we can figure out the highest
       MTU that we have in common. */
    mtu = (btl->device->mtu < endpoint->rem_info.rem_mtu) ?
        btl->device->mtu : endpoint->rem_info.rem_mtu;

    BTL_VERBOSE(("Set MTU to IBV value %d (%s bytes)", mtu,
                 (mtu == IBV_MTU_256) ? "256" :
                 (mtu == IBV_MTU_512) ? "512" :
                 (mtu == IBV_MTU_1024) ? "1024" :
                 (mtu == IBV_MTU_2048) ? "2048" :
                 (mtu == IBV_MTU_4096) ? "4096" :
                 "unknown (!)"));
    BTL_VERBOSE(("connect qp set to IBV value %d (%s bytes)", mtu,
                 (mtu == IBV_MTU_256) ? "256" :
                 (mtu == IBV_MTU_512) ? "512" :
                 (mtu == IBV_MTU_1024) ? "1024" :
                 (mtu == IBV_MTU_2048) ? "2048" :
                 (mtu == IBV_MTU_4096) ? "4096" :
                 "unknown (!)"));
    
    /* Move the QP into the INIT state */
    memset(&attr, 0, sizeof(attr));
    attr.qp_state = IBV_QPS_INIT;
    if (0 != ib_cm_init_qp_attr(cm_id, &attr, &attr_mask)) {
        BTL_ERROR(("error initializing IB CM qp attr INIT"));
        return OMPI_ERROR;
    }

    if (0 != ibv_modify_qp(qp, &attr, attr_mask)) {
        BTL_ERROR(("error modifying qp to INIT errno says %s", strerror(errno))); 
        return OMPI_ERROR;
    } 

    /* Move the QP into the RTR state */
    attr.qp_state = IBV_QPS_RTR;
    if (0 != ib_cm_init_qp_attr(cm_id, &attr, &attr_mask)) {
        BTL_ERROR(("error initializing IB CM qp attr RTR"));
        return OMPI_ERROR;
    }

    /* The IB CM API just told us a few more attributes about the
       remote side, so save these as well */
    endpoint->rem_info.rem_qps[qp_index].rem_qp_num = attr.dest_qp_num;

    /* Setup attributes */
    attr.path_mtu = mtu;
    attr.rq_psn = endpoint->qps[qp_index].qp->lcl_psn;
    BTL_VERBOSE(("setting rq psn: %d", attr.rq_psn));
    /* IBM CM does not set these values for us */
    attr.max_dest_rd_atomic = mca_btl_openib_component.ib_max_rdma_dst_ops;
    attr.min_rnr_timer = mca_btl_openib_component.ib_min_rnr_timer;
    
    if (0 != ibv_modify_qp(qp, &attr,
                           attr_mask |
                           IBV_QP_PATH_MTU |
                           IBV_QP_MAX_DEST_RD_ATOMIC |
                           IBV_QP_MIN_RNR_TIMER
                           )) {
        BTL_ERROR(("error modifing QP to RTR errno says %s",
                   strerror(errno)));
        return OMPI_ERROR; 
    }
    
    /* All done */
    TIMER_STOP(QP_TO_RTR);
    return OMPI_SUCCESS;
}

/*
 * Move the QP state to RTS
 */
static int qp_to_rts(int qp_index, struct ib_cm_id *cm_id,
                     mca_btl_openib_endpoint_t *endpoint)
{
    int rc, attr_mask;
    struct ibv_qp_attr attr;
    struct ibv_qp *qp = endpoint->qps[qp_index].qp->lcl_qp;

    TIMER_START(QP_TO_RTS);

    /* Setup attributes */
    memset(&attr, 0, sizeof(attr));
    attr.qp_state = IBV_QPS_RTS;
    if (0 != (rc = ib_cm_init_qp_attr(cm_id, &attr, &attr_mask))) {
        BTL_ERROR(("error initializing IB CM qp attr RTS; rc=%d, errno=%d",
                   rc, errno));
        return OMPI_ERROR;
    }
    if (0 != (rc = ibv_modify_qp(qp, &attr, attr_mask))) {
        BTL_ERROR(("error modifing QP (index %d) to RTS errno says %s; rc=%d, errno=%d",
                   qp_index, strerror(errno), rc, errno));
        return OMPI_ERROR; 
    }
    
    /* All done */
    BTL_VERBOSE(("successfully set RTS"));
    TIMER_STOP(QP_TO_RTS);
    return OMPI_SUCCESS;
}

/*
 * Callback (from main thread) when an incoming IBCM request needs to
 * initiate a new connection in the other direction.
 */
static void *callback_start_connect(void *context)
{
    callback_start_connect_data_t *cbdata = 
        (callback_start_connect_data_t *) context;

    BTL_VERBOSE(("ibcm scheduled callback: calling start_connect()"));
    BTL_VERBOSE(("ibcm scheduled callback: cbdata %p",
                 (void*)cbdata));
    BTL_VERBOSE(("ibcm scheduled callback: endpoint %p",
                 (void*)cbdata->cscd_endpoint));
    BTL_VERBOSE(("ibcm scheduled callback: ie %p",
                 (void*)cbdata->cscd_endpoint->endpoint_local_cpc_data));
    BTL_VERBOSE(("ibcm scheduled callback: msg %p",
                 (void*)cbdata->cscd_endpoint->endpoint_remote_cpc_data->cbm_modex_message));
    ibcm_module_start_connect(cbdata->cscd_cpc, cbdata->cscd_endpoint);
    free(cbdata);

    return NULL;
}

/*
 * Passive has received a connection request from a active
 */
static int request_received(ibcm_listen_cm_id_t *cmh, 
                            struct ib_cm_event *event)
{
    int i, rc = OMPI_ERROR;
    mca_btl_openib_proc_t *ib_proc = NULL;
    mca_btl_openib_endpoint_t *endpoint = NULL;
    ibcm_endpoint_t *ie = NULL;
    struct ib_cm_req_event_param *req = &(event->param.req_rcvd);
    modex_msg_t *msg = NULL;
    bool found, do_initiate;
    ibcm_reject_reason_t rej_reason = REJ_MAX;
    ibcm_req_data_t *active_private_data =
        (ibcm_req_data_t*) event->private_data;
    int qp_index = active_private_data->ireqd_qp_index;
    opal_list_item_t *item;
    ibcm_module_list_item_t *imli;
    ibcm_module_t *imodule = NULL;
    ibcm_reply_t *rep;

    TIMER_START(REQUEST_RECEIVED);
    BTL_VERBOSE(("remote qp index %d, remote guid 0x%" PRIx64 ", remote qkey %u, remote qpn %d, remote psn %d",
                qp_index,
                ntoh64(req->primary_path->dgid.global.interface_id),
                req->remote_qkey, req->remote_qpn,
                 req->starting_psn));

    /* Find the ibcm module for this request: remember that IB CM
       events come in per *device*, not per *port*.  So we just got a
       device event, and have to find the ibcm_module_t (i.e., local
       port/openib BTL module ) that corresponds to it. */
    BTL_VERBOSE(("looking for ibcm module -- source port guid: 0x%" PRIx64 " (%p)",
                ntoh64(req->primary_path->sgid.global.interface_id), 
                 (void*)cmh));
    for (item = opal_list_get_first(&(cmh->ibcm_modules));
         item != opal_list_get_end(&(cmh->ibcm_modules));
         item = opal_list_get_next(item)) {
        modex_msg_t *msg;
        imli = (ibcm_module_list_item_t*) item;
        imodule = imli->ibcm_module;
        msg = imli->ibcm_module->cpc.data.cbm_modex_message;
        BTL_VERBOSE(("comparing ibcm module port guid: 0x%" PRIx64,
                     msg->mm_port_guid));
        if (msg->mm_port_guid ==
            ntoh64(req->primary_path->sgid.global.interface_id)) {
            break;
        }
    }
    assert (item != opal_list_get_end(&(cmh->ibcm_modules)));

    /* Find the endpoint corresponding to the remote peer who is
       calling.  First, cycle through all the openib procs. */
    /* JMS: optimization target -- can we send something in private
       data to find the proc directly instead of having to search
       through *all* procs? */
    OPAL_THREAD_LOCK(&mca_btl_openib_component.ib_lock);
    for (found = false, ib_proc = (mca_btl_openib_proc_t*)
             opal_list_get_first(&mca_btl_openib_component.ib_procs);
         !found && 
             ib_proc != (mca_btl_openib_proc_t*)
             opal_list_get_end(&mca_btl_openib_component.ib_procs);
         ib_proc  = (mca_btl_openib_proc_t*)opal_list_get_next(ib_proc)) {
        BTL_VERBOSE(("checking ib_proc %p", (void*)ib_proc));
        /* Now cycle through all the endpoints on that proc */
        for (i = 0; !found && i < (int) ib_proc->proc_endpoint_count; ++i) {
            BTL_VERBOSE(("checking endpoint %d of %d (ep %p, cpc data %p)",
                        i, (int) ib_proc->proc_endpoint_count, 
                        (void*)ib_proc->proc_endpoints[i],
                         (void*)ib_proc->proc_endpoints[i]->endpoint_remote_cpc_data));
            if (NULL == ib_proc->proc_endpoints[i]->endpoint_remote_cpc_data) {
                BTL_VERBOSE(("NULL remote cpc data!"));
            }
            msg = ib_proc->proc_endpoints[i]->endpoint_remote_cpc_data->cbm_modex_message;
            BTL_VERBOSE(("my guid 0x%" PRIx64 ", remote guid 0x%" PRIx64,
                        msg->mm_port_guid,
                         ntoh64(req->primary_path->dgid.global.interface_id)));
            BTL_VERBOSE(("my LID %d, remote LID %d",
                         msg->mm_lid,
                         ntohs(req->primary_path->dlid)));
            if (msg->mm_port_guid == 
                ntoh64(req->primary_path->dgid.global.interface_id) &&
                msg->mm_service_id == active_private_data->ireqd_pid &&
                msg->mm_port_num == req->port &&
                msg->mm_lid == htons(req->primary_path->dlid)) {
                BTL_VERBOSE(("*** found matching endpoint!!!"));
                endpoint = ib_proc->proc_endpoints[i];
                found = true;
            }
        }
    }
    OPAL_THREAD_UNLOCK(&mca_btl_openib_component.ib_lock);
    if (!found) {
        BTL_VERBOSE(("could not find match for calling endpoint!"));
        rc = OMPI_ERR_NOT_FOUND;
        rej_reason = REJ_PEER_NOT_FOUND;
        goto reject;
    }
    BTL_VERBOSE(("Found endpoint %p", (void*)endpoint));

    /* Get our CPC-local data on the endpoint */
    ie = (ibcm_endpoint_t*) endpoint->endpoint_local_cpc_data;

    /* Check initiation direction (see comment above i_initiate()
       function).  If the connection comes from the "wrong" direction,
       then the remote peer expects it to be rejected, and further
       expects us to initiate it in the "right" direction. */
    do_initiate = i_initiate(imodule, endpoint);

    /* See if there is any activity happening on this endpoint
       already.  There's likely little reason to have fine-grained
       rejection reasons; we have them here just to help with
       debugging. */
    opal_mutex_lock(&ie->ie_lock);
    if (do_initiate) {
        BTL_VERBOSE(("request came from wrong direction"));
        rc = OMPI_SUCCESS;
        rej_reason = REJ_WRONG_DIRECTION;
    } else if (ie->ie_connection_flags & CFLAGS_COMPLETED) {
        BTL_VERBOSE(("all QPs already connected"));
        rej_reason = REJ_ALREADY_CONNECTED;
        rc = OMPI_SUCCESS;
    } else if (ie->ie_connection_flags & CFLAGS_ONGOING) {
        /* See if the request for this QP already arrived */
        if (ie->ie_qps_created && 
            IBV_QPS_RESET != endpoint->qps[qp_index].qp->lcl_qp->state) {
            BTL_VERBOSE(("this QP (%d) already connected",
                         qp_index));
            rej_reason = REJ_QP_ALREADY_CONNECTED;
            rc = OMPI_SUCCESS;
        }
    } else {
        /* this is the first activity -- accept */
        BTL_VERBOSE(("first initiation request"));
        ie->ie_connection_flags |= CFLAGS_ONGOING;
    }
    opal_mutex_unlock(&ie->ie_lock);

    /* If logic above selected a rejection reason, reject this
       request.  Note that if the same request arrives again later,
       IBCM will trigger a new event and we'll just reject it
       again. */
    if (REJ_MAX != rej_reason) {
        BTL_VERBOSE(("arbitrartion failed -- reject"));
        goto reject;
    }

    BTL_VERBOSE(("initiation arbitration successful -- proceeding"));

    /* If this is the first request we have received for this
       endpoint, then make *all* the QP's (because we analyze all the
       local QP attributes when making them; it's easier this way).
       If this is not the first request, then assume that all the QPs
       have been created already and we can just lookup what we need. */
    if (!ie->ie_qps_created) {
        /* Set the endpoint_state to "CONNECTING".  This is running
           in the service thread, so we need to do a write barrier. */
        endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;
        opal_atomic_wmb();

        if (OMPI_SUCCESS != (rc = qp_create_all(endpoint, imodule))) {
            rej_reason = REJ_PASSIVE_SIDE_ERROR;
            BTL_ERROR(("qp_create_all failed -- reject"));
            goto reject;
        }
        ie->ie_qps_created = true;
        BTL_VERBOSE(("created qp's"));
    }

    /* Save these numbers on the endpoint for reference.  Other values
       are filled in during qp_to_rtr (because we don't get them until
       we call ib_cm_attr_init()).  We already have the remote LID,
       subnet ID, and MTU from the port's modex message. */
    endpoint->rem_info.rem_qps[qp_index].rem_psn = 
        event->param.req_rcvd.starting_psn;
    endpoint->rem_info.rem_index = active_private_data->ireqd_ep_index;

    /* Move QP to RTR.  Note that you have to do this *before* posting
       receive buffers. */
    if (OMPI_SUCCESS != (rc = qp_to_rtr(qp_index, event->cm_id, endpoint))) {
        BTL_VERBOSE(("failed move to RTR (index %d)", qp_index));
        rej_reason = REJ_PASSIVE_SIDE_ERROR;
        goto reject;
    }

    /* Post a single receive buffer on the smallest QP for the CTS
       protocol */
    if (!ie->ie_recv_buffers_posted) {
        struct ibv_recv_wr *bad_wr, *wr;

        assert(NULL != endpoint->endpoint_cts_frag.super.super.base.super.ptr);
        wr = &(endpoint->endpoint_cts_frag.rd_desc);
        assert(NULL != wr);
        wr->next = NULL;

        OPAL_OUTPUT((-1, "REQUEST posting CTS recv buffer"));
        if (0 != ibv_post_recv(endpoint->qps[mca_btl_openib_component.credits_qp].qp->lcl_qp, wr, &bad_wr)) {
            BTL_VERBOSE(("failed to post CTS recv buffer"));
            rej_reason = REJ_PASSIVE_SIDE_ERROR;
            goto reject;
        }
        ie->ie_recv_buffers_posted = true;

        /* Further, create an array to cache all the active CM ID's so
           that they can be destroyed when the endpoint is
           destroyed */
        ie->ie_cm_id_cache_size = mca_btl_openib_component.num_qps;
        ie->ie_cm_id_cache = calloc(ie->ie_cm_id_cache_size,
                                    sizeof(ibcm_base_cm_id_t*));
        if (NULL == ie->ie_cm_id_cache) {
            rej_reason = REJ_PASSIVE_SIDE_ERROR;
            BTL_ERROR(("malloc failed -- reject"));
            goto reject;
        }
    }

    /* Move QP to RTS.  Note that it is important to do this now
       (vs. doing it when the RTU is received, as shown in the IBCM
       example cmpost.c application) because of a nasty race condition
       due to OMPI's use of multiple QPs for each endpoint.

       Consider a trivial MPI app that calls MPI_INIT, MPI_BARRIER,
       MPI_FINALIZE.  In this case, MPI process A will initiate a
       connection to MPI process B.  All BSRQ QPs will be opened A->B
       (assume that A->B is the "right" direction to initiate).  A
       will then send 1 message to B, probably across the "smallest"
       QP, and then close all 4 QPs.

       It is possible that the QP disconnect requests will arrive at B
       before B has transitioned all of its endpoint QPs to RTS.
       Remember that the disconnect requests will be handled by the
       IBCM kernel module, which will move the QP's ID to a state
       where it is illegal to transition to RTS.  Hence, when MPI
       process B finally processes the RTU / tries to move the QP to
       RTS: ka-boom (note that we saw this happen in practice; this is
       not a theoretical race condition).

       So just avoid the whole mess by transitioning to RTS now, when
       we can guarantee that it's legal to do so. */
    if (OMPI_SUCCESS != (rc = qp_to_rts(qp_index, event->cm_id, endpoint))) {
        BTL_VERBOSE(("failed move to RTS (index %d)", qp_index));
        rej_reason = REJ_PASSIVE_SIDE_ERROR;
        goto reject;
    }

    /* Save the CM ID on the endpoint for destruction later */
    ie->ie_cm_id_cache[qp_index] = OBJ_NEW(ibcm_base_cm_id_t);
    if (NULL == ie->ie_cm_id_cache[qp_index]) {
        BTL_ERROR(("malloc failed"));
        rej_reason = REJ_PASSIVE_SIDE_ERROR;
        goto reject;
    }
    ie->ie_cm_id_cache[qp_index]->cm_id = event->cm_id;

    /* Send reply */
    rep = OBJ_NEW(ibcm_reply_t);
    if (NULL == req) {
        rej_reason = REJ_PASSIVE_SIDE_ERROR;
        BTL_ERROR(("malloc failed!"));
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        BTL_ERROR(("OBJ_NEW failed -- reject"));
        goto reject;
    }
    rep->super.cm_id = event->cm_id;
    rep->endpoint = endpoint;

    rep->cm_rep.qp_num = endpoint->qps[qp_index].qp->lcl_qp->qp_num;
    rep->cm_rep.srq = BTL_OPENIB_QP_TYPE_SRQ(qp_index);
    rep->cm_rep.starting_psn = endpoint->qps[qp_index].qp->lcl_psn;
    BTL_VERBOSE(("setting reply psn %d", 
                 rep->cm_rep.starting_psn));
    rep->cm_rep.responder_resources = req->responder_resources;
    rep->cm_rep.initiator_depth = req->initiator_depth;
    rep->cm_rep.target_ack_delay = 20;
    rep->cm_rep.flow_control = req->flow_control;
    rep->cm_rep.rnr_retry_count = req->rnr_retry_count;
    
    rep->private_data.irepd_request = active_private_data->ireqd_request;
    rep->private_data.irepd_reply = rep;
    rep->private_data.irepd_qp_index = qp_index;
    rep->private_data.irepd_ep_index = endpoint->index;

    if (0 != (rc = ib_cm_send_rep(event->cm_id, &(rep->cm_rep)))) {
        BTL_ERROR(("failed to send reply for qp index %d", qp_index));
        OBJ_RELEASE(rep);
        rej_reason = REJ_PASSIVE_SIDE_ERROR;
        goto reject;
    }
    opal_list_append(&ibcm_pending_replies, &(rep->super.super));
    
    TIMER_STOP(REQUEST_RECEIVED);
    BTL_VERBOSE(("sent reply for qp index %d", qp_index));
    return OMPI_SUCCESS;

 reject:
    /* Reject the request */
    BTL_VERBOSE(("rejecting request"));
    ib_cm_send_rej(event->cm_id, IB_CM_REJ_CONSUMER_DEFINED, 
                   &rej_reason, sizeof(rej_reason),
                   event->private_data, sizeof(ibcm_req_data_t));
    
    /* If we rejected because of the wrong direction, then initiate a
       connection going the other direction. */
    if (REJ_WRONG_DIRECTION == rej_reason) {
        callback_start_connect_data_t *cbdata = malloc(sizeof(*cbdata));
        if (NULL != cbdata) {
            cbdata->cscd_cpc = 
                (ompi_btl_openib_connect_base_module_t *) imodule;
            cbdata->cscd_endpoint = endpoint;
            BTL_VERBOSE(("starting connect in other direction"));
            ompi_btl_openib_fd_run_in_main(callback_start_connect, cbdata);
            
            TIMER_STOP(REQUEST_RECEIVED);
            return OMPI_SUCCESS;
        }
    }

    /* Communicate to the upper layer that the connection on this
       endpoint has failed */
    TIMER_STOP(REQUEST_RECEIVED);
    ompi_btl_openib_fd_run_in_main(mca_btl_openib_endpoint_invoke_error,
                                   endpoint);
    return rc;
}
 
/*
 * Callback (from main thread) when the endpoint has been connected
 */
static void *callback_set_endpoint_cpc_complete(void *context)
{
    mca_btl_openib_endpoint_t *endpoint = (mca_btl_openib_endpoint_t*) context;

    BTL_VERBOSE(("calling endpoint_cpc_complete"));
    mca_btl_openib_endpoint_cpc_complete(endpoint);
    BTL_VERBOSE(("*** CONNECTED endpoint_cpc_complete done!"));

    return NULL;
}

/*
 * Helper function to find a cached CM ID in a list
 */ 
static ibcm_base_cm_id_t *find_cm_id(struct ib_cm_id *cm_id, 
                                     opal_list_t *list)
{
   opal_list_item_t *item;
   ibcm_base_cm_id_t *req;

   for (item = opal_list_get_first(list);
         item != opal_list_get_end(list);
         item = opal_list_get_next(item)) {
        req = (ibcm_base_cm_id_t*) item;
        if (req->cm_id == cm_id) {
            return req;
        }
    }

   return NULL;
}

/*
 * Active has received the reply from the passive.
 */
static int reply_received(ibcm_listen_cm_id_t *cmh, struct ib_cm_event *event)
{
    int rc;
    ibcm_rep_data_t *p = (ibcm_rep_data_t*) event->private_data;
    ibcm_request_t *request = p->irepd_request;
    ibcm_reply_t *reply = p->irepd_reply;
    mca_btl_openib_endpoint_t *endpoint = request->endpoint;
    ibcm_endpoint_t *ie;
    ibcm_rtu_data_t rtu_data;

    TIMER_START(REPLY_RECEIVED);
    BTL_VERBOSE(("got reply! (qp index %d) endpoint: %p",
                 p->irepd_qp_index, (void*) endpoint));

    ie = (ibcm_endpoint_t*) endpoint->endpoint_local_cpc_data;
    endpoint->rem_info.rem_qps[p->irepd_qp_index].rem_psn = 
        event->param.rep_rcvd.starting_psn;
    endpoint->rem_info.rem_index = p->irepd_ep_index;

    /* Move the QP to RTR.  Note that you have to do this *before*
       posting receives. */
    if (OMPI_SUCCESS != (rc = qp_to_rtr(p->irepd_qp_index,
                                        event->cm_id, endpoint))) {
        BTL_VERBOSE(("failed move to RTR"));
        goto error;
    }

    /* Move the QP to RTS */
    if (OMPI_SUCCESS != (rc = qp_to_rts(p->irepd_qp_index,
                                        event->cm_id, endpoint))) {
        BTL_VERBOSE(("failed move to RTS"));
        goto error;
    }

    /* Now that all the qp's are created locally, post a single
       receive buffer on the smallest QP for the CTS protocol.  Be
       sure to only do this for the *first* reply that is received on
       an endpoint.  For all other replies received on an endpoint, we
       can safely assume that the CTS receive buffer has already been
       posted. */
    if (!ie->ie_recv_buffers_posted) {
        struct ibv_recv_wr *bad_wr, *wr;

        OPAL_OUTPUT((-1, "REPLY posting CTS recv buffer"));
        assert(NULL != endpoint->endpoint_cts_frag.super.super.base.super.ptr);
        wr = &(endpoint->endpoint_cts_frag.rd_desc);
        assert(NULL != wr);
        wr->next = NULL;

        if (0 != ibv_post_recv(endpoint->qps[mca_btl_openib_component.credits_qp].qp->lcl_qp, wr, &bad_wr)) {
            /* JMS */
            BTL_VERBOSE(("failed to post CTS recv buffer"));
            goto error;
        }

        ie->ie_recv_buffers_posted = true;
    }

    /* Send the RTU */
    rtu_data.irtud_reply = reply;
    rtu_data.irtud_qp_index = p->irepd_qp_index;
    if (0 != ib_cm_send_rtu(event->cm_id, &rtu_data, sizeof(rtu_data))) {
        BTL_VERBOSE(("failed to send RTU"));
        rc = OMPI_ERR_IN_ERRNO;
        goto error;
    }

    /* Remove the pending request because we won't need to handle
       errors for it */
    BTL_VERBOSE(("reply received cm id %p -- original cached req %p",
                 (void*)cmh->listen_cm_id, (void*)request));
    opal_list_remove_item(&ibcm_pending_requests, &(request->super.super));
    OBJ_RELEASE(request);

    /* Have all the QP's been connected?  If so, tell the main BTL
       that we're done. */
    if (0 == --(ie->ie_qps_to_connect)) {
        BTL_VERBOSE(("REPLY telling main BTL we're connected"));
        ompi_btl_openib_fd_run_in_main(callback_set_endpoint_cpc_complete, endpoint);
    }

    TIMER_STOP(REPLY_RECEIVED);
    return OMPI_SUCCESS;

 error:
    /* Communicate to the upper layer that the connection on this
       endpoint has failed */
    ompi_btl_openib_fd_run_in_main(mca_btl_openib_endpoint_invoke_error,
                                   endpoint);
    return rc;
}

/*
 * Passive has received "ready to use" from the active
 */
static int ready_to_use_received(ibcm_listen_cm_id_t *h,
                                 struct ib_cm_event *event)
{
    ibcm_rtu_data_t *p = (ibcm_rtu_data_t*) event->private_data;
    ibcm_reply_t *reply = p->irtud_reply;
    mca_btl_openib_endpoint_t *endpoint = reply->endpoint;
    ibcm_endpoint_t *ie = (ibcm_endpoint_t*) endpoint->endpoint_local_cpc_data;

    TIMER_START(RTU_RECEIVED);
    BTL_VERBOSE(("got RTU! (index %d)", p->irtud_qp_index));

    /* Remove the pending reply because we won't need to handle errors
       for it */
    BTL_VERBOSE(("RTU received cm id %p -- original cached reply %p",
                 (void*)event->cm_id, (void*)reply));
    opal_list_remove_item(&ibcm_pending_replies, &(reply->super.super));
    OBJ_RELEASE(reply);

    /* Have all the QP's been connected?  If so, tell the main BTL
       that we're done. */
    if (0 == --(ie->ie_qps_to_connect)) {
        BTL_VERBOSE(("RTU telling main BTL we're connected"));
        ompi_btl_openib_fd_run_in_main(callback_set_endpoint_cpc_complete, endpoint);
    }

    BTL_VERBOSE(("all done"));
    TIMER_STOP(RTU_RECEIVED);
    return OMPI_SUCCESS;
}


static int reject_received(ibcm_listen_cm_id_t *cmh, struct ib_cm_event *event)
{
    enum ib_cm_rej_reason reason = event->param.rej_rcvd.reason;
    ibcm_reject_reason_t *rej_reason = 
        (ibcm_reject_reason_t *) event->param.rej_rcvd.ari;

    TIMER_START(REJECT_RECEIVED);
    BTL_VERBOSE(("reject received: reason %d, official reason: %d",
                 reason, *rej_reason));

    /* Determine if we expected this reject or not */

    if (IB_CM_REJ_CONSUMER_DEFINED == reason &&
        REJ_WRONG_DIRECTION == *rej_reason) {
        ibcm_req_data_t *my_private_data =
            (ibcm_req_data_t*) event->private_data;
        ibcm_request_t *request = my_private_data->ireqd_request;
        mca_btl_openib_endpoint_t *endpoint = request->endpoint;
        ibcm_endpoint_t *ie = (ibcm_endpoint_t*) 
            endpoint->endpoint_local_cpc_data;

        BTL_VERBOSE(("got WRONG_DIRECTION reject, endpoint: %p, pid %d, ep_index %d, qp_index %d",
                    (void*)my_private_data->ireqd_request->endpoint,
                    my_private_data->ireqd_pid,
                    my_private_data->ireqd_ep_index,
                     my_private_data->ireqd_qp_index));
        if (NULL == ie->ie_bogus_qp) {
            BTL_VERBOSE(("WRONG_DIRECTION unexpected!"));
        } else {

            /* Remove from the global pending_requests list because we
               no longer need to handle errors for it */
            BTL_VERBOSE(("reply received cm id %p -- original cached req %p",
                        (void*)cmh->listen_cm_id, 
                         (void*)request));
            opal_list_remove_item(&ibcm_pending_requests, 
                                  &(request->super.super));

            /* We ack the event and then destroy the CM ID (you *must*
               ACK it first -- the destroy will block until all
               outstand events on this ID are complete) */
            BTL_VERBOSE(("destroying bogus CM ID: %p",
                         (void*)request->super.cm_id));
            ib_cm_ack_event(event);
            ib_cm_destroy_id(request->super.cm_id);

            /* Destroy the QP */
            BTL_VERBOSE(("destroying bogus qp"));
            ibv_destroy_qp(ie->ie_bogus_qp);
            ie->ie_bogus_qp = NULL;

            /* Free the object */
            OBJ_RELEASE(request);
        }

        TIMER_STOP(REJECT_RECEIVED);
        return OMPI_SUCCESS;
    }

    BTL_VERBOSE(("got unexpected reject type: %d",
                 reason));
    /* Communicate to the upper layer that the connection on this
       endpoint has failed */
    ompi_btl_openib_fd_run_in_main(mca_btl_openib_endpoint_invoke_error, NULL);
    return OMPI_ERR_NOT_FOUND;
}

static int request_error(ibcm_listen_cm_id_t *cmh, struct ib_cm_event *event)
{
    mca_btl_openib_endpoint_t *endpoint = NULL;

    BTL_VERBOSE(("request error!"));

    if (IBV_WC_RESP_TIMEOUT_ERR != event->param.send_status) {
        orte_show_help("help-mpi-btl-openib-cpc-ibcm.txt",
                       "unhandled error", true,
                       "request", orte_process_info.nodename, 
                       event->param.send_status);
    } else {
        ibcm_request_t *req;
        BTL_ERROR(("Got timeout in IBCM request (CM ID: %p)", 
                   (void*)event->cm_id));
        req = (ibcm_request_t*) find_cm_id(event->cm_id, 
                                           &ibcm_pending_requests);
        if (NULL == req) {
            orte_show_help("help-mpi-btl-openib-cpc-ibcm.txt",
                           "timeout not found", true,
                           "request", orte_process_info.nodename);
        } else {
            endpoint = req->endpoint;
        }
    }

    /* Communicate to the upper layer that the connection on this
       endpoint has failed */
    ompi_btl_openib_fd_run_in_main(mca_btl_openib_endpoint_invoke_error, 
                                   endpoint);
    return OMPI_SUCCESS;
}


static int reply_error(ibcm_listen_cm_id_t *cmh, struct ib_cm_event *event)
{
    mca_btl_openib_endpoint_t *endpoint = NULL;

    if (IBV_WC_RESP_TIMEOUT_ERR != event->param.send_status) {
        orte_show_help("help-mpi-btl-openib-cpc-ibcm.txt",
                       "unhandled error", true,
                       "reply", orte_process_info.nodename, 
                       event->param.send_status);
    } else {
        ibcm_reply_t *rep;
        BTL_ERROR(("Got timeout in IBCM reply (id: %p)",
                   (void*)event->cm_id));
        rep = (ibcm_reply_t*) find_cm_id(event->cm_id, 
                                         &ibcm_pending_replies);
        if (NULL == rep) {
            orte_show_help("help-mpi-btl-openib-cpc-ibcm.txt",
                           "timeout not found", true,
                           "reply", orte_process_info.nodename);
        } else {
            endpoint = rep->endpoint;
        }
    }

    /* Communicate to the upper layer that the connection on this
       endpoint has failed */
    ompi_btl_openib_fd_run_in_main(mca_btl_openib_endpoint_invoke_error, 
                                   endpoint);
    return OMPI_SUCCESS;
}


static void *ibcm_event_dispatch(int fd, int flags, void *context)
{
    bool want_ack;
    int rc;
    ibcm_listen_cm_id_t *cmh = (ibcm_listen_cm_id_t*) context;
    struct ib_cm_event *e = NULL;

    OPAL_OUTPUT((-1, "ibcm dispatch: on device 0x%" PRIx64", fd %d", 
                 (uint64_t) cmh->cm_device, fd));
    TIMER_START(CM_GET_EVENT);
    /* Blocks until next event, which should be immediately (because
       we shouldn't call this dispatch function unless there's
       something ready to read) */
    rc = ib_cm_get_event(cmh->cm_device, &e);
    TIMER_STOP(CM_GET_EVENT);
    if (-ENODATA == rc) {
        OPAL_OUTPUT((-1, "ibcm dispatch: GOT NOT DATA!"));
        return NULL;
    }
    while (-1 == rc && EAGAIN == errno) {
        OPAL_OUTPUT((-1, "ibcm dispatch: GOT EAGAIN!"));
        /* Try again */
        rc = ib_cm_get_event(cmh->cm_device, &e);
    }
    if (0 == rc && NULL != e) {
        want_ack = true;
        switch (e->event) {
        case IB_CM_REQ_RECEIVED:
            OPAL_OUTPUT((-1, "ibcm dispatch: request received on fd %d", fd));
            /* Incoming request */
            rc = request_received(cmh, e);
            break;
            
        case IB_CM_REP_RECEIVED:
            OPAL_OUTPUT((-1, "ibcm dispatch: reply received on fd %d", fd));
            /* Reply received */
            rc = reply_received(cmh, e);
            break;
            
        case IB_CM_RTU_RECEIVED:
            OPAL_OUTPUT((-1, "ibcm dispatch: RTU received on fd %d", fd));
            /* Ready to use! */
            rc = ready_to_use_received(cmh, e);
            break;
                        
        case IB_CM_REJ_RECEIVED:
            OPAL_OUTPUT((-1, "ibcm dispatch: reject received on fd %d", fd));
            /* Rejected connection */
            rc = reject_received(cmh, e);
            /* reject_received() called ib_cm_ack_event so that the CM
               ID could be freed */
            want_ack = false;
            break;
            
        case IB_CM_REQ_ERROR:
            OPAL_OUTPUT((-1, "ibcm dispatch: request error received on fd %d", fd));
            /* Request error */
            rc = request_error(cmh, e);
            break;
            
        case IB_CM_REP_ERROR:
            OPAL_OUTPUT((-1, "ibcm dispatch: reply error received on fd %d", fd));
            /* Reply error */
            rc = reply_error(cmh, e);
            break;
            
        case IB_CM_DREQ_RECEIVED:
        case IB_CM_DREP_RECEIVED:
        case IB_CM_DREQ_ERROR:
            OPAL_OUTPUT((-1, "ibcm dispatch: %s received on fd %d",
                        (IB_CM_DREQ_RECEIVED == e->event) ? "disconnect request" :
                        (IB_CM_DREP_RECEIVED == e->event) ? "disconnect reply" :
                        "disconnect request error", fd));
            /* We don't care */
            rc = OMPI_SUCCESS;
            break;
            
        default:
            /* This would be odd */
            OPAL_OUTPUT((-1, "ibcm dispatch: unhandled event received on fd %d", fd));
            rc = OMPI_ERR_NOT_FOUND;
            break;
        }

        if (want_ack) {
            TIMER_START(CM_ACK_EVENT);
            ib_cm_ack_event(e);
            TIMER_STOP(CM_ACK_EVENT);
        }

        if (OMPI_SUCCESS != rc) {
            BTL_VERBOSE(("An error occurred handling an IBCM event.  Bad things are likely to happen."));
            /* If we needed to abort (i.e., call the BTL error_cb()
               function), the dispatch function would have done that
               already */
        }
    } else {
        OPAL_OUTPUT((-1, "Got weird value back from ib_cm_get_event: %d", rc));
    }

    return NULL;
}
