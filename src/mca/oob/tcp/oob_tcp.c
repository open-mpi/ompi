/* 
 * $HEADER$
 */

#include "ompi_config.h"
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "util/output.h"
#include "util/if.h"
#include "mca/oob/tcp/oob_tcp.h"
#include "mca/ns/ns.h"
#include "mca/gpr/base/base.h"
#include "mca/gpr/gpr.h"
#include "mca/pcmclient/pcmclient.h"
#include "mca/pcmclient/base/base.h"

                                                                  
/*
 * Data structure for accepting connections.
 */
                                                                                                      
struct mca_oob_tcp_event_t {
    ompi_list_item_t item;
    ompi_event_t event;
};
typedef struct mca_oob_tcp_event_t mca_oob_tcp_event_t;
                                                                                                      
static void mca_oob_tcp_event_construct(mca_oob_tcp_event_t* event)
{
    OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    ompi_list_append(&mca_oob_tcp_component.tcp_events, &event->item);
    OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
}
                                                                                                      
static void mca_oob_tcp_event_destruct(mca_oob_tcp_event_t* event)
{
    OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    ompi_list_remove_item(&mca_oob_tcp_component.tcp_events, &event->item);
    OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
}
                                                                                                      
OBJ_CLASS_INSTANCE(
    mca_oob_tcp_event_t,
    ompi_list_item_t,
    mca_oob_tcp_event_construct,
    mca_oob_tcp_event_destruct);
                                                                                                      
/*
 * Local utility functions
 */

static int  mca_oob_tcp_create_listen(void);
static void mca_oob_tcp_recv_handler(int sd, short flags, void* user);
static void mca_oob_tcp_accept(void);


struct mca_oob_tcp_subscription_t {
    ompi_list_item_t item;
    mca_ns_base_jobid_t jobid;
};
typedef struct mca_oob_tcp_subscription_t mca_oob_tcp_subscription_t;

OBJ_CLASS_INSTANCE(
    mca_oob_tcp_subscription_t,
    ompi_list_item_t,
    NULL,
    NULL);



/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_oob_tcp_component_t mca_oob_tcp_component = {
  {
    {
        MCA_OOB_BASE_VERSION_1_0_0,
        "tcp", /* MCA module name */
        1,  /* MCA component major version */
        0,  /* MCA component minor version */
        0,  /* MCA component release version */
        mca_oob_tcp_component_open,  /* component open */
        mca_oob_tcp_component_close /* component close */
    },
    {
        false /* checkpoint / restart */
    },
    mca_oob_tcp_component_init 
  }
};

static mca_oob_t mca_oob_tcp = {
    mca_oob_tcp_get_addr,
    mca_oob_tcp_set_addr,
    mca_oob_tcp_ping,
    mca_oob_tcp_send,
    mca_oob_tcp_recv,
    mca_oob_tcp_send_nb,
    mca_oob_tcp_recv_nb,
    mca_oob_tcp_recv_cancel,
    mca_oob_tcp_init,
    mca_oob_tcp_fini,
};


/*
 * Utility function to register/lookup module parameters.
 */

static inline int mca_oob_tcp_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("oob","tcp",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}


static inline char* mca_oob_tcp_param_register_str(
    const char* param_name,
    const char* default_value)
{
    int id = mca_base_param_register_string("oob","tcp",param_name,NULL,default_value);
    char* param_value = NULL;
    mca_base_param_lookup_string(id,&param_value);
    return param_value;
}


/*
 * Initialize global variables used w/in this module.
 */
int mca_oob_tcp_component_open(void)
{
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_subscriptions, ompi_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_peer_list,     ompi_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_peer_tree,     ompi_rb_tree_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_peer_names,    ompi_rb_tree_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_peer_free,     ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_msgs,          ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_lock,          ompi_mutex_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_events,        ompi_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_msg_post,      ompi_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_msg_recv,      ompi_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_match_lock,    ompi_mutex_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_match_cond,    ompi_condition_t);

    /* register oob module parameters */
    mca_oob_tcp_component.tcp_peer_limit =
        mca_oob_tcp_param_register_int("peer_limit", -1);
    mca_oob_tcp_component.tcp_peer_retries =
        mca_oob_tcp_param_register_int("peer_retries", 60);
    mca_oob_tcp_component.tcp_debug =
        mca_oob_tcp_param_register_int("debug", 1);

    /* initialize state */
    mca_oob_tcp_component.tcp_listen_sd = -1;
    mca_oob_tcp_component.tcp_match_count = 0;
    return OMPI_SUCCESS;
}


/*
 * Cleanup of global variables used by this module.
 */

int mca_oob_tcp_component_close(void)
{
    /* cleanup resources */
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_peer_list);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_peer_tree);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_peer_free);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_events);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_subscriptions);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_msgs);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_lock);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_msg_post);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_msg_recv);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_match_lock);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_match_cond);
    return OMPI_SUCCESS;
}


/*
 *  Called by mca_oob_tcp_recv_handler() when the TCP listen
 *  socket has pending connection requests. Accept incoming
 *  requests and queue for completion of the connection handshake.
*/

static void mca_oob_tcp_accept(void)
{
    while(true) {
        ompi_socklen_t addrlen = sizeof(struct sockaddr_in);
        struct sockaddr_in addr;
        mca_oob_tcp_event_t* event;
        int sd;
 
        sd = accept(mca_oob_tcp_component.tcp_listen_sd, (struct sockaddr*)&addr, &addrlen);
        if(sd < 0) {
            if(errno == EINTR)
                continue;
            if(errno != EAGAIN || errno != EWOULDBLOCK)
                ompi_output(0, "mca_oob_tcp_accept: accept() failed with errno %d.", errno);
            return;
        }
                                                                                                                   
        /* wait for receipt of peers process identifier to complete this connection */
        event = OBJ_NEW(mca_oob_tcp_event_t);
        ompi_event_set(&event->event, sd, OMPI_EV_READ, mca_oob_tcp_recv_handler, event);
        ompi_event_add(&event->event, 0);
    }
}

/*
 * Create a listen socket and bind to all interfaces
 */
                                                                                                                   
static int mca_oob_tcp_create_listen(void)
{
    int flags;
    int optval = 1;
    struct sockaddr_in inaddr;
    ompi_socklen_t addrlen;

    /* create a listen socket for incoming connections */
    mca_oob_tcp_component.tcp_listen_sd = socket(AF_INET, SOCK_STREAM, 0);
    if(mca_oob_tcp_component.tcp_listen_sd < 0) {
        ompi_output(0,"mca_oob_tcp_component_init: socket() failed with errno=%d", errno);
        return OMPI_ERROR;
    }
    /* allow port to be re-used - for temporary fixed port numbers */
    if (setsockopt(
        mca_oob_tcp_component.tcp_listen_sd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval)) < 0) {
        ompi_output(0, "mca_oob_tcp_create_listen: setsockopt(SO_REUSEADDR) failed with errno=%d\n", 
            errno);
    }

    memset(&inaddr, 0, sizeof(inaddr));
    inaddr.sin_family = AF_INET;
    inaddr.sin_addr.s_addr = INADDR_ANY;
    inaddr.sin_port = 0;

    if(bind(mca_oob_tcp_component.tcp_listen_sd, (struct sockaddr*)&inaddr, sizeof(inaddr)) < 0) {
        ompi_output(0,"mca_oob_tcp_create_listen: bind() failed with errno=%d", errno);
        return OMPI_ERROR;
    }

    /* resolve system assigned port */
    addrlen = sizeof(struct sockaddr_in);
    if(getsockname(mca_oob_tcp_component.tcp_listen_sd, (struct sockaddr*)&inaddr, &addrlen) < 0) {
        ompi_output(0, "mca_oob_tcp_create_listen: getsockname() failed with errno=%d", errno);
        return OMPI_ERROR;
    }
    mca_oob_tcp_component.tcp_listen_port = inaddr.sin_port;
                                                                                                                   
    /* setup listen backlog to maximum allowed by kernel */
    if(listen(mca_oob_tcp_component.tcp_listen_sd, SOMAXCONN) < 0) {
        ompi_output(0, "mca_oob_tcp_component_init: listen() failed with errno=%d", errno);
        return OMPI_ERROR;
    }
                                                                                                                   
    /* set socket up to be non-blocking, otherwise accept could block */
    if((flags = fcntl(mca_oob_tcp_component.tcp_listen_sd, F_GETFL, 0)) < 0) {
        ompi_output(0, "mca_oob_tcp_component_init: fcntl(F_GETFL) failed with errno=%d", errno);
        return OMPI_ERROR;
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(mca_oob_tcp_component.tcp_listen_sd, F_SETFL, flags) < 0) {
            ompi_output(0, "mca_oob_tcp_component_init: fcntl(F_SETFL) failed with errno=%d", errno);
            return OMPI_ERROR;
        }
    }
                                                                                                                   
    /* register listen port */
    ompi_event_set(
        &mca_oob_tcp_component.tcp_recv_event,
        mca_oob_tcp_component.tcp_listen_sd,
        OMPI_EV_READ|OMPI_EV_PERSIST,
        mca_oob_tcp_recv_handler,
        0);
    ompi_event_add(&mca_oob_tcp_component.tcp_recv_event, 0);
    return OMPI_SUCCESS;
}


/*
 * Event callback when there is data available on the registered
 * socket to recv.
 */

static void mca_oob_tcp_recv_handler(int sd, short flags, void* user)
{
    ompi_process_name_t guid[2];
    mca_oob_tcp_peer_t* peer;
    int rc;
    mca_oob_tcp_event_t* event = user;

    /* accept new connections on the listen socket */
    if(mca_oob_tcp_component.tcp_listen_sd == sd) {
        mca_oob_tcp_accept();
        return;
    }
    OBJ_RELEASE(event);

    /* recv the process identifier */
    while((rc = recv(sd, guid, sizeof(guid), 0)) != sizeof(guid)) {
        if(rc >= 0) {
            if(mca_oob_tcp_component.tcp_debug > 3) {
                ompi_output(0, "[%d,%d,%d] mca_oob_tcp_recv_handler: peer closed connection",
                    OMPI_NAME_ARGS(mca_oob_name_self));
            }
            close(sd);
            return;
        }
        if(errno != EINTR) {
            ompi_output(0, "[%d,%d,%d] mca_oob_tcp_recv_handler: recv() failed with errno=%d\n", 
                OMPI_NAME_ARGS(mca_oob_name_self), errno);
            close(sd);
            return;
        }
    }
    OMPI_PROCESS_NAME_NTOH(guid[0]);
    OMPI_PROCESS_NAME_NTOH(guid[1]);

    /* now set socket up to be non-blocking */
    if((flags = fcntl(sd, F_GETFL, 0)) < 0) {
        ompi_output(0, "[%d,%d,%d] mca_oob_tcp_recv_handler: fcntl(F_GETFL) failed with errno=%d", 
                OMPI_NAME_ARGS(mca_oob_name_self), errno);
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(sd, F_SETFL, flags) < 0) {
            ompi_output(0, "[%d,%d,%d] mca_oob_tcp_recv_handler: fcntl(F_SETFL) failed with errno=%d", 
                OMPI_NAME_ARGS(mca_oob_name_self), errno);
        }
    }

    /* check for wildcard name - if this is true - we allocate a name from the name server 
     * and return to the peer 
     */
    if(mca_oob_tcp_process_name_compare(guid, MCA_OOB_NAME_ANY) == 0) {
        guid->jobid = ompi_name_server.create_jobid();
        guid->vpid = ompi_name_server.reserve_range(guid->jobid,1);
        ompi_name_server.assign_cellid_to_process(guid);
    }

    /* lookup the corresponding process */
    peer = mca_oob_tcp_peer_lookup(guid);
    if(NULL == peer) {
        ompi_output(0, "[%d,%d,%d] mca_oob_tcp_recv_handler: unable to locate peer",
                OMPI_NAME_ARGS(mca_oob_name_self));
        close(sd);
        return;
    }
    /* is the peer instance willing to accept this connection */
    if(mca_oob_tcp_peer_accept(peer, sd) == false) {
        if(mca_oob_tcp_component.tcp_debug > 1) {
            ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_recv_handler: "
                    "rejected connection from [%d,%d,%d] connection state %d",
                    OMPI_NAME_ARGS(mca_oob_name_self),
                    OMPI_NAME_ARGS(peer->peer_name),
                    OMPI_NAME_ARGS(guid[0]),
                    peer->peer_state);
        }
        close(sd);
        return;
    }
}

/*
 * Component initialization - create a module.
 * (1) initialize static resources
 * (2) create listen socket
 */
mca_oob_t* mca_oob_tcp_component_init(int* priority, bool *allow_multi_user_threads, bool *have_hidden_threads)
{
    *priority = 1;
    *allow_multi_user_threads = true;
    *have_hidden_threads = OMPI_HAVE_THREADS;

    /* are there any interfaces? */
    if(ompi_ifcount() == 0)
        return NULL;

    /* initialize data structures */
    ompi_rb_tree_init(&mca_oob_tcp_component.tcp_peer_tree, (ompi_rb_tree_comp_fn_t)mca_oob_tcp_process_name_compare);
    ompi_rb_tree_init(&mca_oob_tcp_component.tcp_peer_names, (ompi_rb_tree_comp_fn_t)mca_oob_tcp_process_name_compare);

    ompi_free_list_init(&mca_oob_tcp_component.tcp_peer_free,
        sizeof(mca_oob_tcp_peer_t),
        OBJ_CLASS(mca_oob_tcp_peer_t),
        8,  /* initial number */
        mca_oob_tcp_component.tcp_peer_limit, /* maximum number */
        8,  /* increment to grow by */
        NULL); /* use default allocator */

    ompi_free_list_init(&mca_oob_tcp_component.tcp_msgs,
        sizeof(mca_oob_tcp_msg_t),
        OBJ_CLASS(mca_oob_tcp_msg_t),
        8,  /* initial number */
       -1,  /* maximum number */
        8,  /* increment to grow by */
        NULL); /* use default allocator */

    /* intialize event library */
    memset(&mca_oob_tcp_component.tcp_recv_event, 0, sizeof(ompi_event_t));
    memset(&mca_oob_tcp_component.tcp_send_event, 0, sizeof(ompi_event_t));

    /* create a listen socket */
    if(mca_oob_tcp_create_listen() != OMPI_SUCCESS) {
        ompi_output(0, "mca_oob_tcp_init: unable to create listen socket\n");
        return NULL;
    }
    return &mca_oob_tcp;
}


/*
 * Callback from registry on change to subscribed segments.
 */

static void mca_oob_tcp_registry_callback(
    ompi_registry_notify_message_t* msg,
    void* cbdata)
{
    ompi_list_item_t* item;
    if(mca_oob_tcp_component.tcp_debug > 1) {
        ompi_output(0, "[%d,%d,%d] mca_oob_tcp_registry_callback\n",
            OMPI_NAME_ARGS(mca_oob_name_self));
    }

    /* process the callback */
    OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    while((item = ompi_list_remove_first(&msg->data)) != NULL) {

        ompi_registry_value_t* value = (ompi_registry_value_t*)item; 
        ompi_buffer_t buffer;
        mca_oob_tcp_addr_t* addr, *existing;
        mca_oob_tcp_peer_t* peer;

        /* transfer ownership of registry object to buffer and unpack */
        ompi_buffer_init_preallocated(&buffer, value->object, value->object_size);
        value->object = NULL;
        value->object_size = 0;
        addr = mca_oob_tcp_addr_unpack(buffer);
        ompi_buffer_free(buffer);
        if(NULL == addr) {
            ompi_output(0, "[%d,%d,%d] mca_oob_tcp_registry_callback: unable to unpack peer address\n",
                OMPI_NAME_ARGS(mca_oob_name_self));
            OBJ_RELEASE(item);
            continue;
        }

        if(mca_oob_tcp_component.tcp_debug > 1) {
            ompi_output(0, "[%d,%d,%d] mca_oob_tcp_registry_callback: received peer [%d,%d,%d]\n",
                OMPI_NAME_ARGS(mca_oob_name_self),
                OMPI_NAME_ARGS(addr->addr_name));
        }

        /* check for existing cache entry */
        existing = ompi_rb_tree_find(&mca_oob_tcp_component.tcp_peer_names, &addr->addr_name);
        if(NULL != existing) {
            /* TSW - need to update existing entry */
            OBJ_RELEASE(addr);
            continue;
        }

        /* insert into cache and notify peer */
        ompi_rb_tree_insert(&mca_oob_tcp_component.tcp_peer_names, &addr->addr_name, addr);
        peer = ompi_rb_tree_find(&mca_oob_tcp_component.tcp_peer_tree, &addr->addr_name);
        if(NULL != peer)
            mca_oob_tcp_peer_resolved(peer, addr);

        OBJ_RELEASE(item);
    }
    OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
}

/*
 * Attempt to resolve peer name.
 */

int mca_oob_tcp_resolve(mca_oob_tcp_peer_t* peer)
{
     mca_oob_tcp_addr_t* addr;
     mca_oob_tcp_subscription_t* subscription;
     ompi_list_item_t* item;
     char segment[32], *jobid;
     int rc;
  
     /* if the address is already cached - simply return it */
     OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
     addr = ompi_rb_tree_find(&mca_oob_tcp_component.tcp_peer_names, &peer->peer_name);
     if(NULL != addr) {
         OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
         mca_oob_tcp_peer_resolved(peer, addr);
         return OMPI_SUCCESS;
     }

     /* check to see if we have subscribed to this registry segment */
     for( item =  ompi_list_get_first(&mca_oob_tcp_component.tcp_subscriptions);
          item != ompi_list_get_end(&mca_oob_tcp_component.tcp_subscriptions);
          item =  ompi_list_get_next(item)) {
         subscription = (mca_oob_tcp_subscription_t*)item;
         if(subscription->jobid == peer->peer_name.jobid) {
             OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
             return OMPI_SUCCESS;
         }
     }

     /* otherwise - need to subscribe to this registry segment 
      * record the subscription 
     */
     subscription = OBJ_NEW(mca_oob_tcp_subscription_t);
     subscription->jobid = peer->peer_name.jobid;
     ompi_list_append(&mca_oob_tcp_component.tcp_subscriptions, &subscription->item);
     OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);

     /* subscribe */
     jobid = ompi_name_server.get_jobid_string(&peer->peer_name);
     sprintf(segment, "oob-tcp-%s", jobid);
     rc = ompi_registry.subscribe(
         OMPI_REGISTRY_OR,
         OMPI_REGISTRY_NOTIFY_ADD_ENTRY|OMPI_REGISTRY_NOTIFY_DELETE_ENTRY|
         OMPI_REGISTRY_NOTIFY_MODIFICATION|OMPI_REGISTRY_NOTIFY_PRE_EXISTING,
         segment,
         NULL,
         mca_oob_tcp_registry_callback,
         NULL);
     if(rc != OMPI_SUCCESS) {
         ompi_output(0, "mca_oob_tcp_resolve: ompi_registry.subscribe failed with error status: %d\n", rc);
         return rc;
     }
     free(jobid);
     return OMPI_SUCCESS;
}


/*
 * Setup contact information in the registry.
 */
int mca_oob_tcp_init(void)
{
    char *keys[2], *jobid;
    void *addr;
    int32_t size;
    char segment[32];
    ompi_buffer_t buffer;
    ompi_process_name_t* peers;
    mca_oob_tcp_subscription_t* subscription;
    size_t npeers;
    int rc;
    ompi_list_item_t* item;

    /* iterate through the open connections and send an ident message to all peers -
     * note that we initially come up w/out knowing our process name - and are assigned
     * a temporary name by our peer. once we have determined our real name - we send it
     * to the peer.
    */
    OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    for(item =  ompi_list_get_first(&mca_oob_tcp_component.tcp_peer_list);
        item != ompi_list_get_end(&mca_oob_tcp_component.tcp_peer_list);
        item =  ompi_list_get_next(item)) {
        mca_oob_tcp_peer_t* peer = (mca_oob_tcp_peer_t*)item;
        mca_oob_tcp_peer_send_ident(peer);
    }

    rc = mca_pcmclient.pcmclient_get_peers(&peers, &npeers);
    if(rc != OMPI_SUCCESS) {
        OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
        return rc;
    }

    jobid = ompi_name_server.get_jobid_string(&mca_oob_name_self);
    sprintf(segment, "oob-tcp-%s", jobid);
    if(mca_oob_tcp_component.tcp_debug > 1) {
        ompi_output(0, "[%d,%d,%d] mca_oob_tcp_init: calling ompi_registry.synchro(%s,%d)\n", 
            OMPI_NAME_ARGS(mca_oob_name_self),
            segment,
            npeers);
    }

    /* register synchro callback to receive notification when all processes have registered */
    subscription = OBJ_NEW(mca_oob_tcp_subscription_t);
    subscription->jobid = mca_oob_name_self.jobid;
    ompi_list_append(&mca_oob_tcp_component.tcp_subscriptions, &subscription->item);
    OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);

    rc = ompi_registry.synchro(
        OMPI_REGISTRY_OR,
        OMPI_REGISTRY_SYNCHRO_MODE_ASCENDING|OMPI_REGISTRY_SYNCHRO_MODE_ONE_SHOT,
        segment,
        NULL,
        npeers,
        mca_oob_tcp_registry_callback,
        NULL);
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "mca_oob_tcp_init: registry synchro failed with error code %d.", rc);
        return rc;
    }

    /* put our contact info in registry */
    keys[0] = ompi_name_server.get_proc_name_string(&mca_oob_name_self);
    keys[1] = NULL;

    if(mca_oob_tcp_component.tcp_debug > 1) {
        ompi_output(0, "[%d,%d,%d] mca_oob_tcp_init: calling ompi_registry.put(%s,%s)\n", 
            OMPI_NAME_ARGS(mca_oob_name_self),
            segment,
            keys[0]);
    }

    ompi_buffer_init(&buffer, 128);
    mca_oob_tcp_addr_pack(buffer);
    ompi_buffer_get(buffer, &addr, &size);
    rc = ompi_registry.put(OMPI_REGISTRY_OVERWRITE, segment, keys, addr, size);
    ompi_buffer_free(buffer);
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "[%d,%d,%d] mca_oob_tcp_init: registry put failed with error code %d.",
            OMPI_NAME_ARGS(mca_oob_name_self), rc);
        return rc;
    }
    free(jobid);
    free(keys[0]);
    return OMPI_SUCCESS;
}

/*
 * Module cleanup.
 */
int mca_oob_tcp_fini(void)
{
    ompi_list_item_t *item;
    OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    ompi_event_disable(); /* disable event processing */

    /* close listen socket */
    if (mca_oob_tcp_component.tcp_listen_sd >= 0) {
        ompi_event_del(&mca_oob_tcp_component.tcp_recv_event); 
        close(mca_oob_tcp_component.tcp_listen_sd);
        mca_oob_tcp_component.tcp_listen_sd = -1;
    }

    /* cleanup all peers */
    for(item = ompi_list_remove_first(&mca_oob_tcp_component.tcp_peer_list);
        item != NULL;
        item = ompi_list_remove_first(&mca_oob_tcp_component.tcp_peer_list)) {
        mca_oob_tcp_peer_t* peer = (mca_oob_tcp_peer_t*)item;
        MCA_OOB_TCP_PEER_RETURN(peer);
    }

    /* delete any pending events */
    for(item =  ompi_list_remove_first(&mca_oob_tcp_component.tcp_events);
        item != NULL;
        item =  ompi_list_remove_first(&mca_oob_tcp_component.tcp_events)) {
        mca_oob_tcp_event_t* event = (mca_oob_tcp_event_t*)item;
        ompi_event_del(&event->event);
        OBJ_RELEASE(event);
    }

    ompi_event_enable();
    OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
    return OMPI_SUCCESS;
}


/*
* Compare two process names for equality.
*
* @param  n1  Process name 1.
* @param  n2  Process name 2.
* @return     (-1 for n1<n2 0 for equality, 1 for n1>n2)
*
* Note that the definition of < or > is somewhat arbitrary -
* just needs to be consistently applied to maintain an ordering
* when process names are used as indices.
*/


int mca_oob_tcp_process_name_compare(const ompi_process_name_t* n1, const ompi_process_name_t* n2)
{
   if(n1->cellid < n2->cellid)
       return -1;
   else if(n1->cellid > n2->cellid)
       return 1;
   else if(n1->jobid < n2->jobid)
       return -1;
   else if(n1->jobid > n2->jobid)
       return 1;
   else if(n1->vpid < n2->vpid)
       return -1;
   else if(n1->vpid > n2->vpid)
       return 1;
   return(0);
}


/*
* Return local process address as a URI string.
*/

char* mca_oob_tcp_get_addr(void)
{
    int i;
    char *contact_info = malloc((ompi_ifcount()+1) * 32);
    char *ptr = contact_info;
    *ptr = 0;

    for(i=ompi_ifbegin(); i>0; i=ompi_ifnext(i)) {
        struct sockaddr_in addr;
        ompi_ifindextoaddr(i, (struct sockaddr*)&addr, sizeof(addr));
        if(ompi_ifcount() > 1 && addr.sin_addr.s_addr == inet_addr("127.0.0.1"))
            continue;
        if(ptr != contact_info) {
            ptr += sprintf(ptr, ";");
        }
        ptr += sprintf(ptr, "tcp://%s:%d", inet_ntoa(addr.sin_addr), ntohs(mca_oob_tcp_component.tcp_listen_port));
    }
    return contact_info;
}

/*
* Parse a URI string into an IP address and port number.
*/

int mca_oob_tcp_parse_uri(const char* uri, struct sockaddr_in* inaddr)
{
    char* tmp = strdup(uri);
    char* ptr = tmp + 6;
    char* addr = ptr;
    char* port;
    if(strncmp(tmp, "tcp://", 6) != 0) {
        free(tmp);
        return OMPI_ERR_BAD_PARAM;
    }

    ptr = strchr(addr, ':');
    if(NULL == ptr) {
        free(tmp);
        return OMPI_ERR_BAD_PARAM;
    }

    *ptr = '\0';
    ptr++;
    port = ptr;

    memset(inaddr, 0, sizeof(inaddr));
    inaddr->sin_family = AF_INET;
    inaddr->sin_addr.s_addr = inet_addr(addr);
    if(inaddr->sin_addr.s_addr == INADDR_ANY) {
        free(tmp);
        return OMPI_ERR_BAD_PARAM;
    }
    inaddr->sin_port = htons(atoi(port));
    free(tmp);
    return OMPI_SUCCESS;
}


/*
 * Setup address in the cache. Note that this could be called multiple
 * times if a given destination exports multiple addresses.
 */

int mca_oob_tcp_set_addr(const ompi_process_name_t* name, const char* uri)
{
    struct sockaddr_in inaddr;
    mca_oob_tcp_addr_t* addr;
    int rc;
    if((rc = mca_oob_tcp_parse_uri(uri,&inaddr)) != OMPI_SUCCESS)
        return rc;

    OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    addr = (mca_oob_tcp_addr_t*)ompi_rb_tree_find(&mca_oob_tcp_component.tcp_peer_names, (ompi_process_name_t*)name);
    if(NULL == addr) {
        addr = OBJ_NEW(mca_oob_tcp_addr_t);
        addr->addr_name = *name;
        ompi_rb_tree_insert(&mca_oob_tcp_component.tcp_peer_names, &addr->addr_name, addr);
    }
    rc = mca_oob_tcp_addr_insert(addr, &inaddr);
    OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
    return rc;
}

