/* 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/orte_types.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <fcntl.h>
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#include "include/ompi_socket_errno.h"
#include "util/output.h"
#include "util/if.h"
#include "class/ompi_proc_table.h"
#include "mca/oob/tcp/oob_tcp.h"
#include "mca/errmgr/errmgr.h"
#include "mca/ns/ns.h"
#include "mca/gpr/gpr.h"

#define IMPORTANT_WINDOWS_COMMENT() \
            /* In windows, many of the socket functions return an EWOULDBLOCK instead of \
               things like EAGAIN, EINPROGRESS, etc. It has been verified that this will \
               not conflict with other error codes that are returned by these functions \
               under UNIX/Linux environments */
                                                                  
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
    orte_jobid_t jobid;
    orte_gpr_notify_id_t subid;
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
    mca_oob_xcast
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
#ifdef WIN32
    WSADATA win_sock_data;
    if (WSAStartup(MAKEWORD(2,2), &win_sock_data) != 0) {
        ompi_output (0, "mca_oob_tcp_component_init: failed to initialise windows sockets: error %d\n", WSAGetLastError());
        return OMPI_ERROR;
    }
#endif
    
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_subscriptions, ompi_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_peer_list,     ompi_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_peers,     ompi_hash_table_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_peer_names,    ompi_hash_table_t);
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
    mca_oob_tcp_component.tcp_include =
        mca_oob_tcp_param_register_str("include", NULL);
    mca_oob_tcp_component.tcp_exclude =
        mca_oob_tcp_param_register_str("exclude", NULL);

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
#ifdef WIN32
    WSACleanup();
#endif
    
    /* cleanup resources */
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_peer_list);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_peers);
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
            IMPORTANT_WINDOWS_COMMENT();
            if(ompi_socket_errno == EINTR)
                continue;
            if(ompi_socket_errno != EAGAIN || ompi_socket_errno != EWOULDBLOCK)
                ompi_output(0, "mca_oob_tcp_accept: accept() failed with errno %d.", ompi_socket_errno);
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
        ompi_output(0,"mca_oob_tcp_component_init: socket() failed with errno=%d", ompi_socket_errno);
        return OMPI_ERROR;
    }
    /* allow port to be re-used - for temporary fixed port numbers */
    if (setsockopt(
        mca_oob_tcp_component.tcp_listen_sd, SOL_SOCKET, SO_REUSEADDR, (char *)&optval, sizeof(optval)) < 0) {
        ompi_output(0, "mca_oob_tcp_create_listen: setsockopt(SO_REUSEADDR) failed with errno=%d\n", 
            ompi_socket_errno);
    }

    memset(&inaddr, 0, sizeof(inaddr));
    inaddr.sin_family = AF_INET;
    inaddr.sin_addr.s_addr = INADDR_ANY;
    inaddr.sin_port = 0;

    if(bind(mca_oob_tcp_component.tcp_listen_sd, (struct sockaddr*)&inaddr, sizeof(inaddr)) < 0) {
        ompi_output(0,"mca_oob_tcp_create_listen: bind() failed with errno=%d", ompi_socket_errno);
        return OMPI_ERROR;
    }

    /* resolve system assigned port */
    addrlen = sizeof(struct sockaddr_in);
    if(getsockname(mca_oob_tcp_component.tcp_listen_sd, (struct sockaddr*)&inaddr, &addrlen) < 0) {
        ompi_output(0, "mca_oob_tcp_create_listen: getsockname() failed with errno=%d", ompi_socket_errno);
        return OMPI_ERROR;
    }
    mca_oob_tcp_component.tcp_listen_port = inaddr.sin_port;
                                                                                                                   
    /* setup listen backlog to maximum allowed by kernel */
    if(listen(mca_oob_tcp_component.tcp_listen_sd, SOMAXCONN) < 0) {
        ompi_output(0, "mca_oob_tcp_component_init: listen() failed with errno=%d", ompi_socket_errno);
        return OMPI_ERROR;
    }
                                                                                                                   
    /* set socket up to be non-blocking, otherwise accept could block */
    if((flags = fcntl(mca_oob_tcp_component.tcp_listen_sd, F_GETFL, 0)) < 0) {
        ompi_output(0, "mca_oob_tcp_component_init: fcntl(F_GETFL) failed with errno=%d", ompi_socket_errno);
        return OMPI_ERROR;
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(mca_oob_tcp_component.tcp_listen_sd, F_SETFL, flags) < 0) {
            ompi_output(0, "mca_oob_tcp_component_init: fcntl(F_SETFL) failed with errno=%d", ompi_socket_errno);
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
    orte_process_name_t guid[2];
    mca_oob_tcp_peer_t* peer;
    int rc, cmpval;
    mca_oob_tcp_event_t* event = (mca_oob_tcp_event_t *)user;

    /* accept new connections on the listen socket */
    if(mca_oob_tcp_component.tcp_listen_sd == sd) {
        mca_oob_tcp_accept();
        return;
    }
    OBJ_RELEASE(event);

    /* recv the process identifier */
    while((rc = recv(sd, (char *)guid, sizeof(guid), 0)) != sizeof(guid)) {
        if(rc >= 0) {
            if(mca_oob_tcp_component.tcp_debug > 3) {
                ompi_output(0, "[%d,%d,%d] mca_oob_tcp_recv_handler: peer closed connection",
                    ORTE_NAME_ARGS(orte_process_info.my_name));
            }
            close(sd);
            return;
        }
        if(ompi_socket_errno != EINTR) {
            ompi_output(0, "[%d,%d,%d] mca_oob_tcp_recv_handler: recv() failed with errno=%d\n", 
                ORTE_NAME_ARGS(orte_process_info.my_name), ompi_socket_errno);
            close(sd);
            return;
        }
    }
    OMPI_PROCESS_NAME_NTOH(guid[0]);
    OMPI_PROCESS_NAME_NTOH(guid[1]);

    /* now set socket up to be non-blocking */
    if((flags = fcntl(sd, F_GETFL, 0)) < 0) {
        ompi_output(0, "[%d,%d,%d] mca_oob_tcp_recv_handler: fcntl(F_GETFL) failed with errno=%d", 
                ORTE_NAME_ARGS(orte_process_info.my_name), ompi_socket_errno);
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(sd, F_SETFL, flags) < 0) {
            ompi_output(0, "[%d,%d,%d] mca_oob_tcp_recv_handler: fcntl(F_SETFL) failed with errno=%d", 
                ORTE_NAME_ARGS(orte_process_info.my_name), ompi_socket_errno);
        }
    }

    /* check for wildcard name - if this is true - we allocate a name from the name server 
     * and return to the peer 
     */
    cmpval = orte_ns.compare(ORTE_NS_CMP_ALL, guid, MCA_OOB_NAME_ANY);
    if (cmpval == 0) {
        if (ORTE_SUCCESS != orte_ns.create_jobid(&guid->jobid)) {
           return;
        }
        if (ORTE_SUCCESS != orte_ns.reserve_range(guid->jobid, 1, &guid->vpid)) {
           return;
        }
        if (ORTE_SUCCESS != orte_ns.assign_cellid_to_process(guid)) {
           return;
        }
    }

    /* lookup the corresponding process */
    peer = mca_oob_tcp_peer_lookup(guid);
    if(NULL == peer) {
        ompi_output(0, "[%d,%d,%d] mca_oob_tcp_recv_handler: unable to locate peer",
                ORTE_NAME_ARGS(orte_process_info.my_name));
        close(sd);
        return;
    }
    /* is the peer instance willing to accept this connection */
    if(mca_oob_tcp_peer_accept(peer, sd) == false) {
        if(mca_oob_tcp_component.tcp_debug > 1) {
            ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_recv_handler: "
                    "rejected connection from [%d,%d,%d] connection state %d",
                    ORTE_NAME_ARGS(orte_process_info.my_name),
                    ORTE_NAME_ARGS(&(peer->peer_name)),
                    ORTE_NAME_ARGS(&(guid[0])),
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
mca_oob_t* mca_oob_tcp_component_init(int* priority)
{
    *priority = 1;

    /* are there any interfaces? */
    if(ompi_ifcount() == 0)
        return NULL;

    /* initialize data structures */
    ompi_hash_table_init(&mca_oob_tcp_component.tcp_peers, 128);
    ompi_hash_table_init(&mca_oob_tcp_component.tcp_peer_names, 128);

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

void mca_oob_tcp_registry_callback(
    orte_gpr_notify_data_t* data,
    void* cbdata)
{
    int32_t i;
    if(mca_oob_tcp_component.tcp_debug > 1) {
        ompi_output(0, "[%d,%d,%d] mca_oob_tcp_registry_callback\n",
            ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    /* process the callback */
    OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    for(i = 0; i < data->cnt; i++) {
        orte_gpr_value_t* value = data->values[i];
        orte_buffer_t buffer;
        mca_oob_tcp_addr_t* addr, *existing;
        mca_oob_tcp_peer_t* peer;
        int32_t j;

        for(j = 0; j < value->cnt; j++) {

            /* check to make sure this is the requested key */
            orte_gpr_keyval_t* keyval = value->keyvals[j];
            if(strcmp(keyval->key,"oob-tcp") != 0)
                continue;

            /* transfer ownership of registry object to buffer and unpack */
            OBJ_CONSTRUCT(&buffer, orte_buffer_t);
            if(orte_dps.load(&buffer, 
                             keyval->value.byteobject.bytes, 
                             keyval->value.byteobject.size) != ORTE_SUCCESS) {
                /* TSW - throw ERROR */
                continue;
            }
            keyval->type = ORTE_NULL;
            keyval->value.byteobject.bytes = NULL;
            keyval->value.byteobject.size = 0;
            addr = mca_oob_tcp_addr_unpack(&buffer);
            OBJ_DESTRUCT(&buffer);
            if(NULL == addr) {
                ompi_output(0, "[%d,%d,%d] mca_oob_tcp_registry_callback: unable to unpack peer address\n",
                    ORTE_NAME_ARGS(orte_process_info.my_name));
                continue;
            }

            if(mca_oob_tcp_component.tcp_debug > 1) {
                ompi_output(0, "[%d,%d,%d] mca_oob_tcp_registry_callback: received peer [%d,%d,%d]\n",
                    ORTE_NAME_ARGS(orte_process_info.my_name),
                    ORTE_NAME_ARGS(&(addr->addr_name)));
            }

            /* check for existing cache entry */
            existing = (mca_oob_tcp_addr_t *)ompi_hash_table_get_proc(
                &mca_oob_tcp_component.tcp_peer_names, &addr->addr_name);
            if(NULL != existing) {
                /* TSW - need to update existing entry */
                OBJ_RELEASE(addr);
                continue;
            }

            /* insert into cache and notify peer */
            ompi_hash_table_set_proc(&mca_oob_tcp_component.tcp_peer_names, &addr->addr_name, addr);
            peer = (mca_oob_tcp_peer_t *)ompi_hash_table_get_proc(
                &mca_oob_tcp_component.tcp_peers, &addr->addr_name);
            if(NULL != peer)
                mca_oob_tcp_peer_resolved(peer, addr);
        }
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
     char *segment, *jobid;
     char *keys[2];
     int rc;
  
     /* if the address is already cached - simply return it */
     OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
     addr = (mca_oob_tcp_addr_t *)ompi_hash_table_get_proc(&mca_oob_tcp_component.tcp_peer_names, 
         &peer->peer_name);
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

     if (ORTE_SUCCESS != (rc = orte_ns.get_jobid_string(&jobid, &peer->peer_name))) {
        return rc;
     }
     asprintf(&segment, "%s-%s", ORTE_JOB_SEGMENT, jobid);
     keys[0] = "oob-tcp";
     keys[1] = NULL;

#if 0
     rc = orte_gpr.subscribe(
         ORTE_GPR_OR,
         ORTE_GPR_NOTIFY_ON_STARTUP|ORTE_GPR_NOTIFY_INCLUDE_STARTUP_DATA|
		 ORTE_GPR_NOTIFY_ON_SHUTDOWN|ORTE_GPR_NOTIFY_INCLUDE_SHUTDOWN_DATA|
		 ORTE_GPR_NOTIFY_PRE_EXISTING,
         segment,
         NULL,
         keys,
         &subscription->subid,
         mca_oob_tcp_registry_callback,
         NULL);

     free(segment);
     free(jobid);
     return rc;
#else 
     return ORTE_SUCCESS;
#endif
}


/*
 * Setup contact information in the registry.
 */
int mca_oob_tcp_init(void)
{
    orte_jobid_t jobid;
    orte_buffer_t *buffer;
    orte_gpr_value_t trig, *trigs, *value;
    mca_oob_tcp_subscription_t *subscription;
    orte_gpr_subscription_t sub, *subs;
    int rc;
    ompi_list_item_t* item;

    /* get my jobid */

    if (ORTE_SUCCESS != (rc = orte_ns.get_jobid(&jobid, 
                                                orte_process_info.my_name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

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

    /* register subscribe callback to receive notification when all processes have registered */
    subscription = OBJ_NEW(mca_oob_tcp_subscription_t);
    subscription->jobid = jobid;
    ompi_list_append(&mca_oob_tcp_component.tcp_subscriptions, &subscription->item);
    OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);

    if(mca_oob_tcp_component.tcp_debug > 1) {
        ompi_output(0, "[%d,%d,%d] mca_oob_tcp_init: calling orte_gpr.subscribe\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    /* setup the subscription description value */
    OBJ_CONSTRUCT(&sub, orte_gpr_subscription_t);
    sub.addr_mode = ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR;
    if (ORTE_SUCCESS != (rc = orte_ns.get_jobid(&jobid, orte_process_info.my_name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(sub.segment), jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    sub.num_tokens= 0;
    sub.tokens = NULL;
    sub.num_keys = 1;
    sub.keys = (char**)malloc(sizeof(char*));
    if(NULL == sub.keys) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    sub.keys[0] = strdup("oob-tcp");
    if (NULL == sub.keys[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    sub.cbfunc = mca_oob_tcp_registry_callback;
    sub.user_tag = NULL;

    /* setup the trigger value */
    OBJ_CONSTRUCT(&trig, orte_gpr_value_t);
    trig.addr_mode = ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR;
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(trig.segment), jobid))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&value);
        OBJ_DESTRUCT(&trig);
        return rc;
    }
    trig.num_tokens = 1;
    trig.tokens = (char**)malloc(sizeof(char*));
    if (NULL == trig.tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.tokens[0] = strdup(ORTE_JOB_GLOBALS);
    if (NULL == trig.tokens[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.keyvals = (orte_gpr_keyval_t**)malloc(2*sizeof(orte_gpr_keyval_t*));
    if(NULL == trig.keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.cnt = 2;
    trig.keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if(NULL == trig.keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.keyvals[0]->key = strdup(ORTE_JOB_SLOTS_KEY);
    trig.keyvals[0]->type = ORTE_NULL;
    
    trig.keyvals[1] = OBJ_NEW(orte_gpr_keyval_t);
    if(NULL == trig.keyvals[1]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.keyvals[1]->key = strdup(ORTE_PROC_NUM_AT_STG1);
    trig.keyvals[1]->type = ORTE_NULL;
    
    trigs = &trig;
    subs = &sub;
    rc = orte_gpr.subscribe(
        ORTE_GPR_NOTIFY_ADD_ENTRY | ORTE_GPR_NOTIFY_VALUE_CHG |
        ORTE_GPR_TRIG_CMP_LEVELS | ORTE_GPR_TRIG_ONE_SHOT,
        1, &subs,
        1, &trigs,
        &subscription->subid);
    if(rc != OMPI_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&sub);
        OBJ_DESTRUCT(&trig);
        return rc;
    }
    OBJ_DESTRUCT(&sub);
    OBJ_DESTRUCT(&trig);  /* done with these */

    buffer = OBJ_NEW(orte_buffer_t);
    if(buffer == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    rc = mca_oob_tcp_addr_pack(buffer);
    if(rc != OMPI_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        return rc;
    }
    
    /* put our contact info in registry */
    value = OBJ_NEW(orte_gpr_value_t);
    if (NULL == value) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value->addr_mode = ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_XAND;
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(value->segment), jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    value->cnt = 1;
    value->keyvals = (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
    if(NULL == value->keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == value->keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&(value->tokens),
                                &(value->num_tokens), orte_process_info.my_name))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        return rc;
    }

    (value->keyvals[0])->type = ORTE_BYTE_OBJECT;
    (value->keyvals[0])->key = strdup("oob-tcp");
    rc = orte_dps.unload(buffer, (void**)&(value->keyvals[0])->value.byteobject.bytes,
                                &(value->keyvals[0])->value.byteobject.size);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        OBJ_RELEASE(buffer);
        return rc;
    }

    if(mca_oob_tcp_component.tcp_debug > 1) {
        ompi_output(0, "[%d,%d,%d] mca_oob_tcp_init: calling orte_gpr.put(%s)\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name),
            value->segment);
    }

    rc = orte_gpr.put(1, &value);
    if(rc != OMPI_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        OBJ_RELEASE(buffer);
        return rc;
    }
    OBJ_RELEASE(buffer);
    OBJ_RELEASE(value);

    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
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


int mca_oob_tcp_process_name_compare(const orte_process_name_t* n1, const orte_process_name_t* n2)
{
    return orte_ns.compare(ORTE_NS_CMP_ALL, n1, n2);
}


/*
* Return local process address as a URI string.
*/

char* mca_oob_tcp_get_addr(void)
{
    int i;
    char *contact_info = (char *)malloc((ompi_ifcount()+1) * 32);
    char *ptr = contact_info;
    *ptr = 0;

    for(i=ompi_ifbegin(); i>0; i=ompi_ifnext(i)) {
        struct sockaddr_in addr;
        char name[32];
        ompi_ifindextoname(i, name, sizeof(name));
        if (mca_oob_tcp_component.tcp_include != NULL &&
            strstr(mca_oob_tcp_component.tcp_include,name) == NULL)
            continue;
        if (mca_oob_tcp_component.tcp_exclude != NULL &&
            strstr(mca_oob_tcp_component.tcp_exclude,name) != NULL)
            continue;
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

int mca_oob_tcp_set_addr(const orte_process_name_t* name, const char* uri)
{
    struct sockaddr_in inaddr;
    mca_oob_tcp_addr_t* addr;
    mca_oob_tcp_peer_t* peer;
    int rc;
    if((rc = mca_oob_tcp_parse_uri(uri,&inaddr)) != OMPI_SUCCESS)
        return rc;

    OMPI_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    addr = (mca_oob_tcp_addr_t*)ompi_hash_table_get_proc(&mca_oob_tcp_component.tcp_peer_names, name);
    if(NULL == addr) {
        addr = OBJ_NEW(mca_oob_tcp_addr_t);
        addr->addr_name = *name;
        ompi_hash_table_set_proc(&mca_oob_tcp_component.tcp_peer_names, &addr->addr_name, addr);
    }
    rc = mca_oob_tcp_addr_insert(addr, &inaddr);
    peer = (mca_oob_tcp_peer_t *)ompi_hash_table_get_proc(
        &mca_oob_tcp_component.tcp_peers, &addr->addr_name);
    if(NULL != peer) {
        mca_oob_tcp_peer_resolved(peer, addr);
    }
    OMPI_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
    return rc;
}

