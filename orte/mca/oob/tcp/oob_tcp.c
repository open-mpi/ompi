/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC. 
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * In windows, many of the socket functions return an EWOULDBLOCK
 * instead of things like EAGAIN, EINPROGRESS, etc. It has been
 * verified that this will not conflict with other error codes that
 * are returned by these functions under UNIX/Linux environments 
 */

#include "orte_config.h"

#include "orte/orte_types.h"

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
#if OPAL_WANT_IPV6
#  ifdef HAVE_NETDB_H
#  include <netdb.h>
#  endif
#endif

#include "opal/util/error.h"
#include "opal/opal_socket_errno.h"
#include "opal/util/output.h"
#include "opal/util/if.h"
#include "opal/util/net.h"
#include "opal/class/opal_hash_table.h"
#include "orte/class/orte_proc_table.h"
#include "orte/mca/oob/tcp/oob_tcp.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"

#if defined(__WINDOWS__)
static opal_mutex_t windows_callback;
#endif  /* defined(__WINDOWS__) */

/*
 * Data structure for accepting connections.
 */
struct mca_oob_tcp_event_t {
    opal_list_item_t item;
    opal_event_t event;
};
typedef struct mca_oob_tcp_event_t mca_oob_tcp_event_t;

static void mca_oob_tcp_event_construct(mca_oob_tcp_event_t* event)
{
    OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    opal_list_append(&mca_oob_tcp_component.tcp_events, &event->item);
    OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
}

static void mca_oob_tcp_event_destruct(mca_oob_tcp_event_t* event)
{
    OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    opal_list_remove_item(&mca_oob_tcp_component.tcp_events, &event->item);
    OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
}

OBJ_CLASS_INSTANCE(
    mca_oob_tcp_event_t,
    opal_list_item_t,
    mca_oob_tcp_event_construct,
    mca_oob_tcp_event_destruct);

/*
 * Local utility functions
 */

static int  mca_oob_tcp_create_listen(int *target_sd, uint16_t af_family);
static int  mca_oob_tcp_create_listen_thread(void);
static void mca_oob_tcp_recv_handler(int sd, short flags, void* user);
static void mca_oob_tcp_accept(int incoming_sd);

struct mca_oob_tcp_subscription_t {
    opal_list_item_t item;
    orte_jobid_t jobid;
    orte_gpr_subscription_id_t subid;
};
typedef struct mca_oob_tcp_subscription_t mca_oob_tcp_subscription_t;

OBJ_CLASS_INSTANCE(
    mca_oob_tcp_subscription_t,
    opal_list_item_t,
    NULL,
    NULL);

OBJ_CLASS_INSTANCE(
                   mca_oob_tcp_pending_connection_t,
                   opal_free_list_item_t,
                   NULL,
                   NULL);

int mca_oob_tcp_output_handle = 0;


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
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
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
    mca_oob_xcast,
    mca_oob_xcast_nb,
    mca_oob_xcast_gate,
    mca_oob_tcp_ft_event,
    mca_oob_tcp_register_contact_info,
    mca_oob_tcp_register_subscription,
    mca_oob_tcp_get_contact_info,
    mca_oob_tcp_registry_callback
};

/*
 * Initialize global variables used w/in this module.
 */
int mca_oob_tcp_component_open(void)
{
    int value = 0;
    char *listen_type, *str;
    int tmp;

#ifdef __WINDOWS__
    WSADATA win_sock_data;
    if (WSAStartup(MAKEWORD(2,2), &win_sock_data) != 0) {
        opal_output (0, "mca_oob_tcp_component_init: failed to initialise windows sockets: error %d\n", WSAGetLastError());
        return ORTE_ERROR;
    }
#endif

    mca_base_param_reg_int(&mca_oob_tcp_component.super.oob_base,
                           "verbose",
                           "Verbose level for the OOB tcp component",
                           false, false,
                           0,
                           &value);
    mca_oob_tcp_output_handle = opal_output_open(NULL);
    opal_output_set_verbosity(mca_oob_tcp_output_handle, value);

    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_subscriptions, opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_peer_list,     opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_peers,         opal_hash_table_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_peer_names,    opal_hash_table_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_peer_free,     opal_free_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_msgs,          opal_free_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_lock,          opal_mutex_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_events,        opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_msg_post,      opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_msg_recv,      opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_msg_completed, opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_match_lock,    opal_mutex_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_match_cond,    opal_condition_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_listen_thread, opal_thread_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_pending_connections_fl, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_pending_connections, opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_copy_out_connections, opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_copy_in_connections, opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_connections_return, opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_connections_return_copy, opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_pending_connections_lock, opal_mutex_t);

    /* register oob module parameters */
    mca_base_param_reg_int(&mca_oob_tcp_component.super.oob_base,
                           "peer_limit",
                           "Maximum number of peer connections to simultaneously maintain (-1 = infinite)",
                           false, false, -1,
                           &mca_oob_tcp_component.tcp_peer_limit);

    mca_base_param_reg_int(&mca_oob_tcp_component.super.oob_base,
                           "peer_retries",
                           "Number of times to try shutting down a connection before giving up",
                           false, false, 60,
                           &mca_oob_tcp_component.tcp_peer_retries);

    mca_base_param_reg_int(&mca_oob_tcp_component.super.oob_base,
                           "debug",
                           "Enable (1) / disable (0) debugging output for this component",
                           false, false, 0,
                           &mca_oob_tcp_component.tcp_debug);

    mca_base_param_reg_int(&mca_oob_tcp_component.super.oob_base,
                           "sndbuf",
                           "TCP socket send buffering size (in bytes)",
                           false, false, 128 * 1024,
                           &mca_oob_tcp_component.tcp_sndbuf);

    mca_base_param_reg_int(&mca_oob_tcp_component.super.oob_base,
                           "rcvbuf",
                           "TCP socket receive buffering size (in bytes)",
                           false, false, 128 * 1024,
                           &mca_oob_tcp_component.tcp_rcvbuf);

    mca_base_param_reg_string(&mca_oob_tcp_component.super.oob_base,
                              "if_include",
                              "Comma-delimited list of TCP interfaces to use",
                              false, false, NULL, 
                              &mca_oob_tcp_component.tcp_include);
    mca_base_param_reg_string(&mca_oob_tcp_component.super.oob_base,
                              "include",
                              "Obsolete synonym for oob_tcp_if_include",
                              true, false, NULL, &str);
    if (NULL != str) {
        if (NULL == mca_oob_tcp_component.tcp_include) {
            mca_oob_tcp_component.tcp_include = str;
        } else {
            free(str);
        }
    }

    mca_base_param_reg_string(&mca_oob_tcp_component.super.oob_base,
                              "if_exclude",
                              "Comma-delimited list of TCP interfaces to exclude",
                              false, false, NULL, 
                              &mca_oob_tcp_component.tcp_exclude);
    mca_base_param_reg_string(&mca_oob_tcp_component.super.oob_base,
                              "exclude",
                              "Obsolete synonym for oob_tcp_if_exclude",
                              true, false, NULL, &str);
    if (NULL != str) {
        if (NULL == mca_oob_tcp_component.tcp_exclude) {
            mca_oob_tcp_component.tcp_exclude = str;
        } else {
            free(str);
        }
    }

    mca_base_param_reg_int(&mca_oob_tcp_component.super.oob_base,
                           "connect_sleep",
                           "Enable (1) / disable (0) random sleep for connection wireup",
                           false,
                           false,
                           1,
                           &mca_oob_tcp_component.connect_sleep);

    mca_base_param_reg_string(&mca_oob_tcp_component.super.oob_base,
                              "listen_mode",
                              "Mode for HNP to accept incoming connections: event, listen_thread",
                              false,
                              false,
                              "event",
                              &listen_type);
    
    if (0 == strcmp(listen_type, "event")) {
        mca_oob_tcp_component.tcp_listen_type = OOB_TCP_EVENT;
    } else if (0 == strcmp(listen_type, "listen_thread")) {
        mca_oob_tcp_component.tcp_listen_type = OOB_TCP_LISTEN_THREAD;
    } else {
        opal_output(0, "Invalid value for oob_tcp_listen_mode parameter: %s",
                    listen_type);
        return ORTE_ERROR;
    }

    mca_base_param_reg_int(&mca_oob_tcp_component.super.oob_base,
                           "listen_thread_max_queue",
                           "High water mark for queued accepted socket list size",
                           false,
                           false,
                           10,
                           &mca_oob_tcp_component.tcp_copy_max_size);

    mca_base_param_reg_int(&mca_oob_tcp_component.super.oob_base,
                           "listen_thread_max_time",
                           "Maximum amount of time (in milliseconds) to wait between processing accepted socket list",
                           false,
                           false,
                           10,
                           &tmp);

#if OPAL_TIMER_USEC_NATIVE
    mca_oob_tcp_component.tcp_copy_delta = tmp * 1000;
#else
    mca_oob_tcp_component.tcp_copy_delta = tmp *
        opal_timer_base_get_freq() / 1000;
#endif

    mca_base_param_reg_int(&mca_oob_tcp_component.super.oob_base,
                           "accept_spin_count",
                           "Number of times to let accept return EWOULDBLOCK before updating accepted socket list",
                           false,
                           false,
                           10,
                           &mca_oob_tcp_component.tcp_copy_spin_count);

    /* initialize state */
    mca_oob_tcp_component.tcp_shutdown = false;
    mca_oob_tcp_component.tcp_listen_sd = -1;
#if OPAL_WANT_IPV6    
    mca_oob_tcp_component.tcp6_listen_sd = -1;
#endif
    mca_oob_tcp_component.tcp_match_count = 0;

    mca_oob_tcp_component.tcp_last_copy_time = 0;

    /* updated with real value during tcp_init */
    mca_oob_tcp_component.tcp_ignore_localhost = true;

    return ORTE_SUCCESS;
}

#if defined(__WINDOWS__)
static int oob_tcp_windows_progress_callback( void )
{
    opal_list_item_t* item;
    mca_oob_tcp_msg_t* msg;
	int event_count = 0;

	/* Only one thread at the time is allowed to execute callbacks */
	if( !opal_mutex_trylock(&windows_callback) )
		return 0;

    OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    while(NULL != 
         (item = opal_list_remove_first(&mca_oob_tcp_component.tcp_msg_completed))) {
        msg = (mca_oob_tcp_msg_t*)item;
        OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
        msg->msg_cbfunc( msg->msg_rc, 
                         &msg->msg_peer, 
                         msg->msg_uiov, 
                         msg->msg_ucnt, 
                         msg->msg_hdr.msg_tag, 
                         msg->msg_cbdata);
		event_count++;
        OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
        MCA_OOB_TCP_MSG_RETURN(msg);
    }
    OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);

	opal_mutex_unlock(&windows_callback);

    return event_count;
}
#endif  /* defined(__WINDOWS__) */

/*
 * Cleanup of global variables used by this module.
 */

int mca_oob_tcp_component_close(void)
{
#if defined(__WINDOWS__)
    opal_progress_unregister(oob_tcp_windows_progress_callback);
	OBJ_DESTRUCT( &windows_callback );
    WSACleanup();
#endif  /* defined(__WINDOWS__) */

    /* cleanup resources */

    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_pending_connections_lock);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_connections_return_copy);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_connections_return);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_copy_out_connections);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_pending_connections);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_pending_connections_fl);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_listen_thread);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_match_cond);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_match_lock);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_msg_completed);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_msg_recv);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_msg_post);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_events);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_lock);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_msgs);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_peer_free);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_peer_names);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_peers);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_subscriptions);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_peer_list);

    return ORTE_SUCCESS;
}


/*
 *  Called by mca_oob_tcp_recv_handler() when the TCP listen
 *  socket has pending connection requests. Accept incoming
 *  requests and queue for completion of the connection handshake.
*/

static void mca_oob_tcp_accept(int incoming_sd)
{
    while(true) {
#if OPAL_WANT_IPV6
        opal_socklen_t addrlen = sizeof(struct sockaddr_in6);
        struct sockaddr_in6 addr;
#else
        opal_socklen_t addrlen = sizeof(struct sockaddr_in);
        struct sockaddr_in addr;
#endif
        mca_oob_tcp_event_t* event;
        int sd;

        sd = accept(incoming_sd, (struct sockaddr*)&addr, &addrlen);
        if(sd < 0) {
            if(opal_socket_errno == EINTR) {
                continue;
            }
            if(opal_socket_errno != EAGAIN && opal_socket_errno != EWOULDBLOCK) {
                opal_output(0, "mca_oob_tcp_accept: accept() failed: %s (%d).", 
                            strerror(opal_socket_errno), opal_socket_errno);
            }
            return;
        }

        /* setup socket options */
        mca_oob_tcp_set_socket_options(sd);

        /* log the accept */
        if (mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_CONNECT) {
            opal_output(0, "%s mca_oob_tcp_accept: %s:%d\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name),
                        opal_net_get_hostname((struct sockaddr*) &addr),
                        opal_net_get_port((struct sockaddr*) &addr));
        }

        /* wait for receipt of peers process identifier to complete this connection */
        event = OBJ_NEW(mca_oob_tcp_event_t);
        opal_event_set(&event->event, sd, OPAL_EV_READ, mca_oob_tcp_recv_handler, event);
        opal_event_add(&event->event, 0);
    }
}

/*
 * Create a listen socket and bind to all interfaces
 */

static int mca_oob_tcp_create_listen(int *target_sd, uint16_t af_family)
{
    int flags;
#if OPAL_WANT_IPV6
    struct sockaddr_in6 inaddr;
#else
    struct sockaddr_in inaddr;
#endif
    opal_socklen_t addrlen;

    /* create a listen socket for incoming connections */
    *target_sd = socket(af_family, SOCK_STREAM, 0);
    
    if(*target_sd < 0) {
        if (EAFNOSUPPORT != opal_socket_errno) {
            opal_output(0,"mca_oob_tcp_component_init: socket() failed: %s (%d)", 
                        strerror(opal_socket_errno), opal_socket_errno);
        }
        return ORTE_ERR_IN_ERRNO;
    }

    /* setup socket options */
    mca_oob_tcp_set_socket_options(*target_sd);

    /* bind address */
    memset(&inaddr, 0, sizeof(inaddr));
#if OPAL_WANT_IPV6
    {
        struct addrinfo hints, *res = NULL;
        int error;
        
        memset (&hints, 0, sizeof(hints));
        hints.ai_family = af_family;
        hints.ai_socktype = SOCK_STREAM;
        hints.ai_flags = AI_PASSIVE;
        
        if ((error = getaddrinfo(NULL, "0", &hints, &res))) {
            opal_output (0,
                         "mca_oob_tcp_create_listen: unable to resolve. %s\n",
                         gai_strerror (error));
            return ORTE_ERROR;
        }
        
        memcpy (&inaddr, res->ai_addr, res->ai_addrlen);
        addrlen = res->ai_addrlen;
        freeaddrinfo (res);
        
#ifdef IPV6_V6ONLY
        /* in case of AF_INET6, disable v4-mapped addresses */
        if (AF_INET6 == af_family) {
            int flg = 0;
            if (setsockopt (*target_sd, IPPROTO_IPV6, IPV6_V6ONLY,
                            &flg, sizeof (flg)) < 0) {
                opal_output(0,
                            "mca_oob_tcp_create_listen: unable to disable v4-mapped addresses\n");
            }
        }
#endif /* IPV6_V6ONLY */
    }
#else
    inaddr.sin_family = AF_INET;
    inaddr.sin_addr.s_addr = INADDR_ANY;
    inaddr.sin_port = 0;
    addrlen = sizeof(struct sockaddr_in);
#endif

    if(bind(*target_sd, (struct sockaddr*)&inaddr, addrlen) < 0) {
        opal_output(0,"mca_oob_tcp_create_listen: bind() failed: %s (%d)", 
                    strerror(opal_socket_errno), opal_socket_errno);
        return ORTE_ERROR;
    }

    /* resolve system assigned port */
    if(getsockname(*target_sd, (struct sockaddr*)&inaddr, &addrlen) < 0) {
        opal_output(0, "mca_oob_tcp_create_listen: getsockname(): %s (%d)", 
                    strerror(opal_socket_errno), opal_socket_errno);
        return ORTE_ERROR;
    }
#if OPAL_WANT_IPV6
    if (AF_INET == af_family) {
        mca_oob_tcp_component.tcp_listen_port = inaddr.sin6_port;
    }
    if (AF_INET6 == af_family) {
        mca_oob_tcp_component.tcp6_listen_port = inaddr.sin6_port;
    }
#else
    mca_oob_tcp_component.tcp_listen_port = inaddr.sin_port;
#endif
    
    /* setup listen backlog to maximum allowed by kernel */
    if(listen(*target_sd, SOMAXCONN) < 0) {
        opal_output(0, "mca_oob_tcp_component_init: listen(): %s (%d)", 
                    strerror(opal_socket_errno), opal_socket_errno);
        return ORTE_ERROR;
    }

    /* set socket up to be non-blocking, otherwise accept could block */
    if((flags = fcntl(*target_sd, F_GETFL, 0)) < 0) {
        opal_output(0, "mca_oob_tcp_component_init: fcntl(F_GETFL) failed: %s (%d)", 
                    strerror(opal_socket_errno), opal_socket_errno);
        return ORTE_ERROR;
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(*target_sd, F_SETFL, flags) < 0) {
            opal_output(0, "mca_oob_tcp_component_init: fcntl(F_SETFL) failed: %s (%d)", 
                        strerror(opal_socket_errno), opal_socket_errno);
            return ORTE_ERROR;
        }
    }

    /* register listen port */
#if OPAL_WANT_IPV6
    if (AF_INET == af_family) {
        opal_event_set(
                       &mca_oob_tcp_component.tcp_recv_event,
                       *target_sd,
                       OPAL_EV_READ|OPAL_EV_PERSIST,
                       mca_oob_tcp_recv_handler,
                       0);
        opal_event_add(&mca_oob_tcp_component.tcp_recv_event, 0);
    }
    
    if (AF_INET6 == af_family) {
        opal_event_set(
                       &mca_oob_tcp_component.tcp6_recv_event,
                       *target_sd,
                       OPAL_EV_READ|OPAL_EV_PERSIST,
                       mca_oob_tcp_recv_handler,
                       0);
        opal_event_add(&mca_oob_tcp_component.tcp6_recv_event, 0);
    }
#else
    opal_event_set(
                   &mca_oob_tcp_component.tcp_recv_event,
                   *target_sd,
                   OPAL_EV_READ|OPAL_EV_PERSIST,
                   mca_oob_tcp_recv_handler,
                   0);
    opal_event_add(&mca_oob_tcp_component.tcp_recv_event, 0);
#endif
    return ORTE_SUCCESS;
}


static void* mca_oob_tcp_listen_thread(opal_object_t *obj)
{
    int rc, count;
    opal_socklen_t addrlen = sizeof(struct sockaddr_in);
    opal_free_list_item_t *fl_item;
    mca_oob_tcp_pending_connection_t *item;
    struct timeval timeout;
    fd_set readfds;

    while (false == mca_oob_tcp_component.tcp_shutdown) {
        count = 0;

        FD_ZERO(&readfds);
        FD_SET(mca_oob_tcp_component.tcp_listen_sd, &readfds);
        timeout.tv_sec = 0;
        timeout.tv_usec = 10000;

        rc = select(mca_oob_tcp_component.tcp_listen_sd + 1, &readfds,
                    NULL, NULL, &timeout);
        if (rc < 0) {
            if (EAGAIN != opal_socket_errno && EINTR != opal_socket_errno) {
                perror("select");
            }
            continue;
        }

        while (count < mca_oob_tcp_component.tcp_copy_spin_count && 
               opal_list_get_size(&mca_oob_tcp_component.tcp_copy_in_connections) < 
               (size_t) mca_oob_tcp_component.tcp_copy_max_size) {
            OPAL_FREE_LIST_WAIT(&mca_oob_tcp_component.tcp_pending_connections_fl, 
                                fl_item, rc);
            item = (mca_oob_tcp_pending_connection_t*) fl_item;
            item->fd = accept(mca_oob_tcp_component.tcp_listen_sd, 
                              (struct sockaddr*)&(item->addr), &addrlen);
            if(item->fd < 0) {
                OPAL_FREE_LIST_RETURN(&mca_oob_tcp_component.tcp_pending_connections_fl, 
                                      fl_item);
                if (mca_oob_tcp_component.tcp_shutdown) return NULL;

                if(opal_socket_errno != EAGAIN || opal_socket_errno != EWOULDBLOCK) {
                    opal_output(0, "mca_oob_tcp_accept: accept() failed: %s (%d).",
                                strerror(opal_socket_errno), opal_socket_errno);
                    CLOSE_THE_SOCKET(item->fd);
                    return NULL;
                }

                count++;
                continue;
            }

            if (mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_CONNECT) {
                opal_output(0, "%s mca_oob_tcp_listen_thread: (%d, %d) %s:%d\n",
                            ORTE_NAME_PRINT(orte_process_info.my_name),
                            item->fd, opal_socket_errno,
                            inet_ntoa(item->addr.sin_addr),
                            item->addr.sin_port);
            }

            opal_list_append(&mca_oob_tcp_component.tcp_copy_in_connections,
                             (opal_list_item_t*) item);
        }

        if (0 < opal_list_get_size(&mca_oob_tcp_component.tcp_copy_in_connections)) {
            opal_mutex_lock(&mca_oob_tcp_component.tcp_pending_connections_lock);
            opal_list_join(&mca_oob_tcp_component.tcp_pending_connections,
                           opal_list_get_end(&mca_oob_tcp_component.tcp_pending_connections),
                           &mca_oob_tcp_component.tcp_copy_in_connections);
            while (NULL != (fl_item = (opal_free_list_item_t*) opal_list_remove_first(&mca_oob_tcp_component.tcp_connections_return_copy))) {
                OPAL_FREE_LIST_RETURN(&mca_oob_tcp_component.tcp_pending_connections_fl, fl_item);
            }
            opal_mutex_unlock(&mca_oob_tcp_component.tcp_pending_connections_lock);
        }
    }

    return NULL;
}

/* called from opal_progress() to create the oob contact information
   for the file descriptors accepted() by the accept thread. */
static int mca_oob_tcp_listen_progress(void)
{
    int count = 0;
    mca_oob_tcp_pending_connection_t *item;
    mca_oob_tcp_event_t* event;
#if OPAL_TIMER_USEC_NATIVE
    opal_timer_t now = opal_timer_base_get_usec();
#else
    opal_timer_t now = opal_timer_base_get_cycles();
#endif  /* OPAL_TIMER_USEC_NATIVE */

    /* if we've not pulled pending connections for a while OR we've
       hit the high water mark of pending connections, grab all the
       pending connections */
    if ((now - mca_oob_tcp_component.tcp_last_copy_time > 
         mca_oob_tcp_component.tcp_copy_delta) ||
        ((size_t) mca_oob_tcp_component.tcp_copy_max_size <
         opal_list_get_size(&mca_oob_tcp_component.tcp_pending_connections))) {

        /* copy the pending connections from the list the accept
           thread is inserting into into a temporary list for us to
           process from.  Then copy the returned free list items into
           that thread's return list, so it can free them soonish.
           This is an O(1) operation, so we minimize the lock time. */
        opal_mutex_lock(&mca_oob_tcp_component.tcp_pending_connections_lock);
        opal_list_join(&mca_oob_tcp_component.tcp_copy_out_connections,
                       opal_list_get_end(&mca_oob_tcp_component.tcp_copy_out_connections),
                       &mca_oob_tcp_component.tcp_pending_connections);
        opal_list_join(&mca_oob_tcp_component.tcp_connections_return_copy,
                       opal_list_get_end(&mca_oob_tcp_component.tcp_connections_return_copy),
                       &mca_oob_tcp_component.tcp_connections_return);
        opal_mutex_unlock(&mca_oob_tcp_component.tcp_pending_connections_lock);

        /* process al the connections */
        while (NULL != (item = (mca_oob_tcp_pending_connection_t*) 
                        opal_list_remove_first(&mca_oob_tcp_component.
                                               tcp_copy_out_connections))) {

            /* setup socket options */
            mca_oob_tcp_set_socket_options(item->fd);

            /* log the accept */
            if (mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_CONNECT) {
                opal_output(0, "%s mca_oob_tcp_listen_progress: %s:%d\n",
                            ORTE_NAME_PRINT(orte_process_info.my_name),
                            inet_ntoa(item->addr.sin_addr),
                            item->addr.sin_port);
            }

            /* wait for receipt of peers process identifier to
               complete this connection */
            event = OBJ_NEW(mca_oob_tcp_event_t);
            opal_event_set(&event->event, item->fd, OPAL_EV_READ, mca_oob_tcp_recv_handler, event);
            opal_event_add(&event->event, 0);
            /* put on the needs returning list */
            opal_list_append(&mca_oob_tcp_component.tcp_connections_return, 
                             (opal_list_item_t*) item);
            count++;
        }

        mca_oob_tcp_component.tcp_last_copy_time = now;
    }

    return count;
}


static int mca_oob_tcp_create_listen_thread(void)
{
    struct sockaddr_in inaddr;
    opal_socklen_t addrlen;
    int flags;

    /* create a listen socket for incoming connections */
    mca_oob_tcp_component.tcp_listen_sd = socket(AF_INET, SOCK_STREAM, 0);
    if(mca_oob_tcp_component.tcp_listen_sd < 0) {
        opal_output(0,"mca_oob_tcp_component_init: socket() failed: %s (%d)",
                    strerror(opal_socket_errno), opal_socket_errno);
        return ORTE_ERROR;
    }

    /* setup socket options */
    mca_oob_tcp_set_socket_options(mca_oob_tcp_component.tcp_listen_sd);

    /* bind address */
    memset(&inaddr, 0, sizeof(inaddr));
    inaddr.sin_family = AF_INET;
    inaddr.sin_addr.s_addr = INADDR_ANY;
    inaddr.sin_port = 0;

    if(bind(mca_oob_tcp_component.tcp_listen_sd, (struct sockaddr*)&inaddr, sizeof(inaddr)) < 0) {
        opal_output(0,"mca_oob_tcp_create_listen: bind() failed: %s (%d)", 
                    strerror(opal_socket_errno), opal_socket_errno);
        return ORTE_ERROR;
    }

    /* resolve system assigned port */
    addrlen = sizeof(struct sockaddr_in);
    if(getsockname(mca_oob_tcp_component.tcp_listen_sd, (struct sockaddr*)&inaddr, &addrlen) < 0) {
        opal_output(0, "mca_oob_tcp_create_listen: getsockname() failed: %s (%d)", 
                    strerror(opal_socket_errno), opal_socket_errno);
        return ORTE_ERROR;
    }
    mca_oob_tcp_component.tcp_listen_port = inaddr.sin_port;

    /* setup listen backlog to maximum allowed by kernel */
    if(listen(mca_oob_tcp_component.tcp_listen_sd, SOMAXCONN) < 0) {
        opal_output(0, "mca_oob_tcp_component_init: listen() failed: %s (%d)", 
                    strerror(opal_socket_errno), opal_socket_errno);
        return ORTE_ERROR;
    }

    /* set socket up to be non-blocking, otherwise accept could block */
    if((flags = fcntl(mca_oob_tcp_component.tcp_listen_sd, F_GETFL, 0)) < 0) {
        opal_output(0, "mca_oob_tcp_component_init: fcntl(F_GETFL) failed: %s (%d)", 
                    strerror(opal_socket_errno), opal_socket_errno);
        return ORTE_ERROR;
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(mca_oob_tcp_component.tcp_listen_sd, F_SETFL, flags) < 0) {
            opal_output(0, "mca_oob_tcp_component_init: fcntl(F_SETFL) failed: %s (%d)", 
                        strerror(opal_socket_errno), opal_socket_errno);
            return ORTE_ERROR;
        }
    }

    /* start the listen thread */
    mca_oob_tcp_component.tcp_listen_thread.t_run = mca_oob_tcp_listen_thread;
    mca_oob_tcp_component.tcp_listen_thread.t_arg = NULL;

    return opal_thread_start(&mca_oob_tcp_component.tcp_listen_thread);
}


/*
 * Handle probe
 */
static void mca_oob_tcp_recv_probe(int sd, mca_oob_tcp_hdr_t* hdr)
{
    unsigned char* ptr = (unsigned char*)hdr;
    size_t cnt = 0;

    hdr->msg_type = MCA_OOB_TCP_PROBE;
    hdr->msg_dst = hdr->msg_src;
    hdr->msg_src = *orte_process_info.my_name;
    MCA_OOB_TCP_HDR_HTON(hdr);

    while(cnt < sizeof(mca_oob_tcp_hdr_t)) {
        int retval = send(sd, (char *)ptr+cnt, sizeof(mca_oob_tcp_hdr_t)-cnt, 0);
        if(retval < 0) {
            if(opal_socket_errno != EINTR && opal_socket_errno != EAGAIN && opal_socket_errno != EWOULDBLOCK) {
                opal_output(0, "%s-%s mca_oob_tcp_peer_recv_probe: send() failed: %s (%d)\n",
                    ORTE_NAME_PRINT(orte_process_info.my_name),
                    ORTE_NAME_PRINT(&(hdr->msg_src)),
                    strerror(opal_socket_errno),
                    opal_socket_errno);
                CLOSE_THE_SOCKET(sd);
                return;
            }
            continue;
        }
        cnt += retval;
    }
    CLOSE_THE_SOCKET(sd);
}

/*
 * Handle connection request
 */
static void mca_oob_tcp_recv_connect(int sd, mca_oob_tcp_hdr_t* hdr)
{
    mca_oob_tcp_peer_t* peer;
    int flags;
    int cmpval;

    /* now set socket up to be non-blocking */
    if((flags = fcntl(sd, F_GETFL, 0)) < 0) {
        opal_output(0, "%s mca_oob_tcp_recv_handler: fcntl(F_GETFL) failed: %s (%d)",
               ORTE_NAME_PRINT(orte_process_info.my_name), strerror(opal_socket_errno), opal_socket_errno);
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(sd, F_SETFL, flags) < 0) {
            opal_output(0, "%s mca_oob_tcp_recv_handler: fcntl(F_SETFL) failed: %s (%d)",
                ORTE_NAME_PRINT(orte_process_info.my_name), strerror(opal_socket_errno), opal_socket_errno);
        }
    }

    /* check for invalid name - if this is true - we allocate a name from the name server
     * and return to the peer
     */
    cmpval = orte_ns.compare_fields(ORTE_NS_CMP_ALL, &hdr->msg_src, ORTE_NAME_INVALID);
    if (cmpval == ORTE_EQUAL) {
        if (ORTE_SUCCESS != orte_ns.create_jobid(&hdr->msg_src.jobid, NULL)) {
           return;
        }
        if (ORTE_SUCCESS != orte_ns.reserve_range(hdr->msg_src.jobid, 1, &hdr->msg_src.vpid)) {
           return;
        }
    }

    /* lookup the corresponding process */
    peer = mca_oob_tcp_peer_lookup(&hdr->msg_src);
    if(NULL == peer) {
        opal_output(0, "%s mca_oob_tcp_recv_handler: unable to locate peer",
                ORTE_NAME_PRINT(orte_process_info.my_name));
        CLOSE_THE_SOCKET(sd);
        return;
    }
    /* is the peer instance willing to accept this connection */
    if(mca_oob_tcp_peer_accept(peer, sd) == false) {
        if(mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_CONNECT_FAIL) {
            opal_output(0, "%s-%s mca_oob_tcp_recv_handler: "
                    "rejected connection from %s connection state %d",
                    ORTE_NAME_PRINT(orte_process_info.my_name),
                    ORTE_NAME_PRINT(&(peer->peer_name)),
                    ORTE_NAME_PRINT(&(hdr->msg_src)),
                    peer->peer_state);
        }
        CLOSE_THE_SOCKET(sd);
        return;
    }
}

/*
 * Event callback when there is data available on the registered
 * socket to recv.
 */

static void mca_oob_tcp_recv_handler(int sd, short flags, void* user)
{
    mca_oob_tcp_hdr_t hdr;
    mca_oob_tcp_event_t* event = (mca_oob_tcp_event_t *)user;
    int rc;

    /* accept new connections on the listen socket */
#if OPAL_WANT_IPV6    
    if((mca_oob_tcp_component.tcp_listen_sd == sd) ||
       (mca_oob_tcp_component.tcp6_listen_sd == sd)) {
#else
    if(mca_oob_tcp_component.tcp_listen_sd == sd) {
#endif
        mca_oob_tcp_accept(sd);
        return;
    }
    OBJ_RELEASE(event);

    /* Some mem checkers don't realize that hdr will guarantee to be
       fully filled in during the read(), below :-( */
    OMPI_DEBUG_ZERO(hdr);

    /* recv the process identifier */
    while((rc = recv(sd, (char *)&hdr, sizeof(hdr), 0)) != sizeof(hdr)) {
        if(rc >= 0) {
            if(mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_CONNECT_FAIL) {
                opal_output(0, "%s mca_oob_tcp_recv_handler: peer closed connection",
                    ORTE_NAME_PRINT(orte_process_info.my_name));
            }
            CLOSE_THE_SOCKET(sd);
            return;
        }
        if(opal_socket_errno != EINTR) {
            opal_output(0, "%s mca_oob_tcp_recv_handler: recv() failed: %s (%d)\n",
                ORTE_NAME_PRINT(orte_process_info.my_name), strerror(opal_socket_errno), opal_socket_errno);
            CLOSE_THE_SOCKET(sd);
            return;
        }
    }
    MCA_OOB_TCP_HDR_NTOH(&hdr);

    /* dispatch based on message type */
    switch(hdr.msg_type) {
        case MCA_OOB_TCP_PROBE:
            mca_oob_tcp_recv_probe(sd, &hdr);
            break;
        case MCA_OOB_TCP_CONNECT:
            mca_oob_tcp_recv_connect(sd, &hdr);
            break;
        default:
            opal_output(0, "%s mca_oob_tcp_recv_handler: invalid message type: %d\n",
                ORTE_NAME_PRINT(orte_process_info.my_name), hdr.msg_type);
            CLOSE_THE_SOCKET(sd);
            break;
    }

}


/*
 * Component initialization - create a module.
 * (1) initialize static resources
 * (2) create listen socket
 */
mca_oob_t* mca_oob_tcp_component_init(int* priority)
{
    int i;

    *priority = 1;

    /* are there any interfaces? */
    if(opal_ifcount() <= 0)
        return NULL;

    /* see if we should use localhost as an address.  We should do so
       if after looking at all available interfaces (based on what we
       find and what the user restricts with MCA parameters) there are
       only local addresses available. */
    mca_oob_tcp_component.tcp_ignore_localhost = false;
    for (i = opal_ifbegin() ; i > 0 ; i = opal_ifnext(i)) {
        char name[32];
        struct sockaddr_storage inaddr;
        opal_ifindextoname(i, name, sizeof(name));
        if (mca_oob_tcp_component.tcp_include != NULL &&
            strstr(mca_oob_tcp_component.tcp_include,name) == NULL) {
            continue;
        }
        if (mca_oob_tcp_component.tcp_exclude != NULL &&
            strstr(mca_oob_tcp_component.tcp_exclude,name) != NULL) {
            continue;
        }
        opal_ifindextoaddr(i, (struct sockaddr*) &inaddr, sizeof(inaddr));
        if(!opal_net_islocalhost((struct sockaddr*) &inaddr)) {
            mca_oob_tcp_component.tcp_ignore_localhost = true;
        }
    }

    /* initialize data structures */
    opal_hash_table_init(&mca_oob_tcp_component.tcp_peers, 128);
    opal_hash_table_init(&mca_oob_tcp_component.tcp_peer_names, 128);

    opal_free_list_init(&mca_oob_tcp_component.tcp_peer_free,
        sizeof(mca_oob_tcp_peer_t),
        OBJ_CLASS(mca_oob_tcp_peer_t),
        8,  /* initial number */
        mca_oob_tcp_component.tcp_peer_limit, /* maximum number */
        8);  /* increment to grow by */

    opal_free_list_init(&mca_oob_tcp_component.tcp_msgs,
        sizeof(mca_oob_tcp_msg_t),
        OBJ_CLASS(mca_oob_tcp_msg_t),
        8,  /* initial number */
       -1,  /* maximum number */
        8);  /* increment to grow by */

    /* intialize event library */
    memset(&mca_oob_tcp_component.tcp_recv_event, 0, sizeof(opal_event_t));
    memset(&mca_oob_tcp_component.tcp_send_event, 0, sizeof(opal_event_t));
#if OPAL_WANT_IPV6
    memset(&mca_oob_tcp_component.tcp6_recv_event, 0, sizeof(opal_event_t));
    memset(&mca_oob_tcp_component.tcp6_send_event, 0, sizeof(opal_event_t));
#endif

#if defined(__WINDOWS__)
    /* Register the libevent callback which will trigger the OOB
     * completion callbacks. */
	 OBJ_CONSTRUCT(&windows_callback, opal_mutex_t);
     opal_progress_register(oob_tcp_windows_progress_callback);
#endif  /* defined(__WINDOWS__) */

    return &mca_oob_tcp;
}

/*
 * Callback from registry on change to subscribed segments.
 */

void mca_oob_tcp_registry_callback(
    orte_gpr_notify_data_t* data,
    void* cbdata)
{
    orte_std_cntr_t i, j, k;
    int rc;
    orte_gpr_value_t **values, *value;
    orte_gpr_keyval_t *keyval;
    orte_byte_object_t *bo;
    orte_buffer_t buffer;
    mca_oob_tcp_addr_t* addr, *existing;
    mca_oob_tcp_peer_t* peer;

    if(mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_INFO) {
        opal_output(0, "%s mca_oob_tcp_registry_callback\n",
            ORTE_NAME_PRINT(orte_process_info.my_name));
    }

    /* process the callback */
    OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    values = (orte_gpr_value_t**)(data->values)->addr;
    for(i = 0, k=0; k < data->cnt &&
                    i < (data->values)->size; i++) {
        if (NULL != values[i]) {
            k++;
            value = values[i];
            for(j = 0; j < value->cnt; j++) {

                /* check to make sure this is the requested key */
                keyval = value->keyvals[j];
                if(strcmp(keyval->key, ORTE_OOB_TCP_KEY) != 0)
                    continue;

                /* transfer ownership of registry object to buffer and unpack */
                OBJ_CONSTRUCT(&buffer, orte_buffer_t);
                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&bo, keyval->value, ORTE_BYTE_OBJECT))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                if(orte_dss.load(&buffer, bo->bytes, bo->size) != ORTE_SUCCESS) {
                    /* TSW - throw ERROR */
                    continue;
                }
                /* protect the values from the release */
                keyval->value->type = ORTE_NULL;
                keyval->value->data = NULL;
                /* unpack the buffer */
                addr = mca_oob_tcp_addr_unpack(&buffer);
                OBJ_DESTRUCT(&buffer);
                if(NULL == addr) {
                    opal_output(0, "%s mca_oob_tcp_registry_callback: unable to unpack peer address\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name));
                    continue;
                }

                if(mca_oob_tcp_component.tcp_debug > OOB_TCP_DEBUG_INFO) {
                    opal_output(0, "%s mca_oob_tcp_registry_callback: received peer %s\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name),
                        ORTE_NAME_PRINT(&(addr->addr_name)));
                }

                /* check for existing cache entry */
                existing = (mca_oob_tcp_addr_t *)orte_hash_table_get_proc(
                    &mca_oob_tcp_component.tcp_peer_names, &addr->addr_name);
                if(NULL != existing && ORTE_EQUAL != orte_dss.compare(ORTE_PROC_MY_NAME, &addr->addr_name, ORTE_NAME)) {
                    /* need to update existing entry - but don't update our own entry! */
                    if(mca_oob_tcp_component.tcp_debug > OOB_TCP_DEBUG_INFO) {
                        opal_output( 0, "%s Received OOB update for %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(&addr->addr_name) );
                    }
                    orte_hash_table_set_proc(&mca_oob_tcp_component.tcp_peer_names, &addr->addr_name, addr);
                    OBJ_RELEASE(addr);
                    continue;
                }

                /* insert into cache and notify peer */
                orte_hash_table_set_proc(&mca_oob_tcp_component.tcp_peer_names, &addr->addr_name, addr);
                peer = (mca_oob_tcp_peer_t *)orte_hash_table_get_proc(
                    &mca_oob_tcp_component.tcp_peers, &addr->addr_name);
                if(NULL != peer)
                    mca_oob_tcp_peer_resolved(peer, addr);
            }
        }
    }
    OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
}

/*
 * Attempt to resolve peer name.
 */

int mca_oob_tcp_resolve(mca_oob_tcp_peer_t* peer)
{
    mca_oob_tcp_addr_t* addr;

    /* if the address is already cached - simply return it */
    OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    addr = (mca_oob_tcp_addr_t *)orte_hash_table_get_proc(&mca_oob_tcp_component.tcp_peer_names,
         &peer->peer_name);
    if(NULL != addr) {
         OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
         mca_oob_tcp_peer_resolved(peer, addr);
         return ORTE_SUCCESS;
    }
    OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);

    /* if we don't know it, then report unknown - don't try to go get it */
    return ORTE_ERR_ADDRESSEE_UNKNOWN;
}


/*
 * Setup contact information in the registry.
 */
int mca_oob_tcp_init(void)
{
    orte_jobid_t jobid;
    int rc;
    int randval = orte_process_info.num_procs;

    if (0 == randval) randval = 10; 

    /* random delay to stagger connections back to seed */
#if defined(__WINDOWS__)
    if(1 == mca_oob_tcp_component.connect_sleep) {
        Sleep((orte_process_info.my_name->vpid % randval % 1000) * 100);
    }
#elif defined(HAVE_USLEEP)
    if(1 == mca_oob_tcp_component.connect_sleep) {
        usleep((orte_process_info.my_name->vpid % randval % 1000) * 1000);
    }
#endif

    /* get my jobid */
    jobid = ORTE_PROC_MY_NAME->jobid;
    
    /* create a listen socket */
    if ((OOB_TCP_LISTEN_THREAD == mca_oob_tcp_component.tcp_listen_type) && 
        orte_process_info.seed) {  
        if (mca_oob_tcp_create_listen_thread() != ORTE_SUCCESS) {
            opal_output(0, "mca_oob_tcp_init: unable to create listen thread");
            return ORTE_ERROR;
        }
        opal_free_list_init(&mca_oob_tcp_component.tcp_pending_connections_fl,
                            sizeof(mca_oob_tcp_pending_connection_t),
                            OBJ_CLASS(mca_oob_tcp_pending_connection_t),
                            16,  /* initial number */
                            -1,  /* maximum number */
                            16);  /* increment to grow by */
        opal_progress_register(mca_oob_tcp_listen_progress);
        if (mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_INFO) {
            opal_output(0, "%s accepting connections via listen thread",
                        ORTE_NAME_PRINT(orte_process_info.my_name));
        }
    } else {
        /* fix up the listen_type, since we might have been in thread,
           but can't do that since we weren't the HNP. */
        mca_oob_tcp_component.tcp_listen_type = OOB_TCP_EVENT;

        rc = mca_oob_tcp_create_listen(&mca_oob_tcp_component.tcp_listen_sd,
                                       AF_INET);
        if (ORTE_SUCCESS != rc && 
            (EAFNOSUPPORT != opal_socket_errno ||
             mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_CONNECT)) {
            opal_output(0,
                        "mca_oob_tcp_init: unable to create IPv4 listen socket: %s\n",
                        opal_strerror(rc));
        }
#if OPAL_WANT_IPV6
        rc = mca_oob_tcp_create_listen(&mca_oob_tcp_component.tcp6_listen_sd,
                                       AF_INET6);
        if (ORTE_SUCCESS != rc && 
            (EAFNOSUPPORT != opal_socket_errno ||
             mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_CONNECT)) {
            opal_output(0,
                        "mca_oob_tcp_init: unable to create IPv6 listen socket: %s\n",
                        opal_strerror(rc));
        }
#endif
        if (mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_INFO) {
            opal_output(0, "%s accepting connections via event library",
                        ORTE_NAME_PRINT(orte_process_info.my_name));
        }
    }

    return ORTE_SUCCESS;
}


int mca_oob_tcp_register_subscription(orte_jobid_t jobid, char *trigger)
{
    char *sub_name, *segment, *trig_name;
    mca_oob_tcp_subscription_t *subscription;
    orte_gpr_subscription_id_t sub_id;
    int rc;
    
    /* register subscribe callback to receive notification when all processes have registered */
    subscription = OBJ_NEW(mca_oob_tcp_subscription_t);
    subscription->jobid = jobid;
    OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    opal_list_append(&mca_oob_tcp_component.tcp_subscriptions, &subscription->item);
    OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);

    if(mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_ALL) {
        opal_output(0, "%s mca_oob_tcp_init: calling orte_gpr.subscribe\n",
            ORTE_NAME_PRINT(orte_process_info.my_name));
    }

    if (ORTE_SUCCESS != (rc = orte_schema.get_std_subscription_name(&sub_name,
                                ORTE_OOB_SUBSCRIPTION, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* attach to the specified trigger */
    if (ORTE_SUCCESS != (rc = orte_schema.get_std_trigger_name(&trig_name,
                                    trigger, jobid))) {
        ORTE_ERROR_LOG(rc);
        free(sub_name);
        return rc;
    }

    /* define the segment */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        ORTE_ERROR_LOG(rc);
        free(sub_name);
        free(trig_name);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_1(&sub_id, trig_name, sub_name,
                                         ORTE_GPR_NOTIFY_DELETE_AFTER_TRIG,
                                         ORTE_GPR_KEYS_OR | ORTE_GPR_TOKENS_OR | ORTE_GPR_STRIPPED,
                                         segment,
                                         NULL,  /* look at all containers on this segment */
                                         ORTE_OOB_TCP_KEY,
                                         mca_oob_tcp_registry_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        free(sub_name);
        free(trig_name);
        free(segment);
        return rc;
    }


    /* the id of each subscription is recorded
     * here so we can (if desired) cancel that subscription later
     */
    subscription->subid = sub_id;
    /* done with these, so release any memory */
    free(trig_name);
    free(sub_name);

    return ORTE_SUCCESS;    
}

int mca_oob_tcp_register_contact_info(void)
{
    orte_std_cntr_t i, num_tokens;
    orte_buffer_t *buffer;
    orte_data_value_t *values[2];
    orte_byte_object_t bo;
    char *tmp, *tmp2, *tmp3;
    char *segment, **tokens;
    char *keys[] = { ORTE_OOB_TCP_KEY, ORTE_PROC_RML_IP_ADDRESS_KEY};
    int rc;
    
    /* setup to put our contact info on registry */
    buffer = OBJ_NEW(orte_buffer_t);
    if(buffer == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS != (rc = mca_oob_tcp_addr_pack(buffer))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        return rc;
    }

    /* extract payload for storage */
    if (ORTE_SUCCESS != (rc = orte_dss.unload(buffer, (void**)&(bo.bytes), &(bo.size)))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        return rc;
    }
    OBJ_RELEASE(buffer);
    values[0] = OBJ_NEW(orte_data_value_t);
    if (NULL == values[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    values[0]->type = ORTE_BYTE_OBJECT;
    if (ORTE_SUCCESS != (rc = orte_dss.copy(&(values[0]->data), &bo, ORTE_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* setup the IP address for storage */
    tmp = mca_oob.oob_get_addr();
    tmp2 = strrchr(tmp, '/') + 1;
    tmp3 = strrchr(tmp, ':');
    if(NULL == tmp2 || NULL == tmp3) {
        opal_output(0, "%s mca_oob_tcp_init: invalid address \'%s\' "
                    "returned for selected oob interfaces.\n",
                    ORTE_NAME_PRINT(orte_process_info.my_name), tmp);
        ORTE_ERROR_LOG(ORTE_ERROR);
        free(tmp);
        free(bo.bytes);
        return ORTE_ERROR;
    }
    *tmp3 = '\0';
    values[1] = OBJ_NEW(orte_data_value_t);
    if (NULL == values[1]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    values[1]->type = ORTE_STRING;
    values[1]->data = strdup(tmp2);
    free(tmp);

    /* define the segment */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(values[0]);
        OBJ_RELEASE(values[1]);
        return rc;
    }
        
    /* get the process tokens */
    if (ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&tokens, &num_tokens,
                                    orte_process_info.my_name))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        OBJ_RELEASE(values[0]);
        OBJ_RELEASE(values[1]);
        return rc;
    }

    /* put our contact info in registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.put_N(ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_XAND,
                                        segment, tokens, 2, keys, values))) {
        ORTE_ERROR_LOG(rc);
    }

    free(segment);
    for(i=0; i < num_tokens; i++) {
        free(tokens[i]);
        tokens[i] = NULL;
    }
    if (NULL != tokens) free(tokens);
    OBJ_RELEASE(values[0]);
    OBJ_RELEASE(values[1]);

    return rc;
}


static int get_contact_info(orte_jobid_t job, char **tokens, orte_gpr_notify_data_t **data)
{
    char *segment;
    char *keys[] = {
        ORTE_OOB_TCP_KEY,
        ORTE_PROC_RML_IP_ADDRESS_KEY,
        NULL
    };
    orte_gpr_value_t **values;
    orte_std_cntr_t cnt, i, idx;
    int rc;
    
    /* define the segment */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* get the data */
    if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_TOKENS_AND | ORTE_GPR_KEYS_OR,
                                           segment, tokens, keys, &cnt, &values))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        return rc;
    }
    
    /* see if we got data back */
    if (0 < cnt) {
        /* build the data into the notify_data object. If the data
         * pointer is NULL, then we are the first values, so initialize
         * it. Otherwise, just add the data to it
         */
        if (NULL == *data) {
            *data = OBJ_NEW(orte_gpr_notify_data_t);
        }
        for (i=0; i < cnt; i++) {
            if (ORTE_SUCCESS != (rc = orte_pointer_array_add(&idx, (*data)->values, (void*)values[i]))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            ++(*data)->cnt;
        }
    }
    

    return ORTE_SUCCESS;
}

int mca_oob_tcp_get_contact_info(orte_process_name_t *name, orte_gpr_notify_data_t **data)
{
    char **tokens=NULL;
    orte_std_cntr_t num_tokens;
    int rc;
    
    /* if the vpid is WILDCARD, then we want the info from all procs in the specified job. This
     * is the default condition, so do nothing for this case. If the vpid is not WILDCARD,
     * then go get the process tokens
     */
    if (ORTE_VPID_WILDCARD != name->vpid) {
        if (ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&tokens, &num_tokens, name))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    /* If the jobid is not WILDCARD, then we only want the info from the specified job -
     * this is the most common case, so treat it first
     */
    if (ORTE_JOBID_WILDCARD != name->jobid) {
        if (ORTE_SUCCESS != (rc = get_contact_info(name->jobid, tokens, data))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }
    
    /* if the jobid is WILDCARD, then we want the info from all jobs. */
    
    return ORTE_SUCCESS;
}


/*
 * Module cleanup.
 */
int mca_oob_tcp_fini(void)
{
    opal_list_item_t *item;
    OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    opal_event_disable(); /* disable event processing */

    /* close listen socket */
    if (mca_oob_tcp_component.tcp_listen_sd >= 0) {
        if (OOB_TCP_EVENT == mca_oob_tcp_component.tcp_listen_type) {
            opal_event_del(&mca_oob_tcp_component.tcp_recv_event);
            CLOSE_THE_SOCKET(mca_oob_tcp_component.tcp_listen_sd);
#if OPAL_WANT_IPV6
            if (mca_oob_tcp_component.tcp6_listen_sd >= 0) {
                opal_event_del(&mca_oob_tcp_component.tcp6_recv_event);
                CLOSE_THE_SOCKET(mca_oob_tcp_component.tcp6_listen_sd);
                mca_oob_tcp_component.tcp6_listen_sd = -1;
            }
#endif
        } else if (OOB_TCP_LISTEN_THREAD == mca_oob_tcp_component.tcp_listen_type) {
            void *data;
            /* adi@2007-04-12: Bug, FIXME:
             * once the thread listener is IPv6 capable, don't forget to
             * close the v6 socket
             */
            mca_oob_tcp_component.tcp_shutdown = true;
            CLOSE_THE_SOCKET(mca_oob_tcp_component.tcp_listen_sd);
            opal_thread_join(&mca_oob_tcp_component.tcp_listen_thread, &data);
            opal_progress_unregister(mca_oob_tcp_listen_progress);
        }
        mca_oob_tcp_component.tcp_listen_sd = -1;
    }

    /* cleanup all peers */
    for(item = opal_list_remove_first(&mca_oob_tcp_component.tcp_peer_list);
        item != NULL;
        item = opal_list_remove_first(&mca_oob_tcp_component.tcp_peer_list)) {
        mca_oob_tcp_peer_t* peer = (mca_oob_tcp_peer_t*)item;
        MCA_OOB_TCP_PEER_RETURN(peer);
    }

    /* delete any pending events */
    for( item = opal_list_get_first(&mca_oob_tcp_component.tcp_events);
        item != opal_list_get_end(&mca_oob_tcp_component.tcp_events);
        item = opal_list_get_first(&mca_oob_tcp_component.tcp_events) ) {
        mca_oob_tcp_event_t* event = (mca_oob_tcp_event_t*)item;
        opal_event_del(&event->event);
        OBJ_RELEASE(event);
    }

    opal_event_enable();
    OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
    return ORTE_SUCCESS;
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
*
* Currently, this function is ONLY used in one place - in oob_tcp_send.c to
* determine if the recipient of the message-to-be-sent is ourselves. Hence,
* this comparison is okay to be LITERAL and can/should use the ns.compare_fields
* function
*/


int mca_oob_tcp_process_name_compare(const orte_process_name_t* n1, const orte_process_name_t* n2)
{
    return orte_ns.compare_fields(ORTE_NS_CMP_ALL, n1, n2);
}


/*
* Return local process address as a URI string.
*/

char* mca_oob_tcp_get_addr(void)
{
    int i;
    char *contact_info = (char *)malloc((opal_ifcount()+1) * 32);
    char *ptr = contact_info;
    *ptr = 0;

    for(i=opal_ifbegin(); i>0; i=opal_ifnext(i)) {
        struct sockaddr_storage addr;
        char name[32];
        opal_ifindextoname(i, name, sizeof(name));
        if (mca_oob_tcp_component.tcp_include != NULL &&
            strstr(mca_oob_tcp_component.tcp_include,name) == NULL) {
            continue;
        }
        if (mca_oob_tcp_component.tcp_exclude != NULL &&
            strstr(mca_oob_tcp_component.tcp_exclude,name) != NULL) {
            continue;
        }
        opal_ifindextoaddr(i, (struct sockaddr*) &addr, sizeof(addr));
        if(mca_oob_tcp_component.tcp_ignore_localhost && 
           opal_net_islocalhost((struct sockaddr*) &addr)) {
            continue;
        }
        if(ptr != contact_info) {
            ptr += sprintf(ptr, ";");
        }

        if (addr.ss_family == AF_INET) {
            ptr += sprintf(ptr, "tcp://%s:%d", opal_net_get_hostname((struct sockaddr*) &addr),
                           ntohs(mca_oob_tcp_component.tcp_listen_port));
        }
        
#if OPAL_WANT_IPV6
        if (addr.ss_family == AF_INET6) {
            ptr += sprintf(ptr, "tcp6://%s:%d", opal_net_get_hostname((struct sockaddr*) &addr),
                           ntohs(mca_oob_tcp_component.tcp6_listen_port));
        }
#endif

    }
    return contact_info;
}

/*
* Parse a URI string into an IP address and port number.
*/

#if OPAL_WANT_IPV6
int mca_oob_tcp_parse_uri(const char* uri, struct sockaddr_in6* inaddr)
#else
int mca_oob_tcp_parse_uri(const char* uri, struct sockaddr_in* inaddr)
#endif
{
    char* tmp = strdup(uri);
    char* ptr = tmp + 6;
    char* addr = ptr;
    char* port;
#if OPAL_WANT_IPV6
    uint16_t af_family = AF_INET;
#endif
    
#if OPAL_WANT_IPV6
    if(strncmp(tmp, "tcp6://", 7) == 0) {
        af_family = AF_INET6;
        addr++; /* we have one more character to skip ('[') */
    } else {
        if(strncmp(tmp, "tcp://", 6) != 0) {
            free(tmp);
            return ORTE_ERR_BAD_PARAM;
        }
    }
#else    
    if(strncmp(tmp, "tcp://", 6) != 0) {
        free(tmp);
        return ORTE_ERR_BAD_PARAM;
    }
#endif
    
    ptr = strrchr(addr, ':');
    if(NULL == ptr) {
        free(tmp);
        return ORTE_ERR_BAD_PARAM;
    }

    *ptr = '\0';
    ptr++;
    port = ptr;

    memset(inaddr, 0, sizeof(inaddr));
#if OPAL_WANT_IPV6
    {
        int error;
        struct addrinfo hints, *res;
        memset(&hints, 0, sizeof(hints));
        hints.ai_family = af_family;
        hints.ai_socktype = SOCK_STREAM;
        error = getaddrinfo (addr, NULL, &hints, &res);
        
        if (error) {
            opal_output (0, "oob_tcp_parse_uri: Could not resolve %s. [Error: %s]\n",
                         addr, gai_strerror (error));
            free (tmp);
            return ORTE_ERR_BAD_PARAM;
        }
        
        if (res->ai_family != af_family) {
            /* should never happen */
            opal_output (0, "oob_tcp_parse_uri: getaddrinfo returned wrong af_family for %s",
                         addr);
            free (tmp);
            return ORTE_ERROR;
        }
        
        memcpy (inaddr, res->ai_addr, res->ai_addrlen);
        freeaddrinfo (res);
    }
    
    if (inaddr->sin6_family != af_family) {
        /* should never happen */
        opal_output (0, "oob_tcp_parse_uri: getaddrinfo+memcpy resulted in wrong af_family for %s",
                     addr);
        free (tmp);
        return ORTE_ERROR;
    }
    
    inaddr->sin6_port = htons(atoi(port));
#else
    inaddr->sin_family = AF_INET;
    inaddr->sin_addr.s_addr = inet_addr(addr);
    if(inaddr->sin_addr.s_addr == INADDR_ANY) {
        free(tmp);
        return ORTE_ERR_BAD_PARAM;
    }
    inaddr->sin_port = htons(atoi(port));
#endif
    free(tmp);
    return ORTE_SUCCESS;
}


/*
 * Setup address in the cache. Note that this could be called multiple
 * times if a given destination exports multiple addresses.
 */

int mca_oob_tcp_set_addr(const orte_process_name_t* name, const char* uri)
{
#if OPAL_WANT_IPV6
    struct sockaddr_in6 inaddr;
#else
    struct sockaddr_in inaddr;
#endif
    mca_oob_tcp_addr_t* addr;
    mca_oob_tcp_peer_t* peer;
    int rc;
    if((rc = mca_oob_tcp_parse_uri(uri,&inaddr)) != ORTE_SUCCESS)
        return rc;

    OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    addr = (mca_oob_tcp_addr_t*)orte_hash_table_get_proc(&mca_oob_tcp_component.tcp_peer_names, name);
    if(NULL == addr) {
        addr = OBJ_NEW(mca_oob_tcp_addr_t);
        addr->addr_name = *name;
        orte_hash_table_set_proc(&mca_oob_tcp_component.tcp_peer_names, &addr->addr_name, addr);
    }
    rc = mca_oob_tcp_addr_insert(addr, &inaddr);
    peer = (mca_oob_tcp_peer_t *)orte_hash_table_get_proc(
        &mca_oob_tcp_component.tcp_peers, &addr->addr_name);
    if(NULL != peer) {
        mca_oob_tcp_peer_resolved(peer, addr);
    }
    OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
    return rc;
}


/* Dummy function for when we are not using FT. */
#if OPAL_ENABLE_FT == 0
int mca_oob_tcp_ft_event(int state) {
    return ORTE_SUCCESS;
}
#else
int mca_oob_tcp_ft_event(int state) {
    int  exit_status = ORTE_SUCCESS;

    if(OPAL_CRS_CHECKPOINT == state) {
        /*
         * Disable event processing while we are working
         */
        OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
        opal_event_disable();

    }
    else if(OPAL_CRS_CONTINUE == state) {
        /*
         * Resume event processing
         */
        opal_event_enable();
        OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
    }
    else if(OPAL_CRS_RESTART == state) {
        /*
         * Resume event processing
         */
        opal_event_enable();
        OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return exit_status;
}
#endif
