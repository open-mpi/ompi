/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include "include/ompi_socket_errno.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#include <fcntl.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif

#include "include/constants.h"
#include "opal/event/event.h"
#include "util/if.h"
#include "util/argv.h"
#include "opal/util/output.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/base/mca_base_param.h"
#include "mca/ns/ns_types.h"

#include "mca/oob/base/base.h"
#include "ptl_tcp.h"
#include "ptl_tcp_addr.h"
#include "ptl_tcp_proc.h"
#include "ptl_tcp_recvfrag.h"
#include "ptl_tcp_sendfrag.h"
#include "ptl_tcp_sendreq.h"

#define IMPORTANT_WINDOWS_COMMENT() \
            /* In windows, many of the socket functions return an EWOULDBLOCK instead of \
               things like EAGAIN, EINPROGRESS, etc. It has been verified that this will \
               not conflict with other error codes that are returned by these functions \
               under UNIX/Linux environments */

/*
 * Data structure for accepting connections.
 */

struct mca_ptl_tcp_event_t {
    opal_list_item_t item;
    opal_event_t event;
};
typedef struct mca_ptl_tcp_event_t mca_ptl_tcp_event_t;

static void mca_ptl_tcp_event_construct(mca_ptl_tcp_event_t* event)
{
    OPAL_THREAD_LOCK(&mca_ptl_tcp_component.tcp_lock);
    opal_list_append(&mca_ptl_tcp_component.tcp_events, &event->item);
    OPAL_THREAD_UNLOCK(&mca_ptl_tcp_component.tcp_lock);
}

static void mca_ptl_tcp_event_destruct(mca_ptl_tcp_event_t* event)
{
    OPAL_THREAD_LOCK(&mca_ptl_tcp_component.tcp_lock);
    opal_list_remove_item(&mca_ptl_tcp_component.tcp_events, &event->item);
    OPAL_THREAD_UNLOCK(&mca_ptl_tcp_component.tcp_lock);
}

OBJ_CLASS_INSTANCE(
    mca_ptl_tcp_event_t,
    opal_list_item_t,
    mca_ptl_tcp_event_construct,
    mca_ptl_tcp_event_destruct);


/*
 * The PTL TCP component
 */

mca_ptl_tcp_component_t mca_ptl_tcp_component = {
    {
      /* First, the mca_base_module_t struct containing meta
         information about the module itself */
      {
        /* Indicate that we are a pml v1.0.0 module (which also
           implies a specific MCA version) */

        MCA_PTL_BASE_VERSION_1_0_0,

        "tcp", /* MCA module name */
        1,  /* MCA module major version */
        0,  /* MCA module minor version */
        0,  /* MCA module release version */
        mca_ptl_tcp_component_open,  /* module open */
        mca_ptl_tcp_component_close  /* module close */
      },
      
      /* Next the MCA v1.0.0 module meta data */
      
      {
        /* Whether the module is checkpointable or not */
        
        false
      },
      
      mca_ptl_tcp_component_init,  
      mca_ptl_tcp_component_control,
      NULL /*mca_ptl_tcp_component_progress*/,
    }
};

/*
 * functions for receiving event callbacks
 */

static void mca_ptl_tcp_component_recv_handler(int, short, void*);


/*
 * utility routines for parameter registration
 */

static inline char* mca_ptl_tcp_param_register_string(
    const char* param_name, 
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("ptl","tcp",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}
                                                                                                                            
static inline int mca_ptl_tcp_param_register_int(
    const char* param_name, 
    int default_value)
{
    int id = mca_base_param_register_int("ptl","tcp",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/*
 *  Called by MCA framework to open the module, registers
 *  module parameters.
 */

int mca_ptl_tcp_component_open(void)
{
#ifdef WIN32
    WSADATA win_sock_data;
    if (WSAStartup(MAKEWORD(2,2), &win_sock_data) != 0) {
        opal_output (0, "mca_ptl_tcp_component_init: failed to initialise windows sockets:%d\n", WSAGetLastError());
        return OMPI_ERROR;
    }
#endif

    /* initialize state */
    mca_ptl_tcp_component.tcp_listen_sd = -1;
    mca_ptl_tcp_component.tcp_ptl_modules = NULL;
    mca_ptl_tcp_component.tcp_num_ptl_modules = 0;

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_ptl_tcp_component.tcp_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_ptl_tcp_component.tcp_procs, opal_hash_table_t);
    OBJ_CONSTRUCT(&mca_ptl_tcp_component.tcp_pending_acks, opal_list_t);
    OBJ_CONSTRUCT(&mca_ptl_tcp_component.tcp_events, opal_list_t);
    OBJ_CONSTRUCT(&mca_ptl_tcp_component.tcp_send_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_ptl_tcp_component.tcp_recv_frags, ompi_free_list_t);
    opal_hash_table_init(&mca_ptl_tcp_component.tcp_procs, 256);

    /* register TCP module parameters */
    mca_ptl_tcp_component.tcp_if_include =
        mca_ptl_tcp_param_register_string("if_include", "");
    mca_ptl_tcp_component.tcp_if_exclude =
        mca_ptl_tcp_param_register_string("if_exclude", "lo");
    mca_ptl_tcp_component.tcp_free_list_num =
        mca_ptl_tcp_param_register_int("free_list_num", 256);
    mca_ptl_tcp_component.tcp_free_list_max =
        mca_ptl_tcp_param_register_int("free_list_max", -1);
    mca_ptl_tcp_component.tcp_free_list_inc =
        mca_ptl_tcp_param_register_int("free_list_inc", 256);
    mca_ptl_tcp_component.tcp_sndbuf =
        mca_ptl_tcp_param_register_int("sndbuf", 128*1024);
    mca_ptl_tcp_component.tcp_rcvbuf =
        mca_ptl_tcp_param_register_int("rcvbuf", 128*1024);
    mca_ptl_tcp_module.super.ptl_exclusivity =
        mca_ptl_tcp_param_register_int("exclusivity", 0);
    mca_ptl_tcp_module.super.ptl_first_frag_size =
        mca_ptl_tcp_param_register_int("first_frag_size", 64*1024);
    mca_ptl_tcp_module.super.ptl_min_frag_size = 
        mca_ptl_tcp_param_register_int("min_frag_size", 64*1024);
    mca_ptl_tcp_module.super.ptl_max_frag_size =
        mca_ptl_tcp_param_register_int("max_frag_size", -1);
    /* the tcp allocator will never allocate buffers with more than this size */
    mca_ptl_tcp_component.tcp_frag_size =
        mca_ptl_tcp_param_register_int("frag_size", 64*1024);
    /* adapt the first fragment size to fit with the allowed fragment size */
    if( (mca_ptl_tcp_component.tcp_frag_size != 0) &&
        (mca_ptl_tcp_module.super.ptl_first_frag_size > mca_ptl_tcp_component.tcp_frag_size) ) {
        mca_ptl_tcp_module.super.ptl_first_frag_size = mca_ptl_tcp_component.tcp_frag_size;
    }
    return OMPI_SUCCESS;
}

/*
 * module cleanup - sanity checking of queue lengths
 */

int mca_ptl_tcp_component_close(void)
{
    opal_list_item_t* item;
#ifdef WIN32
    WSACleanup();
#endif
#if OMPI_ENABLE_DEBUG
    if (mca_ptl_tcp_component.tcp_send_frags.fl_num_allocated != 
        mca_ptl_tcp_component.tcp_send_frags.super.opal_list_length) {
        opal_output(0, "tcp send frags: %d allocated %d returned\n",
            mca_ptl_tcp_component.tcp_send_frags.fl_num_allocated, 
            mca_ptl_tcp_component.tcp_send_frags.super.opal_list_length);
    }
    if (mca_ptl_tcp_component.tcp_recv_frags.fl_num_allocated != 
        mca_ptl_tcp_component.tcp_recv_frags.super.opal_list_length) {
        opal_output(0, "tcp recv frags: %d allocated %d returned\n",
            mca_ptl_tcp_component.tcp_recv_frags.fl_num_allocated, 
            mca_ptl_tcp_component.tcp_recv_frags.super.opal_list_length);
    }
#endif

    if(NULL != mca_ptl_tcp_component.tcp_if_include)
        free(mca_ptl_tcp_component.tcp_if_include);
    if(NULL != mca_ptl_tcp_component.tcp_if_exclude)
       free(mca_ptl_tcp_component.tcp_if_exclude);
    if (NULL != mca_ptl_tcp_component.tcp_ptl_modules)
        free(mca_ptl_tcp_component.tcp_ptl_modules);
 
    if (mca_ptl_tcp_component.tcp_listen_sd >= 0) {
        opal_event_del(&mca_ptl_tcp_component.tcp_recv_event);
        close(mca_ptl_tcp_component.tcp_listen_sd);
        mca_ptl_tcp_component.tcp_listen_sd = -1;
    }

    /* cleanup any pending events */
    OPAL_THREAD_LOCK(&mca_ptl_tcp_component.tcp_lock);
    for(item =  opal_list_remove_first(&mca_ptl_tcp_component.tcp_events);
        item != NULL; 
        item =  opal_list_remove_first(&mca_ptl_tcp_component.tcp_events)) {
        mca_ptl_tcp_event_t* event = (mca_ptl_tcp_event_t*)item;
        opal_event_del(&event->event);
        OBJ_RELEASE(event);
    }
    OPAL_THREAD_UNLOCK(&mca_ptl_tcp_component.tcp_lock);

    /* release resources */
    OBJ_DESTRUCT(&mca_ptl_tcp_component.tcp_procs);
    OBJ_DESTRUCT(&mca_ptl_tcp_component.tcp_pending_acks);
    OBJ_DESTRUCT(&mca_ptl_tcp_component.tcp_events);
    OBJ_DESTRUCT(&mca_ptl_tcp_component.tcp_send_frags);
    OBJ_DESTRUCT(&mca_ptl_tcp_component.tcp_recv_frags);
    OBJ_DESTRUCT(&mca_ptl_tcp_component.tcp_lock);
    return OMPI_SUCCESS;
}


/*
 *  Create a ptl instance and add to modules list.
 */

static int mca_ptl_tcp_create(int if_index, const char* if_name)
{
    mca_ptl_tcp_module_t* ptl = (mca_ptl_tcp_module_t *)malloc(sizeof(mca_ptl_tcp_module_t));
    char param[256];
    if(NULL == ptl)
        return OMPI_ERR_OUT_OF_RESOURCE;
    memcpy(ptl, &mca_ptl_tcp_module, sizeof(mca_ptl_tcp_module));
    OBJ_CONSTRUCT(&ptl->ptl_peers, opal_list_t);
    mca_ptl_tcp_component.tcp_ptl_modules[mca_ptl_tcp_component.tcp_num_ptl_modules++] = ptl;

    /* initialize the ptl */
    ptl->ptl_ifindex = if_index;
#if MCA_PTL_TCP_STATISTICS
    ptl->ptl_bytes_recv = 0;
    ptl->ptl_bytes_sent = 0;
    ptl->ptl_send_handler = 0;
#endif
    ompi_ifindextoaddr(if_index, (struct sockaddr*)&ptl->ptl_ifaddr, sizeof(ptl->ptl_ifaddr));
    ompi_ifindextomask(if_index, (struct sockaddr*)&ptl->ptl_ifmask, sizeof(ptl->ptl_ifmask));

    /* allow user to specify interface bandwidth */
    sprintf(param, "bandwidth_%s", if_name);
    ptl->super.ptl_bandwidth = mca_ptl_tcp_param_register_int(param, 0);

    /* allow user to override/specify latency ranking */
    sprintf(param, "latency_%s", if_name);
    ptl->super.ptl_latency = mca_ptl_tcp_param_register_int(param, 0);

#if OMPI_ENABLE_DEBUG && 0
    opal_output(0,"interface: %s bandwidth %d latency %d\n", 
        if_name, ptl->super.ptl_bandwidth, ptl->super.ptl_latency);
#endif
    return OMPI_SUCCESS;
}

/*
 * Create a TCP PTL instance for either:
 * (1) all interfaces specified by the user
 * (2) all available interfaces 
 * (3) all available interfaces except for those excluded by the user
 */

static int mca_ptl_tcp_component_create_instances(void)
{
    int if_count = ompi_ifcount();
    int if_index;
    char **include;
    char **exclude;
    char **argv;

    if(if_count <= 0)
        return OMPI_ERROR;

    /* allocate memory for ptls */
    mca_ptl_tcp_component.tcp_max_ptl_modules = if_count;
    mca_ptl_tcp_component.tcp_ptl_modules = (mca_ptl_tcp_module_t **)malloc(if_count * sizeof(mca_ptl_tcp_module_t*));
    if(NULL == mca_ptl_tcp_component.tcp_ptl_modules)
        return OMPI_ERR_OUT_OF_RESOURCE;

    /* if the user specified an interface list - use these exclusively */
    argv = include = ompi_argv_split(mca_ptl_tcp_component.tcp_if_include,',');
    while(argv && *argv) {
        char* if_name = *argv;
        int if_index = ompi_ifnametoindex(if_name);
        if(if_index < 0) {
            opal_output(0,"mca_ptl_tcp_component_init: invalid interface \"%s\"", if_name);
        } else {
            mca_ptl_tcp_create(if_index, if_name);
        }
        argv++;
    }
    ompi_argv_free(include);
    if(mca_ptl_tcp_component.tcp_num_ptl_modules)
        return OMPI_SUCCESS;

    /* if the interface list was not specified by the user, create 
     * a PTL for each interface that was not excluded.
    */
    exclude = ompi_argv_split(mca_ptl_tcp_component.tcp_if_exclude,',');
    for(if_index = ompi_ifbegin(); if_index >= 0; if_index = ompi_ifnext(if_index)) {
        char if_name[32];
        ompi_ifindextoname(if_index, if_name, sizeof(if_name));

        /* check to see if this interface exists in the exclude list */
        if(ompi_ifcount() > 1) {
            argv = exclude;
            while(argv && *argv) {
                if(strncmp(*argv,if_name,strlen(*argv)) == 0)
                    break;
                argv++;
            }
            /* if this interface was not found in the excluded list - create a PTL */
            if(argv == 0 || *argv == 0) {
                mca_ptl_tcp_create(if_index, if_name);
            }
        } else {
            mca_ptl_tcp_create(if_index, if_name);
        }
    }
    ompi_argv_free(exclude);
    return OMPI_SUCCESS;
}

/*
 * Create a listen socket and bind to all interfaces
 */

static int mca_ptl_tcp_component_create_listen(void)
{
    int flags;
    struct sockaddr_in inaddr; 
    ompi_socklen_t addrlen;

    /* create a listen socket for incoming connections */
    mca_ptl_tcp_component.tcp_listen_sd = socket(AF_INET, SOCK_STREAM, 0);
    if(mca_ptl_tcp_component.tcp_listen_sd < 0) {
        opal_output(0,"mca_ptl_tcp_component_init: socket() failed with errno=%d", ompi_socket_errno);
        return OMPI_ERROR;
    }
    mca_ptl_tcp_set_socket_options(mca_ptl_tcp_component.tcp_listen_sd);
                                                                                                      
    /* bind to all addresses and dynamically assigned port */
    memset(&inaddr, 0, sizeof(inaddr));
    inaddr.sin_family = AF_INET;
    inaddr.sin_addr.s_addr = INADDR_ANY;
    inaddr.sin_port = 0;
                                                                                                      
    if(bind(mca_ptl_tcp_component.tcp_listen_sd, (struct sockaddr*)&inaddr, sizeof(inaddr)) < 0) {
        opal_output(0,"mca_ptl_tcp_component_init: bind() failed with errno=%d", ompi_socket_errno);
        return OMPI_ERROR;
    }
                                                                                                      
    /* resolve system assignend port */
    addrlen = sizeof(struct sockaddr_in);
    if(getsockname(mca_ptl_tcp_component.tcp_listen_sd, (struct sockaddr*)&inaddr, &addrlen) < 0) {
        opal_output(0, "mca_ptl_tcp_component_init: getsockname() failed with errno=%d", ompi_socket_errno);
        return OMPI_ERROR;
    }
    mca_ptl_tcp_component.tcp_listen_port = inaddr.sin_port;

    /* setup listen backlog to maximum allowed by kernel */
    if(listen(mca_ptl_tcp_component.tcp_listen_sd, SOMAXCONN) < 0) {
        opal_output(0, "mca_ptl_tcp_component_init: listen() failed with errno=%d", ompi_socket_errno);
        return OMPI_ERROR;
    }

    /* set socket up to be non-blocking, otherwise accept could block */
    if((flags = fcntl(mca_ptl_tcp_component.tcp_listen_sd, F_GETFL, 0)) < 0) {
        opal_output(0, "mca_ptl_tcp_component_init: fcntl(F_GETFL) failed with errno=%d", ompi_socket_errno);
        return OMPI_ERROR;
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(mca_ptl_tcp_component.tcp_listen_sd, F_SETFL, flags) < 0) {
            opal_output(0, "mca_ptl_tcp_component_init: fcntl(F_SETFL) failed with errno=%d", ompi_socket_errno);
            return OMPI_ERROR;
        }
    }

    /* register listen port */
    opal_event_set(
        &mca_ptl_tcp_component.tcp_recv_event,
        mca_ptl_tcp_component.tcp_listen_sd, 
        OPAL_EV_READ|OPAL_EV_PERSIST, 
        mca_ptl_tcp_component_recv_handler, 
        0);
    return OMPI_SUCCESS;
}

/*
 *  Register TCP module addressing information. The MCA framework
 *  will make this available to all peers. 
 */

static int mca_ptl_tcp_component_exchange(void)
{
     int rc=0;
     size_t i=0;
     size_t size = mca_ptl_tcp_component.tcp_num_ptl_modules * sizeof(mca_ptl_tcp_addr_t);
     if(mca_ptl_tcp_component.tcp_num_ptl_modules != 0) {
         mca_ptl_tcp_addr_t *addrs = (mca_ptl_tcp_addr_t *)malloc(size);
         for(i=0; i<mca_ptl_tcp_component.tcp_num_ptl_modules; i++) {
             mca_ptl_tcp_module_t* ptl = mca_ptl_tcp_component.tcp_ptl_modules[i];
             addrs[i].addr_inet    = ptl->ptl_ifaddr.sin_addr;
             addrs[i].addr_port    = mca_ptl_tcp_component.tcp_listen_port;
             addrs[i].addr_inuse   = 0;
         }
         rc =  mca_base_modex_send(&mca_ptl_tcp_component.super.ptlm_version, addrs, size);
         free(addrs);
     }
     return rc;
}

/*
 *  TCP module initialization:
 *  (1) read interface list from kernel and compare against module parameters
 *      then create a PTL instance for selected interfaces
 *  (2) setup TCP listen socket for incoming connection attempts
 *  (3) register PTL parameters with the MCA
 */
mca_ptl_base_module_t** mca_ptl_tcp_component_init(int *num_ptl_modules, 
                                                   bool enable_progress_threads,
                                                   bool enable_mpi_threads)
{
    mca_ptl_base_module_t **ptls;
    *num_ptl_modules = 0;

    ompi_free_list_init(&mca_ptl_tcp_component.tcp_send_frags, 
        sizeof(mca_ptl_tcp_send_frag_t),
        OBJ_CLASS(mca_ptl_tcp_send_frag_t),
        mca_ptl_tcp_component.tcp_free_list_num,
        mca_ptl_tcp_component.tcp_free_list_max,
        mca_ptl_tcp_component.tcp_free_list_inc,
        NULL); /* use default allocator */

    ompi_free_list_init(&mca_ptl_tcp_component.tcp_recv_frags, 
        sizeof(mca_ptl_tcp_recv_frag_t),
        OBJ_CLASS(mca_ptl_tcp_recv_frag_t),
        mca_ptl_tcp_component.tcp_free_list_num,
        mca_ptl_tcp_component.tcp_free_list_max,
        mca_ptl_tcp_component.tcp_free_list_inc,
        NULL); /* use default allocator */

    /* create a PTL TCP module for selected interfaces */
    if(mca_ptl_tcp_component_create_instances() != OMPI_SUCCESS)
        return 0;

    /* create a TCP listen socket for incoming connection attempts */
    if(mca_ptl_tcp_component_create_listen() != OMPI_SUCCESS)
        return 0;

    /* publish TCP parameters with the MCA framework */
    if(mca_ptl_tcp_component_exchange() != OMPI_SUCCESS)
        return 0;

    ptls = (mca_ptl_base_module_t **)malloc(mca_ptl_tcp_component.tcp_num_ptl_modules * 
                  sizeof(mca_ptl_base_module_t*));
    if(NULL == ptls)
        return NULL;

    memcpy(ptls, mca_ptl_tcp_component.tcp_ptl_modules, mca_ptl_tcp_component.tcp_num_ptl_modules*sizeof(mca_ptl_tcp_module_t*));
    *num_ptl_modules = mca_ptl_tcp_component.tcp_num_ptl_modules;

    return ptls;
}

/*
 *  TCP module control
 */

int mca_ptl_tcp_component_control(int param, void* value, size_t size)
{
    switch(param) {
        case MCA_PTL_ENABLE:
            if(*(int*)value) {
                opal_event_add(&mca_ptl_tcp_component.tcp_recv_event, 0);
                if(opal_hash_table_get_size(&mca_ptl_tcp_component.tcp_procs) > 0) {
                    opal_progress_events(OPAL_EVLOOP_NONBLOCK);
                }
            } else {
                opal_event_del(&mca_ptl_tcp_component.tcp_recv_event);
            }
            break;
        default:
            break;
    }
    return OMPI_SUCCESS;
}


/*
 *  TCP module progress.
 */

int mca_ptl_tcp_component_progress(mca_ptl_tstamp_t tstamp)
{
    return OMPI_SUCCESS;
}


/*
 *  Called by mca_ptl_tcp_component_recv() when the TCP listen
 *  socket has pending connection requests. Accept incoming
 *  requests and queue for completion of the connection handshake.
*/


static void mca_ptl_tcp_component_accept(void)
{
    while(true) {
        ompi_socklen_t addrlen = sizeof(struct sockaddr_in);
        struct sockaddr_in addr;
        mca_ptl_tcp_event_t *event;
        int sd = accept(mca_ptl_tcp_component.tcp_listen_sd, (struct sockaddr*)&addr, &addrlen);
        if(sd < 0) {
            IMPORTANT_WINDOWS_COMMENT();
            if(ompi_socket_errno == EINTR)
                continue;
            if(ompi_socket_errno != EAGAIN || ompi_socket_errno != EWOULDBLOCK)
                opal_output(0, "mca_ptl_tcp_component_accept: accept() failed with errno %d.", ompi_socket_errno);
            return;
        }
        mca_ptl_tcp_set_socket_options(sd);

        /* wait for receipt of peers process identifier to complete this connection */
         
        event = OBJ_NEW(mca_ptl_tcp_event_t);
        opal_event_set(&event->event, sd, OPAL_EV_READ, mca_ptl_tcp_component_recv_handler, event);
        opal_event_add(&event->event, 0);
    }
}


/*
 * Event callback when there is data available on the registered 
 * socket to recv.
 */
static void mca_ptl_tcp_component_recv_handler(int sd, short flags, void* user)
{
    orte_process_name_t guid;
    struct sockaddr_in addr;
    int retval;
    mca_ptl_tcp_proc_t* ptl_proc;
    ompi_socklen_t addr_len = sizeof(addr);
    mca_ptl_tcp_event_t *event = (mca_ptl_tcp_event_t *)user;

    /* accept new connections on the listen socket */
    if(mca_ptl_tcp_component.tcp_listen_sd == sd) {
        mca_ptl_tcp_component_accept();
        return;
    }
    OBJ_RELEASE(event);

    /* recv the process identifier */
    retval = recv(sd, (char *)&guid, sizeof(guid), 0);
    if(retval != sizeof(guid)) {
        close(sd);
        return;
    }

    /* now set socket up to be non-blocking */
    if((flags = fcntl(sd, F_GETFL, 0)) < 0) {
        opal_output(0, "mca_ptl_tcp_component_recv_handler: fcntl(F_GETFL) failed with errno=%d", ompi_socket_errno);
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(sd, F_SETFL, flags) < 0) {
            opal_output(0, "mca_ptl_tcp_component_recv_handler: fcntl(F_SETFL) failed with errno=%d", ompi_socket_errno);
        }
    }
   
    /* lookup the corresponding process */
    ptl_proc = mca_ptl_tcp_proc_lookup(&guid);
    if(NULL == ptl_proc) {
        opal_output(0, "mca_ptl_tcp_component_recv_handler: unable to locate process");
        close(sd);
        return;
    }

    /* lookup peer address */
    if(getpeername(sd, (struct sockaddr*)&addr, &addr_len) != 0) {
        opal_output(0, "mca_ptl_tcp_component_recv_handler: getpeername() failed with errno=%d", ompi_socket_errno);
        close(sd);
        return;
    }

    /* are there any existing peer instances will to accept this connection */
    if(mca_ptl_tcp_proc_accept(ptl_proc, &addr, sd) == false) {
        close(sd);
        return;
    }
}

