/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * In windows, many of the socket functions return an EWOULDBLOCK
 * instead of \ things like EAGAIN, EINPROGRESS, etc. It has been
 * verified that this will \ not conflict with other error codes that
 * are returned by these functions \ under UNIX/Linux environments 
 */

#include "ompi_config.h"

#include "opal/opal_socket_errno.h"
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

#include "ompi/constants.h"
#include "opal/event/event.h"
#include "opal/util/if.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/ns/ns_types.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"

#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/pml/base/pml_base_module_exchange.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/btl/base/btl_base_error.h"
#include "btl_tcp.h"
#include "btl_tcp_addr.h"
#include "btl_tcp_proc.h"
#include "btl_tcp_frag.h"
#include "btl_tcp_endpoint.h" 
#include "ompi/mca/btl/base/base.h" 
#include "ompi/datatype/convertor.h" 


mca_btl_tcp_component_t mca_btl_tcp_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */

            MCA_BTL_BASE_VERSION_1_0_1,

            "tcp", /* MCA component name */
            1,  /* MCA component major version */
            0,  /* MCA component minor version */
            0,  /* MCA component release version */
            mca_btl_tcp_component_open,  /* component open */
            mca_btl_tcp_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */

        {
            /* Whether the component is checkpointable or not */

            false
        },

        mca_btl_tcp_component_init,  
        NULL,
    }
};

/*
 * utility routines for parameter registration
 */

static inline char* mca_btl_tcp_param_register_string(
                                                     const char* param_name, 
                                                     const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("btl","tcp",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static inline int mca_btl_tcp_param_register_int(
        const char* param_name, 
        int default_value)
{
    int id = mca_base_param_register_int("btl","tcp",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}


/*
 * Data structure for accepting connections.
 */

struct mca_btl_tcp_event_t {
    opal_list_item_t item;
    opal_event_t event;
};
typedef struct mca_btl_tcp_event_t mca_btl_tcp_event_t;

static void mca_btl_tcp_event_construct(mca_btl_tcp_event_t* event)
{
    OPAL_THREAD_LOCK(&mca_btl_tcp_component.tcp_lock);
    opal_list_append(&mca_btl_tcp_component.tcp_events, &event->item);
    OPAL_THREAD_UNLOCK(&mca_btl_tcp_component.tcp_lock);
}

static void mca_btl_tcp_event_destruct(mca_btl_tcp_event_t* event)
{
    OPAL_THREAD_LOCK(&mca_btl_tcp_component.tcp_lock);
    opal_list_remove_item(&mca_btl_tcp_component.tcp_events, &event->item);
    OPAL_THREAD_UNLOCK(&mca_btl_tcp_component.tcp_lock);
}

OBJ_CLASS_INSTANCE(
    mca_btl_tcp_event_t,
    opal_list_item_t,
    mca_btl_tcp_event_construct,
    mca_btl_tcp_event_destruct);


/*
 * functions for receiving event callbacks
 */

static void mca_btl_tcp_component_recv_handler(int, short, void*);


/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_btl_tcp_component_open(void)
{
#ifdef __WINDOWS__
    WSADATA win_sock_data;
    if (WSAStartup(MAKEWORD(2,2), &win_sock_data) != 0) {
        BTL_ERROR(("failed to initialise windows sockets:%d", WSAGetLastError()));
        return OMPI_ERROR;
    }
#endif

    /* initialize state */
    mca_btl_tcp_component.tcp_listen_sd = -1;
    mca_btl_tcp_component.tcp_num_btls=0;
    mca_btl_tcp_component.tcp_btls=NULL;
    
    /* initialize objects */ 
    OBJ_CONSTRUCT(&mca_btl_tcp_component.tcp_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_btl_tcp_component.tcp_procs, opal_hash_table_t);
    OBJ_CONSTRUCT(&mca_btl_tcp_component.tcp_events, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_tcp_component.tcp_frag_eager, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_tcp_component.tcp_frag_max, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_tcp_component.tcp_frag_user, ompi_free_list_t);
    opal_hash_table_init(&mca_btl_tcp_component.tcp_procs, 256);

    /* register TCP component parameters */
    mca_btl_tcp_component.tcp_if_include =
        mca_btl_tcp_param_register_string("if_include", "");
    mca_btl_tcp_component.tcp_if_exclude =
        mca_btl_tcp_param_register_string("if_exclude", "lo");
    mca_btl_tcp_component.tcp_free_list_num =
        mca_btl_tcp_param_register_int ("free_list_num", 8);
    mca_btl_tcp_component.tcp_free_list_max =
        mca_btl_tcp_param_register_int ("free_list_max", -1);
    mca_btl_tcp_component.tcp_free_list_inc =
        mca_btl_tcp_param_register_int ("free_list_inc", 32);
    mca_btl_tcp_component.tcp_sndbuf =
        mca_btl_tcp_param_register_int ("sndbuf", 128*1024);
    mca_btl_tcp_component.tcp_rcvbuf =
        mca_btl_tcp_param_register_int ("rcvbuf", 128*1024);
    mca_btl_tcp_component.tcp_endpoint_cache =
        mca_btl_tcp_param_register_int ("endpoint_cache", 30*1024);
    mca_btl_tcp_module.super.btl_exclusivity =
        mca_btl_tcp_param_register_int ("exclusivity", MCA_BTL_EXCLUSIVITY_LOW);
    mca_btl_tcp_module.super.btl_eager_limit =
        mca_btl_tcp_param_register_int ("eager_limit", 64*1024);
    mca_btl_tcp_module.super.btl_min_send_size =
        mca_btl_tcp_param_register_int ("min_send_size", 64*1024);
    mca_btl_tcp_module.super.btl_max_send_size =
        mca_btl_tcp_param_register_int ("max_send_size", 128*1024);
    
    mca_btl_tcp_module.super.btl_min_rdma_size =
        mca_btl_tcp_param_register_int("min_rdma_size", 128*1024);
    mca_btl_tcp_module.super.btl_max_rdma_size =
        mca_btl_tcp_param_register_int("max_rdma_size", INT_MAX);
    mca_btl_tcp_module.super.btl_flags  =
        mca_btl_tcp_param_register_int("flags", MCA_BTL_FLAGS_PUT |
                                       MCA_BTL_FLAGS_SEND_INPLACE |
                                       MCA_BTL_FLAGS_NEED_CSUM | 
                                       MCA_BTL_FLAGS_NEED_ACK |
                                       MCA_BTL_FLAGS_FAKE_RDMA);
    return OMPI_SUCCESS;
}


/*
 * module cleanup - sanity checking of queue lengths
 */

int mca_btl_tcp_component_close(void)
{
    opal_list_item_t* item;

    if(NULL != mca_btl_tcp_component.tcp_if_include)
        free(mca_btl_tcp_component.tcp_if_include);
    if(NULL != mca_btl_tcp_component.tcp_if_exclude)
       free(mca_btl_tcp_component.tcp_if_exclude);
    if (NULL != mca_btl_tcp_component.tcp_btls)
        free(mca_btl_tcp_component.tcp_btls);
 
    if (mca_btl_tcp_component.tcp_listen_sd >= 0) {
        opal_event_del(&mca_btl_tcp_component.tcp_recv_event);
        CLOSE_THE_SOCKET(mca_btl_tcp_component.tcp_listen_sd);
        mca_btl_tcp_component.tcp_listen_sd = -1;
    }

    /* cleanup any pending events */
    OPAL_THREAD_LOCK(&mca_btl_tcp_component.tcp_lock);
    for(item =  opal_list_remove_first(&mca_btl_tcp_component.tcp_events);
        item != NULL; 
        item =  opal_list_remove_first(&mca_btl_tcp_component.tcp_events)) {
        mca_btl_tcp_event_t* event = (mca_btl_tcp_event_t*)item;
        opal_event_del(&event->event);
        OBJ_RELEASE(event);
    }
    OPAL_THREAD_UNLOCK(&mca_btl_tcp_component.tcp_lock);

    /* release resources */
    OBJ_DESTRUCT(&mca_btl_tcp_component.tcp_procs);
    OBJ_DESTRUCT(&mca_btl_tcp_component.tcp_events);
    OBJ_DESTRUCT(&mca_btl_tcp_component.tcp_frag_eager);
    OBJ_DESTRUCT(&mca_btl_tcp_component.tcp_frag_max);
    OBJ_DESTRUCT(&mca_btl_tcp_component.tcp_frag_user);
    OBJ_DESTRUCT(&mca_btl_tcp_component.tcp_lock);

#ifdef __WINDOWS__
    WSACleanup();
#endif

    return OMPI_SUCCESS;
}


/*
 *  Create a btl instance and add to modules list.
 */

static int mca_btl_tcp_create(int if_index, const char* if_name)
{
    struct mca_btl_tcp_module_t* btl = (struct mca_btl_tcp_module_t *)malloc(sizeof(mca_btl_tcp_module_t));
    char param[256];
    if(NULL == btl)
        return OMPI_ERR_OUT_OF_RESOURCE;
    memcpy(btl, &mca_btl_tcp_module, sizeof(mca_btl_tcp_module));
    OBJ_CONSTRUCT(&btl->tcp_endpoints, opal_list_t);
    mca_btl_tcp_component.tcp_btls[mca_btl_tcp_component.tcp_num_btls++] = btl;

    /* initialize the btl */
    btl->tcp_ifindex = if_index;
#if MCA_BTL_TCP_STATISTICS
    btl->tcp_bytes_recv = 0;
    btl->tcp_bytes_sent = 0;
    btl->tcp_send_handler = 0;
#endif
    opal_ifindextoaddr(if_index, (struct sockaddr*)&btl->tcp_ifaddr, sizeof(btl->tcp_ifaddr));
    opal_ifindextomask(if_index, (struct sockaddr*)&btl->tcp_ifmask, sizeof(btl->tcp_ifmask));

    /* allow user to specify interface bandwidth */
    sprintf(param, "bandwidth_%s", if_name);
    btl->super.btl_bandwidth = mca_btl_tcp_param_register_int(param, 0);

    /* allow user to override/specify latency ranking */
    sprintf(param, "latency_%s", if_name);
    btl->super.btl_latency = mca_btl_tcp_param_register_int(param, 0);

#if 0 && OMPI_ENABLE_DEBUG
    BTL_OUTPUT(("interface: %s bandwidth %d latency %d",
        if_name, btl->super.btl_bandwidth, btl->super.btl_latency));
#endif
    return OMPI_SUCCESS;
}

/*
 * Create a TCP BTL instance for either:
 * (1) all interfaces specified by the user
 * (2) all available interfaces 
 * (3) all available interfaces except for those excluded by the user
 */

static int mca_btl_tcp_component_create_instances(void)
{
    int if_count = opal_ifcount();
    int if_index;
    char **include;
    char **exclude;
    char **argv;

    if(if_count <= 0)
        return OMPI_ERROR;

    /* allocate memory for btls */
    mca_btl_tcp_component.tcp_btls = (mca_btl_tcp_module_t **)malloc(if_count * sizeof(mca_btl_tcp_module_t*));
    if(NULL == mca_btl_tcp_component.tcp_btls)
        return OMPI_ERR_OUT_OF_RESOURCE;

    /* if the user specified an interface list - use these exclusively */
    argv = include = opal_argv_split(mca_btl_tcp_component.tcp_if_include,',');
    while(argv && *argv) {
        char* if_name = *argv;
        int if_index = opal_ifnametoindex(if_name);
        if(if_index < 0) {
            BTL_ERROR(("invalid interface \"%s\"", if_name));
        } else {
            mca_btl_tcp_create(if_index, if_name);
        }
        argv++;
    }
    opal_argv_free(include);
    if(mca_btl_tcp_component.tcp_num_btls)
        return OMPI_SUCCESS;

    /* if the interface list was not specified by the user, create 
     * a BTL for each interface that was not excluded.
    */
    exclude = opal_argv_split(mca_btl_tcp_component.tcp_if_exclude,',');
    for(if_index = opal_ifbegin(); if_index >= 0; if_index = opal_ifnext(if_index)) {
        char if_name[32];
        opal_ifindextoname(if_index, if_name, sizeof(if_name));

        /* check to see if this interface exists in the exclude list */
        if(opal_ifcount() > 1) {
            argv = exclude;
            while(argv && *argv) {
                if(strncmp(*argv,if_name,strlen(*argv)) == 0)
                    break;
                argv++;
            }
            /* if this interface was not found in the excluded list - create a BTL */
            if(argv == 0 || *argv == 0) {
                mca_btl_tcp_create(if_index, if_name);
            }
        } else {
            mca_btl_tcp_create(if_index, if_name);
        }
    }
    opal_argv_free(exclude);
    return OMPI_SUCCESS;
}

/*
 * Create a listen socket and bind to all interfaces
 */

static int mca_btl_tcp_component_create_listen(void)
{
    int flags;
    struct sockaddr_in inaddr; 
    opal_socklen_t addrlen;

    /* create a listen socket for incoming connections */
    mca_btl_tcp_component.tcp_listen_sd = socket(AF_INET, SOCK_STREAM, 0);
    if(mca_btl_tcp_component.tcp_listen_sd < 0) {
        BTL_ERROR(("socket() failed with errno=%d", opal_socket_errno));
        return OMPI_ERROR;
    }
    mca_btl_tcp_set_socket_options(mca_btl_tcp_component.tcp_listen_sd);
                                                                                                      
    /* bind to all addresses and dynamically assigned port */
    memset(&inaddr, 0, sizeof(inaddr));
    inaddr.sin_family = AF_INET;
    inaddr.sin_addr.s_addr = INADDR_ANY;
    inaddr.sin_port = 0;
                                                                                                      
    if(bind(mca_btl_tcp_component.tcp_listen_sd, (struct sockaddr*)&inaddr, sizeof(inaddr)) < 0) {
        BTL_ERROR(("bind() failed with errno=%d", opal_socket_errno));
        return OMPI_ERROR;
    }
                                                                                                      
    /* resolve system assignend port */
    addrlen = sizeof(struct sockaddr_in);
    if(getsockname(mca_btl_tcp_component.tcp_listen_sd, (struct sockaddr*)&inaddr, &addrlen) < 0) {
        BTL_ERROR(("getsockname() failed with errno=%d", opal_socket_errno));
        return OMPI_ERROR;
    }
    mca_btl_tcp_component.tcp_listen_port = inaddr.sin_port;

    /* setup listen backlog to maximum allowed by kernel */
    if(listen(mca_btl_tcp_component.tcp_listen_sd, SOMAXCONN) < 0) {
        BTL_ERROR(("listen() failed with errno=%d", opal_socket_errno));
        return OMPI_ERROR;
    }

    /* set socket up to be non-blocking, otherwise accept could block */
    if((flags = fcntl(mca_btl_tcp_component.tcp_listen_sd, F_GETFL, 0)) < 0) {
        BTL_ERROR(("fcntl(F_GETFL) failed with errno=%d", opal_socket_errno));
        return OMPI_ERROR;
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(mca_btl_tcp_component.tcp_listen_sd, F_SETFL, flags) < 0) {
            BTL_ERROR(("fcntl(F_SETFL) failed with errno=%d", opal_socket_errno));
            return OMPI_ERROR;
        }
    }

    /* register listen port */
    opal_event_set(
        &mca_btl_tcp_component.tcp_recv_event,
        mca_btl_tcp_component.tcp_listen_sd, 
        OPAL_EV_READ|OPAL_EV_PERSIST, 
        mca_btl_tcp_component_recv_handler, 
        0);
    opal_event_add(&mca_btl_tcp_component.tcp_recv_event,0);
    return OMPI_SUCCESS;
}

/*
 *  Register TCP module addressing information. The MCA framework
 *  will make this available to all peers. 
 */

static int mca_btl_tcp_component_exchange(void)
{
     int rc=0;
     size_t i=0;
     size_t size = mca_btl_tcp_component.tcp_num_btls * sizeof(mca_btl_tcp_addr_t);
     if(mca_btl_tcp_component.tcp_num_btls != 0) {
         mca_btl_tcp_addr_t *addrs = (mca_btl_tcp_addr_t *)malloc(size);
         for(i=0; i<mca_btl_tcp_component.tcp_num_btls; i++) {
             struct mca_btl_tcp_module_t* btl = mca_btl_tcp_component.tcp_btls[i];
             addrs[i].addr_inet    = btl->tcp_ifaddr.sin_addr;
             addrs[i].addr_port    = mca_btl_tcp_component.tcp_listen_port;
             addrs[i].addr_inuse   = 0;
         }
         rc =  mca_pml_base_modex_send(&mca_btl_tcp_component.super.btl_version, addrs, size);
         free(addrs);
     }
     return rc;
}

/*
 *  TCP module initialization:
 *  (1) read interface list from kernel and compare against module parameters
 *      then create a BTL instance for selected interfaces
 *  (2) setup TCP listen socket for incoming connection attempts
 *  (3) register BTL parameters with the MCA
 */
mca_btl_base_module_t** mca_btl_tcp_component_init(int *num_btl_modules, 
                                                   bool enable_progress_threads,
                                                   bool enable_mpi_threads)
{
    mca_btl_base_module_t **btls;
    *num_btl_modules = 0;

    /* initialize free lists */
    ompi_free_list_init( &mca_btl_tcp_component.tcp_frag_eager,
                         sizeof (mca_btl_tcp_frag_eager_t) + 
                         mca_btl_tcp_module.super.btl_eager_limit,
                         OBJ_CLASS (mca_btl_tcp_frag_eager_t),
                         mca_btl_tcp_component.tcp_free_list_num,
                         mca_btl_tcp_component.tcp_free_list_max,
                         mca_btl_tcp_component.tcp_free_list_inc,
                         NULL );
                                                                                                                                  
    ompi_free_list_init( &mca_btl_tcp_component.tcp_frag_max,
                         sizeof (mca_btl_tcp_frag_max_t) + 
                         mca_btl_tcp_module.super.btl_max_send_size,
                         OBJ_CLASS (mca_btl_tcp_frag_max_t),
                         mca_btl_tcp_component.tcp_free_list_num,
                         mca_btl_tcp_component.tcp_free_list_max,
                         mca_btl_tcp_component.tcp_free_list_inc,
                         NULL );
                                                                                                                                  
    ompi_free_list_init( &mca_btl_tcp_component.tcp_frag_user,
                         sizeof (mca_btl_tcp_frag_user_t),
                         OBJ_CLASS (mca_btl_tcp_frag_user_t),
                         mca_btl_tcp_component.tcp_free_list_num,
                         mca_btl_tcp_component.tcp_free_list_max,
                         mca_btl_tcp_component.tcp_free_list_inc,
                         NULL );
                                                                                                                                  
    /* create a BTL TCP module for selected interfaces */
    if(mca_btl_tcp_component_create_instances() != OMPI_SUCCESS)
        return 0;

    /* create a TCP listen socket for incoming connection attempts */
    if(mca_btl_tcp_component_create_listen() != OMPI_SUCCESS)
        return 0;

    /* publish TCP parameters with the MCA framework */
    if(mca_btl_tcp_component_exchange() != OMPI_SUCCESS)
        return 0;

    btls = (mca_btl_base_module_t **)malloc(mca_btl_tcp_component.tcp_num_btls * 
                  sizeof(mca_btl_base_module_t*));
    if(NULL == btls)
        return NULL;

    memcpy(btls, mca_btl_tcp_component.tcp_btls, mca_btl_tcp_component.tcp_num_btls*sizeof(mca_btl_tcp_module_t*));
    *num_btl_modules = mca_btl_tcp_component.tcp_num_btls;
    return btls;
}

/*
 *  TCP module control
 */

int mca_btl_tcp_component_control(int param, void* value, size_t size)
{
    return OMPI_SUCCESS;
}


/*
 *  Called by mca_btl_tcp_component_recv() when the TCP listen
 *  socket has pending connection requests. Accept incoming
 *  requests and queue for completion of the connection handshake.
*/


static void mca_btl_tcp_component_accept(void)
{
    while(true) {
        opal_socklen_t addrlen = sizeof(struct sockaddr_in);
        struct sockaddr_in addr;
        mca_btl_tcp_event_t *event;
        int sd = accept(mca_btl_tcp_component.tcp_listen_sd, (struct sockaddr*)&addr, &addrlen);
        if(sd < 0) {
            if(opal_socket_errno == EINTR)
                continue;
            if(opal_socket_errno != EAGAIN && opal_socket_errno != EWOULDBLOCK)
                BTL_ERROR(("accept() failed with errno %d.", opal_socket_errno));
            return;
        }
        mca_btl_tcp_set_socket_options(sd);

        /* wait for receipt of peers process identifier to complete this connection */
         
        event = OBJ_NEW(mca_btl_tcp_event_t);
        opal_event_set(&event->event, sd, OPAL_EV_READ, mca_btl_tcp_component_recv_handler, event);
        opal_event_add(&event->event, 0);
    }
}


/*
 * Event callback when there is data available on the registered 
 * socket to recv.
 */
static void mca_btl_tcp_component_recv_handler(int sd, short flags, void* user)
{
    orte_process_name_t guid;
    struct sockaddr_in addr;
    int retval;
    mca_btl_tcp_proc_t* btl_proc;
    opal_socklen_t addr_len = sizeof(addr);
    mca_btl_tcp_event_t *event = (mca_btl_tcp_event_t *)user;

    /* accept new connections on the listen socket */
    if(mca_btl_tcp_component.tcp_listen_sd == sd) {
        mca_btl_tcp_component_accept();
        return;
    }
    OBJ_RELEASE(event);

    /* recv the process identifier */
    retval = recv(sd, (char *)&guid, sizeof(guid), 0);
    if(retval != sizeof(guid)) {
        CLOSE_THE_SOCKET(sd);
        return;
    }
    ORTE_PROCESS_NAME_NTOH(guid);

    /* now set socket up to be non-blocking */
    if((flags = fcntl(sd, F_GETFL, 0)) < 0) {
        BTL_ERROR(("fcntl(F_GETFL) failed with errno=%d", opal_socket_errno));
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(sd, F_SETFL, flags) < 0) {
            BTL_ERROR(("fcntl(F_SETFL) failed with errno=%d", opal_socket_errno));
        }
    }
   
    /* lookup the corresponding process */
    btl_proc = mca_btl_tcp_proc_lookup(&guid);
    if(NULL == btl_proc) {
        BTL_ERROR(("errno=%d",errno));
        CLOSE_THE_SOCKET(sd);
        return;
    }

    /* lookup peer address */
    if(getpeername(sd, (struct sockaddr*)&addr, &addr_len) != 0) {
        BTL_ERROR(("getpeername() failed with errno=%d", opal_socket_errno));
        CLOSE_THE_SOCKET(sd);
        return;
    }

    /* are there any existing peer instances will to accept this connection */
    if(mca_btl_tcp_proc_accept(btl_proc, &addr, sd) == false) {
        CLOSE_THE_SOCKET(sd);
        return;
    }
}

