/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2012 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_BTL_TCP_H
#define MCA_BTL_TCP_H

#include "opal_config.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* Open MPI includes */
#include "opal/mca/event/event.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/btl/base/base.h"
#include "opal/mca/mpool/mpool.h"
#include "opal/class/opal_hash_table.h"

#define MCA_BTL_TCP_STATISTICS 0
BEGIN_C_DECLS


/**
 * TCP BTL component.
 */

struct mca_btl_tcp_component_t {
    mca_btl_base_component_3_0_0_t super;   /**< base BTL component */
    uint32_t tcp_addr_count;                /**< total number of addresses */
    uint32_t tcp_num_btls;                  /**< number of interfaces available to the TCP component */
    unsigned int tcp_num_links;             /**< number of logical links per physical device */
    struct mca_btl_tcp_module_t **tcp_btls; /**< array of available BTL modules */
    struct mca_btl_tcp_proc_t* tcp_local;   /**< local proc struct */
    int tcp_free_list_num;                  /**< initial size of free lists */
    int tcp_free_list_max;                  /**< maximum size of free lists */
    int tcp_free_list_inc;                  /**< number of elements to alloc when growing free lists */
    int tcp_endpoint_cache;                 /**< amount of cache on each endpoint */
    opal_proc_table_t tcp_procs;            /**< hash table of tcp proc structures */
    opal_mutex_t tcp_lock;                  /**< lock for accessing module state */

    opal_event_t tcp_recv_event;            /**< recv event for IPv4 listen socket */
    int tcp_listen_sd;                      /**< IPv4 listen socket for incoming connection requests */
    unsigned short tcp_listen_port;         /**< IPv4 listen port */
    int tcp_port_min;                       /**< IPv4 minimum port */
    int tcp_port_range;                     /**< IPv4 port range */
#if OPAL_ENABLE_IPV6
    opal_event_t tcp6_recv_event;           /**< recv event for IPv6 listen socket */
    int tcp6_listen_sd;                     /**< IPv6 listen socket for incoming connection requests */
    unsigned short tcp6_listen_port;        /**< IPv6 listen port */
    int tcp6_port_min;                      /**< IPv4 minimum port */
    int tcp6_port_range;                    /**< IPv4 port range */
#endif
    /* Port range restriction */

    char*  tcp_if_include;                  /**< comma seperated list of interface to include */
    char*  tcp_if_exclude;                  /**< comma seperated list of interface to exclude */
    int    tcp_sndbuf;                      /**< socket sndbuf size */
    int    tcp_rcvbuf;                      /**< socket rcvbuf size */
    int    tcp_disable_family;              /**< disabled AF_family */

    /* free list of fragment descriptors */
    opal_free_list_t tcp_frag_eager;
    opal_free_list_t tcp_frag_max;
    opal_free_list_t tcp_frag_user;

    /* Do we want to use TCP_NODELAY? */
    int    tcp_not_use_nodelay;

    /* do we want to warn on all excluded interfaces
     * that are not found?
     */
    bool report_all_unfound_interfaces;
};
typedef struct mca_btl_tcp_component_t mca_btl_tcp_component_t;

OPAL_MODULE_DECLSPEC extern mca_btl_tcp_component_t mca_btl_tcp_component;

/**
 * BTL Module Interface
 */
struct mca_btl_tcp_module_t {
    mca_btl_base_module_t  super;  /**< base BTL interface */
    uint16_t           tcp_ifkindex; /** <BTL kernel interface index */
#if 0
    int                tcp_ifindex; /**< BTL interface index */
#endif
    struct sockaddr_storage tcp_ifaddr; /**< BTL interface address */
    uint32_t           tcp_ifmask;  /**< BTL interface netmask */
    opal_list_t        tcp_endpoints;
    mca_btl_base_module_error_cb_fn_t tcp_error_cb;  /**< Upper layer error callback */
#if MCA_BTL_TCP_STATISTICS
    size_t tcp_bytes_sent;
    size_t tcp_bytes_recv;
    size_t tcp_send_handler;
#endif
};
typedef struct mca_btl_tcp_module_t mca_btl_tcp_module_t;
extern mca_btl_tcp_module_t mca_btl_tcp_module;

#define CLOSE_THE_SOCKET(socket)   {(void)shutdown(socket, SHUT_RDWR); (void)close(socket);}

/**
 * TCP component initialization.
 *
 * @param num_btl_modules (OUT)           Number of BTLs returned in BTL array.
 * @param allow_multi_user_threads (OUT)  Flag indicating wether BTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       Flag indicating wether BTL uses threads (TRUE)
 */
extern mca_btl_base_module_t** mca_btl_tcp_component_init(
    int *num_btl_modules,
    bool allow_multi_user_threads,
    bool have_hidden_threads
);


/**
 * TCP component progress.
 */
extern int mca_btl_tcp_component_progress(void);



/**
 * Cleanup any resources held by the BTL.
 *
 * @param btl  BTL instance.
 * @return     OPAL_SUCCESS or error status on failure.
 */

extern int mca_btl_tcp_finalize(
    struct mca_btl_base_module_t* btl
);


/**
 * PML->BTL notification of change in the process list.
 *
 * @param btl (IN)
 * @param nprocs (IN)     Number of processes
 * @param procs (IN)      Set of processes
 * @param peers (OUT)     Set of (optional) peer addressing info.
 * @param peers (IN/OUT)  Set of processes that are reachable via this BTL.
 * @return     OPAL_SUCCESS or error status on failure.
 *
 */

extern int mca_btl_tcp_add_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct opal_proc_t **procs,
    struct mca_btl_base_endpoint_t** peers,
    opal_bitmap_t* reachable
);

/**
 * PML->BTL notification of change in the process list.
 *
 * @param btl (IN)     BTL instance
 * @param nproc (IN)   Number of processes.
 * @param procs (IN)   Set of processes.
 * @param peers (IN)   Set of peer data structures.
 * @return             Status indicating if cleanup was successful
 *
 */

extern int mca_btl_tcp_del_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct opal_proc_t **procs,
    struct mca_btl_base_endpoint_t** peers
);


/**
 * Initiate an asynchronous send.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transfered
 * @param tag (IN)         The tag value used to notify the peer.
 */

extern int mca_btl_tcp_send(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* btl_peer,
    struct mca_btl_base_descriptor_t* descriptor,
    mca_btl_base_tag_t tag
);


/**
 * Initiate an asynchronous put.
 */

int mca_btl_tcp_put (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                     uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                     mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                     int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);


/**
 * Initiate an asynchronous get.
 */

int mca_btl_tcp_get (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                     uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                     mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                     int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

/**
 * Allocate a descriptor with a segment of the requested size.
 * Note that the BTL layer may choose to return a smaller size
 * if it cannot support the request.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */

extern mca_btl_base_descriptor_t* mca_btl_tcp_alloc(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    uint8_t order,
    size_t size,
    uint32_t flags);


/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param descriptor (IN)  Allocated descriptor.
 */

extern int mca_btl_tcp_free(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_descriptor_t* des);


/**
 * Prepare a descriptor for send/rdma using the supplied
 * convertor. If the convertor references data that is contigous,
 * the descriptor may simply point to the user buffer. Otherwise,
 * this routine is responsible for allocating buffer space and
 * packing if required.
 *
 * @param btl (IN)          BTL module
 * @param endpoint (IN)     BTL peer addressing
 * @param convertor (IN)    Data type convertor
 * @param reserve (IN)      Additional bytes requested by upper layer to precede user data
 * @param size (IN/OUT)     Number of bytes to prepare (IN), number of bytes actually prepared (OUT)
*/

mca_btl_base_descriptor_t* mca_btl_tcp_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* peer,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags
);


/**
  * Fault Tolerance Event Notification Function
  * @param state Checkpoint Stae
  * @return OPAL_SUCCESS or failure status
  */
int mca_btl_tcp_ft_event(int state);

END_C_DECLS
#endif
