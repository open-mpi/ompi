/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Laboratory
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2013-2015 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "opal_config.h"

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
#if OPAL_ENABLE_IPV6
#  ifdef HAVE_NETDB_H
#  include <netdb.h>
#  endif
#endif
#include <ctype.h>
#include <limits.h>

#include "opal/mca/event/event.h"
#include "opal/util/if.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/util/net.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"
#include "opal/constants.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/btl/base/base.h" 
#include "opal/mca/mpool/base/base.h" 
#include "opal/mca/btl/base/btl_base_error.h"
#include "opal/mca/pmix/pmix.h"

#include "btl_tcp.h"
#include "btl_tcp_addr.h"
#include "btl_tcp_proc.h"
#include "btl_tcp_frag.h"
#include "btl_tcp_endpoint.h" 
#if OPAL_CUDA_SUPPORT
#include "opal/mca/common/cuda/common_cuda.h"
#endif /* OPAL_CUDA_SUPPORT */

/* 
 * Local functions
 */
static int mca_btl_tcp_component_register(void);
static int mca_btl_tcp_component_open(void);
static int mca_btl_tcp_component_close(void);

mca_btl_tcp_component_t mca_btl_tcp_component = {
    .super = {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        .btl_version = {
            MCA_BTL_DEFAULT_VERSION("tcp"),
            .mca_open_component = mca_btl_tcp_component_open,
            .mca_close_component = mca_btl_tcp_component_close,
            .mca_register_component_params = mca_btl_tcp_component_register,
        },
        .btl_data = {
            /* The component is checkpoint ready */
            .param_field = MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        .btl_init = mca_btl_tcp_component_init,
    }
};

/*
 * utility routines for parameter registration
 */

static inline char* mca_btl_tcp_param_register_string(
        const char* param_name, 
        const char* help_string,
        const char* default_value,
        int level,
        char **storage)
{
    *storage = (char *) default_value;
    (void) mca_base_component_var_register(&mca_btl_tcp_component.super.btl_version,
                                           param_name, help_string, MCA_BASE_VAR_TYPE_STRING,
                                           NULL, 0, 0, level,
                                           MCA_BASE_VAR_SCOPE_READONLY, storage);
    return *storage;
}

static inline int mca_btl_tcp_param_register_int(
        const char* param_name, 
        const char* help_string,
        int default_value,
        int level,
        int *storage)
{
    *storage = default_value;
    (void) mca_base_component_var_register(&mca_btl_tcp_component.super.btl_version,
                                           param_name, help_string, MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, 0, level,
                                           MCA_BASE_VAR_SCOPE_READONLY, storage);
    return *storage;
}

static inline unsigned int mca_btl_tcp_param_register_uint(
        const char* param_name, 
        const char* help_string,
        unsigned int default_value,
        int level,
        unsigned int *storage)
{
    *storage = default_value;
    (void) mca_base_component_var_register(&mca_btl_tcp_component.super.btl_version,
                                           param_name, help_string, MCA_BASE_VAR_TYPE_UNSIGNED_INT,
                                           NULL, 0, 0, level,
                                           MCA_BASE_VAR_SCOPE_READONLY, storage);
    return *storage;
}


/*
 * Data structure for accepting connections.
 */

struct mca_btl_tcp_event_t {
    opal_list_item_t item;
    opal_event_t event;
};
typedef struct mca_btl_tcp_event_t mca_btl_tcp_event_t;

OBJ_CLASS_INSTANCE( mca_btl_tcp_event_t, opal_list_item_t,
                    NULL, NULL);


/*
 * functions for receiving event callbacks
 */
static void mca_btl_tcp_component_recv_handler(int, short, void*);
static void mca_btl_tcp_component_accept_handler(int, short, void*);

static int mca_btl_tcp_component_verify(void)
{
    if( mca_btl_tcp_component.tcp_port_min > USHRT_MAX ) {
        opal_show_help("help-mpi-btl-tcp.txt", "invalid minimum port",
                       true, "v4", opal_process_info.nodename,
                       mca_btl_tcp_component.tcp_port_min );
        mca_btl_tcp_component.tcp_port_min = 1024;
    }
#if OPAL_ENABLE_IPV6
    if( mca_btl_tcp_component.tcp6_port_min > USHRT_MAX ) {
        opal_show_help("help-mpi-btl-tcp.txt", "invalid minimum port",
                       true, "v6", opal_process_info.nodename,
                       mca_btl_tcp_component.tcp6_port_min );
        mca_btl_tcp_component.tcp6_port_min = 1024;
    }
#endif

    return OPAL_SUCCESS;
}

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

static int mca_btl_tcp_component_register(void)
{
    char* message;

    /* register TCP component parameters */
    mca_btl_tcp_param_register_uint("links", NULL, 1, OPAL_INFO_LVL_4, &mca_btl_tcp_component.tcp_num_links);
    mca_btl_tcp_param_register_string("if_include", "Comma-delimited list of devices and/or CIDR notation of networks to use for MPI communication (e.g., \"eth0,192.168.0.0/16\").  Mutually exclusive with btl_tcp_if_exclude.", "", OPAL_INFO_LVL_1, &mca_btl_tcp_component.tcp_if_include);
    mca_btl_tcp_param_register_string("if_exclude", "Comma-delimited list of devices and/or CIDR notation of networks to NOT use for MPI communication -- all devices not matching these specifications will be used (e.g., \"eth0,192.168.0.0/16\").  If set to a non-default value, it is mutually exclusive with btl_tcp_if_include.", 
                                      "127.0.0.1/8,sppp",
                                      OPAL_INFO_LVL_1, &mca_btl_tcp_component.tcp_if_exclude);

    mca_btl_tcp_param_register_int ("free_list_num", NULL, 8, OPAL_INFO_LVL_5,  &mca_btl_tcp_component.tcp_free_list_num);
    mca_btl_tcp_param_register_int ("free_list_max", NULL, -1, OPAL_INFO_LVL_5,  &mca_btl_tcp_component.tcp_free_list_max);
    mca_btl_tcp_param_register_int ("free_list_inc", NULL, 32, OPAL_INFO_LVL_5,  &mca_btl_tcp_component.tcp_free_list_inc);
    mca_btl_tcp_param_register_int ("sndbuf", NULL, 128*1024, OPAL_INFO_LVL_4, &mca_btl_tcp_component.tcp_sndbuf);
    mca_btl_tcp_param_register_int ("rcvbuf", NULL, 128*1024, OPAL_INFO_LVL_4, &mca_btl_tcp_component.tcp_rcvbuf);
    mca_btl_tcp_param_register_int ("endpoint_cache",
        "The size of the internal cache for each TCP connection. This cache is"
        " used to reduce the number of syscalls, by replacing them with memcpy."
        " Every read will read the expected data plus the amount of the"
                                    " endpoint_cache", 30*1024, OPAL_INFO_LVL_4, &mca_btl_tcp_component.tcp_endpoint_cache);
    mca_btl_tcp_param_register_int ("use_nagle", "Whether to use Nagle's algorithm or not (using Nagle's algorithm may increase short message latency)", 0, OPAL_INFO_LVL_4, &mca_btl_tcp_component.tcp_not_use_nodelay);
    mca_btl_tcp_param_register_int( "port_min_v4", 
                                    "The minimum port where the TCP BTL will try to bind (default 1024)",
                                    1024, OPAL_INFO_LVL_2, &mca_btl_tcp_component.tcp_port_min);

    asprintf( &message, 
              "The number of ports where the TCP BTL will try to bind (default %d)."
              " This parameter together with the port min, define a range of ports"
              " where Open MPI will open sockets.",
              (0x1 << 16) - mca_btl_tcp_component.tcp_port_min - 1 );
    mca_btl_tcp_param_register_int( "port_range_v4", message,
                                    (0x1 << 16) - mca_btl_tcp_component.tcp_port_min - 1,
                                    OPAL_INFO_LVL_2, &mca_btl_tcp_component.tcp_port_range);
    free(message);
#if OPAL_ENABLE_IPV6
    mca_btl_tcp_param_register_int( "port_min_v6",
                                    "The minimum port where the TCP BTL will try to bind (default 1024)", 1024,
                                    OPAL_INFO_LVL_2, & mca_btl_tcp_component.tcp6_port_min );
    asprintf( &message, 
              "The number of ports where the TCP BTL will try to bind (default %d)."
              " This parameter together with the port min, define a range of ports"
              " where Open MPI will open sockets.",
              (0x1 << 16) - mca_btl_tcp_component.tcp6_port_min - 1 );
    mca_btl_tcp_param_register_int( "port_range_v6", message,
                                    (0x1 << 16) - mca_btl_tcp_component.tcp6_port_min - 1,
                                    OPAL_INFO_LVL_2, &mca_btl_tcp_component.tcp6_port_range );
    free(message);
#endif

    mca_btl_tcp_component.report_all_unfound_interfaces = false;
    (void) mca_base_component_var_register(&mca_btl_tcp_component.super.btl_version,
                                           "warn_all_unfound_interfaces",
                                           "Issue a warning for all unfound interfaces included in if_exclude",
                                           MCA_BASE_VAR_TYPE_BOOL,
                                           NULL, 0, 0, OPAL_INFO_LVL_2,
                                           MCA_BASE_VAR_SCOPE_READONLY, &mca_btl_tcp_component.report_all_unfound_interfaces);

    mca_btl_tcp_module.super.btl_exclusivity =  MCA_BTL_EXCLUSIVITY_LOW + 100;
    mca_btl_tcp_module.super.btl_eager_limit = 64*1024;
    mca_btl_tcp_module.super.btl_rndv_eager_limit = 64*1024;
    mca_btl_tcp_module.super.btl_max_send_size = 128*1024;
    mca_btl_tcp_module.super.btl_rdma_pipeline_send_length = 128*1024;
    mca_btl_tcp_module.super.btl_rdma_pipeline_frag_size = INT_MAX;
    mca_btl_tcp_module.super.btl_min_rdma_pipeline_size = 0;
    mca_btl_tcp_module.super.btl_flags = MCA_BTL_FLAGS_PUT |
                                       MCA_BTL_FLAGS_SEND_INPLACE |
                                       MCA_BTL_FLAGS_NEED_CSUM |
                                       MCA_BTL_FLAGS_NEED_ACK |
                                       MCA_BTL_FLAGS_HETEROGENEOUS_RDMA;

    mca_btl_tcp_module.super.btl_bandwidth = 100;
    mca_btl_tcp_module.super.btl_latency = 100;

    mca_btl_base_param_register(&mca_btl_tcp_component.super.btl_version,
                                &mca_btl_tcp_module.super);

    mca_btl_tcp_param_register_int ("disable_family", NULL, 0, OPAL_INFO_LVL_2,  &mca_btl_tcp_component.tcp_disable_family);

    return mca_btl_tcp_component_verify();
}

static int mca_btl_tcp_component_open(void)
{
    if (OPAL_SUCCESS != mca_btl_tcp_component_verify()) {
        return OPAL_ERROR;
    }

    /* initialize state */
    mca_btl_tcp_component.tcp_listen_sd = -1;
#if OPAL_ENABLE_IPV6
    mca_btl_tcp_component.tcp6_listen_sd = -1;
#endif
    mca_btl_tcp_component.tcp_num_btls=0;
    mca_btl_tcp_component.tcp_addr_count = 0;
    mca_btl_tcp_component.tcp_btls=NULL;
    
    /* initialize objects */ 
    OBJ_CONSTRUCT(&mca_btl_tcp_component.tcp_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_btl_tcp_component.tcp_procs, opal_proc_table_t);
    OBJ_CONSTRUCT(&mca_btl_tcp_component.tcp_frag_eager, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_tcp_component.tcp_frag_max, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_tcp_component.tcp_frag_user, opal_free_list_t);
    opal_proc_table_init(&mca_btl_tcp_component.tcp_procs, 16, 256);

    /* if_include and if_exclude need to be mutually exclusive */
    if (OPAL_SUCCESS != 
        mca_base_var_check_exclusive("ompi",
        mca_btl_tcp_component.super.btl_version.mca_type_name,
        mca_btl_tcp_component.super.btl_version.mca_component_name,
        "if_include",
        mca_btl_tcp_component.super.btl_version.mca_type_name,
        mca_btl_tcp_component.super.btl_version.mca_component_name,
        "if_exclude")) {
        /* Return ERR_NOT_AVAILABLE so that a warning message about
           "open" failing is not printed */
        return OPAL_ERR_NOT_AVAILABLE;
    }
    
    return OPAL_SUCCESS;
}


/*
 * module cleanup - sanity checking of queue lengths
 */

static int mca_btl_tcp_component_close(void)
{
    if (NULL != mca_btl_tcp_component.tcp_btls)
        free(mca_btl_tcp_component.tcp_btls);
  
    if (mca_btl_tcp_component.tcp_listen_sd >= 0) {
        opal_event_del(&mca_btl_tcp_component.tcp_recv_event);
        CLOSE_THE_SOCKET(mca_btl_tcp_component.tcp_listen_sd);
        mca_btl_tcp_component.tcp_listen_sd = -1;
    }
#if OPAL_ENABLE_IPV6
    if (mca_btl_tcp_component.tcp6_listen_sd >= 0) {
        opal_event_del(&mca_btl_tcp_component.tcp6_recv_event);
        CLOSE_THE_SOCKET(mca_btl_tcp_component.tcp6_listen_sd);
        mca_btl_tcp_component.tcp6_listen_sd = -1;
    }
#endif

    /* release resources */
    OBJ_DESTRUCT(&mca_btl_tcp_component.tcp_procs);
    OBJ_DESTRUCT(&mca_btl_tcp_component.tcp_frag_eager);
    OBJ_DESTRUCT(&mca_btl_tcp_component.tcp_frag_max);
    OBJ_DESTRUCT(&mca_btl_tcp_component.tcp_frag_user);
    OBJ_DESTRUCT(&mca_btl_tcp_component.tcp_lock);

#if OPAL_CUDA_SUPPORT
    mca_common_cuda_fini();
#endif /* OPAL_CUDA_SUPPORT */

    return OPAL_SUCCESS;
}


/*
 *  Create a btl instance and add to modules list.
 */

static int mca_btl_tcp_create(int if_kindex, const char* if_name)
{
    struct mca_btl_tcp_module_t* btl;
    char param[256];
    int i;

    for( i = 0; i < (int)mca_btl_tcp_component.tcp_num_links; i++ ) {
        btl = (struct mca_btl_tcp_module_t *)malloc(sizeof(mca_btl_tcp_module_t));
        if(NULL == btl)
            return OPAL_ERR_OUT_OF_RESOURCE;
        memcpy(btl, &mca_btl_tcp_module, sizeof(mca_btl_tcp_module));
        OBJ_CONSTRUCT(&btl->tcp_endpoints, opal_list_t);
        mca_btl_tcp_component.tcp_btls[mca_btl_tcp_component.tcp_num_btls++] = btl;

        /* initialize the btl */
        btl->tcp_ifkindex = (uint16_t) if_kindex;
#if MCA_BTL_TCP_STATISTICS
        btl->tcp_bytes_recv = 0;
        btl->tcp_bytes_sent = 0;
        btl->tcp_send_handler = 0;
#endif

        /* allow user to specify interface bandwidth */
        sprintf(param, "bandwidth_%s", if_name);
        mca_btl_tcp_param_register_uint(param, NULL, btl->super.btl_bandwidth, OPAL_INFO_LVL_5, &btl->super.btl_bandwidth);

        /* allow user to override/specify latency ranking */
        sprintf(param, "latency_%s", if_name);
        mca_btl_tcp_param_register_uint(param, NULL, btl->super.btl_latency, OPAL_INFO_LVL_5,  &btl->super.btl_latency);
        if( i > 0 ) {
            btl->super.btl_bandwidth >>= 1;
            btl->super.btl_latency   <<= 1;
        }

        /* allow user to specify interface bandwidth */
        sprintf(param, "bandwidth_%s:%d", if_name, i);
        mca_btl_tcp_param_register_uint(param, NULL, btl->super.btl_bandwidth, OPAL_INFO_LVL_5, &btl->super.btl_bandwidth);

        /* allow user to override/specify latency ranking */
        sprintf(param, "latency_%s:%d", if_name, i);
        mca_btl_tcp_param_register_uint(param, NULL, btl->super.btl_latency, OPAL_INFO_LVL_5, &btl->super.btl_latency);
#if 0 && OPAL_ENABLE_DEBUG
        BTL_OUTPUT(("interface %s instance %i: bandwidth %d latency %d\n", if_name, i,
                    btl->super.btl_bandwidth, btl->super.btl_latency));
#endif
    }
    return OPAL_SUCCESS;
}

/*
 * Go through a list of argv; if there are any subnet specifications
 * (a.b.c.d/e), resolve them to an interface name (Currently only
 * supporting IPv4).  If unresolvable, warn and remove.
 */
static char **split_and_resolve(char **orig_str, char *name, bool reqd)
{
    int i, ret, save, if_index;
    char **argv, *str, *tmp;
    char if_name[IF_NAMESIZE];
    struct sockaddr_storage argv_inaddr, if_inaddr;
    uint32_t argv_prefix;

    /* Sanity check */
    if (NULL == orig_str || NULL == *orig_str) {
        return NULL;
    }

    argv = opal_argv_split(*orig_str, ',');
    if (NULL == argv) {
        return NULL;
    }
    for (save = i = 0; NULL != argv[i]; ++i) {
        if (isalpha(argv[i][0])) {
            argv[save++] = argv[i];
            continue;
        }

        /* Found a subnet notation.  Convert it to an IP
           address/netmask.  Get the prefix first. */
        argv_prefix = 0;
        tmp = strdup(argv[i]);
        str = strchr(argv[i], '/');
        if (NULL == str) {
            opal_show_help("help-mpi-btl-tcp.txt", "invalid if_inexclude",
                           true, name, opal_process_info.nodename, 
                           tmp, "Invalid specification (missing \"/\")");
            free(argv[i]);
            free(tmp);
            continue;
        }
        *str = '\0';
        argv_prefix = atoi(str + 1);

        /* Now convert the IPv4 address */
        ((struct sockaddr*) &argv_inaddr)->sa_family = AF_INET;
        ret = inet_pton(AF_INET, argv[i], 
                        &((struct sockaddr_in*) &argv_inaddr)->sin_addr);
        free(argv[i]);

        if (1 != ret) {
            opal_show_help("help-mpi-btl-tcp.txt", "invalid if_inexclude",
                           true, name, opal_process_info.nodename, tmp,
                           "Invalid specification (inet_pton() failed)");
            free(tmp);
            continue;
        }
        opal_output_verbose(20, opal_btl_base_framework.framework_output, 
                            "btl: tcp: Searching for %s address+prefix: %s / %u",
                            name,
                            opal_net_get_hostname((struct sockaddr*) &argv_inaddr),
                            argv_prefix);
            
        /* Go through all interfaces and see if we can find a match */
        for (if_index = opal_ifbegin(); if_index >= 0; 
             if_index = opal_ifnext(if_index)) {
            opal_ifindextoaddr(if_index, 
                               (struct sockaddr*) &if_inaddr,
                               sizeof(if_inaddr));
            if (opal_net_samenetwork((struct sockaddr*) &argv_inaddr,
                                     (struct sockaddr*) &if_inaddr,
                                     argv_prefix)) {
                break;
            }
        }
        
        /* If we didn't find a match, keep trying */
        if (if_index < 0) {
            if (reqd || mca_btl_tcp_component.report_all_unfound_interfaces) {
                opal_show_help("help-mpi-btl-tcp.txt", "invalid if_inexclude",
                               true, name, opal_process_info.nodename, tmp,
                               "Did not find interface matching this subnet");
            }
            free(tmp);
            continue;
        }

        /* We found a match; get the name and replace it in the
           argv */
        opal_ifindextoname(if_index, if_name, sizeof(if_name));
        opal_output_verbose(20, opal_btl_base_framework.framework_output, 
                            "btl: tcp: Found match: %s (%s)",
                            opal_net_get_hostname((struct sockaddr*) &if_inaddr),
                            if_name);
        argv[save++] = strdup(if_name);
        free(tmp);
    }

    /* The list may have been compressed if there were invalid
       entries, so ensure we end it with a NULL entry */
    argv[save] = NULL;
    free(*orig_str);
    *orig_str = opal_argv_join(argv, ',');
    return argv;
}


/*
 * Create a TCP BTL instance for either:
 * (1) all interfaces specified by the user
 * (2) all available interfaces 
 * (3) all available interfaces except for those excluded by the user
 */

static int mca_btl_tcp_component_create_instances(void)
{
    const int if_count = opal_ifcount();
    int if_index;
    int kif_count = 0;
    int *kindexes; /* this array is way too large, but never too small */
    char **include = NULL;
    char **exclude = NULL;
    char **argv;
    int ret = OPAL_SUCCESS;

    if(if_count <= 0) {
        return OPAL_ERROR;
    }

    kindexes = (int *) malloc(sizeof(int) * if_count);
    if (NULL == kindexes) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* calculate the number of kernel indexes (number of physical NICs) */
    {
        int j;

        /* initialize array to 0. Assumption: 0 isn't a valid kernel index */
        memset (kindexes, 0, sizeof(int) * if_count);

        /* assign the corresponding kernel indexes for all opal_list indexes
         * (loop over all addresses)
         */
        for(if_index = opal_ifbegin(); if_index >= 0; if_index = opal_ifnext(if_index)){
            int index = opal_ifindextokindex (if_index);
            if (index > 0) {
                bool want_this_if = true;

                /* Have we seen this if already? */
                for (j = 0; want_this_if && (j < kif_count); j++) {
                    if (kindexes[j] == index) {
                        want_this_if = false;
                    }
                }

                if (want_this_if) {
                    kindexes[kif_count] = index;
                    kif_count++;
                }
            }
        }
    }

    /* allocate memory for btls */
    mca_btl_tcp_component.tcp_btls = (mca_btl_tcp_module_t**)malloc(mca_btl_tcp_component.tcp_num_links *
                                                                    kif_count * sizeof(mca_btl_tcp_module_t*));
    if(NULL == mca_btl_tcp_component.tcp_btls) {
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    mca_btl_tcp_component.tcp_addr_count = if_count;

    /* if the user specified an interface list - use these exclusively */
    argv = include = split_and_resolve(&mca_btl_tcp_component.tcp_if_include,
                                       "include", true);
    while(argv && *argv) {
        char* if_name = *argv;
        int if_index = opal_ifnametokindex(if_name);
        if(if_index < 0) {
            BTL_ERROR(("invalid interface \"%s\"", if_name));
            ret = OPAL_ERR_NOT_FOUND;
            goto cleanup;
        }
        mca_btl_tcp_create(if_index, if_name);
        argv++;
    }

    /* If we made any modules, then the "include" list was non-empty,
       and therefore we're done. */
    if (mca_btl_tcp_component.tcp_num_btls > 0) {
        ret = OPAL_SUCCESS;
        goto cleanup;
    }

    /* if the interface list was not specified by the user, create 
     * a BTL for each interface that was not excluded.
    */
    exclude = split_and_resolve(&mca_btl_tcp_component.tcp_if_exclude,
                                "exclude", false);
    {
        int i;
        for(i = 0; i < kif_count; i++) {
            /* IF_NAMESIZE is defined in opal/util/if.h */
            char if_name[IF_NAMESIZE];
            if_index = kindexes[i];

            opal_ifkindextoname(if_index, if_name, sizeof(if_name));

            /* check to see if this interface exists in the exclude list */
            argv = exclude;
            while(argv && *argv) {
                if(strncmp(*argv,if_name,strlen(*argv)) == 0)
                    break;
                argv++;
            }
            /* if this interface was not found in the excluded list, create a BTL */
            if(argv == 0 || *argv == 0) {
                mca_btl_tcp_create(if_index, if_name);
            }
        }
    }

 cleanup:
    if (NULL != include) {
        opal_argv_free(include);
    }
    if (NULL != exclude) {
        opal_argv_free(exclude);
    }
    if (NULL != kindexes) {
        free(kindexes);
    }
    return ret;
}

/*
 * Create a listen socket and bind to all interfaces
 */

static int mca_btl_tcp_component_create_listen(uint16_t af_family)
{
    int flags;
    int sd;
    struct sockaddr_storage inaddr;
    opal_socklen_t addrlen;

    /* create a listen socket for incoming connections */
    sd = socket(af_family, SOCK_STREAM, 0);
    if(sd < 0) {
        if (EAFNOSUPPORT != opal_socket_errno) {
            BTL_ERROR(("socket() failed: %s (%d)",
                       strerror(opal_socket_errno), opal_socket_errno));
        }
        return OPAL_ERR_IN_ERRNO;
    }

    mca_btl_tcp_set_socket_options(sd);

#if OPAL_ENABLE_IPV6
    {
        struct addrinfo hints, *res = NULL;
        int error;

        memset (&hints, 0, sizeof(hints));
        hints.ai_family = af_family;
        hints.ai_socktype = SOCK_STREAM;
        hints.ai_flags = AI_PASSIVE;

        if ((error = getaddrinfo(NULL, "0", &hints, &res))) {
            opal_output (0,
               "mca_btl_tcp_create_listen: unable to resolve. %s\n",
               gai_strerror (error));
            CLOSE_THE_SOCKET(sd);
            return OPAL_ERROR;
        }

        memcpy (&inaddr, res->ai_addr, res->ai_addrlen);
        addrlen = res->ai_addrlen;
        freeaddrinfo (res);

#ifdef IPV6_V6ONLY
        /* in case of AF_INET6, disable v4-mapped addresses */
        if (AF_INET6 == af_family) {
            int flg = 1;
            if (setsockopt (sd, IPPROTO_IPV6, IPV6_V6ONLY,
                            (char *) &flg, sizeof (flg)) < 0) {
                opal_output(0,
                    "mca_btl_tcp_create_listen: unable to disable v4-mapped addresses\n");
            }
        }
#endif /* IPV6_V6ONLY */
    }
#else
    ((struct sockaddr_in*) &inaddr)->sin_family = AF_INET;
    ((struct sockaddr_in*) &inaddr)->sin_addr.s_addr = INADDR_ANY;
    addrlen = sizeof(struct sockaddr_in);
#endif

    {  /* Don't reuse ports */
        int flg = 0;
        if (setsockopt (sd, SOL_SOCKET, SO_REUSEADDR, (const char *)&flg, sizeof (flg)) < 0) {
            BTL_ERROR(("mca_btl_tcp_create_listen: unable to unset the "
                       "SO_REUSEADDR option (%s:%d)\n",
                       strerror(opal_socket_errno), opal_socket_errno));
            CLOSE_THE_SOCKET(sd);
            return OPAL_ERROR;
        }
    }

    {
        int index, range, port;
        
#if OPAL_ENABLE_IPV6
        if (AF_INET6 == af_family) {
            range = mca_btl_tcp_component.tcp6_port_range;
            port = mca_btl_tcp_component.tcp6_port_min;
        } else
#endif  /* OPAL_ENABLE_IPV6 */
        {
            range = mca_btl_tcp_component.tcp_port_range;
            port = mca_btl_tcp_component.tcp_port_min;
        }

        for( index = 0;  index < range; index++ ) {
#if OPAL_ENABLE_IPV6
            ((struct sockaddr_in6*) &inaddr)->sin6_port = htons(port + index);
#else
            ((struct sockaddr_in*) &inaddr)->sin_port = htons(port + index);
#endif  /* OPAL_ENABLE_IPV6 */
            if(bind(sd, (struct sockaddr*)&inaddr, addrlen) < 0) {
                if( (EADDRINUSE == opal_socket_errno) || (EADDRNOTAVAIL == opal_socket_errno) ) {
                    continue;
                }
                BTL_ERROR(("bind() failed: %s (%d)",
                          strerror(opal_socket_errno), opal_socket_errno));
                CLOSE_THE_SOCKET(sd);
                return OPAL_ERROR;
            }
            goto socket_binded;
        }
#if OPAL_ENABLE_IPV6
        if (AF_INET6 == af_family) {
            BTL_ERROR(("bind6() failed: no port available in the range [%d..%d]",
                       mca_btl_tcp_component.tcp6_port_min,
                       mca_btl_tcp_component.tcp6_port_min + range));
        } else
#endif  /* OPAL_ENABLE_IPV6 */
        {
            BTL_ERROR(("bind() failed: no port available in the range [%d..%d]",
                       mca_btl_tcp_component.tcp_port_min,
                       mca_btl_tcp_component.tcp_port_min + range));
        }
        CLOSE_THE_SOCKET(sd);
        return OPAL_ERROR;
    }
  socket_binded:
    /* resolve system assignend port */
    if(getsockname(sd, (struct sockaddr*)&inaddr, &addrlen) < 0) {
        BTL_ERROR(("getsockname() failed: %s (%d)",
                   strerror(opal_socket_errno), opal_socket_errno));
        CLOSE_THE_SOCKET(sd);
        return OPAL_ERROR;
    }

#if OPAL_ENABLE_IPV6
    if (AF_INET6 == af_family) {
        mca_btl_tcp_component.tcp6_listen_port = ((struct sockaddr_in6*) &inaddr)->sin6_port;
        mca_btl_tcp_component.tcp6_listen_sd = sd;
    } else
#endif
    {
        mca_btl_tcp_component.tcp_listen_port = ((struct sockaddr_in*) &inaddr)->sin_port;
        mca_btl_tcp_component.tcp_listen_sd = sd;
    }

    /* setup listen backlog to maximum allowed by kernel */
    if(listen(sd, SOMAXCONN) < 0) {
        BTL_ERROR(("listen() failed: %s (%d)", 
                   strerror(opal_socket_errno), opal_socket_errno));
        CLOSE_THE_SOCKET(sd);
        return OPAL_ERROR;
    }

    /* set socket up to be non-blocking, otherwise accept could block */
    if((flags = fcntl(sd, F_GETFL, 0)) < 0) {
        BTL_ERROR(("fcntl(F_GETFL) failed: %s (%d)",
                   strerror(opal_socket_errno), opal_socket_errno));
        CLOSE_THE_SOCKET(sd);
        return OPAL_ERROR;
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(sd, F_SETFL, flags) < 0) {
            BTL_ERROR(("fcntl(F_SETFL) failed: %s (%d)",
                       strerror(opal_socket_errno), opal_socket_errno));
            CLOSE_THE_SOCKET(sd);
            return OPAL_ERROR;
        }
    }

    /* register listen port */
#if OPAL_ENABLE_IPV6
    if (AF_INET6 == af_family) {
        opal_event_set(opal_event_base, &mca_btl_tcp_component.tcp6_recv_event,
                        mca_btl_tcp_component.tcp6_listen_sd,
                        OPAL_EV_READ|OPAL_EV_PERSIST,
                        mca_btl_tcp_component_accept_handler,
                        0 );
        opal_event_add(&mca_btl_tcp_component.tcp6_recv_event, 0);
    } else
#endif
    {
        opal_event_set(opal_event_base, &mca_btl_tcp_component.tcp_recv_event,
                        mca_btl_tcp_component.tcp_listen_sd,
                        OPAL_EV_READ|OPAL_EV_PERSIST,
                        mca_btl_tcp_component_accept_handler,
                        0 );
        opal_event_add(&mca_btl_tcp_component.tcp_recv_event, 0);
    }
    return OPAL_SUCCESS;
}

/*
 *  Register TCP module addressing information. The MCA framework
 *  will make this available to all peers. 
 */

static int mca_btl_tcp_component_exchange(void)
{
     int rc = 0, index;
     size_t i = 0;
     size_t size = mca_btl_tcp_component.tcp_addr_count * 
                   mca_btl_tcp_component.tcp_num_links * sizeof(mca_btl_tcp_addr_t);
     /* adi@2007-04-12:
      *
      * We'll need to explain things a bit here:
      *    1. We normally have as many BTLs as physical NICs.
      *    2. With num_links, we now have num_btl = num_links * #NICs
      *    3. we might have more than one address per NIC
      */
     size_t xfer_size = 0; /* real size to transfer (may differ from 'size') */
     size_t current_addr = 0;

     if(mca_btl_tcp_component.tcp_num_btls != 0) {
         mca_btl_tcp_addr_t *addrs = (mca_btl_tcp_addr_t *)malloc(size);
         memset(addrs, 0, size);

         /* here we start populating our addresses */
         for( i = 0; i < mca_btl_tcp_component.tcp_num_btls; i++ ) {
             for (index = opal_ifbegin(); index >= 0;
                     index = opal_ifnext(index)) {
                 struct sockaddr_storage my_ss;

                 /* look if the address belongs to this (enabled) NIC.
                  * If not, go to next address
                  */
                 if (opal_ifindextokindex (index) !=
                         mca_btl_tcp_component.tcp_btls[i]->tcp_ifkindex) {
                     continue;
                 }

                 if (OPAL_SUCCESS != 
                     opal_ifindextoaddr(index, (struct sockaddr*) &my_ss,
                                        sizeof (my_ss))) {
                     opal_output (0, 
                             "btl_tcp_component: problems getting address for index %i (kernel index %i)\n",
                             index, opal_ifindextokindex (index));
                     continue;
                 }

#if OPAL_ENABLE_IPV6
                 if ((AF_INET6 == my_ss.ss_family) &&
                     (6 != mca_btl_tcp_component.tcp_disable_family)) {
                     memcpy(&addrs[current_addr].addr_inet,
                             &((struct sockaddr_in6*)&my_ss)->sin6_addr,
                             sizeof(addrs[0].addr_inet));
                     addrs[current_addr].addr_port = 
                         mca_btl_tcp_component.tcp6_listen_port;
                     addrs[current_addr].addr_family = MCA_BTL_TCP_AF_INET6;
                     xfer_size += sizeof (mca_btl_tcp_addr_t);
                     addrs[current_addr].addr_inuse   = 0;
                     addrs[current_addr].addr_ifkindex =
                         opal_ifindextokindex (index);
                     current_addr++;
                 } else
#endif
                 if ((AF_INET == my_ss.ss_family) &&
                     (4 != mca_btl_tcp_component.tcp_disable_family)) {
                     memcpy(&addrs[current_addr].addr_inet, 
                             &((struct sockaddr_in*)&my_ss)->sin_addr,
                             sizeof(addrs[0].addr_inet));
                     addrs[current_addr].addr_port = 
                         mca_btl_tcp_component.tcp_listen_port;
                     addrs[current_addr].addr_family = MCA_BTL_TCP_AF_INET;
                     xfer_size += sizeof (mca_btl_tcp_addr_t);
                     addrs[current_addr].addr_inuse   = 0;
                     addrs[current_addr].addr_ifkindex =
                         opal_ifindextokindex (index);
                     current_addr++;
                 }
             } /* end of for opal_ifbegin() */
         } /* end of for tcp_num_btls */
         OPAL_MODEX_SEND(rc, PMIX_SYNC_REQD, PMIX_GLOBAL,
                         &mca_btl_tcp_component.super.btl_version, 
                         addrs, xfer_size);
         free(addrs);
     } /* end if */
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
    int ret = OPAL_SUCCESS;
    mca_btl_base_module_t **btls;
    *num_btl_modules = 0;

    /* initialize free lists */
    opal_free_list_init( &mca_btl_tcp_component.tcp_frag_eager,
                         sizeof (mca_btl_tcp_frag_eager_t) + 
                         mca_btl_tcp_module.super.btl_eager_limit,
                         opal_cache_line_size,
                         OBJ_CLASS (mca_btl_tcp_frag_eager_t),
                         0,opal_cache_line_size,
                         mca_btl_tcp_component.tcp_free_list_num,
                         mca_btl_tcp_component.tcp_free_list_max,
                         mca_btl_tcp_component.tcp_free_list_inc,
                         NULL, 0, NULL, NULL, NULL );

    opal_free_list_init( &mca_btl_tcp_component.tcp_frag_max,
                         sizeof (mca_btl_tcp_frag_max_t) + 
                         mca_btl_tcp_module.super.btl_max_send_size,
                         opal_cache_line_size,
                         OBJ_CLASS (mca_btl_tcp_frag_max_t),
                         0,opal_cache_line_size,
                         mca_btl_tcp_component.tcp_free_list_num,
                         mca_btl_tcp_component.tcp_free_list_max,
                         mca_btl_tcp_component.tcp_free_list_inc,
                         NULL, 0, NULL, NULL, NULL );

    opal_free_list_init( &mca_btl_tcp_component.tcp_frag_user,
                         sizeof (mca_btl_tcp_frag_user_t),
                         opal_cache_line_size,
                         OBJ_CLASS (mca_btl_tcp_frag_user_t),
                         0,opal_cache_line_size,
                         mca_btl_tcp_component.tcp_free_list_num,
                         mca_btl_tcp_component.tcp_free_list_max,
                         mca_btl_tcp_component.tcp_free_list_inc,
                         NULL, 0, NULL, NULL, NULL );

    /* create a BTL TCP module for selected interfaces */
    if(OPAL_SUCCESS != (ret = mca_btl_tcp_component_create_instances() )) {
        return 0;
    }

    /* create a TCP listen socket for incoming connection attempts */
    if(OPAL_SUCCESS != (ret = mca_btl_tcp_component_create_listen(AF_INET) )) {
        return 0;
    }
#if OPAL_ENABLE_IPV6
    if((ret = mca_btl_tcp_component_create_listen(AF_INET6)) != OPAL_SUCCESS) {
        if (!(OPAL_ERR_IN_ERRNO == ret &&
              EAFNOSUPPORT == opal_socket_errno)) {
            opal_output (0, "mca_btl_tcp_component: IPv6 listening socket failed\n");
            return 0;
        }
    }
#endif

    /* publish TCP parameters with the MCA framework */
    if(OPAL_SUCCESS != (ret = mca_btl_tcp_component_exchange() )) {
        return 0;
    }

    btls = (mca_btl_base_module_t **)malloc(mca_btl_tcp_component.tcp_num_btls * 
                  sizeof(mca_btl_base_module_t*));
    if(NULL == btls) {
        return NULL;
    }

#if OPAL_CUDA_SUPPORT
    mca_common_cuda_stage_one_init();
#endif /* OPAL_CUDA_SUPPORT */

    memcpy(btls, mca_btl_tcp_component.tcp_btls, mca_btl_tcp_component.tcp_num_btls*sizeof(mca_btl_tcp_module_t*));
    *num_btl_modules = mca_btl_tcp_component.tcp_num_btls;
    return btls;
}


/**
 * Called by the event engine when the listening socket has
 * a connection event. Accept the incoming connection request
 * and queue them for completion of the connection handshake.
 */
static void mca_btl_tcp_component_accept_handler( int incoming_sd,
                                                  short ignored,
                                                  void* unused )
{
    while(true) {
#if OPAL_ENABLE_IPV6
        struct sockaddr_in6 addr;
#else
        struct sockaddr_in addr;
#endif
        opal_socklen_t addrlen = sizeof(addr);

        mca_btl_tcp_event_t *event;
        int sd = accept(incoming_sd, (struct sockaddr*)&addr, &addrlen);
        if(sd < 0) {
            if(opal_socket_errno == EINTR)
                continue;
            if (opal_socket_errno != EAGAIN && 
                opal_socket_errno != EWOULDBLOCK) {
                opal_show_help("help-mpi-btl-tcp.txt", "accept failed",
                               true, opal_process_info.nodename,
                               getpid(), 
                               opal_socket_errno,
                               strerror(opal_socket_errno));
            }
            return;
        }
        mca_btl_tcp_set_socket_options(sd);

        /* wait for receipt of peers process identifier to complete this connection */
        event = OBJ_NEW(mca_btl_tcp_event_t);
        opal_event_set(opal_event_base, &event->event, sd, OPAL_EV_READ, mca_btl_tcp_component_recv_handler, event);
        opal_event_add(&event->event, 0);
    }
}


/**
 * Event callback when there is data available on the registered 
 * socket to recv. This callback is triggered only once per lifetime
 * for any socket, in the beginning when we setup the handshake
 * protocol.
 */
static void mca_btl_tcp_component_recv_handler(int sd, short flags, void* user)
{
    opal_process_name_t guid;
    struct sockaddr_storage addr;
    int retval;
    mca_btl_tcp_proc_t* btl_proc;
    opal_socklen_t addr_len = sizeof(addr);
    mca_btl_tcp_event_t *event = (mca_btl_tcp_event_t *)user;

    OBJ_RELEASE(event);

    /* recv the process identifier */
    retval = recv(sd, (char *)&guid, sizeof(guid), 0);
    if(retval != sizeof(guid)) {
        CLOSE_THE_SOCKET(sd);
        return;
    }
    OPAL_PROCESS_NAME_NTOH(guid);

    /* now set socket up to be non-blocking */
    if((flags = fcntl(sd, F_GETFL, 0)) < 0) {
        BTL_ERROR(("fcntl(F_GETFL) failed: %s (%d)",
                   strerror(opal_socket_errno), opal_socket_errno));
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(sd, F_SETFL, flags) < 0) {
            BTL_ERROR(("fcntl(F_SETFL) failed: %s (%d)",
                       strerror(opal_socket_errno), opal_socket_errno));
        }
    }
   
    /* lookup the corresponding process */
    btl_proc = mca_btl_tcp_proc_lookup(&guid);
    if(NULL == btl_proc) {
        CLOSE_THE_SOCKET(sd);
        return;
    }

    /* lookup peer address */
    if(getpeername(sd, (struct sockaddr*)&addr, &addr_len) != 0) {
        BTL_ERROR(("getpeername() failed: %s (%d)", 
                   strerror(opal_socket_errno), opal_socket_errno));
        CLOSE_THE_SOCKET(sd);
        return;
    }

    /* are there any existing peer instances will to accept this connection */
    (void)mca_btl_tcp_proc_accept(btl_proc, (struct sockaddr*)&addr, sd);
}

