/*
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 *
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_COMMON_OFACM_BASE_H
#define OMPI_COMMON_OFACM_BASE_H
#include "ompi_config.h"

#include <stdio.h>
#include <stdarg.h>

#include "orte/runtime/orte_globals.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "connect.h"

BEGIN_C_DECLS

#define HAVE_XRC (1 == OMPI_HAVE_CONNECTX_XRC)

extern int ompi_common_ofacm_base_output;
extern int ompi_common_ofacm_base_verbose; /* disabled by default */
/* File for sl data produced only for a 3D-Torus Cluster */
extern char* ompi_common_ofacm_three_dim_torus; 

static inline int ompi_common_ofacm_base_err(const char* fmt, ...)
{
    va_list list;
    int ret;

    va_start(list, fmt);
    ret = vfprintf(stderr, fmt, list);
    va_end(list);
    return ret;
}

#define OFACM_ERROR(args)                                  \
    do {                                                   \
        ompi_common_ofacm_base_err("[%s]%s[%s:%d:%s] ",     \
            orte_process_info.nodename,                     \
            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),             \
            __FILE__, __LINE__, __func__);                  \
        ompi_common_ofacm_base_err args;                    \
        ompi_common_ofacm_base_err("\n");                   \
    } while(0);

#if OPAL_ENABLE_DEBUG
#define OFACM_VERBOSE(args)                                    \
    do {                                                         \
        if(ompi_common_ofacm_base_verbose > 0) {                            \
            ompi_common_ofacm_base_err("[%s]%s[%s:%d:%s] ",                \
                    orte_process_info.nodename,                   \
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),  \
                    __FILE__, __LINE__, __func__);               \
            ompi_common_ofacm_base_err args;                               \
            ompi_common_ofacm_base_err("\n");                              \
        }                                                         \
    } while(0); 
#else
#define OFACM_VERBOSE(args) 
#endif

/* 
 * PUBLIC functions
 * ****************
 */

/*
 * Open function
 */
OMPI_DECLSPEC int ompi_common_ofacm_base_register(mca_base_component_t *base);

/*
 * Query CPCs to see if they want to run on a specific port.
 * Input:
 *  port - port information
 * Output:
 *  cpcs - list of availible cpcs
 *  num_cpcs - number of cpcs
 */
OMPI_DECLSPEC int ompi_common_ofacm_base_select_for_local_port
    (ompi_common_ofacm_base_dev_desc_t *dev,
     ompi_common_ofacm_base_module_t ***cpcs, int *num_cpcs);

/*
 * Select function
 * Input:
 * local_cpcs - local cpc modules
 * num_local_cpcs - number of local cpc modules
 * remote_cpc_data - cpc information from remote peer
 * remote_cpc_data_count - num of remote information from remote peer
 * Output:
 * ret_local_cpc - matched cpc module
 * ret_remote_cpc_data - matched remote cpc data
 */
OMPI_DECLSPEC int ompi_common_ofacm_base_find_match
    (ompi_common_ofacm_base_module_t **local_cpcs, int num_local_cpcs,
     ompi_common_ofacm_base_module_data_t *remote_cpc_data, int remote_cpc_data_count,
     ompi_common_ofacm_base_module_t **ret_local_cpc,
     ompi_common_ofacm_base_module_data_t **ret_remote_cpc_data);

/*
 * Find a CPC's index so that we can send it in the modex
 */
OMPI_DECLSPEC int ompi_common_ofacm_base_get_cpc_index
    (ompi_common_ofacm_base_component_t *cpc);

/*
 * Start a new connection to an endpoint
 */
OMPI_DECLSPEC int ompi_common_ofacm_base_start_connect
        (struct ompi_common_ofacm_base_local_connection_context_t *context);

/*
 * Component-wide CPC finalize
 */
OMPI_DECLSPEC void ompi_common_ofacm_base_finalize(void);

/*
 * Component-wide CPC init
 */
OMPI_DECLSPEC int ompi_common_ofacm_base_init(void);

/*
 * Lookup a CPC by its index (received from the modex)
 */
OMPI_DECLSPEC ompi_common_ofacm_base_component_t *
    ompi_common_ofacm_base_get_cpc_byindex(uint8_t index);

/* 
 * PRIVATE functions (called only by cpcs)
 * ***************************************
 */

/*
 * Proc initialization function
 */ 
void ompi_common_ofacm_base_proc_setup
    (ompi_common_ofacm_base_proc_t *proc, 
     ompi_common_ofacm_base_local_connection_context_t *context,
     ompi_proc_t *proc_ompi);
/*
 * Context initialization function
 */ 
int ompi_common_ofacm_base_context_init
    (ompi_common_ofacm_base_local_connection_context_t *context,
     ompi_common_ofacm_base_module_t *cpc,
     ompi_common_ofacm_base_context_connect_cb_fn_t connect_cb,
     ompi_common_ofacm_base_context_error_cb_fn_t error_cb,
     ompi_common_ofacm_base_context_prepare_recv_cb_fn_t prepare_recv_cb,
     ompi_common_ofacm_base_proc_t *proc,
     ompi_common_ofacm_base_qp_config_t *qp_config,
     struct ibv_pd *pd, uint64_t subnet_id, int cpc_type, 
     uint16_t lid, uint16_t rem_lid, 
     int32_t user_context_index, void *user_context);

/*
 *  Remote context initialization.
 *  Returns operation status
 */ 
int ompi_common_ofacm_base_remote_context_init
    (ompi_common_ofacm_base_remote_connection_context_t *context,
     int num_qps, int num_srqs);

/* Find OFACM proc on specific component */
ompi_common_ofacm_base_proc_t* ompi_common_ofacm_base_find_proc
        (ompi_common_ofacm_base_component_t *component, ompi_proc_t *proc);

#if 0
/* 
 * Allocate a CTS frag
 */
int ompi_common_ofacm_base_alloc_cts(
        struct mca_btl_base_endpoint_t *endpoint);

/* 
 * Free a CTS frag
 */
int ompi_common_ofacm_base_free_cts(
        struct mca_btl_base_endpoint_t *endpoint);
#endif

END_C_DECLS

#endif
