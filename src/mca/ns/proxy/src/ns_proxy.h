/* -*- C -*-
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */
#ifndef NS_PROXY_H
#define NS_PROXY_H


#include "ompi_config.h"
#include "include/types.h"
#include "include/constants.h"
#include "class/ompi_list.h"
#include "mca/ns/ns.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/*
 * Module open / close
 */
int mca_ns_proxy_open(void);
int mca_ns_proxy_close(void);


/*
 * Startup / Shutdown
 */
mca_ns_base_module_t* mca_ns_proxy_init(bool *allow_multi_user_threads, bool *have_hidden_threads, int *priority);
int mca_ns_proxy_finalize(void);

/*
 * globals used within proxy component
 */

extern ompi_process_name_t *mca_ns_my_replica;
extern int mca_ns_proxy_debug;

/*
 * proxy function prototypes
 */
mca_ns_base_cellid_t mca_ns_proxy_create_cellid(void);

mca_ns_base_jobid_t mca_ns_proxy_create_jobid(void);

mca_ns_base_vpid_t mca_ns_proxy_reserve_range(mca_ns_base_jobid_t job, mca_ns_base_vpid_t range);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
