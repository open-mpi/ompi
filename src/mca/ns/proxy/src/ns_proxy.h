/* -*- C -*-
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
mca_ns_base_cellid_t ns_proxy_create_cellid(void);

mca_ns_base_jobid_t ns_proxy_create_jobid(void);

mca_ns_base_vpid_t ns_proxy_reserve_range(mca_ns_base_jobid_t job, mca_ns_base_vpid_t range);



#endif
