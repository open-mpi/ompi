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

#endif
