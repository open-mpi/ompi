/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * Contains the internal functions and typedefs for the use of the oob
 */

#ifndef MCA_OOB_H_
#define MCA_OOB_H_

#include "orte_config.h"
#include "orte/types.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_NET_UIO_H
#include <net/uio.h>
#endif

#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/mca/mca.h"

#include "orte/mca/rml/base/base.h"

BEGIN_C_DECLS

typedef bool (*mca_oob_base_component_avail_fn_t)(void);
typedef int (*mca_oob_base_component_startup_fn_t)(void);
typedef void (*mca_oob_base_component_shutdown_fn_t)(void);
typedef int (*mca_oob_base_component_send_fn_t)(orte_rml_send_t *msg);
typedef char* (*mca_oob_base_component_get_addr_fn_t)(void);
typedef int (*mca_oob_base_component_set_addr_fn_t)(orte_process_name_t *peer,
                                                    char **uris);
typedef bool (*mca_oob_base_component_is_reachable_fn_t)(orte_process_name_t *peer);
typedef void (*mca_oob_ping_callback_fn_t)(int status, void *cbdata);

typedef struct {
    mca_base_component_t                      oob_base;
    mca_base_component_data_t                 oob_data;
    int                                       idx;
    int                                       priority;
    mca_oob_base_component_avail_fn_t         available;
    mca_oob_base_component_startup_fn_t       startup;
    mca_oob_base_component_shutdown_fn_t      shutdown;
    mca_oob_base_component_send_fn_t          send_nb;
    mca_oob_base_component_get_addr_fn_t      get_addr;
    mca_oob_base_component_set_addr_fn_t      set_addr;
    mca_oob_base_component_is_reachable_fn_t  is_reachable;
} mca_oob_base_component_t;

/**
 * Macro for use in components that are of type oob
 */
#define MCA_OOB_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "oob", 2, 0, 0

END_C_DECLS

#endif
