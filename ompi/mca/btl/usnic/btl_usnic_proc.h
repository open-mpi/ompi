/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_USNIC_PROC_H
#define MCA_BTL_USNIC_PROC_H

#include "opal/class/opal_object.h"
#include "ompi/proc/proc.h"

#include "btl_usnic.h"
#include "btl_usnic_endpoint.h"

BEGIN_C_DECLS

/**
 * Represents the state of a remote process and the set of addresses
 * that it exports.  An array of these are cached on the usnic
 * component (not the usnic module).
 *
 * Also cache an instance of mca_btl_base_endpoint_t for each BTL
 * module that attempts to open a connection to the process.
 */
typedef struct ompi_btl_usnic_proc_t {
    /** allow proc to be placed on a list */
    opal_list_item_t super;

    /** pointer to corresponding ompi_proc_t */
    ompi_proc_t *proc_ompi;

    /** Addresses received via modex for this remote proc */
    ompi_btl_usnic_addr_t* proc_modex;
    /** Number of entries in the proc_modex array */
    size_t proc_modex_count;
    /** Whether the modex entry is "claimed" by a module or not */
    bool *proc_modex_claimed;

    /** Array of endpoints that have been created to access this proc */
    struct mca_btl_base_endpoint_t **proc_endpoints;
    /** Number of entries in the proc_endpoints array */
    size_t proc_endpoint_count;

    /**
     * A table giving the chosen pairing between modules and endpoint
     * addresses.  It has size mca_btl_usnic_component.num_modules.
     * j=proc_ep_match_table[i] means that
     * mca_btl_usnic_component.usnic_active_modules[i] should be paired with
     * proc_modex[j].  If there is no pairing for proc_modex[i] then
     * proc_ep_match_table[i] will be set to -1
     *
     * If matchings have not yet been computed for this proc, the pointer will
     * be NULL.
     */
    int *proc_ep_match_table;

    /**
     * true iff proc_ep_match_table != NULL and it contains at least one entry
     * that is not equal to -1.
     */
    bool proc_match_exists;
} ompi_btl_usnic_proc_t;

OBJ_CLASS_DECLARATION(ompi_btl_usnic_proc_t);


ompi_btl_usnic_proc_t *ompi_btl_usnic_proc_lookup_ompi(ompi_proc_t* ompi_proc);

struct ompi_btl_usnic_module_t;

ompi_btl_usnic_endpoint_t *
ompi_btl_usnic_proc_lookup_endpoint(struct ompi_btl_usnic_module_t *receiver,
                                    uint64_t sender_hashed_rte_name);

int ompi_btl_usnic_proc_match(ompi_proc_t* ompi_proc,
                              struct ompi_btl_usnic_module_t *module,
                              ompi_btl_usnic_proc_t **proc);
int
ompi_btl_usnic_create_endpoint(struct ompi_btl_usnic_module_t *module,
                ompi_btl_usnic_proc_t *proc,
                ompi_btl_usnic_endpoint_t **endpoint_o);

END_C_DECLS

#endif
