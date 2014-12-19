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
typedef struct opal_btl_usnic_proc_t {
    /** allow proc to be placed on a list */
    opal_list_item_t super;

    /** pointer to corresponding opal_proc_t */
    opal_proc_t *proc_opal;

    /** Addresses received via modex for this remote proc */
    opal_btl_usnic_modex_t* proc_modex;
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
} opal_btl_usnic_proc_t;

OBJ_CLASS_DECLARATION(opal_btl_usnic_proc_t);


opal_btl_usnic_proc_t *opal_btl_usnic_proc_lookup_ompi(opal_proc_t* opal_proc);

struct opal_btl_usnic_module_t;

opal_btl_usnic_endpoint_t *
opal_btl_usnic_proc_lookup_endpoint(struct opal_btl_usnic_module_t *receiver,
                                    uint64_t sender_hashed_rte_name);

int opal_btl_usnic_proc_match(opal_proc_t* opal_proc,
                              struct opal_btl_usnic_module_t *module,
                              opal_btl_usnic_proc_t **proc);
int
opal_btl_usnic_create_endpoint(struct opal_btl_usnic_module_t *module,
                opal_btl_usnic_proc_t *proc,
                opal_btl_usnic_endpoint_t **endpoint_o);

END_C_DECLS

#endif
