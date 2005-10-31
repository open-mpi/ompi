/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "opal/class/opal_list.h"
#include "mca/soh/soh_types.h"

#ifndef ORTE_MCA_RAS_TYPES_H
#define ORTE_MCA_RAS_TYPES_H

/**
 * Struct for holding information about a node (a local copy of what
 * is in the node segment in the registry).
 */
struct orte_ras_node_t {
    /** Base object */
    opal_list_item_t super;
    /** String node name */
    char *node_name;
    /** String of the architecture for the node.  This is permitted to
        be NULL if it is not known. */
    char *node_arch;
    /** The cell ID of this node */
    orte_cellid_t node_cellid;
    /** State of this node; see include/orte_types.h */
    orte_node_state_t node_state;
    /** A "soft" limit on the number of slots available on the node.
        This will typically correspond to the number of physical CPUs
        that we have been allocated on this note and would be the
        "ideal" number of processes for us to launch. */
    size_t node_slots;
    /** How many processes have already been launched, used by one or
        more jobs on this node. */
    size_t node_slots_inuse;
    /** This represents the number of slots we (the allocator) are
        attempting to allocate to the current job - or the number of
        slots allocated to a specific job on a query for the jobs
        allocations */
    size_t node_slots_alloc;
    /** A "hard" limit (if set -- a value of 0 implies no hard limit)
        on the number of slots that can be allocated on a given
        node. This is for some environments (e.g. grid) there may be
        fixed limits on the number of slots that can be used.

        This value also could have been a boolean - but we may want to
        allow the hard limit be different than the soft limit - in
        other words allow the node to be oversubscribed up to a
        specified limit.  For example, if we have two processors, we
        may want to allow up to four processes but no more. */
    size_t node_slots_max;
    /** Username on this node, if specified */
    char * node_username;
    /** For use by the launcher */
    int node_launched;
};

/**
 * Convenience typedef
 */
typedef struct orte_ras_node_t orte_ras_node_t;


ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_ras_node_t);


#endif
