/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file@
 */


#ifndef MCA_LLM_BASE_INTERNAL_H_
#define MCA_LLM_BASE_INTERNAL_H_

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"
#include "class/ompi_value_array.h"


/**
 * Container for per-node hostfile-specific data
 */
struct mca_llm_base_hostfile_node_t {
    /** make us an instance of list item */
    ompi_list_item_t super;
    /** hostname for this node.  Can be used as generic description
        field if hostnames aren't used on this platform */
    char hostname[MAXHOSTNAMELEN];
    /** number of MPI processes Open MPI can start on this host */
    int count;
    /** count argument in the hostfile */
    int given_count;
    /** generic key=value storage mechanism */
    ompi_list_t *info;    
};
/** shorten ompi_rte_base_hostfile_node_t declarations */
typedef struct mca_llm_base_hostfile_node_t mca_llm_base_hostfile_node_t;
/** create the required instance information */
OBJ_CLASS_DECLARATION(mca_llm_base_hostfile_node_t);


/**
 * extra data for the \c ompi_rte_node_allocation_t structures when
 * using the \c mca_llm_base_* functions.
 */
struct mca_llm_base_hostfile_data_t {
    /** make ourselves an instance of the data base class */
    ompi_rte_node_allocation_data_t super;
    /** keep a list of the hosts allocated to this description */
    ompi_list_t *hostlist;
};
/** shorten ompi_rte_base_hostfile_data_t declarations */
typedef struct mca_llm_base_hostfile_data_t mca_llm_base_hostfile_data_t;
/** create the required instance information */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_llm_base_hostfile_data_t);


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    /**
     * Do all the pre-use setup code.  This should only be called by
     * unit tests or mca_llm_base_open.  In other words, you probably
     * don't want to call this function.
    */ 
    OMPI_DECLSPEC void mca_llm_base_setup(void);


    /**
     * Parse input file and return a list of host entries
     *
     * \return ompi_list_t containing a list of
     *         mca_llm_base_hostfile_node_t information.
     */
    OMPI_DECLSPEC ompi_list_t *mca_llm_base_parse_hostfile(const char* filename);


    /**
     * Remove duplicate host entries from the list, editing
     * the count as appropriate and merging key=value pairs.
     *
     * \param hostlist An ompi_list_t containing
     *                 mca_llm_base_hostfile_node_t instances.
     *
     * \note If the same key is used with different values, the hosts
     * are considered different.
     */
    OMPI_DECLSPEC int mca_llm_base_collapse_resources(ompi_list_t *hostlist);


    /**
     * Rearrage the provide hostlist to meet the requirements of
     * nodes / procs.
     *
     * \param hostlist An ompi_list_t containing
     *                 mca_llm_base_hostfile_node_t instances.
     */
    OMPI_DECLSPEC int mca_llm_base_map_resources(int nodes,
                                   int procs,
                                   ompi_list_t *hostlist);


    /**
     * Take a prepped (including mapped) list of
     * mca_llm_base_hostfile_node_t instances and wrap it in an
     * ompi_node_allocation_t list.
     */
    OMPI_DECLSPEC ompi_list_t *mca_llm_base_create_node_allocation(ompi_list_t *hostlist);

    /**
     * free a list of mca_llm_base_hostfile_node_t instances
     */
    OMPI_DECLSPEC void mca_llm_base_deallocate(ompi_list_t *hostlist);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/**
 * Mutex wrapping the \code mca_llm_base_parse_hostfile function.
 */
OMPI_DECLSPEC extern ompi_mutex_t mca_llm_base_parse_mutex;

#endif
