/* -*- C -*-
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

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Parse input file and return a list of host entries
     */
    ompi_list_t *mca_llm_base_parse_hostfile(const char* filename);

    /**
     * Rearrage the provide hostlist to meet the requirements of
     * nodes / procs
     */
    int mca_llm_base_map_resources(int nodes,
                                   int procs,
                                   ompi_list_t *hostlist);

    /**
     * Remove duplicate host entries from the list, editing
     * the count as appropriate and merging key=value pairs.
     *
     * \note If the same key is used with different values, the hosts
     * are considered different.
     */
    int mca_llm_base_collapse_resources(ompi_list_t *hostlist);

    int mca_llm_base_deallocate(ompi_list_t *nodelist);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/**
 * Mutex wrapping the \code mca_llm_base_parse_hostfile function.
 */
extern ompi_mutex_t mca_llm_base_parse_mutex;

#endif
