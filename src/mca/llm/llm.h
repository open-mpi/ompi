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
 * @file
 *
 * Location List Manager (LLM)
 *
 * The Location List Manager (LLM) is an MCA component that provides
 * resource allocation for the Process Control Manager (PCM).  The LLM
 * is intended to be used by the PCM and should not be used outside of
 * the PCM framework.
 *
 */

#ifndef MCA_LLM_H
#define MCA_LLM_H

#include "ompi_config.h"
#include "mca/mca.h"
#include "class/ompi_list.h"
#include "runtime/runtime_types.h"

/*
 * MCA component management functions
 */

/**
 * LLM initialization function
 *
 * Called by the MCA framework to initialize the component.  Will
 * be called exactly once in the lifetime of the process.
 *
 * @param active_pcm (IN) Name of the currently active PCM module,
 *                       as it might be useful in determining
 *                       useability.
 * @param have_threads (IN) Whether the current running process is
 *                       multi-threaded or not.  true means there
 *                       may be concurrent access into the
 *                       underlying components *and* that the
 *                       components may launch new threads.
 * @param priority (OUT) Relative priority or ranking use by MCA to
 *                       select a module.
 *
 * \warning The only requirement on the returned type is that the
 * first sizeof(struct mca_llm_base_module_1_0_0_t) bytes have the
 * same structure as a struct mca_llm_base_module_1_0_0_t.  The llm is
 * free to return a pointer to a larger structure in order to maintain
 * per-module information it may need.  Therefore, the caller should
 * never copy the structure or assume its size.
 */
typedef struct mca_llm_base_module_1_0_0_t* 
(*mca_llm_base_component_init_fn_t)(const char *active_pcm,
                                    bool have_threads,
                                    int *priority);


/**
 * LLM finalization function
 *
 * Called by the MCA framework to finalize the component.  Will be
 * called once per successful call to llm_base_compoenent_init.
 */
typedef int (*mca_llm_base_finalize_fn_t)(struct mca_llm_base_module_1_0_0_t *me);


/** 
 * LLM module version and interface functions
 *
 * \note the first two entries have type names that are a bit
 *  misleading.  The plan is to rename the mca_base_module_*
 * types in the future.
 */
struct mca_llm_base_component_1_0_0_t {
  /** component version */
  mca_base_component_t llm_version;
  /** component data */
  mca_base_component_data_1_0_0_t llm_data;
  /** Function called when component is initialized  */
  mca_llm_base_component_init_fn_t llm_init;
};
/** shorten mca_llm_base_component_1_0_0_t declaration */
typedef struct mca_llm_base_component_1_0_0_t mca_llm_base_component_1_0_0_t;
/** shorten mca_llm_base_component_t declaration */
typedef mca_llm_base_component_1_0_0_t mca_llm_base_component_t;


/*
 * LLM interface functions
 */

/**
 * Allocate requested resources
 *
 * See notes for \c ompi_rte_allocate_resources() for complete
 * description.
 *
 * @param jobid (IN) Jobid with which to associate the given resources.
 * @param nodes (IN) Number of ndoes to try to allocate.  If 0, the
 *                   allocator will try to allocate \c procs processes
 *                   on as many nodes as are needed.  If non-zero, 
 *                   will try to allocate \c procs process slots 
 *                   per node.
 * @param procs (IN) Number of processors to try to allocate.  See the note
 *                   for <code>nodes</code> for usage.
 * @param nodelist (OUT) List of <code>ompi_rte_node_allocation_t</code>s
 *                   describing the allocated resources.
 *
 */
typedef ompi_list_t*
(*mca_llm_base_allocate_resources_fn_t)(struct mca_llm_base_module_1_0_0_t *me,
                                        mca_ns_base_jobid_t jobid, 
                                        int nodes, int procs);


/**
 * Deallocate requested resources
 *
 * Return the resources for the given jobid to the system.
 *
 * @param jobid (IN) Jobid associated with the resources to be freed.
 * @param nodes (IN) Nodelist from associated allocate_resource call.
 *                   All associated memory will be freed as appropriate.
 */
typedef int
(*mca_llm_base_deallocate_resources_fn_t)(struct mca_llm_base_module_1_0_0_t *me,
                                          mca_ns_base_jobid_t jobid,
                                          ompi_list_t *nodelist);


/**
 * Base module structure for the LLM
 *
 * Base module structure for the LLM - presents the required function
 * pointers to the calling interface. 
 */
struct mca_llm_base_module_1_0_0_t {
    /** Function to be called on resource request */
    mca_llm_base_allocate_resources_fn_t llm_allocate_resources;
    /** Function to be called on resource return */ 
    mca_llm_base_deallocate_resources_fn_t llm_deallocate_resources;
    /** Function called when component is finalized */
    mca_llm_base_finalize_fn_t llm_finalize;
};
/** shorten mca_llm_base_module_1_0_0_t declaration */
typedef struct mca_llm_base_module_1_0_0_t mca_llm_base_module_1_0_0_t;
/** shorten mca_llm_base_module_t declaration */
typedef struct mca_llm_base_module_1_0_0_t mca_llm_base_module_t;


/**
 * Macro for use in modules that are of type pml v1.0.0
 */
#define MCA_LLM_BASE_VERSION_1_0_0 \
  /* llm v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* llm v1.0 */ \
  "llm", 1, 0, 0

#endif /* MCA_LLM_H */
