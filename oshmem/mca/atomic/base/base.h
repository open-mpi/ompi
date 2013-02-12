/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#ifndef MCA_ATOMIC_BASE_H
#define MCA_ATOMIC_BASE_H

#include "oshmem_config.h"

#include "oshmem/mca/atomic/atomic.h"
#include "opal/class/opal_list.h"


/*
 * Global functions for MCA overall atomic open and close
 */

BEGIN_C_DECLS

/**
 * Initialize the atomic MCA framework
 *
 * @retval OSHEM_SUCCESS Upon success
 * @retval OSHMEM_ERROR Upon failure
 *
 */
OSHMEM_DECLSPEC int mca_atomic_base_open(void);

/**
 * Create list of available atomic components.
 *
 * @param allow_multi_user_threads Will be set to true if any of the
 * available components will allow multiple user threads
 * @param have_hidden_threads Will be set to true if any of the
 * available components have hidden threads.
 *
 * @retval OSHMEM_SUCCESS If one or more atomic components are available.
 * @retval OSHMEM_ERROR If no atomic components are found to be available.
 *
 */
int mca_atomic_base_find_available(bool enable_progress_threads,
                                   bool enable_threads);


int mca_atomic_base_select(void);


/**
 * Shut down the atomic MCA framework.
 *
 * @retval OSHMEM_SUCCESS Always
 *
 * This function shuts down everything in the atomic MCA framework,
 * and is called during oshmem_shmem_finalize().
 *
 * It must be the last function invoked on the atomic MCA framework.
 */
OSHMEM_DECLSPEC int mca_atomic_base_close(void);


/*
 * Globals
 */


/**
 * ATOMIC framework debugging stream ID used with opal_output() and
 * opal_output_verbose().
 */
OSHMEM_DECLSPEC extern int mca_atomic_base_output;

/**
 * Indicator as to whether the list of opened atomic components is valid or
 * not.
 */
extern bool mca_atomic_base_components_opened_valid;

/**
 * List of all opened components; created when the atomic framework is
 * initialized and destroyed when we reduce the list to all available
 * coll components.
 */
OSHMEM_DECLSPEC extern opal_list_t mca_atomic_base_components_opened;

/**
 * Indicator as to whether the list of available atomic components is valid
 * or not.
 */
extern bool mca_atomic_base_components_available_valid;

/**
 * List of all available components; created by reducing the list of open
 * components to all those who indicate that they may run during this
 * process.
 */
extern opal_list_t mca_atomic_base_components_available;


/* ******************************************************************** */
#ifdef __BASE_FILE__
#define __ATOMIC_FILE__ __BASE_FILE__
#else
#define __ATOMIC_FILE__ __FILE__
#endif

#define ATOMIC_VERBOSE(level, format, ...) \
	    opal_output_verbose(level, mca_atomic_base_output, "%s:%d - %s() " format, \
				                        __ATOMIC_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)

#define ATOMIC_ERROR(format, ... ) \
	    opal_output_verbose(0, mca_atomic_base_output, "Error: %s:%d - %s() " format, \
				                        __SCOLL_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)

END_C_DECLS

#endif /* MCA_ATOMIC_BASE_H */
