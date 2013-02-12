/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#ifndef MCA_SCOLL_BASE_H
#define MCA_SCOLL_BASE_H

#include "oshmem_config.h"

#include "oshmem/mca/memheap/memheap.h"
#include "opal/class/opal_list.h"


/*
 * Global functions for MCA overall collective open and close
 */

BEGIN_C_DECLS

/**
 * Initialize the coll MCA framework
 *
 * @retval OSHEM_SUCCESS Upon success
 * @retval OSHMEM_ERROR Upon failure
 *
 * This must be the first function invoked in the coll MCA
 * framework.  It initializes the coll MCA framework, finds and
 * opens coll components, etc.
 *
 * This function is invoked during oshmem_shmem_init() and during the
 * initialization of the special case of the laminfo command.
 * 
 * This function fills in the internal global variable
 * mca_coll_base_components_opened, which is a list of all coll components
 * that were successfully opened.  This variable should \em only be
 * used by other coll base functions -- it is not considered a
 * public interface member -- and is only mentioned here for
 * completeness.
 */
OSHMEM_DECLSPEC int mca_scoll_base_open(void);

/**
 * Create list of available coll components.
 *
 * @param allow_multi_user_threads Will be set to true if any of the
 * available components will allow multiple user threads
 * @param have_hidden_threads Will be set to true if any of the
 * available components have hidden threads.
 *
 * @retval OSHMEM_SUCCESS If one or more coll components are available.
 * @retval OSHMEM_ERROR If no coll components are found to be available.
 *
 * This function is invoked during oshmem_shmem_init() to query all
 * successfully opened coll components and create a list of all
 * available coll components.
 *
 * This function traverses the (internal global variable)
 * mca_coll_base_components_opened list and queries each component to see
 * if it ever might want to run during this SHMEM process.  It creates
 * another internal global variable list named
 * mca_coll_base_components_available, consisting of a list of components
 * that are available for selection when communicators are created.
 * This variable should \em only be used by other coll base
 * functions -- it is not considered a public interface member --
 * and is only mentioned here for completeness.
 */
OSHMEM_DECLSPEC int mca_scoll_base_find_available(bool enable_progress_threads,
                                  bool enable_threads);


/**
 * Select an available component for a new communicator.
 *
 * @param comm Communicator that the component will be selected for.
 * @param preferred The component that is preferred for this
 * communicator (or NULL).
 *
 * @return OSHMEM_SUCCESS Upon success.
 * @return OSHMEM_ERROR Upon failure.
 *
 * This function is invoked when a new communicator is created and a
 * coll component needs to be selected for it.  It should be invoked
 * near the end of the communicator creation process such that
 * almost everything else is functional on the communicator (e.g.,
 * point-to-point communication).  
 *
 * Note that new communicators may be created as a result of
 * invoking this function.  Specifically: this function is called in
 * the depths of communicator creation, but during the execution of
 * this function, new communicators may be created, and therefore
 * communicator creation functions may be re-entered (albiet with
 * different arguments).
 */
OSHMEM_DECLSPEC int mca_scoll_base_select(struct oshmem_group_t *group);

/**
 * Finalize a coll component on a specific communicator.
 *
 * @param comm The communicator that is being destroyed.
 *
 * @retval OSHMEM_SUCCESS Always.
 *
 * This function is invoked near the beginning of the destruction of
 * a communicator.  It finalizes the coll component associated with the
 * communicator (e.g., allowing the component to clean up and free any
 * resources allocated for that communicator).  Note that similar to
 * mca_coll_base_group_select(), as result of this function, other
 * communicators may also be destroyed.
 */
int mca_scoll_base_group_unselect(struct oshmem_group_t *group);

/**
 * Shut down the coll MCA framework.
 *
 * @retval OSHMEM_SUCCESS Always
 *
 * This function shuts down everything in the coll MCA framework,
 * and is called during oshmem_shmem_finalize().
 *
 * It must be the last function invoked on the coll MCA framework.
 */
OSHMEM_DECLSPEC int mca_scoll_base_close(void);


/*
 * Globals
 */


/**
 * Special synchronization array to do barrier all.
 */
OSHMEM_DECLSPEC extern long* mca_scoll_sync_array;

OSHMEM_DECLSPEC int mca_scoll_enable(void) ;

OSHMEM_DECLSPEC void mca_scoll_disable(void); 


/**
 * SCOLL framework debugging stream ID used with opal_output() and
 * opal_output_verbose().
 */
OSHMEM_DECLSPEC extern int mca_scoll_base_output;

/**
 * Indicator as to whether the list of opened coll components is valid or
 * not.
 */
extern bool mca_scoll_base_components_opened_valid;

/**
 * List of all opened components; created when the coll framework is
 * initialized and destroyed when we reduce the list to all available
 * coll components.
 */
OSHMEM_DECLSPEC extern opal_list_t mca_scoll_base_components_opened;

/**
 * Indicator as to whether the list of available coll components is valid
 * or not.
 */
extern bool mca_scoll_base_components_available_valid;

/**
 * List of all available components; created by reducing the list of open
 * components to all those who indicate that they may run during this
 * process.
 */
extern opal_list_t mca_scoll_base_components_available;

/* ******************************************************************** */
#ifdef __BASE_FILE__
#define __SCOLL_FILE__ __BASE_FILE__
#else
#define __SCOLL_FILE__ __FILE__
#endif

#define SCOLL_VERBOSE(level, format, ...) \
	    opal_output_verbose(level, mca_scoll_base_output, "%s:%d - %s() " format, \
				                        __SCOLL_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)

#define SCOLL_ERROR(format, ... ) \
	    opal_output_verbose(0, mca_scoll_base_output, "Error: %s:%d - %s() " format, \
				                        __SCOLL_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)

END_C_DECLS

#endif /* MCA_SCOLL_BASE_H */
