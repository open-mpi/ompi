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
 * Client side of the PCM interface
 *
 * The pcm interface is divided into two parts - a server side and
 * client side.  The server side is responsible for starting processes,
 * monitoring them, and killing processes.
 *
 * Unlike the server side of the pcm, the client side is a
 * single-instance framework - only one component can be loaded for
 * the lifetime of the process.  The reason for this is simple - only
 * one pcm framework could have been used to start the current
 * process.
 *
 */

#ifndef MCA_PCMCLIENT_H_
#define MCA_PCMCLIENT_H_

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/ns/ns.h"
#include "include/types.h"
#include "runtime/runtime_types.h"


struct mca_pcmclient_base_module_1_0_0_t;
typedef struct mca_pcmclient_base_module_1_0_0_t mca_pcmclient_base_module_1_0_0_t;
typedef struct mca_pcmclient_base_module_1_0_0_t mca_pcmclient_base_module_t;

struct mca_pcmclient_base_component_1_0_0_t;
typedef struct mca_pcmclient_base_component_1_0_0_t mca_pcmclient_base_component_1_0_0_t;
typedef mca_pcmclient_base_component_1_0_0_t mca_pcmclient_base_component_t;


/*
 * MCA component management functions
 */

/**
 * PCM-client initialization function
 *
 * Called by the MCA framework to initialize the component.  Will
 * be called exactly once in the lifetime of the process.
 *
 * @param priority (OUT) Relative priority or ranking use by MCA to
 *                       select a module.
 * @param allow_multiple_user_threads (OUT) Whether this module can
 *                       run with multiple threads making calls into
 *                       the library (equivalent of MPI_THREAD_MULTIPLE 
 *                       from MPI-land).
 * @param have_hidden_threads (OUT) Whether this module needs to start
 *                       a background thread for operation.
 */
typedef mca_pcmclient_base_module_t* 
(*mca_pcmclient_base_component_init_fn_t)(int *priority, 
                                          bool *allow_multiple_user_threads,
                                          bool *have_hidden_threads);


/**
 * PCM Client finalization function
 *
 * Called by the MCA framework to finalize the component.  Will be
 * called once per successful call to pcm_base_compoenent_init.
 */
typedef int 
(*mca_pcmclient_base_component_finalize_fn_t)(void);


/** 
 * PCM Client module version and interface functions
 *
 * \note the first two entries have type names that are a bit
 *  misleading.  The plan is to rename the mca_base_module_*
 * types in the future.
 */
struct mca_pcmclient_base_component_1_0_0_t {
  mca_base_component_t pcmclient_version;
  mca_base_component_data_1_0_0_t pcmclient_data;
  mca_pcmclient_base_component_init_fn_t pcmclient_init;
  mca_pcmclient_base_component_finalize_fn_t pcmclient_finalize;
};


/*
 * PCM interface functions
 */

/**
 * Finish initialization
 *
 * Perform any local registration / cleanup necessary during RTE
 * initialization, but after the OOB / GPR / etc have started
 * execution
 */
typedef int
(*mca_pcmclient_base_module_init_cleanup_fn_t)(void);

/**
 * Get my name
 *
 * @return my name
 */
typedef ompi_process_name_t * 
(*mca_pcmclient_base_module_get_self_fn_t)(void);


/**
 * Get names of peer processes which have been launched
 *
 * @param Nothing
 * @return An array of peer names, including me
 */
typedef int 
(*mca_pcmclient_base_module_get_peers_fn_t)(ompi_process_name_t **peers, 
                                             size_t *npeers);


/**
 * Base module structure for the PCM-CLIENT
 *
 * Base module structure for the PCM-CLIENT - presents the required function
 * pointers to the calling interface. 
 */
struct mca_pcmclient_base_module_1_0_0_t {
    mca_pcmclient_base_module_init_cleanup_fn_t pcmclient_init_cleanup;
    mca_pcmclient_base_module_get_self_fn_t pcmclient_get_self;
    mca_pcmclient_base_module_get_peers_fn_t pcmclient_get_peers;
};


/**
 * Macro for use in modules that are of type pcm v1.0.0
 */
#define MCA_PCMCLIENT_BASE_VERSION_1_0_0 \
  /* pcm v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* pcm v1.0 */ \
  "pcmclient", 1, 0, 0

#endif /* MCA_PCMCLIENT_H_ */
