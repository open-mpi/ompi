/* -*- C -*-
 *
 * $HEADER$
 */

/**
 *  \brief Publish/Subscribe-style global registry database infrastructure
 *
 * LAM/MPI provides a global publish/subscribe-style registry database
 * for use in both LAM and MPI layers.  Data is stored in a flat
 * key=value style database; keys are of type char* and value of type
 * void*.  No endian correction is performed on the data.
 *
 * The registry is implemented as an mca module, using the services
 * provided by the current run-time environment.  In environments with
 * limited native out of band message passing and global registry
 * infrastructure, this data may be stored in the MPI applications
 * themselves.  Care should therefore be used when storing potentially
 * large amounts of data in the global registry.
 *
 * Locality of stored data is unspecified and unknown to the calling
 * application.  The underlying mca module is free to store data
 * wherever practical.  A high quality implementation will provide
 * replication in case of process failure.
 */

#ifndef MCA_REGISTRY_H_
#define MCA_REGISTRY_H_

#include "lam_config.h"

#include "mca/mca.h"


/*
 * Functions every module instance will have to provide
 */
typedef int (*mca_registry_query_fn_t)(int *priority);
typedef struct mca_registry_1_0_0* (*mca_registry_init_fn_t)(void);

  /**
   * Publish a key=value piece of information
   *
   * @param key Character string containing the key
   * @param data Pointer to value data
   * @param data_len Length of data buffer
   *
   * @retval 0 Update succeeded
   * @retval 
   *
   * Add key=value pair to the global registry.  Will overwrite any
   * existing data for the specified key (if it is already in use).
   * Atomicity of the publish is guaranteed - the registry does not
   * have to be locked for a publish to occur safely.
   *
   * \warning May block if the registry entry for key is currently
   * locked by another process.
   */
typedef int (*mca_registry_publish_fn_t)(char* key, void* data, size_t data_len);

  /**
   * Get the value for given key
   *
   * @param key String containing key to search on
   * @param data Pointer to a void* pointer to store data
   * @param datalen Pointer to size_t containing length of data
   *
   * @retval 0 Key was found and data successfully obtained
   * @retval ENOMATCH No such key was found in the database
   * @retval ENOMEM Could not allocate enough memory for data
   *
   * Search for the given key, downloading the corresponding value
   * into the pointer *data if the key exists.  *data will point to
   * the data buffer and data_len will contain the buffer length if
   * the value could be obtained.  On error, *data will be NULL and
   * data_len will be 0.
   *
   * \warning Returned buffer was allocated via lam_malloc and must be
   * freed by the caller using lam_free.
   *
   * \warning May block if the registry entry for key is currently
   * locked by another process.
   */
typedef int (*mca_registry_lookup_fn_t)(char* key, void** data, size_t* data_len);

typedef int (*mca_registry_finalize_fn_t)(void);


/*
 * Ver 1.0.0
 */
typedef struct mca_registry_module_1_0_0 {
  mca_module_1_0_0_t super;

  mca_registry_query_fn_t registry_m_query;
  mca_registry_init_fn_t registry_m_init;

  mca_registry_finalize_fn_t registry_m_finalize;
} mca_registry_module_1_0_0_t;

typedef struct mca_registry_1_0_0 {
  mca_1_0_0_t super;

  mca_registry_publish_fn_t registry_publish;
  mca_registry_lookup_fn_t registry_lookup;
} mca_registry_module_1_0_0_t;

typedef mca_registry_module_1_0_0_t mca_registry_module_t;
typedef mca_registry_1_0_0_t mca_registry_t;


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_registry_base_open(lam_cmd_line_t *cmd);
  int mca_registry_base_close(void);

  bool mca_registry_base_is_checkpointable(void)

  int mca_registry_base_checkpoint(void);
  int mca_registry_base_continue(void);
  int mca_registry_base_restart(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Global struct holding the selected module's function pointers
 */
extern mca_registry_t mca_registry;

#endif
