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
typedef struct mca_registry_1_0_0_t* 
  (*mca_registry_base_init_fn_t)(int *priority, bool *allow_multi_user_threads,
                                 bool *have_hidden_threads);

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
typedef int (*mca_registry_base_publish_fn_t)(char* key, void* data, size_t data_len);

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
typedef int (*mca_registry_base_lookup_fn_t)(char* key, void** data, size_t* data_len);

typedef int (*mca_registry_base_unpublish_fn_t)(char* key);

typedef int (*mca_registry_base_finalize_fn_t)(void);


/*
 * Ver 1.0.0
 */
struct mca_registry_base_module_1_0_0_t {
  mca_base_module_t registrym_version;
  mca_base_module_data_1_0_0_t registrym_data;

  mca_registry_base_init_fn_t registrym_init;
  mca_registry_base_finalize_fn_t registrym_finalize;
};
typedef struct mca_registry_base_module_1_0_0_t mca_registry_base_module_1_0_0_t;

struct mca_registry_1_0_0_t {
  mca_registry_base_publish_fn_t registry_publish;
  mca_registry_base_lookup_fn_t registry_lookup;
  mca_registry_base_unpublish_fn_t registry_unpublish;
};
typedef struct mca_registry_1_0_0_t mca_registry_1_0_0_t;

typedef mca_registry_base_module_1_0_0_t mca_registry_base_module_t;
typedef mca_registry_1_0_0_t mca_registry_t;


/*
 * Macro for use in modules that are of type registry v1.0.0
 */
#define MCA_REGISTRY_BASE_VERSION_1_0_0 \
  /* registry v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* registry v1.0 */ \
  "registry", 1, 0, 0

#endif
