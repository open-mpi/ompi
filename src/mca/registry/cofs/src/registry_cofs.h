/* -*- C -*-
 * 
 * $HEADER$
 *
 */
#include "ompi_config.h"

#include "mca/registry/registry.h"
#include "types.h"

/*
 * Module open / close
 */
int mca_registry_cofs_open(void);
int mca_registry_cofs_close(void);


/*
 * Startup / Shutdown
 */
struct mca_registry_1_0_0_t* 
  mca_registry_cofs_init(int *priority, bool *allow_multi_user_threads,
                         bool *have_hidden_user_threads);
int mca_registry_cofs_finalize(void);


/*
 * "Action" functions
 */
int mca_registry_cofs_publish(char* key, void* data, size_t data_len);
int mca_registry_cofs_lookup(char* key, void** data, size_t* data_len);
int mca_registry_cofs_unpublish(char* key);

extern char mca_registry_cofs_comm_loc[OMPI_PATH_MAX]; /* location for file drop-off */
extern int mca_registry_cofs_my_vpid;
