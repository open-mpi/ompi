/* -*- C -*-
 * 
 * $HEADER$
 *
 */
#include "lam_config.h"

#include "mca/lam/registry/registry.h"
#include "lam/types.h"

/*
 * Module open / close
 */
int mca_registry_cofs_open(lam_cmd_line_t *cmd);
int mca_registry_cofs_close(void);


/*
 * Startup / Shutdown
 */
int mca_registry_cofs_query(int *priority);
struct mca_registry_1_0_0_t* mca_registry_cofs_init(void);
int mca_registry_cofs_finalize(void);


/*
 * "Action" functions
 */
int mca_registry_cofs_publish(char* key, void* data, size_t data_len);
int mca_registry_cofs_lookup(char* key, void** data, size_t* data_len);
int mca_registry_cofs_unpublish(char* key);

extern char mca_registry_cofs_comm_loc[LAM_PATH_MAX]; /* location for file drop-off */
extern int mca_registry_cofs_my_vpid;
