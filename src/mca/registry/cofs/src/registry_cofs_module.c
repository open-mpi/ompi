/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "lam_config.h"

#include "constants.h"
#include "mca/mca.h"
#include "mca/registry/registry.h"
#include "mca/registry/cofs/src/registry_cofs.h"
#include "types.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_registry_base_module_1_0_0_t mca_registry_cofs_module = {
  {
    MCA_REGISTRY_BASE_VERSION_1_0_0,

    "cofs", /* MCA module name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    mca_registry_cofs_open,  /* module open */
    mca_registry_cofs_close /* module close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_registry_cofs_init,    /* module init */
  mca_registry_cofs_finalize
};

struct mca_registry_1_0_0_t mca_registry_cofs_1_0_0 = {
  mca_registry_cofs_publish,
  mca_registry_cofs_lookup,
  mca_registry_cofs_unpublish
};

char mca_registry_cofs_comm_loc[LAM_PATH_MAX];
int mca_registry_cofs_my_vpid;

int
mca_registry_cofs_open(void)
{
  return LAM_SUCCESS;
}


int
mca_registry_cofs_close(void)
{
  return LAM_SUCCESS;
}


struct mca_registry_1_0_0_t*
mca_registry_cofs_init(int *priority, bool *allow_multi_user_threads,
                       bool *have_hidden_user_threads)
{
  char *tmp;
  FILE *fp;

  *priority = 0;
  *allow_multi_user_threads = true;
  *have_hidden_user_threads = false;

  /*
   * BWB - fix me, make register the "right" way...
   */
  tmp = getenv("MCA_common_lam_cofs_comm_dir");
  if (tmp == NULL) {
    /* make it $HOME */
    tmp = getenv("HOME");
    if (tmp == NULL) {
      printf("registry_cofs can not find communication dir\n");
      return NULL;
    }
    snprintf(mca_registry_cofs_comm_loc, LAM_PATH_MAX, "%s/cofs", tmp);
  } else {
    snprintf(mca_registry_cofs_comm_loc, LAM_PATH_MAX, "%s", tmp);
  }

  /*
   * See if we can write in our directory...
   */
  tmp = malloc(strlen(mca_registry_cofs_comm_loc) + 5);
  if (tmp == NULL) return NULL;
  sprintf(tmp, "%s/me", mca_registry_cofs_comm_loc);
  fp = fopen(tmp, "w");
  if (fp == NULL) {
    printf("registry_cofs can not write in communication dir\n");
    free(tmp);
    return NULL;
  }
  fclose(fp);
  unlink(tmp);
  free(tmp);

  /*
   * BWB - fix me, make register the "right" way...
   */
  /* find our vpid */
  tmp = getenv("MCA_common_lam_cofs_my_vpid");
  if (tmp == NULL) {
    printf("registry_cofs can not find vpid\n");
    return NULL;
  }
  mca_registry_cofs_my_vpid = atoi(tmp);
  
  return &mca_registry_cofs_1_0_0;
}


int
mca_registry_cofs_finalize(void)
{
  return LAM_SUCCESS;
}
