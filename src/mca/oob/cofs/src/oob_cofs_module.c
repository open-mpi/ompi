/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "lam_config.h"

#include "constants.h"
#include "mca/mca.h"
#include "mca/oob/oob.h"
#include "mca/oob/cofs/src/oob_cofs.h"
#include "types.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_oob_base_module_1_0_0_t mca_oob_cofs_module = {
  {
    MCA_OOB_BASE_VERSION_1_0_0,

    "cofs", /* MCA module name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    mca_oob_cofs_open,  /* module open */
    mca_oob_cofs_close /* module close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_oob_cofs_init,    /* module init */
  mca_oob_cofs_finalize
};

struct mca_oob_1_0_0_t mca_oob_cofs_1_0_0 = {
  mca_oob_cofs_send,
  mca_oob_cofs_recv,
  mca_oob_cofs_recv_nb,
  mca_oob_cofs_recv_cb
};

char mca_oob_cofs_comm_loc[LAM_PATH_MAX];
int mca_oob_cofs_my_vpid;
uint64_t mca_oob_cofs_serial;

int
mca_oob_cofs_open(void)
{
  return LAM_SUCCESS;
}


int
mca_oob_cofs_close(void)
{
  return LAM_SUCCESS;
}


struct mca_oob_1_0_0_t*
mca_oob_cofs_init(int *priority, bool *allow_multi_user_threads,
                  bool *have_hidden_threads)
{
  char *tmp;
  FILE *fp;

  *priority = 0;
  *allow_multi_user_threads = true;
  *have_hidden_threads = true;

  /*
   * BWB - fix me, make register the "right" way...
   */
  tmp = getenv("MCA_common_lam_cofs_comm_dir");
  if (tmp == NULL) {
    /* make it $HOME */
    tmp = getenv("HOME");
    if (tmp == NULL) {
      printf("oob_cofs can not find communication dir\n");
      return NULL;
    }
    snprintf(mca_oob_cofs_comm_loc, LAM_PATH_MAX, "%s/cofs", tmp);
  } else {
    snprintf(mca_oob_cofs_comm_loc, LAM_PATH_MAX, "%s", tmp);
  }

  /*
   * BWB - fix me, make register the "right" way...
   */
  /* find our vpid */
  tmp = getenv("MCA_common_lam_cofs_my_vpid");
  if (tmp == NULL) {
    printf("oob_cofs can not find vpid\n");
    return NULL;
  }
  mca_oob_cofs_my_vpid = atoi(tmp);

  /*
   * See if we can write in our directory...
   */
  tmp = malloc(strlen(mca_oob_cofs_comm_loc) + 5);
  if (tmp == NULL) return NULL;
  sprintf(tmp, "%s/oob.%d", mca_oob_cofs_comm_loc, mca_oob_cofs_my_vpid);
  fp = fopen(tmp, "w");
  if (fp == NULL) {
    printf("oob_cofs can not write in communication dir\n");
    free(tmp);
    return NULL;
  }
  fclose(fp);
  unlink(tmp);
  free(tmp);

  mca_oob_cofs_serial = 0;
  
  return &mca_oob_cofs_1_0_0;
}


int
mca_oob_cofs_finalize(void)
{
  return LAM_SUCCESS;
}
