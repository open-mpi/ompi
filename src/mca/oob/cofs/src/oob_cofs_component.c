/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "mca/mca.h"
#include "mca/oob/oob.h"
#include "oob_cofs.h"
#include "include/types.h"
#include "util/proc_info.h"
#include "util/output.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_oob_base_component_1_0_0_t mca_oob_cofs_component = {
  {
    MCA_OOB_BASE_VERSION_1_0_0,

    "cofs", /* MCA module name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    NULL,
    NULL,
  },
  {
    false /* checkpoint / restart */
  },
  mca_oob_cofs_init,    /* module init */
  mca_oob_cofs_finalize
};

mca_oob_t mca_oob_cofs = {
  mca_oob_cofs_send,
  mca_oob_cofs_recv,
  mca_oob_cofs_send_nb,
  mca_oob_cofs_recv_nb
};

char mca_oob_cofs_comm_loc[OMPI_PATH_MAX];
int mca_oob_cofs_my_jobid;
int mca_oob_cofs_my_procid;
uint64_t mca_oob_cofs_serial;


mca_oob_t* mca_oob_cofs_init(bool *allow_multi_user_threads, bool *have_hidden_threads)
{
  int len;
  char *tmp;
  FILE *fp;

  *allow_multi_user_threads &= true;

  /*
   * See if we can write in our directory...
   */
  if((tmp = getenv("OMPI_MCA_oob_cofs_dir")) == NULL) {
      ompi_output(0, "mca_oob_cofs_init: invalid/missing OMPI_MCA_oob_cofs_dir\n");
      return NULL;
  }
  strncpy(mca_oob_cofs_comm_loc, tmp, sizeof(mca_oob_cofs_comm_loc));

  len = strlen(tmp) + 32;
  tmp = malloc(len);
  if (tmp == NULL) return NULL;
  snprintf(tmp, len, "%s/oob.%d", mca_oob_cofs_comm_loc, mca_oob_cofs_my_procid);
  fp = fopen(tmp, "w");
  if (fp == NULL) {
    printf("oob_cofs: can not write in communication dir\n");
    free(tmp);
    return NULL;
  }
  fclose(fp);
  unlink(tmp);
  free(tmp);

  mca_oob_cofs_serial = 0;
  return &mca_oob_cofs;
}


int mca_oob_cofs_finalize(void)
{
  return OMPI_SUCCESS;
}
