/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "lam_config.h"

#include "lam/constants.h"
#include "mca/mca.h"
#include "mca/lam/oob/oob.h"
#include "mca/lam/oob/cofs/src/oob_cofs.h"


/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_oob_module_1_0_0_t mca_oob_cofs_module_1_0_0_0 = {
  {
    1,  /* MCA major version */
    0,  /* MCA minor version */
    0,  /* MCA release version */
    "OOB", /* MCA type name */
    1,  /* MCA type major version */
    0,  /* MCA type minor version */
    0,  /* MCA type release version */
    "COFS", /* MCA module name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    mca_oob_cofs_open,  /* module open */
    mca_oob_cofs_close, /* module close */
    false   /* MCA module is not checkpointable */
  },
  mca_oob_cofs_query,  /* module query */
  mca_oob_cofs_init,    /* module init */
  mca_oob_cofs_finalize
};


int
mca_oob_cofs_open(lam_cmd_line_t *cmd)
{
  return LAM_SUCCESS;
}


int
mca_oob_cofs_close(void)
{
  return LAM_SUCCESS;
}


int
mca_oob_cofs_query(int *priority)
{
  *priority = 0;
  return LAM_SUCCESS;
}


struct mca_oob_1_0_0*
mca_oob_cofs_init(void)
{
  /*
   * BWB - fix me, make register the "right" way...
   */
  
  return NULL;
}


int
mca_oob_cofs_finalize(void)
{
  return LAM_SUCCESS;
}
