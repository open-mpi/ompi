/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "mca/coll/base/static-modules.h"


/*
 * Global variables
 */
int mca_coll_base_output = -1;
int mca_coll_base_crossover = 4;
int mca_coll_base_associative = 1;
int mca_coll_base_reduce_crossover = 4;
lam_list_t mca_coll_base_modules_available;


/**
 * Function for finding and opening either all MCA modules, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_coll_base_open(void)
{
  /* Open up all available modules */

  if (LAM_SUCCESS != 
      mca_base_modules_open("coll", 0, mca_coll_base_static_modules, 
                            &mca_coll_base_modules_available)) {
    return LAM_ERROR;
  }

  /* All done */

  return LAM_SUCCESS;
}
