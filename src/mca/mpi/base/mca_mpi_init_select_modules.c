/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "lam/constants.h"
#include "lam/lfc/list.h"
#include "lam/mem/malloc.h"
#include "mca/mpi/base/base.h"
#include "mca/mpi/coll/coll.h"
#include "mca/mpi/coll/base/base.h"
#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/base.h"
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/pml/base/base.h"


/*
 * Look at available pml, ptl, and coll modules and find a set that
 * works nicely.  Also set the final MPI thread level.  There are many
 * factors involved here, and this first implementation is rather
 * simplistic.  
 *
 * The contents of this function will likely be replaced 
 */
int mca_mpi_init_select_modules(int requested, int *provided)
{
  lam_list_t colls;

  /* Make final lists of available modules (i.e., call the query/init
     functions and see if they return happiness).  For pml, there will
     only be one (because there's only one for the whole process), but
     for ptl and coll, we'll get lists back. */
  /* JMS: At some point, we'll need to feed it the thread level to
     ensure to pick one high enough (e.g., if we need CR) */

  if (LAM_SUCCESS != mca_pml_base_select(&mca_pml)) {
    return LAM_ERROR;
  }

  if (LAM_SUCCESS != mca_ptl_base_select()) {
    return LAM_ERROR;
  }

  lam_list_init(&colls);
  if (LAM_SUCCESS != mca_coll_base_select(&colls)) {
    return LAM_ERROR;
  }

  /* Now that we have a final list of all available modules, do the
     selection.  pml is already selected. */

  /* JMS ...Do more here with the thread level, etc.... */
  *provided = requested;

  /* Tell the selected pml module about all the selected ptl
     modules */

  mca_pml.pml_add_ptls(&mca_ptl_base_modules_initialized);

  /* All done */

  return LAM_SUCCESS;
}

