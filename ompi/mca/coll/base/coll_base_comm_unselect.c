/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "opal/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"

extern opal_list_t mca_coll_base_available;


/*
 * This function is called to shut down a collective module on a
 * specific communicator.  If we used some of the basic module
 * functions in here to fill in NULL pointers, we also need to shut
 * down the basic module.
 */
int mca_coll_base_comm_unselect(ompi_communicator_t *comm)
{
  int err;

  /* Shut down the selected module.  Note that this pointer can be
     NULL if only the basic module was selected. */

  if (NULL != comm->c_coll_selected_module &&
      comm->c_coll_basic_module != comm->c_coll_selected_module &&
      NULL != comm->c_coll_selected_module->coll_module_finalize) {
    err = comm->c_coll_selected_module->coll_module_finalize(comm);
    if (OMPI_SUCCESS != err) {
      opal_show_help("help-mca-coll-base", 
                     "comm-unselect:failed-finalize", true);
      return err;
    }
  }

  /* If the basic module was used at all (even if it was just to fill
     in NULL pointers for functions that the selected module did not
     provide), it may have hung stuff on c_coll_comm_basic, and
     therefore needs to be finalized in this scope. */

  if (NULL != comm->c_coll_basic_module &&
      NULL != comm->c_coll_basic_module->coll_module_finalize) {
    err = comm->c_coll_basic_module->coll_module_finalize(comm);
    if (OMPI_SUCCESS != err) {
      opal_show_help("help-mca-coll-base", 
                     "comm-unselect:basic-failed-finalize", true);
      return err;
    }
  }

  /* Zero them all out, since they act as sentinel values */

  comm->c_coll_selected_data = NULL;
  comm->c_coll_selected_module = NULL;
  comm->c_coll_basic_data = NULL;
  comm->c_coll_basic_module = NULL;

  /* All done */

  return OMPI_SUCCESS;
}

