#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "mpi.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/topo/topo.h"
#include "mca/topo/base/base.h"
#include "communicator/communicator.h"

/*
 * This function is used to shut down a topology module 
 * on a communicator. As of now, this should do nothing
 * more than just invoke the finalize on the module which
 * was selected. There is nothing fancy which we need to
 * do as is the case with collectives.
 */ 
int mca_topo_base_comm_unselect(struct ompi_communicator_t *comm) {

   if (NULL != comm->c_topo && NULL != comm->c_topo->topo_module_finalize) {
       return comm->c_topo->topo_module_finalize(comm);
   }

   /* we fall here if there was no topolog module or the selected module 
    * did not have anything to finalize (its func pointer was NULL) */
   return OMPI_SUCCESS;
}
