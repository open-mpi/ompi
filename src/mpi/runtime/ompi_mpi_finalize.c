/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "runtime/runtime.h"
#include "mpi.h"
#include "event/event.h"
#include "group/group.h"
#include "errhandler/errhandler.h"
#include "errhandler/errcode.h"
#include "errhandler/errclass.h"
#include "errhandler/errcode-internal.h"
#include "communicator/communicator.h"
#include "datatype/datatype.h"
#include "op/op.h"
#include "file/file.h"
#include "info/info.h"
#include "util/proc_info.h"
#include "runtime/runtime.h"
#include "runtime/ompi_rte_wait.h"

#include "mca/base/base.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/base.h"
#include "mca/pml/pml.h"
#include "mca/pml/base/base.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "mca/topo/topo.h"
#include "mca/topo/base/base.h"
#include "mca/io/io.h"
#include "mca/io/base/base.h"
#include "mca/oob/base/base.h"
#include "mca/ns/base/base.h"


int ompi_mpi_finalize(void)
{
  int ret;

  ompi_mpi_finalized = true;

  /* Shut down any bindings-specific issues: C++, F77, F90 (may or
     may not be necessary...?) */

  /* Free communication objects */

  /* free window resources */

  /* free file resources */
  if (OMPI_SUCCESS != (ret = ompi_file_finalize())) {
    return ret;
  }

  /* free communicator resources */
  if (OMPI_SUCCESS != (ret = ompi_comm_finalize())) {
      return ret;
  }

  /* Free secondary resources */

  /* free attr resources */
  if (OMPI_SUCCESS != (ret = ompi_attr_finalize())) {
      return ret;
  }

  /* free group resources */
  if (OMPI_SUCCESS != (ret = ompi_group_finalize())) {
      return ret;
  }

  /* free internal error resources */
  if (OMPI_SUCCESS != (ret = ompi_errcode_intern_finalize())) {
      return ret;
  }
     
  /* free error class resources */
  if (OMPI_SUCCESS != (ret = ompi_errclass_finalize())) {
      return ret;
  }

  /* free error code resources */
  if (OMPI_SUCCESS != (ret = ompi_mpi_errcode_finalize())) {
      return ret;
  }

  /* free errhandler resources */
  if (OMPI_SUCCESS != (ret = ompi_errhandler_finalize())) {
      return ret;
  }

  /* free request resources */

  /* Free all other resources */

  /* free op resources */
  if (OMPI_SUCCESS != (ret = ompi_op_finalize())) {
      return ret;
  }

  /* free ddt resources */
  if (OMPI_SUCCESS != (ret = ompi_ddt_finalize())) {
      return ret;
  }

  /* free info resources */
  if (OMPI_SUCCESS != (ret = ompi_info_finalize())) {
      return ret;
  }

  /* Close down MCA modules */

  if (OMPI_SUCCESS != (ret = mca_io_base_close())) {
    return ret;
  }
  if (OMPI_SUCCESS != (ret = mca_topo_base_close())) {
    return ret;
  }
  if (OMPI_SUCCESS != (ret = mca_coll_base_close())) {
    return ret;
  }

  /* unregister process */
  if (OMPI_SUCCESS != (ret = ompi_registry.rte_unregister(
			     ns_base_get_proc_name_string(ompi_process_info.name)))) {
      return ret;
  }

  /* wait for all processes to reach same state */
  if (OMPI_SUCCESS != (ret = ompi_rte_monitor_procs_unregistered())) {
      if (ompi_rte_debug_flag) {
	  ompi_output(0, "mpi_finalize: gave up waiting for other processes to complete");
      }
  }

  /* BWB - fix me hack.  Close down the event library.  This has to
  happen now and not as part of ompi_rte_finalize (where it should be)
  because of a bug in the TCP PTL that exists only if the event
  library is open when the module is shut down.  So shut down
  everything that depends on the event library here (rather than the
  rte_finalize where it should be).*/
  ompi_rte_wait_finalize();
  ompi_event_fini();
  /* BWB - end fix me hack */

  /* cleanup */
  if (OMPI_SUCCESS != (ret = mca_ptl_base_close())) {
    return ret;
  }
  if (OMPI_SUCCESS != (ret = mca_pml_base_close())) {
    return ret;
  }

  /* Leave the RTE */

  if (OMPI_SUCCESS != (ret = ompi_rte_finalize())) {
    return ret;
  }

  /* Close down the MCA */

  if (OMPI_SUCCESS != (ret = mca_base_close())) {
    return ret;
  }
      
  /* Leave OMPI land */

  if (OMPI_SUCCESS != (ret = ompi_finalize())) {
    return ret;
  }
      
  /* All done */

  return MPI_SUCCESS;
}
