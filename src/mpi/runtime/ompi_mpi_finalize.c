/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "mpi.h"
#include "event/event.h"
#include "group/group.h"
#include "errhandler/errcode.h"
#include "errhandler/errclass.h"
#include "communicator/communicator.h"
#include "datatype/datatype.h"
#include "op/op.h"
#include "file/file.h"
#include "info/info.h"
#include "util/proc_info.h"
#include "runtime/runtime.h"
#include "runtime/ompi_progress.h"
#include "runtime/ompi_rte_wait.h"
#include "attribute/attribute.h"

#include "mca/base/base.h"
#include "mca/base/mca_base_module_exchange.h"
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
#include "mca/gpr/base/base.h"


int ompi_mpi_finalize(void)
{
    int ret;
    ompi_rte_process_status_t my_status;
    int my_rank;
    mca_ns_base_jobid_t my_jobid;

    ompi_mpi_finalized = true;
#if OMPI_HAVE_THREADS == 0
    ompi_progress_events(OMPI_EVLOOP_ONCE);
#endif

    /* begin recording compound command */
    ompi_registry.begin_compound_cmd();

    /* Set process status to "terminating"*/
 	my_rank = ompi_comm_rank(&ompi_mpi_comm_world);
    my_status.rank = (int32_t)my_rank;
    my_status.local_pid = (int32_t)ompi_process_info.pid;
    my_status.nodename = strdup(ompi_system_info.nodename);
    my_status.status_key = OMPI_PROC_TERMINATING;
    my_status.exit_code = 0;
    if (OMPI_SUCCESS != (ret = ompi_rte_set_process_status(&my_status, ompi_rte_get_self()))) {
	return ret;
    }

    /* execute the compound command - no return data requested
     */
    ompi_registry.exec_compound_cmd(OMPI_REGISTRY_NO_RETURN_REQUESTED);

    /* wait for all processes to reach same state */
	mca_oob_barrier();
 
 	/* need the following code to cleanup the job in the registry.
 	 * once the state-of-health monitoring system is available, we will
 	 * have that system perform this function. until then, we will have the
 	 * rank 0 process do it.
 	 */
 	 if (0 == my_rank) {
 	 	my_jobid = ompi_name_server.get_jobid(ompi_rte_get_self());
 	 	ompi_rte_job_shutdown(my_jobid);
 	 }
 	 
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

    /* free requests */
    if (OMPI_SUCCESS != (ret = ompi_request_finalize())) {
	return ret;
    }

    /* Now that all MPI objects dealing with communications are gone,
       shut down MCA types having to do with communications */
    if (OMPI_SUCCESS != (ret = mca_ptl_base_close())) {
	return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_pml_base_close())) {
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

    /* free proc resources */
    if ( OMPI_SUCCESS != (ret = ompi_proc_finalize())) {
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

    /* free module exchange resources */
    if (OMPI_SUCCESS != (ret = mca_base_modex_finalize())) {
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
