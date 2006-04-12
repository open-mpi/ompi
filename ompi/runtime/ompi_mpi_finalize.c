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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/event/event.h"
#include "opal/runtime/opal_progress.h"
#include "opal/mca/maffinity/base/base.h"
#include "opal/mca/base/base.h"

#include "orte/util/proc_info.h"
#include "orte/mca/schema/schema.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/soh/soh.h"
#include "orte/mca/soh/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/runtime.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/group/group.h"
#include "ompi/errhandler/errcode.h"
#include "ompi/errhandler/errclass.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/datatype.h"
#include "ompi/op/op.h"
#include "ompi/file/file.h"
#include "ompi/info/info.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/attribute/attribute.h"
#include "ompi/mca/pml/base/pml_base_module_exchange.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/topo/topo.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/mca/io/io.h"
#include "ompi/mca/io/base/base.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/rcache/base/base.h"


int ompi_mpi_finalize(void)
{
    int ret;

    /* Delete attributes on MPI_COMM_SELF per MPI-2:4.8.  Must be done
       before anything else in FINALIZE, and ensure that MPI_FINALIZED
       still returns false. */

    if (NULL != ompi_mpi_comm_self.c_keyhash) {
        ompi_attr_delete_all(COMM_ATTR, &ompi_mpi_comm_self,
                             ompi_mpi_comm_self.c_keyhash);
        OBJ_RELEASE(ompi_mpi_comm_self.c_keyhash);
        ompi_mpi_comm_self.c_keyhash = NULL;
    }

    ompi_mpi_finalized = true;
#if OMPI_ENABLE_PROGRESS_THREADS == 0
    opal_progress_events(OPAL_EVLOOP_NONBLOCK);
#endif
    /* If maffinity was setup, tear it down */
    if (ompi_mpi_maffinity_setup) {
        opal_maffinity_base_close();
    }

    /* Change progress function priority back to RTE level stuff */
    opal_progress_mpi_disable();

    /* begin recording compound command */
/*    if (OMPI_SUCCESS != (ret = orte_gpr.begin_compound_cmd())) {
        return ret;
    }
*/
    /* Set process status to "at stg3" */
    if (ORTE_SUCCESS != (ret = orte_soh.set_proc_soh(orte_process_info.my_name,
                                ORTE_PROC_STATE_AT_STG3, 0))) {
        ORTE_ERROR_LOG(ret);
    }

    /* execute the compound command - no return data requested
     */
/*    if (OMPI_SUCCESS != (ret = orte_gpr.exec_compound_cmd())) {
        return ret;
    }
*/
    /*
     * Wait for everyone to get here
     */
    if (ORTE_SUCCESS != (ret = orte_rml.xcast(NULL, NULL, 0, NULL,
                                 orte_gpr.deliver_notify_msg, NULL))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* Shut down any bindings-specific issues: C++, F77, F90 (may or
       may not be necessary...?) */

    /* Free communication objects */

    /* free window resources */

    /* free file resources */
    if (OMPI_SUCCESS != (ret = ompi_file_finalize())) {
	return ret;
    }

    /* free window resources */
    if (OMPI_SUCCESS != (ret = ompi_win_finalize())) {
	return ret;
    }
    if (OMPI_SUCCESS != (ret = ompi_osc_base_finalize())) {
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
    if (OMPI_SUCCESS != (ret = mca_pml_base_modex_finalize())) {
	return ret;
    }

    /* Close down MCA modules */

    /* io is opened lazily, so it's only necessary to close it if it
       was actually opened */

    if (mca_io_base_components_opened_valid ||
        mca_io_base_components_available_valid) {
        if (OMPI_SUCCESS != (ret = mca_io_base_close())) {
            return ret;
        }
    }
    if (OMPI_SUCCESS != (ret = mca_topo_base_close())) {
	return ret;
    }
    if (OMPI_SUCCESS != (ret = ompi_osc_base_close())) {
	return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_coll_base_close())) {
	return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_mpool_base_close())) {
	return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_rcache_base_close())) { 
        return ret;
    }
    
    /* Set process status to "finalized" */
    if (ORTE_SUCCESS != (ret = orte_soh.set_proc_soh(orte_process_info.my_name,
                                ORTE_PROC_STATE_FINALIZED, 0))) {
        ORTE_ERROR_LOG(ret);
    }

    /*
     * Wait for everyone to get here. This is necessary to allow the soh
     * to update the job state for singletons. Otherwise, we finalize
     * the RTE while the soh is trying to do the update - which causes
     * an ugly race condition
     */
    if (ORTE_SUCCESS != (ret = orte_rml.xcast(NULL, NULL, 0, NULL,
                                 orte_gpr.deliver_notify_msg, NULL))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* Leave the RTE */

    if (OMPI_SUCCESS != (ret = orte_finalize())) {
	return ret;
    }

    /* All done */

    return MPI_SUCCESS;
}
