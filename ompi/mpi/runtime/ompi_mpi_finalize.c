/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "orte_config.h"

#include "include/constants.h"
#include "include/orte_constants.h"
#include "mca/schema/schema.h"

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
#include "util/sys_info.h"
#include "runtime/runtime.h"
#include "runtime/ompi_progress.h"
#include "attribute/attribute.h"

#include "mca/base/base.h"
#include "mca/pml/base/pml_base_module_exchange.h"
#include "mca/pml/pml.h"
#include "mca/pml/base/base.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "mca/topo/topo.h"
#include "mca/topo/base/base.h"
#include "mca/io/io.h"
#include "mca/io/base/base.h"
#include "mca/oob/base/base.h"
#include "mca/ns/ns.h"
#include "mca/gpr/gpr.h"
#include "mca/rml/rml.h"
#include "mca/soh/soh.h"
#include "mca/soh/base/base.h"
#include "mca/errmgr/errmgr.h"
#include "mca/mpool/base/base.h"


int ompi_mpi_finalize(void)
{
    int ret;

    ompi_mpi_finalized = true;
#if OMPI_ENABLE_PROGRESS_THREADS == 0
    ompi_progress_events(OMPI_EVLOOP_NONBLOCK);
#endif

    /* Change progress function priority back to RTE level stuff */
    ompi_progress_mpi_disable();

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
    if (ORTE_SUCCESS != (ret = orte_rml.xcast(NULL, NULL, 0, NULL, NULL))) {
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
    if (OMPI_SUCCESS != (ret = mca_base_modex_finalize())) {
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
    if (OMPI_SUCCESS != (ret = mca_coll_base_close())) {
	return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_mpool_base_close())) {
	return ret;
    }

    /* Set process status to "finalized" */
    if (ORTE_SUCCESS != (ret = orte_soh.set_proc_soh(orte_process_info.my_name,
                                ORTE_PROC_STATE_FINALIZED, 0))) {
        ORTE_ERROR_LOG(ret);
    }

    /* Leave the RTE */

    if (OMPI_SUCCESS != (ret = orte_finalize())) {
	return ret;
    }

    /* All done */

    return MPI_SUCCESS;
}
