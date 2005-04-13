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

#include "ompi_config.h"

#include "include/constants.h"
#include "mpi/runtime/mpiruntime.h"
#include "mpi/runtime/params.h"
#include "runtime/runtime.h"
#include "runtime/ompi_progress.h"
#include "util/sys_info.h"
#include "util/proc_info.h"
#include "util/session_dir.h"
#include "mpi.h"
#include "communicator/communicator.h"
#include "group/group.h"
#include "info/info.h"
#include "util/show_help.h"
#include "util/stacktrace.h"
#include "errhandler/errcode.h"
#include "errhandler/errclass.h"
#include "request/request.h"
#include "op/op.h"
#include "file/file.h"
#include "attribute/attribute.h"
#include "threads/thread.h"

#include "mca/base/base.h"
#include "mca/allocator/base/base.h"
#include "mca/allocator/allocator.h"
#include "mca/mpool/base/base.h"
#include "mca/mpool/mpool.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/base.h"
#include "mca/pml/pml.h"
#include "mca/pml/base/base.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "mca/io/io.h"
#include "mca/io/base/base.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "mca/ns/ns.h"
#include "mca/gpr/gpr.h"
#include "mca/rml/rml.h"
#include "mca/soh/soh.h"
#include "mca/soh/base/base.h"
#include "mca/errmgr/errmgr.h"

#include "runtime/runtime.h"
#include "event/event.h"

/*
 * Global variables and symbols for the MPI layer
 */

bool ompi_mpi_initialized = false;
bool ompi_mpi_finalized = false;

bool ompi_mpi_thread_multiple = false;
int ompi_mpi_thread_requested = MPI_THREAD_SINGLE;
int ompi_mpi_thread_provided = MPI_THREAD_SINGLE;

ompi_thread_t *ompi_mpi_main_thread = NULL;

int ompi_mpi_init(int argc, char **argv, int requested, int *provided)
{
    int ret, param;
    ompi_proc_t** procs;
    size_t nprocs;
    char *error = NULL;
    bool compound_cmd = false;
    
    /* Join the run-time environment - do the things that don't hit
       the registry */

    if (ORTE_SUCCESS != (ret = orte_init_stage1())) {
        error = "ompi_mpi_init: orte_init_stage1 failed";
	    goto error;
    }

    /* If we are not the seed nor a singleton, AND we have not set the
       orte_debug flag, then start recording the compound command that
       starts us up.  if we are the seed or a singleton, then don't do
       this - the registry is local, so we'll just drive it
       directly */

    if (orte_process_info.seed ||
        NULL == orte_process_info.ns_replica ||
        orte_debug_flag) {
        compound_cmd = false;
    } else {
        if (ORTE_SUCCESS != (ret = orte_gpr.begin_compound_cmd())) {
            ORTE_ERROR_LOG(ret);
            error = "ompi_mpi_init: orte_gpr.begin_compound_cmd failed";
            goto error;
        }
        compound_cmd = true;
    }

    /* Now do the things that hit the registry */

    if (ORTE_SUCCESS != (ret = orte_init_stage2())) {
        ORTE_ERROR_LOG(ret);
        error = "ompi_mpi_init: orte_init_stage2 failed";
        goto error;
    }

    /* Once we've joined the RTE, see if any MCA parameters were
       passed to the MPI level */

    if (OMPI_SUCCESS != (ret = ompi_mpi_register_params())) {
        error = "mca_mpi_register_params() failed";
        goto error;
    }

#ifndef WIN32
    if (OMPI_SUCCESS != (ret = ompi_util_register_stackhandlers ())) {
	error = "util_register_stackhandlers() failed";
	goto error;
    }
#endif

    /* Initialize OMPI procs */

    if (OMPI_SUCCESS != (ret = ompi_proc_init())) {
        error = "mca_proc_init() failed";
        goto error;
    }

    /* Open up MPI-related MCA components */

    if (OMPI_SUCCESS != (ret = mca_allocator_base_open())) {
        error = "mca_allocator_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_mpool_base_open())) {
        error = "mca_mpool_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_pml_base_open())) {
        error = "mca_pml_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_ptl_base_open())) {
        error = "mca_ptl_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_coll_base_open())) {
        error = "mca_coll_base_open() failed";
        goto error;
    }

    /* In order to reduce the common case for MPI apps (where they
       don't use MPI-2 IO or MPI-1 topology functions), the io and
       topo frameworks are initialized lazily, at the first use of
       relevant functions (e.g., MPI_FILE_*, MPI_CART_*, MPI_GRAPH_*),
       so they are not opened here. */

    /* Initialize module exchange */

    if (OMPI_SUCCESS != (ret = mca_base_modex_init())) {
        error = "mca_base_modex_init() failed";
        goto error;
    }

    /* Select which MPI components to use */

    if (OMPI_SUCCESS != 
        (ret = mca_mpool_base_init(OMPI_ENABLE_PROGRESS_THREADS,
                                   OMPI_ENABLE_MPI_THREADS))) {
        error = "mca_mpool_base_init() failed";
        goto error;
    }

    if (OMPI_SUCCESS != 
        (ret = mca_pml_base_select(OMPI_ENABLE_PROGRESS_THREADS,
                                   OMPI_ENABLE_MPI_THREADS))) {
        error = "mca_pml_base_select() failed";
        goto error;
    }

    if (OMPI_SUCCESS != 
        (ret = mca_ptl_base_select(OMPI_ENABLE_PROGRESS_THREADS,
                                   OMPI_ENABLE_MPI_THREADS))) {
        error = "mca_ptl_base_select() failed";
        goto error;
    }

    if (OMPI_SUCCESS != 
        (ret = mca_coll_base_find_available(OMPI_ENABLE_PROGRESS_THREADS,
                                            OMPI_ENABLE_MPI_THREADS))) {
        error = "mca_coll_base_find_available() failed";
        goto error;
    }

    /* io and topo components are not selected here -- see comment
       above about the io and topo frameworks being loaded lazily */

    /* Initialize each MPI handle subsystem */
    /* initialize requests */
    if (OMPI_SUCCESS != (ret = ompi_request_init())) {
        error = "ompi_request_init() failed";
        goto error;
    }

    /* initialize info */
    if (OMPI_SUCCESS != (ret = ompi_info_init())) {
        error = "ompi_info_init() failed";
        goto error;
    }

    /* initialize error handlers */
    if (OMPI_SUCCESS != (ret = ompi_errhandler_init())) {
        error = "ompi_errhandler_init() failed";
        goto error;
    }

    /* initialize error codes */
    if (OMPI_SUCCESS != (ret = ompi_mpi_errcode_init())) {
        error = "ompi_mpi_errcode_init() failed";
        goto error;
    }

    /* initialize error classes */
    if (OMPI_SUCCESS != (ret = ompi_errclass_init())) {
        error = "ompi_errclass_init() failed";
        goto error;
    }
    
    /* initialize internal error codes */
    if (OMPI_SUCCESS != (ret = ompi_errcode_intern_init())) {
        error = "ompi_errcode_intern_init() failed";
        goto error;
    }
     
    /* initialize groups  */
    if (OMPI_SUCCESS != (ret = ompi_group_init())) {
        error = "ompi_group_init() failed";
        goto error;
    }

    /* initialize communicators */
    if (OMPI_SUCCESS != (ret = ompi_comm_init())) {
        error = "ompi_comm_init() failed";
        goto error;
    }

    /* initialize datatypes */
    if (OMPI_SUCCESS != (ret = ompi_ddt_init())) {
        error = "ompi_ddt_init() failed";
        goto error;
    }

    /* initialize ops */
    if (OMPI_SUCCESS != (ret = ompi_op_init())) {
        error = "ompi_op_init() failed";
        goto error;
    }

    /* initialize file handles */
    if (OMPI_SUCCESS != (ret = ompi_file_init())) {
        error = "ompi_file_init() failed";
        goto error;
    }

    /* initialize attribute meta-data structure for comm/win/dtype */
    if (OMPI_SUCCESS != (ret = ompi_attr_init())) {
        error = "ompi_attr_init() failed";
        goto error;
    }
    /* do module exchange */
    if (OMPI_SUCCESS != (ret = mca_base_modex_exchange())) {
        error = "ompi_base_modex_exchange() failed";
        goto error;
    }

    /* Let system know we are at STG1 Barrier */

    if (ORTE_SUCCESS != (ret = orte_soh.set_proc_soh(orte_process_info.my_name,
                                ORTE_PROC_STATE_AT_STG1, 0))) {
        ORTE_ERROR_LOG(ret);
        error = "set process state failed";
        goto error;
    }

    /* if the compound command is operative, execute it */

    if (compound_cmd) {
        if (OMPI_SUCCESS != (ret = orte_gpr.exec_compound_cmd())) {
            ORTE_ERROR_LOG(ret);
    	    error = "ompi_rte_init: orte_gpr.exec_compound_cmd failed";
    	    goto error;
        }
    }

     /* FIRST BARRIER - WAIT FOR MSG FROM RMGR_PROC_STAGE_GATE_MGR TO ARRIVE */
    if (ORTE_SUCCESS != (ret = orte_rml.xcast(NULL, NULL, 0, NULL, NULL))) {
        ORTE_ERROR_LOG(ret);
	    error = "ompi_mpi_init: failed to see all procs register\n";
	    goto error;
    }

    /* add all ompi_proc_t's to PML */
    if (NULL == (procs = ompi_proc_world(&nprocs))) {
        error = "ompi_proc_world() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_pml.pml_add_procs(procs, nprocs))) {
        free(procs);
        error = "PML add procs failed";
        goto error;
    }
    free(procs);

    /* start PTL's */
    param = 1;
    if (OMPI_SUCCESS != 
        (ret = mca_pml.pml_control(MCA_PTL_ENABLE, &param, sizeof(param)))) {
        error = "PML control failed";
        goto error;
    }

    /* Figure out the final MPI thread levels.  If we were not
       compiled for support for MPI threads, then don't allow
       MPI_THREAD_MULTIPLE. */

    ompi_mpi_thread_requested = requested;
    if (OMPI_HAVE_THREAD_SUPPORT == 1) {
        ompi_mpi_thread_provided = *provided = MPI_THREAD_SINGLE;
        ompi_mpi_main_thread = NULL;
    } else if (OMPI_ENABLE_MPI_THREADS == 1) {
        ompi_mpi_thread_provided = *provided = requested;
        ompi_mpi_main_thread = ompi_thread_get_self();
    } else {
        if (MPI_THREAD_MULTIPLE == requested) {
            ompi_mpi_thread_provided = *provided = MPI_THREAD_SERIALIZED;
        } else {
            ompi_mpi_thread_provided = *provided = requested;
        }
        ompi_mpi_main_thread = ompi_thread_get_self();
    }

    ompi_mpi_thread_multiple = (ompi_mpi_thread_provided == 
                                MPI_THREAD_MULTIPLE);
    if (OMPI_ENABLE_PROGRESS_THREADS == 1 ||
        OMPI_ENABLE_MPI_THREADS == 1) {
        ompi_set_using_threads(true);
    }

    /* Init coll for the comms */

    if (OMPI_SUCCESS != 
        (ret = mca_coll_base_comm_select(MPI_COMM_SELF, NULL))) {
        error = "mca_coll_base_comm_select(MPI_COMM_SELF) failed";
        goto error;
    }

    if (OMPI_SUCCESS !=
        (ret = mca_coll_base_comm_select(MPI_COMM_WORLD, NULL))) {
        error = "mca_coll_base_comm_select(MPI_COMM_WORLD) failed";
        goto error;
    }

#if OMPI_ENABLE_PROGRESS_THREADS && 0
    /* BWB - XXX - FIXME - is this actually correct? */
    /* setup I/O forwarding */
    if (orte_process_info.seed == false) {
        if (OMPI_SUCCESS != (ret = ompi_mpi_init_io())) {
	        error = "ompi_rte_init_io failed";
	        goto error;
        }
    }
#endif

    /* Let system know we are at STG2 Barrier */

    if (ORTE_SUCCESS != (ret = orte_soh.set_proc_soh(orte_process_info.my_name,
                                ORTE_PROC_STATE_AT_STG2, 0))) {
        ORTE_ERROR_LOG(ret);
        error = "set process state failed";
        goto error;
    }

     /* BWB - is this still needed? */
#if OMPI_ENABLE_PROGRESS_THREADS == 0
    ompi_progress_events(OMPI_EVLOOP_NONBLOCK);
#endif

    /* Second barrier -- wait for message from
       RMGR_PROC_STAGE_GATE_MGR to arrive */

    if (ORTE_SUCCESS != (ret = orte_rml.xcast(NULL, NULL, 0, NULL, NULL))) {
        ORTE_ERROR_LOG(ret);
        error = "ompi_mpi_init: failed to see all procs register\n";
        goto error;
    }

    /* new very last step: check whether we have been spawned or not.
       We introduce that at the very end, since we need collectives,
       datatypes, ptls etc. up and running here.... */

    if (OMPI_SUCCESS != (ret = ompi_comm_dyn_init())) {
        error = "ompi_comm_dyn_init() failed";
        goto error;
    }

 error:
    if (ret != OMPI_SUCCESS) {
        ompi_show_help("help-mpi-runtime",
                       "mpi_init:startup:internal-failure", true,
                       "MPI_INIT", "MPI_INIT", error, ret);
        return ret;
    }

    /* put the event library in "high performance MPI mode" */
    if (OMPI_SUCCESS != ompi_progress_mpi_init()) {
        error = "ompi_progress_mpi_init() failed";
        goto error;
    }

    /* All done.  Wasn't that simple? */

    ompi_mpi_initialized = true;

    if (orte_debug_flag) {
	ompi_output(0, "[%d,%d,%d] ompi_mpi_init completed",
		    ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    return MPI_SUCCESS;
}
