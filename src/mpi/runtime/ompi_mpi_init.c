/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "mpi/runtime/mpiruntime.h"
#include "mpi/runtime/params.h"
#include "runtime/runtime.h"
#include "util/sys_info.h"
#include "util/proc_info.h"
#include "util/session_dir.h"
#include "mpi.h"
#include "communicator/communicator.h"
#include "group/group.h"
#include "info/info.h"
#include "util/show_help.h"
#include "errhandler/errhandler.h"
#include "errhandler/errcode.h"
#include "errhandler/errclass.h"
#include "errhandler/errcode-internal.h"
#include "op/op.h"
#include "file/file.h"

#include "mca/base/base.h"
#include "mca/base/base.h"
#include "mca/allocator/base/base.h"
#include "mca/allocator/allocator.h"
#include "mca/mpool/base/base.h"
#include "mca/mpool/mpool.h"
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

#include "runtime/runtime.h"


/*
 * Global variables and symbols for the MPI layer
 */

bool ompi_mpi_initialized = false;
bool ompi_mpi_finalized = false;

bool ompi_mpi_thread_multiple = false;
int ompi_mpi_thread_requested = MPI_THREAD_SINGLE;
int ompi_mpi_thread_provided = MPI_THREAD_SINGLE;


int ompi_mpi_init(int argc, char **argv, int requested, int *provided)
{
    int ret, param;
    mca_ns_base_jobid_t jobid;
    mca_ns_base_vpid_t vpid;
    bool allow_multi_user_threads;
    bool have_hidden_threads;
    ompi_proc_t** procs;
    size_t nprocs;
    char *error, *jobid_str, *procid_str;

    /* Become an OMPI process */

    if (OMPI_SUCCESS != (ret = ompi_init(argc, argv))) {
        error = "ompi_init() failed";
        goto error;
    }

    /* Open up the MCA */

    if (OMPI_SUCCESS != (ret = mca_base_open())) {
        error = "mca_base_open() failed";
        goto error;
    }

    /* Join the run-time environment */
    allow_multi_user_threads = true;
    have_hidden_threads = false;
    if (OMPI_SUCCESS != (ret = ompi_rte_init_stage1(&allow_multi_user_threads,
						    &have_hidden_threads))) {
	goto error;
    }

    /* parse environmental variables and fill corresponding info structures */
    ompi_rte_parse_environ();

    /* check for existing universe to join */
    if (OMPI_SUCCESS != (ret = ompi_rte_universe_exists())) {
	if (ompi_rte_debug_flag) {
	    ompi_output(0, "ompi_mpi_init: could not join existing universe");
	}
    }

    /* start the rest of the rte */
    if (OMPI_SUCCESS != (ret = ompi_rte_init_stage2(&allow_multi_user_threads,
						    &have_hidden_threads))) {
        error = "mca_rte_init() failed";
        goto error;
    }

    /*****    SET MY NAME   *****/
    if (NULL != ompi_process_info.name) {  /* should NOT have been previously set */
	free(ompi_process_info.name);
    }
    if (NULL == ompi_rte_get_self()) {  /* no name set in environment - must be singleton */
	if (NULL == ompi_process_info.ns_replica) { /* couldn't join existing univ */
	    ompi_process_info.name = ompi_name_server.create_process_name(0,0,0);
	} else {  /* name server exists elsewhere - get a name for me */
	    jobid = ompi_name_server.create_jobid();
	    vpid = ompi_name_server.reserve_range(jobid, 1);
	    ompi_process_info.name = ompi_name_server.create_process_name(0, jobid, vpid);
	}
    } else {  /* name set in environment - record it */
	ompi_process_info.name = ompi_rte_get_self();
    }

    /* setup my session directory */
    jobid_str = ompi_name_server.get_jobid_string(ompi_process_info.name);
    procid_str = ompi_name_server.get_vpid_string(ompi_process_info.name);
 
    if (ompi_rte_debug_flag) {
	ompi_output(0, "[%d,%d,%d] setting up session dir with",
		    ompi_process_info.name->cellid,
		    ompi_process_info.name->jobid,
		    ompi_process_info.name->vpid);
	if (NULL != ompi_process_info.tmpdir_base) {
	    ompi_output(0, "\ttmpdir %s", ompi_process_info.tmpdir_base);
	}
	ompi_output(0, "\tuniverse %s", ompi_process_info.my_universe);
	ompi_output(0, "\tuser %s", ompi_system_info.user);
	ompi_output(0, "\thost %s", ompi_system_info.nodename);
	ompi_output(0, "\tjobid %s", jobid_str);
	ompi_output(0, "\tprocid %s", procid_str);
    }
    if (OMPI_ERROR == ompi_session_dir(true,
				       ompi_process_info.tmpdir_base,
				       ompi_system_info.user,
				       ompi_system_info.nodename, NULL, 
				       ompi_process_info.my_universe,
				       jobid_str, procid_str)) {
	if (jobid_str != NULL) free(jobid_str);
	if (procid_str != NULL) free(procid_str);
	error = "session dir not found or created";
	goto error;
    }


    /* finalize the rte startup */
    if (OMPI_SUCCESS != (ret = ompi_rte_init_finalstage(&allow_multi_user_threads,
							 &have_hidden_threads))) {
        error = "mpi_init: failed in ompi_rte_init\n";
        goto error;
    }

    /*
     *  Register my process info with my replica. Note that this must be done
     *  after the rte init is completed.
     */
    if (OMPI_SUCCESS != (ret = ompi_rte_register())) {
        error = "ompi_rte_init: failed in ompi_rte_register()\n";
        goto error;
    } 

    /* Once we've joined the RTE, see if any MCA parameters were
       passed to the MPI level */

    if (OMPI_SUCCESS != (ret = ompi_mpi_register_params())) {
        error = "mca_mpi_register_params() failed";
        goto error;
    }

    /* initialize ompi procs */
    if (OMPI_SUCCESS != (ret = ompi_proc_init())) {
        error = "mca_proc_init() failed";
        goto error;
    }

    /* Open up relevant MCA modules. */

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
    if (OMPI_SUCCESS != (ret = mca_topo_base_open())) {
        error = "mca_topo_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_io_base_open())) {
        error = "mca_io_base_open() failed";
        goto error;
    }

    /* Select which pml, ptl, and coll modules to use, and determine the
       final thread level */

    if (OMPI_SUCCESS != 
	(ret = mca_base_init_select_components(requested, 
					       allow_multi_user_threads,
					       have_hidden_threads, 
					       provided))) {
        error = "mca_base_init_select_components() failed";
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

    /* save the resulting thread levels */

    ompi_mpi_thread_requested = requested;
    *provided = ompi_mpi_thread_provided;
    ompi_mpi_thread_multiple = (ompi_mpi_thread_provided == 
                                MPI_THREAD_MULTIPLE);

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

    /* Wait for everyone to initialize */

    if (MPI_SUCCESS != (ret = 
                        MPI_COMM_WORLD->c_coll.coll_barrier(MPI_COMM_WORLD))) {
        error = "Barrier over MPI_COMM_WORLD failed";
        goto error;
    }

 error:
    if (ret != OMPI_SUCCESS) {
        ompi_show_help("help-mpi-runtime",
                       "mpi_init:startup:internal-failure", true,
                       "MPI_INIT", "MPI_INIT", error, ret);
        return ret;
    }

    /* All done */

    ompi_mpi_initialized = true;
    ompi_mpi_finalized = false;
    return MPI_SUCCESS;
}
