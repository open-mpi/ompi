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

/** @file **/

#include "orte_config.h"

#include <sys/types.h>
#include <unistd.h>

#include "include/constants.h"
#include "event/event.h"
#include "util/output.h"
#include "threads/mutex.h"
#include "dps/dps.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/rml/base/base.h"
#include "mca/errmgr/base/base.h"
#include "mca/iof/base/base.h"
#include "mca/ns/base/base.h"
#include "mca/gpr/base/base.h"
#include "mca/rmgr/base/base.h"
#include "mca/rmaps/base/base.h"
#include "mca/soh/base/base.h"
#include "util/univ_info.h"
#include "util/proc_info.h"
#include "util/session_dir.h"
#include "util/sys_info.h"
#include "util/cmd_line.h"

#include "runtime/runtime.h"
#include "runtime/runtime_internal.h"
#include "runtime/orte_wait.h"

/**
 * Initialze and setup a process in the ORTE.
 *
 * @retval ORTE_SUCCESS Upon success.
 * @retval ORTE_ERROR Upon failure.
 *
 * This function performs 
 * 
 * Just a note for developer: 

 * So there are 3 ways in which an application can be started
 * 1) rte_boot, followed by mpirun
 * 2) mpirun (alone)
 * 3) singleton (./a.out)
 * 
 * Case 1) If the rte has already been booted, then mpirun will accept
 * an optional command line parameter --universe=[rte universe name]
 * which says which universe this application wants to be a part
 * of. mpirun will then package this universe name and send it to the
 * processes it will be starting off (fork/exec) on local or remote
 * node.The packaging mechanism can be either command line parameter
 * to the a.out it forks or make it part of environment
 * (implementation dependent).  
 *
 * Case 2) When mpirun is done alone and no universe is present, then
 * the mpirun starts off the universe (using rte_boot), then
 * fork/execs the processes, passin g along the [universe_name]. 
 *
 * Case 3) For a singleton, if there is alrady an existing rte
 * universe which it wants to join, it can specify that using the
 * --universe command line. So it will do 
 *
 * $ ./a.out --universe=[universe_name]
 * 
 * In this case, MPI_Init will have to be called as MPI_Init(&argc, &argv)

 * If it does not want to join any existing rte, then it just starts
 * off as ./a.out with no command line option. In that case, MPI_Init
 * does not necesaarily needs to passed argc and argv. Infact if argc
 * and argv are not passed or just have one entry (the command name),
 * then MPI_Init would assume that new rte universe needs to be
 * started up.
 *
 *
 * MPI_Init() will look at its argc, argv. If it find the universe
 * name there, fine. Else it looks at the environment variables for
 * universe_name. If it finds there, fine again. Under such
 * conditions, it joins the existing rte universe. If no universe
 * name is found, it calls rte_boot to start off a new rte universe.
 *
 * For singleton, MPI_Init() do:
 *
 * if (I am a singleton) and (there is no universe)
 *    do rte_boot
 *
 * But if I am not a singleton, then I have been started by mpirun and
 * already provided a universe_name to join. So I wont ever start a
 * universe under such conditons. mpirun will pass me the
 * universe_name (either mpirun would have started the universe, or it
 * would have been already started by rte_boot)
 */

/* globals used by RTE */
int orte_debug_flag=0;

int orte_init(void)
{
    int ret;
    char *universe;
    char *jobid_str = NULL;
    char *procid_str = NULL;
    pid_t pid;

    /* Open up the output streams */
    if (!ompi_output_init()) {
        return OMPI_ERROR;
    }
                                                                                                                   
    /* 
     * If threads are supported - assume that we are using threads - and reset otherwise. 
     */
    ompi_set_using_threads(OMPI_HAVE_THREAD_SUPPORT);
                                                                                                                   
    /* For malloc debugging */
    ompi_malloc_init();

    /* Ensure the universe_info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_univ_info())) {
        return ret;
    }

    /* Ensure the system_info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_sys_info())) {
        return ret;
    }

    /* Ensure the process info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_proc_info())) {
        return ret;
    }
    
    /*
     * Initialize the MCA framework 
     */
    if (OMPI_SUCCESS != (ret = mca_base_open())) {
        return ret;
    }
 
    /*
     * Open the name services to ensure access to local functions 
     */
    if (OMPI_SUCCESS != (ret = orte_ns_base_open())) {
        return ret;
    }

    /* Open the error manager to activate error logging - needs local name services */
    if (ORTE_SUCCESS != (ret = orte_errmgr_base_open())) {
        return ret;
    }
    
    /*****   ERROR LOGGING NOW AVAILABLE *****/
    
    /* check for debug flag */
    if (0 > (ret =  mca_base_param_register_int("orte", "debug", NULL, NULL, 0))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    if (ORTE_SUCCESS != (ret = mca_base_param_lookup_int(ret, &orte_debug_flag))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Initialize the event library 
    */
    if (OMPI_SUCCESS != (ret = ompi_event_init())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Internal startup
     */
    if (OMPI_SUCCESS != (ret = orte_wait_init())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Initialize the data packing service.
     */
    if (ORTE_SUCCESS != (ret = orte_dps_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /*
     * Runtime Messaging Layer
     */
    if (OMPI_SUCCESS != (ret = orte_rml_base_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Runtime Messaging Layer
     */
    if (OMPI_SUCCESS != (ret = orte_rml_base_select())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Registry
     */
    if (ORTE_SUCCESS != (ret = orte_gpr_base_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Initialize schema utilities
     */

    if (ORTE_SUCCESS != (ret = orte_schema_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /* check for existing universe to join */
    if (ORTE_SUCCESS != (ret = orte_universe_exists())) {
        if (orte_debug_flag) {
                ompi_output(0, "orte_init: could not join existing universe");
        }
        if (ORTE_ERR_NOT_FOUND != ret) {
                /* if it exists but no contact could be established,
                 * define unique name based on current one.
                 * and start new universe with me as seed
                 */
                universe = strdup(orte_universe_info.name);
                free(orte_universe_info.name);
                orte_universe_info.name = NULL;
                pid = getpid();
                if (0 > asprintf(&orte_universe_info.name, "%s-%d", universe, pid)) {
                    ompi_output(0, "orte_init: failed to create unique universe name");
                    return ret;
                }
        }

        orte_process_info.seed = true;
        if (NULL != orte_process_info.ns_replica) {
                free(orte_process_info.ns_replica);
                orte_process_info.ns_replica = NULL;
        }
        if (NULL != orte_process_info.gpr_replica) {
                free(orte_process_info.gpr_replica);
                orte_process_info.gpr_replica = NULL;
        }
    }

    /*
     * Name Server 
     */
    if (OMPI_SUCCESS != (ret = orte_ns_base_select())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Registry 
     */
    if (ORTE_SUCCESS != (ret = orte_gpr_base_select())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
 
    /*****    SET MY NAME    *****/
    if (ORTE_SUCCESS != (ret = orte_ns.set_my_name())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* setup my session directory */
    if (ORTE_SUCCESS != (ret = orte_ns.get_jobid_string(&jobid_str, orte_process_info.my_name))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    if (ORTE_SUCCESS != (ret = orte_ns.get_vpid_string(&procid_str, orte_process_info.my_name))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
 
    if (orte_debug_flag) {
        ompi_output(0, "[%d,%d,%d] setting up session dir with",
                    ORTE_NAME_ARGS(orte_process_info.my_name));
        if (NULL != orte_process_info.tmpdir_base) {
            ompi_output(0, "\ttmpdir %s", orte_process_info.tmpdir_base);
        }
        ompi_output(0, "\tuniverse %s", orte_universe_info.name);
        ompi_output(0, "\tuser %s", orte_system_info.user);
        ompi_output(0, "\thost %s", orte_system_info.nodename);
        ompi_output(0, "\tjobid %s", jobid_str);
        ompi_output(0, "\tprocid %s", procid_str);
    }
    if (ORTE_SUCCESS != (ret = orte_session_dir(true,
                                orte_process_info.tmpdir_base,
                                orte_system_info.user,
                                orte_system_info.nodename, NULL,
                                orte_universe_info.name,
                                jobid_str, procid_str))) {
        if (jobid_str != NULL) free(jobid_str);
        if (procid_str != NULL) free(procid_str);
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    if (NULL != jobid_str) {
        free(jobid_str);
    }
    if (NULL != procid_str) {
        free(procid_str);
    }

    /* set contact info for ns/gpr */
    if(NULL != orte_process_info.ns_replica_uri) {
        orte_rml.set_uri(orte_process_info.ns_replica_uri);
    }
    if(NULL != orte_process_info.gpr_replica_uri) {
        orte_rml.set_uri(orte_process_info.gpr_replica_uri);
    }

    /* open/load rmgr/soh */

    if (ORTE_SUCCESS != (ret = orte_rmgr_base_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    if (ORTE_SUCCESS != (ret = orte_soh_base_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

     /* setup jobid-0 */
 
    if(orte_process_info.seed) {
         if (ORTE_SUCCESS != (ret = orte_rmgr_base_set_job_slots(0,1))) {
             ORTE_ERROR_LOG(ret);
             return ret;
         }
         if (ORTE_SUCCESS != (ret = orte_rmaps_base_set_vpid_range(0,0,1))) {
             ORTE_ERROR_LOG(ret);
             return ret;
         }
         if (ORTE_SUCCESS != (ret = orte_rmgr_base_proc_stage_gate_init(0))) {
             ORTE_ERROR_LOG(ret);
             return ret;
         }
    }

    /* 
     * Initialize the selected modules now that all components/name are available.
     */

    if (ORTE_SUCCESS != (ret = orte_rml.init())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    if (ORTE_SUCCESS != (ret = orte_ns.init())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    if (ORTE_SUCCESS != (ret = orte_gpr.init())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /* 
     * setup the resource manager 
     */

    if (ORTE_SUCCESS != (ret = orte_rmgr_base_select())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /*
     * setup the state-of-health monitor
     */
    if (ORTE_SUCCESS != (ret = orte_soh_base_select())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /*
     * setup I/O forwarding system
     */
    if (ORTE_SUCCESS != (ret = orte_iof_base_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    if (ORTE_SUCCESS != (ret = orte_iof_base_select())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
     /* 
     * All done 
     */

    return ORTE_SUCCESS;
}

/* This array is provided so that users can output an intelligible name for a data
 * type during debugging. It is called by the ORTE_DATA_NAME macro defined in
 * include/orte_names.h. For example, you can call it as:
 * ompi_output(0, "data type: %s", ORTE_DATA_NAME(keyval.type));
 * 
 * THE CORRESPONDING MACRO IS DEFINED IN include/orte_names.h
 * AS IS THE EXTERN STATEMENT FOR ACCESSING THIS ARRAY
 */

char *orte_data_strings[] = {
    "DATA_TYPE_NOT_DEFINED",
    "ORTE_BYTE",
    "ORTE_BOOL",
    "ORTE_STRING",
    "ORTE_SIZE",
    /* all the integer flavors */
    "ORTE_INT",
    "ORTE_INT8",
    "ORTE_INT16",
    "ORTE_INT32",
    "ORTE_INT64",
    /* all the unsigned integer flavors */
    "ORTE_UINT",
    "ORTE_UINT8",
    "ORTE_UINT16",
    "ORTE_UINT32",
    "ORTE_UINT64",
    /* all the floating point flavors */
    "ORTE_FLOAT",
    "ORTE_FLOAT4",
    "ORTE_DOUBLE",
    "ORTE_FLOAT8",
    "ORTE_LONG_DOUBLE",
    "ORTE_FLOAT12",
    "ORTE_FLOAT16",
    /* orte-specific typedefs */
    "ORTE_NAME",
    "ORTE_VPID",
    "ORTE_JOBID",
    "ORTE_CELLID",
    "ORTE_NODE_STATE",
    "ORTE_PROC_STATE",
    "ORTE_EXIT_CODE",
    "ORTE_BYTE_OBJECT",
    "ORTE_KEYVAL",
    "ORTE_NOTIFY_ACTION",
    "ORTE_GPR_CMD",
    "ORTE_GPR_NOTIFY_ID",
    "ORTE_GPR_VALUE",
    "ORTE_DATA_TYPE",
    "ORTE_APP_CONTEXT",
    "ORTE_APP_CONTEXT_MAP",
    "ORTE_GPR_ADDR_MODE",
    "ORTE_GPR_SUBSCRIPTION",
    "ORTE_GPR_NOTIFY_DATA",
    "ORTE_NULL"
};


/*
 * Similar to the above, this array is used to output intelligible error
 * messages. It is disturbing to think that we are still outputing error numbers and
 * expecting users to look them up in the "big book" to find out what they represent.
 * This array allows the user to output an actual string representation of the error.
 *
 * THE CORRESPONDING MACRO IS DEFINED IN include/orte_names.h
 * AS IS THE EXTERN STATEMENT FOR ACCESSING THIS ARRAY
 */

char *orte_error_strings[] = {
    "ORTE_SUCCESS",
    "ORTE_ERROR",
    "ORTE_ERR_OUT_OF_RESOURCE", /* fatal error */
    "ORTE_ERR_TEMP_OUT_OF_RESOURCE", /* try again later */
    "ORTE_ERR_RESOURCE_BUSY",
    "ORTE_ERR_BAD_PARAM",     /* equivalent to MPI_ERR_ARG error code */
    "ORTE_ERR_RECV_LESS_THAN_POSTED",
    "ORTE_ERR_RECV_MORE_THAN_POSTED",
    "ORTE_ERR_NO_MATCH_YET",
    "ORTE_ERR_FATAL",
    "ORTE_ERR_NOT_IMPLEMENTED",
    "ORTE_ERR_NOT_SUPPORTED",
    "ORTE_ERR_INTERUPTED",
    "ORTE_ERR_WOULD_BLOCK",
    "ORTE_ERR_IN_ERRNO",
    "ORTE_ERR_UNREACH",
    "ORTE_ERR_NOT_FOUND",
    "ORTE_ERR_BUFFER", /* equivalent to MPI_ERR_BUFFER */
    "ORTE_ERR_REQUEST", /* equivalent to MPI_ERR_REQUEST */
    "ORTE_EXISTS",  /* indicates that the specified object already exists */
    "ORTE_ERR_NO_CONNECTION_ALLOWED", /* indicates that the receiving process does not allow connections */
    "ORTE_ERR_CONNECTION_REFUSED", /* contact made with process, but it refuses any further communication */
    "ORTE_ERR_CONNECTION_FAILED",  /* message sent, but delivery failed */
    "ORTE_ERR_TIMEOUT",
    "ORTE_STARTUP_DETECTED",
    "ORTE_SHUTDOWN_DETECTED",
    "ORTE_PROC_STARTING",
    "ORTE_PROC_STOPPED",
    "ORTE_PROC_TERMINATING",
    "ORTE_PROC_ALIVE",
    "ORTE_PROC_RUNNING",
    "ORTE_PROC_KILLED",
    "ORTE_PROC_EXITED",
    "ORTE_NODE_UP",
    "ORTE_NODE_DOWN",
    "ORTE_NODE_BOOTING",
    "ORTE_NODE_ERROR",
    "ORTE_PACK_MISMATCH",
    "ORTE_ERR_PACK_FAILURE",
    "ORTE_ERR_UNPACK_FAILURE",
    "ORTE_ERR_COMM_FAILURE",
    "ORTE_UNPACK_INADEQUATE_SPACE",
    "ORTE_UNPACK_READ_PAST_END_OF_BUFFER",
    "ORTE_ERR_NOT_AVAILABLE",
    "ORTE_ERR_GPR_DATA_CORRUPT",
    "ORTE_ERR_PERM"
};

