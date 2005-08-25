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
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "ompi_config.h"

#include <sys/types.h>
#include <unistd.h>
#include <signal.h>

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/path.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/runtime/runtime.h"
#include "orte/include/orte_constants.h"
#include "orte/include/orte_types.h"
#include "orte/include/orte_constants.h"
#include "orte/mca/pls/pls.h"
#include "orte/mca/pls/base/base.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "pls_slurm.h"


/*
 * Local functions
 */
static int pls_slurm_launch(orte_jobid_t jobid);
static int pls_slurm_terminate_job(orte_jobid_t jobid);
static int pls_slurm_terminate_proc(const orte_process_name_t *name);
static int pls_slurm_finalize(void);

static int pls_slurm_start_proc(int argc, char **argv, char **env);


/*
 * Global variable
 */
orte_pls_base_module_1_0_0_t orte_pls_slurm_module = {
    pls_slurm_launch,
    pls_slurm_terminate_job,
    pls_slurm_terminate_proc,
    pls_slurm_finalize
};

/*
 * Local variable
 */
static pid_t srun_pid = 0;


/*
 * External
 */
extern char **environ;


static int pls_slurm_launch(orte_jobid_t jobid)
{
    opal_list_t nodes;
    opal_list_item_t* item;
    size_t num_nodes;
    orte_vpid_t vpid;
    int node_name_index;
    int proc_name_index;
    char *jobid_string;
    char *uri, *param;
    char **argv;
    int argc;
    int rc;
    char *tmp;
    char** env;
    char* var;

    /* query the list of nodes allocated to the job - don't need the entire
     * mapping - as the daemon/proxy is responsibe for determining the apps
     * to launch on each node.
     */
    OBJ_CONSTRUCT(&nodes, opal_list_t);

    rc = orte_ras_base_node_query_alloc(&nodes, jobid);
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    /*
     * Allocate a range of vpids for the daemons.
     */
    num_nodes = opal_list_get_size(&nodes);
    if (num_nodes == 0) {
        return ORTE_ERR_BAD_PARAM;
    }
    rc = orte_ns.reserve_range(0, num_nodes, &vpid);
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    /* need integer value for command line parameter */
    asprintf(&jobid_string, "%lu", (unsigned long) jobid);

    /*
     * start building argv array
     */
    argv = NULL;
    argc = 0;

    /* add the srun command */
    opal_argv_append(&argc, &argv, "srun");

    /* "--mpi=lam" implies the task count equals the node count */
    opal_argv_append(&argc, &argv, "--mpi=lam");

    asprintf(&tmp, "--nodes=%lu", num_nodes);
    opal_argv_append(&argc, &argv, tmp);
    free(tmp);

    /* add the daemon command (as specified by user) */
    opal_argv_append(&argc, &argv, mca_pls_slurm_component.orted);
    opal_argv_append(&argc, &argv, "--no-daemonize");
    
    /* check for debug flags */
    orte_pls_base_proxy_mca_argv(&argc, &argv);

    /* proxy information */
    opal_argv_append(&argc, &argv, "--bootproxy");
    opal_argv_append(&argc, &argv, jobid_string);
    opal_argv_append(&argc, &argv, "--name");
    /* JMS: what to do here? */
    proc_name_index = argc;
    opal_argv_append(&argc, &argv, "BOGUS");

    /* tell the daemon how many procs are in the daemon's job */
    /* JMS: what does this do -- is it necessary?  i.e., doesn't the
       daemon pull this info from the gpr? */
    opal_argv_append(&argc, &argv, "--num_procs");
    asprintf(&param, "%lu", (unsigned long)(vpid + num_nodes));
    opal_argv_append(&argc, &argv, param);
    free(param);

    /* tell the daemon the starting vpid of the daemon's job */
    /* JMS: ditto previous comment */
    opal_argv_append(&argc, &argv, "--vpid_start");
    opal_argv_append(&argc, &argv, "0");
    
    opal_argv_append(&argc, &argv, "--nodename");
    node_name_index = argc;
    /* JMS: what to do here? */
    opal_argv_append(&argc, &argv, "BOGUS");

    /* pass along the universe name and location info */
    opal_argv_append(&argc, &argv, "--universe");
    asprintf(&param, "%s@%s:%s", orte_universe_info.uid,
             orte_universe_info.host, orte_universe_info.name);
    opal_argv_append(&argc, &argv, param);
    free(param);
    
    /* setup ns contact info */
    opal_argv_append(&argc, &argv, "--nsreplica");
    if (NULL != orte_process_info.ns_replica_uri) {
        uri = strdup(orte_process_info.ns_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    opal_argv_append(&argc, &argv, param);
    free(uri);
    free(param);

    /* setup gpr contact info */
    opal_argv_append(&argc, &argv, "--gprreplica");
    if (NULL != orte_process_info.gpr_replica_uri) {
        uri = strdup(orte_process_info.gpr_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    opal_argv_append(&argc, &argv, param);
    free(uri);
    free(param);

    if (mca_pls_slurm_component.debug) {
        param = opal_argv_join(argv, ' ');
        if (NULL != param) {
            opal_output(0, "pls:slurm: final top-level argv:");
            opal_output(0, "pls:slurm:     %s", param);
            free(param);
        }
    }

    /* Bookkeeping -- save the node names */
    for (item =  opal_list_get_first(&nodes);
         item != opal_list_get_end(&nodes);
         item =  opal_list_get_next(item)) {
        orte_ras_node_t* node = (orte_ras_node_t*)item;
        orte_process_name_t* name;
        char* name_string;

        /* initialize daemons process name */
        rc = orte_ns.create_process_name(&name, node->node_cellid, 0, vpid);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* setup process name */
        rc = orte_ns.get_proc_name_string(&name_string, name);
        if (ORTE_SUCCESS != rc) {
            opal_output(0, "pls:slurm: unable to create process name");
            exit(-1);
        }

        /* save the daemons name on the node */
        if (ORTE_SUCCESS != 
            (rc = orte_pls_base_proxy_set_node_name(node, jobid, name))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        vpid++;
        free(name);
    }
    
    /* setup environment */
    env = opal_argv_copy(environ);
    var = mca_base_param_environ_variable("seed", NULL, NULL);
    opal_setenv(var, "0", true, &env);

#if 0
    /* JMS What to do for sched_yield? */

    /* set the progress engine schedule for this node.  if node_slots
       is set to zero, then we default to NOT being oversubscribed */
    if (node->node_slots > 0 &&
        node->node_slots_inuse > node->node_slots) {
        if (mca_pls_slurm_component.debug) {
            opal_output(0, "pls:slurm: oversubscribed -- setting mpi_yield_when_idle to 1");
        }
        var = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
        opal_setenv(var, "1", true, &env);
    } else {
        if (mca_pls_slurm_component.debug) {
            opal_output(0, "pls:slurm: not oversubscribed -- setting mpi_yield_when_idle to 0");
        }
        var = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
        opal_setenv(var, "0", true, &env);
    }
    free(var);
#endif
    
    /* exec the daemon */
    rc = pls_slurm_start_proc(argc, argv, env);
    if (ORTE_SUCCESS != rc) {
        opal_output(0, "pls:slurm: start_procs returned error %d", rc);
        goto cleanup;
    }

    /* JMS: short we stash the srun pid in the gpr somewhere for cleanup? */
    /* JMS: how do we catch when srun dies? */
    
cleanup:
    while (NULL != (item = opal_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);
    return rc;
}


static int pls_slurm_terminate_job(orte_jobid_t jobid)
{
    if (0 != srun_pid) {
        kill(srun_pid, SIGHUP);
        /* JMS need appropriate code here to reap */
        srun_pid = 0;
    }
    return orte_pls_base_proxy_terminate_job(jobid);
}


/*
 * The way we've used SLURM, we can't kill individual processes --
 * we'll kill the entire job
 */
static int pls_slurm_terminate_proc(const orte_process_name_t *name)
{
    opal_output(orte_pls_base.pls_output,
                "pls:slurm:terminate_proc: not supported");
    return ORTE_ERR_NOT_SUPPORTED;
}


static int pls_slurm_finalize(void)
{
    /* cleanup any pending recvs */
    orte_rml.recv_cancel(ORTE_RML_NAME_ANY, ORTE_RML_TAG_RMGR_CLNT);

    return ORTE_SUCCESS;
}


static int pls_slurm_start_proc(int argc, char **argv, char **env)
{
    int rc;
    char *exec_argv = opal_path_findv(argv[0], 0, env, NULL);

    if (NULL == exec_argv) {
        return ORTE_ERR_NOT_FOUND;
    }

    srun_pid = fork();
    if (-1 == srun_pid) {
        printf("Fork failed!\n");
        return ORTE_ERR_IN_ERRNO;
    } else if (0 == srun_pid) {
        rc = execve(exec_argv, argv, env);
        printf("execve failed! (%s)\n", argv[0]);
        return ORTE_ERR_IN_ERRNO;
    }

    return ORTE_SUCCESS;
}
