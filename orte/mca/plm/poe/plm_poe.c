/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      IBM Corporation.  All rights reserved.
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

#include "orte_config.h"
#include "orte/constants.h"

#include <fcntl.h>
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#include "opal/mca/base/mca_base_param.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/session_dir.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/poe/plm_poe.h"

/*
 * Local functions
 */
static int init(void);
static int spawn(orte_job_t *jdata);
static int remote_spawn(opal_buffer_t *launch);
static int terminate_job(orte_jobid_t jobid);
static int terminate_orteds(void);
static int terminate_procs(opal_pointer_array_t *procs);
static int signal_job(orte_jobid_t jobid, int32_t signal);
static int finalize(void);

orte_plm_base_module_t orte_plm_poe_module = {
    init,
    orte_plm_base_set_hnp_name,
    spawn,
    remote_spawn,
    terminate_job,
    terminate_orteds,
    terminate_procs,
    signal_job,
    finalize
};

/* local functions */
static void poe_set_handler_default(int sig);
static int poe_argv_append_int(char ***argv, int varname, int min, char *argname);
static void poe_wait_job(pid_t pid, int status, void* cbdata);


static int init(void)
{
    if (0 != strncmp(mca_plm_poe_component.class,"interactive",11)) {
        /* only support interactive launch for now */
        return ORTE_ERR_NOT_IMPLEMENTED;
    }

    /* ensure that static ports were assigned - otherwise, we cant
     * work since we won't know how to talk to anyone else
     */
    if (NULL == getenv("OMPI_MCA_oob_tcp_static_ports") &&
        NULL == getenv("OMPI_MCA_oob_tcp_static_ports_v6")) {
        opal_output(0, "Static ports were not assigned");
        return ORTE_ERR_NOT_SUPPORTED;
    }

    return ORTE_SUCCESS;
}

static int spawn(orte_job_t *jdata)
{
    orte_job_map_t *map;
    FILE *hfp, *cfp;
    char **argv=NULL, *nodelist, **tmp=NULL, *ppnlist, *ppn, *ppnsave;
    int rc, pid, i, j, k;
    sigset_t sigs;
    orte_node_t *node;
    orte_proc_t *proc;
    orte_app_context_t *app;
    bool constantppn;
    int val;

    if( (NULL==(mca_plm_poe_component.cmdfile=tempnam(NULL,NULL))) ||
        (NULL==(cfp=fopen(mca_plm_poe_component.cmdfile,"w"))) ) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* setup the job */
    if (ORTE_SUCCESS != (rc = orte_plm_base_setup_job(jdata))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    mca_plm_poe_component.jobid = jdata->jobid;

    /* Get the map for this job */
    if (NULL == (map = orte_rmaps.get_job_map(jdata->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* create the nodelist */
    for (i=0; i < map->nodes->size; i++) {
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
            continue;
        }
        opal_argv_append_nosize(&tmp, node->name);
    }
    nodelist = opal_argv_join(tmp, ',');
    opal_argv_free(tmp);
    tmp = NULL;

    /* create the ppn list */
    constantppn = true;
    val = -1;
    for (i=0; i < map->nodes->size; i++) {
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
            continue;
        }
        asprintf(&ppn, "%d", (int)node->num_procs);
        opal_argv_append_nosize(&tmp, ppn);
        if (val < 0) {
            val = node->num_procs;
            ppnsave = strdup(ppn);
        } else {
            if (val != (int)node->num_procs) {
                constantppn = false;
                free(ppnsave);
            }
        }
        free(ppn);
    }
    if (constantppn) {
        opal_argv_free(tmp);
        tmp = NULL;
        opal_argv_append_nosize(&tmp, ppnsave);
        free(ppnsave);
    }
    ppnlist = opal_argv_join(tmp, ',');
    opal_argv_free(tmp);
    tmp = NULL;

    if(!strncmp(mca_plm_poe_component.resource_allocation,"hostfile",8)) {

        /* Create a temporary hostlist file if user requests it */

        if( (NULL==(mca_plm_poe_component.hostfile=tempnam(NULL,NULL))) ||
            (NULL==(hfp=fopen(mca_plm_poe_component.hostfile,"w"))) ) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        for (i=0; i < map->nodes->size; i++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
                continue;
            }
            for (j=0; j < node->procs->size; j++) {
                if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, j))) {
                    continue;
                }
                fprintf(hfp,"%s\n",node->name);
            }
        }
        fclose(hfp);
    }

    /* Create a temporary POE command file */

    for (i=0; i < map->nodes->size; i++) {
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
            continue;
        }
        for (j=0; j < node->procs->size; j++) {
            if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, j))) {
                continue;
            }
            app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, proc->app_idx);
            /* insert the values
             * NOTE: we don't have to pass a jobid as the generic ess module
             * will assign an arbitrary one. All we care is that it assign
             * the same value to every proc, which it will.
             */
            fprintf(cfp,"%s",mca_plm_poe_component.env);
            fprintf(cfp, " OMPI_MCA_ess=generic");
            if (ORTE_MAPPING_BYNODE & map->policy) {
                /* vpids were assigned bynode */
                fprintf(cfp, " OMPI_MCA_mapping=bynode");
            } else {
                /* vpids were assigned byslot */
                fprintf(cfp, " OMPI_MCA_mapping=byslot");
            }
            fprintf(cfp, " OMPI_MCA_orte_rank=%u", proc->name.vpid);
            fprintf(cfp, " OMPI_MCA_orte_num_procs=%u", jdata->num_procs);
            fprintf(cfp, " OMPI_MCA_orte_nodes=%s", nodelist);
            fprintf(cfp, " OMPI_MCA_orte_ppn=%s", ppnlist);
            fprintf(cfp, " OMPI_MCA_orte_app_num=%u", proc->app_idx);
            /*for (k=0; NULL != orte_launch_environ[k]; k++) {
                fprintf(cfp, " %s", orte_launch_environ[k]);
            }*/
            fprintf(cfp, " %s", app->app);
            for (k=1; NULL != app->argv[k]; k++) {
                fprintf(cfp, " %s", app->argv[k]);
            }
            fprintf(cfp, "\n");
        }
    }
    fclose(cfp);

    /* Generate POE command line */

    argv = opal_argv_copy(mca_plm_poe_component.argv);

    if(!strncmp(mca_plm_poe_component.resource_allocation,"hostfile",8)) {
        opal_argv_append_nosize(&argv, "-hostfile");
        opal_argv_append_nosize(&argv, mca_plm_poe_component.hostfile);
        opal_argv_append_nosize(&argv, "-resd");
        opal_argv_append_nosize(&argv, "no");
        rc=poe_argv_append_int(&argv, map->num_nodes, 1, "-nodes");
        if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    }

    opal_argv_append_nosize(&argv, "-pgmmodel");
    opal_argv_append_nosize(&argv, "mpmd");
    opal_argv_append_nosize(&argv, "-cmdfile");
    opal_argv_append_nosize(&argv, mca_plm_poe_component.cmdfile);
    opal_argv_append_nosize(&argv, "-labelio");
    opal_argv_append_nosize(&argv, mca_plm_poe_component.mp_labelio);
    opal_argv_append_nosize(&argv, "-stdoutmode");
    opal_argv_append_nosize(&argv, mca_plm_poe_component.mp_stdoutmode);

    rc=poe_argv_append_int(&argv, jdata->num_procs, 1, "-procs");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    rc=poe_argv_append_int(&argv, mca_plm_poe_component.mp_retry, 0, "-retry");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    rc=poe_argv_append_int(&argv, mca_plm_poe_component.mp_infolevel, 0, "-infolevel");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }

    opal_output_verbose(10, orte_plm_globals.output,
                        "%s plm:poe: POE cmdline: %s\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), opal_argv_join(argv, ' '));

    /* Start job with POE */

    pid = fork();
    if(pid < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if(pid == 0) {
        poe_set_handler_default(SIGTERM);
        poe_set_handler_default(SIGINT);
        poe_set_handler_default(SIGHUP);
        poe_set_handler_default(SIGCHLD);
        poe_set_handler_default(SIGPIPE);
        sigprocmask(0, 0, &sigs);
        sigprocmask(SIG_UNBLOCK, &sigs, 0);
        execv(mca_plm_poe_component.path, argv);
        opal_output(0, "orte_plm_poe: execv failed with errno=%d\n", errno);
        exit(-1);
    } else {
        orte_wait_cb(pid, poe_wait_job, NULL);
    }


 cleanup:
    if (NULL != argv) {
        opal_argv_free(argv);
    }
    return rc;
}

static int remote_spawn(opal_buffer_t *launch)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int terminate_job(orte_jobid_t jobid)
{
    orte_errmgr.update_state(jobid, ORTE_JOB_STATE_TERMINATED,
                             NULL, ORTE_PROC_STATE_TERMINATED,
                             0, 0);
    return ORTE_SUCCESS;
}


static int terminate_orteds(void)
{
    orte_errmgr.update_state(ORTE_PROC_MY_NAME->jobid, ORTE_JOB_STATE_TERMINATED,
                             NULL, ORTE_PROC_STATE_TERMINATED,
                             0, 0);
    return ORTE_SUCCESS;
}

static int terminate_procs(opal_pointer_array_t *procs)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int signal_job(orte_jobid_t jobid, int32_t signal)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int finalize(void)
{
    unlink(mca_plm_poe_component.cmdfile);
    unlink(mca_plm_poe_component.hostfile);
    return ORTE_SUCCESS;
}


/****    LOCAL FUNCTIONS    ****/

/**
poe_set_handler_default - set signal handler to default
@param sig signal [IN]
*/
static void poe_set_handler_default(int sig)
{
    struct sigaction act;

    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);
    sigaction(sig, &act, (struct sigaction *)0);
}

/**
poe_argv_append_int - append integer variable to argument variable
@param argv argument variable [OUT]
@param varname variable name [IN]
@param min minimum value [IN]
@param argname argument name [IN]
*/
static int poe_argv_append_int(char ***argv, int varname, int min, char *argname)
{
    char *tmp_string;
    if(varname >= min) {
        opal_argv_append_nosize(argv, argname);
        asprintf(&tmp_string, "%d", varname);
        opal_argv_append_nosize(argv, tmp_string);
        free(tmp_string);
    } else {
        return ORTE_ERR_BAD_PARAM;
    }
    return ORTE_SUCCESS;
}

/**
poe_wait_job - call back when POE finish
@param pid pid
@param status status
@param cbdata call back data
@return error number
*/
static void poe_wait_job(pid_t pid, int status, void* cbdata)
{
    /* get called back when poe job is done */
    if (0 == status) {
        orte_errmgr.update_state(mca_plm_poe_component.jobid, ORTE_JOB_STATE_TERMINATED,
                                 NULL, ORTE_PROC_STATE_TERMINATED,
                                 pid, status);
    } else {
         orte_errmgr.update_state(mca_plm_poe_component.jobid, ORTE_JOB_STATE_ABORTED,
                                 NULL, ORTE_PROC_STATE_ABORTED,
                                 pid, status);
    }
}
