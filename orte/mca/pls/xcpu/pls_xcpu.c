/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

/* @file:
 * xcpu Lancher to launch jobs on compute nodes..
 */

#include "orte_config.h"
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif  /* HAVE_SYS_STAT_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <errno.h>
#include <signal.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/event/event.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/path.h"
#include "opal/util/show_help.h"

#include "orte/dss/dss.h"
#include "orte/util/sys_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/gpr/base/base.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/sds/base/base.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/smr/base/base.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/runtime.h"

#include <math.h>
#include "pls_xcpu.h"
#include "spfs.h"
#include "spclient.h"
#include "strutil.h"
#include "libxcpu.h"

/**
 * Initialization of the xcpu module with all the needed function pointers
 */
orte_pls_base_module_t orte_pls_xcpu_module = {
	orte_pls_xcpu_launch_job,
	orte_pls_xcpu_terminate_job,
	orte_pls_xcpu_terminate_orteds,
	orte_pls_xcpu_terminate_proc,
	orte_pls_xcpu_signal_job,
	orte_pls_xcpu_signal_proc,
    orte_pls_xcpu_cancel_operation,
	orte_pls_xcpu_finalize
};

/* array of *Xpcommand and Xpnodeset, each xcmd/nodeset correspond to one OMPI app_context */
Xpcommand **xcmd_sets;
Xpnodeset **node_sets;
int num_apps;

void
pls_xcpu_stdout_cb(Xpsession *s, u8 *buf, u32 buflen)
{
        fprintf(stdout, "%.*s", buflen, buf);
}

void
pls_xcpu_stderr_cb(Xpsession *s, u8 *buf, u32 buflen)
{
        fprintf(stderr, "%.*s", buflen, buf);
}

void
pls_xcpu_wait_cb(Xpsession *s, u8 *buf, u32 buflen)
{
        Xpnode *nd;

        nd = xp_session_get_node(s);

	/* FixMe: find out the process associated with this session */
	orte_smr.set_proc_state(nd->data, ORTE_PROC_STATE_TERMINATED, 0);
}

static char *
process_list(char **list, char sep)
{
	int i, n, len;
	char *s, *ret;
	char **items;

	/* find list length */
	for(n = 0; list[n] != NULL; n++)
		;

	items = calloc(n, sizeof(char *));
	if (!items)
		return NULL;

	/* quote the items if necessary */
	for(len = 0, i = 0; i < n; i++) {
		items[i] = quotestrdup(list[i]);
		len += strlen(items[i]) + 1;
	}

	ret = malloc(len+1);
	if (!ret)
		return NULL;

	for(s = ret, i = 0; i < n; i++) {
		len = strlen(items[i]);
		memcpy(s, items[i], len);
		s += len;
		*(s++) = sep;
		free(items[i]);
	}

	*s = '\0';
	free(items);
	return ret;
}

static char *
process_env(char **env)
{
	return process_list(env, '\n');
}

static char *
process_argv(char **argv)
{
	return process_list(argv, ' ');
}

static void
pls_xcpu_setup_env(char ***e)
{

	int n, rc;
	char *var, *param, *uri;
	char **env;

	/* FixME: pointer arthematic */
	n = opal_argv_count(*e);
	rc = mca_base_param_build_env(*e, &n, false);
	if (rc != ORTE_SUCCESS) {
		ORTE_ERROR_LOG(rc);
		return rc;
	}

	if (NULL != orte_process_info.ns_replica_uri) {
		uri = strdup(orte_process_info.ns_replica_uri);
	} else {
		uri = orte_rml.get_uri();
	}
	param = mca_base_param_environ_variable("ns", "replica", "uri");
	opal_setenv(param, uri, true, e);
	free(param);
	free(uri);

	if (NULL != orte_process_info.gpr_replica_uri) {
		uri = strdup(orte_process_info.gpr_replica_uri);
	} else {
		uri = orte_rml.get_uri();
	}
	param = mca_base_param_environ_variable("gpr", "replica", "uri");
	opal_setenv(param, uri, true, e);
	free(param);
	free(uri);

#if 0
	/* FixMe: Is this the frontend or backend nodename ? we don't have the starting
	 * daemon. */
	var = mca_base_param_environ_variable("orte", "base", "nodename");
	opal_setenv(var, orte_system_info.nodename, true, e);
	free(var);
#endif

	var = mca_base_param_environ_variable("universe", NULL, NULL);
	asprintf(&param, "%s@%s:%s", orte_universe_info.uid, 
		orte_universe_info.host, orte_universe_info.name);
	opal_setenv(var, param, true, e);

	free(param);
	free(var);
#if 0
	/* FixMe: do this only when we oversubscribe */
        var = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
        opal_setenv(var, "1", true, e);
	free(var);
#endif
	/* merge in environment */
	env = opal_environ_merge(*e, environ);
	opal_argv_free(*e);
	*e = env;

}

/* This is the main function that will launch jobs on remote compute modes
 * @param jobid the jobid of the job to launch
 * @retval ORTE_SUCCESS or error
 */
int
orte_pls_xcpu_launch_job(orte_jobid_t jobid)
{
	int i, fanout, rc;
	int num_processes = 0;
	orte_cellid_t cellid;
	opal_list_item_t *node_item, *proc_item;
	orte_job_map_t *map;
	orte_vpid_t vpid_start, vpid_range;

	/* get the job map */
	rc = orte_rmaps.get_job_map(&map, jobid);
	if (rc != ORTE_SUCCESS) {
		ORTE_ERROR_LOG(rc);
		return rc;
	}
	num_apps = map->num_apps;

	/* next, get the vpid_start and range */
    vpid_start = 0;
	rc = orte_ns.get_vpid_range(jobid, &vpid_range);
	if (rc != ORTE_SUCCESS) {
		ORTE_ERROR_LOG(rc);
		return rc;
	}

	/* get the cellid */
	cellid = orte_process_info.my_name->cellid;

	/* create num_apps of pointers to Xpnodeset and Xpcommand */
	node_sets = (Xpnodeset **) malloc(num_apps * sizeof(Xpnodeset *));
	xcmd_sets = (Xpcommand **) malloc(num_apps * sizeof(Xpcommand *));

	/* create Xpnodeset for each app_context */
	for (i = 0; i < num_apps; i++) {
		node_sets[i] = xp_nodeset_create();
	}

	/* create Xpnode for each mapped proc, add them to corresponding Xpnodeset
	 * according to their app context */
	for (node_item  = opal_list_get_first(&map->nodes);
	     node_item != opal_list_get_end(&map->nodes);
	     node_item  = opal_list_get_next(node_item)) {
		orte_mapped_node_t *node = (orte_mapped_node_t *) node_item;
		for (proc_item  = opal_list_get_first(&node->procs);
		     proc_item != opal_list_get_end(&node->procs);
		     proc_item  = opal_list_get_next(proc_item)) {
			orte_mapped_proc_t *proc = (orte_mapped_proc_t *) proc_item;
			Xpnode *xpnode = xp_node_create(node->nodename, node->nodename,
						     NULL, NULL);
			xpnode->data = &proc->name;
			xp_nodeset_add(node_sets[proc->app_idx], xpnode);
		}
	}

	/* setup envrionment variables for each app context */
	for (i = 0; i < num_apps; i++) {
		/* FixME: how many layers of *? */
		pls_xcpu_setup_env(&map->apps[i]->env);
		num_processes += map->apps[i]->num_procs;
	}

	for (i = 0; i < num_apps; i++) {
		rc = orte_ns_nds_xcpu_put(cellid, jobid, vpid_start,
					  num_processes, &map->apps[i]->env);
		if (rc != ORTE_SUCCESS) {
			ORTE_ERROR_LOG(rc);
			return rc;
		}
	}

	/* create Xpcommand for each app_context from Xpnodeset */
	for (i = 0; i < num_apps; i++) {
		xcmd_sets[i] = xp_command_create(node_sets[i]);

		/* caculate maximum fan out for tree spawn */
		if (mca_pls_xcpu_component.maxsessions < 0) {
			fanout = (int) sqrt(node_sets[i]->len);
			if (fanout*fanout < node_sets[i]->len)
				fanout++;
		} else
			fanout = mca_pls_xcpu_component.maxsessions;
		xcmd_sets[i]->nspawn = fanout;

		/* setup argc, argv and evn in xcpu command */
		xcmd_sets[i]->cwd  = strdup(map->apps[i]->cwd);
		xcmd_sets[i]->env  = process_env(map->apps[i]->env);
		xcmd_sets[i]->argv = process_argv(map->apps[i]->argv);
		xcmd_sets[i]->exec = strdup(map->apps[i]->argv[0]);
		xcmd_sets[i]->copypath = strdup(map->apps[i]->app);
		asprintf(&xcmd_sets[i]->jobid, "%d", jobid);

		/* setup io forwarding */
		xcmd_sets[i]->stdout_cb = pls_xcpu_stdout_cb;
		xcmd_sets[i]->stderr_cb = pls_xcpu_stderr_cb;
		xcmd_sets[i]->wait_cb   = pls_xcpu_wait_cb;

		/* call xp_command_exec(xcmd) */
		if (xp_command_exec(xcmd_sets[i]) < 0)
			goto error;
	}

	/* entering event loop and waiting for termination of processes
	 * by calling xp_commands_wait.
	 * FixME: we are blocked here so both success and faulure cases
	 * fall back to the error handler and all resources are freed.
	 * this should be changed when we have non-blocking command_wait()  */
	if (xp_commands_wait(num_apps, xcmd_sets) < 0) {
		rc = ORTE_ERROR;
	} else {
		rc = ORTE_SUCCESS;
	}

error:
	/* error handling and clean up, kill all the processes */
	for (i = 0; i < num_apps; i++) {
		if (xcmd_sets[i] != NULL) {
		    xp_command_wipe(xcmd_sets[i]);
		    xp_command_destroy(xcmd_sets[i]);
		    xcmd_sets[i] = NULL;
		}
	}

	/* set ORTE error code?? */
	return rc;
}

int orte_pls_xcpu_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs)
{
	int i;

	for (i = 0; i < num_apps; i++) {
		if (xcmd_sets[i] != NULL) {
			xp_command_kill(xcmd_sets[i], SIGTERM);
		}
	}
	return ORTE_SUCCESS;
}

int orte_pls_xcpu_terminate_orteds(struct timeval *timeout, opal_list_t * attrs)
{
	return ORTE_SUCCESS;
}

int orte_pls_xcpu_terminate_proc(const orte_process_name_t* proc_name)
{
	fprintf(stderr, __FILE__ " terminate_proc\n");

	/* libxcpu can not kill individual process in an
	 * Xpcommand/Xpsessionset, only to the whole session set */

	return ORTE_SUCCESS;
}

int orte_pls_xcpu_signal_job(orte_jobid_t jobid, int32_t sig, opal_list_t *attrs)
{
	int i;

	fprintf(stderr, __FILE__ " signal_job, sig = %d\n", sig);

	for (i = 0; i < num_apps; i++) {
		if (xcmd_sets[i] != NULL)
			xp_command_kill(xcmd_sets[i], sig);
	}

	return ORTE_SUCCESS;
}
int orte_pls_xcpu_signal_proc(const orte_process_name_t* proc_name, int32_t sig)
{
	fprintf(stderr, __FILE__ " terminate_proc\n");

	/* libxcpu can not send signal to individual process in an
	 * Xpcommand/Xpsessionset, only to the whole session set */

	return ORTE_SUCCESS;
}

/**
 * Cancel an operation involving comm to an orted
 */
int orte_pls_xcpu_cancel_operation(void)
{
    return ORTE_SUCCESS;
}

int orte_pls_xcpu_finalize(void)
{
	return ORTE_SUCCESS;
}

