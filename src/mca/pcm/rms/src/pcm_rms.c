/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>

#include "pcm_rms.h"
#include "mca/pcm/base/base_job_track.h"
#include "include/constants.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "runtime/runtime.h"
#include "event/event.h"
#include "class/ompi_list.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"
#include "util/argv.h"
#include "util/numtostr.h"
#include "runtime/ompi_rte_wait.h"
#include "util/show_help.h"
#include "util/output.h"


static void internal_wait_cb(pid_t pid, int status, void *data);


/* ok, this is fairly simple in the RMS world */
ompi_list_t *
mca_pcm_rms_allocate_resources(struct mca_pcm_base_module_1_0_0_t* me,
                               mca_ns_base_jobid_t jobid,
                               int nodes, int procs)
{
    ompi_list_t *ret;
    ompi_rte_node_allocation_t *node_alloc;
    int total_procs;

    ret = OBJ_NEW(ompi_list_t);
    if (NULL == ret) {
        errno = ENOMEM;
        return NULL;
    }

    node_alloc = OBJ_NEW(ompi_rte_node_allocation_t);
    if (NULL == node_alloc) {
        OBJ_RELEASE(ret);
        errno = ENOMEM;
        return NULL;
    }

    /* For now, just punt on whether we can actually fullfill the request or not */
    total_procs = (nodes == 0) ? procs : nodes * procs;
    node_alloc->start = 
            (int) ompi_name_server.reserve_range(jobid, total_procs);
    node_alloc->nodes = nodes;
    node_alloc->count = procs;

    ompi_list_append(ret, (ompi_list_item_t*) node_alloc);

    return ret;
}


int
mca_pcm_rms_spawn_procs(struct mca_pcm_base_module_1_0_0_t* me_super,
                        mca_ns_base_jobid_t jobid, ompi_list_t *schedlist)
{
    mca_pcm_rms_module_t *me = (mca_pcm_rms_module_t*) me_super;
    ompi_rte_node_allocation_t *nodes;
    ompi_rte_node_schedule_t *sched;
    char **argv = NULL;
    int argc = 0;
    char *num;
    int i;
    int ret;
    char *tmp;
    pid_t child;
    char **prun_args;
    char *printable;

    /* quick sanity check */
    if (ompi_list_get_size(schedlist) > 1) {
        ompi_show_help("help-mca-pcm-rms.txt", "spawn:multiple-apps", true);
        return OMPI_ERROR;
    }
    sched = (ompi_rte_node_schedule_t*) ompi_list_get_first(schedlist);
    if (ompi_list_get_size(sched->nodelist) > 1) {
        ompi_show_help("help-mca-pcm-rms.txt", "spawn:multiple-nodelists", true);
        return OMPI_ERROR;
    }

    nodes = (ompi_rte_node_allocation_t*) ompi_list_get_first(sched->nodelist);

    /* start building up the argv array */
    ompi_argv_append(&argc, &argv, "prun");
    if (nodes->nodes > 0) {
        /* copy over the number of nodes */
        num = ompi_ltostr(nodes->nodes);
        ompi_argv_append(&argc, &argv, "-N");
        ompi_argv_append(&argc, &argv, num);
        free(num);
        /* and map to fix how they look at their num procs */
        num = ompi_ltostr(nodes->nodes * nodes->count);
        ompi_argv_append(&argc, &argv, "-n");
        ompi_argv_append(&argc, &argv, num);
        free(num);
    } else {
        num = ompi_ltostr(nodes->count);
        ompi_argv_append(&argc, &argv, "-n");
        ompi_argv_append(&argc, &argv, num);
        free(num);
    }

    if (NULL != me->partition) {
        ompi_argv_append(&argc, &argv, "-p");
        ompi_argv_append(&argc, &argv, me->partition); 
    }

    if (NULL != me->prun_args) {
        prun_args = ompi_argv_split(me->prun_args, ' ');
        if (NULL != prun_args) {
            for (i = 0 ; prun_args[i] != NULL ; ++i) {
                ompi_argv_append(&argc, &argv, prun_args[i]);
            }
            ompi_argv_free(prun_args);
        }
    }

    /* copy over the command line arguments */
    for (i = 0 ; i < sched->argc ; ++i) {
        ompi_argv_append(&argc, &argv, (sched->argv)[i]);
    }

    printable = ompi_argv_join(argv, ' ');
    ompi_output_verbose(5, mca_pcm_base_output,
                        "attempting to execute: %s", printable);
    free(printable);

    /* ok, fork! */
    child = fork();
    if (child < 0) {
        ompi_show_help("help-mca-pcm-rms.txt", "spawn:fork-failure", true,
                       strerror(errno));
        return OMPI_ERR_OUT_OF_RESOURCE;
    } else if (0 == child) {
        /* set up environment */
        /* these pointers will last until we exec, so safe to putenv them in the child */
        for (i = 0 ; sched->env[i] != NULL ; ++i) {
            putenv(sched->env[i]);
        }

        /* give our starting vpid count to the other side... */
        asprintf(&tmp, "OMPI_MCA_pcmclient_rms_start_vpid=%d\n", 
                 nodes->start);
        putenv(tmp);

        asprintf(&tmp, "OMPI_MCA_pcmclient_rms_jobid=%d\n", jobid);
        putenv(tmp);

        /* set cwd */
        ret = chdir(sched->cwd);
        if (0 != ret) {
            ompi_show_help("help-mca-pcm-rms.txt", "spawn:chdir", true,
                           sched->cwd, strerror(errno));
            exit(1);
        }

        /* go, go, go! */
        ret = execvp(argv[0], argv);
        ompi_show_help("help-mca-pcm-rms.txt", "spawn:exec-prun", true,
                       argv[0], strerror(errno));
        exit(1);
    } 

    /* ok, I'm the parent - stick the pids where they belong */
    ret = mca_pcm_base_job_list_add_job_info(me->jobs,
                                             jobid, child, nodes->start,
                                             nodes->start + (nodes->nodes == 0) ? 
                                             nodes->count : 
                                             nodes->nodes * nodes->count);
    if (OMPI_SUCCESS != ret) {
        kill(child, SIGKILL);
        return ret;
    }
    ret = ompi_rte_wait_cb(child, internal_wait_cb, NULL);
    if (OMPI_SUCCESS != ret) {
      kill(child, SIGKILL);
      return ret;
    }

    return OMPI_SUCCESS;
}


int
mca_pcm_rms_kill_proc(struct mca_pcm_base_module_1_0_0_t* me_super,
                      ompi_process_name_t *name, int flags)
{
    mca_pcm_rms_module_t *me = (mca_pcm_rms_module_t*) me_super;
    pid_t doomed;

    doomed = mca_pcm_base_job_list_get_starter(me->jobs, 
                                               mca_ns_base_get_jobid(name), 
                                               mca_ns_base_get_vpid(name),
					       true);
    if (doomed > 0) {
        kill(doomed, SIGTERM);
    } else {
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


int
mca_pcm_rms_kill_job(struct mca_pcm_base_module_1_0_0_t* me_super,
                     mca_ns_base_jobid_t jobid, int flags)
{
    mca_pcm_rms_module_t *me = (mca_pcm_rms_module_t*) me_super;
    pid_t *doomed;
    size_t doomed_len, i;
    int ret;

    ret = mca_pcm_base_job_list_get_starters(me->jobs,
                                             jobid, &doomed, &doomed_len, 
                                             true);
    if (OMPI_SUCCESS != ret) return ret;

    for (i = 0 ; i < doomed_len ; ++i) {
        kill(doomed[i], SIGTERM);
    }

    if (NULL != doomed) {
        free(doomed);
    }

    return OMPI_SUCCESS;
}


int
mca_pcm_rms_deallocate_resources(struct mca_pcm_base_module_1_0_0_t* me,
                                 mca_ns_base_jobid_t jobid,
                                 ompi_list_t *nodelist)
{
    if (nodelist != NULL) OBJ_RELEASE(nodelist);

    /* bwb - fix me */

    return OMPI_SUCCESS;
}



static void
internal_wait_cb(pid_t pid, int status, void *data)
{
    mca_pcm_rms_module_t *me = (mca_pcm_rms_module_t*) data;
    mca_ns_base_jobid_t jobid = 0;
    mca_ns_base_vpid_t upper = 0;
    mca_ns_base_vpid_t lower = 0;
    mca_ns_base_vpid_t i = 0;
    int ret;
    ompi_process_name_t *proc_name;
    ompi_rte_process_status_t proc_status;

    ompi_output_verbose(10, mca_pcm_base_output, 
                        "process %d exited with status %d", pid, status);

    ret = mca_pcm_base_job_list_get_job_info(me->jobs, pid, &jobid, 
                                             &lower, &upper, true);
    if (ret != OMPI_SUCCESS) {
        ompi_show_help("help-mca-pcm-rms.txt",
                       "spawn:no-process-record", true, pid, status);
        return;
    }

    /* unregister all the procs */
    proc_status.status_key = OMPI_PROC_KILLED;
    proc_status.exit_code = (ompi_exit_code_t) status;
    for (i = lower ; i <= upper ; ++i) {
	proc_name = mca_ns_base_create_process_name(0, jobid, i);
	ompi_rte_set_process_status(&proc_status, proc_name);
        free(proc_name);
    }
}
