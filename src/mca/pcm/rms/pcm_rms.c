/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include <errno.h>
#include <sys/types.h>
#include <unistd.h>

#include "include/constants.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "mca/pcm/rms/pcm_rms.h"
#include "event/event.h"
#include "class/ompi_list.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"
#include "util/argv.h"
#include "util/numtostr.h"

static mca_pcm_rms_job_item_t *
get_job_item(mca_ns_base_jobid_t jobid)
{
    ompi_list_item_t *item;

    for (item = ompi_list_get_first(&mca_pcm_rms_jobs) ;
         item != ompi_list_get_end(&mca_pcm_rms_jobs) ;
         item = ompi_list_get_next(item) ) {
        mca_pcm_rms_job_item_t  *job_item = (mca_pcm_rms_job_item_t*) item;
        if (job_item->jobid == jobid) return job_item;
    }

    return NULL;
}


static mca_pcm_rms_pids_t *
get_pids_entry(mca_pcm_rms_job_item_t *job_item, mca_ns_base_vpid_t vpid)
{
    ompi_list_item_t *item;
    for (item = ompi_list_get_first(job_item->pids) ;
         item != ompi_list_get_end(job_item->pids) ;
         item = ompi_list_get_next(item) ) {
        mca_pcm_rms_pids_t *pids = (mca_pcm_rms_pids_t*) item;
        if (pids->lower < vpid && pids->upper > vpid) {
            return pids;
        }
    }

    return NULL;
}



static int
add_started_pids(mca_ns_base_jobid_t jobid, pid_t child_pid,
                 mca_ns_base_vpid_t lower, mca_ns_base_vpid_t upper)
{
    mca_pcm_rms_job_item_t *job_item;
    mca_pcm_rms_pids_t *pids;

    job_item = get_job_item(jobid);
    if (NULL == job_item) {
        job_item = OBJ_NEW(mca_pcm_rms_job_item_t);
        if (NULL == job_item) return OMPI_ERROR;
        job_item->jobid = jobid;
    }

    pids = OBJ_NEW(mca_pcm_rms_pids_t);
    if (NULL == pids) return OMPI_ERROR;
    pids->lower = lower;
    pids->upper = upper;
    pids->child = child_pid;

    ompi_list_append(job_item->pids, (ompi_list_item_t*) pids);

    return OMPI_SUCCESS;
}


static pid_t
get_started_pid(mca_ns_base_jobid_t jobid, mca_ns_base_vpid_t vpid)
{
    mca_pcm_rms_job_item_t *job_item;
    mca_pcm_rms_pids_t *pids;

    job_item = get_job_item(jobid);
    if (NULL == job_item) return -1;

    pids = get_pids_entry(job_item, vpid);
    if (NULL == pids) return -1;

    return pids->child;
}


static int
remove_started_pid(pid_t pid)
{
    ompi_list_item_t *job_item, *pid_item;

    /* ugh, this is going to suck as an operation */
    for (job_item = ompi_list_get_first(&mca_pcm_rms_jobs) ;
         job_item != ompi_list_get_end(&mca_pcm_rms_jobs) ;
         job_item = ompi_list_get_next(job_item)) {
        mca_pcm_rms_job_item_t  *job = (mca_pcm_rms_job_item_t*) job_item;
        for (pid_item = ompi_list_get_first(job->pids) ;
             pid_item != ompi_list_get_end(job->pids) ;
             pid_item = ompi_list_get_next(pid_item) ) {
            mca_pcm_rms_pids_t *pid_ent = (mca_pcm_rms_pids_t*) pid_item;
            if (pid_ent->child == pid) {
                /* we have a winner! */
                ompi_list_remove_item(job->pids, pid_item);
                return OMPI_SUCCESS;
            }
        }
    }

    return OMPI_ERROR;
}


/* ok, this is fairly simple in the RMS world */
ompi_list_t *
mca_pcm_rms_allocate_resources(mca_ns_base_jobid_t jobid,
                               int nodes, int procs)
{
    ompi_list_t *ret;
    ompi_rte_node_allocation_t *node_alloc;
    int total_procs;

    ret = OBJ_NEW(ompi_list_t);
    if (NULL != ret) {
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
#if 0 /* BWB - fix me */
    node_alloc->start = (int) ns_base_reserve_range(jobid, total_procs);
#endif
    node_alloc->nodes = nodes;
    node_alloc->count = procs;

    ompi_list_append(ret, (ompi_list_item_t*) node_alloc);

    return ret;
}


bool
mca_pcm_rms_can_spawn(void)
{
    /* it looks like a prun'd job can call prun again...  let's see
       what happens for now.. */
    return true;
}


int
mca_pcm_rms_spawn_procs(mca_ns_base_jobid_t jobid, ompi_list_t *schedlist)
{
    ompi_rte_node_allocation_t *nodes;
    ompi_rte_node_schedule_t *sched;
    char **argv = NULL;
    int argc = 0;
    char *num;
    int i;
    int ret;
    char *tmp;
    pid_t child;

    /* quick sanity check */
    if (ompi_list_get_size(schedlist) > 1) {
        /* BWB: show_help */
        printf("RMS pcm can not cope with multiple schedlist items at this time\n");
        return OMPI_ERROR;
    }
    sched = (ompi_rte_node_schedule_t*) ompi_list_get_first(schedlist);
    if (ompi_list_get_size(sched->nodelist) > 1) {
        /* BWB: show_help */
        printf("RMS pcm can not cope with multiple nodelists at this time\n");
        return OMPI_ERROR;
    }

    nodes = (ompi_rte_node_allocation_t*) ompi_list_get_first(sched->nodelist);

    /* start building up the argv array */
    ompi_argv_append(&argc, &argv, "prun");
    if (nodes->nodes > 0) {
        /* copy over the number of nodes */
        num = ltostr(nodes->nodes);
        ompi_argv_append(&argc, &argv, "-N");
        ompi_argv_append(&argc, &argv, num);
        free(num);
        /* and map to fix how they look at their num procs */
        num = ltostr(nodes->nodes * nodes->count);
        ompi_argv_append(&argc, &argv, "-n");
        ompi_argv_append(&argc, &argv, num);
        free(num);
    } else {
        num = ltostr(nodes->count);
        ompi_argv_append(&argc, &argv, "-n");
        ompi_argv_append(&argc, &argv, num);
        free(num);
    }

    /* copy over the command line arguments */
    for (i = 0 ; i < sched->argc ; ++i) {
        ompi_argv_append(&argc, &argv, (sched->argv)[i]);
    }

    /*ok, fork! */
    child = fork();
    if (child < 0) {
        /* show_help */
        printf("RMS pcm unable to fork\n");
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
            /* BWB show_help */
            printf("RMS pcm can not chdir to %s\n", sched->cwd);
            exit(1);
        }

        /* go, go, go! */
        ret = execvp(argv[0], argv);
        exit(1);
    } 

    /* ok, I'm the parent - stick the pids where they belong */
    ret = add_started_pids(jobid, child, nodes->start,
                           nodes->start + (nodes->nodes == 0) ? 
                                          nodes->count : 
                                          nodes->nodes * nodes->count);
    if (OMPI_SUCCESS != ret) {
        /* BWB show_help */
        printf("show_help: unable to record child pid\n");
        kill(child, SIGKILL);
    }

    return OMPI_SUCCESS;
}


int
mca_pcm_rms_kill_proc(ompi_process_name_t *name, int flags)
{
    mca_pcm_rms_job_item_t *job = get_job_item(ns_base_get_jobid(name));
    pid_t doomed;

    doomed = get_started_pid(ns_base_get_jobid(name), ns_base_get_vpid(name));
    if (doomed > 0) {
        kill(doomed, SIGTERM);
        remove_started_pid(doomed);
    } else {
        return OMPI_ERROR;
    }

    if (0 == ompi_list_get_size((ompi_list_t*) job->pids)) {
        ompi_list_remove_item(&mca_pcm_rms_jobs, (ompi_list_item_t*) job);
    }

    return OMPI_SUCCESS;
}


int
mca_pcm_rms_kill_job(mca_ns_base_jobid_t jobid, int flags)
{
    mca_pcm_rms_job_item_t *job = get_job_item(jobid);
    ompi_list_item_t *item;

    if (job == NULL) return OMPI_ERROR;

    for (item = ompi_list_get_first(job->pids) ;
         item != ompi_list_get_end(job->pids) ;
         item = ompi_list_get_next(job->pids) ) {
        mca_pcm_rms_pids_t *pid = (mca_pcm_rms_pids_t*) item;
        if (pid->child > 0) kill(pid->child, SIGTERM);
        ompi_list_remove_item(job->pids, item);
    }

    ompi_list_remove_item(&mca_pcm_rms_jobs, (ompi_list_item_t*) job);
 
    return OMPI_SUCCESS;
}


int
mca_pcm_rms_deallocate_resources(mca_ns_base_jobid_t jobid,
                                 ompi_list_t *nodelist)
{
    mca_pcm_rms_job_item_t *job;

    if (nodelist != NULL) OBJ_RELEASE(nodelist);

    job = get_job_item(jobid);
    if (NULL != job) {
        ompi_list_remove_item(&mca_pcm_rms_jobs, (ompi_list_item_t*) job);
    }

    return OMPI_SUCCESS;
}
