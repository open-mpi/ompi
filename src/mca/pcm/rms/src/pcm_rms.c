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
#include "include/constants.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "event/event.h"
#include "class/ompi_list.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"
#include "util/argv.h"
#include "util/numtostr.h"



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
    if (mca_pcm_rms_use_ns) {
        node_alloc->start = 
            (int) ompi_name_server.reserve_range(jobid, total_procs);
    } else {
        /* BWB - remove the USE_NS code once the failures in PTL / NS
           due to unexpected offsets are fixed up */
        node_alloc->start = 0;
    }
    node_alloc->nodes = nodes;
    node_alloc->count = procs;

    ompi_list_append(ret, (ompi_list_item_t*) node_alloc);

    return ret;
}


bool
mca_pcm_rms_can_spawn(struct mca_pcm_base_module_1_0_0_t* me)
{
    /* it looks like a prun'd job can call prun again...  let's see
       what happens for now.. */
    return true;
}


int
mca_pcm_rms_spawn_procs(struct mca_pcm_base_module_1_0_0_t* me,
                        mca_ns_base_jobid_t jobid, ompi_list_t *schedlist)
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
    ret = mca_pcm_rms_add_started_pids(jobid, child, nodes->start,
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
mca_pcm_rms_kill_proc(struct mca_pcm_base_module_1_0_0_t* me,
                      ompi_process_name_t *name, int flags)
{
    pid_t doomed;

    doomed = mca_pcm_rms_get_started_pid(ns_base_get_jobid(name), 
                                         ns_base_get_vpid(name), true);
    if (doomed > 0) {
        kill(doomed, SIGTERM);
    } else {
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


int
mca_pcm_rms_kill_job(struct mca_pcm_base_module_1_0_0_t* me,
                     mca_ns_base_jobid_t jobid, int flags)
{
    pid_t *doomed;
    size_t doomed_len;
    int ret, i;

    ret = mca_pcm_rms_get_started_pid_list(jobid, &doomed, &doomed_len, true);
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

    mca_pcm_rms_remove_job(jobid);

    return OMPI_SUCCESS;
}
