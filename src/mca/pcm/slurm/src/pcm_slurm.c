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

#include "pcm_slurm.h"
#include "mca/pcm/base/base_job_track.h"
#include "include/constants.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "event/event.h"
#include "class/ompi_list.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"
#include "util/argv.h"
#include "util/numtostr.h"
#include "runtime/ompi_rte_wait.h"


static void internal_wait_cb(pid_t pid, int status, void *data);


/* ok, this is fairly simple in the SLURM world */
ompi_list_t *
mca_pcm_slurm_allocate_resources(struct mca_pcm_base_module_1_0_0_t* me,
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
mca_pcm_slurm_spawn_procs(struct mca_pcm_base_module_1_0_0_t* me,
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
        printf("SLURM pcm can not cope with multiple schedlist items at this time\n");
        return OMPI_ERROR;
    }
    sched = (ompi_rte_node_schedule_t*) ompi_list_get_first(schedlist);
    if (ompi_list_get_size(sched->nodelist) > 1) {
        /* BWB: show_help */
        printf("SLURM pcm can not cope with multiple nodelists at this time\n");
        return OMPI_ERROR;
    }

    nodes = (ompi_rte_node_allocation_t*) ompi_list_get_first(sched->nodelist);

    /* start building up the argv array */
    ompi_argv_append(&argc, &argv, "srun");
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

    /* copy over the command line arguments */
    for (i = 0 ; i < sched->argc ; ++i) {
        ompi_argv_append(&argc, &argv, (sched->argv)[i]);
    }

    /*ok, fork! */
    child = fork();
    if (child < 0) {
        /* show_help */
        printf("SLURM pcm unable to fork\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    } else if (0 == child) {
        /* set up environment */
        /* these pointers will last until we exec, so safe to putenv them in the child */
        for (i = 0 ; sched->env[i] != NULL ; ++i) {
            putenv(sched->env[i]);
        }

        /* give our starting vpid count to the other side... */
        asprintf(&tmp, "OMPI_MCA_pcmclient_slurm_start_vpid=%d\n", 
                 nodes->start);
        putenv(tmp);

        asprintf(&tmp, "OMPI_MCA_pcmclient_slurm_jobid=%d\n", jobid);
        putenv(tmp);

        /* set cwd */
        ret = chdir(sched->cwd);
        if (0 != ret) {
            /* BWB show_help */
            printf("SLURM pcm can not chdir to %s\n", sched->cwd);
            exit(1);
        }

        /* go, go, go! */
        ret = execvp(argv[0], argv);
        /* BWB show_help */
        printf("SLURM pcm can not exec srun");
        exit(1);
    } 

    /* ok, I'm the parent - stick the pids where they belong */
    ret = mca_pcm_base_add_started_pids(jobid, child, nodes->start,
                                        nodes->start + (nodes->nodes == 0) ? 
                                        nodes->count : 
                                        nodes->nodes * nodes->count);
    if (OMPI_SUCCESS != ret) {
        /* BWB show_help */
        printf("show_help: unable to record child pid\n");
        kill(child, SIGKILL);
    }
    ret = ompi_rte_wait_cb(child, internal_wait_cb, NULL);
    if (OMPI_SUCCESS != ret) {
      /* BWB - show_help */
      printf("show_help: unable to register callback\n");
      kill(child, SIGKILL);
    }

    return OMPI_SUCCESS;
}


int
mca_pcm_slurm_kill_proc(struct mca_pcm_base_module_1_0_0_t* me,
                      ompi_process_name_t *name, int flags)
{
    pid_t doomed;

    doomed = mca_pcm_base_get_started_pid(ns_base_get_jobid(name), 
                                          ns_base_get_vpid(name), true);
    if (doomed > 0) {
        kill(doomed, SIGTERM);
    } else {
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


int
mca_pcm_slurm_kill_job(struct mca_pcm_base_module_1_0_0_t* me,
                     mca_ns_base_jobid_t jobid, int flags)
{
    pid_t *doomed;
    size_t doomed_len;
    int ret, i;

    ret = mca_pcm_base_get_started_pid_list(jobid, &doomed, &doomed_len, true);
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
mca_pcm_slurm_deallocate_resources(struct mca_pcm_base_module_1_0_0_t* me,
                                 mca_ns_base_jobid_t jobid,
                                 ompi_list_t *nodelist)
{
    if (nodelist != NULL) OBJ_RELEASE(nodelist);

    mca_pcm_base_remove_job(jobid);

    return OMPI_SUCCESS;
}



static void
internal_wait_cb(pid_t pid, int status, void *data)
{
    mca_ns_base_jobid_t jobid = 0;
    mca_ns_base_vpid_t upper = 0;
    mca_ns_base_vpid_t lower = 0;
    mca_ns_base_vpid_t i = 0;
    int ret;
    char *test;
    ompi_process_name_t *proc_name;

    printf("pcm_slurm was notified that process %d exited with status %d\n",
           pid, status);

    ret = mca_pcm_base_get_job_info(pid, &jobid, &lower, &upper);
    if (ret != OMPI_SUCCESS) {
        printf("Unfortunately, we could not find the associated job info\n");
    } else {
        printf("  It appears that this starter was assocated with jobid %d\n"
               "  vpids %d to %d\n\n",
               jobid, lower, upper);
    }

    /* unregister all the procs */
#if 0
    /* BWB - fix me when deadlock in gpr is fixed */
    for (i = lower ; i <= upper ; ++i) {
        test = ns_base_get_proc_name_string(ns_base_create_process_name(0, jobid, i));
        ompi_registry.rte_unregister(test);
    }
#endif
}
