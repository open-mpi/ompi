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

static pid_t child = -1;

/* ok, this is fairly simple in the RMS world */
ompi_list_t *
mca_pcm_rms_allocate_resources(int jobid,
                               int nodes, int procs)
{
    ompi_list_t *ret;
    ompi_rte_node_allocation_t *nodes;

    ret = OBJ_NEW(ompi_list_t);
    if (NULL != ret) {
        errno = ENOMEM;
        return NULL;
    }

    nodes = OBJ_NEW(ompi_rte_node_allocation_t);
    if (NULL == nodes) {
        OBJ_RELEASE(ret);
        errno = ENOMEM;
        return NULL;
    }

    /* For now, just punt on whether we can actually fullfill the request or not */
    nodes->start = 0;
    nodes->nodes = nodes;
    nodes->count = procs;

    ompi_list_appand(ret, (ompi_list_item_t*) nodes);

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
mca_pcm_rms_spawn_procs(int jobid, ompi_list_t *schedlist)
{
    ompi_rte_node_allocation_t *nodes;
    ompi_rte_node_schedule_t *sched;
    char **cmdv = NULL;
    int cmdc = 0;
    char *num, *env;
    int i;
    int ret;

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
        return OMPI_OMPI_ERR_OUT_OF_RESOURCE;
    } else if (0 == child) {
        /* set up environment */
        /* these pointers will last until we exec, so safe to putenv them in the child */
        for (i = 0 ; sched->env[i] != NULL ; ++i) {
            putenv(sched->env[i]);
        }

        /* set cwd */
        ret = chdir(sched->cwd);
        if (0 != ret) {
            /* BWB show_help */
            printf("RMS pcm can not chdir to %s\n", sched->cwd);
            exit(1);
        }

        /* go, go, go! */
        ret = execvp(argv[0], argv);
    } 

    return OMPI_SUCCESS;
}


int
mca_pcm_rms_kill_proc(ompi_process_name_t *name, int flags)
{
    if (child > 0) {
        kill(child, SIGTERM);
    }

    return OMPI_SUCCESS;
}


int
mca_pcm_rms_kill_job(int jobid, int flags)
{
    if (child > 0) {
        kill(child, SIGTERM);
    }

    return OMPI_SUCCESS;
}


int
mca_pcm_rms_deallocate_resources(int jobid,
                                     ompi_list_t *nodelist)
{
    if (nodelist != NULL) OBJ_RELEASE(nodelist);

    return OMPI_SUCCESS;
}
