/* -*- C++ -*-
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "util/proc_info.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"
#include "mca/pcm/base/base.h"
#include "runtime/runtime.h"
#include "mca/base/base.h"
#include "util/cmd_line.h"
#include "include/constants.h"

#include <stdio.h>
#include <unistd.h>
#include <sys/param.h>

extern char** environ;

static long num_running_procs;

static int
mpirun_monitor(ompi_process_name_t *name, int newstate, int status)
{
    /* BWB - do state checks and the like... */
    num_running_procs--;
    return OMPI_SUCCESS;
}


int
main(int argc, char *argv[])
{
    bool multi_thread = false;
    bool hidden_thread = false;
    int ret;
    ompi_cmd_line_t *cmd_line = NULL;
    ompi_list_t *nodelist = NULL;
    ompi_list_t schedlist;
    mca_ns_base_jobid_t new_jobid;
    int num_procs = 1;
    ompi_rte_node_schedule_t *sched;
    char cwd[MAXPATHLEN];

    /*
     * Intialize our Open MPI environment
     */
    cmd_line = OBJ_NEW(ompi_cmd_line_t);

    if (OMPI_SUCCESS != ompi_init(argc, argv)) {
        /* BWB show_help */
        printf("show_help: ompi_init failed\n");
        return ret;
    }

    /*
     * Start command line arguments
     */
    if (OMPI_SUCCESS != (ret = mca_base_cmd_line_setup(cmd_line))) {
        /* BWB show_help */
        printf("show_help: mca_base_cmd_line_setup failed\n");
        return ret;
    }

    ompi_cmd_line_make_opt(cmd_line, 'h', "help", 0, 
                           "Show this help message");
    ompi_cmd_line_make_opt3(cmd_line, 'n', "np", "np", 1,
                            "Number of processes to start");
    ompi_cmd_line_make_opt3(cmd_line, '\0', "hostfile", "hostfile", 1,
                           "Host description file");

    if (OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, true, argc, argv) ||
        ompi_cmd_line_is_taken(cmd_line, "help") || 
        ompi_cmd_line_is_taken(cmd_line, "h")) {
        printf("...showing ompi_info help message...\n");
        exit(1);
    }

    if (OMPI_SUCCESS != mca_base_cmd_line_process_args(cmd_line)) {
        /* BWB show_help */
        printf("show_help: mca_base_cmd_line_process_args\n");
        return ret;
    }

    /* get our numprocs */
    if (ompi_cmd_line_is_taken(cmd_line, "np")) {
        num_procs = atoi(ompi_cmd_line_get_param(cmd_line, "np", 0, 0));
        printf("num_procs: %d\n", num_procs);
    }

    /*
     * TSW - temporarily force to be a seed - and to use tcp oob.
     * 
     */
    ompi_process_info.seed = true;
    setenv("OMPI_MCA_oob_base_include", "tcp", 1);

    /*
     * Start the Open MPI Run Time Environment
     */
    if (OMPI_SUCCESS != (ret = mca_base_open())) {
        /* JMS show_help */
        printf("show_help: mca_base_open failed\n");
        return ret;
    }

    if (OMPI_SUCCESS != ompi_rte_init_stage1(&multi_thread, &hidden_thread) ||
	OMPI_SUCCESS != ompi_rte_init_stage2(&multi_thread, &hidden_thread)) {
        /* BWB show_help */
        printf("show_help: ompi_rte_init failed\n");
        return ret;
    }

    /*
     *  Prep for starting a new job
     */

    /* BWB - ompi_rte_get_new_jobid() */
    new_jobid = ompi_name_server.create_jobid();

    /* BWB - fix jobid, procs, and nodes */
    nodelist = ompi_rte_allocate_resources(new_jobid, 0, num_procs);
    if (NULL == nodelist) {
        /* BWB show_help */
        printf("show_help: ompi_rte_allocate_resources failed\n");
        return -1;
    }

    /*
     * Process mapping
     */
    OBJ_CONSTRUCT(&schedlist,  ompi_list_t);
    sched = OBJ_NEW(ompi_rte_node_schedule_t);
    ompi_list_append(&schedlist, (ompi_list_item_t*) sched);
    ompi_cmd_line_get_tail(cmd_line, &(sched->argc), &(sched->argv));
    mca_pcm_base_build_base_env(environ, &(sched->env));
    getcwd(cwd, MAXPATHLEN);
    sched->cwd = strdup(cwd);
    sched->nodelist = nodelist;

    if (sched->argc == 0) {
        printf("no app to start\n");
        return 1;
    }


    /*
     * register the monitor
     */

    ompi_rte_notify(new_jobid,num_procs);

    /*
     * spawn procs
     */
    if (OMPI_SUCCESS != ompi_rte_spawn_procs(new_jobid, &schedlist)) {
        printf("show_help: woops!  we didn't spawn :( \n");
        return -1;
    }

    /*
     * 
     */
   
    ompi_rte_monitor();

    /*
     *   - ompi_rte_kill_job()
     */

    /*
     * Clean up
     */
    if (NULL != nodelist) ompi_rte_deallocate_resources(new_jobid, nodelist);
    if (NULL != cmd_line) OBJ_RELEASE(cmd_line);
    ompi_rte_finalize();
    mca_base_close();
    ompi_finalize();

    OBJ_DESTRUCT(&schedlist);
    return 0;
}

