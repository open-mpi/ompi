/* -*- C++ -*-
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "runtime/runtime.h"
#include "mca/base/base.h"
#include "util/cmd_line.h"
#include "include/constants.h"

#include <stdio.h>

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

    /*
     * Intialize our Open MPI environment
     */
    cmd_line = ompi_cmd_line_create();

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
    ompi_cmd_line_make_opt(cmd_line, '\0', "np", 1,
                           "Number of processes to start");
    ompi_cmd_line_make_opt(cmd_line, 'h', "hostfile", 1,
                           "Host description file");

    if (OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, false, argc, argv) ||
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


    /*
     * Start the Open MPI Run Time Environment
     */
    if (OMPI_SUCCESS != (ret = mca_base_open())) {
        /* JMS show_help */
        printf("show_help: mca_base_open failed\n");
        return ret;
    }

    if (OMPI_SUCCESS != ompi_rte_init(&multi_thread, &hidden_thread)) {
        /* BWB show_help */
        printf("show_help: ompi_rte_init failed\n");
        return ret;
    }


    /*
     *  Prep for starting a new job
     */
    /*
     * BWB: todo:
     *
     *   - ompi_rte_get_new_jobid()
     */

    /* BWB - fix jobid, procs, and nodes */
    if (OMPI_SUCCESS != ompi_rte_allocate_resources(0, 0, 2, &nodelist)) {
        /* BWB show_help */
        printf("show_help: ompi_rte_allocate_resources failed\n");
        return -1;
    }

    /*
     * BWB: todo:
     *
     *   MPI process mapping
     *   - ompi_rte_register_monitor()
     *   - ompi_rte_spawn()
     *   - ompi_rte_monitor()
     *   - ompi_rte_kill_job()
     */


    /*
     * Clean up
     */
    if (NULL != nodelist) ompi_rte_deallocate_resources(0, nodelist);
    if (NULL != cmd_line) ompi_cmd_line_free(cmd_line);
    ompi_rte_finalize();
    mca_base_close();
    ompi_finalize();

    return 0;
}
