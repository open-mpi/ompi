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


int
main(int argc, char *argv[])
{
    bool multi_thread = false;
    bool hidden_thread = false;
    int ret;
    ompi_cmd_line_t *cmd_line = NULL;

    /*
     * Intialize our environment
     */
    cmd_line = ompi_cmd_line_create();

    if (OMPI_SUCCESS != ompi_init(argc, argv)) {
        /* BWB show_help */
        printf("show_help: ompi_init failed\n");
        return ret;
    }

    if (OMPI_SUCCESS != (ret = mca_base_cmd_line_setup(cmd_line))) {
        /* BWB show_help */
        printf("show_help: mca_base_cmd_line_setup failed\n");
        return ret;
    }
    
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
     * Clean up
     */
    ompi_rte_finalize();
    mca_base_close();
    ompi_finalize();

    return 0;
}
