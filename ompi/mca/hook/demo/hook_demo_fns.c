/*
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "hook_demo.h"

#define DEBUG_OUTPUT( fn_name )                     \
    do {                                            \
        opal_output(0, "hook:demo: %s", __func__ ); \
    } while(0)

void ompi_hook_demo_mpi_initialized_top(int *flag) {
    DEBUG_OUTPUT();
}

void ompi_hook_demo_mpi_initialized_bottom(int *flag) {
    DEBUG_OUTPUT();
}


void ompi_hook_demo_mpi_finalized_top(int *flag) {
    DEBUG_OUTPUT();
}

void ompi_hook_demo_mpi_finalized_bottom(int *flag) {
    DEBUG_OUTPUT();
}


void ompi_hook_demo_mpi_init_top(int argc, char **argv, int requested, int *provided) {
    DEBUG_OUTPUT();
}

void ompi_hook_demo_mpi_init_top_post_opal(int argc, char **argv, int requested, int *provided) {
    DEBUG_OUTPUT();
}

void ompi_hook_demo_mpi_init_bottom(int argc, char **argv, int requested, int *provided) {
    DEBUG_OUTPUT();
}

void ompi_hook_demo_mpi_init_error(int argc, char **argv, int requested, int *provided) {
    DEBUG_OUTPUT();
}


void ompi_hook_demo_mpi_finalize_top(void) {
    DEBUG_OUTPUT();
}

void ompi_hook_demo_mpi_finalize_bottom(void) {
    DEBUG_OUTPUT();
}


void ompi_hook_demo_extra_mpi_init_bottom(int argc, char **argv, int requested, int *provided) {
    DEBUG_OUTPUT();
}
