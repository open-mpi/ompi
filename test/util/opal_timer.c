/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <unistd.h>

#include "opal/runtime/opal.h"
#include "opal/mca/timer/base/base.h"

int
main(int argc, char *argv[])
{
    opal_timer_t start, end, diff;

    opal_init(&argc, &argv);

    printf("--> frequency: %llu\n", (unsigned long long) opal_timer_base_get_freq());

#if OPAL_TIMER_CYCLE_SUPPORTED
    printf("--> cycle count\n");
    start = opal_timer_base_get_cycles();
    start = opal_timer_base_get_cycles();
    sleep(1);
    end = opal_timer_base_get_cycles();
    diff = end - start;
    printf("    Slept approximately %llu cycles, or %llu us\n",
           (unsigned long long) diff,
           (unsigned long long) ((diff * 1000000) / opal_timer_base_get_freq()));
#else
    printf("--> cycle count not supported\n");
#endif

#if OPAL_TIMER_USEC_SUPPORTED
    printf("--> usecs\n");
    start = opal_timer_base_get_usec();
    sleep(1);
    end = opal_timer_base_get_usec();
    diff = end - start;
    printf("    Slept approximately %llu us\n", (unsigned long long) diff);
#else
    printf("--> usec timer not supported\n");
#endif

    opal_finalize();

    return 0;
}
