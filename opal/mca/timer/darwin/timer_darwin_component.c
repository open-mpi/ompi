/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/mca/timer/timer.h"
#include "opal/mca/timer/darwin/timer_darwin.h"
#include "opal/constants.h"

opal_timer_t opal_timer_darwin_freq = {0};
mach_timebase_info_data_t opal_timer_darwin_info = {.denom = 0};
opal_timer_t opal_timer_darwin_bias = {0};

static int opal_timer_darwin_open(void);

const opal_timer_base_component_2_0_0_t mca_timer_darwin_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    .timerc_version = {
        OPAL_TIMER_BASE_VERSION_2_0_0,

        /* Component name and version */
        .mca_component_name = "darwin",
        MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                              OPAL_RELEASE_VERSION),

        .mca_open_component = opal_timer_darwin_open,
    },
    .timerc_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};

/* mach_timebase_info() returns a fraction that can be multiplied
   by the difference between two calls to mach_absolute_time() to
   get the number of nanoseconds that passed between the two
   calls.

   On PPC, mach_timebase_info returns numer = 1000000000 and denom
   = 33333335 (or possibly 25000000, depending on the machine).
   mach_absolute_time() returns a cycle count from the global
   clock, which runs at 25 - 33MHz, so dividing the cycle count by
   the frequency gives you seconds between the interval, then
   multiplying by 1000000000 gives you nanoseconds.  Of course,
   you should do the multiply first, then the divide to reduce
   arithmetic errors due to integer math.  But since we want the
   least amount of math in the critical path as possible and
   mach_absolute_time is already a cycle counter, we claim we have
   native cycle count support and set the frequencey to be the
   frequencey of the global clock, which is sTBI.denom *
   (1000000000 / sTBI.numer), which is sTBI.denom * (1 / 1), or
   sTBI.denom.

   On Intel, mach_timebase_info returns numer = 1 nd denom = 1,
   meaning that mach_absolute_time() returns some global clock
   time in nanoseconds.  Because PPC returns a frequency and
   returning a time in microseconds would still require math in
   the critical path (a divide, at that), we pretend that the
   nanosecond timer is instead a cycle counter for a 1GHz clock
   and that we're returning a cycle count natively.  so sTBI.denom
   * (1000000000 / sTBI.numer) gives us 1 * (1000000000 / 1), or
   1000000000, meaning we have a 1GHz clock.

   More generally, since mach_timebase_info() gives the "keys" to
   transition the return from mach_absolute_time() into
   nanoseconds, taking the reverse of that and multipling by
   1000000000 will give you a frequency in cycles / second if you
   think of mach_absolute_time() always returning a cycle count.
*/
int opal_timer_darwin_open(void)
{
    /* Call the opal_timer_base_get_cycles once to start the enging */
    (void)opal_timer_base_get_cycles();

    opal_timer_darwin_freq = opal_timer_darwin_info.denom * (1000000000 / opal_timer_darwin_info.numer);

    return OPAL_SUCCESS;
}
