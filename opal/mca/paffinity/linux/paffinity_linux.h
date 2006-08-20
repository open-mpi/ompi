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

/**
 * @file
 *
 * Processor affinity for Linux.
 *
 * Linux sucks.  There are at least 3 different ways that
 * sched_setaffinity is implemented (only one of which -- the most
 * outdated -- is documented in the sched_setaffinity(2) man page):
 *
 *-----------------------------------------------------------------
 * 1. int sched_setaffinity(pid_t pid, unsigned int len, unsigned
 * long *mask);
 *
 * This originated in 2.5 kernels (which we won't worry about) and
 * some distros back-ported it to their 2.4 kernels.  It's unknown if
 * this appears in any 2.6 kernels.
 *
 * 2. int sched_setaffinity (pid_t __pid, size_t __cpusetsize,
 * const cpu_set_t *__cpuset);
 *
 * This appears to be in recent 2.6 kernels (confirmed in Gentoo
 * 2.6.11).  I don't know when #1 changed into #2.  However, this
 * prototype is nice -- the cpu_set_t type is accompanied by
 * fdset-like CPU_ZERO(), CPU_SET(), CPU_ISSET(), etc. macros.
 *
 * 3. int sched_setaffinity (pid_t __pid, const cpu_set_t *__mask);
 *
 * (note the missing len parameter) This is in at least some Linux
 * distros (e.g., MDK 10.0 with a 2.6.3 kernel, and SGI Altix, even
 * though the Altix uses a 2.4-based kernel and therefore likely
 * back-ported the 2.5 work but modified it for their needs).  Similar
 * to #2, the cpu_set_t type is accompanied by fdset-like CPU_ZERO(),
 * CPU_SET(), CPU_ISSET(), etc. macros.
 *-----------------------------------------------------------------
 *
 * This component has to figure out which one to use.  :-\
 *
 * Also note that at least some distros of Linux have a broken
 * CPU_ZERO macro (a pair of typos in /usr/include/bits/sched.h).
 * MDK 9.2 is the screaming example, but it's pretty old and
 * probably only matters because one of the developers uses that as
 * a compilation machine :-) (it also appears to have been fixed in
 * MDK 10.0, but they also changed from #2 to #3 -- arrgh!).
 * However, there's no way of knowing where these typos came from
 * and if they exist elsewhere.  So it seems safest to extend this
 * configure script to check for a bad CPU_ZERO macro.  #$%#@%$@!!!
 */


#ifndef MCA_PAFFINITY_LINUX_EXPORT_H
#define MCA_PAFFINITY_LINUX_EXPORT_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/paffinity/paffinity.h"


/**
 * Determine whether we have a working CPU_ZERO() macro or not.  If
 * not, use memset().
 */
#ifdef HAVE_CPU_ZERO
#define OMPI_CPU_ZERO(foo) CPU_ZERO(foo)
#else
#include <string.h>
#define OMPI_CPU_ZERO(foo) memset(foo, 0, sizeof(*foo))
#endif


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Globally exported variable
     */
    OPAL_DECLSPEC extern const opal_paffinity_base_component_1_0_0_t
        mca_paffinity_linux_component;


    /**
     * paffinity query API function
     */
    const opal_paffinity_base_module_1_0_0_t *
        opal_paffinity_linux_component_query(int *query);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_PAFFINITY_LINUX_EXPORT_H */
