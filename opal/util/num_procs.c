/*
 * Copyright (c) 2007 The University of Tennessee and The University
 *                    of Tennessee Research Foundation.  All rights
 *                    reserved.
 * Copyright (c) 2007 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <unistd.h>

#include "opal/constants.h"
#include "opal/util/num_procs.h"
#if defined(HAVE_SYS_SYSCTL_H)
#include <sys/sysctl.h>
#endif


/*
 * Simple wrapper to get the number of processors on the local host
 */
int opal_get_num_processors(int *num_procs)
{
#ifdef __WINDOWS__
    /* Need to get the Right code for Windows... */
    return OPAL_ERR_NOT_IMPLEMENTED;
#else
    /* POSIX environments */

#if defined(__APPLE__)
    /* OSX has a special function for this */
    size_t size = sizeof(*num_procs) ;

    if (0 == sysctlbyname( "hw.ncpu", num_procs, &size, NULL, 0)) {
        return OPAL_SUCCESS;
    }
#elif OPAL_HAVE__SC_NPROCESSORS_ONLN
    /* Other POSIX'es can use sysconf() */
    int count = sysconf(_SC_NPROCESSORS_ONLN);
    if (-1 != count) {
        *num_procs = count;
        return OPAL_SUCCESS;
    }
#endif
    /* Default case if we don't know how to get the processor number or if
       something fails. */
    return OPAL_ERR_NOT_FOUND;
#endif
}
