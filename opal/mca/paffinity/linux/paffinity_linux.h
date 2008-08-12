/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 *
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
 * sched_setaffinity is implemented.
 *
 * Fortunately we have an independent project called Portable Linux
 * Processor Affinity (PLPA) which allows us to do processor affinity
 * without knowing which flavor of afffinity is installed on the
 * system a priori - PLPA does a few probes behind the scenes and
 * utilizes the correct syntax to the correct system call to set
 * or get processor affinity for us.
 *
 */


#ifndef MCA_PAFFINITY_LINUX_EXPORT_H
#define MCA_PAFFINITY_LINUX_EXPORT_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/linux/plpa/src/libplpa/plpa.h"


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Globally exported variable
     */
    OPAL_DECLSPEC extern const opal_paffinity_base_component_2_0_0_t
        mca_paffinity_linux_component;


    /**
     * paffinity query API function
     *
     * Query function for paffinity components.  Simply returns a priority
     * to rank it against other available paffinity components (assumedly,
     * only one component will be available per platform, but it's
     * possible that there could be more than one available).
     */
    int opal_paffinity_linux_component_query(mca_base_module_t **module, int *priority);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_PAFFINITY_LINUX_EXPORT_H */
