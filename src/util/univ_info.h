/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** @file:
 *
 * Populates global structure with system-specific information.
 *
 * Notes: add limits.h, compute size of integer and other types via sizeof(type)*CHAR_BIT
 *
 */

#include "orte_config.h"

#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifndef _ORTE_UNIV_INFO_H_
#define _ORTE_UNIV_INFO_H_

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /* Define the info structure underlying the Open MPI universe system
    * instanced in ompi_rte_init.c */

    struct orte_universe_t {
        char *path;
        char *name;
        char *host;
        char *uid;
        bool persistence;
        char *scope;
        bool console;
        char *seed_uri;             /**< OOB contact info for universe seed */
        bool console_connected;     /**< Indicates if console is connected */
        char *scriptfile;           /**< Name of file containing commands to be executed */
    };
    typedef struct orte_universe_t orte_universe_t;

OMPI_DECLSPEC extern orte_universe_t orte_universe_info;


/**
 * \internal
 *
 * Discover the universe info from the environment
 *
 * Called from \c orte_init.
 *
 * @retval ORTE_SUCCESS If values are successfully determined.
 * @retval ORTE_ERROR If the system does not provide the requested information.
 */
OMPI_DECLSPEC int orte_univ_info(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
