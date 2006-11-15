/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

/** @file:
 *
 * Populates global structure with system-specific information.
 *
 * Notes: add limits.h, compute size of integer and other types via sizeof(type)*CHAR_BIT
 *
 */

#ifndef _ORTE_UNIV_INFO_H_
#define _ORTE_UNIV_INFO_H_

#include "orte_config.h"

#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "opal/class/opal_object.h"
#include "opal/class/opal_list.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    
/* Some states for the orte_universe_t.state field */
enum orte_universe_state_t {
    ORTE_UNIVERSE_STATE_PRE_INIT, /* Before initialization */
    ORTE_UNIVERSE_STATE_INIT,     /* In initalization      */
    ORTE_UNIVERSE_STATE_RUNNING,  /* After initalization   */
    ORTE_UNIVERSE_STATE_FINALIZE  /* In Finalization       */
};
typedef  enum orte_universe_state_t orte_universe_state_t;

/* Define the info structure underlying the Open MPI universe system
 * instanced in ompi_rte_init.c */

struct orte_universe_t {
    /** This is an object, so it must have a super */
    opal_list_item_t super;
    orte_universe_state_t state; /**< Indicates state of the universe */
    char *name;
    bool default_name;          /**< Indicates that universe name was not provided */
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

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_universe_t);

ORTE_DECLSPEC extern orte_universe_t orte_universe_info;


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
ORTE_DECLSPEC int orte_univ_info(void);

ORTE_DECLSPEC int orte_univ_info_finalize(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
