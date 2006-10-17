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

#include "orte_config.h"

#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifndef _ORTE_SYS_INFO_H_
#define _ORTE_SYS_INFO_H_

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * System information structure
 *
 * The orte_sys_info() function fills the sysname, nodename, release, version, machine,
 * and user fields
 *
 */
struct orte_sys_info_t {
    bool init;             /**< Certifies that values have been filled.
			    * Certifies that the orte_sys_info() function has been
			    * called at least once so fields have valid values
			    */
    char *sysname;         /**< Name of OS in use on this node. */
    char *nodename;        /**< Fully qualified host name on the network. */
    char *release;	   /**< Release level of the operating system. */
    char *version;	   /**< Version of the operating system release. */
    char *machine;	   /**< Type of hardware composing this node. */
    char *user;            /**< User id on this system. */
    char *suffix;          /**< Automatic suffix added to file names.
			    * Some computing environments automatically "tag" files
			    * created by applications with a computer-generated suffix
			    * to ensure uniqueness of the file name. This field records
			    * that value for future use.
			    */
};
typedef struct orte_sys_info_t orte_sys_info_t;

ORTE_DECLSPEC extern orte_sys_info_t orte_system_info;


/**
 * \internal
 *
 * Discover and record a wide range of information about the system
 * upon which this code is executing. orte_sys_info populates a global
 * variable with information about the system upon which the process
 * is executing.
 *
 * Called from \c orte_init.
 *
 * @retval ORTE_SUCCESS If values are successfully determined.
 * @retval ORTE_ERROR If the system does not provide the requested information.
 */
ORTE_DECLSPEC int orte_sys_info(void);

/*
 * \internal
 * 
 * Free any memory held in the system_info structure
 * 
 * Called from \c orte_finalize
 * 
 * @retval ORTE_SUCCESS If all values successfully released
 * @retval ORTE_ERROR If any problems occur
 */
ORTE_DECLSPEC int orte_sys_info_finalize(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
