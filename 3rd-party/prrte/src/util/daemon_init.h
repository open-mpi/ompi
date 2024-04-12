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
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file **/

#ifndef PRTE_DAEMON_INIT_H
#define PRTE_DAEMON_INIT_H

#include "prte_config.h"

BEGIN_C_DECLS

/*
 * Turn a process into a daemon.
 *
 * This function converts a process into a daemon in an orderly manner. It first forks a child
 * process, then the parent exits. The child continues on to become a session leader, reset the file
 * mode creation mask, and changes working directories to the one specified.
 *
 * @param working_dir Pointer to a character string containing the desired working directory.
 * Providing a value of NULL will cause the function to leave the program in the current working
 * directory.
 * @param parent_fn The function to execute in the parent before exiting
 * a value of NULL will cause the parent to simply exit(0).
 *
 * @retval PRTE_SUCCESS Indicates that the conversion was successful
 * @retval PRTE_ERROR Indicates that the conversion was not successful - a fork could not be
 * completed.
 */
PRTE_EXPORT int prte_daemon_init_callback(char *working_dir, int (*parent_fn)(pid_t child));

END_C_DECLS

static inline int prte_daemon_init(char *working_dir)
{
    return prte_daemon_init_callback(working_dir, NULL);
}

END_C_DECLS

#endif /* PRTE_DAEMON_INIT_H */
