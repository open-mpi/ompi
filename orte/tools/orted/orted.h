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

#ifndef ORTED_H
#define ORTED_H

#include "orte_config.h"
#include "orte/orte_types.h"

#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

typedef struct {
    bool help;
    bool no_daemonize;
    bool debug;
    bool debug_daemons;
    bool debug_daemons_file;
    bool set_sid;
    char* ns_nds;
    char* name;
    char* vpid_start;
    char* num_procs;
    char* universe;
    char **saved_environ;
    int bootproxy;
    int uri_pipe;
    opal_mutex_t mutex;
    opal_condition_t condition;
    bool exit_condition;
    bool spin;
    int mpi_call_yield;
    int reap;
} orted_globals_t;

    
/*
 * Version-related strings and functions
 */

/* extern const char *ver_full; */
/* extern const char *ver_major; */
/* extern const char *ver_minor; */
/* extern const char *ver_release; */
/* extern const char *ver_alpha; */
/* extern const char *ver_beta; */
/* extern const char *ver_svn; */

/* void do_version(bool want_all, opal_cmd_line_t *cmd_line); */
/* void show_ompi_version(const char *scope); */

/*
 * Parameter/configuration-related functions
 */

/* extern char *param_all; */

/* extern char *path_prefix; */
/* extern char *path_bindir; */
/* extern char *path_libdir; */
/* extern char *path_incdir; */
/* extern char *path_pkglibdir; */
/* extern char *path_sysconfdir; */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* ORTED_H */
