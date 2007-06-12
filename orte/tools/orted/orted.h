/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
    int uri_pipe;
    opal_mutex_t mutex;
    opal_condition_t condition;
    bool exit_condition;
    bool spin;
} orted_globals_t;

ORTE_DECLSPEC extern orted_globals_t orted_globals;

/* orted communication functions */
void orte_daemon_recv(int status, orte_process_name_t* sender,
                      orte_buffer_t *buffer, orte_rml_tag_t tag,
                      void* cbdata);

void orte_daemon_recv_routed(int status, orte_process_name_t* sender,
                             orte_buffer_t *buffer, orte_rml_tag_t tag,
                             void* cbdata);

void orte_daemon_recv_gate(int status, orte_process_name_t* sender,
                           orte_buffer_t *buffer, orte_rml_tag_t tag,
                           void* cbdata);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* ORTED_H */
