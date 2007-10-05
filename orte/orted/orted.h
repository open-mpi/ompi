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

#include "orte/dss/dss_types.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/rml/rml_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* main orted routine */
ORTE_DECLSPEC int orte_daemon(int argc, char *argv[]);

/* setup routine - needed to instantiate the orted globals in libopenrte. To
 * make it at least be useful, will also start the necessary communication
 * receive calls for daemon comm
 */
ORTE_DECLSPEC int orte_daemon_setup(void);
    
/* orted communication functions */
ORTE_DECLSPEC void orte_daemon_recv(int status, orte_process_name_t* sender,
                      orte_buffer_t *buffer, orte_rml_tag_t tag,
                      void* cbdata);

ORTE_DECLSPEC void orte_daemon_recv_routed(int status, orte_process_name_t* sender,
                             orte_buffer_t *buffer, orte_rml_tag_t tag,
                             void* cbdata);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* ORTED_H */
