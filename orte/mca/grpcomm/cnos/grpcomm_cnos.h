/* -*- C -*-
 * 
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
 *
 */
#ifndef GRPCOMM_BASIC_H
#define GRPCOMM_BASIC_H

#include "orte_config.h"
#include "orte/orte_types.h"
#include "orte/orte_constants.h"

#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "opal/class/opal_object.h"

#include "orte/mca/grpcomm/grpcomm.h"

BEGIN_C_DECLS

/*
 * globals
 */
/*
 * globals needed within component
 */
 
/*
 * Module open / close
 */
int orte_grpcomm_cnos_open(void);
int orte_grpcomm_cnos_close(void);
orte_grpcomm_base_module_t* orte_grpcomm_cnos_init(int *priority);


/*
 * Startup / Shutdown
 */
int orte_grpcomm_cnos_module_init(void);
int orte_grpcomm_cnos_finalize(void);

/*
 * xcast interfaces
 */

void orte_ns_replica_recv(int status, orte_process_name_t* sender,
                          orte_buffer_t* buffer, orte_rml_tag_t tag, void* cbdata);


ORTE_MODULE_DECLSPEC extern orte_grpcomm_base_component_t mca_grpcomm_cnos_component;
extern orte_grpcomm_base_module_t orte_grpcomm_cnos_module;

END_C_DECLS

#endif
