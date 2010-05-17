/* -*- C -*-
 * 
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */
#ifndef NOTIFIER_HNP_H
#define NOTIFIER_HNP_H

#include "orte_config.h"

#include "orte/types.h"
#include "orte/mca/notifier/notifier.h"
#include "orte/mca/rml/rml.h"
#include "opal/class/opal_pointer_array.h"

BEGIN_C_DECLS

void orte_notifier_hnp_recv_cb(int status, orte_process_name_t* sender,
                               opal_buffer_t* buffer, orte_rml_tag_t tag,
                               void* cbdata);
#if OPAL_ENABLE_DEBUG
void orte_notifier_hnp_exception_cb(const orte_process_name_t* peer, 
                                    orte_rml_exception_t reason);
#endif

extern opal_pointer_array_t orte_notifier_hnp_tables;
extern opal_mutex_t         orte_notifier_hnp_tables_lock;

/*
 * Notifier interfaces
 */

ORTE_MODULE_DECLSPEC extern orte_notifier_base_component_t mca_notifier_hnp_component;
extern orte_notifier_base_module_t orte_notifier_hnp_module;

END_C_DECLS

#endif
