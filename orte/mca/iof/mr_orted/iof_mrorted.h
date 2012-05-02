/*
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#ifndef ORTE_IOF_MR_ORTED_H
#define ORTE_IOF_MR_ORTED_H

#include "orte_config.h"

#include "opal/class/opal_list.h"

#include "orte/mca/rml/rml_types.h"

#include "orte/mca/iof/iof.h"

BEGIN_C_DECLS

/**
 * IOF MR_ORTED Component 
 */
typedef struct { 
    orte_iof_base_component_t super;
    opal_list_t sinks;
    opal_list_t procs;
} orte_iof_mrorted_component_t;

ORTE_MODULE_DECLSPEC extern orte_iof_mrorted_component_t mca_iof_mr_orted_component;
extern orte_iof_base_module_t orte_iof_mrorted_module;

void orte_iof_mrorted_recv(int status, orte_process_name_t* sender,
                           opal_buffer_t* buffer, orte_rml_tag_t tag,
                           void* cbdata);

void orte_iof_mrorted_read_handler(int fd, short event, void *data);
void orte_iof_mrorted_send_xonxoff(orte_process_name_t *name, orte_iof_tag_t tag);

END_C_DECLS

#endif
