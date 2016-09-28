/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2016 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_RML_OOB_RML_OOB_H
#define MCA_RML_OOB_RML_OOB_H

#include "orte_config.h"

#include "opal/dss/dss_types.h"
#include "opal/mca/event/event.h"

#include "orte/mca/oob/oob.h"

#include "orte/mca/rml/base/base.h"

BEGIN_C_DECLS

typedef struct {
    orte_rml_base_module_t  api;
    opal_list_t             queued_routing_messages;
    opal_event_t            *timer_event;
    struct timeval          timeout;
} orte_rml_oob_module_t;

ORTE_MODULE_DECLSPEC extern orte_rml_component_t mca_rml_oob_component;
extern orte_rml_oob_module_t orte_rml_oob_module;

int orte_rml_oob_init(void);
void orte_rml_oob_fini(void *mod);

int orte_rml_oob_send_nb(void *mod,
                         orte_process_name_t* peer,
                         struct iovec* msg,
                         int count,
                         orte_rml_tag_t tag,
                         orte_rml_callback_fn_t cbfunc,
                         void* cbdata);

int orte_rml_oob_send_buffer_nb(void *mod,
                                orte_process_name_t* peer,
                                opal_buffer_t* buffer,
                                orte_rml_tag_t tag,
                                orte_rml_buffer_callback_fn_t cbfunc,
                                void* cbdata);

int orte_rml_oob_ping(void *mod,
                      const char* uri,
                      const struct timeval* tv);

char* orte_rml_oob_get_uri(void);
void orte_rml_oob_set_uri( const char*);
void orte_rml_oob_purge( orte_process_name_t *peer);

END_C_DECLS

#endif
