/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <errno.h>
#include <unistd.h>
#include <string.h>

#include "include/constants.h"
#include "util/output.h"
#include "mca/rml/rml.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"


orte_rml_module_t orte_rml_oob_module = {
    mca_oob_base_module_init,
    NULL,
    (orte_rml_module_get_uri_fn_t)mca_oob_get_contact_info,
    (orte_rml_module_set_uri_fn_t)mca_oob_set_contact_info,
    (orte_rml_module_parse_uris_fn_t)mca_oob_parse_contact_info,
    (orte_rml_module_ping_fn_t)mca_oob_ping,
    (orte_rml_module_send_fn_t)mca_oob_send,
    (orte_rml_module_send_nb_fn_t)mca_oob_send_nb,
    (orte_rml_module_send_buffer_fn_t)mca_oob_send_packed,
    (orte_rml_module_send_buffer_nb_fn_t)mca_oob_send_packed_nb,
    (orte_rml_module_recv_fn_t)mca_oob_recv,
    (orte_rml_module_recv_nb_fn_t)mca_oob_recv_nb,
    (orte_rml_module_recv_buffer_fn_t)mca_oob_recv_packed,
    (orte_rml_module_recv_buffer_nb_fn_t)mca_oob_recv_packed_nb,
    (orte_rml_module_recv_cancel_fn_t)mca_oob_recv_cancel,
    (orte_rml_module_barrier_fn_t)mca_oob_barrier,
    (orte_rml_module_xcast_fn_t)mca_oob_xcast
};


