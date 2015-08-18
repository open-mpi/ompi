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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * ORTE RML Fault Tolerance Wrapper - Ready Message Protocol (FTRM)
 *
 * @file
 */
#ifndef MCA_RML_FTRM_H
#define MCA_RML_FTRM_H

#include "orte_config.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"

BEGIN_C_DECLS

    extern int rml_ftrm_output_handle;

    /*
     * Component Information
     */
    ORTE_MODULE_DECLSPEC extern orte_rml_component_t mca_rml_ftrm_component;
    ORTE_MODULE_DECLSPEC extern orte_rml_module_t    orte_rml_ftrm_module;

    ORTE_MODULE_DECLSPEC extern orte_rml_component_t mca_rml_ftrm_wrapped_component;
    ORTE_MODULE_DECLSPEC extern orte_rml_module_t    orte_rml_ftrm_wrapped_module;

    /*
     * Init (Component)
     */
    orte_rml_module_t* orte_rml_ftrm_component_init(int  *priority);

    /*
     * Init (Module)
     */
    int orte_rml_ftrm_module_enable_comm(void);

    /*
     * Finalize (Module)
     */
    int orte_rml_ftrm_module_finalize(void);

    /*
     * Get URI
     */
    char * orte_rml_ftrm_get_contact_info(void);

    /*
     * Set URI
     */
    void orte_rml_ftrm_set_contact_info(const char* uri);

    /*
     * Ping
     */
    int orte_rml_ftrm_ping(const char* uri, const struct timeval* tv);

    /*
     * Send Non-blocking
     */
    int orte_rml_ftrm_send_nb(orte_process_name_t* peer,
                              struct iovec* msg,
                              int count,
                              orte_rml_tag_t tag,
                              orte_rml_callback_fn_t cbfunc,
                              void* cbdata);

    /*
     * Send Buffer Non-blocking
     */
    int orte_rml_ftrm_send_buffer_nb(orte_process_name_t* peer,
                                     opal_buffer_t* buffer,
                                     orte_rml_tag_t tag,
                                     orte_rml_buffer_callback_fn_t cbfunc,
                                     void* cbdata);

    /*
     * Recv Non-blocking
     */
    void orte_rml_ftrm_recv_nb(orte_process_name_t* peer,
                              orte_rml_tag_t tag,
                              bool persistent,
                              orte_rml_callback_fn_t cbfunc,
                              void* cbdata);

    /*
     * Recv Buffer Non-blocking
     */
    void orte_rml_ftrm_recv_buffer_nb(orte_process_name_t* peer,
                                     orte_rml_tag_t tag,
                                     bool persistent,
                                     orte_rml_buffer_callback_fn_t cbfunc,
                                     void* cbdata);

    /*
     * Recv Cancel
     */
    void orte_rml_ftrm_recv_cancel(orte_process_name_t* peer, orte_rml_tag_t tag);

    /*
     * Register a callback on loss of connection
     */
    int orte_rml_ftrm_add_exception_handler(orte_rml_exception_callback_t cbfunc);
    int orte_rml_ftrm_del_exception_handler(orte_rml_exception_callback_t cbfunc);

    /*
     * FT Event
     */
    int orte_rml_ftrm_ft_event(int state);

    void orte_rml_ftrm_purge(orte_process_name_t *peer);

END_C_DECLS

#endif
