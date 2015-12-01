/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"

#include "orte/util/name_fns.h"

#include "opal/mca/base/base.h"
#include "opal/util/output.h"

#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/rml/rml.h"

#include "rml_ftrm.h"

orte_rml_component_t mca_rml_ftrm_wrapped_component;
orte_rml_module_t   orte_rml_ftrm_wrapped_module;

/*
 * Init (Module)
 */

int orte_rml_ftrm_module_enable_comm(void)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: module_init(): Normal...");

    if( NULL != orte_rml_ftrm_wrapped_module.enable_comm ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.enable_comm() ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}

/*
 * Finalize (Module)
 */
int orte_rml_ftrm_module_finalize(void)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: module_finalize()");


    if( NULL != orte_rml_ftrm_wrapped_module.finalize ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.finalize() ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}


/*
 * Get URI
 */
char * orte_rml_ftrm_get_contact_info(void)
{
    char * rtn_val = NULL;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: get_uri()");


    if( NULL != orte_rml_ftrm_wrapped_module.get_contact_info ) {
        rtn_val = orte_rml_ftrm_wrapped_module.get_contact_info();
    }

    return rtn_val;
}

/*
 * Set CONTACT_INFO
 */
void orte_rml_ftrm_set_contact_info(const char* contact_info)
{
    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: set_contact_info()");

    if( NULL != orte_rml_ftrm_wrapped_module.set_contact_info ) {
        orte_rml_ftrm_wrapped_module.set_contact_info(contact_info);
    }
}


/*
 * Ping
 */
int orte_rml_ftrm_ping(const char* uri, const struct timeval* tv)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: ping()");

    if( NULL != orte_rml_ftrm_wrapped_module.ping ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.ping(uri, tv) ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}


/*
 * Send Non-blocking
 */
int orte_rml_ftrm_send_nb(orte_process_name_t* peer,
                          struct iovec* msg,
                          int count,
                          orte_rml_tag_t tag,
                          orte_rml_callback_fn_t cbfunc,
                          void* cbdata)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: send_nb(%s, %d, %d )",
                        ORTE_NAME_PRINT(peer), count, tag);

    if( NULL != orte_rml_ftrm_wrapped_module.send_nb ) {
        if(ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.send_nb(peer, msg, count, tag, cbfunc, cbdata))) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}

/*
 * Send Buffer Non-blocking
 */
int orte_rml_ftrm_send_buffer_nb(orte_process_name_t* peer,
                                 opal_buffer_t* buffer,
                                 orte_rml_tag_t tag,
                                 orte_rml_buffer_callback_fn_t cbfunc,
                                 void* cbdata)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: send_buffer_nb(%s, %d )",
                        ORTE_NAME_PRINT(peer), tag);

    if( NULL != orte_rml_ftrm_wrapped_module.send_buffer_nb ) {
        if(ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.send_buffer_nb(peer, buffer, tag, cbfunc, cbdata))) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}



/*
 * Recv Non-blocking
 */
void orte_rml_ftrm_recv_nb(orte_process_name_t* peer,
                          orte_rml_tag_t tag,
			  bool persistent,
                          orte_rml_callback_fn_t cbfunc,
                          void* cbdata)
{
    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: recv_nb(%s, %d, %d )",
                        ORTE_NAME_PRINT(peer), tag, persistent);

    if( NULL != orte_rml_ftrm_wrapped_module.recv_nb ) {
        orte_rml_ftrm_wrapped_module.recv_nb(peer, tag, persistent, cbfunc, cbdata);
    }
}

/*
 * Recv Buffer Non-blocking
 */
void orte_rml_ftrm_recv_buffer_nb(orte_process_name_t* peer,
                                 orte_rml_tag_t tag,
                                 bool persistent,
                                 orte_rml_buffer_callback_fn_t cbfunc,
                                 void* cbdata)
{
    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: recv_buffer_nb(%s, %d, %d)",
                        ORTE_NAME_PRINT(peer), tag, persistent);

    if( NULL != orte_rml_ftrm_wrapped_module.recv_buffer_nb ) {
        orte_rml_ftrm_wrapped_module.recv_buffer_nb(peer, tag, persistent, cbfunc, cbdata);
    }
}

/*
 * Recv Cancel
 */
void orte_rml_ftrm_recv_cancel(orte_process_name_t* peer, orte_rml_tag_t tag)
{
    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: recv_cancel()");

    if( NULL != orte_rml_ftrm_wrapped_module.recv_cancel ) {
        orte_rml_ftrm_wrapped_module.recv_cancel(peer, tag);
    }
}


/*
 * Register a callback on loss of connection
 */
int orte_rml_ftrm_add_exception_handler(orte_rml_exception_callback_t cbfunc)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: add_exception_handler()");

    if( NULL != orte_rml_ftrm_wrapped_module.add_exception_handler ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.add_exception_handler(cbfunc) ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}

int orte_rml_ftrm_del_exception_handler(orte_rml_exception_callback_t cbfunc)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: del_exception_handler()");

    if( NULL != orte_rml_ftrm_wrapped_module.del_exception_handler ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.del_exception_handler(cbfunc) ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}

void orte_rml_ftrm_purge(orte_process_name_t *peer)
{
    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: purge()");

    if( NULL != orte_rml_ftrm_wrapped_module.purge ) {
        orte_rml_ftrm_wrapped_module.purge(peer);
    }
}
