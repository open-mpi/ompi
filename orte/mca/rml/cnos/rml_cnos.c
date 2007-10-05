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

#include "orte_config.h"

#include "orte/orte_constants.h"
#include "opal/util/output.h"
#include "orte/mca/rml/base/base.h"
#include "rml_cnos.h"
#include "orte/mca/errmgr/errmgr.h"

#if OMPI_RML_CNOS_HAVE_BARRIER
#include <catamount/cnos_mpi_os.h>
#endif

orte_rml_component_t mca_rml_cnos_component = {
    /* First, the mca_base_component_t struct containing meta
     * information about the component itself */

    {
        /* Indicate that we are a rml v1.0.0 component (which also
         * implies a specific MCA version) */

        ORTE_RML_BASE_VERSION_1_0_0,

        "cnos",			       /* MCA component name */
        ORTE_MAJOR_VERSION,		       /* MCA component major version */
        ORTE_MINOR_VERSION,		       /* MCA component minor version */
        ORTE_RELEASE_VERSION,	       /* MCA component release version */
        orte_rml_cnos_open,		       /* component open */
        orte_rml_cnos_close,		       /* component close */
    }
    ,

    /* Next the MCA v1.0.0 component meta data */
    {
        /* Whether the component is checkpointable or not */
        false}
    ,
    orte_rml_cnos_init
};

orte_rml_module_t orte_rml_cnos_module = {
    orte_rml_cnos_module_enable_comm,
    orte_rml_cnos_module_fini,

    orte_rml_cnos_get_contact_info,
    orte_rml_cnos_set_contact_info,

    orte_rml_cnos_get_new_name,
    orte_rml_cnos_ping,

    orte_rml_cnos_send,
    orte_rml_cnos_send_nb,
    orte_rml_cnos_send_buffer,
    orte_rml_cnos_send_buffer_nb,

    orte_rml_cnos_recv,
    orte_rml_cnos_recv_nb,
    orte_rml_cnos_recv_buffer,
    orte_rml_cnos_recv_buffer_nb,
    orte_rml_cnos_recv_cancel,

    orte_rml_cnos_add_exception_handler,
    orte_rml_cnos_del_exception_handler,

    NULL /* No FT Event function */
};



int
orte_rml_cnos_open(void)
{
    return ORTE_SUCCESS;
}


orte_rml_module_t *
orte_rml_cnos_init(int *priority)
{
    *priority = 0;
    return &orte_rml_cnos_module;
}


int
orte_rml_cnos_close(void)
{
    return ORTE_SUCCESS;
}

int
orte_rml_cnos_module_enable_comm(void)
{
    return ORTE_SUCCESS;
}

int
orte_rml_cnos_module_fini(void)
{
    return ORTE_SUCCESS;
}

char *
orte_rml_cnos_get_contact_info(void)
{
    return "(none)";
}

int
orte_rml_cnos_set_contact_info(const char *name)
{
    return ORTE_ERR_NOT_SUPPORTED;
}


int
orte_rml_cnos_get_new_name(orte_process_name_t *name)
{
    return ORTE_ERR_NOT_SUPPORTED;
}


int
orte_rml_cnos_ping(const char *uri, const struct timeval *tv)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

int
orte_rml_cnos_send(orte_process_name_t * peer,
		     struct iovec *msg, int count, int tag, int flags)
{
    return ORTE_SUCCESS;
}

int
orte_rml_cnos_send_buffer(orte_process_name_t * peer,
			    orte_buffer_t * buffer,
			    orte_rml_tag_t tag, int flags)
{
    return ORTE_SUCCESS;
}

int
orte_rml_cnos_recv(orte_process_name_t * peer,
		     struct iovec *msg,
		     int count, orte_rml_tag_t tag, int flags)
{
    return ORTE_SUCCESS;
}

int
orte_rml_cnos_recv_buffer(orte_process_name_t * peer,
                          orte_buffer_t * buf, orte_rml_tag_t tag, int flags)
{
    return ORTE_SUCCESS;
}

int
orte_rml_cnos_send_nb(orte_process_name_t * peer,
			struct iovec *msg,
			int count,
			orte_rml_tag_t tag,
			int flags, orte_rml_callback_fn_t cbfunc, void *cbdata)
{
    return ORTE_SUCCESS;
}

int
orte_rml_cnos_send_buffer_nb(orte_process_name_t * peer,
			       orte_buffer_t * buffer,
			       orte_rml_tag_t tag,
			       int flags,
			       orte_rml_buffer_callback_fn_t
			       cbfunc, void *cbdata)
{
    return ORTE_SUCCESS;
}

int
orte_rml_cnos_recv_nb(orte_process_name_t * peer,
			struct iovec *msg,
			int count,
			orte_rml_tag_t tag,
			int flags, orte_rml_callback_fn_t cbfunc, void *cbdata)
{
    return ORTE_SUCCESS;
}

int
orte_rml_cnos_recv_buffer_nb(orte_process_name_t * peer,
			       orte_rml_tag_t tag,
			       int flags,
			       orte_rml_buffer_callback_fn_t
			       cbfunc, void *cbdata)
{
    return ORTE_SUCCESS;
}

int
orte_rml_cnos_recv_cancel(orte_process_name_t * peer, orte_rml_tag_t tag)
{
    return ORTE_SUCCESS;
}

int orte_rml_cnos_add_exception_handler(orte_rml_exception_callback_t cbfunc)
{
    return ORTE_SUCCESS;
}

int orte_rml_cnos_del_exception_handler(orte_rml_exception_callback_t cbfunc)
{
    return ORTE_SUCCESS;
}
