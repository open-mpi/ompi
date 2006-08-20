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
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "orte/mca/oob/oob.h"


OBJ_CLASS_INSTANCE(
    mca_oob_base_exception_handler_t,
    opal_list_item_t,
    NULL,
    NULL
);


/**
 *  Register a callback function on loss of a connection.
 */
                                                                                                                 
int mca_oob_add_exception_handler(
    mca_oob_base_exception_fn_t cbfunc)
{
    mca_oob_base_exception_handler_t *eh = OBJ_NEW(mca_oob_base_exception_handler_t);
    eh->cbfunc = cbfunc;
    opal_list_append(&mca_oob_base_exception_handlers, &eh->super);
    return ORTE_SUCCESS;
}
                                                                                                                 
/**
 * Remove a callback
 */
                                                                                                                 
int mca_oob_del_exception_handler(
    mca_oob_base_exception_fn_t cbfunc)
{
    opal_list_item_t* item;
    item = opal_list_get_first(&mca_oob_base_exception_handlers);
    while(item != opal_list_get_end(&mca_oob_base_exception_handlers)) {
        opal_list_item_t* next = opal_list_get_next(item);
        mca_oob_base_exception_handler_t* eh = (mca_oob_base_exception_handler_t*)item;
        if(eh->cbfunc == cbfunc) {
            opal_list_remove_item(&mca_oob_base_exception_handlers, &eh->super);
            OBJ_RELEASE(eh);
        }
        item = next;
    }
    return ORTE_SUCCESS;
}

/**
 * Invoke exception handlers
 */
                                                                                                                 
void mca_oob_call_exception_handlers(
    orte_process_name_t* peer, int exception)
{
    opal_list_item_t* item;
    item = opal_list_get_first(&mca_oob_base_exception_handlers);
    while(item != opal_list_get_end(&mca_oob_base_exception_handlers)) {
        opal_list_item_t* next = opal_list_get_next(item);
        mca_oob_base_exception_handler_t* eh = (mca_oob_base_exception_handler_t*)item;
        eh->cbfunc(peer,exception);
        item = next;
    }
}
                                                                                                                 

