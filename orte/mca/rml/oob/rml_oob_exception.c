/*
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "rml_oob.h"

#include "opal/class/opal_list.h"

struct orte_rml_oob_exception_t {
    opal_list_item_t super;
    orte_rml_exception_callback_t cbfunc;
};
typedef struct orte_rml_oob_exception_t orte_rml_oob_exception_t;
static OBJ_CLASS_INSTANCE(orte_rml_oob_exception_t, opal_list_item_t,
                          NULL, NULL);


void
orte_rml_oob_exception_callback(const orte_process_name_t *peer,
                                orte_rml_exception_t exception)
{
    opal_list_item_t *item;

    for (item = opal_list_get_first(&orte_rml_oob_module.exceptions) ;
         item != opal_list_get_end(&orte_rml_oob_module.exceptions) ;
         item = opal_list_get_next(item)) {
        orte_rml_oob_exception_t *ex = (orte_rml_oob_exception_t*) item;
        ex->cbfunc(peer, exception);
    }
}


int
orte_rml_oob_add_exception(orte_rml_exception_callback_t cbfunc)
{
    orte_rml_oob_exception_t *ex = OBJ_NEW(orte_rml_oob_exception_t);

    if (NULL == ex) return ORTE_ERROR;

    ex->cbfunc = cbfunc;
    opal_list_append(&orte_rml_oob_module.exceptions, &ex->super);

    return ORTE_SUCCESS;
}


int
orte_rml_oob_del_exception(orte_rml_exception_callback_t cbfunc)
{
    opal_list_item_t *item;

    for (item = opal_list_get_first(&orte_rml_oob_module.exceptions) ;
         item != opal_list_get_end(&orte_rml_oob_module.exceptions) ;
         item = opal_list_get_next(item)) {
        orte_rml_oob_exception_t *ex = (orte_rml_oob_exception_t*) item;

        if (cbfunc == ex->cbfunc) {
            opal_list_remove_item(&orte_rml_oob_module.exceptions, item);
            return ORTE_SUCCESS;
        }
    }
    return ORTE_ERR_NOT_FOUND;
}
