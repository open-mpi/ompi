/*
 * $HEADER$
 */

#include "lam/lfc/object.h"

lam_class_info_t lam_object_cls = { "lam_object_t", 0, lam_obj_init, lam_obj_destroy };

void lam_obj_init(lam_object_t *obj)
{
    obj->obj_refcnt = 1;
}

void lam_obj_destroy(lam_object_t *obj)
{
    free(obj);
}

