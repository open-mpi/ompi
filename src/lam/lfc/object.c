/*
 * $HEADER$
 */

/**
 * @file Implementation of lam_object_t, the base lam foundation class
 */

#include "lam/lfc/object.h"

void lam_object_construct(lam_object_t * obj)
{
    obj->obj_reference_count = 1;
}


void lam_object_destruct(lam_object_t * obj)
{
    /* Move along, nothing to see here! */
}


lam_class_info_t lam_object_t_class_info = {
    "lam_object_t",
    0,
    lam_object_construct,
    lam_object_destruct
};



