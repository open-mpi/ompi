/*
 * $HEADER$
 */

#include "request/request.h"

lam_class_t lam_request_t_class = { 
    "lam_request_t", 
    OBJ_CLASS(lam_object_t),
    (lam_construct_t) lam_request_construct,
    (lam_destruct_t) lam_request_destruct,
};


void lam_request_construct(lam_request_t* rq)
{
}


void lam_request_destruct(lam_request_t* rq)
{
}


