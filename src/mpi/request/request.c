/*
 * $HEADER$
 */

#include "mpi/request/request.h"

lam_class_info_t lam_request_t_class_info = { 
    "lam_request_t", 
    CLASS_INFO(lam_object_t),
    (lam_construct_t) lam_request_construct,
    (lam_destruct_t) lam_request_destruct,
};


void lam_request_construct(lam_request_t* rq)
{
    OBJ_CONSTRUCT_SUPER(rq, lam_object_t);
}


void lam_request_destruct(lam_request_t* rq)
{
    OBJ_DESTRUCT_SUPER(rq, lam_object_t);
}


