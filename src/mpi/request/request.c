/*
 * $HEADER$
 */

#include "mpi/request/request.h"

lam_class_info_t lam_request_cls = { 
    "lam_request_t", 
    &lam_object_cls,
    (class_init_t) lam_request_init,
    (class_destroy_t) lam_request_destroy,
};


void lam_request_init(lam_request_t* rq)
{
    SUPER_INIT(rq, &lam_object_cls);
}


void lam_request_destroy(lam_request_t* rq)
{
    SUPER_DESTROY(rq, &lam_object_cls);
}


