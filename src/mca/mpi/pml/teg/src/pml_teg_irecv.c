#include "pml_teg.h"

                                                                                                             
extern int mca_pml_teg_irecv_init(
    void *buf,
    size_t size,
    struct lam_datatype_t *datatype,
    int src,
    int tag,
    bool persistent,
    struct lam_communicator_t* comm,
    struct lam_request_t **request
)
{
    return LAM_ERROR;
}
                                                                                                                              
int mca_pml_teg_irecv(
    void *buf,
    size_t size,
    struct lam_datatype_t *datatype,
    int src,
    int tag,
    struct lam_communicator_t* comm,
    struct lam_request_t **request
)
{
    return LAM_ERROR;
}

