#include "mca/mpi/pml/base/pml_base_sendreq.h"
#include "mca/mpi/pml/base/pml_base_recvreq.h"
#include "pml_teg.h"

#define mca_teg_param_register(n,v) \
    mca_base_param_lookup_int( \
        mca_base_param_register_int("pml","teg",n,0,v))


mca_pml_base_module_1_0_0_t mca_pml_teg_module = {
    /* First, the mca_base_module_t struct containing meta information
       about the module itself */
                                                                                                                            
    {
    /* Indicate that we are a pml v1.0.0 module (which also implies a
       specific MCA version) */
                                                                                                                            
    MCA_PML_BASE_VERSION_1_0_0,
                                                                                                                            
    "teg", /* MCA module name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    mca_pml_teg_open,  /* module open */
    mca_pml_teg_close  /* module close */
    },
                                                                                                                            
    /* Next the MCA v1.0.0 module meta data */
                                                                                                                            
    {
    /* Whether the module is checkpointable or not */
                                                                                                                            
    false
    },
                                                                                                                            
    mca_pml_teg_query,  /* module query */
    mca_pml_teg_init  /* module init */
};
                                                                                                                            

mca_pml_teg_1_0_0_t mca_pml_teg = {
    {
    mca_pml_teg_addprocs,
    mca_pml_teg_isend,
    mca_pml_teg_progress
    }
};


int mca_pml_teg_open(lam_cmd_line_t* cmd_line)
{
    mca_pml_teg.teg_send_free_list_min_pages =
        mca_pml_teg_register_int("send_free_list_min_pages", 10);
    mca_pml_teg.teg_send_free_list_max_pages =
        mca_pml_teg_register_int("send_free_list_max_pages", -1);
    mca_pml_teg.teg_send_free_list_inc_pages =
        mca_pml_teg_register_int("send_free_list_inc_pages", 1);
    mca_pml_teg.teg_recv_free_list_min_pages =
        mca_pml_teg_register_int("recv_free_list_min_pages", 10);
    mca_pml_teg.teg_recv_free_list_max_pages =
        mca_pml_teg_register_int("recv_free_list_max_pages", -1);
    mca_pml_teg.teg_recv_free_list_inc_pages =
        mca_pml_teg_register_int("recv_free_list_inc_pages", 1);
    return LAM_SUCCESS;
}


mca_pml_1_0_0_t* mca_pml_teg_init(
    struct lam_proc_t **procs,
    int nprocs,
    int *max_tag,
    int *max_cid)
{
    int rc;
    lam_free_list_init(&mca_pml_teg.teg_send_free_list);
    if((rc = lam_free_list_init_with(
        &mca_pml_teg.teg_send_free_list,
        sizeof(mca_pml_base_send_request_t),
        mca_pml_teg.teg_send_free_list_min_pages,
        mca_pml_teg.teg_send_free_list_max_pages,
        mca_pml_teg.teg_send_free_list_inc_pages,
        NULL)) != LAM_SUCCESS)
    {
        return NULL;
    }

    lam_free_list_init(&mca_pml_teg.teg_recv_free_list);
    if((rc = lam_free_list_init_with(
        &mca_pml_teg.teg_recv_free_list,
        sizeof(mca_pml_base_recv_request_t),
        mca_pml_teg.teg_recv_free_list_min_pages,
        mca_pml_teg.teg_recv_free_list_max_pages,
        mca_pml_teg.teg_recv_free_list_inc_pages,
        NULL)) != LAM_SUCCESS)
    {
        return NULL;
    }

    lam_free_list_init(&mca_pml_teg.teg_incomplete_sends);
    return &mca_pml_teg.super;
}
                                                                                                 
                                                                                                 
