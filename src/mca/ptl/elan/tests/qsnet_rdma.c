/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "ompi_config.h"
#include "qsnet/fence.h"
#include "ptl_elan.h"
#include "ptl_elan_priv.h"
#include "test_util.h"

extern mca_ptl_elan_module_1_0_0_t mca_ptl_elan_module;

int ptl_elan_rdma_put(); /* Initialize a Tx event */
int ptl_elan_rdma_get(); /* Wait for a recv event */
int test_rdma(mca_ptl_elan_module_1_0_0_t *emp, int reps);

int main (int argc, char ** argv)
{
    bool  allow_threads;
    bool  have_hidden_threads;
    int   input;
    int   num;

    /* Get some environmental variables set for Open MPI, OOB */
    env_init_for_elan();

    /* Initialization */
    mca_ptl_elan_module_open();
    mca_ptl_elan_module_init(&num, &allow_threads, &have_hidden_threads);
    mca_ptl_elan_module_control(1, &input, 4);

    /* Testing QDMA */
    test_rdma(&mca_ptl_elan_module, 1);

    mca_ptl_elan_module_close();
    return 0;
}

int test_rdma(mca_ptl_elan_module_1_0_0_t *emp, int reps)
{
    uint64_t start0, end0;
    double   t;
    int      r;
    double   nsec;

    ELAN4_CTX *ctx;
    RAIL      *rail;
    mca_ptl_elan_t *ptl;

    r   = reps;
    ptl = emp->elan_ptls[0];
    ctx = emp->elan_ctrl->elan_ctx;
    rail = (RAIL *) emp->elan_ctrl->elan_rail[0];
    
    start0 = elan4_clock(ctx);

    if (0 != ptl->elan_vp) {
        r--;
        ptl_elan_rdma_get();
    }

    while (--r >= 0) {
        /* Trigger a send event */
        ptl_elan_rdma_put();
        ptl_elan_rdma_get();
    }

    if (0 != ptl->elan_vp) {
        /* Trigger one more send */
        ptl_elan_rdma_put();
    }

    end0 = elan4_clock(ctx);
    nsec = ((end0 - start0) / reps);
    t = ((double) nsec)/(2*1000.0);

    return(0);
}

int ptl_elan_rdma_put() {
    return OMPI_SUCCESS;
}

/* This function needs no event related knowledge */
int ptl_elan_rdma_get () 
{
    return OMPI_SUCCESS;
}

