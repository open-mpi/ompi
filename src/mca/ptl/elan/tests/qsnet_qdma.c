#include "qsnet/fence.h"
#include "ptl_elan.h"
#include "ptl_elan_priv.h"

extern mca_ptl_elan_module_1_0_0_t mca_ptl_elan_module;

int test_qdma(mca_ptl_elan_module_1_0_0_t *emp, int reps);

int ptl_elan_init_event();
int ptl_elan_free_event();
int ptl_elan_poll_event();
int ptl_elan_wait_event();

int ptl_elan_queue_send();  /* Initialize a Tx event */
int ptl_elan_queue_recv(); /* Wait for a recv event */

int main (int argc, char ** argv)
{
    /* Initialization test */
    ompi_mca_ptl_elan_init (&mca_ptl_elan_module);

    /* Please replace with a barrier at the beginning */

    sleep(5); /* Sleep 5 seconds for others to catch up */

    test_qdma(&mca_ptl_elan_module, 1);

    /* Please replace with a barrier at the end */
    sleep(5); /* Sleep 5 seconds for others to catch up */

    /* Finalize the device */
    ompi_mca_ptl_elan_finalize (&mca_ptl_elan_module);

    /* Tell alive */
    fprintf(stdout, "I am still alive\n");
    fflush(stdout);
    return 0;
}

int test_qdma(mca_ptl_elan_module_1_0_0_t *emp, int reps)
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
        /*elan_queueRxWait(qr,rbuf,waitType);*/
        ptl_elan_queue_recv();
    }

    while (--r >= 0) {
        /* Trigger a send event */
        ptl_elan_queue_send();
        ptl_elan_queue_recv();
    }

    if (0 != ptl->elan_vp) {
        /* Trigger one more send */
        ptl_elan_queue_send();
    }

    end0 = elan4_clock(ctx);
    nsec = ((end0 - start0) / reps);
    t = ((double) nsec)/(2*1000.0);

    return(0);
}

int ptl_elan_queue_send() {
    return OMPI_SUCCESS;
}

/* This function needs no event related knowledge */
int ptl_elan_queue_recv () 
{
    return OMPI_SUCCESS;
}

int ptl_elan_init_event() 
{
    return OMPI_SUCCESS;
}
