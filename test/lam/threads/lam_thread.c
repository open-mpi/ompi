#include "support.h"
#include "lam/constants.h"
#include "lam/threads/thread.h"
#include "lam/os/atomic.h"


static volatile int count = 0;


static void* thr1_run(lam_object_t* obj)
{
    fetchNadd(&count, 1);
    return NULL;
}

static void* thr2_run(lam_object_t* obj)
{
    fetchNadd(&count, 2);
    return NULL;
}

int main(int argc, char** argv)
{
    int rc;
    lam_thread_t thr1;
    lam_thread_t thr2;

    test_init("lam_thread_t");

    OBJ_CONSTRUCT(&thr1, lam_thread_t);
    OBJ_CONSTRUCT(&thr2, lam_thread_t);

    thr1.t_run = thr1_run;
    thr2.t_run = thr2_run;

    rc = lam_thread_start(&thr1);
    test_verify_int(LAM_SUCCESS, rc);

    rc = lam_thread_start(&thr2);
    test_verify_int(LAM_SUCCESS, rc);
   
    rc = lam_thread_join(&thr1, NULL);
    test_verify_int(LAM_SUCCESS, rc);

    rc = lam_thread_join(&thr2, NULL);
    test_verify_int(LAM_SUCCESS, rc);

    test_verify_int(3, count);
    return test_finalize();
}



