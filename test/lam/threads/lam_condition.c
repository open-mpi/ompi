#include <stdio.h>
#include <time.h>
#include "support.h"
#include "lam/constants.h"
#include "lam/threads/thread.h"
#include "lam/threads/condition.h"
#include "lam/os/atomic.h"


lam_mutex_t mutex;
lam_condition_t thr1_cond;
lam_condition_t thr2_cond;

int thr1_count = 0;
int thr2_count = 0;


#define TEST_COUNT 1000000


static void* thr1_run(lam_object_t* obj)
{
    int i;
    lam_mutex_lock(&mutex);
    clock_t c1 = clock();
    for(i=0; i<TEST_COUNT; i++) {
        lam_condition_wait(&thr1_cond, &mutex); 
        lam_condition_signal(&thr2_cond);
        thr1_count++;
    }
    clock_t c2 = clock();
    lam_mutex_unlock(&mutex);
    fprintf(stderr, "thr1: time per iteration: %ld usec\n", (c2 - c1) / TEST_COUNT);
    return NULL;
}

static void* thr2_run(lam_object_t* obj)
{
    int i;
    lam_mutex_lock(&mutex);
    clock_t c1 = clock();
    for(i=0; i<TEST_COUNT; i++) {
        lam_condition_signal(&thr1_cond);
        lam_condition_wait(&thr2_cond, &mutex);
        thr2_count++;
    }
    clock_t c2 = clock();
    lam_mutex_unlock(&mutex);
    fprintf(stderr, "thr2: time per iteration: %ld usec\n", (c2 - c1) / TEST_COUNT);
    return NULL;
}


int main(int argc, char** argv)
{
    int rc;
    lam_thread_t* thr1;
    lam_thread_t* thr2;

    test_init("lam_condition_t");

    OBJ_CONSTRUCT(&mutex, lam_mutex_t);
    OBJ_CONSTRUCT(&thr1_cond, lam_condition_t);
    OBJ_CONSTRUCT(&thr2_cond, lam_condition_t);
  
    thr1 = OBJ_NEW(lam_thread_t);
    thr2 = OBJ_NEW(lam_thread_t);
    thr1->t_run = thr1_run;
    thr2->t_run = thr2_run;

    rc = lam_thread_start(thr1);
    test_verify_int(LAM_SUCCESS, rc);

    rc = lam_thread_start(thr2);
    test_verify_int(LAM_SUCCESS, rc);
   
    rc = lam_thread_join(thr1, NULL);
    test_verify_int(LAM_SUCCESS, rc);
    test_verify_int(TEST_COUNT, thr1_count);

    rc = lam_thread_join(thr2, NULL);
    test_verify_int(LAM_SUCCESS, rc);
    test_verify_int(TEST_COUNT, thr2_count);

    test_finalize();
    return 0;
}



