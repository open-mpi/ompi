#include "lam/runtime/runtime.h"
#include "mca/lam/oob/oob.h"
#include "mca/lam/pcm/pcm.h"
#include "mca/lam/base/base.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

int
main(int argc, char* argv[])
{
  int ret;
  mca_pcm_proc_t *procs;
  size_t nprocs;
  mca_pcm_proc_t *me;
  int left_vpid, right_vpid, me_vpid;
  int count = 2;
  lam_job_handle_t job;
  int data = 0xDEADBEEF;
  int tag = MCA_OOB_ANY_TAG;
  bool threads, hidden;

  printf("hello, world!\n");
  ret = lam_init(argc, argv);
  assert(ret == LAM_SUCCESS);

  ret = mca_base_open();
  assert(ret == LAM_SUCCESS);

  ret = lam_rte_init(&threads, &hidden);
  assert(ret == LAM_SUCCESS);

  ret = mca_pcm.pcm_proc_startup();
  assert(ret == LAM_SUCCESS);

  ret = mca_pcm.pcm_proc_get_peers(&procs, &nprocs);
  assert(ret == LAM_SUCCESS);

  job = mca_pcm.pcm_handle_get();
  assert(job != NULL);

  me = mca_pcm.pcm_proc_get_me();
  assert(me != NULL);

  /* time to play the ring game! */
  me_vpid = me->vpid;
  printf("Hello, World.  I am vpid %d\n", me_vpid);

  left_vpid = me_vpid == 0 ? nprocs - 1 : me_vpid - 1;
  right_vpid = (me_vpid + 1) % nprocs;

  if (me_vpid == 0) {
    printf("vpid %d sending to vpid %d\n", me_vpid, right_vpid);
    ret = mca_oob.oob_send(job, right_vpid, 0, &data, sizeof(int));
    assert(ret == LAM_SUCCESS);
    count--;
  }

  while (count > 0) {
    int *data_ptr;
    size_t data_ptr_len; 
    printf("vpid %d recving from vpid %d\n", me_vpid, left_vpid);
    ret = mca_oob.oob_recv(job, left_vpid, &tag, &data_ptr, &data_ptr_len);
    assert(ret == LAM_SUCCESS);
    assert(data_ptr_len == sizeof(int));
    assert(*data_ptr == data);

    printf("vpid %d sending to vpid %d\n", me_vpid, right_vpid);
    ret = mca_oob.oob_send(job, right_vpid, 0, &data, sizeof(int));
    assert(ret == LAM_SUCCESS);

    count--;
  }


  if (me_vpid == 0) {
    int *data_ptr;
    size_t data_ptr_len;
    printf("vpid %d recving from vpid %d\n", me_vpid, left_vpid);
    ret = mca_oob.oob_recv(job, left_vpid, &tag, &data_ptr, &data_ptr_len);
    assert(ret == LAM_SUCCESS);
    assert(data_ptr_len == sizeof(int));
    assert(*data_ptr == data);
  }

  ret = lam_rte_finalize();
  assert(ret == LAM_SUCCESS);

  return 0;
}
