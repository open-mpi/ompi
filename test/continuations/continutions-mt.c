/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Los Alamos National Security, LLC.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>
#include <pthread.h>

#include "mpi.h"
#include "mpi-ext.h"
//#include "ompi/mpiext/continue/c/mpiext_continue_c.h"

//#define OMPI_HAVE_MPI_EXT_CONTINUE

#ifdef OMPI_HAVE_MPI_EXT_CONTINUE

/* Block a thread on a receive until we release it from the main thread */
static void* thread_recv(void* data) {
  MPI_Request req;
  int val;
  int rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Irecv(&val, 1, MPI_INT, rank, 1002, MPI_COMM_WORLD, &req);
  MPI_Wait(&req, MPI_STATUS_IGNORE);
  return NULL;
}

static int complete_cnt_cb(int status, void *user_data) {
  assert(user_data != NULL);
  assert(MPI_SUCCESS == status);
  _Atomic int *cb_cnt = (_Atomic int*)user_data;
  ++(*cb_cnt);
  return MPI_SUCCESS;
}

int main(int argc, char *argv[])
{
  MPI_Request cont_req, reqs[2];
  _Atomic int cb_cnt;
  int val;
  int rank, size;
  int provided;
  MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
  assert(provided == MPI_THREAD_MULTIPLE);

  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  pthread_t thread;

  pthread_create(&thread, NULL, &thread_recv, NULL);

  /* give enough slack to allow the thread to enter the wait
   * from now on the thread is stuck in MPI_Wait, owning progress
   */
  sleep(2);

  /* initialize the continuation request */
  MPIX_Continue_init(0, 0, &cont_req, MPI_INFO_NULL);

  MPI_Start(&cont_req);

  /**
   * One send, one recv, one continuation
   */
  MPI_Irecv(&val, 1, MPI_INT, rank, 1001, MPI_COMM_WORLD, &reqs[0]);
  MPI_Isend(&val, 1, MPI_INT, rank, 1001, MPI_COMM_WORLD, &reqs[1]);

  cb_cnt = 0;
  MPIX_Continueall(2, reqs, &complete_cnt_cb, &cb_cnt, 0, MPI_STATUSES_IGNORE, cont_req);
  MPI_Wait(&cont_req, MPI_STATUS_IGNORE);
  assert(reqs[0] == MPI_REQUEST_NULL && reqs[1] == MPI_REQUEST_NULL);
  assert(cb_cnt == 1);

  MPI_Start(&cont_req);

  /**
   * One send, one recv, two continuations
   */
  cb_cnt = 0;
  MPI_Irecv(&val, 1, MPI_INT, rank, 1001, MPI_COMM_WORLD, &reqs[0]);
  MPIX_Continue(&reqs[0], &complete_cnt_cb, &cb_cnt, 0, MPI_STATUS_IGNORE, cont_req);

  MPI_Isend(&val, 1, MPI_INT, rank, 1001, MPI_COMM_WORLD, &reqs[1]);
  MPIX_Continue(&reqs[1], &complete_cnt_cb, &cb_cnt, 0, MPI_STATUS_IGNORE, cont_req);

  MPI_Wait(&cont_req, MPI_STATUS_IGNORE);
  assert(cb_cnt == 2);
  assert(reqs[0] == MPI_REQUEST_NULL && reqs[1] == MPI_REQUEST_NULL);

  MPI_Request_free(&cont_req);

  /****************************************************************
   * Do the same thing, but with a poll-only continuation request
   ****************************************************************/

  MPI_Info info;
  MPI_Info_create(&info);
  MPI_Info_set(info, "mpi_continue_poll_only", "true");
  MPI_Info_set(info, "mpi_continue_enqueue_complete", "true");

  /* initialize the continuation request */
  MPIX_Continue_init(0, 0, &cont_req, info);

  MPI_Info_free(&info);

  MPI_Start(&cont_req);

  /**
   * One send, one recv, one continuation
   */
  MPI_Irecv(&val, 1, MPI_INT, rank, 1001, MPI_COMM_WORLD, &reqs[0]);
  MPI_Isend(&val, 1, MPI_INT, rank, 1001, MPI_COMM_WORLD, &reqs[1]);

  cb_cnt = 0;
  MPIX_Continueall(2, reqs, &complete_cnt_cb, &cb_cnt, 0, MPI_STATUSES_IGNORE, cont_req);
  MPI_Wait(&cont_req, MPI_STATUS_IGNORE);
  assert(reqs[0] == MPI_REQUEST_NULL && reqs[1] == MPI_REQUEST_NULL);
  assert(cb_cnt == 1);

  MPI_Start(&cont_req);

  /**
   * One send, one recv, two continuations
   */
  cb_cnt = 0;
  MPI_Irecv(&val, 1, MPI_INT, rank, 1001, MPI_COMM_WORLD, &reqs[0]);
  MPIX_Continue(&reqs[0], &complete_cnt_cb, &cb_cnt, 0, MPI_STATUS_IGNORE, cont_req);

  MPI_Isend(&val, 1, MPI_INT, rank, 1001, MPI_COMM_WORLD, &reqs[1]);
  MPIX_Continue(&reqs[1], &complete_cnt_cb, &cb_cnt, 0, MPI_STATUS_IGNORE, cont_req);

  MPI_Wait(&cont_req, MPI_STATUS_IGNORE);
  assert(reqs[0] == MPI_REQUEST_NULL && reqs[1] == MPI_REQUEST_NULL);
  assert(cb_cnt == 2);

  MPI_Request_free(&cont_req);

  /* release the blocked thread */
  MPI_Send(&val, 1, MPI_INT, rank, 1002, MPI_COMM_WORLD);
  pthread_join(thread, NULL);

  MPI_Finalize();

  return 0;
}
#else
int main(int argc, char *argv[])
{
    return 77;
}
#endif /* HAVE_MEMKIND_H */
