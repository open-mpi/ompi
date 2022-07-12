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
#include <assert.h>

#include "mpi.h"
#include "mpi-ext.h"

//#define OMPI_HAVE_MPI_EXT_CONTINUE

#ifdef OMPI_HAVE_MPI_EXT_CONTINUE

static int complete_cnt_cb(int status, void *user_data) {
  assert(user_data != NULL);
  assert(status == MPI_SUCCESS);
  printf("complete_cnt_cb \n");
  int *cb_cnt = (int*)user_data;
  *cb_cnt = *cb_cnt + 1;
  return MPI_SUCCESS;
}

int main(int argc, char *argv[])
{
  MPI_Request cont_req, cont_req2, reqs[2];
  int cb_cnt;
  int val;
  int rank, size;
  MPI_Init(&argc, &argv);

  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  /* initialize the continuation request */
  MPIX_Continue_init(0, 0, &cont_req, MPI_INFO_NULL);

  MPI_Start(&cont_req);

  /**
   * One send, one recv, one continuation
   */
  MPI_Irecv(&val, 1, MPI_INT, rank, 1001, MPI_COMM_WORLD, &reqs[0]);
  MPI_Isend(&val, 1, MPI_INT, rank, 1001, MPI_COMM_WORLD, &reqs[1]);

  //MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE);

  cb_cnt = 0;
  MPIX_Continueall(2, reqs, &complete_cnt_cb, &cb_cnt, 0, MPI_STATUSES_IGNORE, cont_req);
  assert(reqs[0] == MPI_REQUEST_NULL && reqs[1] == MPI_REQUEST_NULL);
  MPI_Wait(&cont_req, MPI_STATUS_IGNORE);
  assert(cb_cnt == 1);

  /**
   * One send, one recv, two continuations
   */
  cb_cnt = 0;
  MPI_Irecv(&val, 1, MPI_INT, rank, 1001, MPI_COMM_WORLD, &reqs[0]);
  MPIX_Continue(&reqs[0], &complete_cnt_cb, &cb_cnt, 0, MPI_STATUS_IGNORE, cont_req);

  MPI_Isend(&val, 1, MPI_INT, rank, 1001, MPI_COMM_WORLD, &reqs[1]);
  MPIX_Continue(&reqs[1], &complete_cnt_cb, &cb_cnt, 0, MPI_STATUS_IGNORE, cont_req);

  /* deferred start */
  MPI_Start(&cont_req);

  MPI_Wait(&cont_req, MPI_STATUS_IGNORE);
  assert(reqs[0] == MPI_REQUEST_NULL && reqs[1] == MPI_REQUEST_NULL);
  assert(cb_cnt == 2);

  MPI_Start(&cont_req);

  /**
   * One send, one recv, two continuations in two continuation requests
   */
  MPI_Info info;
  MPI_Info_create(&info);
  MPI_Info_set(info, "mpi_continue_poll_only", "true");
  MPI_Info_set(info, "mpi_continue_enqueue_complete", "true");

  /* initialize a poll-only continuation request */
  MPIX_Continue_init(0, 0, &cont_req2, info);

  MPI_Start(&cont_req2);

  cb_cnt = 0;
  MPI_Irecv(&val, 1, MPI_INT, rank, 1001, MPI_COMM_WORLD, &reqs[0]);
  MPIX_Continue(&reqs[0], &complete_cnt_cb, &cb_cnt, 0, MPI_STATUS_IGNORE, cont_req);

  MPI_Isend(&val, 1, MPI_INT, rank, 1001, MPI_COMM_WORLD, &reqs[1]);
  MPIX_Continue(&reqs[1], &complete_cnt_cb, &cb_cnt, 0, MPI_STATUS_IGNORE, cont_req2);

  MPI_Wait(&cont_req, MPI_STATUS_IGNORE);
  assert(reqs[0] == MPI_REQUEST_NULL && reqs[1] == MPI_REQUEST_NULL);
  assert(cb_cnt == 1);

  printf("Waiting for poll-only cont request %p to complete\n", cont_req2);
  MPI_Wait(&cont_req2, MPI_STATUS_IGNORE);
  assert(cb_cnt == 2);

  MPI_Request_free(&cont_req);
  MPI_Request_free(&cont_req2);
  MPI_Finalize();

  return 0;
}
#else
int main(int argc, char *argv[])
{
    return 77;
}
#endif /* HAVE_MEMKIND_H */
