/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2014-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2015-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "pnbc_osc_internal.h"

static inline int pnbc_osc_alltoallv_init(const void* sendbuf, const int *sendcounts, const int *sdispls,
                              MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *rdispls,
                              MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info,
                              ompi_request_t ** request, struct mca_coll_base_module_2_3_0_t *module, bool persistent);

int ompi_coll_libpnbc_osc_alltoallv_init(const void* sendbuf, const int *sendcounts, const int *sdispls,
                        MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *rdispls,
                        MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info,
                        ompi_request_t ** request, struct mca_coll_base_module_2_3_0_t *module) {

    int res = pnbc_osc_alltoallv_init(sendbuf, sendcounts, sdispls, sendtype,
                                      recvbuf, recvcounts, rdispls, recvtype,
                                      comm, info, request, module, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    return OMPI_SUCCESS;
}

static inline int a2av_sched_linear(int crank, int csize, PNBC_OSC_Schedule *schedule,
                                    void *flags, int *fsize,
                                    const void *sendbuf, const int *sendcounts,
                                    const int *sdispls, MPI_Aint sndext, MPI_Datatype sendtype,
                                          void *recvbuf, const int *recvcounts,
                                    const int *rdispls, MPI_Aint rcvext, MPI_Datatype recvtype,
                                    MPI_Aint *abs_rdispls_other);

static int pnbc_osc_alltoallv_init(const void* sendbuf, const int *sendcounts, const int *sdispls,
                  MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *rdispls,
                  MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info,
                  ompi_request_t ** request, struct mca_coll_base_module_2_3_0_t *module, bool persistent)
{
  int res;
  char inplace;
  MPI_Aint sendext, recvext;
  PNBC_OSC_Schedule *schedule;
  int crank, csize;
  MPI_Aint base_recvbuf, abs_recvbuf;
  MPI_Aint *abs_rdispls_other, *abs_rdispls_local;
  MPI_Win win, winflag;
  void *flags = NULL;
  int fsize = 0;
  ompi_coll_libpnbc_osc_module_t *libpnbc_osc_module = (ompi_coll_libpnbc_osc_module_t*) module;

  PNBC_OSC_IN_PLACE(sendbuf, recvbuf, inplace);
  if (inplace) {
    PNBC_OSC_Error("Error: inplace not implemented for PNBC_OSC AlltoallV");
    return OMPI_ERROR;
  }

  res = ompi_datatype_type_extent (recvtype, &recvext);
  if (MPI_SUCCESS != res) {
    PNBC_OSC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  res = ompi_datatype_type_extent (sendtype, &sendext);
  if (MPI_SUCCESS != res) {
    PNBC_OSC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  schedule = OBJ_NEW(PNBC_OSC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  crank = ompi_comm_rank(comm);
  csize = ompi_comm_size(comm);

  // ******************************
  // PUT-BASED WINDOW SETUP - BEGIN
  // ******************************

  // compute absolute displacement as MPI_AINT for the recvbuf pointer
  res = MPI_Get_address(recvbuf, &base_recvbuf);
  if (OMPI_SUCCESS != res) {
    PNBC_OSC_Error ("MPI Error in MPI_Get_address (%i)", res);
    return res;
  }
  abs_recvbuf = MPI_Aint_add(MPI_BOTTOM, base_recvbuf);

  // create an array of displacements where all ranks will gather their window memory base address
  abs_rdispls_other = (MPI_Aint*)malloc(csize * sizeof(MPI_Aint));
  if (OPAL_UNLIKELY(NULL == abs_rdispls_other)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }
  abs_rdispls_local = (MPI_Aint*)malloc(csize * sizeof(MPI_Aint));
  if (OPAL_UNLIKELY(NULL == abs_rdispls_local)) {
    free(abs_rdispls_other);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  // create a dynamic window - data will be received here
  res = ompi_win_create_dynamic(&info->super, comm, &win);
  if (OMPI_SUCCESS != res) {
    PNBC_OSC_Error ("MPI Error in win_create_dynamic (%i)", res);
    free(abs_rdispls_other);
    free(abs_rdispls_local);
    return res;
  }

  // attach all pieces of local recvbuf to local window and record their absolute displacements
  for (int r=0;r<csize;++r) {
    res = win->w_osc_module->osc_win_attach(win, (char*)recvbuf+rdispls[r], recvext*recvcounts[r]);
    if (OMPI_SUCCESS != res) {
      PNBC_OSC_Error ("MPI Error in win_create_dynamic (%i)", res);
      free(abs_rdispls_other);
      free(abs_rdispls_local);
      MPI_Win_free(&win);
      return res;
    }
    PNBC_OSC_DEBUG(1, "[pnbc_alltoallv_init] %d attaches to dynamic window memory for rank %d with address %p (computed from rdispl value %d) of size %d bytes (computed from recvcount value %d)\n",
                   crank, r,
                   (char*)recvbuf+rdispls[r],
                   rdispls[r],
                   recvext*recvcounts[r],
                   recvcounts[r]);

    // compute displacement of local window memory portion
    abs_rdispls_local[r] = MPI_Aint_add(abs_recvbuf, (MPI_Aint)rdispls[r]);
    PNBC_OSC_DEBUG(1, "[nbc_allreduce_init] %d gets address at disp %ld\n",
                   crank, abs_rdispls_local[r]);
  }

  // swap local rdispls for remote rdispls
  // put the displacements for all local portions on the window
  // get the displacements for all other portions on the window
  res = comm->c_coll->coll_alltoall(abs_rdispls_local, csize, MPI_AINT,
                                    abs_rdispls_other, csize, MPI_AINT,
                                    comm, comm->c_coll->coll_alltoall_module);
  if (OMPI_SUCCESS != res) {
    PNBC_OSC_Error ("MPI Error in alltoall for rdispls (%i)", res);
    free(abs_rdispls_other);
    free(abs_rdispls_local);
    MPI_Win_free(&win);
    return res;
  }

  // the local absolute displacement values for portions of recvbuf are only needed remotely
  free(abs_rdispls_local);

  // ****************************
  // PUT_BASED WINDOW SETUP - END
  // ****************************

  res = a2av_sched_linear(crank, csize, schedule, flags, &fsize,
                          sendbuf, sendcounts, sdispls, sendext, sendtype,
                          recvbuf, recvcounts, rdispls, recvext, recvtype,
                          abs_rdispls_other);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_OSC_Error ("MPI Error in a2av_sched_linear (%i)", res);
    free(abs_rdispls_other);
    free(abs_rdispls_local);
    MPI_Win_free(&win);
    OBJ_RELEASE(schedule);
    return res;
  }

  res = PNBC_OSC_Sched_commit(schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_OSC_Error ("MPI Error in PNBC_OSC_Sched_commit (%i)", res);
    free(abs_rdispls_other);
    free(abs_rdispls_local);
    MPI_Win_free(&win);
    OBJ_RELEASE(schedule);
    return res;
  }

  // create a dynamic window - flags will be signalled here
  res = ompi_win_create_dynamic(&info->super, comm, &winflag);
  if (OMPI_SUCCESS != res) {
    PNBC_OSC_Error ("MPI Error in win_create_dynamic (%i)", res);
    free(abs_rdispls_other);
    free(abs_rdispls_local);
    MPI_Win_free(&win);
    OBJ_RELEASE(schedule);
    return res;
  }

  // attach the flags memory to the winflag window (fsize provided by the schedule)
  res = win->w_osc_module->osc_win_attach(winflag, flags, fsize);
  if (OMPI_SUCCESS != res) {
    PNBC_OSC_Error ("MPI Error in win_create_dynamic (%i)", res);
    free(abs_rdispls_other);
    free(abs_rdispls_local);
    MPI_Win_free(&win);
    OBJ_RELEASE(schedule);
    MPI_Win_free(&winflag);
    return res;
  }

  res = PNBC_OSC_Schedule_request_win(schedule, comm, win, winflag, libpnbc_osc_module, persistent, request, flags);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_OSC_Error ("MPI Error in PNBC_OSC_Schedule_request_win (%i)", res);
    free(abs_rdispls_other);
    free(abs_rdispls_local);
    MPI_Win_free(&win);
    OBJ_RELEASE(schedule);
    MPI_Win_free(&winflag);
    return res;
  }

  return OMPI_SUCCESS;
}

static inline int a2av_sched_linear(int crank, int csize, PNBC_OSC_Schedule *schedule,
                                    void *flags, int *fsize,
                                    const void *sendbuf, const int *sendcounts, const int *sdispls,
                                    MPI_Aint sndext, MPI_Datatype sendtype,
                                          void *recvbuf, const int *recvcounts, const int *rdispls,
                                    MPI_Aint rcvext, MPI_Datatype recvtype,
                                    MPI_Aint *abs_rdispls_other) {
  int res;
  char *rbuf, *sbuf;

  // TODO create and setup flags - set fsize to size of memory allocated for flags

  // schedule a copy for the local MPI process, if needed
  if (sendcounts[crank] != 0) {
    sbuf = (char *) sendbuf + sdispls[crank] * sndext;
    rbuf = (char *) recvbuf + rdispls[crank] * rcvext;
    res = PNBC_OSC_Sched_copy(sbuf, false, sendcounts[crank], sendtype,
                              rbuf, false, recvcounts[crank], recvtype, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }
  }

  // schedule a put for each remote target, if needed
  for (int dist = 1 ; dist < csize ; ++dist) {
    // start from dist==1 to exclude local rank, local is a copy not a put
    int orank = (crank + dist) % csize;
    if (sendcounts[orank] != 0) {
      sbuf = (char *) sendbuf + sdispls[orank] * sndext;

      res = PNBC_OSC_Sched_put(sbuf, orank,
                               sendcounts[orank], sendtype,
                               recvcounts[orank], recvtype,
                               abs_rdispls_other[orank],
                               schedule, false);

      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }

    }
  }

  return OMPI_SUCCESS;
}

