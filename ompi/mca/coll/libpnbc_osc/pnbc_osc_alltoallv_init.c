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
 * Copyright (c) 2020      EPCC, The University of Edinburgh. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Author(s): Daniel Holmes  EPCC, The University of Edinburgh
 *
 */
#include "pnbc_osc_internal.h"
#include "pnbc_osc_action_put.h"

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

// pull implies move means get and FLAG means RTS (ready to send)
static inline int a2av_sched_trigger_pull(int crank, int csize, PNBC_OSC_Schedule *schedule,
//                                          FLAG_t **flags, int *fsize, int *req_count,
                                          const void *sendbuf, const int *sendcounts, const int *sdispls,
                                          MPI_Aint sendext, MPI_Datatype sendtype,
                                                void *recvbuf, const int *recvcounts, const int *rdispls,
                                          MPI_Aint recvext, MPI_Datatype recvtype,
                                          MPI_Aint *abs_sdispls_other);

// push implies move means put and FLAG means CTS (clear to send)
static inline int a2av_sched_trigger_push(int crank, int csize, PNBC_OSC_Schedule *schedule,
                                          void *flags, int *fsize, int *req_count,
                                          const void *sendbuf, const int *sendcounts, const int *sdispls,
                                          MPI_Aint sendext, MPI_Datatype sendtype,
                                                void *recvbuf, const int *recvcounts, const int *rdispls,
                                          MPI_Aint recvext, MPI_Datatype recvtype,
                                          MPI_Aint *abs_rdispls_other);

static inline int a2av_sched_linear_rget(int crank, int csize, PNBC_OSC_Schedule *schedule,
                                         void *flags, int *fsize, int *req_count,
                                         const void *sendbuf, const int *sendcounts, const int *sdispls,
                                         MPI_Aint sendext, MPI_Datatype sendtype,
                                               void *recvbuf, const int *recvcounts, const int *rdispls,
                                         MPI_Aint recvext, MPI_Datatype recvtype,
                                         MPI_Aint *abs_sdispls_other);

static inline int a2av_sched_linear_rput(int crank, int csize, PNBC_OSC_Schedule *schedule,
                                         void *flags, int *fsize, int *req_count,
                                         const void *sendbuf, const int *sendcounts, const int *sdispls,
                                         MPI_Aint sendext, MPI_Datatype sendtype,
                                               void *recvbuf, const int *recvcounts, const int *rdispls,
                                         MPI_Aint recvext, MPI_Datatype recvtype,
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
  int req_count;
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

  res = MPI_Win_lock_all(MPI_MODE_NOCHECK, win);
  if (OMPI_SUCCESS != res) {
    PNBC_OSC_Error ("MPI Error in MPI_Win_lock_all (%i)", res);
    free(abs_rdispls_other);
    free(abs_rdispls_local);
    MPI_Win_free(&win);
    return res;
  }

  // ****************************
  // PUT_BASED WINDOW SETUP - END
  // ****************************

  res = a2av_sched_linear_rput(crank, csize, schedule, flags, &fsize, &req_count,
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

  if (0 == fsize) {
    winflag = MPI_WIN_NULL;
  } else {
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

    // lock the flags window at all other processes
    res = MPI_Win_lock_all(MPI_MODE_NOCHECK, winflag);
    if (OMPI_SUCCESS != res) {
      PNBC_OSC_Error ("MPI Error in MPI_Win_lock_all for winflag (%i)", res);
      free(abs_rdispls_other);
      free(abs_rdispls_local);
      MPI_Win_free(&win);
      return res;
    }

  }

  res = PNBC_OSC_Schedule_request_win(schedule, comm, win, winflag, req_count, libpnbc_osc_module, persistent, request, flags);
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

static inline int a2av_sched_trigger_pull(int crank, int csize, PNBC_OSC_Schedule *schedule,
//                                          FLAG_t **flags, int *fsize, int *req_count,
                                          const void *sendbuf, const int *sendcounts, const int *sdispls,
                                          MPI_Aint sendext, MPI_Datatype sendtype,
                                                void *recvbuf, const int *recvcounts, const int *rdispls,
                                          MPI_Aint recvext, MPI_Datatype recvtype,
                                          MPI_Aint *abs_sdispls_other) {
  // pull implies move means get and FLAG means RTS (ready to send)
  int res = OMPI_SUCCESS;

  schedule = OBJ_NEW(PNBC_OSC_Schedule);

  schedule->triggers = malloc(6 * csize * sizeof(triggerable_t));
  triggerable_t *triggers_phase0 = &(schedule->triggers[0 * csize * sizeof(triggerable_t)]);
  triggerable_t *triggers_phase1 = &(schedule->triggers[1 * csize * sizeof(triggerable_t)]);
  triggerable_t *triggers_phase2 = &(schedule->triggers[2 * csize * sizeof(triggerable_t)]);
  triggerable_t *triggers_phase3 = &(schedule->triggers[3 * csize * sizeof(triggerable_t)]);
  triggerable_t *triggers_phase4 = &(schedule->triggers[4 * csize * sizeof(triggerable_t)]);
  triggerable_t *triggers_phase5 = &(schedule->triggers[5 * csize * sizeof(triggerable_t)]);

  schedule->flags = malloc(2 * csize * sizeof(FLAG_t));
  FLAG_t *flags_FLAG = &(schedule->flags[0 * csize * sizeof(FLAG_t)]);
  FLAG_t *flags_DONE = &(schedule->flags[1 * csize * sizeof(FLAG_t)]);

  schedule->requests = malloc(3 * csize * sizeof(MPI_Request*));
  MPI_Request **requests_rputFLAG = &(schedule->requests[0 * csize * sizeof(MPI_Request*)]);
  MPI_Request **requests_moveData = &(schedule->requests[1 * csize * sizeof(MPI_Request*)]);
  MPI_Request **requests_rputDONE = &(schedule->requests[2 * csize * sizeof(MPI_Request*)]);

  for (int p=0;p<csize;++p) {
    int orank = (crank+p)%csize;

    triggers_phase0[orank].trigger = &(schedule->triggers_active);
    triggers_phase0[orank].triggered = &triggered_all_bynonzero_int;
    triggers_phase0[orank].action = &action_all_put;
    triggers_phase0[orank].action_cbstate = &action_putargs[orank];

    triggers_phase1[orank].trigger = &flags_FLAG[orank];
    triggers_phase2[orank].trigger = &requests_moveData[orank];
    triggers_phase3[orank].trigger = &requests_rputFLAG[orank];
    triggers_phase4[orank].trigger = &flags_DONE[orank];
    triggers_phase5[orank].trigger = &requests_rputDONE[orank];

  }

  // schedule a copy for the local MPI process, if needed
  if (recvcounts[crank] != 0) {
  }

  return res;
}

static inline int a2av_sched_trigger_push(int crank, int csize, PNBC_OSC_Schedule *schedule,
                                          void *flags, int *fsize, int *req_count,
                                          const void *sendbuf, const int *sendcounts, const int *sdispls,
                                          MPI_Aint sendext, MPI_Datatype sendtype,
                                                void *recvbuf, const int *recvcounts, const int *rdispls,
                                          MPI_Aint recvext, MPI_Datatype recvtype,
                                          MPI_Aint *abs_rdispls_other) {
  // push implies move means put and FLAG means CTS (clear to send)
  int res = OMPI_SUCCESS;

  // schedule a copy for the local MPI process, if needed
  if (sendcounts[crank] != 0) {
  }

  return res;
}

static inline int a2av_sched_linear_rget(int crank, int csize, PNBC_OSC_Schedule *schedule,
                                         void *flags, int *fsize, int *req_count,
                                         const void *sendbuf, const int *sendcounts, const int *sdispls,
                                         MPI_Aint sendext, MPI_Datatype sendtype,
                                               void *recvbuf, const int *recvcounts, const int *rdispls,
                                         MPI_Aint recvext, MPI_Datatype recvtype,
                                         MPI_Aint *abs_sdispls_other) {
  int res;
  char *rbuf, *sbuf;

  // schedule a copy for the local MPI process, if needed
  if (recvcounts[crank] != 0) {
    sbuf = (char *) sendbuf + sdispls[crank] * sendext;
    rbuf = (char *) recvbuf + rdispls[crank] * recvext;
    res = PNBC_OSC_Sched_copy(sbuf, false, sendcounts[crank], sendtype,
                              rbuf, false, recvcounts[crank], recvtype, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }
  }

  *req_count = csize - 1;

  // schedule a get for each remote target, if needed
  for (int dist = 1 ; dist < csize ; ++dist) {
    // start from dist==1 to exclude local rank, local is a copy not a get
    int orank = (crank + dist) % csize;
    if (recvcounts[orank] != 0) {
      rbuf = (char *) recvbuf + rdispls[orank] * recvext;

      res = PNBC_OSC_Sched_rget(rbuf, orank,
                                sendcounts[orank], sendtype,
                                recvcounts[orank], recvtype,
                                abs_sdispls_other[orank],
                                schedule, false);

      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }

    }
  }

  return OMPI_SUCCESS;
}

static inline int a2av_sched_linear_rput(int crank, int csize, PNBC_OSC_Schedule *schedule,
                                         void *flags, int *fsize, int *req_count,
                                         const void *sendbuf, const int *sendcounts, const int *sdispls,
                                         MPI_Aint sendext, MPI_Datatype sendtype,
                                               void *recvbuf, const int *recvcounts, const int *rdispls,
                                         MPI_Aint recvext, MPI_Datatype recvtype,
                                         MPI_Aint *abs_rdispls_other) {
  int res;
  char *rbuf, *sbuf;

  // schedule a copy for the local MPI process, if needed
  if (sendcounts[crank] != 0) {
    sbuf = (char *) sendbuf + sdispls[crank] * sendext;
    rbuf = (char *) recvbuf + rdispls[crank] * recvext;
    res = PNBC_OSC_Sched_copy(sbuf, false, sendcounts[crank], sendtype,
                              rbuf, false, recvcounts[crank], recvtype, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }
  }

  *req_count = csize - 1;

  // schedule a put for each remote target, if needed
  for (int dist = 1 ; dist < csize ; ++dist) {
    // start from dist==1 to exclude local rank, local is a copy not a put
    int orank = (crank + dist) % csize;
    if (sendcounts[orank] != 0) {
      sbuf = (char *) sendbuf + sdispls[orank] * sendext;

      res = PNBC_OSC_Sched_rput(sbuf, orank,
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

