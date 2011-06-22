/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2011, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_MPICOM_H
#define _VT_MPICOM_H

#ifdef __cplusplus
# define EXTERN extern "C" 
#else
# define EXTERN extern 
#endif

#include "config.h"

#include "vt_inttypes.h"

#include "mpi.h"

EXTERN void     vt_comm_init(void);
EXTERN void     vt_comm_finalize(void);

EXTERN void     vt_group_to_bitvector(MPI_Group group);

EXTERN void     vt_group_create(MPI_Group group);
EXTERN void     vt_group_free(MPI_Group group);
EXTERN uint32_t vt_group_id(MPI_Group group);
EXTERN uint32_t vt_group_search(MPI_Group group);

EXTERN void     vt_comm_create(MPI_Comm comm);
EXTERN void     vt_comm_free(MPI_Comm comm);
EXTERN uint32_t vt_comm_id(MPI_Comm comm);

EXTERN uint32_t vt_rank_to_pe(VT_MPI_INT rank, MPI_Comm comm);

#if defined(HAVE_MPI2_1SIDED) && HAVE_MPI2_1SIDED
EXTERN void     vt_win_create(MPI_Win win, MPI_Comm comm);
EXTERN void     vt_win_free(MPI_Win win);
EXTERN void     vt_win_id(MPI_Win win, MPI_Comm* comm, uint32_t* gid, uint32_t* wid);
EXTERN void     vt_win_set_gid(MPI_Win win, uint32_t gid);
#endif /* HAVE_MPI2_1SIDED */

/* MPI communicator |-> VampirTrace communicator id */
#define VT_COMM_ID(c) vt_comm_id(c)

/* Rank with respect to arbitrary communicator |-> global rank */
#define VT_RANK_TO_PE(r,c) (((c)==MPI_COMM_WORLD) ? (uint32_t)r : vt_rank_to_pe(r,c))

#endif
