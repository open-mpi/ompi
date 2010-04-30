/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2010, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vt_env.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_memhook.h"
#include "vt_mpicom.h"
#include "vt_mpireg.h"
#include "vt_mpireq.h"
#include "vt_pform.h"
#include "vt_thrd.h"
#include "vt_trc.h"
#include "vt_unimci.h"
#if defined (HAVE_MPI2_IO) && HAVE_MPI2_IO
# include "vt_mpifile.h"
# include "vt_iowrap.h"
#endif /* HAVE_MPI2_IO */

#include "mpi.h"

/* this macro disables tracing before calling PMPI for collective operations
  :TODO: that's only a temporary solution to avoid unsorted timestamps in
         trace buffer */
#define CALL_PMPI_COLLOP(_call, _result) {      \
  uint8_t off = vt_is_trace_on();               \
  if ( off ) vt_trace_off(0, 0);                \
  _result = _call;                              \
  if ( off ) vt_trace_on(0);                    \
}

/* this macro calls PMPI (CALL_PMPI_COLLOP for collective operations) */
#define CALL_PMPI(_call, _result, _collop)      \
  if (_collop) CALL_PMPI_COLLOP(_call, _result) \
  else _result = _call;

/* macros for calling PMPI and do something before and after it
   syntax: CALL_PMPI_#args(call, [arg1, arg2, ...], result,
                           collop, record, time)
   call           = MPI function call
   arg1, arg2,... = arguments of MPI function
   result         = return value of MPI function
   collop         = flag: is MPI function a collective operation? (0/1)
   record         = flag: was previous enter event recorded? (0/1)
   time           = timestamp for additional events/marker */
#define CALL_PMPI_0(_call, _result, _collop, _record, _time)                   \
  VT_UNIMCI_CHECK_PRE(_call, ("", 0, 0), _record, _time);                      \
  CALL_PMPI(P##_call(), _result, _collop);                                     \
  VT_UNIMCI_CHECK_POST(_call, ("", 0, 0), _record, _time);

#define CALL_PMPI_1(_call, _arg1, _result, _collop, _record, _time)            \
  VT_UNIMCI_CHECK_PRE(_call, (_arg1, "", 0, 0), _record, _time);               \
  CALL_PMPI(P##_call(_arg1), _result, _collop);                                \
  VT_UNIMCI_CHECK_POST(_call, (_arg1, "", 0, 0), _record, _time);

#define CALL_PMPI_2(_call, _arg1, _arg2, _result, _collop, _record, _time)     \
  VT_UNIMCI_CHECK_PRE(_call, (_arg1, _arg2, "", 0, 0), _record, _time);        \
  CALL_PMPI(P##_call(_arg1, _arg2), _result, _collop);                         \
  VT_UNIMCI_CHECK_POST(_call, (_arg1, _arg2, "", 0, 0), _record, _time);

#define CALL_PMPI_3(_call, _arg1, _arg2, _arg3, _result, _collop, _record,     \
                    _time)                                                     \
  VT_UNIMCI_CHECK_PRE(_call, (_arg1, _arg2, _arg3, "", 0, 0), _record, _time); \
  CALL_PMPI(P##_call(_arg1, _arg2, _arg3), _result, _collop);                  \
  VT_UNIMCI_CHECK_POST(_call, (_arg1, _arg2, _arg3, "", 0, 0), _record, _time);

#define CALL_PMPI_4(_call, _arg1, _arg2, _arg3, _arg4, _result, _collop,       \
                    _record, _time)                                            \
  VT_UNIMCI_CHECK_PRE(_call, (_arg1, _arg2, _arg3, _arg4, "", 0, 0),           \
                      _record, _time);                                         \
  CALL_PMPI(P##_call(_arg1, _arg2, _arg3, _arg4), _result, _collop);           \
  VT_UNIMCI_CHECK_POST(_call, (_arg1, _arg2, _arg3, _arg4, "", 0, 0),          \
                       _record, _time);

#define CALL_PMPI_5(_call, _arg1, _arg2, _arg3, _arg4, _arg5, _result, _collop,\
                    _record, _time)                                            \
  VT_UNIMCI_CHECK_PRE(_call, (_arg1, _arg2, _arg3, _arg4, _arg5, "", 0, 0),    \
                      _record, _time);                                         \
  CALL_PMPI(P##_call(_arg1, _arg2, _arg3, _arg4, _arg5), _result, _collop);    \
  VT_UNIMCI_CHECK_POST(_call, (_arg1, _arg2, _arg3, _arg4, _arg5, "", 0, 0),   \
                       _record, _time);

#define CALL_PMPI_6(_call, _arg1, _arg2, _arg3, _arg4, _arg5, _arg6, _result,  \
                    _collop, _record, _time)                                   \
  VT_UNIMCI_CHECK_PRE(_call, (_arg1, _arg2, _arg3, _arg4, _arg5, _arg6,        \
                      "", 0, 0), _record, _time);                              \
  CALL_PMPI(P##_call(_arg1, _arg2, _arg3, _arg4, _arg5, _arg6), _result,       \
            _collop);                                                          \
  VT_UNIMCI_CHECK_POST(_call, (_arg1, _arg2, _arg3, _arg4, _arg5, _arg6,       \
                       "", 0, 0), _record, _time);

#define CALL_PMPI_7(_call, _arg1, _arg2, _arg3, _arg4, _arg5, _arg6, _arg7,    \
                    _result, _collop, _record, _time)                          \
  VT_UNIMCI_CHECK_PRE(_call, (_arg1, _arg2, _arg3, _arg4, _arg5, _arg6, _arg7, \
                      "", 0, 0), _record, _time);                              \
  CALL_PMPI(P##_call(_arg1, _arg2, _arg3, _arg4, _arg5, _arg6, _arg7), _result,\
           _collop);                                                           \
  VT_UNIMCI_CHECK_POST(_call, (_arg1, _arg2, _arg3, _arg4, _arg5, _arg6, _arg7,\
                       "", 0, 0), _record, _time);

#define CALL_PMPI_8(_call, _arg1, _arg2, _arg3, _arg4, _arg5, _arg6, _arg7,    \
                    _arg8, _result, _collop, _record, _time)                   \
  VT_UNIMCI_CHECK_PRE(_call, (_arg1, _arg2, _arg3, _arg4, _arg5, _arg6, _arg7, \
                      _arg8, "", 0, 0), _record, _time);                       \
  CALL_PMPI(P##_call(_arg1, _arg2, _arg3, _arg4, _arg5, _arg6, _arg7, _arg8),  \
            _result, _collop);                                                 \
  VT_UNIMCI_CHECK_POST(_call, (_arg1, _arg2, _arg3, _arg4, _arg5, _arg6, _arg7,\
                       _arg8, "", 0, 0), _record, _time);

#define CALL_PMPI_9(_call, _arg1, _arg2, _arg3, _arg4, _arg5, _arg6, _arg7,    \
                    _arg8, _arg9, _result, _collop, _record, _time)            \
  VT_UNIMCI_CHECK_PRE(_call, (_arg1, _arg2, _arg3, _arg4, _arg5, _arg6, _arg7, \
                      _arg8, _arg9, "", 0, 0), _record, _time);                \
  CALL_PMPI(P##_call(_arg1, _arg2, _arg3, _arg4, _arg5, _arg6, _arg7, _arg8,   \
            _arg9), _result, _collop);                                         \
  VT_UNIMCI_CHECK_POST(_call, (_arg1, _arg2, _arg3, _arg4, _arg5, _arg6, _arg7,\
                       _arg8, _arg9, "", 0, 0), _record, _time);

#define CALL_PMPI_12(_call, _arg1, _arg2, _arg3, _arg4, _arg5, _arg6, _arg7,   \
                     _arg8, _arg9, _arg10, _arg11, _arg12, _result, _collop,   \
                     _record, _time)                                           \
  VT_UNIMCI_CHECK_PRE(_call, (_arg1, _arg2, _arg3, _arg4, _arg5, _arg6, _arg7, \
                      _arg8, _arg9, _arg10, _arg11, _arg12, "", 0, 0),         \
                      _record, _time);                                         \
  CALL_PMPI(P##_call(_arg1, _arg2, _arg3, _arg4, _arg5, _arg6, _arg7, _arg8,   \
            _arg9, _arg10, _arg11, _arg12), _result, _collop);                 \
  VT_UNIMCI_CHECK_POST(_call, (_arg1, _arg2, _arg3, _arg4, _arg5, _arg6, _arg7,\
                       _arg8, _arg9, _arg10, _arg11, _arg12, "", 0, 0),        \
                       _record, _time);

static MPI_Status *my_status_array = 0;
static VT_MPI_INT my_status_array_size = 0;

static uint8_t is_rma_putre = 1;

static MPI_Status* vt_get_status_array(VT_MPI_INT size) {
  if (my_status_array_size == 0) {
    /* -- never used: initialize -- */
    my_status_array = (MPI_Status*)malloc(size * sizeof(MPI_Status));
    if ( my_status_array == NULL ) vt_error();
    my_status_array_size = size;
  } else if (size > my_status_array_size) {
    /* -- not enough room: expand -- */
    my_status_array = (MPI_Status*)realloc(my_status_array, size * sizeof(MPI_Status));
    if ( my_status_array == NULL ) vt_error();
    my_status_array_size = size;
  }
  return my_status_array;
}


/*
 *-----------------------------------------------------------------------------
 *
 * Init and finalize
 *
 *-----------------------------------------------------------------------------
 */

/* dummy function 'user' entered */
uint8_t vt_enter_user_called = 0;

/* initialized once from environment variable */
uint8_t vt_mpitrace = 1;

/* changed with every MPI_TRACE_ON/MPI_TRACE_OFF */
uint8_t vt_mpi_trace_is_on = 1;

#define IS_MPI_TRACE_ON ( vt_is_alive && vt_mpi_trace_is_on )
#define MPI_TRACE_OFF() \
  VT_MEMHOOKS_OFF(); \
  vt_mpi_trace_is_on = 0;
#define MPI_TRACE_ON() \
  VT_MEMHOOKS_ON(); \
  vt_mpi_trace_is_on = vt_mpitrace;

/* -- MPI_Init -- */

VT_MPI_INT MPI_Init( VT_MPI_INT* argc, char*** argv )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  /* shall I trace MPI events? */
  vt_mpi_trace_is_on = vt_mpitrace = vt_env_mpitrace();

  /* first event?
     -> initialize VT and enter dummy function 'user' */
  if ( !vt_is_alive )
    {
      vt_open();
      time = vt_pform_wtime();
      vt_enter_user(&time);
      vt_enter_user_called = 1;
    }

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_INIT]);

      CALL_PMPI_2(MPI_Init, argc, argv, result, 0, was_recorded, &time);

      /* initialize mpi event handling */
      vt_mpi_init();

      /* initialize communicator management */
      vt_comm_init();

      /* initialize file management */
#if defined (HAVE_MPI2_IO) && HAVE_MPI2_IO
      vt_mpifile_init();
#endif /* HAVE_MPI2_IO */

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_2(MPI_Init, argc, argv, result, 0, 0, NULL);

      /* initialize mpi event handling */
      vt_mpi_init();

      /* initialize communicator management */
      vt_comm_init();

      /* initialize file management */
#if defined (HAVE_MPI2_IO) && HAVE_MPI2_IO
      vt_mpifile_init();
#endif /* HAVE_MPI2_IO */
    }

  return result;
}

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD

/* -- MPI_Init_thread -- */

VT_MPI_INT MPI_Init_thread( VT_MPI_INT* argc, char*** argv,
                            VT_MPI_INT required, VT_MPI_INT* provided )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  /* shall I trace MPI events? */
  vt_mpi_trace_is_on = vt_mpitrace = vt_env_mpitrace();

  /* first event?
     -> initialize VT and enter dummy function 'user' */
  if ( !vt_is_alive )
    {
      vt_open();
      time = vt_pform_wtime();
      vt_enter_user(&time);
      vt_enter_user_called = 1;
    }

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_INIT_THREAD]);

      CALL_PMPI_4(MPI_Init_thread, argc, argv, required, provided, result,
                  0, was_recorded, &time);

      switch (required)
      {
        case MPI_THREAD_SINGLE:
        case MPI_THREAD_FUNNELED:
          break;
        case MPI_THREAD_SERIALIZED:
        case MPI_THREAD_MULTIPLE:
          if (*provided == MPI_THREAD_SERIALIZED ||
              *provided == MPI_THREAD_MULTIPLE)
          {
            vt_error_msg("MPI thread support levels MPI_THREAD_SERIALIZED and "
                         "MPI_THREAD_MULTIPLE not yet supported");
          }
          break;
        default:
          vt_error_msg("Unknown level of MPI thread support required");
          break;
      }

      /* initialize mpi event handling */
      vt_mpi_init();

      /* initialize communicator management */
      vt_comm_init();

      /* initialize file management */
#if defined (HAVE_MPI2_IO) && HAVE_MPI2_IO
      vt_mpifile_init();
#endif /* HAVE_MPI2_IO */

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_4(MPI_Init_thread, argc, argv, required, provided, result,
                  0, 0, NULL);

      /* initialize mpi event handling */
      vt_mpi_init();

      /* initialize communicator management */
      vt_comm_init();

      /* initialize file management */
#if defined (HAVE_MPI2_IO) && HAVE_MPI2_IO
      vt_mpifile_init();
#endif /* HAVE_MPI2_IO */
    }

  return result;
}

#endif /* HAVE_MPI2_THREAD */

/* -- MPI_Finalize -- */

VT_MPI_INT MPI_Finalize( void )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_FINALIZE]);

      /* finalize communicator, request, and file management */
      vt_comm_finalize();
      vt_request_finalize();
#if defined (HAVE_MPI2_IO) && HAVE_MPI2_IO
      vt_mpifile_finalize();
#endif /* HAVE_MPI2_IO */

      /* finalize mpi event handling */
      vt_mpi_finalize();

      CALL_PMPI_0(MPI_Finalize, result, 0, was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      /* finalize communicator, request, and file management */
      vt_comm_finalize();
      vt_request_finalize();
#if defined (HAVE_MPI2_IO) && HAVE_MPI2_IO
      vt_mpifile_finalize();
#endif /* HAVE_MPI2_IO */

      /* finalize mpi event handling */
      vt_mpi_finalize();

      CALL_PMPI_0(MPI_Finalize, result, 0, 0, NULL);
    }

  /* exit dummy function 'user', if necessary */
  if (vt_enter_user_called)
    {
      time = vt_pform_wtime();
      vt_exit_user(&time);
    }

  return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Communicator management
 *
 *-----------------------------------------------------------------------------
 */

/* ------- Constructors ------- */

/* -- MPI_Comm_dup -- */

VT_MPI_INT MPI_Comm_dup( MPI_Comm comm,
                         MPI_Comm* newcomm )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_COMM_DUP]);

      CALL_PMPI_2(MPI_Comm_dup, comm, newcomm, result, 0, was_recorded, &time);

      vt_comm_create(*newcomm);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_2(MPI_Comm_dup, comm, newcomm, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Comm_create -- */

VT_MPI_INT MPI_Comm_create( MPI_Comm comm,
                            MPI_Group group,
                            MPI_Comm* newcomm )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_COMM_CREATE]);

      CALL_PMPI_3(MPI_Comm_create, comm, group, newcomm, result,
                  0, was_recorded, &time);

      if ( *newcomm != MPI_COMM_NULL)
        vt_comm_create(*newcomm);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_3(MPI_Comm_create, comm, group, newcomm, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_Comm_split -- */

VT_MPI_INT MPI_Comm_split( MPI_Comm comm,
                           VT_MPI_INT color,
                           VT_MPI_INT key,
                           MPI_Comm* newcomm )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_COMM_SPLIT]);

      CALL_PMPI_4(MPI_Comm_split, comm, color, key, newcomm, result,
                  0, was_recorded, &time);

      if ( *newcomm != MPI_COMM_NULL)
        vt_comm_create(*newcomm);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_4(MPI_Comm_split, comm, color, key, newcomm, result,
                  0, 0, NULL);
    }

  return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Group management
 *
 *-----------------------------------------------------------------------------
 */

/* -- MPI_Group_union -- */

VT_MPI_INT MPI_Group_union( MPI_Group group1,
                            MPI_Group group2,
                            MPI_Group* newgroup )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();
      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_GROUP_UNION]);

      CALL_PMPI_3(MPI_Group_union, group1, group2, newgroup, result, 0, 
                  was_recorded, &time);
      if ( *newgroup != MPI_GROUP_NULL)
        vt_group_create(*newgroup);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_3(MPI_Group_union, group1, group2, newgroup, result, 0,
                  0, NULL);
    }

  return result;
}

/* -- MPI_Group_intersection -- */

VT_MPI_INT MPI_Group_intersection( MPI_Group group1,
                                   MPI_Group group2,
                                   MPI_Group* newgroup )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();
      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_GROUP_INTERSECTION]);

      CALL_PMPI_3(MPI_Group_intersection, group1, group2, newgroup, result, 0,
                  was_recorded, &time);
      if ( *newgroup != MPI_GROUP_NULL)
        vt_group_create(*newgroup);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_3(MPI_Group_intersection, group1, group2, newgroup, result, 0,
                  0, NULL);
    }

  return result;
}

/* -- MPI_Group_difference -- */

VT_MPI_INT MPI_Group_difference( MPI_Group group1,
                                 MPI_Group group2,
                                 MPI_Group* newgroup )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();
      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_GROUP_INCL]);

      CALL_PMPI_3(MPI_Group_difference, group1, group2, newgroup, result, 0,
                  was_recorded, &time);

      if ( *newgroup != MPI_GROUP_NULL)
        vt_group_create(*newgroup);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_3(MPI_Group_difference, group1, group2, newgroup, result, 0,
                  0, NULL);
    }

  return result;
}

/* -- MPI_Group_incl -- */

VT_MPI_INT MPI_Group_incl( MPI_Group group,
                           int n,
                           int* ranks,
                           MPI_Group* newgroup )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();
      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_GROUP_INCL]);

      CALL_PMPI_4(MPI_Group_incl, group, n, ranks, newgroup, result, 0,
                  was_recorded, &time);
      result = PMPI_Group_incl(group, n, ranks, newgroup);
      if ( *newgroup != MPI_GROUP_NULL)
        vt_group_create(*newgroup);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_4(MPI_Group_incl, group, n, ranks, newgroup, result, 0,
                  0, NULL);
    }

  return result;
}

/* -- MPI_Group_excl -- */

VT_MPI_INT MPI_Group_excl( MPI_Group group,
                           int n,
                           int* ranks,
                           MPI_Group* newgroup )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();
      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_GROUP_EXCL]);

      CALL_PMPI_4(MPI_Group_excl, group, n, ranks, newgroup, result, 0,
                  was_recorded, &time);
      if ( *newgroup != MPI_GROUP_NULL)
        vt_group_create(*newgroup);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_4(MPI_Group_excl, group, n, ranks, newgroup, result, 0,
                  0, NULL);
    }

  return result;
}

/* -- MPI_Group_range_incl -- */

VT_MPI_INT MPI_Group_range_incl( MPI_Group group,
                                 int n,
                                 int ranges[][3],
                                 MPI_Group* newgroup )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();
      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_GROUP_RANGE_INCL]);

      CALL_PMPI_4(MPI_Group_range_incl, group, n, ranges, newgroup, result, 0,
                  was_recorded, &time);
      if ( *newgroup != MPI_GROUP_NULL)
        vt_group_create(*newgroup);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_4(MPI_Group_range_incl, group, n, ranges, newgroup, result, 0,
                  0, NULL);
    }

  return result;
}

/* -- MPI_Group_range_excl --*/

VT_MPI_INT MPI_Group_range_excl( MPI_Group group,
                                 int n,
                                 int ranges[][3],
                                 MPI_Group* newgroup )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();
      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_GROUP_RANGE_EXCL]);

      CALL_PMPI_4(MPI_Group_range_excl, group, n, ranges, newgroup, result, 0,
                  was_recorded, &time);
      if ( *newgroup != MPI_GROUP_NULL)
        vt_group_create(*newgroup);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_4(MPI_Group_range_excl, group, n, ranges, newgroup, result, 0,
                  0, NULL);
    }

  return result;
}

/* -- MPI_Group_free -- */

VT_MPI_INT MPI_Group_free( MPI_Group* group )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();
      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_GROUP_FREE]);

      vt_group_free(*group);
      CALL_PMPI_1(MPI_Group_free, group, result, 0, was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_1(MPI_Group_free, group, result, 0, 0, NULL);
    }

  return result;
}

#if defined(HAVE_MPI2_1SIDED) && HAVE_MPI2_1SIDED

/*
 *-----------------------------------------------------------------------------
 *
 * Window management
 *
 *-----------------------------------------------------------------------------
 */

/* ------- Constructor ------- */

/* -- MPI_Win_create -- */

VT_MPI_INT MPI_Win_create( void* base,
                           MPI_Aint size,
                           VT_MPI_INT disp_unit,
                           MPI_Info info,
                           MPI_Comm comm, 
                           MPI_Win* win )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_WIN_CREATE]);

      CALL_PMPI_6(MPI_Win_create, base, size, disp_unit, info, comm, win,
                  result, 0, was_recorded, &time);

      /* register window */
      if ( *win != MPI_WIN_NULL)
        vt_win_create(*win, comm);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_6(MPI_Win_create, base, size, disp_unit, info, comm, win,
                  result, 0, 0, NULL);
    }

  return result;
}

/* ------- Destructor ------- */

/* -- MPI_Win_free -- */

VT_MPI_INT MPI_Win_free ( MPI_Win* win )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_WIN_FREE]);

      vt_win_free(*win);
      CALL_PMPI_1(MPI_Win_free, win, result, 0, was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(&time);
  
      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_1(MPI_Win_free, win, result, 0, 0, NULL);
    }
  
  return result;
}

#endif /* HAVE_MPI2_1SIDED */

/*
 *-----------------------------------------------------------------------------
 *
 * Cartesian Toplogy functions
 *
 *-----------------------------------------------------------------------------
 */

/* -- MPI_Cart_create -- */

VT_MPI_INT MPI_Cart_create( MPI_Comm comm_old,
                            VT_MPI_INT ndims,
                            VT_MPI_INT* dims,
                            VT_MPI_INT* periodv,
                            VT_MPI_INT reorder,
                            MPI_Comm* comm_cart )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_CART_CREATE]);

      CALL_PMPI_6(MPI_Cart_create, comm_old, ndims, dims, periodv, reorder,
                  comm_cart, result, 0, was_recorded, &time);

      if ( *comm_cart != MPI_COMM_NULL)
	vt_comm_create(*comm_cart);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_6(MPI_Cart_create, comm_old, ndims, dims, periodv, reorder,
                  comm_cart, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Cart_sub -- */

VT_MPI_INT MPI_Cart_sub( MPI_Comm comm,
                         VT_MPI_INT* rem_dims,
                         MPI_Comm* newcomm )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_CART_SUB]);

      CALL_PMPI_3(MPI_Cart_sub, comm, rem_dims, newcomm, result,
                  0, was_recorded, &time);

      if ( *newcomm != MPI_COMM_NULL)
        vt_comm_create(*newcomm);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_3(MPI_Cart_sub, comm, rem_dims, newcomm, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_Graph_create -- */

VT_MPI_INT MPI_Graph_create( MPI_Comm comm_old,
                             VT_MPI_INT nnodes,
                             VT_MPI_INT* index,
                             VT_MPI_INT* edges,
                             VT_MPI_INT reorder,
                             MPI_Comm* comm_graph )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_GRAPH_CREATE]);

      CALL_PMPI_6(MPI_Graph_create, comm_old, nnodes, index, edges, reorder,
                  comm_graph, result, 0, was_recorded, &time);

      if ( *comm_graph != MPI_COMM_NULL)
        vt_comm_create(*comm_graph);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_6(MPI_Graph_create, comm_old, nnodes, index, edges, reorder,
                  comm_graph, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Intercomm_create -- */

VT_MPI_INT MPI_Intercomm_create( MPI_Comm local_comm,
                                 VT_MPI_INT local_leader,
                                 MPI_Comm peer_comm,
                                 VT_MPI_INT remote_leader,
                                 VT_MPI_INT tag,
                                 MPI_Comm* newintercomm )

{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_INTERCOMM_CREATE]);

      CALL_PMPI_6(MPI_Intercomm_create, local_comm, local_leader, peer_comm,
                  remote_leader, tag, newintercomm, result,
                  0, was_recorded, &time);

      if ( *newintercomm != MPI_COMM_NULL)
        vt_comm_create(*newintercomm);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_6(MPI_Intercomm_create, local_comm, local_leader, peer_comm,
                  remote_leader, tag, newintercomm, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_Intercomm_merge -- */

VT_MPI_INT MPI_Intercomm_merge( MPI_Comm intercomm,
                                VT_MPI_INT high,
                                MPI_Comm* newcomm )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_INTERCOMM_MERGE]);

      CALL_PMPI_3(MPI_Intercomm_merge, intercomm, high, newcomm, result,
                  0, was_recorded, &time);

      if ( *newcomm != MPI_COMM_NULL)
        vt_comm_create(*newcomm);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_3(MPI_Intercomm_merge, intercomm, high, newcomm, result,
                  0, 0, NULL);
    }

  return result;
}


/* ------- Destructors ------- */

/* -- MPI_Comm_free -- */

VT_MPI_INT MPI_Comm_free( MPI_Comm* comm )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_COMM_FREE]);

      vt_comm_free(*comm);

      CALL_PMPI_1(MPI_Comm_free, comm, result, 0, was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_1(MPI_Comm_free, comm, result, 0, 0, NULL);
    }

  return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Point-to-point communication
 *
 *-----------------------------------------------------------------------------
 */

/* ------- Synchronous ------- */


/* -- MPI_Send -- */

VT_MPI_INT MPI_Send( void* buf,
                     VT_MPI_INT count,
                     MPI_Datatype datatype,
                     VT_MPI_INT dest,
                     VT_MPI_INT tag,
                     MPI_Comm comm )
{
  VT_MPI_INT result, sz;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_SEND]);

      if ( (dest != MPI_PROC_NULL) && (was_recorded) )
        {
          PMPI_Type_size(datatype, &sz);
          vt_mpi_send(&time, VT_RANK_TO_PE(dest, comm), VT_COMM_ID(comm),
                      tag, count * sz);
        }

      CALL_PMPI_6(MPI_Send, buf, count, datatype, dest, tag, comm, result,
                  0, was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_6(MPI_Send, buf, count, datatype, dest, tag, comm, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_Bsend -- */

VT_MPI_INT MPI_Bsend( void* buf,
                      VT_MPI_INT count,
                      MPI_Datatype datatype,
                      VT_MPI_INT dest,
                      VT_MPI_INT tag,
                      MPI_Comm comm )
{
  VT_MPI_INT result, sz;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_BSEND]);

      if ( (dest != MPI_PROC_NULL) && (was_recorded) )
        {
          PMPI_Type_size(datatype, &sz);
          vt_mpi_send(&time, VT_RANK_TO_PE(dest, comm), VT_COMM_ID(comm),
                      tag, count * sz);
        }

      CALL_PMPI_6(MPI_Bsend, buf, count, datatype, dest, tag, comm, result,
                  0, was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_6(MPI_Bsend, buf, count, datatype, dest, tag, comm, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_Rsend -- */

VT_MPI_INT MPI_Rsend( void* buf,
                      VT_MPI_INT count,
                      MPI_Datatype datatype,
                      VT_MPI_INT dest,
                      VT_MPI_INT tag,
                      MPI_Comm comm )
{
  VT_MPI_INT result, sz;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_RSEND]);

      if ( (dest != MPI_PROC_NULL) && (was_recorded) )
        {
          PMPI_Type_size(datatype, &sz);
          vt_mpi_send(&time, VT_RANK_TO_PE(dest, comm), VT_COMM_ID(comm),
                      tag, count * sz);
        }

      CALL_PMPI_6(MPI_Rsend, buf, count, datatype, dest, tag, comm, result,
                  0, was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_6(MPI_Rsend, buf, count, datatype, dest, tag, comm, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_Ssend -- */

VT_MPI_INT MPI_Ssend( void* buf,
                      VT_MPI_INT count,
                      MPI_Datatype datatype,
                      VT_MPI_INT dest,
                      VT_MPI_INT tag,
                      MPI_Comm comm )
{
  VT_MPI_INT result, sz;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_SSEND]);

      if ( (dest != MPI_PROC_NULL) && (was_recorded) )
        {
          PMPI_Type_size(datatype, &sz);
          vt_mpi_send(&time, VT_RANK_TO_PE(dest, comm), VT_COMM_ID(comm),
                      tag, count * sz);
        }

      CALL_PMPI_6(MPI_Ssend, buf, count, datatype, dest, tag, comm, result,
                  0, was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_6(MPI_Ssend, buf, count, datatype, dest, tag, comm, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_Recv -- */

VT_MPI_INT MPI_Recv( void* buf,
                     VT_MPI_INT count,
                     MPI_Datatype datatype,
                     VT_MPI_INT source, VT_MPI_INT tag,
                     MPI_Comm comm,
                     MPI_Status* status )
{
  VT_MPI_INT result, sz;
  MPI_Status mystatus;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_RECV]);

      if (status == MPI_STATUS_IGNORE) status = &mystatus;

      CALL_PMPI_7(MPI_Recv, buf, count, datatype, source, tag, comm, status,
                  result, 0, was_recorded, &time);

      time = vt_pform_wtime();

      if (source != MPI_PROC_NULL && result == MPI_SUCCESS && was_recorded)
        {
          PMPI_Type_size(datatype, &sz);
          PMPI_Get_count(status, datatype, &count);
          if (count == MPI_UNDEFINED)
            count = 0;
          vt_mpi_recv(&time, VT_RANK_TO_PE(status->MPI_SOURCE, comm),
                      VT_COMM_ID(comm), status->MPI_TAG, count * sz);
        }

      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_7(MPI_Recv, buf, count, datatype, source, tag, comm, status,
                  result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Sendrecv -- */

VT_MPI_INT MPI_Sendrecv( void* sendbuf,
                         VT_MPI_INT sendcount,
                         MPI_Datatype sendtype,
                         VT_MPI_INT dest,
                         VT_MPI_INT sendtag,
                         void* recvbuf,
                         VT_MPI_INT recvcount,
                         MPI_Datatype recvtype,
                         VT_MPI_INT source,
                         VT_MPI_INT recvtag,
                         MPI_Comm comm,
                         MPI_Status* status )
{
  VT_MPI_INT result, sendsz, recvsz;
  MPI_Status mystatus;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_SENDRECV]);

      if ( (dest != MPI_PROC_NULL) && (was_recorded) )
        {
          PMPI_Type_size(sendtype, &sendsz);
          vt_mpi_send(&time, VT_RANK_TO_PE(dest, comm), VT_COMM_ID(comm),
                      sendtag, sendcount * sendsz);
        }
      if (status == MPI_STATUS_IGNORE) status = &mystatus;

      CALL_PMPI_12(MPI_Sendrecv, sendbuf, sendcount, sendtype, dest,   sendtag,
                   recvbuf, recvcount, recvtype, source, recvtag, comm, status,
                   result, 0, was_recorded, &time);

      time = vt_pform_wtime();

      if (source != MPI_PROC_NULL && result == MPI_SUCCESS && was_recorded)
        {
          PMPI_Type_size(recvtype, &recvsz);
          PMPI_Get_count(status, recvtype, &recvcount);
          if (recvcount == MPI_UNDEFINED)
            recvcount = 0;
          vt_mpi_recv(&time, VT_RANK_TO_PE(status->MPI_SOURCE, comm),
                      VT_COMM_ID(comm), status->MPI_TAG, recvcount * recvsz);
        }

      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_12(MPI_Sendrecv, sendbuf, sendcount, sendtype, dest,   sendtag,
                   recvbuf, recvcount, recvtype, source, recvtag, comm, status,
                   result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Sendrecv_replace -- */

VT_MPI_INT MPI_Sendrecv_replace( void* buf,
                                 VT_MPI_INT count,
                                 MPI_Datatype datatype,
                                 VT_MPI_INT dest,
                                 VT_MPI_INT sendtag,
                                 VT_MPI_INT source,
                                 VT_MPI_INT recvtag,
                                 MPI_Comm comm,
                                 MPI_Status* status )
{
  VT_MPI_INT result, sz;
  MPI_Status mystatus;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_SENDRECV_REPLACE]);

      PMPI_Type_size(datatype, &sz);
      if ( (dest != MPI_PROC_NULL) && (was_recorded) )
        {
          vt_mpi_send(&time, VT_RANK_TO_PE(dest, comm),
                      VT_COMM_ID(comm),
                      sendtag,
                      count * sz);
        }
      if (status == MPI_STATUS_IGNORE) status = &mystatus;

      CALL_PMPI_9(MPI_Sendrecv_replace, buf, count, datatype, dest, sendtag,
                  source, recvtag, comm, status, result,
                  0, was_recorded, &time);

      time = vt_pform_wtime();

      if (source != MPI_PROC_NULL && result == MPI_SUCCESS)
        {
          vt_mpi_recv(&time, VT_RANK_TO_PE(status->MPI_SOURCE, comm),
                      VT_COMM_ID(comm), status->MPI_TAG, count * sz);
        }

      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_9(MPI_Sendrecv_replace, buf, count, datatype, dest, sendtag,
                  source, recvtag, comm, status, result,
                  0, 0, NULL);
    }

  return result;
}

/* ------- Aynchronous ------- */

/* -- MPI_Isend -- */

VT_MPI_INT MPI_Isend( void* buf,
                      VT_MPI_INT count,
                      MPI_Datatype datatype,
                      VT_MPI_INT dest,
                      VT_MPI_INT tag,
                      MPI_Comm comm,
                      MPI_Request* request )
{
  VT_MPI_INT result, sz;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_ISEND]);

      if ( (dest != MPI_PROC_NULL) && (was_recorded) )
        {
          PMPI_Type_size(datatype, &sz);
          vt_mpi_send(&time, VT_RANK_TO_PE(dest, comm), VT_COMM_ID(comm),
                      tag, count * sz);
        }

      CALL_PMPI_7(MPI_Isend, buf, count, datatype, dest, tag, comm, request,
                  result, 0, was_recorded, &time);

      /* no need to save send request as we already created send event,
       * so why saving request, and then have all kinds of trouble handling
       * it correctly
      if (dest != MPI_PROC_NULL && result == MPI_SUCCESS)
        vt_request_create(*request, ERF_SEND, tag, dest, count*sz, comm);
       */

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_7(MPI_Isend, buf, count, datatype, dest, tag, comm, request,
                  result, 0, 0, NULL);
    }

  return result;
}


/* -- MPI_Irecv -- */

VT_MPI_INT MPI_Irecv( void* buf,
                      VT_MPI_INT count,
                      MPI_Datatype datatype,
                      VT_MPI_INT source,
                      VT_MPI_INT tag,
                      MPI_Comm comm,
                      MPI_Request* request )
{
  VT_MPI_INT result, sz;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_IRECV]);

      PMPI_Type_size(datatype, &sz);

      CALL_PMPI_7(MPI_Irecv, buf, count, datatype, source, tag, comm, request,
                  result, 0, was_recorded, &time);

      if (source != MPI_PROC_NULL && result == MPI_SUCCESS)
        vt_request_create(*request, ERF_RECV,
                          tag, 0, count * sz, datatype, comm);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_7(MPI_Irecv, buf, count, datatype, source, tag, comm, request,
                  result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Ibsend -- */

VT_MPI_INT MPI_Ibsend( void* buf,
                       VT_MPI_INT count,
                       MPI_Datatype datatype,
                       VT_MPI_INT dest,
                       VT_MPI_INT tag,
                       MPI_Comm comm,
                       MPI_Request *request )
{
  VT_MPI_INT result, sz;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_IBSEND]);

      if ( (dest != MPI_PROC_NULL) && (was_recorded) )
        {
          PMPI_Type_size(datatype, &sz);
          vt_mpi_send(&time, VT_RANK_TO_PE(dest, comm), VT_COMM_ID(comm),
                      tag, count * sz);
        }

      CALL_PMPI_7(MPI_Ibsend, buf, count, datatype, dest, tag, comm, request,
                  result, 0, was_recorded, &time);

      /* no need to save send request as we already created send event,
       * so why saving request, and then have all kinds of trouble handling
       * it correctly
      if (dest != MPI_PROC_NULL && result == MPI_SUCCESS)
        vt_request_create(*request, ERF_SEND, tag, dest, count*sz, comm);
       */

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_7(MPI_Ibsend, buf, count, datatype, dest, tag, comm, request,
                  result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Issend -- */

VT_MPI_INT MPI_Issend( void* buf,
                       VT_MPI_INT count,
                       MPI_Datatype datatype,
                       VT_MPI_INT dest,
                       VT_MPI_INT tag,
                       MPI_Comm comm,
                       MPI_Request *request)
{
  VT_MPI_INT result, sz;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_ISSEND]);

      if ( (dest != MPI_PROC_NULL) && (was_recorded) )
        {
          PMPI_Type_size(datatype, &sz);
          vt_mpi_send(&time, VT_RANK_TO_PE(dest, comm), VT_COMM_ID(comm),
                      tag, count * sz);
        }

      CALL_PMPI_7(MPI_Issend, buf, count, datatype, dest, tag, comm, request,
                  result, 0, was_recorded, &time);

      /* no need to save send request as we already created send event,
       * so why saving request, and then have all kinds of trouble handling
       * it correctly
      if (dest != MPI_PROC_NULL && result == MPI_SUCCESS)
        vt_request_create(*request, ERF_SEND, tag, dest, count*sz, comm);
       */

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_7(MPI_Issend, buf, count, datatype, dest, tag, comm, request,
                  result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Irsend -- */

VT_MPI_INT MPI_Irsend( void* buf,
                       VT_MPI_INT count,
                       MPI_Datatype datatype,
                       VT_MPI_INT dest,
                       VT_MPI_INT tag,
                       MPI_Comm comm,
                       MPI_Request* request )
{
  VT_MPI_INT result, sz;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_IRSEND]);

      if ( (dest != MPI_PROC_NULL) && (was_recorded) )
        {
          PMPI_Type_size(datatype, &sz);
          vt_mpi_send(&time, VT_RANK_TO_PE(dest, comm), VT_COMM_ID(comm),
                      tag, count * sz);
        }

      CALL_PMPI_7(MPI_Irsend, buf, count, datatype, dest, tag, comm, request,
                  result, 0, was_recorded, &time);

      /* no need to save send request as we already created send event,
       * so why saving request, and then have all kinds of trouble handling
       * it correctly
      if (dest != MPI_PROC_NULL && result == MPI_SUCCESS)
        vt_request_create(*request, ERF_SEND, tag, dest, count*sz, comm);
       */

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_7(MPI_Irsend, buf, count, datatype, dest, tag, comm, request,
                  result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Wait -- */

VT_MPI_INT MPI_Wait( MPI_Request* request,
                     MPI_Status* status )
{
  VT_MPI_INT result;
  MPI_Status mystatus;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      struct VTRequest* orig_req;

      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_WAIT]);

      if (status == MPI_STATUS_IGNORE) status = &mystatus;
      orig_req = vt_request_get(*request);

      CALL_PMPI_2(MPI_Wait, request, status, result, 0, was_recorded, &time);

      time = vt_pform_wtime();
      vt_check_request(&time, orig_req, status, was_recorded);

      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_2(MPI_Wait, request, status, result, 0, 0, NULL);
    }


  return result;
}

/* -- MPI_Waitall -- */

VT_MPI_INT MPI_Waitall( VT_MPI_INT count,
                        MPI_Request* requests,
                        MPI_Status* array_of_statuses )
{
  VT_MPI_INT result, i;
  struct VTRequest* orig_req;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_WAITALL]);

      if (array_of_statuses == MPI_STATUSES_IGNORE) {
        array_of_statuses = vt_get_status_array(count);
      }
      vt_save_request_array(requests, count);

      CALL_PMPI_3(MPI_Waitall, count, requests, array_of_statuses, result,
                  0, was_recorded, &time);

      time = vt_pform_wtime();

      for (i = 0; i < count; i++)
        {
          orig_req = vt_saved_request_get(i);
          vt_check_request(&time, orig_req, &(array_of_statuses[i]), was_recorded);
        }

      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_3(MPI_Waitall, count, requests, array_of_statuses, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_Waitany -- */

VT_MPI_INT MPI_Waitany( VT_MPI_INT count,
                        MPI_Request* requests,
                        VT_MPI_INT* index,
                        MPI_Status* status )
{
  VT_MPI_INT result;
  struct VTRequest* orig_req;
  MPI_Status mystatus;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_WAITANY]);

      if (status == MPI_STATUS_IGNORE) status = &mystatus;
      vt_save_request_array(requests, count);

      CALL_PMPI_4(MPI_Waitany, count, requests, index, status, result,
                  0, was_recorded, &time);

      orig_req = vt_saved_request_get(*index);

      time = vt_pform_wtime();
      vt_check_request(&time, orig_req, status, was_recorded);

      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_4(MPI_Waitany, count, requests, index, status, result,
                  0, 0, NULL);
    }


  return result;
}

/* -- MPI_Waitsome -- */

VT_MPI_INT MPI_Waitsome( VT_MPI_INT incount,
                         MPI_Request* array_of_requests,
                         VT_MPI_INT* outcount,
                         VT_MPI_INT* array_of_indices,
                         MPI_Status* array_of_statuses )
{
  VT_MPI_INT result, i;
  struct VTRequest* orig_req;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_WAITSOME]);

      if (array_of_statuses == MPI_STATUSES_IGNORE) {
        array_of_statuses = vt_get_status_array(incount);
      }
      vt_save_request_array(array_of_requests, incount);

      CALL_PMPI_5(MPI_Waitsome, incount, array_of_requests, outcount,
                  array_of_indices, array_of_statuses, result,
                  0, was_recorded, &time);

      time = vt_pform_wtime();

      for (i=0; i<*outcount; ++i)
        {
          orig_req = vt_saved_request_get(array_of_indices[i]);
          vt_check_request(&time, orig_req, &(array_of_statuses[i]), was_recorded);
        }

      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_5(MPI_Waitsome, incount, array_of_requests, outcount,
                  array_of_indices, array_of_statuses, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_Test -- */

VT_MPI_INT MPI_Test( MPI_Request* request,
                     VT_MPI_INT* flag,
                     MPI_Status* status )
{
  VT_MPI_INT result;
  struct VTRequest* orig_req;
  MPI_Status mystatus;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_TEST]);

      if (status == MPI_STATUS_IGNORE) status = &mystatus;
      orig_req = vt_request_get(*request);

      CALL_PMPI_3(MPI_Test, request, flag, status, result,
                  0, was_recorded, &time);

      time = vt_pform_wtime();

      if (*flag) vt_check_request(&time, orig_req, status, was_recorded);

      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_3(MPI_Test, request, flag, status, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_Testany -- */

VT_MPI_INT MPI_Testany( VT_MPI_INT count,
                        MPI_Request* array_of_requests,
                        VT_MPI_INT* index,
                        VT_MPI_INT* flag,
                        MPI_Status* status )
{
  VT_MPI_INT result;
  struct VTRequest* orig_req;
  MPI_Status mystatus;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_TESTANY]);

      if (status == MPI_STATUS_IGNORE) status = &mystatus;
      vt_save_request_array(array_of_requests, count);

      CALL_PMPI_5(MPI_Testany, count, array_of_requests, index, flag, status,
                  result, 0, was_recorded, &time);

      time = vt_pform_wtime();

      if (*flag && *index != MPI_UNDEFINED)
        {
          orig_req = vt_saved_request_get(*index);
          vt_check_request(&time, orig_req, status, was_recorded);
        }

      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_5(MPI_Testany, count, array_of_requests, index, flag, status,
                  result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Testall -- */

VT_MPI_INT MPI_Testall( VT_MPI_INT count,
                        MPI_Request* array_of_requests,
                        VT_MPI_INT* flag,
                        MPI_Status* array_of_statuses )
{
  VT_MPI_INT result, i;
  struct VTRequest* orig_req;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_TESTALL]);

      if (array_of_statuses == MPI_STATUSES_IGNORE) {
        array_of_statuses = vt_get_status_array(count);
      }
      vt_save_request_array(array_of_requests, count);

      CALL_PMPI_4(MPI_Testall, count, array_of_requests, flag,
                  array_of_statuses, result, 0, was_recorded, &time);

      time = vt_pform_wtime();

      if (*flag)
        {
	  for (i = 0; i < count; i++)
            {
	      orig_req = vt_saved_request_get(i);
              vt_check_request(&time, orig_req, &(array_of_statuses[i]), was_recorded);
            }
        }

      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_4(MPI_Testall, count, array_of_requests, flag,
                  array_of_statuses, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Testsome -- */

VT_MPI_INT MPI_Testsome( VT_MPI_INT incount,
                         MPI_Request* array_of_requests,
                         VT_MPI_INT* outcount,
                         VT_MPI_INT* array_of_indices,
                         MPI_Status* array_of_statuses )
{
  VT_MPI_INT result, i;
  struct VTRequest* orig_req;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_TESTSOME]);

      if (array_of_statuses == MPI_STATUSES_IGNORE) {
        array_of_statuses = vt_get_status_array(incount);
      }
      vt_save_request_array(array_of_requests, incount);

      CALL_PMPI_5(MPI_Testsome, incount, array_of_requests, outcount,
                  array_of_indices, array_of_statuses, result,
                  0, was_recorded, &time);

      time = vt_pform_wtime();

      for (i=0; i<*outcount; ++i)
        {
          orig_req = vt_saved_request_get(array_of_indices[i]);
          vt_check_request(&time, orig_req, &(array_of_statuses[i]), was_recorded);
        }

      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_5(MPI_Testsome, incount, array_of_requests, outcount,
                  array_of_indices, array_of_statuses, result,
                  0, 0, NULL);
    }

  return result;
}

/* ------- Persistent requests ------- */

/* -- MPI_Send_init -- */

VT_MPI_INT MPI_Send_init( void* buf,
                          VT_MPI_INT count,
                          MPI_Datatype datatype,
                          VT_MPI_INT dest,
                          VT_MPI_INT tag,
                          MPI_Comm comm,
                          MPI_Request* request )
{
  VT_MPI_INT result, sz;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_SEND_INIT]);

      PMPI_Type_size(datatype, &sz);

      CALL_PMPI_7(MPI_Send_init, buf, count, datatype, dest, tag, comm,
                  request, result, 0, was_recorded, &time);

      if (dest != MPI_PROC_NULL && result == MPI_SUCCESS)
        vt_request_create(*request, (ERF_SEND | ERF_IS_PERSISTENT),
                           tag, dest, count*sz, datatype, comm);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_7(MPI_Send_init, buf, count, datatype, dest, tag, comm,
                  request, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Recv_init -- */

VT_MPI_INT MPI_Recv_init( void* buf,
                          VT_MPI_INT count,
                          MPI_Datatype datatype,
                          VT_MPI_INT source,
                          VT_MPI_INT tag,
                          MPI_Comm comm,
                          MPI_Request* request )
{
  VT_MPI_INT result, sz;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_RECV_INIT]);

      PMPI_Type_size(datatype, &sz);

      CALL_PMPI_7(MPI_Recv_init, buf, count, datatype, source, tag, comm,
                  request, result, 0, was_recorded, &time);

      if (source != MPI_PROC_NULL && result == MPI_SUCCESS)
        vt_request_create(*request, (ERF_RECV | ERF_IS_PERSISTENT),
                           tag, source, count * sz, datatype, comm);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_7(MPI_Recv_init, buf, count, datatype, source, tag, comm,
                  request, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Bsend_init -- */

VT_MPI_INT MPI_Bsend_init( void* buf,
                           VT_MPI_INT count,
                           MPI_Datatype datatype,
                           VT_MPI_INT dest,
                           VT_MPI_INT tag,
                           MPI_Comm comm,
                           MPI_Request* request )
{
  VT_MPI_INT result, sz;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_BSEND_INIT]);

      PMPI_Type_size(datatype, &sz);

      CALL_PMPI_7(MPI_Bsend_init, buf, count, datatype, dest, tag, comm,
                  request, result, 0, was_recorded, &time);

      if (dest != MPI_PROC_NULL && result == MPI_SUCCESS)
        vt_request_create(*request, (ERF_SEND | ERF_IS_PERSISTENT),
                           tag, dest, count*sz, datatype, comm);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_7(MPI_Bsend_init, buf, count, datatype, dest, tag, comm,
                  request, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Ssend_init -- */

VT_MPI_INT MPI_Ssend_init( void* buf,
                           VT_MPI_INT count,
                           MPI_Datatype datatype,
                           VT_MPI_INT dest,
                           VT_MPI_INT tag,
                           MPI_Comm comm,
                           MPI_Request* request )
{
  VT_MPI_INT result, sz;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_SSEND_INIT]);

      PMPI_Type_size(datatype, &sz);

      CALL_PMPI_7(MPI_Ssend_init, buf, count, datatype, dest, tag, comm,
                  request, result, 0, was_recorded, &time);

      if (dest != MPI_PROC_NULL && result == MPI_SUCCESS)
        vt_request_create(*request, (ERF_SEND | ERF_IS_PERSISTENT),
                           tag, dest, count*sz, datatype, comm);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_7(MPI_Ssend_init, buf, count, datatype, dest, tag, comm,
                  request, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Rsend_init -- */

VT_MPI_INT MPI_Rsend_init( void* buf,
                           VT_MPI_INT count,
                           MPI_Datatype datatype,
                           VT_MPI_INT dest,
                           VT_MPI_INT tag,
                           MPI_Comm comm,
                           MPI_Request* request )
{
  VT_MPI_INT result, sz;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_RSEND_INIT]);

      PMPI_Type_size(datatype, &sz);

      CALL_PMPI_7(MPI_Rsend_init, buf, count, datatype, dest, tag, comm,
                  request, result, 0, was_recorded, &time);

      if (dest != MPI_PROC_NULL && result == MPI_SUCCESS)
        vt_request_create(*request, (ERF_SEND | ERF_IS_PERSISTENT),
                           tag, dest, count*sz, datatype, comm);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_7(MPI_Rsend_init, buf, count, datatype, dest, tag, comm,
                  request, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Start -- */

VT_MPI_INT MPI_Start( MPI_Request* request )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      struct VTRequest* req;

      MPI_TRACE_OFF();

      time = vt_pform_wtime();

      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_START]);

      req = vt_request_get(*request);
      if (req)
        {
          if (req->flags & ERF_IS_PERSISTENT )
            {
              req->flags |= ERF_IS_ACTIVE;
              if ((req->flags & ERF_SEND) && (req->dest != MPI_PROC_NULL) && (was_recorded))
                vt_mpi_send(&time, VT_RANK_TO_PE(req->dest, req->comm),
                            VT_COMM_ID(req->comm), req->tag,  req->bytes);
            }
        }

      CALL_PMPI_1(MPI_Start, request, result, 0, was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_1(MPI_Start, request, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Startall -- */

VT_MPI_INT MPI_Startall( VT_MPI_INT count,
                         MPI_Request* array_of_requests )
{
  VT_MPI_INT result, i;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      struct VTRequest* req;
      MPI_Request *request;

      MPI_TRACE_OFF();

      time = vt_pform_wtime();

      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_STARTALL]);

      for (i = 0; i < count; i++)
        {
          request=&array_of_requests[i];
          req = vt_request_get(*request);
          if (req)
            {
              if (req->flags & ERF_IS_PERSISTENT )
                {
                  req->flags |= ERF_IS_ACTIVE;
                  if ((req->flags & ERF_SEND) && (req->dest != MPI_PROC_NULL) && (was_recorded))
                    vt_mpi_send(&time, VT_RANK_TO_PE(req->dest, req->comm),
                                VT_COMM_ID(req->comm), req->tag,  req->bytes);
                }
            }
        }

      CALL_PMPI_2(MPI_Startall, count, array_of_requests, result,
                  0, was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
  }
  else
  {
    CALL_PMPI_2(MPI_Startall, count, array_of_requests, result,
                0, 0, NULL);
  }
  return result;
}

/* -- MPI_Request_free -- */

VT_MPI_INT MPI_Request_free( MPI_Request* request )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      struct VTRequest* req;

      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_REQUEST_FREE]);

      req = vt_request_get(*request);
      if (req && (req->flags & ERF_IS_PERSISTENT))
        {
          if (req->flags & ERF_IS_ACTIVE )
            /* mark active requests for deallocation */
            req->flags |= ERF_DEALLOCATE;
          else
            /* deallocate inactive requests -*/
            vt_request_free(req);
        }
      /* -- else non-persistent requests:
       *    + we don't track non-persistent sends
       *    + MPI standard strongly suggests to deallocate non-persistent
       *      recv's only by waot or test
       *    ==> nothing to do here
       */

      CALL_PMPI_1(MPI_Request_free, request, result, 0, was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_1(MPI_Request_free, request, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Cancel -- */

VT_MPI_INT MPI_Cancel( MPI_Request* request )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_CANCEL]);

      /* -- do not really know what to do here ?!?
       *    would need to find out if canceled communcation completed
       *    sucessfully or was canceled sucessfully (probably possible
       *    by using PMPI_Test_cancelled) but whatever we do here,
       *    we end up by an invalid trace as there we cannot remove the
       *    send events already put in the trace buffer, and so the
       *    message matching in the analysis will fail in any case
       */

      CALL_PMPI_1(MPI_Cancel, request, result, 0, was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_1(MPI_Cancel, request, result, 0, 0, NULL);
    }

  return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Collective communication
 *
 *-----------------------------------------------------------------------------
 */

/* -- MPI_Allreduce -- */

VT_MPI_INT MPI_Allreduce ( void* sendbuf,
                           void* recvbuf,
                           VT_MPI_INT count,
                           MPI_Datatype datatype,
                           MPI_Op op,
                           MPI_Comm comm )
{
  VT_MPI_INT result, sz, N;
  uint64_t time, etime;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_ALLREDUCE]);

      CALL_PMPI_6(MPI_Allreduce, sendbuf, recvbuf, count, datatype, op, comm,
                  result, 1, was_recorded, &time);

      if (was_recorded)
        {
          PMPI_Type_size(datatype, &sz);
          PMPI_Comm_size(comm, &N);

          etime = vt_pform_wtime();
          vt_mpi_collexit(&time, &etime,
                          vt_mpi_regid[VT__MPI_ALLREDUCE],
                          VT_NO_ID, VT_COMM_ID(comm), &comm,
                          N*count*sz, count*sz);
        }
      else
        {
          time = vt_pform_wtime();
          vt_exit(&time);
        }

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_6(MPI_Allreduce, sendbuf, recvbuf, count, datatype, op, comm,
                  result, 1, 0, NULL);
    }

  return result;
}

/* -- MPI_Barrier -- */

VT_MPI_INT MPI_Barrier( MPI_Comm comm )
{
  VT_MPI_INT result;
  uint64_t time, etime;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_BARRIER]);

      CALL_PMPI_1(MPI_Barrier, comm, result, 1, was_recorded, &time);

      if (was_recorded)
        {
          etime = vt_pform_wtime();
          vt_mpi_collexit(&time, &etime,
                          vt_mpi_regid[VT__MPI_BARRIER],
                          VT_NO_ID, VT_COMM_ID(comm), &comm,
                          0, 0);
        }
      else
        {
          time = vt_pform_wtime();
          vt_exit(&time);
        }

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_1(MPI_Barrier, comm, result, 1, 0, NULL);
    }

  return result;
}

/* -- MPI_Bcast -- */

VT_MPI_INT MPI_Bcast( void* buf,
                      VT_MPI_INT count,
                      MPI_Datatype datatype,
                      VT_MPI_INT root,
                      MPI_Comm comm )
{
  VT_MPI_INT result, sz, N, isroot, me;
  uint64_t time, etime;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_BCAST]);

      CALL_PMPI_5(MPI_Bcast, buf, count, datatype, root, comm, result,
                  1, was_recorded, &time);

      if (was_recorded)
        {
          PMPI_Type_size(datatype, &sz);
          PMPI_Comm_rank(comm, &me);
          isroot = ( me == root );
          if ( isroot )
            PMPI_Comm_size(comm, &N);
          else
            N = 1;

          etime = vt_pform_wtime();
          vt_mpi_collexit(&time, &etime,
                          vt_mpi_regid[VT__MPI_BCAST],
                          VT_RANK_TO_PE(root, comm), VT_COMM_ID(comm), &comm,
                          (N-1)*count*sz, (!isroot)*count*sz);
        }
      else
        {
          time = vt_pform_wtime();
          vt_exit(&time);
        }

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_5(MPI_Bcast, buf, count, datatype, root, comm, result,
                  1, 0, NULL);
    }

  return result;
}

/* -- MPI_Gather -- */

VT_MPI_INT MPI_Gather( void* sendbuf,
                       VT_MPI_INT sendcount,
                       MPI_Datatype sendtype,
                       void* recvbuf,
                       VT_MPI_INT recvcount,
                       MPI_Datatype recvtype,
                       VT_MPI_INT root,
                       MPI_Comm comm )
{
  VT_MPI_INT result, ssz, rsz, N, me;
  uint64_t time, etime;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_GATHER]);

      CALL_PMPI_8(MPI_Gather, sendbuf, sendcount, sendtype, recvbuf, recvcount,
                  recvtype, root, comm, result, 1, was_recorded, &time);

      if (was_recorded)
        {
          PMPI_Type_size(sendtype, &ssz);
          PMPI_Comm_rank(comm, &me);
          if ( me == root ) {
            PMPI_Comm_size(comm, &N);
            PMPI_Type_size(recvtype, &rsz);
          } else {
            N = rsz = 0;
          }

          etime = vt_pform_wtime();
          vt_mpi_collexit(&time, &etime,
                          vt_mpi_regid[VT__MPI_GATHER],
                          VT_RANK_TO_PE(root, comm), VT_COMM_ID(comm), &comm,
                          sendcount*ssz, N*recvcount*rsz);
        }
      else
        {
          time = vt_pform_wtime();
          vt_exit(&time);
        }

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_8(MPI_Gather, sendbuf, sendcount, sendtype, recvbuf, recvcount,
                  recvtype, root, comm, result, 1, 0, NULL);
    }

  return result;
}

/* -- MPI_Reduce -- */

VT_MPI_INT MPI_Reduce( void* sendbuf,
                       void* recvbuf,
                       VT_MPI_INT count,
                       MPI_Datatype datatype,
                       MPI_Op op,
                       VT_MPI_INT root,
                       MPI_Comm comm )
{
  VT_MPI_INT result, sz, isroot, me;
  uint64_t time, etime;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_REDUCE]);

      CALL_PMPI_7(MPI_Reduce, sendbuf, recvbuf, count, datatype, op, root,
                  comm, result, 1, was_recorded, &time);

      if (was_recorded)
        {
          PMPI_Type_size(datatype, &sz);
          PMPI_Comm_rank(comm, &me);
          isroot = ( me == root );

          etime = vt_pform_wtime();
          vt_mpi_collexit(&time, &etime,
                          vt_mpi_regid[VT__MPI_REDUCE],
                          VT_RANK_TO_PE(root, comm), VT_COMM_ID(comm), &comm,
                          count*sz, isroot*count*sz);
        }
      else
        {
          time = vt_pform_wtime();
          vt_exit(&time);
        }

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_7(MPI_Reduce, sendbuf, recvbuf, count, datatype, op, root,
                  comm, result, 1, 0, NULL);
    }

  return result;
}

/* -- MPI_Gatherv -- */

VT_MPI_INT MPI_Gatherv( void* sendbuf,
                        VT_MPI_INT sendcount,
                        MPI_Datatype sendtype,
                        void* recvbuf,
                        VT_MPI_INT *recvcounts,
                        VT_MPI_INT *displs,
                        MPI_Datatype recvtype,
                        VT_MPI_INT root,
                        MPI_Comm comm )
{
  VT_MPI_INT result, recvsz, sendsz, recvcount, me, N, i;
  uint64_t time, etime;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_GATHERV]);

      CALL_PMPI_9(MPI_Gatherv, sendbuf, sendcount, sendtype, recvbuf, recvcounts,
                  displs, recvtype, root, comm, result, 1, was_recorded, &time);

      if (was_recorded)
        {
          PMPI_Type_size(recvtype, &recvsz);
          PMPI_Type_size(sendtype, &sendsz);
          PMPI_Comm_rank(comm, &me);

          recvcount = recvsz = 0;
          if ( me == root ) {
            PMPI_Comm_size(comm, &N);
            PMPI_Type_size(recvtype, &recvsz);
            for(i = 0; i<N; i++) recvcount += recvcounts[i];
          }

          etime = vt_pform_wtime();
          vt_mpi_collexit(&time, &etime,
                          vt_mpi_regid[VT__MPI_GATHERV],
                          VT_RANK_TO_PE(root, comm), VT_COMM_ID(comm), &comm,
                          sendcount * sendsz, recvcount * recvsz);
        }
      else
        {
          time = vt_pform_wtime();
          vt_exit(&time);
        }

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_9(MPI_Gatherv, sendbuf, sendcount, sendtype, recvbuf, recvcounts,
                  displs, recvtype, root, comm, result, 1, 0, NULL);
    }

  return result;
}

/* -- MPI_Allgather -- */

VT_MPI_INT MPI_Allgather( void* sendbuf,
                          VT_MPI_INT sendcount,
                          MPI_Datatype sendtype,
                          void* recvbuf,
                          VT_MPI_INT recvcount,
                          MPI_Datatype recvtype,
                          MPI_Comm comm )
{
  VT_MPI_INT result, recvsz, sendsz, N;
  uint64_t time, etime;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_ALLGATHER]);

      CALL_PMPI_7(MPI_Allgather, sendbuf, sendcount, sendtype, recvbuf,
                  recvcount, recvtype, comm, result, 1, was_recorded, &time);

      if (was_recorded)
        {
          PMPI_Type_size(recvtype, &recvsz);
          PMPI_Type_size(sendtype, &sendsz);
          PMPI_Comm_size(comm, &N);

          etime = vt_pform_wtime();
          vt_mpi_collexit(&time, &etime,
                          vt_mpi_regid[VT__MPI_ALLGATHER],
                          VT_NO_ID, VT_COMM_ID(comm), &comm,
                          N * sendcount * sendsz, N * recvcount * recvsz);
        }
      else
        {
          time = vt_pform_wtime();
          vt_exit(&time);
        }

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_7(MPI_Allgather, sendbuf, sendcount, sendtype, recvbuf,
                  recvcount, recvtype, comm, result, 1, 0, NULL);
    }

  return result;
}

/* -- MPI_Allgatherv -- */

VT_MPI_INT MPI_Allgatherv( void* sendbuf,
                           VT_MPI_INT sendcount,
                           MPI_Datatype sendtype,
                           void* recvbuf,
                           VT_MPI_INT *recvcounts,
                           VT_MPI_INT *displs,
                           MPI_Datatype recvtype,
                           MPI_Comm comm )
{
  VT_MPI_INT result, recvcount, recvsz, sendsz, i, N;
  uint64_t time, etime;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_ALLGATHERV]);

      CALL_PMPI_8(MPI_Allgatherv, sendbuf, sendcount, sendtype, recvbuf,
                  recvcounts, displs, recvtype, comm, result,
                  1, was_recorded, &time);

      if (was_recorded)
        {
          PMPI_Type_size(recvtype, &recvsz);
          PMPI_Type_size(sendtype, &sendsz);
          PMPI_Comm_size(comm, &N);
          recvcount = 0;
          for(i = 0; i<N; i++) recvcount += recvcounts[i];

          etime = vt_pform_wtime();
          vt_mpi_collexit(&time, &etime,
                          vt_mpi_regid[VT__MPI_ALLGATHERV],
                          VT_NO_ID, VT_COMM_ID(comm), &comm,
                          N * sendcount * sendsz, recvcount * recvsz);
        }
      else
        {
          time = vt_pform_wtime();
          vt_exit(&time);
        }

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_8(MPI_Allgatherv, sendbuf, sendcount, sendtype, recvbuf,
                  recvcounts, displs, recvtype, comm, result,
                  1, 0, NULL);
    }

  return result;
}

/* -- MPI_Alltoall -- */

VT_MPI_INT MPI_Alltoall( void* sendbuf,
                         VT_MPI_INT sendcount,
                         MPI_Datatype sendtype,
                         void* recvbuf,
                         VT_MPI_INT recvcount,
                         MPI_Datatype recvtype,
                         MPI_Comm comm )
{
  VT_MPI_INT result, recvsz, sendsz, N;
  uint64_t time, etime;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_ALLTOALL]);

      CALL_PMPI_7(MPI_Alltoall, sendbuf, sendcount, sendtype, recvbuf,
                  recvcount, recvtype, comm, result, 1, was_recorded, &time);

      if (was_recorded)
        {
          PMPI_Type_size(recvtype, &recvsz);
          PMPI_Type_size(sendtype, &sendsz);
          PMPI_Comm_size(comm, &N);

          etime = vt_pform_wtime();
          vt_mpi_collexit(&time, &etime,
                          vt_mpi_regid[VT__MPI_ALLTOALL],
                          VT_NO_ID, VT_COMM_ID(comm), &comm,
                          sendsz * sendcount * N, recvsz * recvcount * N);
        }
      else
        {
          time = vt_pform_wtime();
          vt_exit(&time);
        }

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_7(MPI_Alltoall, sendbuf, sendcount, sendtype, recvbuf,
                  recvcount, recvtype, comm, result, 1, 0, NULL);
    }

  return result;
}

/* -- MPI_Alltoallv -- */

VT_MPI_INT MPI_Alltoallv( void* sendbuf,
                          VT_MPI_INT* sendcounts,
                          VT_MPI_INT* sdispls,
                          MPI_Datatype sendtype,
                          void* recvbuf,
                          VT_MPI_INT* recvcounts,
                          VT_MPI_INT* rdispls,
                          MPI_Datatype recvtype,
                          MPI_Comm comm )
{
  VT_MPI_INT result, recvcount=0, sendcount=0, recvsz, sendsz, N, i;
  uint64_t time, etime;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_ALLTOALLV]);

      CALL_PMPI_9(MPI_Alltoallv, sendbuf, sendcounts, sdispls, sendtype,
                  recvbuf, recvcounts, rdispls, recvtype, comm, result,
                  1, was_recorded, &time);

      if (was_recorded)
        {
          PMPI_Type_size(recvtype, &recvsz);
          PMPI_Type_size(sendtype, &sendsz);
          PMPI_Comm_size(comm, &N);
          for(i = 0; i<N; i++)
            {
              recvcount += recvcounts[i];
              sendcount += sendcounts[i];
            }

          etime = vt_pform_wtime();
          vt_mpi_collexit(&time, &etime,
                          vt_mpi_regid[VT__MPI_ALLTOALLV],
                          VT_NO_ID, VT_COMM_ID(comm), &comm,
                          sendsz * sendcount, recvsz * recvcount);
        }
      else
        {
          time = vt_pform_wtime();
          vt_exit(&time);
        }

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_9(MPI_Alltoallv, sendbuf, sendcounts, sdispls, sendtype,
                  recvbuf, recvcounts, rdispls, recvtype, comm, result,
                  1, 0, NULL);
    }

  return result;
}

/* -- MPI_Scan -- */

VT_MPI_INT MPI_Scan( void* sendbuf,
                     void* recvbuf,
                     VT_MPI_INT count,
                     MPI_Datatype datatype,
                     MPI_Op op,
                     MPI_Comm comm )
{
  VT_MPI_INT result, size, me, N;
  uint64_t time, etime;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_SCAN]);

      CALL_PMPI_6(MPI_Scan, sendbuf, recvbuf, count, datatype, op, comm,
                  result, 1, was_recorded, &time);

      if (was_recorded)
        {
          PMPI_Type_size(datatype, &size);
          PMPI_Comm_rank(comm, &me);
          PMPI_Comm_size(comm, &N);

          etime = vt_pform_wtime();
          vt_mpi_collexit(&time, &etime,
                          vt_mpi_regid[VT__MPI_SCAN],
                          VT_NO_ID, VT_COMM_ID(comm), &comm,
                          (N-me) * size * count, size * count);
        }
      else
        {
          time = vt_pform_wtime();
          vt_exit(&time);
        }

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_6(MPI_Scan, sendbuf, recvbuf, count, datatype, op, comm,
                  result, 1, 0, NULL);
    }

  return result;
}

/* -- MPI_Scatter -- */

VT_MPI_INT MPI_Scatter( void* sendbuf,
                        VT_MPI_INT sendcount,
                        MPI_Datatype sendtype,
                        void* recvbuf,
                        VT_MPI_INT recvcount,
                        MPI_Datatype recvtype,
                        VT_MPI_INT root,
                        MPI_Comm comm )
{
  VT_MPI_INT result, sendsz, recvsz, N, me;
  uint64_t time, etime;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_SCATTER]);

      CALL_PMPI_8(MPI_Scatter, sendbuf, sendcount, sendtype, recvbuf,
                  recvcount, recvtype, root, comm, result,
                  1, was_recorded, &time);

      if (was_recorded)
        {
          PMPI_Type_size(recvtype, &recvsz);
          PMPI_Comm_rank(comm, &me);
          if ( me == root ) {
            PMPI_Comm_size(comm, &N);
            PMPI_Type_size(sendtype, &sendsz);
          } else {
            N = sendsz = 0;
          }

          etime = vt_pform_wtime();
          vt_mpi_collexit(&time, &etime,
                          vt_mpi_regid[VT__MPI_SCATTER],
                          VT_RANK_TO_PE(root, comm), VT_COMM_ID(comm), &comm,
                          N * sendcount * sendsz, recvcount * recvsz);
        }
      else
        {
          time = vt_pform_wtime();
          vt_exit(&time);
        }

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_8(MPI_Scatter, sendbuf, sendcount, sendtype, recvbuf,
                  recvcount, recvtype, root, comm, result,
                  1, 0, NULL);
    }

  return result;
}


/* -- MPI_Scatterv -- */

VT_MPI_INT MPI_Scatterv( void* sendbuf,
                         VT_MPI_INT* sendcounts,
                         VT_MPI_INT* displs,
                         MPI_Datatype sendtype,
                         void* recvbuf,
                         VT_MPI_INT recvcount,
                         MPI_Datatype recvtype,
                         VT_MPI_INT root,
                         MPI_Comm comm )
{
  VT_MPI_INT result, sendcount, recvsz, sendsz, me, N, i;
  uint64_t time, etime;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_SCATTERV]);

      CALL_PMPI_9(MPI_Scatterv, sendbuf, sendcounts, displs, sendtype, recvbuf,
                  recvcount, recvtype, root, comm, result,
                  1, was_recorded, &time);

      if (was_recorded)
        {
          sendcount = sendsz = 0;
          PMPI_Type_size(recvtype, &recvsz);
          PMPI_Comm_rank(comm, &me);
          if ( me == root ) {
            PMPI_Comm_size(comm, &N);
            PMPI_Type_size(sendtype, &sendsz);
            for(i = 0; i<N; i++) sendcount += sendcounts[i];
          }

          etime = vt_pform_wtime();
          vt_mpi_collexit(&time, &etime,
                          vt_mpi_regid[VT__MPI_SCATTERV],
                          VT_RANK_TO_PE(root, comm), VT_COMM_ID(comm), &comm,
                          sendcount * sendsz, recvcount * recvsz);
        }
      else
        {
          time = vt_pform_wtime();
          vt_exit(&time);
        }

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_9(MPI_Scatterv, sendbuf, sendcounts, displs, sendtype, recvbuf,
                  recvcount, recvtype, root, comm, result,
                  1, 0, NULL);
    }

  return result;
}

/* -- MPI_Reduce_scatter -- */

VT_MPI_INT MPI_Reduce_scatter( void* sendbuf,
                               void* recvbuf,
                               VT_MPI_INT* recvcounts,
                               MPI_Datatype datatype,
                               MPI_Op op,
                               MPI_Comm comm )
{
  VT_MPI_INT result, i, size, N, count = 0;
  uint64_t time, etime;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_REDUCE_SCATTER]);

      CALL_PMPI_6(MPI_Reduce_scatter, sendbuf, recvbuf, recvcounts, datatype,
                  op, comm, result, 1, was_recorded, &time);

      if (was_recorded)
        {
          PMPI_Type_size(datatype, &size);
          PMPI_Comm_size(comm, &N);
          for(i = 0; i<N; i++) count += recvcounts[i];

          etime = vt_pform_wtime();
          vt_mpi_collexit(&time, &etime,
                          vt_mpi_regid[VT__MPI_REDUCE_SCATTER],
                          VT_NO_ID, VT_COMM_ID(comm), &comm,
                          count*size, count*size);
        }
      else
        {
          time = vt_pform_wtime();
          vt_exit(&time);
        }

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_6(MPI_Reduce_scatter, sendbuf, recvbuf, recvcounts, datatype,
                  op, comm, result, 1, 0, NULL);
    }

  return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * MPI-2 One-sided communications
 *
 *-----------------------------------------------------------------------------
 */

#if defined(HAVE_MPI2_1SIDED) && HAVE_MPI2_1SIDED

/* -- MPI_Put -- */

VT_MPI_INT MPI_Put( void*  origin_addr,
                    VT_MPI_INT origin_count,
                    MPI_Datatype origin_datatype,
                    VT_MPI_INT target_rank,
                    MPI_Aint target_disp,
                    VT_MPI_INT target_count,
                    MPI_Datatype target_datatype,
                    MPI_Win win )
{
  VT_MPI_INT result, size;
  uint64_t time;
  uint32_t gid, wid;
  uint8_t was_recorded;

  MPI_Comm comm;

  if(IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();
      
      time = vt_pform_wtime();

      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_PUT]);

      CALL_PMPI_8(MPI_Put, origin_addr, origin_count, origin_datatype,
                  target_rank, target_disp, target_count, target_datatype,
                  win, result, 0, was_recorded, &time);

      if ( (target_rank != MPI_PROC_NULL) && (was_recorded) )
        {
          PMPI_Type_size(origin_datatype, &size);
          vt_win_id(win, &comm, &gid, &wid);
          if(is_rma_putre)
            vt_mpi_rma_putre(&time, VT_RANK_TO_PE(target_rank, comm), 
                             gid, wid, size*origin_count );
          else
            vt_mpi_rma_put(&time, VT_RANK_TO_PE(target_rank, comm), 
                           gid, wid, size*origin_count );
        }

      time = vt_pform_wtime();
      vt_exit(&time);
      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_8(MPI_Put, origin_addr, origin_count, origin_datatype,
                  target_rank, target_disp, target_count, target_datatype,
                  win, result, 0, 0, NULL);
    }
     
  return result;                   
}

/* -- MPI_Get -- */

VT_MPI_INT MPI_Get( void* origin_addr,
                    VT_MPI_INT origin_count,
                    MPI_Datatype origin_datatype,
                    VT_MPI_INT target_rank,
                    MPI_Aint target_disp,
                    VT_MPI_INT target_count,
                    MPI_Datatype target_datatype,
                    MPI_Win win)
{
  VT_MPI_INT result, size;
  uint64_t time;
  uint32_t gid, wid;
  uint8_t was_recorded;

  MPI_Comm comm;

  if(IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_GET]);

      CALL_PMPI_8(MPI_Get, origin_addr, origin_count, origin_datatype, 
                  target_rank, target_disp, target_count, target_datatype,
                  win, result, 0, was_recorded, &time);     

      if ( (target_rank != MPI_PROC_NULL) && (was_recorded) )
        {

          PMPI_Type_size(target_datatype, &size);
          vt_win_id(win, &comm, &gid, &wid);
          vt_mpi_rma_get(&time, VT_RANK_TO_PE(target_rank, comm), gid,
                         wid, target_count * size);

        }

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_8(MPI_Get, origin_addr, origin_count, origin_datatype, 
                  target_rank, target_disp, target_count, target_datatype,
                  win, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Accumulate -- */

VT_MPI_INT MPI_Accumulate( void* origin_addr,
                           VT_MPI_INT origin_count,
                           MPI_Datatype origin_datatype,
                           VT_MPI_INT target_rank,
                           MPI_Aint target_disp,
                           VT_MPI_INT target_count,
                           MPI_Datatype target_datatype,
                           MPI_Op op,
                           MPI_Win win )
{
  VT_MPI_INT result, size;
  uint64_t time;
  uint32_t gid, wid;
  uint8_t was_recorded;

  MPI_Comm comm;

  if(IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();
 
      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_ACCUMULATE]);

      CALL_PMPI_9(MPI_Accumulate, origin_addr, origin_count, origin_datatype,
                  target_rank, target_disp, target_count, target_datatype, op,
                  win, result, 0, was_recorded, &time);

      if ( (target_rank != MPI_PROC_NULL) && (was_recorded) )
        {
          PMPI_Type_size(origin_datatype, &size);
          vt_win_id(win, &comm, &gid, &wid);
          if(is_rma_putre)
            vt_mpi_rma_putre(&time, VT_RANK_TO_PE(target_rank, comm), 
                             gid, wid, size*origin_count );
          else
            vt_mpi_rma_put(&time, VT_RANK_TO_PE(target_rank, comm), 
                           gid, wid, size*origin_count );
        }

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_9(MPI_Accumulate, origin_addr, origin_count, origin_datatype,
                  target_rank, target_disp, target_count, target_datatype, op,
                  win, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Win_fence -- */

VT_MPI_INT MPI_Win_fence( VT_MPI_INT assert,
                          MPI_Win win )
{
  VT_MPI_INT result;
  uint64_t time;
  uint32_t gid, wid;
  uint8_t was_recorded;

  MPI_Comm comm;

  if(IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_WIN_FENCE]);

      CALL_PMPI_2(MPI_Win_fence, assert, win, result, 0, was_recorded, &time);

      time = vt_pform_wtime();

      if(was_recorded)
        {
          /* write RMA-END */
          vt_win_id(win, &comm, &gid, &wid);
          vt_mpi_rma_end(&time, gid, wid);
        }
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_2(MPI_Win_fence, assert, win, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Win_start -- */

VT_MPI_INT MPI_Win_start( MPI_Group group,
                          VT_MPI_INT assert,
                          MPI_Win win )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if(IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_WIN_START]);

      CALL_PMPI_3(MPI_Win_start, group, assert, win, result, 0, was_recorded,
                  &time);

      vt_win_set_gid(win, vt_group_id(group));

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_3(MPI_Win_start, group, assert, win, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Win_complete -- */

VT_MPI_INT MPI_Win_complete( MPI_Win win )
{
  VT_MPI_INT result;
  uint64_t time;
  uint32_t gid, wid;
  uint8_t was_recorded;

  MPI_Comm comm;

  if(IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_WIN_COMPLETE]);

      CALL_PMPI_1(MPI_Win_complete, win, result, 0, was_recorded, &time);

      time = vt_pform_wtime();

      vt_win_id(win, &comm, &gid, &wid);
      if(was_recorded)
        {
          /* write RMA-END */
          vt_mpi_rma_end(&time, gid, wid);
        }

      vt_win_set_gid(win,VT_COMM_ID(comm));
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_1(MPI_Win_complete, win, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Win_post -- */

VT_MPI_INT MPI_Win_post( MPI_Group group,
                         VT_MPI_INT assert,
                         MPI_Win win )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if(IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_WIN_POST]);

      CALL_PMPI_3(MPI_Win_post, group, assert, win, result, 0, was_recorded,
                  &time);

      vt_win_set_gid(win, vt_group_id(group));

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_3(MPI_Win_post, group, assert, win, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_Win_wait -- */

VT_MPI_INT MPI_Win_wait( MPI_Win win )
{
  VT_MPI_INT result;
  uint64_t time;
  uint32_t gid, wid;
  uint8_t was_recorded;

  MPI_Comm comm;

  if(IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_WIN_WAIT]);

      CALL_PMPI_1(MPI_Win_wait, win, result, 0, was_recorded, &time);

      time = vt_pform_wtime();

      vt_win_id(win, &comm, &gid, &wid);
      if(was_recorded)
        {
          /* write RMA-END */
          vt_mpi_rma_end(&time, gid, wid);
        }

      vt_win_set_gid(win,VT_COMM_ID(comm));

      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_1(MPI_Win_wait, win, result, 0, 0, NULL);
    }

  return result;
}

#if defined(HAVE_PMPI_WIN_TEST) && HAVE_PMPI_WIN_TEST

/* -- MPI_Win_test -- */

VT_MPI_INT MPI_Win_test( MPI_Win win, VT_MPI_INT* flag )
{
  VT_MPI_INT result;
  uint64_t time;
  uint32_t gid, wid;
  uint8_t was_recorded;

  MPI_Comm comm;

  if(IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_WIN_TEST]);

      CALL_PMPI_2(MPI_Win_test, win, flag, result, 0, was_recorded, &time);
      result = PMPI_Win_test(win, flag);

      time = vt_pform_wtime();

      vt_win_id(win, &comm, &gid, &wid);
      if( (*flag) && (was_recorded) )
        {
          /* write RMA-END */
          vt_mpi_rma_end(&time, gid, wid);
        } 

      if( (*flag))
        vt_win_set_gid(win,VT_COMM_ID(comm));

      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_2(MPI_Win_test, win, flag, result, 0, 0, NULL);
    }

  return result;
}

#endif /* HAVE_PMPI_WIN_TEST */

#if defined(HAVE_PMPI_WIN_LOCK) && HAVE_PMPI_WIN_LOCK

/* -- MPI_Win_lock -- */

VT_MPI_INT MPI_Win_lock( VT_MPI_INT lock_type,
                         VT_MPI_INT rank,
                         VT_MPI_INT assert,
                         MPI_Win win )
{
  VT_MPI_INT result;
  uint64_t time;
  uint8_t was_recorded;

  if(IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_WIN_LOCK]);

      CALL_PMPI_4(MPI_Win_lock, lock_type, rank, assert, win, result, 0,
                  was_recorded, &time);

      is_rma_putre = 0;

      time = vt_pform_wtime();
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_4(MPI_Win_lock, lock_type, rank, assert, win, result, 0,
                  0, NULL);
    }

  return result;
}

#endif /* HAVE_PMPI_WIN_LOCK */

#if defined(HAVE_PMPI_WIN_UNLOCK) && HAVE_PMPI_WIN_UNLOCK

/* -- MPI_Win_unlock -- */

VT_MPI_INT MPI_Win_unlock( VT_MPI_INT rank,
                           MPI_Win win )
{
  VT_MPI_INT result;
  uint64_t time;
  uint32_t gid, wid;
  uint8_t was_recorded;

  MPI_Comm comm;

  if(IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_WIN_UNLOCK]);

      CALL_PMPI_2(MPI_Win_unlock, rank, win, result, 0, was_recorded, &time);

      time = vt_pform_wtime();

      if(was_recorded)
        {
          /*write RMA_END */
          vt_win_id(win, &comm, &gid, &wid);
          vt_mpi_rma_end(&time, gid, wid);
          is_rma_putre = 1;
        }

      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_2(MPI_Win_unlock, rank, win, result, 0, 0, NULL);
    }

  return result;
}

#endif /* HAVE_PMPI_WIN_UNLOCK */

#endif /* HAVE_MPI2_1SIDED */

/*
 *-----------------------------------------------------------------------------
 *
 * MPI-2 Extended collective communication
 *
 *-----------------------------------------------------------------------------
 */

#if defined(HAVE_MPI2_EXTCOLL) && HAVE_MPI2_EXTCOLL

/* -- MPI_Alltoallw -- */

VT_MPI_INT MPI_Alltoallw( void* sendbuf,
                          VT_MPI_INT* sendcounts,
                          VT_MPI_INT* sdispls,
                          MPI_Datatype* sendtypes,
                          void* recvbuf,
                          VT_MPI_INT* recvcounts,
                          VT_MPI_INT* rdispls,
                          MPI_Datatype *recvtypes,
                          MPI_Comm comm )
{
  VT_MPI_INT result, recvcount=0, sendcount=0, recvsz, sendsz, N, i;
  uint64_t time, etime;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_ALLTOALLW]);

      CALL_PMPI_9(MPI_Alltoallw, sendbuf, sendcounts, sdispls, sendtypes,
                  recvbuf, recvcounts, rdispls, recvtypes, comm, result,
                  1, was_recorded, &time);

      if (was_recorded)
        {
          PMPI_Comm_size(comm, &N);
          for(i = 0; i<N; i++)
            {
              PMPI_Type_size(recvtypes[i], &recvsz);
              PMPI_Type_size(sendtypes[i], &sendsz);
              recvcount += recvsz * recvcounts[i];
              sendcount += sendsz * sendcounts[i];
            }

          etime = vt_pform_wtime();
          vt_mpi_collexit(&time, &etime,
                          vt_mpi_regid[VT__MPI_ALLTOALLW],
                          VT_NO_ID, VT_COMM_ID(comm), &comm,
                          sendcount, recvcount);
        }
      else
        {
          time = vt_pform_wtime();
          vt_exit(&time);
        }

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_9(MPI_Alltoallw, sendbuf, sendcounts, sdispls, sendtypes,
                  recvbuf, recvcounts, rdispls, recvtypes, comm, result,
                  1, 0, NULL);
    }

  return result;
}

/* -- MPI_Exscan -- */

VT_MPI_INT MPI_Exscan( void* sendbuf,
                       void* recvbuf,
                       VT_MPI_INT count,
                       MPI_Datatype datatype,
                       MPI_Op op,
                       MPI_Comm comm )
{
  VT_MPI_INT result, size, me, N;
  uint64_t time, etime;
  uint8_t was_recorded;

  if (IS_MPI_TRACE_ON)
    {
      MPI_TRACE_OFF();

      time = vt_pform_wtime();
      was_recorded = vt_enter(&time, vt_mpi_regid[VT__MPI_EXSCAN]);

      CALL_PMPI_6(MPI_Exscan, sendbuf, recvbuf, count, datatype, op, comm,
                  result, 1, was_recorded, &time);

      if (was_recorded)
      {
        PMPI_Type_size(datatype, &size);
        PMPI_Comm_rank(comm, &me);
        PMPI_Comm_size(comm, &N);

        etime = vt_pform_wtime();
        vt_mpi_collexit(&time, &etime,
                        vt_mpi_regid[VT__MPI_EXSCAN],
                        VT_NO_ID, VT_COMM_ID(comm), &comm,
                        (N-me-1) * size * count, size * count);
        }
      else
        {
          time = vt_pform_wtime();
          vt_exit(&time);
        }

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_6(MPI_Exscan, sendbuf, recvbuf, count, datatype, op, comm,
                  result, 1, 0, NULL);
    }

  return result;
}

#endif /* HAVE_MPI2_EXTCOLL */

/*
 *-----------------------------------------------------------------------------
 *
 * MPI-2 I/O
 *
 *-----------------------------------------------------------------------------
 */

#if defined(HAVE_MPI2_IO) && HAVE_MPI2_IO

/* Some macros, to ease the programming, and because MaJu likes them so much ;-)
 */ 

/**
 * Write function enter record and make MPI_Status available.
 */
#define MPIIO_ENTER_IO_W_STATUS(REGIONID) \
  uint64_t time; \
  MPI_Status mystatus; \
  uint8_t was_recorded; \
  MPI_TRACE_OFF(); \
  time = vt_pform_wtime(); \
  was_recorded = vt_enter(&time, vt_mpi_regid[REGIONID]); \
  if (was_recorded && (status == MPI_STATUS_IGNORE)) \
    { \
      status = &mystatus; \
    }

/**
 * Write function enter record, I/O begin record, make handleid available.
 */
#define MPIIO_ENTER_IO_W_HANDLE(REGIONID) \
  uint64_t time; \
  uint64_t handleid = 0; \
  uint8_t was_recorded; \
  MPI_TRACE_OFF(); \
  time = vt_pform_wtime(); \
  was_recorded = vt_enter(&time, vt_mpi_regid[REGIONID]); \
  if (was_recorded) \
    { \
      handleid = VTTHRD_IO_NEXT_HANDLEID(VTTHRD_MY_VTTHRD); \
      vt_iobegin(&time, handleid); \
    }

/**
 * Write function enter record, I/O begin record, save handleid and datatype
 * with the associated MPI_File fh.
 */
#define MPIIO_ENTER_IO_SPLITCOLL(REGIONID) \
  uint64_t time; \
  uint8_t was_recorded; \
  MPI_TRACE_OFF(); \
  time = vt_pform_wtime(); \
  was_recorded = vt_enter(&time, vt_mpi_regid[REGIONID]); \
  if (was_recorded) \
    { \
      uint64_t handleid; \
      vt_mpifile_data *fdata; \
      handleid = VTTHRD_IO_NEXT_HANDLEID(VTTHRD_MY_VTTHRD); \
      vt_iobegin(&time, handleid); \
      fdata = vt_mpifile_get_data(fh); \
      fdata->split_collective_id = handleid; \
      fdata->datatype = datatype; \
    }

/**
 * Write function enter record, I/O begin record, make MPI_Status and handleid
 * available.
 */
#define MPIIO_ENTER_IO_W_HANDLE_STATUS(REGIONID) \
  uint64_t time; \
  uint64_t handleid = 0; \
  MPI_Status mystatus; \
  uint8_t was_recorded; \
  MPI_TRACE_OFF(); \
  time = vt_pform_wtime(); \
  was_recorded = vt_enter(&time, vt_mpi_regid[REGIONID]); \
  if (was_recorded) \
    { \
      handleid = VTTHRD_IO_NEXT_HANDLEID(VTTHRD_MY_VTTHRD); \
      vt_iobegin(&time, handleid); \
      if (status == MPI_STATUS_IGNORE) \
        status = &mystatus; \
    }

/**
 * Just write function leave record.
 */
#define MPIIO_LEAVE_IO() \
  time = vt_pform_wtime(); \
  vt_exit(&time); \
  MPI_TRACE_ON()

/**
 * Write I/O end record, leave record (Used for 0-byte operations like open,
 * close, seek). Needs handleid.
 */
#define MPIIO_LEAVE_IO_W_HANDLE(IOOP) \
  time = vt_pform_wtime(); \
  if (was_recorded) \
    { \
      uint32_t fid = vt_mpifile_get_id(fh); \
      if (result == MPI_SUCCESS) \
        { \
          vt_ioend(&time, fid, handleid, IOOP, 0); \
        } \
      else \
        { \
          vt_ioend(&time, fid, handleid, IOOP | VT_IOFLAG_IOFAILED, 0); \
        } \
    } \
  vt_exit(&time); \
  MPI_TRACE_ON()

/**
 * If nonblocking function was successful, create vt_request object; if not,
 * write I/O end record indicating failure; write function leave record in any
 * case.
 */
#define MPIIO_LEAVE_IO_W_REQ(IOOP) \
  time = vt_pform_wtime(); \
  if (was_recorded) \
    { \
      uint32_t fid = vt_mpifile_get_id(fh); \
      if (result == MPI_SUCCESS) \
        { \
          vt_iorequest_create(*request, datatype, handleid, fid, IOOP); \
        } \
      else \
        { \
          vt_ioend(&time, fid, handleid, IOOP | VT_IOFLAG_IOFAILED, 0); \
        } \
    } \
  vt_exit(&time); \
  MPI_TRACE_ON()

/**
 * Write I/O end record, function leave record for this _splitcollective_
 * operation. Retrieves all needed information from the associated
 * vt_mpifile_data object.
 */
#define MPIIO_LEAVE_IO_SPLITCOLL(IOOP) \
  time = vt_pform_wtime(); \
  if (was_recorded) \
    { \
      vt_mpifile_data *fdata = vt_mpifile_get_data(fh); \
      if (result == MPI_SUCCESS) \
        { \
          VT_MPI_INT sz, cnt; \
          PMPI_Type_size(fdata->datatype, &sz); \
          PMPI_Get_count(status, fdata->datatype, &cnt); \
          if (cnt == MPI_UNDEFINED) \
            cnt = 0; \
          vt_ioend(&time, fdata->fid, fdata->split_collective_id, IOOP, (uint64_t)cnt * (uint64_t)sz); \
        } \
      else \
        { \
          vt_ioend(&time, fdata->fid, fdata->split_collective_id, IOOP | VT_IOFLAG_IOFAILED, 0); \
        } \
    } \
  vt_exit(&time); \
  MPI_TRACE_ON()

/**
 * Write I/O end record, function leave record for this operation (usually
 * simple read or write). Needs handleid and status object.
 */
#define MPIIO_LEAVE_IO_W_HANDLE_STATUS(IOOP) \
  time = vt_pform_wtime(); \
  if (was_recorded) \
    { \
      uint32_t fid = vt_mpifile_get_id(fh); \
      if (result == MPI_SUCCESS) \
        { \
          VT_MPI_INT sz, cnt; \
          PMPI_Type_size(datatype, &sz); \
          PMPI_Get_count(status, datatype, &cnt); \
          if (cnt == MPI_UNDEFINED) \
            cnt = 0; \
          vt_ioend(&time, fid, handleid, IOOP, (uint64_t)cnt * (uint64_t)sz); \
        } \
      else \
        { \
          vt_ioend(&time, fid, handleid, IOOP | VT_IOFLAG_IOFAILED, 0); \
        } \
    } \
  vt_exit(&time); \
  MPI_TRACE_ON()

/*
 *-----------------------------------------------------------------------------
 *
 * File access
 *
 *-----------------------------------------------------------------------------
 */

/* -- MPI_File_close -- */

VT_MPI_INT MPI_File_close( MPI_File* fh )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPI_File bak = *fh;
      MPIIO_ENTER_IO_W_HANDLE(VT__MPI_FILE_CLOSE);

      CALL_PMPI_1(MPI_File_close, fh, result, 0, was_recorded, &time);

      time = vt_pform_wtime();
      if (was_recorded)
        {
          uint32_t fid = vt_mpifile_get_id(bak);
          if (result == MPI_SUCCESS)
            vt_ioend(&time, fid, handleid, VT_IOOP_CLOSE, 0);
          else
            vt_ioend(&time, fid, handleid, VT_IOOP_CLOSE | VT_IOFLAG_IOFAILED, 0);
        }
      vt_mpifile_free(bak);
      vt_exit(&time);
      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_1(MPI_File_close, fh, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_open -- */

VT_MPI_INT MPI_File_open( MPI_Comm comm,
                          char* filename,
                          VT_MPI_INT amode,
                          MPI_Info info,
                          MPI_File* fh )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      uint32_t fid;

      MPIIO_ENTER_IO_W_HANDLE(VT__MPI_FILE_OPEN);

      CALL_PMPI_5(MPI_File_open, comm, filename, amode, info, fh, result,
                  0, was_recorded, &time);

      time = vt_pform_wtime();
      fid = vt_mpifile_create(*fh, filename);

      if (was_recorded)
        {
          if (result == MPI_SUCCESS)
            {
              vt_ioend(&time, fid, handleid, VT_IOOP_OPEN, 0);
            }
          else
            {
              vt_ioend(&time, fid, handleid, VT_IOOP_OPEN | VT_IOFLAG_IOFAILED, 0);
            }
        }
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_5(MPI_File_open, comm, filename, amode, info, fh, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_delete -- */

VT_MPI_INT MPI_File_delete( char* filename,
                            MPI_Info info )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE(VT__MPI_FILE_DELETE);

      CALL_PMPI_2(MPI_File_delete, filename, info, result,
                  0, was_recorded, &time);

      time = vt_pform_wtime();
      if (was_recorded)
        {
          uint32_t fid = vt_mpifilename_get_id(filename);
          if (result == MPI_SUCCESS)
            {
              vt_ioend(&time, fid, handleid, VT_IOOP_UNLINK, 0);
            }
          else
            {
              vt_ioend(&time, fid, handleid, VT_IOOP_UNLINK | VT_IOFLAG_IOFAILED, 0);
            }
        }
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_2(MPI_File_delete, filename, info, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_sync -- */

VT_MPI_INT MPI_File_sync( MPI_File fh )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE(VT__MPI_FILE_SYNC);

      CALL_PMPI_1(MPI_File_sync, fh, result, 0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_HANDLE(VT_IOOP_SYNC);
    }
  else
    {
      CALL_PMPI_1(MPI_File_sync, fh, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_preallocate -- */

VT_MPI_INT MPI_File_preallocate( MPI_File fh,
                                 MPI_Offset size )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE(VT__MPI_FILE_PREALLOCATE);

      CALL_PMPI_2(MPI_File_preallocate, fh, size, result,
                  0, was_recorded, &time);

      time = vt_pform_wtime();
      if (was_recorded)
        {
          uint32_t fid = vt_mpifile_get_id(fh);
          if (result == MPI_SUCCESS)
            {
              vt_ioend(&time, fid, handleid, VT_IOOP_WRITE, (uint64_t)size);
            }
          else
            {
              vt_ioend(&time, fid, handleid, VT_IOOP_WRITE | VT_IOFLAG_IOFAILED, 0);
            }
        }
      vt_exit(&time);

      MPI_TRACE_ON();
    }
  else
    {
      CALL_PMPI_2(MPI_File_preallocate, fh, size, result,
                  0, 0, NULL);
    }

  return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Data access with individual file pointers
 *
 *-----------------------------------------------------------------------------
 */

/* -- MPI_File_iread -- */
VT_MPI_INT MPI_File_iread( MPI_File fh,
                           void* buf,
                           VT_MPI_INT count,
                           MPI_Datatype datatype,
                           MPI_Request* request )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE(VT__MPI_FILE_IREAD);

      CALL_PMPI_5(MPI_File_iread, fh, buf, count, datatype, request, result,
                  0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_REQ(VT_IOOP_READ | VT_IOFLAG_ASYNC);
    }
  else
    {
      CALL_PMPI_5(MPI_File_iread, fh, buf, count, datatype, request, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_iwrite -- */

VT_MPI_INT MPI_File_iwrite( MPI_File fh,
                            void* buf,
                            VT_MPI_INT count,
                            MPI_Datatype datatype,
                            MPI_Request* request )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE(VT__MPI_FILE_IWRITE);

      CALL_PMPI_5(MPI_File_iwrite, fh, buf, count, datatype, request, result,
                  0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_REQ(VT_IOOP_WRITE | VT_IOFLAG_ASYNC);
    }
  else
    {
      CALL_PMPI_5(MPI_File_iwrite, fh, buf, count, datatype, request, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_read -- */

VT_MPI_INT MPI_File_read( MPI_File fh,
                          void* buf,
                          VT_MPI_INT count,
                          MPI_Datatype datatype,
                          MPI_Status* status )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE_STATUS(VT__MPI_FILE_READ);

      CALL_PMPI_5(MPI_File_read, fh, buf, count, datatype, status, result,
                  0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_HANDLE_STATUS(VT_IOOP_READ);
    }
  else
    {
      CALL_PMPI_5(MPI_File_read, fh, buf, count, datatype, status, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_read_all -- */

VT_MPI_INT MPI_File_read_all( MPI_File fh,
                              void* buf,
                              VT_MPI_INT count,
                              MPI_Datatype datatype,
                              MPI_Status* status )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE_STATUS(VT__MPI_FILE_READ_ALL);

      CALL_PMPI_5(MPI_File_read_all, fh, buf, count, datatype, status, result,
                  0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_HANDLE_STATUS(VT_IOOP_READ | VT_IOFLAG_COLL);
    }
  else
    {
      CALL_PMPI_5(MPI_File_read_all, fh, buf, count, datatype, status, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_seek -- */

VT_MPI_INT MPI_File_seek( MPI_File fh,
                          MPI_Offset offset,
                          VT_MPI_INT whence )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE(VT__MPI_FILE_SEEK);

      CALL_PMPI_3(MPI_File_seek, fh, offset, whence, result,
                  0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_HANDLE(VT_IOOP_SEEK);
    }
  else
    {
      CALL_PMPI_3(MPI_File_seek, fh, offset, whence, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_write -- */

VT_MPI_INT MPI_File_write( MPI_File fh,
                           void* buf,
                           VT_MPI_INT count,
                           MPI_Datatype datatype,
                           MPI_Status* status )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE_STATUS(VT__MPI_FILE_WRITE);

      CALL_PMPI_5(MPI_File_write, fh, buf, count, datatype, status, result,
                  0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_HANDLE_STATUS(VT_IOOP_WRITE);
    }
  else
    {
      CALL_PMPI_5(MPI_File_write, fh, buf, count, datatype, status, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_write_all -- */

VT_MPI_INT MPI_File_write_all( MPI_File fh,
                               void* buf,
                               VT_MPI_INT count,
                               MPI_Datatype datatype,
                               MPI_Status* status )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE_STATUS(VT__MPI_FILE_WRITE_ALL);

      CALL_PMPI_5(MPI_File_write_all, fh, buf, count, datatype, status, result,
                  0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_HANDLE_STATUS(VT_IOOP_WRITE | VT_IOFLAG_COLL);
    }
  else
    {
      CALL_PMPI_5(MPI_File_write_all, fh, buf, count, datatype, status, result,
                  0, 0, NULL);
    }

  return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Split collective data access routines
 *
 *-----------------------------------------------------------------------------
 */

/* -- MPI_File_read_all_begin -- */

VT_MPI_INT MPI_File_read_all_begin( MPI_File fh,
                                    void* buf,
                                    VT_MPI_INT count,
                                    MPI_Datatype datatype )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_SPLITCOLL(VT__MPI_FILE_READ_ALL_BEGIN);

      CALL_PMPI_4(MPI_File_read_all_begin, fh, buf, count, datatype, result,
                  0, was_recorded, &time);

      MPIIO_LEAVE_IO();
    }
  else
    {
      CALL_PMPI_4(MPI_File_read_all_begin, fh, buf, count, datatype, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_read_all_end -- */

VT_MPI_INT MPI_File_read_all_end( MPI_File fh,
                                  void* buf,
                                  MPI_Status* status )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_STATUS(VT__MPI_FILE_READ_ALL_END);

      CALL_PMPI_3(MPI_File_read_all_end, fh, buf, status, result,
                  0, was_recorded, &time);

      MPIIO_LEAVE_IO_SPLITCOLL(VT_IOOP_READ | VT_IOFLAG_ASYNC | VT_IOFLAG_COLL);
    }
  else
    {
      CALL_PMPI_3(MPI_File_read_all_end, fh, buf, status, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_read_at_all_begin -- */

VT_MPI_INT MPI_File_read_at_all_begin( MPI_File fh,
                                       MPI_Offset offset,
                                       void* buf,
                                       VT_MPI_INT count,
                                       MPI_Datatype datatype )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_SPLITCOLL(VT__MPI_FILE_READ_AT_ALL_BEGIN);

      CALL_PMPI_5(MPI_File_read_at_all_begin, fh, offset, buf, count, datatype,
                  result, 0, was_recorded, &time);

      MPIIO_LEAVE_IO();
    }
  else
    {
      CALL_PMPI_5(MPI_File_read_at_all_begin, fh, offset, buf, count, datatype,
                  result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_read_at_all_end -- */

VT_MPI_INT MPI_File_read_at_all_end( MPI_File fh,
                                     void* buf,
                                     MPI_Status* status )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_STATUS(VT__MPI_FILE_READ_AT_ALL_END);

      CALL_PMPI_3(MPI_File_read_at_all_end, fh, buf, status, result,
                  0, was_recorded, &time);

      MPIIO_LEAVE_IO_SPLITCOLL(VT_IOOP_READ | VT_IOFLAG_ASYNC | VT_IOFLAG_COLL);
    }
  else
    {
      CALL_PMPI_3(MPI_File_read_at_all_end, fh, buf, status, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_read_ordered_begin -- */

VT_MPI_INT MPI_File_read_ordered_begin( MPI_File fh,
                                        void* buf,
                                        VT_MPI_INT count,
                                        MPI_Datatype datatype )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_SPLITCOLL(VT__MPI_FILE_READ_ORDERED_BEGIN);

      CALL_PMPI_4(MPI_File_read_ordered_begin, fh, buf, count, datatype,
                  result, 0, was_recorded, &time);

      MPIIO_LEAVE_IO();
    }
  else
    {
      CALL_PMPI_4(MPI_File_read_ordered_begin, fh, buf, count, datatype,
                  result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_read_ordered_end -- */

VT_MPI_INT MPI_File_read_ordered_end( MPI_File fh,
                                      void* buf,
                                      MPI_Status* status )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_STATUS(VT__MPI_FILE_READ_ORDERED_END);

      CALL_PMPI_3(MPI_File_read_ordered_end, fh, buf, status, result,
                  0, was_recorded, &time);

      MPIIO_LEAVE_IO_SPLITCOLL(VT_IOOP_READ | VT_IOFLAG_ASYNC | VT_IOFLAG_COLL);
    }
  else
    {
      CALL_PMPI_3(MPI_File_read_ordered_end, fh, buf, status, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_write_all_begin -- */

VT_MPI_INT MPI_File_write_all_begin( MPI_File fh,
                                     void* buf,
                                     VT_MPI_INT count,
                                     MPI_Datatype datatype )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_SPLITCOLL(VT__MPI_FILE_WRITE_ALL_BEGIN);

      CALL_PMPI_4(MPI_File_write_all_begin, fh, buf, count, datatype, result,
                  0, was_recorded, &time);

      MPIIO_LEAVE_IO();
    }
  else
    {
      CALL_PMPI_4(MPI_File_write_all_begin, fh, buf, count, datatype, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_write_all_end -- */

VT_MPI_INT MPI_File_write_all_end( MPI_File fh,
                                   void* buf,
                                   MPI_Status* status )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_STATUS(VT__MPI_FILE_WRITE_ALL_END);

      CALL_PMPI_3(MPI_File_write_all_end, fh, buf, status, result,
                  0, was_recorded, &time);

      MPIIO_LEAVE_IO_SPLITCOLL(VT_IOOP_WRITE | VT_IOFLAG_ASYNC | VT_IOFLAG_COLL);
    }
  else
    {
      CALL_PMPI_3(MPI_File_write_all_end, fh, buf, status, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_write_at_all_begin -- */

VT_MPI_INT MPI_File_write_at_all_begin( MPI_File fh,
                                        MPI_Offset offset,
                                        void* buf,
                                        VT_MPI_INT count,
                                        MPI_Datatype datatype )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_SPLITCOLL(VT__MPI_FILE_WRITE_AT_ALL_BEGIN);

      CALL_PMPI_5(MPI_File_write_at_all_begin, fh, offset, buf, count,
                  datatype, result, 0, was_recorded, &time);

      MPIIO_LEAVE_IO();
    }
  else
    {
      CALL_PMPI_5(MPI_File_write_at_all_begin, fh, offset, buf, count,
                  datatype, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_write_at_all_end -- */

VT_MPI_INT MPI_File_write_at_all_end( MPI_File fh,
                                      void* buf,
                                      MPI_Status* status )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_STATUS(VT__MPI_FILE_WRITE_AT_ALL_END);

      CALL_PMPI_3(MPI_File_write_at_all_end, fh, buf, status, result,
                  0, was_recorded, &time);

      MPIIO_LEAVE_IO_SPLITCOLL(VT_IOOP_WRITE | VT_IOFLAG_ASYNC | VT_IOFLAG_COLL);
    }
  else
    {
      CALL_PMPI_3(MPI_File_write_at_all_end, fh, buf, status, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_write_ordered_begin -- */

VT_MPI_INT MPI_File_write_ordered_begin( MPI_File fh,
                                         void* buf,
                                         VT_MPI_INT count,
                                         MPI_Datatype datatype )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_SPLITCOLL(VT__MPI_FILE_WRITE_ORDERED_BEGIN);

      CALL_PMPI_4(MPI_File_write_ordered_begin, fh, buf, count, datatype,
                  result, 0, was_recorded, &time);

      MPIIO_LEAVE_IO();
    }
  else
    {
      CALL_PMPI_4(MPI_File_write_ordered_begin, fh, buf, count, datatype,
                  result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_write_ordered_end -- */

VT_MPI_INT MPI_File_write_ordered_end( MPI_File fh,
                                       void* buf,
                                       MPI_Status* status )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_STATUS(VT__MPI_FILE_WRITE_ORDERED_END);

      CALL_PMPI_3(MPI_File_write_ordered_end, fh, buf, status, result,
                  0, was_recorded, &time);

      MPIIO_LEAVE_IO_SPLITCOLL(VT_IOOP_WRITE | VT_IOFLAG_ASYNC | VT_IOFLAG_COLL);
    }
  else
    {
      CALL_PMPI_3(MPI_File_write_ordered_end, fh, buf, status, result,
                  0, 0, NULL);
    }

  return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Data access with explicit offsets
 *
 *-----------------------------------------------------------------------------
 */

/* -- MPI_File_iread_at -- */

VT_MPI_INT MPI_File_iread_at( MPI_File fh,
                              MPI_Offset offset,
                              void* buf,
                              VT_MPI_INT count,
                              MPI_Datatype datatype,
                              MPI_Request* request )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE(VT__MPI_FILE_IREAD_AT);

      CALL_PMPI_6(MPI_File_iread_at, fh, offset, buf, count, datatype, request,
                  result, 0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_REQ(VT_IOOP_WRITE | VT_IOFLAG_ASYNC);
    }
  else
    {
      CALL_PMPI_6(MPI_File_iread_at, fh, offset, buf, count, datatype, request,
                  result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_iwrite_at -- */

VT_MPI_INT MPI_File_iwrite_at( MPI_File fh,
                               MPI_Offset offset,
                               void* buf,
                               VT_MPI_INT count,
                               MPI_Datatype datatype,
                               MPI_Request* request )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE(VT__MPI_FILE_IWRITE_AT);

      CALL_PMPI_6(MPI_File_iwrite_at, fh, offset, buf, count, datatype,
                  request, result, 0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_REQ(VT_IOOP_WRITE | VT_IOFLAG_ASYNC);
    }
  else
    {
      CALL_PMPI_6(MPI_File_iwrite_at, fh, offset, buf, count, datatype,
                  request, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_read_at -- */

VT_MPI_INT MPI_File_read_at( MPI_File fh,
                             MPI_Offset offset,
                             void* buf,
                             VT_MPI_INT count,
                             MPI_Datatype datatype,
                             MPI_Status* status )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE_STATUS(VT__MPI_FILE_READ_AT);

      CALL_PMPI_6(MPI_File_read_at, fh, offset, buf, count, datatype, status,
                  result, 0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_HANDLE_STATUS(VT_IOOP_READ);
    }
  else
    {
      CALL_PMPI_6(MPI_File_read_at, fh, offset, buf, count, datatype, status,
                  result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_read_at_all -- */

VT_MPI_INT MPI_File_read_at_all( MPI_File fh,
                                 MPI_Offset offset,
                                 void* buf,
                                 VT_MPI_INT count,
                                 MPI_Datatype datatype,
                                 MPI_Status* status )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE_STATUS(VT__MPI_FILE_READ_AT_ALL);

      CALL_PMPI_6(MPI_File_read_at_all, fh, offset, buf, count, datatype,
                  status, result, 0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_HANDLE_STATUS(VT_IOOP_READ | VT_IOFLAG_COLL);
    }
  else
    {
      CALL_PMPI_6(MPI_File_read_at_all, fh, offset, buf, count, datatype,
                  status, result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_write_at -- */

VT_MPI_INT MPI_File_write_at( MPI_File fh,
                              MPI_Offset offset,
                              void* buf,
                              VT_MPI_INT count,
                              MPI_Datatype datatype,
                              MPI_Status* status )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE_STATUS(VT__MPI_FILE_WRITE_AT);

      CALL_PMPI_6(MPI_File_write_at, fh, offset, buf, count, datatype, status,
                  result, 0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_HANDLE_STATUS(VT_IOOP_WRITE);
    }
  else
    {
      CALL_PMPI_6(MPI_File_write_at, fh, offset, buf, count, datatype, status,
                  result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_write_at_all -- */

VT_MPI_INT MPI_File_write_at_all( MPI_File fh,
                                  MPI_Offset offset,
                                  void* buf,
                                  VT_MPI_INT count,
                                  MPI_Datatype datatype,
                                  MPI_Status* status )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE_STATUS(VT__MPI_FILE_WRITE_AT_ALL);

      CALL_PMPI_6(MPI_File_write_at_all, fh, offset, buf, count, datatype,
                  status, result, 0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_HANDLE_STATUS(VT_IOOP_WRITE | VT_IOFLAG_COLL);
    }
  else
    {
      CALL_PMPI_6(MPI_File_write_at_all, fh, offset, buf, count, datatype,
                  status, result, 0, 0, NULL);
    }

  return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Data access with shared file pointers
 *
 *-----------------------------------------------------------------------------
 */

/* -- MPI_File_iread_shared -- */

VT_MPI_INT MPI_File_iread_shared( MPI_File fh,
                                  void* buf,
                                  VT_MPI_INT count,
                                  MPI_Datatype datatype,
                                  MPI_Request* request )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE(VT__MPI_FILE_IREAD_SHARED);

      CALL_PMPI_5(MPI_File_iread_shared, fh, buf, count, datatype, request,
                  result, 0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_REQ(VT_IOOP_READ | VT_IOFLAG_ASYNC);
    }
  else
    {
      CALL_PMPI_5(MPI_File_iread_shared, fh, buf, count, datatype, request,
                  result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_iwrite_shared -- */

VT_MPI_INT MPI_File_iwrite_shared( MPI_File fh,
                                   void* buf,
                                   VT_MPI_INT count,
                                   MPI_Datatype datatype,
                                   MPI_Request* request )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE(VT__MPI_FILE_IWRITE_SHARED);

      CALL_PMPI_5(MPI_File_iwrite_shared, fh, buf, count, datatype, request,
                  result, 0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_REQ(VT_IOOP_WRITE | VT_IOFLAG_ASYNC);
    }
  else
    {
      CALL_PMPI_5(MPI_File_iwrite_shared, fh, buf, count, datatype, request,
                  result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_read_ordered -- */

VT_MPI_INT MPI_File_read_ordered( MPI_File fh,
                                  void* buf,
                                  VT_MPI_INT count,
                                  MPI_Datatype datatype,
                                  MPI_Status* status )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE_STATUS(VT__MPI_FILE_READ_ORDERED);

      CALL_PMPI_5(MPI_File_read_ordered, fh, buf, count, datatype, status,
                  result, 0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_HANDLE_STATUS(VT_IOOP_READ | VT_IOFLAG_COLL);
    }
  else
    {
      CALL_PMPI_5(MPI_File_read_ordered, fh, buf, count, datatype, status,
                  result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_read_shared -- */

VT_MPI_INT MPI_File_read_shared( MPI_File fh,
                                 void* buf,
                                 VT_MPI_INT count,
                                 MPI_Datatype datatype,
                                 MPI_Status* status )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE_STATUS(VT__MPI_FILE_READ_SHARED);

      CALL_PMPI_5(MPI_File_read_shared, fh, buf, count, datatype, status,
                  result, 0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_HANDLE_STATUS(VT_IOOP_READ);
    }
  else
    {
      CALL_PMPI_5(MPI_File_read_shared, fh, buf, count, datatype, status,
                  result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_seek_shared -- */

VT_MPI_INT MPI_File_seek_shared( MPI_File fh,
                                 MPI_Offset offset,
                                 VT_MPI_INT whence )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE(VT__MPI_FILE_SEEK_SHARED);

      CALL_PMPI_3(MPI_File_seek_shared, fh, offset, whence, result,
                  0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_HANDLE(VT_IOOP_SEEK);
    }
  else
    {
      CALL_PMPI_3(MPI_File_seek_shared, fh, offset, whence, result,
                  0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_write_ordered -- */

VT_MPI_INT MPI_File_write_ordered( MPI_File fh,
                                   void* buf,
                                   VT_MPI_INT count,
                                   MPI_Datatype datatype,
                                   MPI_Status* status )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE_STATUS(VT__MPI_FILE_WRITE_ORDERED);

      CALL_PMPI_5(MPI_File_write_ordered, fh, buf, count, datatype, status,
                  result, 0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_HANDLE_STATUS(VT_IOOP_WRITE | VT_IOFLAG_COLL);
    }
  else
    {
      CALL_PMPI_5(MPI_File_write_ordered, fh, buf, count, datatype, status,
                  result, 0, 0, NULL);
    }

  return result;
}

/* -- MPI_File_write_shared -- */

VT_MPI_INT MPI_File_write_shared( MPI_File fh,
                                  void* buf,
                                  VT_MPI_INT count,
                                  MPI_Datatype datatype,
                                  MPI_Status* status )
{
  VT_MPI_INT result;

  if (IS_MPI_TRACE_ON)
    {
      MPIIO_ENTER_IO_W_HANDLE_STATUS(VT__MPI_FILE_WRITE_SHARED);

      CALL_PMPI_5(MPI_File_write_shared, fh, buf, count, datatype, status,
                  result, 0, was_recorded, &time);

      MPIIO_LEAVE_IO_W_HANDLE_STATUS(VT_IOOP_WRITE);
    }
  else
    {
      CALL_PMPI_5(MPI_File_write_shared, fh, buf, count, datatype, status,
                  result, 0, 0, NULL);
    }

  return result;
}

#undef MPIIO_ENTER_IO_SPLITCOLL
#undef MPIIO_ENTER_IO_W_HANDLE
#undef MPIIO_ENTER_IO_W_HANDLE_STATUS
#undef MPIIO_ENTER_IO_W_STATUS
#undef MPIIO_LEAVE_IO
#undef MPIIO_LEAVE_IO_SPLITCOLL
#undef MPIIO_LEAVE_IO_W_HANDLE
#undef MPIIO_LEAVE_IO_W_HANDLE_STATUS
#undef MPIIO_LEAVE_IO_W_REQ

#endif /* HAVE_MPI2_IO */

/* include generated wrapper functions */
#include "vt_mpiwrap.gen.c"

