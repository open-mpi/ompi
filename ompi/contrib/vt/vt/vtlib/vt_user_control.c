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

#include <string.h>
#include "vt_defs.h"
#include "vt_env.h"
#include "vt_fbindings.h"
#include "vt_inttypes.h"
#include "vt_memhook.h"
#include "vt_pform.h"
#include "vt_trc.h"
#if ((defined(VT_MPI) || defined(VT_HYB)) \
    && defined(VT_ETIMESYNC) && TIMER_IS_GLOBAL == 0)
# include "vt_esync.h"
#endif /* (VT_MPI || VT_HYB) && VT_ETIMESYNC && TIMER_IS_GLOBAL */
#define VTRACE
#undef VTRACE_NO_CONTROL
#include "vt_user.h"

static int vt_init = 1;        /* is initialization needed? */

#define VT_INIT \
  if ( vt_init ) { \
    vt_init = 0; \
    vt_open(); \
  }

int VT_User_is_trace_on__()
{
  int ret;

  VT_MEMHOOKS_OFF();

  VT_INIT;

  ret = vt_is_trace_on();

  VT_MEMHOOKS_ON();

  return ret;
}

void VT_User_trace_on__()
{
  VT_MEMHOOKS_OFF();

  VT_INIT;

  vt_trace_on(1);

  VT_MEMHOOKS_ON();
}

void VT_User_trace_off__()
{
  VT_MEMHOOKS_OFF();

  VT_INIT;

  vt_trace_off(1, 0);

  VT_MEMHOOKS_ON();
}

void VT_User_buffer_flush__()
{
  VT_MEMHOOKS_OFF();

  VT_INIT;

  vt_buffer_flush();

  VT_MEMHOOKS_ON();
}

void VT_User_timesync__()
{
#if ((defined(VT_MPI) || defined(VT_HYB)) \
    && defined(VT_ETIMESYNC) && TIMER_IS_GLOBAL == 0)
  VT_MEMHOOKS_OFF();

  VT_INIT;

  if ( vt_num_traces > 1 && vt_env_etimesync() )
    vt_esync(MPI_COMM_WORLD);

  VT_MEMHOOKS_ON();
#endif /* (VT_MPI || VT_HYB) && VT_ETIMESYNC && TIMER_IS_GLOBAL */
}

void VT_User_update_counter__()
{
  VT_MEMHOOKS_OFF();

  VT_INIT;

  vt_update_counter();

  VT_MEMHOOKS_ON();
}

/*
 * Fortran version
 */

VT_DECLDEF(void VT_User_is_trace_on___f(int* ierr))
{
  *ierr = VT_User_is_trace_on__();
} VT_GENERATE_F77_BINDINGS(vt_user_is_trace_on__, VT_USER_IS_TRACE_ON__,
                           VT_User_is_trace_on___f,
                           (int* ierr), (ierr))

VT_GENERATE_F77_BINDINGS(vt_user_trace_on__, VT_USER_TRACE_ON__,
                         VT_User_trace_on__,
                         (void), ())

VT_GENERATE_F77_BINDINGS(vt_user_trace_off__, VT_USER_TRACE_OFF__,
                         VT_User_trace_off__,
                         (void), ())

VT_GENERATE_F77_BINDINGS(vt_user_buffer_flush__, VT_USER_BUFFER_FLUSH__,
                         VT_User_buffer_flush__,
                         (void), ())

VT_GENERATE_F77_BINDINGS(vt_user_timesync__, VT_USER_TIMESYNC__,
                         VT_User_timesync__,
                         (void), ())

VT_GENERATE_F77_BINDINGS(vt_user_update_counter__, VT_USER_UPDATE_COUNTER__,
                         VT_User_update_counter__,
                         (void), ())
