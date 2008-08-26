/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich GmbH, Federal
 * Republic of Germany
 *
 * See the file COPYRIGHT in the package base directory for details
 **/

#include "config.h"

#include "vt_pform.h"

#include <sys/time.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>
#include <intrinsics.h>
#include <sys/types.h>
#include <sys/sv2/apteamctl.h>

#ifndef TIMER_PAPI_REAL_CYC
#  define TIMER_PAPI_REAL_CYC 10
#endif
#ifndef TIMER_PAPI_REAL_USEC
#  define TIMER_PAPI_REAL_USEC 11
#endif

#if TIMER != TIMER_GETTIMEOFDAY && \
    TIMER != TIMER_RTC && \
    TIMER != TIMER_PAPI_REAL_CYC && \
    TIMER != TIMER_PAPI_REAL_USEC
#  error Unknown timer specified! Check the timer configuration in 'config.h'.
#endif

#if TIMER == TIMER_RTC
  static uint64_t vt_rtc_base = 0;
  static uint64_t vt_ticks_per_sec = 1;
# pragma omp threadprivate(vt_rtc_base)
#elif TIMER == TIMER_GETTIMEOFDAY
  static uint64_t vt_time_base = 0;
#elif TIMER == TIMER_PAPI_REAL_CYC
# include <vt_metric.h>
#elif TIMER == TIMER_PAPI_REAL_USEC
# include <vt_metric.h>
  static uint64_t vt_time_base = 0;
#endif

static long vt_nodeid = -1L;
static int vt_mspmode = 1;

/* platform specific initialization */
void vt_pform_init() {
  struct timeval tp;
  static ApTeam_t app;
  int i;

#if TIMER == TIMER_RTC
  vt_ticks_per_sec = (uint64_t)sysconf(_SC_SV2_USER_TIME_RATE);
# pragma omp parallel
  {
    gettimeofday(&tp, 0);
    vt_rtc_base = _rtc();
  }
#elif TIMER == TIMER_GETTIMEOFDAY
  gettimeofday(&tp, 0);
  vt_time_base = tp.tv_sec - (tp.tv_sec & 0xFFFF);
#elif TIMER == TIMER_PAPI_REAL_USEC
  vt_time_base = vt_metric_real_usec();
#endif

  if (apteamctl(ApTeam_Status, 0, 0, &app) == 1) {
    vt_mspmode = (app.flags & APTEAM_MSP);
    app.pes = (ApPe_t*)malloc(app.pecount * sizeof(ApPe_t));
    if (apteamctl(ApTeam_Status, 0, 0, &app) == 1) {
      for (i=0; i<_num_pes(); i++) {
        if (_my_pe() == app.pes[i].lpe) vt_nodeid = (long)app.pes[i].place;
      }
    }
  }
}

/* directory of global file system  */
char* vt_pform_gdir() {
  return ".";
}

/* directory of local file system  */
char* vt_pform_ldir() {
  #ifdef PFORM_LDIR
    return PFORM_LDIR;
  #else
    return "/tmp";
  #endif
}

/* clock resolution */
uint64_t vt_pform_clockres() {
#if TIMER == TIMER_RTC
  return vt_ticks_per_sec;
#elif TIMER == TIMER_GETTIMEOFDAY
  return 1e6;
#elif TIMER == TIMER_PAPI_REAL_CYC
  return vt_metric_clckrt();
#elif TIMER == TIMER_PAPI_REAL_USEC
  return 1e6;
#endif
}

/* local or global wall-clock time */
uint64_t vt_pform_wtime() {
#if TIMER == TIMER_RTC
  return (uint64_t)_rtc() - vt_rtc_base;
#elif TIMER == TIMER_GETTIMEOFDAY
  struct timeval tp;
  gettimeofday(&tp, 0);
  return ((tp.tv_sec - vt_time_base) * 1e6) + tp.tv_usec;
#elif TIMER == TIMER_PAPI_REAL_CYC
  return vt_metric_real_cyc();
#elif TIMER == TIMER_PAPI_REAL_USEC
  return vt_metric_real_usec() - vt_time_base;
#endif
}

/* unique numeric SMP-node identifier */
long vt_pform_node_id() {
  return vt_nodeid;
}

/* unique string SMP-node identifier */
char* vt_pform_node_name() {
  static char node[20];
  sprintf(node, "node%03d", vt_nodeid);
  return node;              
}

/* number of CPUs */
int vt_pform_num_cpus() {
  if (vt_mspmode)
    return 4;
  else
    return 16;
}
