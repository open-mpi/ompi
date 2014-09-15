#ifndef MPIGCLOCK_H
#define MPIGCLOCK_H

#include <stdio.h>
#include "hpctimer.h"

#define INVALIDTIME -1.0
#define MPIGCLOCK_RTTMIN_NOTCHANGED_MAX 100
#define MPIGCLOCK_MSGTAG 128

/* mpigclock_measure_offset_adaptive: Measures clock's offset of peer. */
double mpigclock_sync_linear(MPI_Comm comm, int root, double *rtt);

#endif