/*
 * Copyright (c) 2010-2011, Siberian State University of Telecommunications
 *                          and Information Sciences. All rights reserved.
 * Copyright (c) 2010-2011, A.V. Rzhanov Institute of Semiconductor Physics SB RAS.
 *                          All rights reserved.
 *
 * mpigclock.c: MPI clock synchronization.
 * http://mpiperf.cpct.sibsutis.ru/index.php
 *
 * Copyright (C) 2011 Mikhail Kurnosov <mkurnosov@gmail.com>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * Neither the name of the the copyright holders nor the
 * names of its contributors may be used to endorse or promote products
 * derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <stdio.h>
#include <stdlib.h>

#include <mpi.h>

#include "mpigclock.h"
#include "hpctimer.h"

#define INVALIDTIME -1.0
#define MPIGCLOCK_RTTMIN_NOTCHANGED_MAX 100
#define MPIGCLOCK_MSGTAG 128

static double mpigclock_measure_offset_adaptive(MPI_Comm comm, int root, int peer, double *min_rtt, double root_offset);


/*
 * mpigclock_sync_linear: Clock synchronization algorithm with O(n) steps.
 */
double mpigclock_sync_linear(MPI_Comm comm, int root, double *rtt)
{
    int peer, rank, commsize;
    double ret = 0;

    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &commsize);

    for (peer = 1; peer < commsize; peer++) {
        MPI_Barrier(comm);
        if (rank == root || rank == peer) {
            ret = mpigclock_measure_offset_adaptive(comm, root, peer, rtt, 0.0);
        }
    }
    return ret;
}

/*
 * mpigclock_sync_log: Clock synchronization algorithm with O(logn) steps.
 * rtt argumrnt does not have a meaning
 */
double mpigclock_sync_log(MPI_Comm comm, int root, double *rtt)
{
    int peer, rank, commsize;
    double root_offset = 0;
    double ret = 0;

    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &commsize);

    /* I am a peer */
    if (rank != root) {
        root = (rank - 1) / 2;
        MPI_Recv(&root_offset, 1, MPI_DOUBLE, root, MPIGCLOCK_MSGTAG, comm, MPI_STATUS_IGNORE);
        ret = mpigclock_measure_offset_adaptive(comm, root, rank, rtt, root_offset);
    }

    root_offset = ret;

    /* I am a root */
    *rtt = 0;
    peer = 2 * rank + 1;
    if (peer < commsize) {
        MPI_Send(&root_offset, 1, MPI_DOUBLE, peer, MPIGCLOCK_MSGTAG, comm);
        mpigclock_measure_offset_adaptive(comm, rank, peer, rtt, root_offset);
    }

    *rtt = 0;
    peer = 2 * rank + 2;
    if (peer < commsize) {
        MPI_Send(&root_offset, 1, MPI_DOUBLE, peer, MPIGCLOCK_MSGTAG, comm);
        mpigclock_measure_offset_adaptive(comm, rank, peer, rtt, root_offset);
    }

    return ret;
}

/* mpigclock_measure_offset_adaptive: Measures clock's offset of peer. */
static double mpigclock_measure_offset_adaptive(MPI_Comm comm, int root, int peer, double *min_rtt, double root_offset)
{
    int rank, commsize, rttmin_notchanged = 0;
    double starttime, stoptime, peertime, rtt, rttmin = 1E12,
           invalidtime = INVALIDTIME, offset;

    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &commsize);

    offset = 0.0;
    for (;;) {
        if (rank != root) {
            /* Peer process */
            starttime = hpctimer_wtime();
            MPI_Send(&starttime, 1, MPI_DOUBLE, root, MPIGCLOCK_MSGTAG, comm);
            MPI_Recv(&peertime, 1, MPI_DOUBLE, root, MPIGCLOCK_MSGTAG, comm,
                     MPI_STATUS_IGNORE);
            stoptime = hpctimer_wtime();
            rtt = stoptime - starttime;

            if (rtt < rttmin) {
                rttmin = rtt;
                rttmin_notchanged = 0;
                offset =  peertime - rtt / 2.0 - starttime;
            } else {
                if (++rttmin_notchanged == MPIGCLOCK_RTTMIN_NOTCHANGED_MAX) {
                    MPI_Send(&invalidtime, 1, MPI_DOUBLE, root, MPIGCLOCK_MSGTAG,
                             comm);
                    break;
                }
            }
        } else {
            /* Root process */
            MPI_Recv(&starttime, 1, MPI_DOUBLE, peer, MPIGCLOCK_MSGTAG, comm,
                     MPI_STATUS_IGNORE);
            peertime = hpctimer_wtime() + root_offset;
            if (starttime < 0.0) {
                break;
            }
            MPI_Send(&peertime, 1, MPI_DOUBLE, peer, MPIGCLOCK_MSGTAG, comm);
        }
    } /* for */

    if( rank != root ){
        *min_rtt = rttmin;
    } else {
        rtt = 0.0;
    }
    return offset;
}
