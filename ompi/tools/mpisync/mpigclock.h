/*
 * Copyright (c) 2010-2011, Siberian State University of Telecommunications
 *                          and Information Sciences. All rights reserved.
 * Copyright (c) 2010-2011, A.V. Rzhanov Institute of Semiconductor Physics SB RAS.
 *                          All rights reserved.
 *
 * mpigclock.h: MPI clock synchronization.
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
