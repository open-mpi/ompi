/*
 * Copyright (c) 2010-2011, Siberian State University of Telecommunications
 *                          and Information Sciences. All rights reserved.
 * Copyright (c) 2010-2011, A.V. Rzhanov Institute of Semiconductor Physics SB RAS.
 *                          All rights reserved.
 *
 * hpctimer.c: High-Resolution timers library.
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

#include <sys/time.h>
#include <unistd.h>

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <math.h>
#include <inttypes.h>

#include <mpi.h>

#include "hpctimer.h"

#define NELEMS(v) (sizeof(v) / sizeof((v)[0]))

/*
 * Compilers macro:
 * __GNUC__ - GCC
 * __SUNPRO_C - Solaris Studio
 * __INTEL_COMPILER - Intel C++ Compiler
 * __xlC__ || __IBMC__ - IBM C Compiler
 * __PATHSCALE__ - PathScale Compiler
 * __PGI - PGI Compiler
 * __DECC - DEC Compiler
 * __HP_cc - HP Compiler
 * __SX - NEC SX Compiler
 * __COMO__ - Comeau C++
 * _CRAYC - Cray C Compiler
 * sgi || __sgi - SGI Compiler
 */

#if defined(__GNUC__)
#   define __inline__ __inline__
#   define __asm__ __asm__
#   define __volatile__ __volatile__
#elif defined(__SUNPRO_C)
#   define __inline__ __inline__
#   define __asm__ __asm__
#   define __volatile__ __volatile__
#endif

typedef int (*hpctimer_initialize_func_ptr_t)(void);
typedef void (*hpctimer_finalize_func_ptr_t)(void);
typedef int (*hpctimer_isimplemented_func_ptr_t)(void);
typedef double (*hpctimer_wtime_func_ptr_t)(void);

typedef struct hpctimer {
    char *name;
    hpctimer_initialize_func_ptr_t initialize;
    hpctimer_finalize_func_ptr_t finalize;
    hpctimer_isimplemented_func_ptr_t isimplemented;
    hpctimer_wtime_func_ptr_t wtime;
} hpctimer_t;

static uint64_t hpctimer_overhead;  /* Timer overhead (seconds) */
static uint64_t hpctimer_freq;      /* Timer frequency (ticks per usec) */

static double hpctimer_wtime_tsc(void);
static int hpctimer_tsc_initialize(void);
static __inline__ uint64_t hpctimer_gettsc(void);
static uint64_t hpctimer_measure_overhead(void);
static uint64_t hpctimer_calibrate_sleep(uint64_t overhead);
static double hpctimer_wtime_gettimeofday(void);

/*
 * Timers
 */
static hpctimer_t hpctimer_timers[] = {
    {"MPI_Wtime", NULL, NULL, NULL, MPI_Wtime},
    {"gettimeofday", NULL, NULL, NULL, hpctimer_wtime_gettimeofday},
    {"tsc", hpctimer_tsc_initialize, NULL, NULL, hpctimer_wtime_tsc}
};

static hpctimer_wtime_func_ptr_t hpctimer_wtime_func_ptr = NULL;
static int hpctimer_timer = -1;

/* hpctimer_initialize: */
int hpctimer_initialize(const char *timername)
{
    hpctimer_wtime_func_ptr = NULL;
    hpctimer_timer = -1;
    unsigned int i;
    for (i = 0; i < NELEMS(hpctimer_timers); i++) {
        if (hpctimer_timers[i].isimplemented != NULL) {
            if (!hpctimer_timers[i].isimplemented()) {
                continue;
            }
        }
        if (strcasecmp(timername, hpctimer_timers[i].name) == 0) {
            hpctimer_wtime_func_ptr = hpctimer_timers[i].wtime;
            hpctimer_timer = i;
            if (hpctimer_timers[i].initialize) {
                return hpctimer_timers[i].initialize();
            }
            return HPCTIMER_SUCCESS;
        }
    }
    return HPCTIMER_FAILURE;
}

/* hpctimer_finalize: */
void hpctimer_finalize(void)
{
    if (hpctimer_timers[hpctimer_timer].finalize) {
        hpctimer_timers[hpctimer_timer].finalize();
    }
    hpctimer_wtime_func_ptr = NULL;
}

/* hpctimer_print_timers: */
void hpctimer_print_timers(void)
{
    unsigned int i;

    printf("Supported timers:\n");
    for (i = 0; i < NELEMS(hpctimer_timers); i++) {
        if (hpctimer_timers[i].isimplemented != NULL) {
            if (!hpctimer_timers[i].isimplemented()) {
                continue;
            }
        }
        printf("    %s\n", hpctimer_timers[i].name);
    }
}

/*
 * hpctimer_sanity_check: Returns 1 if the results of measures
 *                        by timer are correct.
 */
int hpctimer_sanity_check(void)
{
    enum { NTESTS = 4 };
    double start, stop, currtime, prevtime = 0.0, err = 0.05;
    int sanity = 1;

    int delay = 0;
    for (delay = 1; delay < NTESTS; delay++) {
        start = hpctimer_wtime();
        sleep(delay);
        stop = hpctimer_wtime();
        currtime = stop - start;
        if (delay > 1) {
            if (fabs(prevtime - currtime / delay) > prevtime * err) {
                sanity = 0;
            }
            /*
            printf("# timer sleep %d sec.; timer result: %.6f; diff: %.6f\n",
                   delay - 1, currtime / delay, fabs(prevtime - currtime / delay));
            */
        }
        prevtime = currtime / delay;
    }
    return sanity;
}

/* hpctimer_wtime: Returns walltime in seconds. */
double hpctimer_wtime(void)
{
    return hpctimer_wtime_func_ptr();
}

/* hpctimer_wtime_gettimeofday: */
static double hpctimer_wtime_gettimeofday(void)
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (double)tv.tv_sec + 1E-6 * tv.tv_usec;
}

/*
 * hpctimer_wtime_tsc: Returns TSC-based walltime in seconds.
 */
static double hpctimer_wtime_tsc(void)
{
    return (double)(hpctimer_gettsc() - hpctimer_overhead) / (double)hpctimer_freq;
}

/*
 * hpctimer_tsc_initialize: Initializes TSC-based timer.
 *
 * The code is based on recommendations from manual of Intel Corp.
 * "Using the RDTSC Instruction for Performance Monitoring".
 */
static int hpctimer_tsc_initialize(void)
{
    hpctimer_overhead = hpctimer_measure_overhead();
    hpctimer_freq = hpctimer_calibrate_sleep(hpctimer_overhead);
    return HPCTIMER_SUCCESS;
}

/*
 * hpctimer_gettsc: Returns TSC value.
 */
static __inline__ uint64_t hpctimer_gettsc(void)
{
#if defined(__x86_64__)
    uint32_t low, high;
    __asm__ __volatile__(
        "xorl %%eax, %%eax\n"
        "cpuid\n"
        ::: "%rax", "%rbx", "%rcx", "%rdx"
    );
    __asm__ __volatile__(
        "rdtsc\n"
        : "=a" (low), "=d" (high)
    );
    return ((uint64_t)high << 32) | low;

#elif defined(__i386__)
    uint64_t tsc;
    __asm__ __volatile__(
        "xorl %%eax, %%eax\n"
        "cpuid\n"
        ::: "%eax", "%ebx", "%ecx", "%edx"
    );
    __asm__ __volatile__(
        "rdtsc\n"
        : "=A" (tsc)
    );
    return tsc;
#else
#   error "Unsupported platform"
#endif
}

/* hpctimer_measure_overhead: Returns overhead of TSC reading (in tics). */
static uint64_t hpctimer_measure_overhead(void)
{
    enum {
        TSC_OVERHEAD_NTESTS = 10
    };
    int i;
    uint64_t count, overhead = (uint64_t)~0x01;

    /* Make warm-up passes and determine timer overhead */
    for (i = 0; i < TSC_OVERHEAD_NTESTS; i++) {
        count = hpctimer_gettsc();
        count = hpctimer_gettsc() - count;
        if (count < overhead) {
            overhead = count;
        }
    }
    return overhead;
}

/*
 * hpctimer_calibrate_adaptive: Returns number of TSC tics per second.
 *                              Adaptive algorithm based on sleep.
 */
/*
static uint64_t hpctimer_calibrate_adaptive(uint64_t overhead)
{
    enum {
        TSC_CALIBRATE_NTESTS = 2
    };
    int i;
    uint64_t count, freq;

    freq = (uint64_t)(~0x01);
    for (i = 0; i < TSC_CALIBRATE_NTESTS; i++) {
        count = hpctimer_gettsc();
        sleep(1);
        count = hpctimer_gettsc() - count - overhead;
        if (count < 0)
            count = 0;
        if (count < freq) {
            freq = count;
            i = 0;
        }
    }
    return freq;
}
*/

/*
 * hpctimer_calibrate_sleep: Returns number of TSC tics per second.
 */
static uint64_t hpctimer_calibrate_sleep(uint64_t overhead)
{
    uint64_t count;
    int delay = 3;

    count = hpctimer_gettsc();
    sleep(delay);
    count = hpctimer_gettsc() - count - overhead;
    return count / delay;
}

/*
 * hpctimer_calibrate_loop: Returns number of TSC tics per second.
 */
/*
static uint64_t hpctimer_calibrate_loop(uint64_t overhead)
{
    enum {
        TSC_CALIBRATE_NTESTS = 2
    };
    uint64_t count, countmin = (uint64_t)~0x01;
    struct timeval tv1, tv2;
    int i, j;
    __volatile__ int dummy = 0;

    for (i = 0; i < TSC_CALIBRATE_NTESTS; i++) {
        gettimeofday(&tv1, NULL);
        count = hpctimer_gettsc();
        for (j = 0; j < 10000000; j++) {
            dummy++;
        }
        count = hpctimer_gettsc() - count - overhead;
        gettimeofday(&tv2, NULL);
        if (count < 0)
            count = 0;
        if (count < countmin)
            countmin = count;
    }
    return countmin * 1000000 / (tv2.tv_sec * 1000000 + tv2.tv_usec -
                                 tv1.tv_sec * 1000000 - tv1.tv_usec);
}
*/
