/*
 * Copyright (c) 2010-2011, Siberian State University of Telecommunications 
 *                         and Information Sciences. All rights reserved.
 * Copyright (c) 2010-2011, A.V. Rzhanov Institute of Semiconductor Physics SB RAS.
 *                        All rights reserved.
 *
 * hpctimer.h: High-Resolution timers library.
 *
 * Copyright (C) 2011 Mikhail Kurnosov <mkurnosov@gmail.com>
 *
 * This source code is part of MPIPerf project: http://mpiperf.cpct.sibsutis.ru/index.php/Main/Documentation
 */

#ifndef HPCTIMER_H
#define HPCTIMER_H

#ifdef __cplusplus
extern "C" {
#endif

enum {
    HPCTIMER_SUCCESS = 0,
    HPCTIMER_FAILURE = 1
};

int hpctimer_initialize(const char *timername);
void hpctimer_finalize(void);
double hpctimer_wtime(void);
int hpctimer_sanity_check(void);
void hpctimer_print_timers(void);

#ifdef __cplusplus
}
#endif

#endif /* HPCTIMER_H */
