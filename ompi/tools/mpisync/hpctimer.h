/*
 * Copyright (c) 2010-2011, Siberian State University of Telecommunications
 *                          and Information Sciences. All rights reserved.
 * Copyright (c) 2010-2011, A.V. Rzhanov Institute of Semiconductor Physics SB RAS.
 *                          All rights reserved.
 *
 * hpctimer.h: High-Resolution timers library.
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
