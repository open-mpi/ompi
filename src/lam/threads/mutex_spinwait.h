/*
 * Copyright 2002-2003. The Regents of the University of California. This material
 * was produced under U.S. Government contract W-7405-ENG-36 for Los Alamos
 * National Laboratory, which is operated by the University of California for
 * the U.S. Department of Energy. The Government is granted for itself and
 * others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
 * license in this material to reproduce, prepare derivative works, and
 * perform publicly and display publicly. Beginning five (5) years after
 * October 10,2002 subject to additional five-year worldwide renewals, the
 * Government is granted for itself and others acting on its behalf a paid-up,
 * nonexclusive, irrevocable worldwide license in this material to reproduce,
 * prepare derivative works, distribute copies to the public, perform publicly
 * and display publicly, and to permit others to do so. NEITHER THE UNITED
 * STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR THE UNIVERSITY OF
 * CALIFORNIA, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR
 * IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY,
 * COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR
 * PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY
 * OWNED RIGHTS.
                                                                                                     
 * Additionally, this program is free software; you can distribute it and/or
 * modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2 of the License,
 * or any later version.  Accordingly, this program is distributed in the hope
 * that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef _MUTEX_SPINWAIT_
#define _MUTEX_SPINWAIT_

#include <pthread.h>
#include "os/atomic.h"

#ifndef MUTEX_SPINWAIT
#define MUTEX_SPINWAIT 10000
#endif


typedef struct lam_mutex {
     volatile int     mtx_spinlock;
     volatile int     mtx_waiting;
     pthread_mutex_t  mtx_lock;
     pthread_cond_t   mtx_cond;
} lam_mutex_t;


static inline void lam_mtx_init(lam_mutex_t* m) 
{
    m->mtx_spinlock = 0;
    m->mtx_waiting = 0;
    pthread_mutex_init(&m->mtx_lock, 0);
    pthread_cond_init(&m->mtx_cond, 0);
}


static inline void lam_mtx_lock(lam_mutex_t* m)
{
    unsigned long cnt = 0;
    int locked;

    fetchNadd(&m->mtx_waiting, 1);
    while((locked = fetchNset(&m->mtx_spinlock, 1)) == 1 && cnt++ < MUTEX_SPINWAIT)
        ;
    if(locked) {
        pthread_mutex_lock(&m->mtx_lock);
        while(fetchNset(&m->mtx_spinlock, 1) == 1)
            pthread_cond_wait(&m->mtx_cond, &m->mtx_lock);
        pthread_mutex_unlock(&m->mtx_lock);
    }
    fetchNadd(&m->mtx_waiting, -1);
}


static inline int lam_mtx_trylock(lam_mutex_t* m)
{
    return (fetchNset(&m->mtx_spinlock, 1) == 0);
}


static inline void lam_mtx_unlock(lam_mutex_t* m)
{
    fetchNset(&m->mtx_spinlock, 0); 
    if(m->mtx_waiting) {
        pthread_cond_signal(&m->mtx_cond);
    }
}

#endif

