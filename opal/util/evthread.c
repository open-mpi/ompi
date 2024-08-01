/*
 * Copyright 2009-2012 Niels Provos and Nick Mathewson
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include "event2/event-config.h"
#include "opal/mca/threads/mutex.h"
#include "opal/mca/threads/tsd.h"
#include "opal/mca/threads/condition.h"
#include "opal/mca/threads/threads.h"
/* With glibc we need to define _GNU_SOURCE to get PTHREAD_MUTEX_RECURSIVE.
 * This comes from evconfig-private.h
 */

struct event_base;
#include "event2/thread.h"
#include "event2/util.h"
#include <stdlib.h>
#include <string.h>


static void *
evthread_opal_lock_alloc(unsigned locktype)
{
	opal_thread_internal_mutex_t *lock = malloc(sizeof(opal_thread_internal_mutex_t));
	if (!lock)
		return NULL;
	if (opal_thread_internal_mutex_init(lock, locktype & EVTHREAD_LOCKTYPE_RECURSIVE)) {
		free(lock);
		return NULL;
	}
	return lock;
}

static void
evthread_opal_lock_free(void *lock_, unsigned locktype)
{
	opal_thread_internal_mutex_t *lock = lock_;
	opal_thread_internal_mutex_destroy(lock);
	free(lock);
}

static int
evthread_opal_lock(unsigned mode, void *lock_)
{
	opal_mutex_t *lock = lock_;
	if (mode & EVTHREAD_TRY)
		return opal_mutex_trylock(lock);
	else {
		opal_mutex_lock(lock);
		return 1;
	}
}

static int
evthread_opal_unlock(unsigned mode, void *lock_)
{
	opal_mutex_t *lock = lock_;
 	opal_mutex_unlock(lock);
	return 1;
}

static unsigned long
evthread_opal_get_id(void)
{
	union {
		opal_thread_t thr;
#if EVENT__SIZEOF_PTHREAD_T > EVENT__SIZEOF_LONG
		ev_uint64_t id;
#else
		unsigned long id;
#endif
	} r;
#if EVENT__SIZEOF_PTHREAD_T < EVENT__SIZEOF_LONG
	memset(&r, 0, sizeof(r));
#endif
	r.thr = *opal_thread_get_self();
	return (unsigned long)r.id;
}

static void *
evthread_opal_cond_alloc(unsigned condflags)
{
	opal_cond_t *cond = malloc(sizeof(opal_cond_t));
	if (!cond)
		return NULL;
	if (opal_cond_init(cond)) {
		free(cond);
		return NULL;
	}
	return cond;
}

static void
evthread_opal_cond_free(void *cond_)
{
	opal_cond_t *cond = cond_;
	opal_cond_destroy(cond);
	free(cond);
}

static int
evthread_opal_cond_signal(void *cond_, int broadcast)
{
	opal_cond_t *cond = cond_;
	int r;
	if (broadcast)
		r = opal_cond_broadcast(cond);
	else
		r = opal_cond_signal(cond);
	return r ? -1 : 0;
}

static int
evthread_opal_cond_wait(void *cond_, void *lock_, const struct timeval *tv)
{
	int r;
	opal_condition_t *cond = cond_;
	opal_mutex_t *lock = lock_;

	if (tv) {
		struct timeval now, abstime;
		struct timespec ts;
		evutil_gettimeofday(&now, NULL);
		evutil_timeradd(&now, tv, &abstime);
		ts.tv_sec = abstime.tv_sec;
		ts.tv_nsec = abstime.tv_usec*1000;
		r = opal_condition_timedwait(cond, lock, &ts);
		if (r == ETIMEDOUT)
			return 1;
		else if (r)
			return -1;
		else
			return 0;
	} else {
		r = opal_cond_wait(cond, lock);
		return r ? -1 : 0;
	}
}

int
opal_event_use_threads(void)
{
	struct evthread_lock_callbacks cbs = {
		EVTHREAD_LOCK_API_VERSION,
		EVTHREAD_LOCKTYPE_RECURSIVE,
		evthread_opal_lock_alloc,
		evthread_opal_lock_free,
		evthread_opal_lock,
		evthread_opal_unlock
	};
	struct evthread_condition_callbacks cond_cbs = {
		EVTHREAD_CONDITION_API_VERSION,
		evthread_opal_cond_alloc,
		evthread_opal_cond_free,
		evthread_opal_cond_signal,
		evthread_opal_cond_wait
	};


	evthread_set_lock_callbacks(&cbs);
	evthread_set_condition_callbacks(&cond_cbs);
	evthread_set_id_callback(evthread_opal_get_id);
	return 0;
}
