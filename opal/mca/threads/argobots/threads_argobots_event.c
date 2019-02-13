
#include "opal/mca/threads/threads.h"
#include "opal/mca/threads/argobots/threads_argobots.h"
#include "opal/mca/event/libevent2022/libevent/include/event2/thread.h"
#include "opal/mca/event/libevent2022/libevent/include/event2/event-config.h"
#include "opal/mca/event/libevent2022/libevent/include/event2/util.h"

#include <abt.h>

static void *
evthread_argobots_lock_alloc(unsigned locktype)
{
    ABT_mutex lock;
	if (locktype & EVTHREAD_LOCKTYPE_RECURSIVE) {
        ABT_mutex_attr abt_mutex_attr;
        ABT_mutex_attr_create(&abt_mutex_attr);
        ABT_mutex_attr_set_recursive(abt_mutex_attr, ABT_TRUE);
        ABT_mutex_create_with_attr(abt_mutex_attr, &lock);
        ABT_mutex_attr_free(&abt_mutex_attr);
    } else {
        ABT_mutex_create(&lock);
    }
	return lock;
}

static void
evthread_argobots_lock_free(void *_lock, unsigned locktype)
{
	ABT_mutex lock = _lock;
	ABT_mutex_free(&lock);
}

static int
evthread_argobots_lock(unsigned mode, void *_lock)
{
	int ret;
	ABT_mutex lock = _lock;
	if (mode & EVTHREAD_TRY) {
		ret = ABT_mutex_trylock(lock);
	} else {
		ret = ABT_mutex_lock(lock);
	}
	return ret;
}

static int
evthread_argobots_unlock(unsigned mode, void *_lock)
{
	ABT_mutex lock = _lock;
	int ret = ABT_mutex_unlock(lock);
	/* This yield is necessary to avoid taking a lock consecutively. */
	ABT_thread_yield();
	return ret;
}

static unsigned long
evthread_argobots_get_id(void)
{
	ABT_thread thr;
	ABT_thread_self(&thr);
	return (unsigned long)((intptr_t)thr);
}

static void *
evthread_argobots_cond_alloc(unsigned condflags)
{
	ABT_cond cond;
	ABT_cond_create(&cond);
	return cond;
}

static void
evthread_argobots_cond_free(void *_cond)
{
	ABT_cond cond = _cond;
	ABT_cond_free(&cond);
}

static int
evthread_argobots_cond_signal(void *_cond, int broadcast)
{
	ABT_cond cond = _cond;
	int r;
	if (broadcast)
		r = ABT_cond_broadcast(cond);
	else
		r = ABT_cond_signal(cond);
	return r ? -1 : 0;
}

static int
evthread_argobots_cond_wait(void *_cond, void *_lock, const struct timeval *tv)
{
	int r;
	ABT_cond cond = _cond;
	ABT_mutex lock = _lock;

	if (tv) {
		struct timeval now, abstime;
		struct timespec ts;
		evutil_gettimeofday(&now, NULL);
		evutil_timeradd(&now, tv, &abstime);
		ts.tv_sec = abstime.tv_sec;
		ts.tv_nsec = abstime.tv_usec*1000;
		r = ABT_cond_timedwait(cond, lock, &ts);
		if (r != 0)
			return 1;
		else
			return 0;
	} else {
		r = ABT_cond_wait(cond, lock);
		return r ? -1 : 0;
	}
}

void opal_event_use_threads(void) {
	struct evthread_lock_callbacks cbs = {
		EVTHREAD_LOCK_API_VERSION,
		EVTHREAD_LOCKTYPE_RECURSIVE,
		evthread_argobots_lock_alloc,
		evthread_argobots_lock_free,
		evthread_argobots_lock,
		evthread_argobots_unlock
	};
	struct evthread_condition_callbacks cond_cbs = {
		EVTHREAD_CONDITION_API_VERSION,
		evthread_argobots_cond_alloc,
		evthread_argobots_cond_free,
		evthread_argobots_cond_signal,
		evthread_argobots_cond_wait
	};
	ensure_init_argobots();
	evthread_set_lock_callbacks(&cbs);
	evthread_set_condition_callbacks(&cond_cbs);
	evthread_set_id_callback(evthread_argobots_get_id);
}
