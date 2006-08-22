#ifndef MISC_H
#define MISC_H

#include "opal/event/event.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

int gettimeofday(struct timeval *,struct timezone *);

OPAL_DECLSPEC void *win32_init	(void);
OPAL_DECLSPEC int win32_insert(struct win32op *win32op, opal_event *ev);
OPAL_DECLSPEC int win32_del(struct win32op *win32op, opal_event *ev);
OPAL_DECLSPEC int win32_dispatch(struct event_base *base, struct win32op *win32op,
	                             struct timeval *tv);
OPAL_DECLSPEC int win32_recalc	(struct event_base *base, void *, int);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
