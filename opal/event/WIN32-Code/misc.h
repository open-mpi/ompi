#ifndef MISC_H
#define MISC_H

#include "opal/event/event.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

int gettimeofday(struct timeval *,struct timezone *);

OPAL_DECLSPEC extern void *win32_init	(void);
OPAL_DECLSPEC extern int win32_insert(struct win32op *win32op, struct opal_event *ev);
OPAL_DECLSPEC extern int win32_del(struct win32op *win32op, struct opal_event *ev);
OPAL_DECLSPEC extern int win32_dispatch(struct event_base *base, struct win32op *win32op,
	                                    struct timeval *tv);
OPAL_DECLSPEC extern int win32_recalc	(struct event_base *base, void *, int);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
