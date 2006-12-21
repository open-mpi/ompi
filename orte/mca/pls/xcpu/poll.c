/*
 * Copyright (C) 2006 by Latchesar Ionkov <lucho@ionkov.net>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * LATCHESAR IONKOV AND/OR ITS SUPPLIERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/poll.h>
#include <errno.h>
#include <assert.h>
#include <limits.h>
#include "spfs.h"
//#include "spfsimpl.h"
#include "orte_config.h"
#include "opal/event/event.h"
#include "opal/runtime/opal_progress.h"

enum {
	TblModified	= 1,
	ChunkSize	= 4
};

enum {
	Readable	= 1,
	Writable	= 2,
	Error		= 4,

	Notifying	= 32,
	Removed		= 64
};

typedef struct Spolltbl Spolltbl;
struct Spolltbl {
	int		shutdown;
	int		looping;
	Spfd*		spfds;
};

struct Spfd {
	int		fd;
	opal_event_t	opevent;
	int		flags;
	int		events;
	void*		aux;
	void		(*notify)(Spfd *, void *);

	Spfd*		prev;
	Spfd*		next;
};

static Spolltbl ptbl;
static struct timeval tval = { 5, 0 };
static struct timeval *sptval = &tval;


static void spfd_handler(int fd, short event, void *aux);
static void sp_setup_event(Spfd *spfd);

void
sp_poll_stop()
{
	ptbl.shutdown = 1;
}

int
sp_poll_looping()
{
	return ptbl.looping;
}

Spfd *
spfd_add(int fd, void (*notify)(Spfd *, void *), void *aux)
{
	Spfd *spfd;

	spfd = sp_malloc(sizeof(*spfd));
	if (!spfd)
		return NULL;

//	fprintf(stderr, "spfd_add spfd %p fd %d\n", spfd, fd);
	fcntl(fd, F_SETFL, O_NONBLOCK);
	spfd->fd = fd;
	spfd->flags = 0;
	spfd->events = OPAL_EV_READ | OPAL_EV_WRITE;
	spfd->aux = aux;
	spfd->notify = notify;

	spfd->prev = NULL;
	spfd->next = ptbl.spfds;
	ptbl.spfds = spfd;

	sp_setup_event(spfd);
	return spfd;
}

void
spfd_remove(Spfd *spfd)
{
//	fprintf(stderr, "spfd_remove spfd %p\n", spfd);
	if (spfd->prev)
		spfd->prev->next = spfd->next;
	else
		ptbl.spfds = spfd->next;

	if (spfd->next)
		spfd->next->prev = spfd->prev;

	if (spfd->flags & Notifying)
		spfd->flags |= Removed;
	else {
		opal_event_del(&spfd->opevent);
		free(spfd);
	}
}

void
spfd_remove_all(void)
{
	Spfd *spfd, *spfd1;

	spfd = ptbl.spfds;
	while (spfd != NULL) {
		spfd1 = spfd->next;
		opal_event_del(&spfd->opevent);
		free(spfd);
		spfd = spfd1;
	}
}

int
spfd_can_read(Spfd *spfd)
{
	return spfd->flags & Readable;
}

int
spfd_can_write(Spfd *spfd)
{
	return spfd->flags & Writable;
}

int
spfd_has_error(Spfd *spfd)
{
	return spfd->flags & Error;
}

int
spfd_read(Spfd *spfd, void *buf, int buflen)
{
	int ret;

	if (buflen)
		ret = read(spfd->fd, buf, buflen);
	else
		ret = 0;

	spfd->flags &= ~Readable;
	spfd->events |= OPAL_EV_READ;
	if (!(spfd->flags & Notifying)) {
		opal_event_del(&spfd->opevent);
		sp_setup_event(spfd);
	}

	return ret;
}

int
spfd_write(Spfd *spfd, void *buf, int buflen)
{
	int ret;

	if (buflen)
		ret = write(spfd->fd, buf, buflen);
	else
		ret = 0;

	spfd->flags &= ~Writable;
	spfd->events |= OPAL_EV_WRITE;
	if (!(spfd->flags & Notifying)) {
		opal_event_del(&spfd->opevent);
		sp_setup_event(spfd);
	}

	return ret;
}

static void
spfd_handler(int fd, short event, void *aux)
{
	int flags, events;
	Spfd *spfd;

	spfd = aux;

//	fprintf(stderr, "spfd_handler spfd %p event %d events %d flags %d\n", spfd, event, spfd->events, spfd->flags);
	flags = spfd->flags;
	events = spfd->events;

	if (event & OPAL_EV_READ) {
		spfd->events &= ~OPAL_EV_READ;
		flags |= Readable;
	}

	if (event & OPAL_EV_WRITE) {
		spfd->events &= ~OPAL_EV_WRITE;
		flags |= Writable;
	}

	if (spfd->flags != flags) {
		spfd->flags = flags | Notifying;
		(*spfd->notify)(spfd, spfd->aux);
		spfd->flags &= ~Notifying;
	}

	if (spfd->flags & Removed) {
		free(spfd);
		return;
	}

	sp_setup_event(spfd);
}

static void
sp_setup_event(Spfd *spfd)
{
//	fprintf(stderr, "sp_setup_event ");
//	sp_printtime(stderr);
//	fprintf(stderr, " spfd %p events %d\n", spfd, spfd->events);
	opal_event_set(&spfd->opevent, spfd->fd, spfd->events, spfd_handler, spfd);
	opal_event_add(&spfd->opevent, sptval);
}

void
sp_poll_once(void)
{
	ptbl.looping = 1;
	opal_progress();
	ptbl.looping = 0;
}

void
sp_poll_loop()
{
	ptbl.shutdown = 0;
	ptbl.looping = 1;
	while (!ptbl.shutdown) {
		opal_progress();
	}
	ptbl.looping = 0;
}
