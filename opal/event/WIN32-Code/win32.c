/*
 * Copyright 2000-2002 Niels Provos <provos@citi.umich.edu>
 * Copyright 2003 Michael A. Davis <mike@datanerds.net>
 * All rights reserved.
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
#include "opal_config.h"
#ifdef _MSC_VER
#include "./config.h"
#else
/* Avoid the windows/msvc thing. */
#include "../config.h"
#endif

#include <winsock2.h>
#include <windows.h>
#include <sys/types.h>
#include <sys/queue.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <assert.h>

#define RB_AUGMENT(x) (void)(x)
#include "./tree.h"
#include "opal/event/event_rename.h"
#include "opal/event/log.h"
#include "opal/event/event.h"
#include "opal/event/event-internal.h"
#include "opal/event/WIN32-Code/misc.h"

#define XFREE(ptr) do { if (ptr) free(ptr); } while(0)

extern struct event_list timequeue;
extern struct event_list addqueue;
extern struct event_list signalqueue;


struct win_fd_set {
    u_int fd_count;
    SOCKET fd_array[1];
};

int evsigcaught[NSIG];
volatile sig_atomic_t signal_caught = 0;
/* MSDN says this is required to handle SIGFPE */
volatile double SIGFPE_REQ = 0.0f;

static void signal_handler(int sig);
void signal_process(struct event_base *base);
int signal_recalc(struct event_base *base);

struct event_entry {
    RB_ENTRY(event_entry) node;
    SOCKET sock;
    int read_pos;
    int write_pos;
    struct event *read_event;
    struct event *write_event;
};

static int
compare(struct event_entry *a, struct event_entry *b)
{
    if (a->sock < b->sock)
        return -1;
    else if (a->sock > b->sock)
        return 1;
    else
        return 0;
}

struct win32op {
    int fd_setsz;
    struct win_fd_set *readset_in;
    struct win_fd_set *writeset_in;
    struct win_fd_set *readset_out;
    struct win_fd_set *writeset_out;
    struct win_fd_set *exset_out;
    int n_events;
    int n_events_alloc;
    opal_event_t **events;
};


void *win32_init    (struct event_base *);
int win32_insert    (void *, struct event *);
int win32_del    (void *, struct event *);
int win32_dispatch    (struct event_base *base, void *, struct timeval *);
void win32_dealloc    (struct event_base *, void *);

struct eventop win32ops = {
	"win32",
	win32_init,
	win32_insert,
	win32_del,
	win32_dispatch,
	win32_dealloc,
	0
};


#define FD_SET_ALLOC_SIZE(n) ((sizeof(struct win_fd_set) + ((n)-1)*sizeof(SOCKET)))

#define NEVENT 64
void *
win32_init(void)
{
    struct win32op *winop;
    size_t size;
    if (!(winop = (struct win32op*)calloc(1, sizeof(struct win32op))))
        return NULL;
    winop->fd_setsz = NEVENT;
    size = FD_SET_ALLOC_SIZE(NEVENT);
    if (!(winop->readset_in = (struct win_fd_set*)malloc(size)))
        goto err;
    if (!(winop->writeset_in = (struct win_fd_set*)malloc(size)))
        goto err;
    if (!(winop->readset_out = (struct win_fd_set*)malloc(size)))
        goto err;
    if (!(winop->writeset_out = (struct win_fd_set*)malloc(size)))
        goto err;
    if (!(winop->exset_out = (struct win_fd_set*)malloc(size)))
        goto err;
    winop->n_events = 0;
    winop->n_events_alloc = NEVENT;
    if (!(winop->events = (opal_event_t**)malloc(NEVENT*sizeof(opal_event_t*))))
        goto err;
    winop->readset_in->fd_count = winop->writeset_in->fd_count = 0;
    winop->readset_out->fd_count = winop->writeset_out->fd_count
        = winop->exset_out->fd_count = 0;

    return (winop);

 err:
    XFREE(winop->readset_in);
    XFREE(winop->writeset_in);
    XFREE(winop->readset_out);
    XFREE(winop->writeset_out);
    XFREE(winop->exset_out);
    XFREE(winop->events);
    XFREE(winop);
    return (NULL);
}

int
win32_recalc(struct event_base *base, void *arg, int max)
{
    return 0/*(signal_recalc())*/;
}

static bool win32_is_fd_a_socket( int fd )
{
    int error;
    u_long value = 123456;

    if( SOCKET_ERROR == WSAHtonl( fd, value, &value ) ) {
        error = WSAGetLastError();
        return false;
    }
    return true;
}


void CALLBACK win32_socket_event_callback( void* lpParameter, BOOLEAN TimerOrWaitFired )
{
    opal_event_t* master = (opal_event_t*)lpParameter;
    WSANETWORKEVENTS network_events;
    int got, error;
    opal_event_t* next;

    assert( FALSE == TimerOrWaitFired );

    /* The handle will be automatically reset */
    if( SOCKET_ERROR == WSAEnumNetworkEvents( master->ev_fd, master->base_handle, &network_events ) ) {
        error = WSAGetLastError();
        return;
    }

    do {
        got = 0;
        next = master->ev_similar;
        if( network_events.lNetworkEvents  & (FD_READ | FD_ACCEPT) ) {
            if( master->ev_events & OPAL_EV_READ ) {
                network_events.lNetworkEvents &= ~(FD_READ | FD_ACCEPT);
                got |= OPAL_EV_READ;
            }
        }
        if( master->ev_events & OPAL_EV_WRITE ) {
            got |= OPAL_EV_WRITE;
            if( 0 == WSASetEvent(master->base_handle) ) {
                int error = WSAGetLastError();
            }
        }

        if( got ) {
            if (!(master->ev_events & OPAL_EV_PERSIST)) {
                opal_event_del(master);
            }

            SleepEx(1, true);
            (*master->ev_callback)((int)master->ev_fd, got, master->ev_arg);
            /*opal_event_active( ev, got, 1 );*/
        }
        master = next;
    } while( master != ((opal_event_t*)lpParameter) );
}

void CALLBACK win32_file_event_callback( void* lpParameter, BOOLEAN TimerOrWaitFired )
{
    opal_event_t* ev = (opal_event_t*)lpParameter;
    int got = 0;

    assert( FALSE == TimerOrWaitFired );

    if( ev->ev_events & OPAL_EV_READ ) {
        got |= OPAL_EV_READ;
    }
    if( ev->ev_events & OPAL_EV_WRITE ) {
        got |= OPAL_EV_WRITE;
    }

    if (!(ev->ev_events & OPAL_EV_PERSIST)) {
        opal_event_del(ev);
    }

    if( got ) {
        /*(*ev->ev_callback)((int)ev->ev_fd, got, ev->ev_arg);*/
        opal_event_active( ev, got, 1 );
    }
}

static int win32_recompute_event( opal_event_t* master )
{
    long flags = FD_CLOSE;
    opal_event_t* temp;
    int error;

    if( INVALID_HANDLE_VALUE == master->base_handle ) {
        master->base_handle = WSACreateEvent();
        if( INVALID_HANDLE_VALUE == master->base_handle ) {
            return 0;
        }
    }

    /* Compute the flags we're looking at */
    temp = master;
    do {
        if( temp->ev_events & OPAL_EV_READ )  flags |= FD_READ | FD_ACCEPT;
        if( temp->ev_events & OPAL_EV_WRITE ) flags |= FD_WRITE | FD_CONNECT;
        temp = temp->ev_similar;
    } while( temp != master );

    if( SOCKET_ERROR == WSAEventSelect( master->ev_fd, master->base_handle, flags ) ) {
        error = WSAGetLastError();
        WSACloseEvent( master->base_handle );
        master->base_handle = INVALID_HANDLE_VALUE;
        return 0;
    }
    if( INVALID_HANDLE_VALUE == master->registered_handle ) {
        if( 0 == RegisterWaitForSingleObject( &master->registered_handle, master->base_handle, win32_socket_event_callback,
                                              (void*)master, INFINITE, WT_EXECUTEINWAITTHREAD ) ) {
            error = GetLastError();
            WSACloseEvent( master->base_handle );
            master->base_handle = INVALID_HANDLE_VALUE;
            master->registered_handle = INVALID_HANDLE_VALUE;
            return 0;
        }
    }
    if( flags & FD_WRITE ) {
        if( 0 == WSASetEvent(master->base_handle) ) {
            error = WSAGetLastError();
        }
    }
    return 1;
}


int
win32_insert(struct win32op *win32op, opal_event_t *ev)
{
    int i;
    opal_event_t* master = NULL;

    if (ev->ev_events & OPAL_EV_SIGNAL) {
        if (ev->ev_events & (OPAL_EV_READ|OPAL_EV_WRITE))
            event_errx(1, "%s: EV_SIGNAL incompatible use",
                       __func__);
        if( signal(OPAL_EVENT_SIGNAL(ev), signal_handler) == SIG_ERR)
            return (-1);

        return (0);
    }

    /**
     * Find a place for the current event.
     */
    for( i = 0; i < win32op->n_events; ++i ) {
        if( win32op->events[i]->ev_fd != ev->ev_fd ) continue;
        master = win32op->events[i];
        if( master == ev ) {
            event_debug( ("%s: Event for %d already inserted.",
                         __func__, (int)ev->ev_fd));
            return (0);
        }

        if( master->ev_events & ev->ev_events ) {
            event_debug( ("%d Event for %d already have a similar event posted.",
                          __func__, (int)ev->ev_fd) );
        }
        ev->ev_similar = master->ev_similar;
        master->ev_similar = ev;
        break;
    }
    if( NULL == master ) {
        event_debug(("%s: adding event for %d", __func__, (int)ev->ev_fd));

        if (win32op->n_events_alloc == win32op->n_events) {
            size_t sz;
            win32op->n_events_alloc *= 2;
            sz = sizeof(opal_event_t*) * win32op->n_events_alloc;
            if (!(win32op->events = (opal_event_t**)realloc(win32op->events, sz)))
                return (-1);
        }
        win32op->events[win32op->n_events++] = ev;
        ev->ev_similar = ev;
        master = ev;
    }
    ev->base_handle       = INVALID_HANDLE_VALUE;
    ev->registered_handle = INVALID_HANDLE_VALUE;
    /**
     * Decide if we have a socket or a normal file descriptor. If it's a socket
     * create a WSA event otherwise a normal file event will be what we need.
     */
    if( win32_is_fd_a_socket(ev->ev_fd) ) {
        win32_recompute_event( master );
    } else {
        /*
        if( ev->ev_events & OPAL_EV_READ )  flags |= FD_READ;
        if( ev->ev_events & OPAL_EV_WRITE ) flags |= FD_WRITE;
        ev->base_handle = (HANDLE)_get_osfhandle( ev->ev_fd );
        if( INVALID_HANDLE_VALUE == ev->base_handle ) {
            int error = errno;
        }
        if( 0 == RegisterWaitForSingleObject( &ev->registered_handle, ev->base_handle, win32_file_event_callback,
                                              (void*)ev, INFINITE, WT_EXECUTEINWAITTHREAD ) ) {
            error = GetLastError();
        }*/
    }

    return (0);

}

int
win32_del(struct win32op *win32op, opal_event_t *ev)
{
    int i, error;
    opal_event_t *master = NULL, *temp;

    if (ev->ev_events & OPAL_EV_SIGNAL)
        return (signal(OPAL_EVENT_SIGNAL(ev), SIG_IGN) != SIG_ERR);

    for( i = 0; i < win32op->n_events; ++i ) {
        if( win32op->events[i]->ev_fd != ev->ev_fd ) continue;
        master = win32op->events[i];
        break;
    }
    if( NULL == master ) {
        event_debug(("%s: Unable to remove non-inserted event for %d",
                 __func__, ev->ev_fd));
        return (-1);
    }
    event_debug(("%s: Removing event for %d", __func__, ev->ev_fd));

    /**
     * Remove the current event and recompute the registered event
     * based on the opal events pending on the same master.
     */
    if( master == ev ) {
        /* Disable all pending events */
        if( INVALID_HANDLE_VALUE != ev->registered_handle ) {
            if( 0 == UnregisterWait(ev->registered_handle) ) {
                error = GetLastError();
            }
            ev->registered_handle = INVALID_HANDLE_VALUE;
        }
        if( ev->ev_similar == ev ) {
            /* Only one event in the queue. Remove everything. */
            if( INVALID_HANDLE_VALUE != ev->base_handle ) { /* socket */
                /* Now detach the base event from the socket. */
                if( SOCKET_ERROR == WSAEventSelect( ev->ev_fd, ev->base_handle, 0) ) {
                    error = WSAGetLastError();
                }
                /* Finally, destroy the event handle. */
                if( 0 == WSACloseEvent(ev->base_handle) ) {
                    error = WSAGetLastError();
                }
                ev->base_handle = INVALID_HANDLE_VALUE;
            }

            if (i != --win32op->n_events) {
                win32op->events[i] = win32op->events[win32op->n_events];
            }

            return 0;
        }
        master = ev->ev_similar;
        master->base_handle = ev->base_handle;
        master->registered_handle = ev->registered_handle;
        win32op->events[i] = master;
        temp = master;
        while( temp->ev_similar != ev ) {
            temp = temp->ev_similar;
        }
        temp->ev_similar = master;
    } else {
        temp = master;
        while( temp->ev_similar != ev ) {
            temp = temp->ev_similar;
        }
        temp->ev_similar = ev->ev_similar;
    }
    win32_recompute_event( master );

    return 0;
}


int
win32_dispatch( struct event_base *base, struct win32op *win32op,
                struct timeval *tv )
{
    DWORD milisec;

    /*milisec = tv->tv_sec * 1000;
    if( tv->tv_usec > 1000 ) {
        milisec += tv->tv_usec / 1000;
    }*/
    milisec = tv->tv_sec;  /* BLAH BLAH REMOVE ME */
    SleepEx( milisec, TRUE );
    if( 0 != signal_caught ) {
        signal_process(base);
        signal_recalc(base);
    }
    return 0;

}


void
win32_dealloc(struct event_base *_base, void *arg)
{
    struct win32op *win32op = arg;

    evsignal_dealloc(_base);
    if (win32op->readset_in)
        free(win32op->readset_in);
    if (win32op->writeset_in)
        free(win32op->writeset_in);
    if (win32op->readset_out)
        free(win32op->readset_out);
    if (win32op->writeset_out)
        free(win32op->writeset_out);
    if (win32op->exset_out)
        free(win32op->exset_out);
    /* XXXXX free the tree. */

    memset(win32op, 0, sizeof(win32op));
    free(win32op);
}

static void
signal_handler(int sig)
{
    evsigcaught[sig]++;
    signal_caught = 1;
}

int
signal_recalc(struct event_base base)
{
    struct event *ev;

    /* Reinstall our signal handler. */
    TAILQ_FOREACH(ev, &base.sig.signalqueue, ev_signal_next) {
        if((int)signal(EVENT_SIGNAL(ev), signal_handler) == -1)
            return (-1);
    }
    return (0);
}

void
signal_process(struct event_base base)
{
    struct event *ev;
    short ncalls;

    TAILQ_FOREACH(ev, &base.sig.signalqueue, ev_signal_next) {
        ncalls = evsigcaught[EVENT_SIGNAL(ev)];
        if (ncalls) {
            if (!(ev->ev_events & EV_PERSIST))
                event_del(ev);
            event_active(ev, EV_SIGNAL, ncalls);
        }
    }

    memset(evsigcaught, 0, sizeof(evsigcaught));
    signal_caught = 0;
}

