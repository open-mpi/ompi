/*
 *
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

/* buffer.c */
#define evbuffer_add opal_evbuffer_add
#define evbuffer_add_buffer opal_evbuffer_add_buffer
#define evbuffer_add_vprintf opal_evbuffer_add_vprintf
#define evbuffer_add_printf opal_evbuffer_add_printf
#define evbuffer_drain opal_evbuffer_drain
#define evbuffer_expand opal_evbuffer_expand
#define evbuffer_find opal_evbuffer_find
#define evbuffer_free opal_evbuffer_free
#define evbuffer_new opal_evbuffer_new
#define evbuffer_read opal_evbuffer_read
#define evbuffer_readline opal_evbuffer_readline
#define evbuffer_remove opal_evbuffer_remove
#define evbuffer_setcb opal_evbuffer_setcb
#define evbuffer_write opal_evbuffer_write

/* devpoll.c */
#define devpollops opal_devpollops

/* epoll_sub.c */
/* these symbols should *NOT* be renamed */

/* evbuffer.c */
#define bufferevent_disable opal_bufferevent_disable
#define bufferevent_enable opal_bufferevent_enable
#define bufferevent_free opal_bufferevent_free
#define bufferevent_new opal_bufferevent_new
#define bufferevent_priority_set opal_bufferevent_priority_set
#define bufferevent_read opal_bufferevent_read
#define bufferevent_settimeout opal_bufferevent_settimeout
#define bufferevent_setwatermark opal_bufferevent_setwatermark
#define bufferevent_write opal_bufferevent_write
#define bufferevent_write_buffer opal_bufferevent_write_buffer

/* event.c */
#define current_base opal_current_base
#define event_base_loopexit opal_event_base_loopexit
#define event_get_method opal_event_get_method
#define event_get_version opal_event_get_version
#define event_gotsig opal_event_gotsig
#define event_sigcb opal_event_sigcb

/* log.c */
#define _event_debugx opal__event_debugx
#define event_err opal_event_err
#define event_errx opal_event_errx
#define event_msgx opal_event_msgx
#define event_set_log_callback opal_event_set_log_callback
#define event_warn opal_event_warn
#define event_warnx opal_event_warnx

/* poll.c */
#define poll_add opal_poll_add
#define poll_del opal_poll_del
#define poll_dispatch opal_poll_dispatch
#define poll_init opal_poll_init
#define poll_recalc opal_poll_recalc

