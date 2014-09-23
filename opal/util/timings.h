/*
 * Copyright (C) 2014      Artem Polyakov <artpol84@gmail.com>
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_UTIL_TIMING_H
#define OPAL_UTIL_TIMING_H

#include "opal/class/opal_list.h"
#include "opal/runtime/opal_params.h"

#if OPAL_ENABLE_TIMING

#define OPAL_TIMING_DESCR_MAX 1024
#define OPAL_TIMING_BUFSIZE 32
#define OPAL_TIMING_OUTBUF_SIZE (10*1024)

typedef enum { TEVENT, TBEGIN, TEND } opal_event_type_t;


typedef struct {
    opal_list_item_t super;
    int fib;
    opal_event_type_t type;
    const char *func;
    const char *file;
    int line;
    double ts, ts_ovh;
    char descr[OPAL_TIMING_DESCR_MAX];
    int id;
} opal_timing_event_t;

typedef struct opal_timing_t
{
    int cur_id;
    opal_list_t *events;
    opal_timing_event_t *buffer;
    size_t buffer_offset, buffer_size;
} opal_timing_t;

typedef struct {
    opal_timing_t *t;
    opal_timing_event_t *ev;
    int errcode;
} opal_timing_prep_t;

/**
 * Read synchronisation information from the file
 * provided through the MCA parameter.
 * Should not be directly used, for service purposes.
 *
 * @param sync_file Name of the file to read
  *
 * @retval OPAL_SUCCESS On success
 * @retval OPAL_ERROR On failure
 */
int opal_timing_clocksync_read(char *sync_file);

/**
 * Pass string representation of ORTE job ID down to the OPAL.
 * Should not be directly used, for service purposes.
 *
 * @param jid job id
  *
 * @retval OPAL_SUCCESS On success
 * @retval OPAL_ERROR On failure
 */
int opal_timing_set_jobid(char *jid);

/**
 * Initialize timing structure.
 *
 * @param t pointer to the timing handler structure
  *
 * @retval OPAL_SUCCESS On success
 * @retval OPAL_ERROR On failure
 */
void opal_timing_init(opal_timing_t *t);

/**
 * Prepare timing event, do all printf-like processing.
 * Should not be directly used, for service purposes.
 *
 * @param t pointer to the timing handler structure
 * @param fmt printf-like format
 * @param ... other parameters that should be converted to string representation
 *
 * @retval partly filled opal_timing_prep_t structure
  */
opal_timing_prep_t opal_timing_prep_ev(opal_timing_t *t, const char *fmt, ...);

/**
 * Enqueue timing event into the list of events in handler 't'.
 *
 * @param p result of opal_timing_prep_ev
 * @param func function name where event occurs
 * @param file file name where event occurs
 * @param line line number in the file
 *
 * @retval
 */
void opal_timing_add_step(opal_timing_prep_t p,
                          const char *func, const char *file, int line);

/**
 * Report all events that were enqueued in the timing handler 't'.
 * - if fname == NULL the output will be done using opal_output and
 * each line will be prefixed with "prefix" to ease grep'ing.
 * - otherwise the corresponding file will be used for output in "append" mode
 * WARRNING: not all filesystems provide enough support for that feature, some records may
 * disappear.
 *
 * @param t timing handler
 * @param accovh consider malloc overhead introduced by timing code
 * @param prefix prefix to use when no fname was specifyed to ease grep'ing
 * @param fname name of the output file (may be NULL)
 *
 * @retval OPAL_SUCCESS On success
 * @retval OPAL_ERROR or OPAL_ERR_OUT_OF_RESOURCE On failure
 */
int opal_timing_report(opal_timing_t *t, bool accovh, const char *prefix, char *fname);

/**
 * Release all memory allocated for the timing handler 't'.
 *
 * @param t timing handler
 *
 * @retval
 */
void opal_timing_release(opal_timing_t *t);

/**
 * Main macro for use in declaring opal timing handler;
 * will be "compiled out" when OPAL is configured without
 * --enable-timing.
 *
 */
#define OPAL_TIMING_DECLARE(t) opal_timing_t t;   // must have the semicolon here to avoid warnings when not enabled

/**
 * Main macro for use in declaring external opal timing handler;
 * will be "compiled out" when OPAL is configured without
 * --enable-timing.
 *
 */
#define OPAL_TIMING_DECLARE_EXT(x, t) x extern opal_timing_t t;  // must have the semicolon here to avoid warnings when not enabled

/**
 * Main macro for use in initializing opal timing handler;
 * will be "compiled out" when OPAL is configured without
 * --enable-timing.
 *
 * @see opal_timing_init()
 */
#define OPAL_TIMING_INIT(t) opal_timing_init(t)

/**
 * Main macro for use in adding new timing event for the specifyed timing handler;
 * will be "compiled out" when OPAL is configured without
 * --enable-timing.
 *
 * @see opal_timing_add_step()
 */
#define OPAL_TIMING_EVENT(x) opal_timing_add_step( opal_timing_prep_ev x, __FUNCTION__, __FILE__, __LINE__)

/**
 * Main macro for use in reporting collected events;
 * will be "compiled out" when OPAL is configured without
 * --enable-timing.
 *
 * @param enable flag that enables/disables reporting. Used for fine-grained timing.
 * @see opal_timing_report()
 */
#define OPAL_TIMING_REPORT(enable, t, prefix) { \
    if( enable ) { \
        opal_timing_report(t, opal_timing_overhead, prefix, opal_timing_output); \
    } \
}

/**
 * Main macro for use in releasing allocated resources;
 * will be "compiled out" when OPAL is configured without
 * --enable-timing.
 *
 * @see opal_timing_release()
 */
#define OPAL_TIMING_RELEASE(t) opal_timing_release(t)

#else

#define OPAL_TIMING_DECLARE(t)

#define OPAL_TIMING_DECLARE_EXT(x, t)

#define OPAL_TIMING_INIT(t)

#define OPAL_TIMING_EVENT(x)

#define OPAL_TIMING_REPORT(enable, t, prefix)

#define OPAL_TIMING_RELEASE(t)

#endif

#endif
