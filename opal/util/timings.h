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

#include "opal_config.h"

#include "opal/class/opal_list.h"
#include "opal/runtime/opal_params.h"

#if OPAL_ENABLE_TIMING

#define OPAL_TIMING_DESCR_MAX 1024
#define OPAL_TIMING_BUFSIZE 32
#define OPAL_TIMING_OUTBUF_SIZE (10*1024)

typedef enum {
    OPAL_TIMING_TRACE,
    OPAL_TIMING_INTDESCR,
    OPAL_TIMING_INTBEGIN,
    OPAL_TIMING_INTEND
} opal_event_type_t;

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

typedef double (*get_ts_t)(void);

typedef struct opal_timing_t
{
    int next_id_cntr;
    // not thread safe!
    // The whole implementation is not thread safe now
    // since it is supposed to be used in orte service
    // thread only. Fix in the future or now?
    int current_id;
    opal_list_t *events;
    opal_timing_event_t *buffer;
    size_t buffer_offset, buffer_size;
    get_ts_t get_ts;
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
 * Should not be directly used - for service purposes only.
 *
 * @param t pointer to the timing handler structure
 * @param fmt printf-like format
 * @param ... other parameters that should be converted to string representation
 *
 * @retval partly filled opal_timing_prep_t structure
  */
opal_timing_prep_t opal_timing_prep_ev(opal_timing_t *t, const char *fmt, ...);

/**
 * Prepare timing event, ignore printf-like processing.
 * Should not be directly used - for service purposes only.
 *
 * @param t pointer to the timing handler structure
 * @param fmt printf-like format
 * @param ... other parameters that should be converted to string representation
 *
 * @retval partly filled opal_timing_prep_t structure
  */
opal_timing_prep_t opal_timing_prep_ev_end(opal_timing_t *t, const char *fmt, ...);

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
void opal_timing_add_step(opal_timing_prep_t p, const char *func,
                          const char *file, int line);

/**
 * Enqueue the description of the interval into a list of events
 * in handler 't'.
 *
 * @param p result of opal_timing_prep_ev
 * @param func function name where event occurs
 * @param file file name where event occurs
 * @param line line number in the file
 *
 * @retval id of event interval
 */
int opal_timing_descr(opal_timing_prep_t p, const char *func,
                      const char *file, int line);

/**
 * Enqueue the beginning of timing interval that already has the
 * description and assigned id into the list of events
 * in handler 't'.
 *
 * @param p result of opal_timing_prep_ev
 * @param func function name where event occurs
 * @param file file name where event occurs
 * @param line line number in the file
 *
 * @retval
 */
void opal_timing_start_id(opal_timing_t *t, int id, const char *func,
                          const char *file, int line);

/**
 * Enqueue the end of timing interval that already has
 * description and assigned id into the list of events
 * in handler 't'.
 *
 * @param p result of opal_timing_prep_ev
 * @param func function name where event occurs
 * @param file file name where event occurs
 * @param line line number in the file
 *
 * @retval
 */
void opal_timing_end(opal_timing_t *t, int id, const char *func,
                     const char *file, int line );

/**
 * Enqueue both description and start of timing interval
 * into the list of events and assign its id.
 *
 * @param p result of opal_timing_prep_ev
 * @param func function name where event occurs
 * @param file file name where event occurs
 * @param line line number in the file
 *
 * @retval interval id
 */
static inline int opal_timing_start_init(opal_timing_prep_t p,
                           const char *func, const char *file, int line)
{
    int id = opal_timing_descr(p, func, file, line);
    if( id < 0 )
        return id;
    opal_timing_start_id(p.t, id, func, file, line);
    return id;
}

/**
 * The wrapper that is used to stop last measurement in OPAL_TIMING_MNEXT.
 *
 * @param p result of opal_timing_prep_ev
 * @param func function name where event occurs
 * @param file file name where event occurs
 * @param line line number in the file
 *
 * @retval interval id
 */
void opal_timing_end_prep(opal_timing_prep_t p,
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
 * @param account_overhead consider malloc overhead introduced by timing code
 * @param prefix prefix to use when no fname was specifyed to ease grep'ing
 * @param fname name of the output file (may be NULL)
 *
 * @retval OPAL_SUCCESS On success
 * @retval OPAL_ERROR or OPAL_ERR_OUT_OF_RESOURCE On failure
 */
int opal_timing_report(opal_timing_t *t, char *fname);

/**
 * Report all intervals that were enqueued in the timing handler 't'.
 * - if fname == NULL the output will be done using opal_output and
 * each line will be prefixed with "prefix" to ease grep'ing.
 * - otherwise the corresponding file will be used for output in "append" mode
 * WARRNING: not all filesystems provide enough support for that feature, some records may
 * disappear.
 *
 * @param t timing handler
 * @param account_overhead consider malloc overhead introduced by timing code
  * @param fname name of the output file (may be NULL)
 *
 * @retval OPAL_SUCCESS On success
 * @retval OPAL_ERROR or OPAL_ERR_OUT_OF_RESOURCE On failure
 */
int opal_timing_deltas(opal_timing_t *t, char *fname);

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
#define OPAL_TIMING_DECLARE(t) opal_timing_t t;   /* need semicolon here to avoid warnings when not enabled */

/**
 * Main macro for use in declaring external opal timing handler;
 * will be "compiled out" when OPAL is configured without
 * --enable-timing.
 *
 */
#define OPAL_TIMING_DECLARE_EXT(x, t) x extern opal_timing_t t;  /* need semicolon here to avoid warnings when not enabled */

/**
 * Main macro for use in initializing opal timing handler;
 * will be "compiled out" when OPAL is configured without
 * --enable-timing.
 *
 * @see opal_timing_init()
 */
#define OPAL_TIMING_INIT(t) opal_timing_init(t)

/**
 * Macro that enqueues event with its description to the specified
 * timing handler;
 * will be "compiled out" when OPAL is configured without
 * --enable-timing.
 *
 * @see opal_timing_add_step()
 */
#define OPAL_TIMING_EVENT(x) opal_timing_add_step( opal_timing_prep_ev x, __FUNCTION__, __FILE__, __LINE__)

/**
 * MDESCR: Measurement DESCRiption
 * Introduce new timing measurement with string description for the specified
 * timing handler;
 * will be "compiled out" when OPAL is configured without
 * --enable-timing.
 *
 * @see opal_timing_descr()
 */
#define OPAL_TIMING_MDESCR(x) opal_timing_descr( opal_timing_prep_ev x, __FUNCTION__, __FILE__, __LINE__)

/**
 * MSTART_ID: Measurement START by ID.
 * Marks the beginning of the measurement with ID=id on the
 * specified timing handler;
 * will be "compiled out" when OPAL is configured without
 * --enable-timing.
 *
 * @see opal_timing_start_id()
 */
#define OPAL_TIMING_MSTART_ID(t, id) opal_timing_start_id(t, id, __FUNCTION__, __FILE__, __LINE__)

/**
 * MSTART: Measurement START
 * Introduce new timing measurement conjuncted with its start
 * on the specifyed timing handler;
 * will be "compiled out" when OPAL is configured without
 * --enable-timing.
 *
 * @see opal_timing_start_init()
 */
#define OPAL_TIMING_MSTART(x) opal_timing_start_init( opal_timing_prep_ev x, __FUNCTION__, __FILE__, __LINE__)

/**
 * MSTOP: STOP Measurement
 * Finishes the most recent measurement on the specifyed timing handler;
 * will be "compiled out" when OPAL is configured without
 * --enable-timing.
 *
 * @see opal_timing_end()
 */
#define OPAL_TIMING_MSTOP(t) opal_timing_end(t, -1, __FUNCTION__, __FILE__, __LINE__)

/**
 * MSTOP_ID: STOP Measurement with ID=id.
 * Finishes the measurement with give ID on the specifyed timing handler;
 * will be "compiled out" when OPAL is configured without
 * --enable-timing.
 *
 * @see opal_timing_end()
 */
#define OPAL_TIMING_MSTOP_ID(t, id) opal_timing_end(t, id, __FUNCTION__, __FILE__, __LINE__)

/**
 * MNEXT: start NEXT Measurement
 * Convinient macro, may be implemented with the sequence of three previously
 * defined macroses:
 * - finish current measurement (OPAL_TIMING_MSTOP);
 * - introduce new timing measurement (OPAL_TIMING_MDESCR);
 * - starts next measurement (OPAL_TIMING_MSTART_ID)
 * on the specifyed timing handler;
 * will be "compiled out" when OPAL is configured without
 * --enable-timing.
 *
 * @see opal_timing_start_init()
 */
#define OPAL_TIMING_MNEXT(x) ( \
    opal_timing_end_prep(opal_timing_prep_ev_end x,             \
                            __FUNCTION__, __FILE__, __LINE__ ), \
    opal_timing_start_init( opal_timing_prep_ev x,              \
                            __FUNCTION__, __FILE__, __LINE__)   \
)

/**
 * The macro for use in reporting collected events with absolute values;
 * will be "compiled out" when OPAL is configured without
 * --enable-timing.
 *
 * @param enable flag that enables/disables reporting. Used for fine-grained timing.
 * @see opal_timing_report()
 */
#define OPAL_TIMING_REPORT(enable, t) { \
    if( enable ) { \
        opal_timing_report(t, opal_timing_output); \
    } \
}

/**
 * The macro for use in reporting collected events with relative times;
 * will be "compiled out" when OPAL is configured without
 * --enable-timing.
 *
 * @param enable flag that enables/disables reporting. Used for fine-grained timing.
 * @see opal_timing_deltas()
 */
#define OPAL_TIMING_DELTAS(enable, t) { \
    if( enable ) { \
        opal_timing_deltas(t, opal_timing_output); \
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

#define OPAL_TIMING_MDESCR(x)

#define OPAL_TIMING_MSTART_ID(t, id)

#define OPAL_TIMING_MSTART(x)

#define OPAL_TIMING_MSTOP(t)

#define OPAL_TIMING_MSTOP_ID(t, id)

#define OPAL_TIMING_MNEXT(x)

#define OPAL_TIMING_REPORT(enable, t)

#define OPAL_TIMING_DELTAS(enable, t)

#define OPAL_TIMING_RELEASE(t)

#endif

#endif
