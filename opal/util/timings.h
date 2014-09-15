/*
 * ? Copyrights ?
 */

#ifndef OPAL_SYS_TIMING_H
#define OPAL_SYS_TIMING_H

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
} opal_timing_prep_t;

int opal_timing_clksync_read(char *opal_clksync_file);
int opal_timing_set_jobid(char *jid);

void opal_timing_init(opal_timing_t *t);

opal_timing_prep_t opal_timing_prep_ev(opal_timing_t *t, const char *fmt, ...);
void opal_timing_add_step(opal_timing_prep_t p,
                          const char *func, const char *file, int line);

/*
opal_timing_prep_t opal_timing_prep_end(opal_timing_t *t, int id, const char *fmt, ...);
int opal_timing_begin(opal_timing_t *t, char *file, int line);
void opal_timing_end(opal_timing_prep_t p, char *file, int line);
*/

int opal_timing_report(opal_timing_t *t, bool account_overhead, const char *prefix, char *fname);
void opal_timing_release(opal_timing_t *t);

#define OPAL_TIMING_DECLARE(t) opal_timing_t t;   // must have the semicolon here to avoid warnings when not enabled

#define OPAL_TIMING_DECLARE_EXT(x, t) x extern opal_timing_t t;  // must have the semicolon here to avoid warnings when not enabled

#define OPAL_TIMING_INIT(t) opal_timing_init(t)

#define OPAL_TIMING_EVENT(x) opal_timing_add_step( opal_timing_prep_ev x, __FUNCTION__, __FILE__, __LINE__)

#define OPAL_TIMING_REPORT(enable, t, prefix) { \
    if( enable ) { \
        opal_timing_report(t, opal_timing_account_overhead, prefix, opal_timing_file); \
    } \
}

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
