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

typedef enum {
    OPAL_TIMING_AUTOMATIC_TIMER,
    OPAL_TIMING_GET_TIME_OF_DAY,
    OPAL_TIMING_CYCLE_NATIVE,
    OPAL_TIMING_USEC_NATIVE
} opal_timer_type_t;

#if OPAL_ENABLE_TIMING

typedef double (*opal_timing_ts_func_t)(void);

#define OPAL_TIMING_STR_LEN 256
typedef struct {
    char id[OPAL_TIMING_STR_LEN], cntr_env[OPAL_TIMING_STR_LEN];
    int enabled, error;
    int cntr;
    double ts;
    opal_timing_ts_func_t get_ts;
} opal_timing_env_t;

opal_timing_ts_func_t opal_timing_ts_func(opal_timer_type_t type);


/* TODO: turn as much as possible into macro's
 * once debugged
 */

static inline opal_timing_env_t
OPAL_TIMING_ENV_START_TYPE(char *func, opal_timer_type_t type, char *prefix)
{
    opal_timing_env_t h;
    int n;

    /* TODO: remove this when tested! */
    h.enabled = 0;
    return h;

    if( NULL == prefix ){
        prefix = "";
    }

    h.error = 0;
    n = snprintf(h.id, OPAL_TIMING_STR_LEN, "%s%s", prefix, func);
    if( n > OPAL_TIMING_STR_LEN ){
        /* TODO: output truncated:
         * disable this timing and set the error
         * sign
         */
    }

    /* TODO same length check here */
    sprintf(h.cntr_env,"%s_CNT", h.id);
    h.get_ts = opal_timing_ts_func(type);
    h.ts = h.get_ts();
    h.enabled = 1;

    char *ptr = getenv(h.id);
    if( NULL == ptr || strcmp(ptr, "1")){
        h.enabled = 0;
    }
    ptr = getenv(h.cntr_env);
    h.cntr = 0;
    if( NULL != ptr ){
        h.cntr = atoi(ptr);
    }
    return h;
}

#define OPAL_TIMING_ENV_INIT(name)                                                     \
    opal_timing_env_t name ## _val, *name = &(name ## _val);                            \
    *name = OPAL_TIMING_ENV_START_TYPE(__FUNCTION__, OPAL_TIMING_AUTOMATIC_TIMER, "");

/* We use function names for identification
 * however this might be a problem for the private
 * functions declared as static as their names may
 * conflict.
 * Use prefix to do a finer-grained identification if needed
 */
#define OPAL_TIMING_ENV_INIT_PREFIX(prefix, name)                                          \
    opal_timing_env_t name ## _val, *name = &(name ## _val);                                \
    name = OPAL_TIMING_ENV_START_TYPE(__FUNCTION__, OPAL_TIMING_AUTOMATIC_TIMER, prefix);


/* TODO: according to https://en.wikipedia.org/wiki/C99
 * varadic macroses are part of C99 and C11. Is it safe to use them here?
 */
static inline void
OPAL_TIMING_ENV_NEXT(opal_timing_env_t *h, char *fmt, ... )
{
    if( !h->enabled ){
        return;
    }
    /* enabled codepath */
    va_list ap;
    int n;
    char buf[256], buf2[256];
    double time = h->get_ts() - h->ts;

    sprintf(buf, "%s_DESC_%d", h->id, h->cntr);
    /* TODO: check that write succeded */

    va_start(ap, fmt);
    n= vsnprintf(buf2, 256, fmt, ap);
    /* TODO: check that write succeded */
    va_end(ap);

    setenv(buf, buf2, 1);

    sprintf(buf, "%s_VAL_%d", h->id, h->cntr);
    /* TODO: check that write succeded */
    sprintf(buf2, "%lf", time);
    /* TODO: check that write succeded */
    setenv(buf, buf2, 1);

    h->cntr++;
    sprintf(buf, "%d", h->cntr);
    setenv(h->cntr_env, buf, 1);

    /* We don't include env operations into the consideration.
     * Hopefully this will help to make measurements more accurate.
     */
    h->ts = h->get_ts();
}

/* This function supposed to be called from the code that will
 * do the postprocessing, i.e. OMPI timing portion that will
 * do the reduction of accumulated values
 */
/* TODO: turn into a macro */
static inline int OPAL_TIMING_ENV_CNT_PREFIX(char *prefix, char *func)
{
    char ename[256];
    sprintf(ename, "%s%s_CNT", prefix, func);
    char *ptr = getenv(ename);
    if( !ptr ){
        return 0;
    }
    return atoi(ptr);
}

#define OPAL_TIMING_ENV_CNT(func) \
    OPAL_TIMING_ENV_CNT_PREFIX("", char *func)

/* TODO: make a macro */
static inline double
OPAL_TIMING_ENV_GETDESC_PREFIX(char *prefix, char *func, int i, char **desc)
{
    char vname[256];
    double ts;
    sprintf(vname, "%s_INT_%d_DESC", prefix, i);
    *desc = getenv(vname);
    sprintf(vname, "%s_INT_%d_VAL",prefix, i);
    char *ptr = getenv(vname);
    sscanf(ptr,"%lf", &ts);
    return ts;
}

#define OPAL_TIMING_ENV_GETDESC(func, index, desc) \
    OPAL_TIMING_ENV_GETDESC_PREFIX("", func, index, desc)

#define OSHTMNG_ENV_APPEND(prefix) {                          \
    char *enabled;                                            \
    int cnt = OSHTMNG_ENV_COUNT(prefix);                      \
    enabled = getenv(prefix);                                 \
    if( NULL != enabled && !strcmp(enabled, "1") )  {         \
        char ename[256];                                      \
        sprintf(ename, "OSHTMNG_%s", OSHTMNG_prefix);         \
        setenv(ename, "1", 1);                                \
    }                                                         \
    int i;                                                    \
    for(i = 0; i < cnt; i++){                                 \
        char *desc;                                           \
        double ts = OSHTMNG_ENV_GETBYIDX(prefix, i, &desc);   \
        OSHTMNG_END1(desc, ts);                               \
    }                                                         \
}

#else

#define OPAL_TIMING_ENV_START_TYPE(func, type, prefix)

#define OPAL_TIMING_ENV_INIT(name)

#define OPAL_TIMING_ENV_INIT_PREFIX(prefix)

/* TODO: according to https://en.wikipedia.org/wiki/C99
 * varadic macroses are part of C99 and C11. Is it safe to use them here?
 */
#define OPAL_TIMING_ENV_NEXT(h, fmt, ... )

#define OPAL_TIMING_ENV_CNT_PREFIX(prefix, func)

#define OPAL_TIMING_ENV_CNT(func)

#define OPAL_TIMING_ENV_GETDESC_PREFIX(prefix, func, i, desc)

#define OPAL_TIMING_ENV_GETDESC(func, index, desc)

#endif

#endif
