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

#define OPAL_ENABLE_TIMING 1

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

#define OPAL_TIMING_ENV_START_TYPE(func, type, prefix) ({                     \
    __label__ exit;                                                           \
    opal_timing_env_t h;                                                      \
    char *ptr = NULL;                                                         \
    char *_prefix = prefix;                                                   \
    int n;                                                                    \
    if( NULL == prefix ){                                                     \
        _prefix = "";                                                         \
    }                                                                         \
    h.error = 0;                                                              \
    n = snprintf(h.id, OPAL_TIMING_STR_LEN, "%s%s", _prefix, func);           \
    if( n > OPAL_TIMING_STR_LEN ){                                            \
         h.error = 1;                                                         \
         goto exit;                                                           \
    }                                                                         \
    n = sprintf(h.cntr_env,"%s_CNT", h.id);                                   \
    if( n > OPAL_TIMING_STR_LEN ){                                            \
        h.error = 1;                                                          \
        goto exit;                                                            \
    }                                                                         \
    h.get_ts = opal_timing_ts_func(type);                                     \
    h.enabled = 1;                                                            \
    h.cntr = 0;                                                               \
    ptr = getenv(h.id);                                                       \
    if( NULL != ptr ){                                                        \
        h.error = 1;                                                          \
        goto exit;                                                            \
    }                                                                         \
    h.ts = h.get_ts();                                                        \
    setenv(h.id, "1", 1);                                                     \
    setenv(h.cntr_env, "0", 1);                                               \
exit:                                                                         \
    if ( 0 != h.error ){                                                      \
        h.enabled = 0;                                                        \
    }                                                                         \
    h;                                                                        \
})

#define OPAL_TIMING_ENV_INIT(name)                                                          \
    opal_timing_env_t name ## _val, *name = &(name ## _val);                                \
    *name = OPAL_TIMING_ENV_START_TYPE(__FUNCTION__, OPAL_TIMING_AUTOMATIC_TIMER, "");

/* We use function names for identification
 * however this might be a problem for the private
 * functions declared as static as their names may
 * conflict.
 * Use prefix to do a finer-grained identification if needed
 */
#define OPAL_TIMING_ENV_INIT_PREFIX(prefix, name)                                           \
    opal_timing_env_t name ## _val, *name = &(name ## _val);                                \
    *name = OPAL_TIMING_ENV_START_TYPE(__FUNCTION__, OPAL_TIMING_AUTOMATIC_TIMER, prefix);

#define OPAL_TIMING_ENV_NEXT(h, fmt, ...) ({                                   \
    __label__ exit;                                                            \
    int n;                                                                     \
    char buf1[OPAL_TIMING_STR_LEN], buf2[OPAL_TIMING_STR_LEN];                 \
    double time;                                                               \
    if( h->enabled ){                                                          \
        /* enabled codepath */                                                 \
        time = h->get_ts() - h->ts;                                            \
        n = snprintf(buf1, OPAL_TIMING_STR_LEN, "OMPI_TIMING_%s_DESC_%d", h->id, h->cntr); \
        if ( n > OPAL_TIMING_STR_LEN ){                                        \
            h->error = 1;                                                      \
            goto exit;                                                         \
        }                                                                      \
        n = snprintf(buf2, OPAL_TIMING_STR_LEN, fmt, ## __VA_ARGS__ );         \
        if ( n > OPAL_TIMING_STR_LEN ){                                        \
            h->error = 1;                                                      \
            goto exit;                                                         \
        }                                                                      \
        setenv(buf1, buf2, 1);                                                 \
        n = snprintf(buf1, OPAL_TIMING_STR_LEN, "OMPI_TIMING_%s_VAL_%d", h->id, h->cntr);  \
        if ( n > OPAL_TIMING_STR_LEN ){                                        \
            h->error = 1;                                                      \
            goto exit;                                                         \
        }                                                                      \
        n = snprintf(buf2, OPAL_TIMING_STR_LEN, "%lf", time);                  \
        if ( n > OPAL_TIMING_STR_LEN ){                                        \
            h->error = 1;                                                      \
            goto exit;                                                         \
        }                                                                      \
        setenv(buf1, buf2, 1);                                                 \
        h->cntr++;                                                             \
        sprintf(buf1, "%d", h->cntr);                                          \
        setenv(h->cntr_env, buf1, 1);                                          \
        /* We don't include env operations into the consideration.
         * Hopefully this will help to make measurements more accurate.
         */                                                                    \
        h->ts = h->get_ts();                                                   \
exit:;                                                                         \
    }                                                                          \
})

/* This function supposed to be called from the code that will
 * do the postprocessing, i.e. OMPI timing portion that will
 * do the reduction of accumulated values
 */
#define OPAL_TIMING_ENV_CNT_PREFIX(prefix, func) ({                            \
    __label__ exit;                                                            \
    char ename[OPAL_TIMING_STR_LEN];                                           \
    int cnt = 0;                                                               \
    char *ptr = NULL;                                                          \
    int n = snprintf(ename, OPAL_TIMING_STR_LEN, "%s%s_CNT", prefix, func);    \
    if ( n > OPAL_TIMING_STR_LEN ){ goto exit; }                               \
    ptr = getenv(ename);                                                       \
    if( NULL != ptr ){ cnt = atoi(ptr); }                                      \
exit:                                                                          \
    cnt;                                                                       \
})

#define OPAL_TIMING_ENV_CNT(func)                                              \
    OPAL_TIMING_ENV_CNT_PREFIX("", func)

#define OPAL_TIMING_ENV_GETDESC_PREFIX(prefix, func, i, desc) ({               \
    char vname[OPAL_TIMING_STR_LEN];                                           \
    double ts = 0.0;                                                           \
    sprintf(vname, "OMPI_TIMING_%s%s_DESC_%d", prefix, func, i);                           \
    *desc = getenv(vname);                                                     \
    sprintf(vname, "OMPI_TIMING_%s%s_VAL_%d",prefix, func, i);                             \
    char *ptr = getenv(vname);                                                 \
    if ( NULL != ptr ) {                                                       \
        sscanf(ptr,"%lf", &ts);                                                \
    }                                                                          \
    ts;                                                                        \
})

#define OPAL_TIMING_ENV_GETDESC(func, index, desc)                             \
    OPAL_TIMING_ENV_GETDESC_PREFIX("", func, index, desc)

#else

#define OPAL_TIMING_ENV_START_TYPE(func, type, prefix)

#define OPAL_TIMING_ENV_INIT(name)

#define OPAL_TIMING_ENV_INIT_PREFIX(prefix)

#define OPAL_TIMING_ENV_NEXT(h, fmt, ... )

#define OPAL_TIMING_ENV_CNT_PREFIX(prefix, func)

#define OPAL_TIMING_ENV_CNT(func)

#define OPAL_TIMING_ENV_GETDESC_PREFIX(prefix, func, i, desc)

#define OPAL_TIMING_ENV_GETDESC(func, index, desc)

#endif

#endif
