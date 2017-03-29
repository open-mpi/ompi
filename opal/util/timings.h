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

#define OPAL_TIMING_ENV_START_TYPE(func, type, prefix) ({                     \
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
    }                                                                         \
    n = sprintf(h.cntr_env,"OMPI_TIMING_%s%s_CNT", prefix, h.id);             \
    if( n > OPAL_TIMING_STR_LEN ){                                            \
        h.error = 1;                                                          \
    }                                                                         \
    ptr = getenv(h.id);                                                       \
    if( NULL == ptr || strcmp(ptr, "1")){                                     \
        h.enabled = 0;                                                        \
    }                                                                         \
    h.get_ts = opal_timing_ts_func(type);                                     \
    ptr = getenv("OPAL_TIMING_ENABLE");                                       \
    if (NULL != ptr) {                                                        \
        h.enabled = atoi(ptr);                                                \
    }                                                                         \
    h.cntr = 0;                                                               \
    ptr = getenv(h.id);                                                       \
    if( NULL != ptr ){                                                        \
        h.cntr = atoi(ptr);                                                   \
    }                                                                         \
    h.ts = h.get_ts();                                                        \
    if ( 0 != h.error ){                                                      \
        h.enabled = 0;                                                        \
    }                                                                         \
    h;                                                                        \
})

#define OPAL_TIMING_ENV_INIT(name)                                            \
    opal_timing_env_t name ## _val, *name = &(name ## _val);                  \
    *name = OPAL_TIMING_ENV_START_TYPE(__FUNCTION__, OPAL_TIMING_AUTOMATIC_TIMER, "");

/* We use function names for identification
 * however this might be a problem for the private
 * functions declared as static as their names may
 * conflict.
 * Use prefix to do a finer-grained identification if needed
 */
#define OPAL_TIMING_ENV_INIT_PREFIX(prefix, name)                             \
    opal_timing_env_t name ## _val, *name = &(name ## _val);                  \
    *name = OPAL_TIMING_ENV_START_TYPE(__FUNCTION__, OPAL_TIMING_AUTOMATIC_TIMER, prefix);

#define OPAL_TIMING_ENV_NEXT(h, fmt, ...) ({                                  \
    int n;                                                                    \
    char buf1[OPAL_TIMING_STR_LEN], buf2[OPAL_TIMING_STR_LEN];                \
    double time;                                                              \
    char *filename;                                                           \
    if( h->enabled ){                                                         \
        /* enabled codepath */                                                \
        time = h->get_ts() - h->ts;                                           \
        n = snprintf(buf1, OPAL_TIMING_STR_LEN, "OMPI_TIMING_%s_DESC_%d", h->id, h->cntr); \
        if ( n > OPAL_TIMING_STR_LEN ){                                       \
            h->error = 1;                                                     \
        }                                                                     \
        n = snprintf(buf2, OPAL_TIMING_STR_LEN, fmt, ## __VA_ARGS__ );        \
        if ( n > OPAL_TIMING_STR_LEN ){                                       \
            h->error = 1;                                                     \
        }                                                                     \
        setenv(buf1, buf2, 1);                                                \
        n = snprintf(buf1, OPAL_TIMING_STR_LEN, "OMPI_TIMING_%s_VAL_%d", h->id, h->cntr);  \
        if ( n > OPAL_TIMING_STR_LEN ){                                       \
            h->error = 1;                                                     \
        }                                                                     \
        n = snprintf(buf2, OPAL_TIMING_STR_LEN, "%lf", time);                 \
        if ( n > OPAL_TIMING_STR_LEN ){                                       \
            h->error = 1;                                                     \
        }                                                                     \
        setenv(buf1, buf2, 1);                                                \
        filename = strrchr(__FILE__, '/') + 1;                                \
        n = snprintf(buf1, OPAL_TIMING_STR_LEN, "OMPI_TIMING_%s_FILE_%d", h->id, h->cntr); \
        if ( n > OPAL_TIMING_STR_LEN ){                                       \
            h->error = 1;                                                     \
        }                                                                     \
        n = snprintf(buf2, OPAL_TIMING_STR_LEN, "%s", filename);              \
        if ( n > OPAL_TIMING_STR_LEN ){                                       \
            h->error = 1;                                                     \
        }                                                                     \
        setenv(buf1, buf2, 1);                                                \
        h->cntr++;                                                            \
        sprintf(buf1, "%d", h->cntr);                                         \
        setenv(h->cntr_env, buf1, 1);                                         \
        /* We don't include env operations into the consideration.
         * Hopefully this will help to make measurements more accurate.
         */                                                                   \
        h->ts = h->get_ts();                                                  \
    }                                                                         \
    if (h->error) {                                                           \
        n = snprintf(buf1, OPAL_TIMING_STR_LEN, "OMPI_TIMING_%s_ERROR", h->id);\
        if ( n > OPAL_TIMING_STR_LEN ){                                       \
            h->error = 1;                                                     \
        }                                                                     \
        n = snprintf(buf2, OPAL_TIMING_STR_LEN, "%d", h->error);              \
        if ( n > OPAL_TIMING_STR_LEN ){                                       \
            h->error = 1;                                                     \
        }                                                                     \
        setenv(buf1, buf2, 1);                                                \
    }                                                                         \
})

/* This function supposed to be called from the code that will
 * do the postprocessing, i.e. OMPI timing portion that will
 * do the reduction of accumulated values
 */
#define OPAL_TIMING_ENV_CNT_PREFIX(prefix, func) ({                           \
    char ename[OPAL_TIMING_STR_LEN];                                          \
    int cnt = 0;                                                              \
    char *ptr = NULL;                                                         \
    int n = snprintf(ename, OPAL_TIMING_STR_LEN, "OMPI_TIMING_%s%s_CNT", prefix, func);    \
    if ( n <= OPAL_TIMING_STR_LEN ){                                          \
        ptr = getenv(ename);                                                  \
        if( NULL != ptr ){ cnt = atoi(ptr); };                                \
    }                                                                         \
    cnt;                                                                      \
})

#define OPAL_TIMING_ENV_ERROR_PREFIX(prefix, func) ({                         \
    char ename[OPAL_TIMING_STR_LEN];                                          \
    int error = 0;                                                            \
    char *ptr = NULL;                                                         \
    int n = snprintf(ename, OPAL_TIMING_STR_LEN, "OMPI_TIMING_%s%s_ERROR", prefix, func);    \
    if ( n <= OPAL_TIMING_STR_LEN ){                                          \
        ptr = getenv(ename);                                                  \
        if( NULL != ptr ){ error = atoi(ptr); };                              \
    }                                                                         \
    error;                                                                    \
})

#define OPAL_TIMING_ENV_CNT(func)                                             \
    OPAL_TIMING_ENV_CNT_PREFIX("", func)

#define OPAL_TIMING_ENV_GETDESC_PREFIX(prefix, filename, func, i, desc) ({    \
    char vname[OPAL_TIMING_STR_LEN];                                          \
    double ts = 0.0;                                                          \
    sprintf(vname, "OMPI_TIMING_%s%s_FILE_%d", prefix, func, i);              \
    *filename = getenv(vname);                                                \
    sprintf(vname, "OMPI_TIMING_%s%s_DESC_%d", prefix, func, i);              \
    *desc = getenv(vname);                                                    \
    sprintf(vname, "OMPI_TIMING_%s%s_VAL_%d", prefix, func, i);               \
    char *ptr = getenv(vname);                                                \
    if ( NULL != ptr ) {                                                      \
        sscanf(ptr,"%lf", &ts);                                               \
    }                                                                         \
    ts;                                                                       \
})

#define OPAL_TIMING_ENV_GETDESC(file, func, index, desc)                      \
    OPAL_TIMING_ENV_GETDESC_PREFIX("", file, func, index, desc)

#else

#define OPAL_TIMING_ENV_START_TYPE(func, type, prefix)

#define OPAL_TIMING_ENV_INIT(name)

#define OPAL_TIMING_ENV_INIT_PREFIX(prefix)

#define OPAL_TIMING_ENV_NEXT(h, fmt, ... )

#define OPAL_TIMING_ENV_CNT_PREFIX(prefix, func)

#define OPAL_TIMING_ENV_CNT(func)

#define OPAL_TIMING_ENV_GETDESC_PREFIX(prefix, func, i, desc)

#define OPAL_TIMING_ENV_GETDESC(func, index, desc)

#define OPAL_TIMING_ENV_ERROR_PREFIX(prefix, func)

#endif

#endif
