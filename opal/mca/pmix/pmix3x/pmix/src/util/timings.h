/*
 * Copyright (C) 2014      Artem Polyakov <artpol84@gmail.com>
 * Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2017-2018 Mellanox Technologies Ltd. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_UTIL_TIMING_H
#define PMIX_UTIL_TIMING_H

#include <src/include/pmix_config.h>
#include <pmix_rename.h>

typedef enum {
    PMIX_TIMING_AUTOMATIC_TIMER,
    PMIX_TIMING_GET_TIME_OF_DAY,
    PMIX_TIMING_CYCLE_NATIVE,
    PMIX_TIMING_USEC_NATIVE
} pmix_timer_type_t;

typedef double (*pmix_timing_ts_func_t)(void);

#if PMIX_ENABLE_TIMING

#define PMIX_TIMING_STR_LEN 256

typedef struct {
    char id[PMIX_TIMING_STR_LEN], cntr_env[PMIX_TIMING_STR_LEN];
    int enabled, error;
    int cntr;
    double ts;
    pmix_timing_ts_func_t get_ts;
} pmix_timing_env_t;

pmix_timing_ts_func_t pmix_timing_ts_func(pmix_timer_type_t type);

#define PMIX_TIMING_ENV_START_TYPE(func, _nm, type, prefix)                       \
    do {                                                                          \
        char *ptr = NULL;                                                         \
        char *_prefix = prefix;                                                   \
        int n;                                                                    \
        if( NULL == prefix ){                                                     \
            _prefix = "";                                                         \
        }                                                                         \
        (_nm)->error = 0;                                                         \
        n = snprintf((_nm)->id, PMIX_TIMING_STR_LEN, "%s%s", _prefix, func);      \
        if( n > PMIX_TIMING_STR_LEN ){                                            \
             (_nm)->error = 1;                                                    \
        }                                                                         \
        n = sprintf((_nm)->cntr_env,"PMIX_TIMING_%s%s_CNT", prefix, (_nm)->id);   \
        if( n > PMIX_TIMING_STR_LEN ){                                            \
            (_nm)->error = 1;                                                     \
        }                                                                         \
        ptr = getenv((_nm)->id);                                                  \
        if( NULL == ptr || strcmp(ptr, "1")){                                     \
            (_nm)->enabled = 0;                                                   \
        }                                                                         \
        (_nm)->get_ts = pmix_timing_ts_func(type);                                \
        ptr = getenv("PMIX_TIMING_ENABLE");                                       \
        if (NULL != ptr) {                                                        \
            (_nm)->enabled = atoi(ptr);                                           \
        }                                                                         \
        (_nm)->cntr = 0;                                                          \
        ptr = getenv((_nm)->cntr_env);                                            \
        if( NULL != ptr ){                                                        \
            (_nm)->cntr = atoi(ptr);                                              \
        }                                                                         \
        (_nm)->ts = (_nm)->get_ts();                                              \
        if ( 0 != (_nm)->error ){                                                 \
            (_nm)->enabled = 0;                                                   \
        }                                                                         \
    } while(0)

#define PMIX_TIMING_ENV_DEF(name)                                                 \
    pmix_timing_env_t name ## _val, *name = &(name ## _val);

#define PMIX_TIMING_ENV_IMPORT(name)                                              \
    extern pmix_timing_env_t *name;

#define PMIX_TIMING_ENV_START(name)                                               \
    PMIX_TIMING_ENV_START_TYPE(__func__, name, PMIX_TIMING_AUTOMATIC_TIMER, "");

#define PMIX_TIMING_ENV_INIT(name)                                                \
    PMIX_TIMING_ENV_DEF(name);                                                    \
    PMIX_TIMING_ENV_START(name);

/* We use function names for identification
 * however this might be a problem for the private
 * functions declared as static as their names may
 * conflict.
 * Use prefix to do a finer-grained identification if needed
 */
#define PMIX_TIMING_ENV_INIT_PREFIX(prefix, name)                                 \
    do {                                                                          \
        pmix_timing_env_t name ## _val, *name = &(name ## _val);                  \
        *name = PMIX_TIMING_ENV_START_TYPE(__func__, PMIX_TIMING_AUTOMATIC_TIMER, prefix); \
    } while(0)

#define PMIX_TIMING_ENV_NEXT(h, ...)                                              \
    do {                                                                          \
        int n;                                                                    \
        char buf1[PMIX_TIMING_STR_LEN], buf2[PMIX_TIMING_STR_LEN];                \
        double time;                                                              \
        char *filename, *func;                                                    \
        if( h->enabled ){                                                         \
            /* enabled codepath */                                                \
            time = h->get_ts() - h->ts;                                           \
            n = snprintf(buf1, PMIX_TIMING_STR_LEN, "PMIX_TIMING_%s_DESC_%d", h->id, h->cntr); \
            if ( n > PMIX_TIMING_STR_LEN ){                                       \
                h->error = 1;                                                     \
            }                                                                     \
            n = snprintf(buf2, PMIX_TIMING_STR_LEN, __VA_ARGS__ );                \
            if ( n > PMIX_TIMING_STR_LEN ){                                       \
                h->error = 1;                                                     \
            }                                                                     \
            setenv(buf1, buf2, 1);                                                \
            n = snprintf(buf1, PMIX_TIMING_STR_LEN, "PMIX_TIMING_%s_VAL_%d", h->id, h->cntr);  \
            if ( n > PMIX_TIMING_STR_LEN ){                                       \
                h->error = 1;                                                     \
            }                                                                     \
            n = snprintf(buf2, PMIX_TIMING_STR_LEN, "%lf", time);                 \
            if ( n > PMIX_TIMING_STR_LEN ){                                       \
                h->error = 1;                                                     \
            }                                                                     \
            setenv(buf1, buf2, 1);                                                \
            filename = strrchr(__FILE__, '/') + 1;                                \
            n = snprintf(buf1, PMIX_TIMING_STR_LEN, "PMIX_TIMING_%s_FILE_%d", h->id, h->cntr); \
            if ( n > PMIX_TIMING_STR_LEN ){                                       \
                h->error = 1;                                                     \
            }                                                                     \
            n = snprintf(buf2, PMIX_TIMING_STR_LEN, "%s", filename);              \
            if ( n > PMIX_TIMING_STR_LEN ){                                       \
                h->error = 1;                                                     \
            }                                                                     \
            setenv(buf1, buf2, 1);                                                \
            n = snprintf(buf1, PMIX_TIMING_STR_LEN, "PMIX_TIMING_%s_FUNC_%d", h->id, h->cntr); \
            if ( n > PMIX_TIMING_STR_LEN ){                                       \
                h->error = 1;                                                     \
            }                                                                     \
            n = snprintf(buf2, PMIX_TIMING_STR_LEN, "%s", __func__);              \
            if ( n > PMIX_TIMING_STR_LEN ){                                       \
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
            n = snprintf(buf1, PMIX_TIMING_STR_LEN, "PMIX_TIMING_%s_ERROR", h->id);\
            if ( n > PMIX_TIMING_STR_LEN ){                                       \
                h->error = 1;                                                     \
            }                                                                     \
            n = snprintf(buf2, PMIX_TIMING_STR_LEN, "%d", h->error);              \
            if ( n > PMIX_TIMING_STR_LEN ){                                       \
                h->error = 1;                                                     \
            }                                                                     \
            setenv(buf1, buf2, 1);                                                \
        }                                                                         \
    } while(0)

/* This function supposed to be called from the code that will
 * do the postprocessing, i.e. OMPI timing portion that will
 * do the reduction of accumulated values
 */
#define PMIX_TIMING_ENV_CNT_PREFIX(prefix, func, _cnt)                            \
    do {                                                                          \
        char ename[PMIX_TIMING_STR_LEN];                                          \
        char *ptr = NULL;                                                         \
        int n = snprintf(ename, PMIX_TIMING_STR_LEN, "PMIX_TIMING_%s%s_CNT", prefix, func);    \
        (_cnt) = 0;                                                               \
        if ( n <= PMIX_TIMING_STR_LEN ){                                          \
            ptr = getenv(ename);                                                  \
            if( NULL != ptr ){ (_cnt) = atoi(ptr); };                             \
        }                                                                         \
    } while(0)

#define PMIX_TIMING_ENV_ERROR_PREFIX(prefix, func, _err)                          \
    do {                                                                          \
        char ename[PMIX_TIMING_STR_LEN];                                          \
        (_err) = 0;                                                               \
        char *ptr = NULL;                                                         \
        int n = snprintf(ename, PMIX_TIMING_STR_LEN, "PMIX_TIMING_%s%s_ERROR", prefix, func);    \
        if ( n <= PMIX_TIMING_STR_LEN ){                                          \
            ptr = getenv(ename);                                                  \
            if( NULL != ptr ){ (_err) = atoi(ptr); };                             \
        }                                                                         \
    } while(0)

#define PMIX_TIMING_ENV_CNT(func, _cnt)                                           \
    PMIX_TIMING_ENV_CNT_PREFIX("", func, _cnt)

#define PMIX_TIMING_ENV_GETDESC_PREFIX(prefix, filename, func, i, desc, _t)       \
    do {                                                                          \
        char vname[PMIX_TIMING_STR_LEN];                                          \
        (_t) = 0.0;                                                               \
        sprintf(vname, "PMIX_TIMING_%s%s_FILE_%d", prefix, func, i);              \
        *filename = getenv(vname);                                                \
        sprintf(vname, "PMIX_TIMING_%s%s_DESC_%d", prefix, func, i);              \
        *desc = getenv(vname);                                                    \
        sprintf(vname, "PMIX_TIMING_%s%s_VAL_%d", prefix, func, i);               \
        char *ptr = getenv(vname);                                                \
        if ( NULL != ptr ) {                                                      \
            sscanf(ptr,"%lf", &(_t));                                             \
        }                                                                         \
    } while(0)

#define PMIX_TIMING_ENV_GETDESC(file, func, index, desc)                          \
    PMIX_TIMING_ENV_GETDESC_PREFIX("", file, func, index, desc)


#else

#define PMIX_TIMING_ENV_START_TYPE(func, type, prefix)

#define PMIX_TIMING_ENV_INIT(name)

#define PMIX_TIMING_ENV_INIT_PREFIX(prefix)

#define PMIX_TIMING_ENV_NEXT(h, ... )

#define PMIX_TIMING_ENV_CNT_PREFIX(prefix, func)

#define PMIX_TIMING_ENV_CNT(func)

#define PMIX_TIMING_ENV_GETDESC_PREFIX(prefix, func, i, desc)

#define PMIX_TIMING_ENV_GETDESC(func, index, desc)

#define PMIX_TIMING_ENV_ERROR_PREFIX(prefix, func)

#define PMIX_TIMING_ENV_DEF(name)

#define PMIX_TIMING_ENV_START(name)

#endif

#endif
