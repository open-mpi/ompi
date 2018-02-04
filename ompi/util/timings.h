/*
 * Copyright (c) 2017      Mellanox Technologies Ltd. All rights reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_UTIL_TIMING_H
#define OMPI_UTIL_TIMING_H

#include "opal/util/timings.h"
/* TODO: we need access to MPI_* functions */

#if (OPAL_ENABLE_TIMING)

#if !defined(OPAL_PMIX_RENAME_PREFIX)
#define OPAL_PMIX_RENAME_PREFIX ""
#endif

typedef struct {
    char desc[OPAL_TIMING_STR_LEN];
    double ts;
    char *file;
    char *prefix;
}   ompi_timing_val_t;

typedef struct {
    ompi_timing_val_t *val;
    int use;
    struct ompi_timing_list_t *next;
} ompi_timing_list_t;

typedef struct ompi_timing_t {
    double ts;
    const char *prefix;
    int size;
    int cnt;
    int error;
    int enabled;
    opal_timing_ts_func_t get_ts;
    ompi_timing_list_t *timing;
    ompi_timing_list_t *cur_timing;
} ompi_timing_t;

#define OMPI_TIMING_INIT(_size)                                                \
    ompi_timing_t OMPI_TIMING;                                                 \
    OMPI_TIMING.prefix = __func__;                                             \
    OMPI_TIMING.size = _size;                                                  \
    OMPI_TIMING.get_ts = opal_timing_ts_func(OPAL_TIMING_AUTOMATIC_TIMER);     \
    OMPI_TIMING.cnt = 0;                                                       \
    OMPI_TIMING.error = 0;                                                     \
    OMPI_TIMING.ts = OMPI_TIMING.get_ts();                                     \
    OMPI_TIMING.enabled = 0;                                                   \
    {                                                                          \
        char *ptr;                                                             \
        ptr = getenv("OMPI_TIMING_ENABLE");                                    \
        if (NULL != ptr) {                                                     \
            OMPI_TIMING.enabled = atoi(ptr);                                   \
        }                                                                      \
        if (OMPI_TIMING.enabled) {                                             \
            setenv("OPAL_TIMING_ENABLE", "1", 1);                              \
            OMPI_TIMING.timing = (ompi_timing_list_t*)malloc(sizeof(ompi_timing_list_t));              \
            memset(OMPI_TIMING.timing, 0, sizeof(ompi_timing_list_t));         \
            OMPI_TIMING.timing->val = (ompi_timing_val_t*)malloc(sizeof(ompi_timing_val_t) * _size);   \
            OMPI_TIMING.cur_timing = OMPI_TIMING.timing;                       \
        }                                                                      \
    }

#define OMPI_TIMING_ITEM_EXTEND                                                    \
    do {                                                                           \
        if (OMPI_TIMING.enabled) {                                                 \
            OMPI_TIMING.cur_timing->next = (struct ompi_timing_list_t*)malloc(sizeof(ompi_timing_list_t)); \
            OMPI_TIMING.cur_timing = (ompi_timing_list_t*)OMPI_TIMING.cur_timing->next;                    \
            memset(OMPI_TIMING.cur_timing, 0, sizeof(ompi_timing_list_t));                                 \
            OMPI_TIMING.cur_timing->val = malloc(sizeof(ompi_timing_val_t) * OMPI_TIMING.size);            \
        }                                                                          \
    } while(0)

#define OMPI_TIMING_FINALIZE                                                       \
    do {                                                                           \
        if (OMPI_TIMING.enabled) {                                                 \
            ompi_timing_list_t *t = OMPI_TIMING.timing, *tmp;                      \
            while ( NULL != t) {                                                   \
                tmp = t;                                                           \
                t = (ompi_timing_list_t*)t->next;                                  \
                free(tmp->val);                                                    \
                free(tmp);                                                         \
            }                                                                      \
            OMPI_TIMING.timing = NULL;                                             \
            OMPI_TIMING.cur_timing = NULL;                                         \
            OMPI_TIMING.cnt = 0;                                                   \
        }                                                                          \
    } while(0)

#define OMPI_TIMING_NEXT(...)                                                      \
    do {                                                                           \
        if (!OMPI_TIMING.error && OMPI_TIMING.enabled) {                           \
            char *f = strrchr(__FILE__, '/') + 1;                                  \
            int len = 0;                                                           \
            if (OMPI_TIMING.cur_timing->use >= OMPI_TIMING.size){                  \
                OMPI_TIMING_ITEM_EXTEND;                                           \
            }                                                                      \
            len = snprintf(OMPI_TIMING.cur_timing->val[OMPI_TIMING.cur_timing->use].desc,        \
                OPAL_TIMING_STR_LEN, ##__VA_ARGS__);                               \
            if (len >= OPAL_TIMING_STR_LEN) {                                      \
                OMPI_TIMING.error = 1;                                             \
            }                                                                      \
            OMPI_TIMING.cur_timing->val[OMPI_TIMING.cur_timing->use].file = strdup(f);     \
            OMPI_TIMING.cur_timing->val[OMPI_TIMING.cur_timing->use].prefix = strdup(__func__);      \
            OMPI_TIMING.cur_timing->val[OMPI_TIMING.cur_timing->use++].ts =        \
                OMPI_TIMING.get_ts() - OMPI_TIMING.ts;                             \
            OMPI_TIMING.cnt++;                                                     \
            OMPI_TIMING.ts = OMPI_TIMING.get_ts();                                 \
        }                                                                          \
    } while(0)

#define OMPI_TIMING_APPEND(filename,func,desc,ts)                                  \
    do {                                                                           \
        if (OMPI_TIMING.cur_timing->use >= OMPI_TIMING.size){                      \
            OMPI_TIMING_ITEM_EXTEND;                                               \
        }                                                                          \
        int len = snprintf(OMPI_TIMING.cur_timing->val[OMPI_TIMING.cur_timing->use].desc,        \
            OPAL_TIMING_STR_LEN, "%s", desc);                                      \
        if (len >= OPAL_TIMING_STR_LEN) {                                          \
            OMPI_TIMING.error = 1;                                                 \
        }                                                                          \
        OMPI_TIMING.cur_timing->val[OMPI_TIMING.cur_timing->use].prefix = func;    \
        OMPI_TIMING.cur_timing->val[OMPI_TIMING.cur_timing->use].file = filename;  \
        OMPI_TIMING.cur_timing->val[OMPI_TIMING.cur_timing->use++].ts = ts;        \
        OMPI_TIMING.cnt++;                                                         \
    } while(0)

#define OMPI_TIMING_IMPORT_OPAL_PREFIX(_prefix, func)                              \
    do {                                                                           \
        if (!OMPI_TIMING.error && OMPI_TIMING.enabled) {                           \
            int cnt;                                                               \
            int i;                                                                 \
            double ts;                                                             \
            OPAL_TIMING_ENV_CNT(func, cnt);                                        \
            OPAL_TIMING_ENV_ERROR_PREFIX(_prefix, func, OMPI_TIMING.error);        \
            for(i = 0; i < cnt; i++){                                              \
                char *desc, *filename;                                             \
                OPAL_TIMING_ENV_GETDESC_PREFIX(_prefix, &filename, func, i, &desc, ts);  \
                OMPI_TIMING_APPEND(filename, func, desc, ts);                      \
            }                                                                      \
        }                                                                          \
    } while(0)

#define OMPI_TIMING_IMPORT_OPAL(func)                                              \
        OMPI_TIMING_IMPORT_OPAL_PREFIX("", func);

#define OMPI_TIMING_OUT                                                           \
    do {                                                                          \
        if (OMPI_TIMING.enabled) {                                                \
            int i, size, rank;                                                    \
            MPI_Comm_size(MPI_COMM_WORLD, &size);                                 \
            MPI_Comm_rank(MPI_COMM_WORLD, &rank);                                 \
            int error = 0;                                                        \
                                                                                  \
            MPI_Reduce(&OMPI_TIMING.error, &error, 1,                             \
                MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);                             \
                                                                                  \
            if (error) {                                                          \
                if (0 == rank) {                                                  \
                    printf("==OMPI_TIMING== error: something went wrong, timings doesn't work\n"); \
                }                                                                 \
            }                                                                     \
            else {                                                                \
                double *avg = (double*)malloc(sizeof(double) * OMPI_TIMING.cnt);  \
                double *min = (double*)malloc(sizeof(double) * OMPI_TIMING.cnt);  \
                double *max = (double*)malloc(sizeof(double) * OMPI_TIMING.cnt);  \
                char **desc = (char**)malloc(sizeof(char*) * OMPI_TIMING.cnt);    \
                char **prefix = (char**)malloc(sizeof(char*) * OMPI_TIMING.cnt);  \
                char **file = (char**)malloc(sizeof(char*) * OMPI_TIMING.cnt);    \
                                                                                  \
                if( OMPI_TIMING.cnt > 0 ) {                                       \
                    OMPI_TIMING.ts = OMPI_TIMING.get_ts();                        \
                    ompi_timing_list_t *timing = OMPI_TIMING.timing;              \
                    i = 0;                                                        \
                    do {                                                          \
                        int use;                                                  \
                        for (use = 0; use < timing->use; use++) {                 \
                            MPI_Reduce(&timing->val[use].ts, avg + i, 1,          \
                                MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);          \
                            MPI_Reduce(&timing->val[use].ts, min + i, 1,          \
                                MPI_DOUBLE, MPI_MIN, 0, MPI_COMM_WORLD);          \
                            MPI_Reduce(&timing->val[use].ts, max + i, 1,          \
                                MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);          \
                            desc[i] = timing->val[use].desc;                      \
                            prefix[i] = timing->val[use].prefix;                  \
                            file[i] = timing->val[use].file;                      \
                            i++;                                                  \
                        }                                                         \
                        timing = (ompi_timing_list_t*)timing->next;               \
                    } while (timing != NULL);                                     \
                                                                                  \
                    if( 0 == rank ){                                              \
                        if (OMPI_TIMING.timing->next) {                           \
                            printf("==OMPI_TIMING== warning: added the extra timings allocation that might misrepresent the results.\n"            \
                                   "==OMPI_TIMING==          Increase the inited size of timings to avoid extra allocation during runtime.\n");    \
                        }                                                         \
                                                                                  \
                        printf("------------------ %s ------------------\n",      \
                                OMPI_TIMING.prefix);                              \
                        for(i=0; i< OMPI_TIMING.cnt; i++){                        \
                            avg[i] /= size;                                       \
                            printf("[%s:%s:%s]: %lf / %lf / %lf\n",               \
                                file[i], prefix[i], desc[i], avg[i], min[i], max[i]); \
                        }                                                         \
                        printf("[%s:overhead]: %lf \n", OMPI_TIMING.prefix,       \
                                OMPI_TIMING.get_ts() - OMPI_TIMING.ts);           \
                    }                                                             \
                }                                                                 \
                free(avg);                                                        \
                free(min);                                                        \
                free(max);                                                        \
                free(desc);                                                       \
                free(prefix);                                                     \
                free(file);                                                       \
            }                                                                     \
        }                                                                         \
    } while(0)

/* PMIx timings OMPI side */
#define PMIX_TIMING_ENV_CNT_PREFIX(prefix, func, _cnt)                            \
    do {                                                                          \
        char ename[OPAL_TIMING_STR_LEN];                                          \
        char *ptr = NULL;                                                         \
        int n = snprintf(ename, OPAL_TIMING_STR_LEN, "PMIX_TIMING_%s%s%s_CNT", prefix, OPAL_PMIX_RENAME_PREFIX, func);    \
        (_cnt) = 0;                                                               \
        if ( n <= OPAL_TIMING_STR_LEN ){                                          \
            ptr = getenv(ename);                                                  \
            if (NULL == ptr) {                                                    \
                n = snprintf(ename, OPAL_TIMING_STR_LEN, "PMIX_TIMING_%s%s_CNT", prefix, func); \
                if ( n <= OPAL_TIMING_STR_LEN ){ ptr = getenv(ename); };          \
            }                                                                     \
            if( NULL != ptr ){ (_cnt) = atoi(ptr); };                             \
        }                                                                         \
    } while(0)

#define PMIX_TIMING_ENV_CNT(func, _cnt)                                           \
    PMIX_TIMING_ENV_CNT_PREFIX("", func, _cnt)

#define PMIX_TIMING_ENV_GETDESC_PREFIX(prefix, filename, func, i, desc, funcname, _t)       \
    do {                                                                          \
        char vname[OPAL_TIMING_STR_LEN];                                          \
        (_t) = 0.0;                                                               \
        sprintf(vname, "PMIX_TIMING_%s%s%s_FILE_%d", prefix, OPAL_PMIX_RENAME_PREFIX, func, i); \
        *filename = getenv(vname);                                                \
        if (NULL == *filename) {                                                  \
            sprintf(vname, "PMIX_TIMING_%s%s_FILE_%d", prefix, func, i);          \
            *filename = getenv(vname);                                            \
        };                                                                        \
        sprintf(vname, "PMIX_TIMING_%s%s%s_DESC_%d", prefix, OPAL_PMIX_RENAME_PREFIX, func, i); \
        *desc = getenv(vname);                                                    \
        if (NULL == *desc) {                                                      \
            sprintf(vname, "PMIX_TIMING_%s%s_DESC_%d", prefix, func, i);          \
            *desc = getenv(vname);                                                \
        };                                                                        \
        sprintf(vname, "PMIX_TIMING_%s%s%s_VAL_%d", prefix, OPAL_PMIX_RENAME_PREFIX, func, i); \
        char *ptr = getenv(vname);                                                \
        if (NULL == ptr) {                                                        \
            sprintf(vname, "PMIX_TIMING_%s%s_VAL_%d", prefix, func, i);           \
            ptr = getenv(vname);                                                  \
        };                                                                        \
        sprintf(vname, "PMIX_TIMING_%s%s%s_FUNC_%d", prefix, OPAL_PMIX_RENAME_PREFIX, func, i); \
        *funcname = getenv(vname);                                                \
        if (NULL == *funcname) {                                                  \
            sprintf(vname, "PMIX_TIMING_%s%s_FUNC_%d", prefix, func, i);          \
            *funcname = getenv(vname);                                            \
        };                                                                        \
        if ( NULL != ptr ) {                                                      \
            sscanf(ptr,"%lf", &(_t));                                             \
        }                                                                         \
    } while(0)

#define PMIX_TIMING_ENV_ERROR_PREFIX(prefix, func, _err)                          \
    do {                                                                          \
        char ename[OPAL_TIMING_STR_LEN];                                          \
        (_err) = 0;                                                               \
        char *ptr = NULL;                                                         \
        int n = snprintf(ename, OPAL_TIMING_STR_LEN, "PMIX_TIMING_%s%s%s_ERROR", prefix, OPAL_PMIX_RENAME_PREFIX, func);  \
        if ( n <= OPAL_TIMING_STR_LEN ){                                          \
            ptr = getenv(ename);                                                  \
            if (NULL == ptr) {                                                    \
                n = snprintf(ename, OPAL_TIMING_STR_LEN, "PMIX_TIMING_%s%s_ERROR", prefix, func);  \
                if ( n <= OPAL_TIMING_STR_LEN ){                                  \
                    ptr = getenv(ename);                                          \
                }                                                                 \
            }                                                                     \
            if( NULL != ptr ){ (_err) = atoi(ptr); };                             \
        }                                                                         \
    } while(0)

#define PMIX_TIMING_RESET_CNTR_PREFIX(_prefix, func)                              \
    do {                                                                          \
        char vname[OPAL_TIMING_STR_LEN];                                          \
        int n = snprintf(vname, OPAL_TIMING_STR_LEN, "PMIX_TIMING_%s%s%s_CNT", _prefix, OPAL_PMIX_RENAME_PREFIX, func); \
        if ( n <= OPAL_TIMING_STR_LEN ){                                          \
            setenv(vname, "0", 1);                                                \
        }                                                                         \
        n = snprintf(vname, OPAL_TIMING_STR_LEN, "PMIX_TIMING_%s%s_CNT", _prefix, func); \
        if ( n <= OPAL_TIMING_STR_LEN ){                                          \
            setenv(vname, "0", 1);                                                \
        }                                                                         \
    } while(0)

#define OMPI_TIMING_IMPORT_PMIX_PREFIX(_prefix, func)                             \
    do {                                                                          \
        if (!OMPI_TIMING.error && OMPI_TIMING.enabled) {                          \
            int cnt;                                                              \
            int i;                                                                \
            double ts;                                                            \
            PMIX_TIMING_ENV_CNT(func, cnt);                                       \
            PMIX_TIMING_ENV_ERROR_PREFIX(_prefix, func, OMPI_TIMING.error);       \
            for(i = 0; i < cnt; i++){                                             \
                char *desc, *filename, *funcname;                                 \
                PMIX_TIMING_ENV_GETDESC_PREFIX(_prefix, &filename, func, i, &desc, &funcname, ts);  \
                if (NULL != strstr(funcname, OPAL_PMIX_RENAME_PREFIX)) {          \
                    funcname += strlen(OPAL_PMIX_RENAME_PREFIX);                  \
                }                                                                 \
                OMPI_TIMING_APPEND(filename, funcname, desc, ts);                 \
            }                                                                     \
            PMIX_TIMING_RESET_CNTR_PREFIX(_prefix, func);                         \
        }                                                                         \
    } while(0)

#define OMPI_TIMING_IMPORT_PMIX(func)                                             \
        OMPI_TIMING_IMPORT_PMIX_PREFIX("", func);
#else
#define OMPI_TIMING_INIT(size)

#define OMPI_TIMING_NEXT(...)

#define OMPI_TIMING_APPEND(desc,ts)

#define OMPI_TIMING_OUT

#define OMPI_TIMING_IMPORT_OPAL(func)

#define OMPI_TIMING_IMPORT_PMIX(func)

#define OMPI_TIMING_FINALIZE

#endif

#endif
