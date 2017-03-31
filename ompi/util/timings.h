#ifndef OMPI_UTIL_TIMING_H
#define OMPI_UTIL_TIMING_H

#include "opal/util/timings.h"
/* TODO: we need access to MPI_* functions */

#if (OPAL_ENABLE_TIMING)

typedef struct {
    char **desc;
    double *in;
    int use;
} ompi_timing_item_t;

typedef struct ompi_timing_list_t {
    ompi_timing_item_t *item;
    struct ompi_timing_list_t *next;
} ompi_timing_list_t;

typedef struct {
    double ts;
    const char *prefix;
    int inum;
    int cnt;
    int error;
    int enabled;
    opal_timing_ts_func_t get_ts;
    ompi_timing_list_t *timing;
} ompi_timing_env_t;

#define OMPI_TIMIG_ENV_ITEM_IDX(idx) ({                                        \
    __label__ exit;                                                            \
    ompi_timing_list_t *t = OMPI_TIMING_env.timing;                            \
    ompi_timing_item_t *item = NULL;                                           \
    int i, n;                                                                  \
    if ( idx > OMPI_TIMING_env.cnt) { goto exit; }                             \
    n = idx / OMPI_TIMING_env.inum;                                            \
    for (i = 0; i < n; i++) {                                                  \
        t = t->next;                                                           \
        if ( NULL == t ) { goto exit; }                                        \
    }                                                                          \
    i = (idx % OMPI_TIMING_env.inum);                                          \
    item = t->item;                                                            \
exit:                                                                          \
    item;                                                                      \
})

#define OMPI_TIMING_ITEM_INIT(item) {                                          \
    *item = (ompi_timing_item_t*)malloc(sizeof(ompi_timing_item_t));           \
    (*item)->in = (double*)malloc(sizeof(double) * OMPI_TIMING_env.inum);      \
    (*item)->desc = (char**)malloc(sizeof(char**) * OMPI_TIMING_env.inum);     \
    (*item)->use = 0;                                                          \
}

#define OMPI_TIMING_ENV_ITEM_EXTEND ({                                         \
    ompi_timing_list_t *t = OMPI_TIMING_env.timing;                            \
    while( NULL != t->next ) { t = t->next; };                                 \
    t->next = (ompi_timing_list_t*)malloc(sizeof(ompi_timing_list_t));         \
    if ( NULL == t->next) {                                                    \
        printf("OMPI_TIMING [%d %s]: error memory allocation\n",               \
            __LINE__, __FUNCTION__);                                           \
        abort();                                                               \
    }                                                                          \
    t->next->next = NULL;                                                      \
    OMPI_TIMING_ITEM_INIT(&t->next->item);                                     \
})

#define OMPI_TIMING_ENV_FINALIZE ({                                            \
    __label__ exit;                                                            \
    ompi_timing_list_t *t = OMPI_TIMING_env.timing, *tmp;                      \
    if ( NULL == t) { goto exit; }                                             \
    do {                                                                       \
        tmp = t;                                                               \
        t = t->next;                                                           \
        if ( tmp->item ) {                                                     \
            if ( tmp->item->desc ) {                                           \
                int i;                                                         \
                for (i = 0; i < tmp->item->use; i++) {                         \
                    free(tmp->item->desc[i]);                                  \
                    tmp->item->desc[i] = NULL;                                 \
                }                                                              \
                free(tmp->item->desc);                                         \
                tmp->item->desc = NULL;                                        \
            }                                                                  \
            if ( tmp->item->in ) { free(tmp->item->in); }                      \
            free(tmp->item);                                                   \
            tmp->item = NULL;                                                  \
        }                                                                      \
        free(tmp);                                                             \
        tmp = NULL;                                                            \
    } while ( NULL != t);                                                      \
exit:;                                                                         \
    OMPI_TIMING_env.cnt = 0;                                                   \
})

#define OMPI_TIMING_INIT(_inum)                                                \
    ompi_timing_env_t OMPI_TIMING_env;                                         \
    OMPI_TIMING_env.prefix = __FUNCTION__;                                     \
    OMPI_TIMING_env.inum = _inum;                                              \
    OMPI_TIMING_env.get_ts = opal_timing_ts_func(OPAL_TIMING_AUTOMATIC_TIMER); \
    OMPI_TIMING_env.cnt = 0;                                                   \
    OMPI_TIMING_env.error = 0;                                                 \
    OMPI_TIMING_env.ts = OMPI_TIMING_env.get_ts();                             \
    OMPI_TIMING_env.timing = (ompi_timing_list_t*)malloc(sizeof(ompi_timing_list_t));       \
    OMPI_TIMING_env.timing->next = NULL;                                       \
    OMPI_TIMING_ITEM_INIT(&OMPI_TIMING_env.timing->item);                      \

#define OMPI_TIMING_NEXT(fmt, ...) {                                           \
    ompi_timing_item_t *item;                                                  \
    if( OMPI_TIMING_env.cnt &&                                                 \
                !(OMPI_TIMING_env.cnt % OMPI_TIMING_env.inum) ){               \
        OMPI_TIMING_ENV_ITEM_EXTEND;                                           \
    }                                                                          \
    item = OMPI_TIMIG_ENV_ITEM_IDX(OMPI_TIMING_env.cnt);                       \
    if ( NULL == item ) {                                                      \
        printf("OMPI_TIMING [%d %s]: item not found\n",                        \
            __LINE__, __FUNCTION__);                                           \
        abort();                                                               \
    }                                                                          \
    item->in[OMPI_TIMING_env.cnt % OMPI_TIMING_env.inum] =                     \
                        OMPI_TIMING_env.get_ts() - OMPI_TIMING_env.ts;         \
    asprintf(&item->desc[OMPI_TIMING_env.cnt % OMPI_TIMING_env.inum],          \
            fmt, ## __VA_ARGS__);                                              \
    item->use++;                                                               \
    OMPI_TIMING_env.cnt++;                                                     \
    OMPI_TIMING_env.ts = OMPI_TIMING_env.get_ts();                             \
}

#define OMPI_TIMING_APPEND(desc,ts) {                                          \
    ompi_timing_item_t *item;                                                  \
    if( OMPI_TIMING_env.cnt && !(OMPI_TIMING_env.cnt % OMPI_TIMING_env.inum)){ \
        OMPI_TIMING_ENV_ITEM_EXTEND;                                           \
    }                                                                          \
    item = OMPI_TIMIG_ENV_ITEM_IDX(OMPI_TIMING_env.cnt);                       \
    if ( NULL == item ) {                                                      \
        printf("OMPI_TIMING [%d %s]: item not found\n",                        \
            __LINE__, __FUNCTION__);                                           \
        abort();                                                               \
    }                                                                          \
    item->in[OMPI_TIMING_env.cnt % OMPI_TIMING_env.inum] = ts;                 \
    item->desc[OMPI_TIMING_env.cnt % OMPI_TIMING_env.inum] = strdup(desc);     \
    item->use++;                                                               \
    OMPI_TIMING_env.cnt++;                                                     \
 }

#define OMPI_TIMING_IMPORT_OPAL_PREFIX(_prefix, func) {                        \
    char *enabled;                                                             \
    int cnt = OPAL_TIMING_ENV_CNT(func);                                       \
    int i;                                                                     \
    for(i = 0; i < cnt; i++){                                                  \
        char *desc;                                                            \
        double ts = OPAL_TIMING_ENV_GETDESC_PREFIX(_prefix, func, i, &desc);   \
        OMPI_TIMING_APPEND(desc, ts);                                          \
    }                                                                          \
}

#define OMPI_TIMING_IMPORT_OPAL(func) \
    OMPI_TIMING_IMPORT_OPAL_PREFIX("", func)

#define OMPI_TIMING_OUT ({                                                     \
    int i, size, rank;                                                         \
    MPI_Comm_size(MPI_COMM_WORLD, &size);                                      \
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);                                      \
    int n = (OMPI_TIMING_env.cnt - 1)/ OMPI_TIMING_env.inum + 1;               \
                                                                               \
    double *avg = (double*)malloc(sizeof(double) * OMPI_TIMING_env.cnt);       \
    double *min = (double*)malloc(sizeof(double) * OMPI_TIMING_env.cnt);       \
    double *max = (double*)malloc(sizeof(double) * OMPI_TIMING_env.cnt);       \
    char **desc = (char**)malloc(sizeof(char**) * OMPI_TIMING_env.cnt);        \
                                                                               \
    if( OMPI_TIMING_env.cnt > 0 ) {                                            \
        OMPI_TIMING_env.ts = OMPI_TIMING_env.get_ts();                         \
        ompi_timing_list_t *lst = OMPI_TIMING_env.timing;                      \
                                                                               \
        for (i = 0; i < n; i++) {                                              \
            int get_cnt = lst->item->use;                                      \
            double *avg_ptr = avg + (OMPI_TIMING_env.inum * i);                \
            double *min_ptr = min + (OMPI_TIMING_env.inum * i);                \
            double *max_ptr = max + (OMPI_TIMING_env.inum * i);                \
                                                                               \
            MPI_Reduce(lst->item->in, avg_ptr, get_cnt,                        \
                        MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);               \
            MPI_Reduce(lst->item->in, min_ptr, get_cnt,                        \
                        MPI_DOUBLE, MPI_MIN, 0, MPI_COMM_WORLD);               \
            MPI_Reduce(lst->item->in, max_ptr, get_cnt,                        \
                        MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);               \
            memcpy(desc + OMPI_TIMING_env.inum * i,                            \
                        lst->item->desc, sizeof(char**) * get_cnt);            \
                                                                               \
            lst = lst->next;                                                   \
        }                                                                      \
                                                                               \
        if( 0 == rank ){                                                       \
            printf("------------------ %s ------------------\n",               \
                    OMPI_TIMING_env.prefix);                                   \
            for(i=0; i< OMPI_TIMING_env.cnt; i++){                             \
                avg[i] /= size;                                                \
                printf("[%s:%s]: %lf / %lf / %lf\n",                           \
                    OMPI_TIMING_env.prefix, desc[i], avg[i], min[i], max[i]);  \
            }                                                                  \
            printf("[%s:overhead]: %lf \n", OMPI_TIMING_env.prefix,            \
                    OMPI_TIMING_env.get_ts() - OMPI_TIMING_env.ts);            \
        }                                                                      \
    }                                                                          \
    free(avg);                                                                 \
    free(min);                                                                 \
    free(max);                                                                 \
    free(desc);                                                                \
})


#else
#define OMPI_TIMING_INIT(inum)

#define OMPI_TIMING_NEXT(fmt, ...)

#define OMPI_TIMING_APPEND(desc,ts)

#define OMPI_TIMING_OUT

#define OMPI_TIMING_IMPORT_OPAL(func)

#endif

#endif
