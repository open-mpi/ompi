#ifndef OMPI_UTIL_TIMING_H
#define OMPI_UTIL_TIMING_H

#include "opal/util/timings.h"
/* TODO: we need access to MPI_* functions */

#if (1 && OPAL_ENABLE_TIMING)

typedef struct {
    char **desc;
    double *in;
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

/*
#define OMPI_TIMIG_ENV_ITEM_IDX(idx) ({                                        \
    __label__ exit;                                                            \
    ompi_timing_list_t *t = NULL;                                              \
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
*/

inline static ompi_timing_item_t * OMPI_TIMIG_ENV_ITEM_IDX(ompi_timing_env_t *OMPI_TIMING_env, int idx) {
    __label__ exit;
    ompi_timing_list_t *t = OMPI_TIMING_env->timing;
    ompi_timing_item_t *item = NULL;
    int i, n;
    if ( idx > OMPI_TIMING_env->cnt) { goto exit; }
    n = idx / OMPI_TIMING_env->inum;
    for (i = 0; i < n; i++) {
        t = t->next;
        if ( NULL == t ) { goto exit; }
    }
    i = (idx % OMPI_TIMING_env->inum);
    item = t->item;
exit:
    return item;
}

static inline void OMPI_TIMING_ITEM_INIT(ompi_timing_env_t *OMPI_TIMING_env, ompi_timing_item_t **item) {
    *item = (ompi_timing_item_t*)malloc(sizeof(ompi_timing_item_t));
    (*item)->in = (double*)malloc(sizeof(double) * OMPI_TIMING_env->inum);
    (*item)->desc = (char**)malloc(sizeof(char**) * OMPI_TIMING_env->inum);
}

/*
#define OMPI_TIMING_ITEM_INIT(item) {                                          \
    item->in = (double*)malloc(sizeof(double) * OMPI_TIMING_env.inum);         \
    item->max = (double*)malloc(sizeof(double) * OMPI_TIMING_env.inum);        \
    item->min = (double*)malloc(sizeof(double) * OMPI_TIMING_env.inum);        \
    item->avg = (double*)malloc(sizeof(double) * OMPI_TIMING_env.inum);        \
    item->desc = (char**)malloc(sizeof(char*) * OMPI_TIMING_env.inum);         \
    /*memset(item, 0, sizeof(ompi_timing_item_t) * OMPI_TIMING_env.inum);        \
}
*/
/*
#define OMPI_TIMING_ENV_ITEM_EXTEND ({                                         \
    ompi_timing_list_t *t = OMPI_TIMING_env.timing;                            \
    while( NULL != t->next ) { t = t->next; };                                 \
    t->next = (ompi_timing_list_t*)malloc(sizeof(ompi_timing_list_t));         \
    if ( NULL == t->next) {                                                    \
        printf("OMPI_TIMING [%d %s]: error memory allocation\n",               \
            __LINE__, __FUNCTION__);                                           \
        abort();                                                               \
    }                                                                          \
    memset(t->next, 0, sizeof(ompi_timing_list_t) * OMPI_TIMING_env.inum);     \
    OMPI_TIMING_ITEM_INIT(t->next);                                            \
})
*/

inline static void OMPI_TIMING_ENV_ITEM_EXTEND(ompi_timing_env_t *OMPI_TIMING_env) {
    ompi_timing_list_t *t = OMPI_TIMING_env->timing;
    while( NULL != t->next ) { t = t->next; };
    t->next = (ompi_timing_list_t*)malloc(sizeof(ompi_timing_list_t));
    if ( NULL == t->next) {
        printf("OMPI_TIMING [%d %s]: error memory allocation\n",
            __LINE__, __FUNCTION__);
        abort();
    }
    t->next->next = NULL;
    OMPI_TIMING_ITEM_INIT(OMPI_TIMING_env, &t->next->item);
}

inline static void OMPI_TIMING_ENV_FINALIZE(ompi_timing_env_t *OMPI_TIMING_env) {
    __label__ exit;
    ompi_timing_list_t *t = OMPI_TIMING_env->timing, *tmp;
    if ( NULL == t) { goto exit; }
    do {
        tmp = t;
        t = t->next;
        if ( tmp->item ) {
            if ( tmp->item->desc ) { free(tmp->item->desc); }
            if ( tmp->item->in ) { free(tmp->item->in); }
            free(tmp->item);
        }
        free(tmp);
        tmp = NULL;
    } while ( NULL != t);
exit:;
    OMPI_TIMING_env->cnt = 0;
}

/*
#define OMPI_TIMING_ENV_FINALIZE ({                                            \
    __label__ exit;                                                            \
    ompi_timing_list_t *t = OMPI_TIMING_env.timing, *tmp;                      \
    if ( NULL == t) { goto exit; }                                             \
    do {                                                                       \
        tmp = t;                                                               \
        t = t->next;                                                           \
        if ( tmp->item ) {                                                     \
            if ( tmp->item->desc ) { free(tmp->item->desc); }                  \
            if ( tmp->item->in ) { free(tmp->item->desc); }                    \
            free(tmp->item);                                                   \
        }                                                                      \
        free(tmp);                                                             \
    } while ( NULL != t->next);                                                \
    tmp = t;                                                                   \
    if ( tmp->item ) {                                                         \
        if ( tmp->item->desc ) { free(tmp->item->desc); }                      \
        if ( tmp->item->in ) { free(tmp->item->desc); }                        \
        free(tmp->item);                                                       \
    }                                                                          \
    free(OMPI_TIMING_env.timing);                                              \
    OMPI_TIMING_env.timing = NULL;                                             \
exit:;                                                                         \
    OMPI_TIMING_env.cnt = 0;                                                   \
})*/

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
    /*OMPI_TIMING_env.timing->item = (ompi_timing_item_t*)malloc(sizeof(ompi_timing_item_t));*/ \
    OMPI_TIMING_ITEM_INIT(&OMPI_TIMING_env, &OMPI_TIMING_env.timing->item);                       \

/*
#define OMPI_TIMING_NEXT(fmt, ...) {                                           \
    ompi_timing_item_t *item;                                                  \
    if( OMPI_TIMING_env.inum == OMPI_TIMING_env.cnt ){                         \
        OMPI_TIMING_ENV_ITEM_EXTEND;                                           \
    }                                                                          \
    item = OMPI_TIMIG_ENV_ITEM_IDX(OMPI_TIMING_env.cnt);                       \
    if ( NULL == item ) {                                                      \
        printf("OMPI_TIMING [%d %s]: item not found\n",                        \
            __LINE__, __FUNCTION__);                                           \
        abort();                                                               \
    }                                                                          \
    item->in[OMPI_TIMING_env.cnt] = OMPI_TIMING_env.get_ts() - OMPI_TIMING_env.ts;         \
    asprintf(&item->desc[OMPI_TIMING_env.cnt], fmt, ## __VA_ARGS__);           \
    OMPI_TIMING_env.cnt++;                                                     \
    OMPI_TIMING_env.ts = OMPI_TIMING_env.get_ts();                             \
}*/

inline static void __OMPI_TIMING_NEXT(ompi_timing_env_t *OMPI_TIMING_env, char * desc) {
    ompi_timing_item_t *item;
    if( OMPI_TIMING_env->cnt && !(OMPI_TIMING_env->cnt % OMPI_TIMING_env->inum) ){
        OMPI_TIMING_ENV_ITEM_EXTEND(OMPI_TIMING_env);
    }
    item = OMPI_TIMIG_ENV_ITEM_IDX(OMPI_TIMING_env, OMPI_TIMING_env->cnt);
    if ( NULL == item ) {
        printf("OMPI_TIMING [%d %s]: item not found\n",
            __LINE__, __FUNCTION__);
        abort();
    }
    item->in[OMPI_TIMING_env->cnt % OMPI_TIMING_env->inum] = OMPI_TIMING_env->get_ts() - OMPI_TIMING_env->ts;
    item->desc[OMPI_TIMING_env->cnt % OMPI_TIMING_env->inum] = strdup(desc);
    OMPI_TIMING_env->cnt++;
    OMPI_TIMING_env->ts = OMPI_TIMING_env->get_ts();
}

#define OMPI_TIMING_NEXT(desc) \
    __OMPI_TIMING_NEXT(&OMPI_TIMING_env, desc)

#define OMPI_TIMING_APPEND(desc,ts) {                                          \
    ompi_timing_item_t *item;                                                  \
    if( OMPI_TIMING_env.cnt && !(OMPI_TIMING_env.cnt % OMPI_TIMING_env.inum) ){                         \
        OMPI_TIMING_ENV_ITEM_EXTEND(&OMPI_TIMING_env);                                           \
    }                                                                          \
    item = OMPI_TIMIG_ENV_ITEM_IDX(&OMPI_TIMING_env, OMPI_TIMING_env.cnt);                       \
    if ( NULL == item ) {                                                      \
        printf("OMPI_TIMING [%d %s]: item not found\n",                        \
            __LINE__, __FUNCTION__);                                           \
        abort();                                                               \
    }                                                                          \
    item->in[OMPI_TIMING_env.cnt % OMPI_TIMING_env.inum] = ts;                                        \
    item->desc[OMPI_TIMING_env.cnt % OMPI_TIMING_env.inum] = strdup(desc);                            \
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

inline static void __OMPI_TIMING_OUT(ompi_timing_env_t *OMPI_TIMING_env) {
    int i, size, rank;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    int n = (OMPI_TIMING_env->cnt - 1)/ OMPI_TIMING_env->inum + 1;
    int tail = OMPI_TIMING_env->cnt % OMPI_TIMING_env->inum;
    int get_cnt = OMPI_TIMING_env->inum;

    double *avg = (double*)malloc(sizeof(double) * OMPI_TIMING_env->cnt);
    double *min = (double*)malloc(sizeof(double) * OMPI_TIMING_env->cnt);
    double *max = (double*)malloc(sizeof(double) * OMPI_TIMING_env->cnt);
    char **desc = (char**)malloc(sizeof(char**) * OMPI_TIMING_env->cnt);

    if( OMPI_TIMING_env->cnt > 0 ) {
        OMPI_TIMING_env->ts = OMPI_TIMING_env->get_ts();
        ompi_timing_list_t *lst = OMPI_TIMING_env->timing;

        for (i = 0; i < n; i++) {
            if (i == (n-1)) {
                get_cnt = tail == 0 ? OMPI_TIMING_env->inum : tail;
            }
            double *avg_ptr = avg + (OMPI_TIMING_env->inum * i);
            double *min_ptr = min + (OMPI_TIMING_env->inum * i);
            double *max_ptr = max + (OMPI_TIMING_env->inum * i);

            MPI_Reduce(lst->item->in, avg_ptr, get_cnt, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
            MPI_Reduce(lst->item->in, min_ptr, get_cnt, MPI_DOUBLE, MPI_MIN, 0, MPI_COMM_WORLD);
            MPI_Reduce(lst->item->in, max_ptr, get_cnt, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);
            memcpy(desc + OMPI_TIMING_env->inum * i, lst->item->desc, sizeof(char**) * get_cnt);

            lst = lst->next;
        }

        if( 0 == rank ){
            printf("------------------ %s ------------------\n",
                    OMPI_TIMING_env->prefix);
            for(i=0; i< OMPI_TIMING_env->cnt; i++){
                avg[i] /= size;
                printf("[%s:%s]: %lf / %lf / %lf\n",
                    OMPI_TIMING_env->prefix, desc[i], avg[i], min[i], max[i]);
            }
            printf("[%s:overhead]: %lf \n", OMPI_TIMING_env->prefix,
                    OMPI_TIMING_env->get_ts() - OMPI_TIMING_env->ts);
        }
    }
    free(avg);
    free(min);
    free(max);
    free(desc);
}


#define OMPI_TIMING_OUT \
    __OMPI_TIMING_OUT(&OMPI_TIMING_env)

#if 0
#define OMPI_TIMING_OUT {                                                      \
    int i, size, rank;                                                         \
    MPI_Comm_size(MPI_COMM_WORLD, &size);                                      \
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);                                      \
    char ename[1024];                                                          \
    sprintf(ename, "OMPI_TIMING_%s", OMPI_TIMING_env.prefix);                  \
    char *ptr = getenv(ename);                                                 \
                                                                               \
    if( NULL != ptr ) {                                                        \
        OMPI_TIMING_env.ts = OMPI_TIMING_env.get_ts();                         \
        MPI_Reduce(&OMPI_TIMING_env.timing->item->in, &OMPI_TIMING_env.timing->item->avg, OMPI_TIMING_env.cnt,           \
                    MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);                   \
        /*MPI_Reduce(OMPI_TIMING_in, OMPI_TIMING_min, OMPI_TIMING_cnt,           \
                    MPI_DOUBLE, MPI_MIN, 0, MPI_COMM_WORLD);                   \
        MPI_Reduce(OMPI_TIMING_in, OMPI_TIMING_max, OMPI_TIMING_cnt,           \
                    MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);                   \
        */                                                                       \
        if( 0 == rank ){                                                       \
            printf("------------------ %s ------------------\n",               \
                    OMPI_TIMING_env.prefix);                                   \
            for(i=0; i< OMPI_TIMING_env.cnt; i++){                                 \
                OMPI_TIMING_env.timing->item->avg[i] /= size;                                    \
                                printf("[%s:%s]: %lf\n",                           \
                    OMPI_TIMING_env.prefix, OMPI_TIMING_env.timing->item->desc[i],                    \
                    OMPI_TIMING_env.timing->item->avg[i]);                           \
\
                /*printf("[%s:%s]: %lf / %lf / %lf\n",                           \
                    OMPI_TIMING_prefix,OMPI_TIMING_desc[i],                    \
                    OMPI_TIMING_avg[i], OMPI_TIMING_min[i],                    \
                    OMPI_TIMING_max[i]); */                                      \
            }                                                                  \
            printf("[%s:overhead]: %lf \n", OMPI_TIMING_env.prefix,                \
                    OMPI_TIMING_env.get_ts() - OMPI_TIMING_env.ts);                    \
        }                                                                      \
    }                                                                          \
}
#endif

#else
#define OMPI_TIMING_INIT(inum)

#define OMPI_TIMING_NEXT(fmt, ...)

#define OMPI_TIMING_APPEND(desc,ts)

#define OMPI_TIMING_OUT

#define OMPI_TIMING_IMPORT_OPAL(func)

#endif

#endif
