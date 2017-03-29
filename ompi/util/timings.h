#ifndef OMPI_UTIL_TIMING_H
#define OMPI_UTIL_TIMING_H

#include "opal/util/timings.h"
/* TODO: we need access to MPI_* functions */

#if (0 && OPAL_ENABLE_TIMING)

/* TODO: replace with opal_timing function */
static inline double OMPI_TIMING_GET_TS(void)
{
    struct timespec ts;
    double ret;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    ret = ts.tv_sec + 1E-9 * ts.tv_nsec;
    return ret;
}

/* TODO:
 * - create a structure to hold this variables
 * - use dyncamically extendable arrays
 */
#define OMPI_TIMING_INIT(inum)                                  \
        double OMPI_TIMING_ts = OMPI_TIMING_GET_TS();           \
        const char *OMPI_TIMING_prefix = __FUNCTION__;          \
        int OMPI_TIMING_cnt = 0;                                \
        int OMPI_TIMING_inum = inum;                            \
        double OMPI_TIMING_in[inum]  = { 0.0 };                 \
        double OMPI_TIMING_max[inum] = { 0.0 };                 \
        double OMPI_TIMING_min[inum] = { 0.0 };                 \
        double OMPI_TIMING_avg[inum] = { 0.0 };                 \
        char *OMPI_TIMING_desc[inum] = { 0 };                   \


/* TODO: provide printf-like interfase allowing to build a string
 * at runtime, like OPAL_TIMING_NEXT()
 */
#define OMPI_TIMING_NEXT(desc) {                                       \
    char *ptr = strrchr(__FILE__, '/');                           \
    if( NULL == ptr ){                                            \
        ptr = __FILE__;                                           \
    } else {                                                      \
        ptr++;                                                    \
    }                                                             \
    if( OMPI_TIMING_inum <= OMPI_TIMING_cnt ){                            \
        printf("OMPI_TIMING [%s:%d %s]: interval count overflow!!\n", \
            ptr, __LINE__, __FUNCTION__);                         \
        abort();                                                  \
    }                                                             \
    OMPI_TIMING_in[OMPI_TIMING_cnt] =    OMPI_TIMING_GET_TS() - OMPI_TIMING_ts;   \
    OMPI_TIMING_desc[OMPI_TIMING_cnt++] = desc;                           \
    OMPI_TIMING_ts = OMPI_TIMING_GET_TS();                                \
}

#define OMPI_TIMING_APPEND(desc,ts) {                                   \
    char *ptr = strrchr(__FILE__, '/');                           \
    if( NULL == ptr ){                                            \
        ptr = __FILE__;                                           \
    } else {                                                      \
        ptr++;                                                    \
    }                                                             \
    if( OMPI_TIMING_inum <= OMPI_TIMING_cnt ){                            \
        printf("OMPI_TIMING [%s:%d %s]: interval count overflow!!\n", \
            ptr, __LINE__, __FUNCTION__);                         \
        abort();                                                  \
    }                                                             \
    OMPI_TIMING_in[OMPI_TIMING_cnt] = ts;                                 \
    OMPI_TIMING_desc[OMPI_TIMING_cnt++] = desc;                           \
}

#define OMPI_TIMING_IMPORT_OPAL(func) {                          \
    char *enabled;                                            \
    int cnt = OPAL_TIMING_ENV_CNT(func);                      \
    if( 0 < cnt )  {         \
        char ename[256];                                      \
        sprintf(ename, "OMPI_TIMING_%s", OMPI_TIMING_prefix);         \
        setenv(ename, "1", 1);                                \
    }                                                         \
    int i;                                                    \
    for(i = 0; i < cnt; i++){                                 \
        char *desc;                                           \
        double ts = OPAL_TIMING_ENV_GETDESC(prefix, i, &desc);   \
        OMPI_TIMING_APPEND(desc, ts);                               \
    }                                                         \
}


#define OMPI_TIMING_OUT {                                                   \
    int i, size, rank;                                                  \
    MPI_Comm_size(MPI_COMM_WORLD, &size);                               \
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);                               \
    char ename[1024];                                                   \
    sprintf(ename, "OMPI_TIMING_%s", OMPI_TIMING_prefix);                       \
    char *ptr = getenv(ename);                                          \
                                                                        \
    if( NULL != ptr ) {                                                 \
        OMPI_TIMING_ts = OMPI_TIMING_GET_TS();                                  \
        MPI_Reduce(OMPI_TIMING_in, OMPI_TIMING_avg, OMPI_TIMING_cnt, MPI_DOUBLE,    \
                    MPI_SUM, 0, MPI_COMM_WORLD);                        \
        MPI_Reduce(OMPI_TIMING_in, OMPI_TIMING_min, OMPI_TIMING_cnt, MPI_DOUBLE,    \
                    MPI_MIN, 0, MPI_COMM_WORLD);                        \
        MPI_Reduce(OMPI_TIMING_in, OMPI_TIMING_max, OMPI_TIMING_cnt, MPI_DOUBLE,    \
                    MPI_MAX, 0, MPI_COMM_WORLD);                        \
                                                                        \
        if( 0 == rank ){                                                \
            printf("------------------ %s ------------------\n",        \
                    OMPI_TIMING_prefix);                                    \
            for(i=0; i< OMPI_TIMING_cnt; i++){                              \
                OMPI_TIMING_avg[i] /= size;                                 \
                printf("[%s:%s]: %lf / %lf / %lf\n",                    \
                    OMPI_TIMING_prefix,OMPI_TIMING_desc[i],                     \
                    OMPI_TIMING_avg[i], OMPI_TIMING_min[i], OMPI_TIMING_max[i]);    \
            }                                                           \
            printf("[%s:overhead]: %lf \n", OMPI_TIMING_prefix,             \
                    OMPI_TIMING_GET_TS() - OMPI_TIMING_ts);                     \
        }                                                               \
    }                                                                   \
}

#else
#define OMPI_TIMING_INIT(inum)

#define OMPI_TIMING_NEXT(desc)

#define OMPI_TIMING_APPEND(desc,ts)

#define OMPI_TIMING_OUT

#define OMPI_TIMING_IMPORT_OPAL(func)

#endif

#endif
