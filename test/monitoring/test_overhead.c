/*
 * Copyright (c) 2016-2017 Inria.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
  Measurement for the pml_monitoring component overhead

  Designed by Clement Foyer <clement.foyer@inria.fr>
  Contact the authors for questions.

  To be run as:


*/

#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <time.h>
#include <string.h>
#include "mpi.h"

#define NB_ITER 1000
#define FULL_NB_ITER (size_world * NB_ITER)
#define MAX_SIZE (1024 * 1024 * 1.4)
#define NB_OPS 6

static int rank_world = -1;
static int size_world = 0;
static int to = -1;
static int from = -1;
static MPI_Win win = MPI_WIN_NULL;

/* Sorting results */
static int comp_double(const void*_a, const void*_b)
{
    const double*a = _a;
    const double*b = _b;
    if(*a < *b)
        return -1;
    else if(*a > *b)
        return 1;
    else
        return 0;
}

/* Timing */
static inline void get_tick(struct timespec*t)
{
#if defined(__bg__)
#  define CLOCK_TYPE CLOCK_REALTIME
#elif defined(CLOCK_MONOTONIC_RAW)
#  define CLOCK_TYPE CLOCK_MONOTONIC_RAW
#elif defined(CLOCK_MONOTONIC)
#  define CLOCK_TYPE CLOCK_MONOTONIC
#endif
#if defined(CLOCK_TYPE)
    clock_gettime(CLOCK_TYPE, t);
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    t->tv_sec = tv.tv_sec;
    t->tv_nsec = tv.tv_usec * 1000;
#endif
}
static inline double timing_delay(const struct timespec*const t1, const struct timespec*const t2)
{
    const double delay = 1000000.0 * (t2->tv_sec - t1->tv_sec) + (t2->tv_nsec - t1->tv_nsec) / 1000.0;
    return delay;
}

/* Operations */
static inline void op_send(double*res, void*sbuf, int size, int tagno, void*rbuf) {
    MPI_Request request;
    struct timespec start, end;

    /* Post to be sure no unexpected message will be generated */
    MPI_Irecv(rbuf, size, MPI_BYTE, from, tagno, MPI_COMM_WORLD, &request);
            
    /* Token ring to synchronize */
    /* We message the sender to make him know we are ready to
       receive (even for non-eager mode sending) */
    if( 0 == rank_world ) {
        MPI_Send(NULL, 0, MPI_BYTE, from, 100, MPI_COMM_WORLD);
        MPI_Recv(NULL, 0, MPI_BYTE, to, 100, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    } else {
        MPI_Recv(NULL, 0, MPI_BYTE, to, 100, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        MPI_Send(NULL, 0, MPI_BYTE, from, 100, MPI_COMM_WORLD);
    }

    /* do monitored operation */
    get_tick(&start);
    MPI_Send(sbuf, size, MPI_BYTE, to, tagno, MPI_COMM_WORLD);
    get_tick(&end);

    MPI_Wait(&request, MPI_STATUS_IGNORE);
    *res = timing_delay(&start, &end);
}

static inline void op_send_pingpong(double*res, void*sbuf, int size, int tagno, void*rbuf) {
    struct timespec start, end;

    MPI_Barrier(MPI_COMM_WORLD);

    /* do monitored operation */
    if(rank_world % 2) { /* Odd ranks : Recv - Send */
        MPI_Recv(rbuf, size, MPI_BYTE, from, tagno, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        MPI_Send(sbuf, size, MPI_BYTE, from, tagno, MPI_COMM_WORLD);
        MPI_Barrier(MPI_COMM_WORLD);
        get_tick(&start);
        MPI_Send(sbuf, size, MPI_BYTE, from, tagno, MPI_COMM_WORLD);
        MPI_Recv(rbuf, size, MPI_BYTE, from, tagno, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        get_tick(&end);
    } else { /* Even ranks : Send - Recv */
        get_tick(&start);
        MPI_Send(sbuf, size, MPI_BYTE, to, tagno, MPI_COMM_WORLD);
        MPI_Recv(rbuf, size, MPI_BYTE, to, tagno, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        get_tick(&end);
        MPI_Barrier(MPI_COMM_WORLD);
        MPI_Recv(rbuf, size, MPI_BYTE, to, tagno, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        MPI_Send(sbuf, size, MPI_BYTE, to, tagno, MPI_COMM_WORLD);
    }

    *res = timing_delay(&start, &end) / 2;
}

static inline void op_coll(double*res, void*buff, int size, int tagno, void*rbuf) {
    struct timespec start, end;
    MPI_Barrier(MPI_COMM_WORLD);

    /* do monitored operation */
    get_tick(&start);
    MPI_Bcast(buff, size, MPI_BYTE, 0, MPI_COMM_WORLD);
    get_tick(&end);

    *res = timing_delay(&start, &end);
}

static inline void op_a2a(double*res, void*sbuf, int size, int tagno, void*rbuf) {
    struct timespec start, end;
    MPI_Barrier(MPI_COMM_WORLD);

    /* do monitored operation */
    get_tick(&start);
    MPI_Alltoall(sbuf, size, MPI_BYTE, rbuf, size, MPI_BYTE, MPI_COMM_WORLD);
    get_tick(&end);

    *res = timing_delay(&start, &end);
}

static inline void op_put(double*res, void*sbuf, int size, int tagno, void*rbuf) {
    struct timespec start, end;

    MPI_Win_lock(MPI_LOCK_EXCLUSIVE, to, 0, win);

    /* do monitored operation */
    get_tick(&start);
    MPI_Put(sbuf, size, MPI_BYTE, to, 0, size, MPI_BYTE, win);
    MPI_Win_unlock(to, win);
    get_tick(&end);

    *res = timing_delay(&start, &end);
}

static inline void op_get(double*res, void*rbuf, int size, int tagno, void*sbuf) {
    struct timespec start, end;

    MPI_Win_lock(MPI_LOCK_SHARED, to, 0, win);

    /* do monitored operation */
    get_tick(&start);
    MPI_Get(rbuf, size, MPI_BYTE, to, 0, size, MPI_BYTE, win);
    MPI_Win_unlock(to, win);
    get_tick(&end);

    *res = timing_delay(&start, &end);
}

static inline void do_bench(int size, char*sbuf, double*results,
                            void(*op)(double*, void*, int, int, void*)) {
    int iter;
    int tagno = 201;
    char*rbuf = sbuf ? sbuf + size : NULL;

    if(op == op_put || op == op_get){
	win = MPI_WIN_NULL;
	MPI_Win_create(rbuf, size, 1, MPI_INFO_NULL, MPI_COMM_WORLD, &win);
    }

    for( iter = 0; iter < NB_ITER; ++iter ) {
        op(&results[iter], sbuf, size, tagno, rbuf);
        MPI_Barrier(MPI_COMM_WORLD);
    }

    if(op == op_put || op == op_get){
	MPI_Win_free(&win);
	win = MPI_WIN_NULL;
    }
}

int main(int argc, char* argv[])
{
    int size, iter, nop;
    char*sbuf = NULL;
    double results[NB_ITER];
    void(*op)(double*, void*, int, int, void*);
    char name[255];
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank_world);
    MPI_Comm_size(MPI_COMM_WORLD, &size_world);
    to = (rank_world + 1) % size_world;
    from = (rank_world + size_world - 1) % size_world;

    double full_res[FULL_NB_ITER];

    for( nop = 0; nop < NB_OPS; ++nop ) {
        switch(nop) {
        case 0:
            op = op_send;
            sprintf(name, "MPI_Send");
            break;
        case 1:
            op = op_coll;
            sprintf(name, "MPI_Bcast");
            break;
        case 2:
            op = op_a2a;
            sprintf(name, "MPI_Alltoall");
            break;
        case 3:
            op = op_send_pingpong;
            sprintf(name, "MPI_Send_pp");
            break;
        case 4:
            op = op_put;
            sprintf(name, "MPI_Put");
            break;
        case 5:
            op = op_get;
            sprintf(name, "MPI_Get");
            break;
        }

        if( 0 == rank_world )
            printf("# %s%%%d\n# size  \t|  latency \t| 10^6 B/s \t| MB/s   \t| median  \t| q1     \t| q3     \t| d1     \t| d9     \t| avg    \t| max\n", name, size_world);

        for(size = 0; size < MAX_SIZE; size = ((int)(size * 1.4) > size) ? (size * 1.4) : (size + 1)) {
            /* Init buffers */
            if( 0 != size ) {
                sbuf = (char *)realloc(sbuf, (size_world + 1) * size); /* sbuf + alltoall recv buf */
            }        

            do_bench(size, sbuf, results, op);

            MPI_Gather(results, NB_ITER, MPI_DOUBLE, full_res, NB_ITER, MPI_DOUBLE, 0, MPI_COMM_WORLD);

            if( 0 == rank_world ) {
                qsort(full_res, FULL_NB_ITER, sizeof(double), &comp_double);
                const double min_lat = full_res[0];
                const double max_lat = full_res[FULL_NB_ITER - 1];
                const double med_lat = full_res[(FULL_NB_ITER - 1) / 2];
                const double q1_lat  = full_res[(FULL_NB_ITER - 1) / 4];
                const double q3_lat  = full_res[ 3 * (FULL_NB_ITER - 1) / 4];
                const double d1_lat  = full_res[(FULL_NB_ITER - 1) / 10];
                const double d9_lat  = full_res[ 9 * (FULL_NB_ITER - 1) / 10];
                double avg_lat = 0.0;
                for( iter = 0; iter < FULL_NB_ITER; iter++ ){
                    avg_lat += full_res[iter];
                }
                avg_lat /= FULL_NB_ITER;
                const double bw_million_byte = size / min_lat;
                const double bw_mbyte        = bw_million_byte / 1.048576;

                printf("%9lld\t%9.3lf\t%9.3f\t%9.3f\t%9.3lf\t%9.3lf\t%9.3lf\t%9.3lf\t%9.3lf\t%9.3lf\t%9.3lf",
                       (long long)size, min_lat, bw_million_byte, bw_mbyte,
                       med_lat, q1_lat, q3_lat, d1_lat, d9_lat,
                       avg_lat, max_lat);
                printf("\n");
            }
        }
        free(sbuf);
        sbuf = NULL;
    }

    MPI_Finalize();
    return EXIT_SUCCESS;
}
