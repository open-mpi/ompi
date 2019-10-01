/*
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * Copyright (c) 2016      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <getopt.h>
#include <limits.h>
#include <string.h>

#include "pmi.h"

#include <time.h>
#define GET_TS ({ \
    struct timespec ts;                     \
    double ret;                             \
    clock_gettime(CLOCK_MONOTONIC, &ts);    \
    ret = ts.tv_sec + 1E-9 * ts.tv_nsec;    \
    ret;                                    \
})


int key_size = 100, key_count = 10, rank_shift;
int direct_modex = 0, debug_on = 0;

static void usage(const char *argv0)
{
    printf("Usage:\n");
    printf("  %s [options]              start the benchmark\n", argv0);
    printf("\n");
    printf("  -s, --key-size=<size>     size of the key's submitted\n");
    printf("  -c, --key-count=<size>    number of keys submitted to local and remote parts\n");
    printf("  -d, --direct-modex        use direct modex if available\n");
    printf("  --debug                   force all processes to print out the timings themself\n");
}


void parse_options(int argc, char **argv)
{
    extern char *optarg;
    extern int optind;
    struct option long_options[] = {
        { "help",           0, NULL, 'h' },
        /* IB options */
        { "key-size",       1, NULL, 's' },
        { "key-count",      1, NULL, 'c' },
        { "direct-modex",   0, NULL, 'd' },
        { "debug",          0, NULL, '0' },
        { 0 }
    };

    while (1) {
        int c;
        c = getopt_long(argc, argv, "hs:c:d0", long_options, NULL);

        if (c == -1)
            break;
        switch (c) {
        case 's':
            key_size = atoi(optarg);
            /* Make sure that we transform it to int as
             * this is what will be the key value type
             */
            key_size = key_size / 4 + !!(key_size % 4);
            break;
        case 'c':
            key_count = atoi(optarg);
            break;
        case 'd':
            direct_modex = 1;
            break;
        case '0':
            debug_on = 1;
            break;
        case 'h':
        default:
            usage(argv[0]);
            exit(0);
        }
    }

    rank_shift = 10;
    while( rank_shift <= key_count ){
        rank_shift *= 10;
    }
}

void fill_remote_ranks(int *local_ranks, int local_cnt, int *remote_ranks, int size)
{
    int i, k;
    for(i = 0, k = 0; i < size && k < (size - local_cnt); i++ ){
        int j, flag = 1;
        for(j=0; j < local_cnt; j++){
            if( i == local_ranks[j] ){
                flag = 0;
                break;
            }
        }
        if( flag ){
            remote_ranks[k] = i;
            k++;
        }
    }
}

int store_double(char *name, double val)
{
    char buf[128];
    sprintf(buf,"%lf",val);

}

int get_mem_usage(double *_pss, double *_rss) {
    char data[PATH_MAX];
    FILE *smaps;
    double pss = 0.0, rss = 0.0;
    char *line = NULL;
    size_t size = 0;
    pid_t pid = getpid();

    *_pss = 0.0;
    *_rss = 0.0;

    memset(data, 0, sizeof(data));
    snprintf(data, sizeof(data), "/proc/%d/smaps", pid);

    if (NULL == (smaps = fopen(data, "r"))) {
        return -1;
    }

    while ((size = getline(&line, &size, smaps)) != -1) {
        if (0 == strncmp(line, "Pss", strlen("Pss"))) {
            sscanf(line, "Pss: %lf", &pss);
            *_pss += pss;
        }
        if (0 == strncmp(line, "Rss", strlen("Pss"))) {
            sscanf(line, "Rss: %lf", &rss);
            *_rss += pss;
        }
    }
    free(line);
    fclose(smaps);

    return 0;
}

int main(int argc, char **argv)
{
    int rc;
    char *key_name;
    int *key_val;
    int rank, nproc;
    int cnt;
    int *local_ranks, local_cnt;
    int *remote_ranks, remote_cnt;
    double start, total_start, get_loc_time = 0, get_rem_time = 0, put_loc_time = 0,
           put_rem_time = 0, commit_time = 0, fence_time = 0, init_time = 0, total_time = 0;
    int get_loc_cnt = 0, get_rem_cnt = 0, put_loc_cnt = 0, put_rem_cnt = 0;
    double mem_pss = 0.0, mem_rss = 0.0;
    char have_shmem;
    size_t shmem_job_info, shmem_all;

    parse_options(argc, argv);

    total_start = GET_TS;
    start = GET_TS;
    pmi_init(&rank, &nproc);
    init_time += GET_TS - start;

    pmi_get_local_ranks(&local_ranks, &local_cnt);
    remote_cnt = nproc - local_cnt;
    if( remote_cnt ){
        remote_ranks = calloc(remote_cnt, sizeof(int));
        fill_remote_ranks(local_ranks, local_cnt, remote_ranks, nproc);
    }

    pmi_get_shmem_size(&have_shmem, &shmem_job_info);

    /*
     * Make sure that no other rank started publishing keys in the dstore
     * before we finished with shmem size screening
     */
    pmi_fence( 0 );

    if( 0 == rank && debug_on ){
        int i;
        fprintf(stderr,"%d: local ranks: ", rank);
        for(i = 0; i < local_cnt; i++){
            fprintf(stderr,"%d ", local_ranks[i]);
        }
        fprintf(stderr,"\n");
        fflush(stderr);
    }

    key_val = calloc(key_size, sizeof(int));
    for (cnt=0; cnt < key_count; cnt++) {
        int i;
        if( local_cnt > 0 ){
            (void)asprintf(&key_name, "KEY-%d-local-%d", rank, cnt);
            for(i=0; i < key_size; i++){
                key_val[i] = rank * rank_shift + cnt;
            }
            put_loc_cnt++;
            start = GET_TS;
            pmi_put_key_loc(key_name, key_val, key_size);
            put_loc_time += GET_TS - start;
            free(key_name);
        }
        if( remote_cnt > 0 ){
            (void)asprintf(&key_name, "KEY-%d-remote-%d", rank, cnt);
            for(i=0; i < key_size; i++){
                key_val[i] = rank * rank_shift + cnt;
            }
            put_rem_cnt++;
            start = GET_TS;
            pmi_put_key_rem(key_name, key_val, key_size);
            put_rem_time += GET_TS - start;
            free(key_name);
        }
    }
    free(key_val);

    start = GET_TS;
    pmi_commit();
    commit_time += GET_TS - start;

    start = GET_TS;
    pmi_fence( !direct_modex );
    fence_time += GET_TS - start;



    for (cnt=0; cnt < key_count; cnt++) {
        int i;

        for(i = 0; i < remote_cnt; i++){
            int rank = remote_ranks[i], j;
            int *key_val, key_size_new;
            double start;
            (void)asprintf(&key_name, "KEY-%d-remote-%d", rank, cnt);

            start = GET_TS;
            pmi_get_key_rem(rank, key_name, &key_val, &key_size_new);
            get_rem_time += GET_TS - start;
            get_rem_cnt++;

            if( key_size != key_size_new ){
                fprintf(stderr,"%d: error in key %s sizes: %d vs %d\n",
                        rank, key_name, key_size, key_size_new);
                abort();
            }

            for(j=0; j < key_size; j++){
                if( key_val[j] != rank * rank_shift + cnt ){
                    fprintf(stderr, "%d: error in key %s value (byte %d)\n",
                            rank, key_name, j);
                    abort();
                }
            }
            free(key_name);
            free(key_val);
        }

         // check the returned data
        for(i = 0; i < local_cnt; i++){
            int rank = local_ranks[i], j;
            int *key_val, key_size_new;
            double start;
            (void)asprintf(&key_name, "KEY-%d-local-%d", rank, cnt);

            start = GET_TS;
            pmi_get_key_loc(rank, key_name, &key_val, &key_size_new);
            get_loc_time += GET_TS - start;
            get_loc_cnt++;

            if( key_size != key_size_new ){
                fprintf(stderr,"%d: error in key %s sizes: %d vs %d\n",
                        rank, key_name, key_size, key_size_new);
                abort();
            }

            for(j=0; j < key_size; j++){
                if( key_val[j] != rank * rank_shift + cnt ){
                    fprintf(stderr, "%d: error in key %s value (byte %d)",
                            rank, key_name, j);
                    abort();
                }
            }
            free(key_name);
            free(key_val);
        }
    }

    total_time = GET_TS - total_start;

    if (0 != get_mem_usage(&mem_pss, &mem_rss)) {
        fprintf(stderr, "Rank %d: error get memory usage", rank);
        abort();
    }

    if( debug_on ){
        fprintf(stderr,"%d: get: total %lf avg loc %lf rem %lf all %lf ; put: %lf %lf commit: %lf fence %lf\n",
                rank, (get_loc_time + get_rem_time),
                get_loc_time/get_loc_cnt, get_rem_time/get_rem_cnt,
                (get_loc_time + get_rem_time)/(get_loc_cnt + get_rem_cnt),
                put_loc_time/put_loc_cnt, put_rem_time/put_rem_cnt,
                commit_time, fence_time);
    }

    pmi_get_shmem_size(&have_shmem, &shmem_all);
    /*
     * The barrier ensures that all procs finished key fetching
     * we had issues with dstor/lockless case evaluation
     */
    pmi_fence( 0 );

    /* Out of the perf path - send our results to rank 0 using same PMI */
    char key[128];
    sprintf(key, "PMIX_PERF_get_total_time.%d", rank);
    pmi_put_double(key, get_rem_time + get_loc_time);

    sprintf(key, "PMIX_PERF_get_loc_time.%d", rank);
    pmi_put_double(key, get_loc_cnt ? get_loc_time/get_loc_cnt : 0 );

    sprintf(key, "PMIX_PERF_get_rem_time.%d", rank);
    pmi_put_double(key, get_rem_cnt ? get_rem_time/get_rem_cnt : 0);

    sprintf(key, "PMIX_PERF_get_time.%d", rank);
    pmi_put_double(key, (get_loc_time + get_rem_time)/(get_loc_cnt + get_rem_cnt) );

    sprintf(key, "PMIX_PERF_put_loc_time.%d", rank);
    pmi_put_double(key, put_loc_cnt ? put_loc_time / put_loc_cnt : 0);

    sprintf(key, "PMIX_PERF_put_rem_time.%d", rank);
    pmi_put_double(key, put_rem_cnt ? put_rem_time / put_rem_cnt : 0);

    sprintf(key, "PMIX_PERF_commit_time.%d", rank);
    pmi_put_double(key, commit_time);

    sprintf(key, "PMIX_PERF_fence_time.%d", rank);
    pmi_put_double(key, fence_time);

    sprintf(key, "PMIX_PERF_init_time.%d", rank);
    pmi_put_double(key, init_time);

    sprintf(key, "PMIX_PERF_total_time.%d", rank);
    pmi_put_double(key, total_time);

    sprintf(key, "PMIX_PERF_mem_pss.%d", rank);
    pmi_put_double(key, mem_pss);

    sprintf(key, "PMIX_PERF_mem_rss.%d", rank);
    pmi_put_double(key, mem_rss);

    pmi_commit();
    pmi_fence( 1 );

    if( rank == 0 ){
        double  cum_get_total_time = 0,
                cum_get_loc_time = 0,
                cum_get_rem_time = 0,
                cum_get_time = 0,
                cum_put_total_time = 0,
                cum_put_loc_time = 0,
                cum_put_rem_time = 0,
                cum_commit_time = 0,
                cum_fence_time = 0,
                cum_init_time = 0,
                cum_total_time = 0,
                cum_mem_pss = 0.0;

        double  min_get_loc_time = get_loc_time / get_loc_cnt,
                max_get_loc_time = get_loc_time / get_loc_cnt,
                min_get_rem_time = get_rem_time / get_rem_cnt,
                max_get_rem_time = get_rem_time / get_rem_cnt,
                min_init_time = init_time,
                max_init_time = init_time,
                min_total_time = total_time,
                max_total_time = total_time,
                min_mem_pss = mem_pss,
                max_mem_pss = 0.0;

        int min_get_loc_idx = 0, max_get_loc_idx = 0;
        int min_get_rem_idx = 0, max_get_rem_idx = 0;

        char c_get_ltime[128], c_get_rtime[128], c_get_ttime[128];
        char c_put_ltime[128], c_put_rtime[128];
        int i;
        for(i = 0; i < nproc; i++){
            double val;
            sprintf(key, "PMIX_PERF_get_total_time.%d", i);
            cum_get_total_time += pmi_get_double(i, key);

            sprintf(key, "PMIX_PERF_get_loc_time.%d", i);
            val = pmi_get_double(i, key);
            cum_get_loc_time += val;
            if( min_get_loc_time > val ){
                min_get_loc_time = val;
                min_get_loc_idx = i;
            }
            if( max_get_loc_time < val ){
                max_get_loc_time = val;
                max_get_loc_idx = i;
            }

            sprintf(key, "PMIX_PERF_get_rem_time.%d", i);
            val = pmi_get_double(i, key);
            cum_get_rem_time += val;
            if( min_get_rem_time > val ){
                min_get_rem_time = val;
                min_get_rem_idx = i;
            }
            if( max_get_rem_time < val ){
                max_get_rem_time = val;
                max_get_rem_idx = i;
            }

            sprintf(key, "PMIX_PERF_get_time.%d", i);
            cum_get_time += pmi_get_double(i, key);

            sprintf(key, "PMIX_PERF_put_loc_time.%d", i);
            cum_put_loc_time += pmi_get_double(i, key);

            sprintf(key, "PMIX_PERF_put_rem_time.%d", i);
            cum_put_rem_time += pmi_get_double(i, key);

            sprintf(key, "PMIX_PERF_commit_time.%d", i);
            cum_commit_time += pmi_get_double(i, key);

            sprintf(key, "PMIX_PERF_fence_time.%d", i);
            cum_fence_time += pmi_get_double(i, key);

            sprintf(key, "PMIX_PERF_init_time.%d", i);
            val = pmi_get_double(i, key);
            cum_init_time += val;
            if (min_init_time > val) {
                min_init_time = val;
            }
            if (max_init_time < val) {
                max_init_time = val;
            }

            sprintf(key, "PMIX_PERF_total_time.%d", i);
            val = pmi_get_double(i, key);
            cum_total_time += val;
            if (min_total_time > val) {
                min_total_time = val;
            }
            if (max_total_time < val) {
                max_total_time = val;
            }

            sprintf(key, "PMIX_PERF_mem_pss.%d", i);
            val = pmi_get_double(i, key);
            cum_mem_pss += val;
            if (min_mem_pss > val) {
                min_mem_pss = val;
            }
            if (max_mem_pss < val) {
                max_mem_pss = val;
            }
        }

        if( get_loc_cnt ){
            sprintf(c_get_ltime,"%lf", cum_get_loc_time / nproc);
        } else {
            sprintf(c_get_ltime,"--------");
        }
        if( get_rem_cnt ){
            sprintf(c_get_rtime,"%lf", cum_get_rem_time / nproc);
        } else {
            sprintf(c_get_rtime,"--------");
        }

        if( get_loc_cnt + get_rem_cnt ){
            sprintf(c_get_ttime,"%lf", cum_get_time / nproc);
        } else {
            sprintf(c_get_ttime,"--------");
        }

        if( put_loc_cnt ){
            sprintf(c_put_ltime,"%lf", cum_put_loc_time / nproc);
            cum_put_total_time += cum_put_loc_time;
        } else {
            sprintf(c_put_ltime,"--------");
        }
        if( put_rem_cnt ){
            sprintf(c_put_rtime,"%lf", cum_put_rem_time / nproc);
            cum_put_total_time += cum_put_rem_time;
        } else {
            sprintf(c_put_rtime,"--------");
        }

        fprintf(stderr,"init: %lf; put: %lf; commit: %lf; fence: %lf; get: %lf; total: %lf\n",
                cum_init_time / nproc,
                cum_put_total_time / nproc,
                cum_commit_time / nproc, cum_fence_time / nproc,
                cum_get_total_time / nproc,
                cum_total_time / nproc);
        fprintf(stderr,"init:          max %lf min %lf\n",  max_init_time, min_init_time);
        fprintf(stderr,"put:           loc %s rem %s\n", c_put_ltime, c_put_rtime);
        fprintf(stderr,"get:           loc %s rem %s all %s\n", c_get_ltime, c_get_rtime, c_get_ttime);
        fprintf(stderr,"get:           min loc %lf rem %lf (loc: %d, rem: %d)\n",
                min_get_loc_time, min_get_rem_time, min_get_loc_idx, min_get_rem_idx);
        fprintf(stderr,"get:           max loc %lf rem %lf (loc: %d, rem: %d)\n",
                max_get_loc_time, max_get_rem_time, max_get_loc_idx, max_get_rem_idx);
        fprintf(stderr,"total:         max %lf min %lf\n", max_total_time, min_total_time);
        fprintf(stderr,"mem:           loc %0.2lf avg %0.2lf min %0.2lf max %0.2lf total %0.2lf Kb\n",
                mem_pss, cum_mem_pss / nproc, min_mem_pss, max_mem_pss, cum_mem_pss);
        if( have_shmem ) {
            fprintf(stderr,"shmem:         job_info: %0.2lf total %0.2lf Kb\n",
                    (double)shmem_job_info / 1024, (double)shmem_all / 1024);
        }

        /* debug printout *//*
        for(i = 0; i < nproc; i++){
            double val;
            printf("%d: ", i);
            sprintf(key, "PMIX_PERF_get_loc_time.%d", i);
            printf("local = %lf ", pmi_get_double(i, key));

            sprintf(key, "PMIX_PERF_get_rem_time.%d", i);
            printf("remote = %lf\n", pmi_get_double(i, key));
        }
*/
    }

    pmi_fini();

    return 0;
}
