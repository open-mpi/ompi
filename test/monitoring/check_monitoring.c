/*
 * Copyright (c) 2016-2017 Inria.  All rights reserved.
 * Copyright (c) 2017      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
  Check the well working of the monitoring component for Open-MPI.

  To be run as:

  mpirun -np 4 --mca pml_monitoring_enable 2 ./check_monitoring
*/

#include <mpi.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define PVAR_GENERATE_VARIABLES(pvar_prefix, pvar_name, pvar_class)     \
    /* Variables */                                                     \
    static MPI_T_pvar_handle pvar_prefix ## _handle;                    \
    static const char pvar_prefix ## _pvar_name[] = pvar_name;          \
    static int pvar_prefix ## _pvar_idx;                                \
    /* Functions */                                                     \
    static inline int pvar_prefix ## _start(MPI_T_pvar_session session) \
    {                                                                   \
        int MPIT_result;                                                \
        MPIT_result = MPI_T_pvar_start(session, pvar_prefix ## _handle); \
        if( MPI_SUCCESS != MPIT_result ) {                              \
            fprintf(stderr, "Failed to start handle on \"%s\" pvar, check that you have " \
                    "enabled the monitoring component.\n", pvar_prefix ## _pvar_name); \
            MPI_Abort(MPI_COMM_WORLD, MPIT_result);                     \
        }                                                               \
        return MPIT_result;                                             \
    }                                                                   \
    static inline int pvar_prefix ## _init(MPI_T_pvar_session session)  \
    {                                                                   \
        int MPIT_result;                                                \
        /* Get index */                                                 \
        MPIT_result = MPI_T_pvar_get_index(pvar_prefix ## _pvar_name,   \
                                           pvar_class,                  \
                                           &(pvar_prefix ## _pvar_idx)); \
        if( MPI_SUCCESS != MPIT_result ) {                              \
            fprintf(stderr, "Cannot find monitoring MPI_Tool \"%s\" pvar, check that you have " \
                    "enabled the monitoring component.\n", pvar_prefix ## _pvar_name); \
            MPI_Abort(MPI_COMM_WORLD, MPIT_result);                     \
            return MPIT_result;                                         \
        }                                                               \
        /* Allocate handle */                                           \
        /* Allocating a new PVAR in a session will reset the counters */ \
        int count;                                                      \
        MPIT_result = MPI_T_pvar_handle_alloc(session, pvar_prefix ## _pvar_idx, \
                                              MPI_COMM_WORLD, &(pvar_prefix ## _handle), \
                                              &count);                  \
        if( MPI_SUCCESS != MPIT_result ) {                              \
            fprintf(stderr, "Failed to allocate handle on \"%s\" pvar, check that you have " \
                    "enabled the monitoring component.\n", pvar_prefix ## _pvar_name); \
            MPI_Abort(MPI_COMM_WORLD, MPIT_result);                     \
            return MPIT_result;                                         \
        }                                                               \
        /* Start PVAR */                                                \
        return pvar_prefix ## _start(session);                          \
    }                                                                   \
    static inline int pvar_prefix ## _stop(MPI_T_pvar_session session)  \
    {                                                                   \
        int MPIT_result;                                                \
        MPIT_result = MPI_T_pvar_stop(session, pvar_prefix ## _handle); \
        if( MPI_SUCCESS != MPIT_result ) {                              \
            fprintf(stderr, "Failed to stop handle on \"%s\" pvar, check that you have " \
                    "enabled the monitoring component.\n", pvar_prefix ## _pvar_name); \
            MPI_Abort(MPI_COMM_WORLD, MPIT_result);                     \
        }                                                               \
        return MPIT_result;                                             \
    }                                                                   \
    static inline int pvar_prefix ## _finalize(MPI_T_pvar_session session) \
    {                                                                   \
        int MPIT_result;                                                \
        /* Stop PVAR */                                                 \
        MPIT_result = pvar_prefix ## _stop(session);                    \
        /* Free handle */						\
        MPIT_result = MPI_T_pvar_handle_free(session, &(pvar_prefix ## _handle)); \
        if( MPI_SUCCESS != MPIT_result ) {				\
            fprintf(stderr, "Failed to allocate handle on \"%s\" pvar, check that you have " \
                    "enabled the monitoring component.\n", pvar_prefix ## _pvar_name); \
            MPI_Abort(MPI_COMM_WORLD, MPIT_result);			\
            return MPIT_result;                                         \
        }								\
        return MPIT_result;						\
    }                                                                   \
    static inline int pvar_prefix ## _read(MPI_T_pvar_session session, void*values) \
    {                                                                   \
        int MPIT_result;                                                \
        /* Stop pvar */                                                 \
        MPIT_result = pvar_prefix ## _stop(session);                    \
        /* Read values */						\
        MPIT_result = MPI_T_pvar_read(session, pvar_prefix ## _handle, values); \
        if( MPI_SUCCESS != MPIT_result ) {				\
            fprintf(stderr, "Failed to read handle on \"%s\" pvar, check that you have " \
                    "enabled the monitoring component.\n", pvar_prefix ## _pvar_name); \
            MPI_Abort(MPI_COMM_WORLD, MPIT_result);			\
        }								\
        /* Start and return */                                          \
        return pvar_prefix ## _start(session);                          \
    }

#define GENERATE_CS(prefix, pvar_name_prefix, pvar_class_c, pvar_class_s) \
    PVAR_GENERATE_VARIABLES(prefix ## _count, pvar_name_prefix "_count", pvar_class_c) \
    PVAR_GENERATE_VARIABLES(prefix ## _size, pvar_name_prefix "_size", pvar_class_s) \
    static inline int pvar_ ## prefix ## _init(MPI_T_pvar_session session) \
    {                                                                   \
        prefix ## _count_init(session);                                 \
        return prefix ## _size_init(session);                           \
    }                                                                   \
    static inline int pvar_ ## prefix ## _finalize(MPI_T_pvar_session session) \
    {                                                                   \
        prefix ## _count_finalize(session);                             \
        return prefix ## _size_finalize(session);                       \
    }                                                                   \
    static inline void pvar_ ## prefix ## _read(MPI_T_pvar_session session, \
                                                size_t*cvalues, size_t*svalues) \
    {                                                                   \
        /* Read count values */                                         \
        prefix ## _count_read(session, cvalues);                        \
        /* Read size values */                                          \
        prefix ## _size_read(session, svalues);                         \
    }

GENERATE_CS(pml, "pml_monitoring_messages", MPI_T_PVAR_CLASS_SIZE, MPI_T_PVAR_CLASS_SIZE)
GENERATE_CS(osc_s, "osc_monitoring_messages_sent", MPI_T_PVAR_CLASS_SIZE, MPI_T_PVAR_CLASS_SIZE)
GENERATE_CS(osc_r, "osc_monitoring_messages_recv", MPI_T_PVAR_CLASS_SIZE, MPI_T_PVAR_CLASS_SIZE)
GENERATE_CS(coll, "coll_monitoring_messages", MPI_T_PVAR_CLASS_SIZE, MPI_T_PVAR_CLASS_SIZE)
GENERATE_CS(o2a, "coll_monitoring_o2a", MPI_T_PVAR_CLASS_COUNTER, MPI_T_PVAR_CLASS_AGGREGATE)
GENERATE_CS(a2o, "coll_monitoring_a2o", MPI_T_PVAR_CLASS_COUNTER, MPI_T_PVAR_CLASS_AGGREGATE)
GENERATE_CS(a2a, "coll_monitoring_a2a", MPI_T_PVAR_CLASS_COUNTER, MPI_T_PVAR_CLASS_AGGREGATE)

static size_t *old_cvalues, *old_svalues;

static inline void pvar_all_init(MPI_T_pvar_session*session, int world_size)
{
    int MPIT_result, provided;
    MPIT_result = MPI_T_init_thread(MPI_THREAD_SINGLE, &provided);
    if (MPIT_result != MPI_SUCCESS) {
        fprintf(stderr, "Failed to initialiaze MPI_Tools sub-system.\n");
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
    MPIT_result = MPI_T_pvar_session_create(session);
    if (MPIT_result != MPI_SUCCESS) {
        printf("Failed to create a session for PVARs.\n");
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
    old_cvalues = malloc(2 * world_size * sizeof(size_t));
    old_svalues = old_cvalues + world_size;
    pvar_pml_init(*session);
    pvar_osc_s_init(*session);
    pvar_osc_r_init(*session);
    pvar_coll_init(*session);
    pvar_o2a_init(*session);
    pvar_a2o_init(*session);
    pvar_a2a_init(*session);
}

static inline void pvar_all_finalize(MPI_T_pvar_session*session)
{
    int MPIT_result;
    pvar_pml_finalize(*session);
    pvar_osc_s_finalize(*session);
    pvar_osc_r_finalize(*session);
    pvar_coll_finalize(*session);
    pvar_o2a_finalize(*session);
    pvar_a2o_finalize(*session);
    pvar_a2a_finalize(*session);
    free(old_cvalues);
    MPIT_result = MPI_T_pvar_session_free(session);
    if (MPIT_result != MPI_SUCCESS) {
        printf("Failed to close a session for PVARs.\n");
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
    (void)MPI_T_finalize();
}

static inline int pvar_pml_check(MPI_T_pvar_session session, int world_size, int world_rank)
{
    int i, ret = MPI_SUCCESS;
    size_t *cvalues, *svalues;
    cvalues = malloc(2 * world_size * sizeof(size_t));
    svalues = cvalues + world_size;
    /* Get values */
    pvar_pml_read(session, cvalues, svalues);
    for( i = 0; i < world_size && MPI_SUCCESS == ret; ++i ) {
        /* Check count values */
        if( i == world_rank && (cvalues[i] - old_cvalues[i]) != (size_t) 0 ) {
            fprintf(stderr, "Error in %s: count_values[%d]=%zu, and should be equal to %zu.\n",
                    __func__, i, cvalues[i] - old_cvalues[i], (size_t) 0);
            ret = -1;
        } else if ( i != world_rank && (cvalues[i] - old_cvalues[i]) < (size_t) world_size ) {
            fprintf(stderr, "Error in %s: count_values[%d]=%zu, and should be >= %zu.\n",
                    __func__, i, cvalues[i] - old_cvalues[i], (size_t) world_size);
            ret = -1;
        }
        /* Check size values */
        if( i == world_rank && (svalues[i] - old_svalues[i]) != (size_t) 0 ) {
            fprintf(stderr, "Error in %s: size_values[%d]=%zu, and should be equal to %zu.\n",
                    __func__, i, svalues[i] - old_svalues[i], (size_t) 0);
            ret = -1;
        } else if ( i != world_rank && (svalues[i] - old_svalues[i]) < (size_t) (world_size * 13 * sizeof(char)) ) {
            fprintf(stderr, "Error in %s: size_values[%d]=%zu, and should be >= %zu.\n",
                    __func__, i, svalues[i] - old_svalues[i], (size_t) (world_size * 13 * sizeof(char)));
            ret = -1;
        }
    }
    if( MPI_SUCCESS == ret ) {
        fprintf(stdout, "Check PML...[ OK ]\n");
    } else {
        fprintf(stdout, "Check PML...[FAIL]\n");
    }
    /* Keep old PML values */
    memcpy(old_cvalues, cvalues, 2 * world_size * sizeof(size_t));
    /* Free arrays */
    free(cvalues);
    return ret;
}

static inline int pvar_osc_check(MPI_T_pvar_session session, int world_size, int world_rank)
{
    int i, ret = MPI_SUCCESS;
    size_t *cvalues, *svalues;
    cvalues = malloc(2 * world_size * sizeof(size_t));
    svalues = cvalues + world_size;
    /* Get OSC values */
    memset(cvalues, 0, 2 * world_size * sizeof(size_t));
    /* Check OSC sent values */
    pvar_osc_s_read(session, cvalues, svalues);
    for( i = 0; i < world_size && MPI_SUCCESS == ret; ++i ) {
        /* Check count values */
        if( cvalues[i] < (size_t) world_size ) {
            fprintf(stderr, "Error in %s: count_values[%d]=%zu, and should be >= %zu.\n",
                    __func__, i, cvalues[i], (size_t) world_size);
            ret = -1;
        }
        /* Check size values */
        if( svalues[i] < (size_t) (world_size * 13 * sizeof(char)) ) {
            fprintf(stderr, "Error in %s: size_values[%d]=%zu, and should be >= %zu.\n",
                    __func__, i, svalues[i], (size_t) (world_size * 13 * sizeof(char)));
            ret = -1;
        }
    }
    /* Check OSC received values */
    pvar_osc_r_read(session, cvalues, svalues);
    for( i = 0; i < world_size && MPI_SUCCESS == ret; ++i ) {
        /* Check count values */
        if( cvalues[i] < (size_t) world_size ) {
            fprintf(stderr, "Error in %s: count_values[%d]=%zu, and should be >= %zu.\n",
                    __func__, i, cvalues[i], (size_t) world_size);
            ret = -1;
        }
        /* Check size values */
        if( svalues[i] < (size_t) (world_size * 13 * sizeof(char)) ) {
            fprintf(stderr, "Error in %s: size_values[%d]=%zu, and should be >= %zu.\n",
                    __func__, i, svalues[i], (size_t) (world_size * 13 * sizeof(char)));
            ret = -1;
        }
    }
    if( MPI_SUCCESS == ret ) {
        fprintf(stdout, "Check OSC...[ OK ]\n");
    } else {
        fprintf(stdout, "Check OSC...[FAIL]\n");
    }
    /* Keep old PML values */
    memcpy(old_cvalues, cvalues, 2 * world_size * sizeof(size_t));
    /* Free arrays */
    free(cvalues);
    return ret;
}

static inline int pvar_coll_check(MPI_T_pvar_session session, int world_size, int world_rank) {
    int i, ret = MPI_SUCCESS;
    size_t count, size;
    size_t *cvalues, *svalues;
    cvalues = malloc(2 * world_size * sizeof(size_t));
    svalues = cvalues + world_size;
    /* Get COLL values */
    pvar_coll_read(session, cvalues, svalues);
    for( i = 0; i < world_size && MPI_SUCCESS == ret; ++i ) {
        /* Check count values */
        if( i == world_rank && cvalues[i] != (size_t) 0 ) {
            fprintf(stderr, "Error in %s: count_values[%d]=%zu, and should be equal to %zu.\n",
                    __func__, i, cvalues[i], (size_t) 0);
            ret = -1;
        } else if ( i != world_rank && cvalues[i] < (size_t) (world_size + 1) * 4 ) {
            fprintf(stderr, "Error in %s: count_values[%d]=%zu, and should be >= %zu.\n",
                    __func__, i, cvalues[i], (size_t) (world_size + 1) * 4);
            ret = -1;
        }
        /* Check size values */
        if( i == world_rank && svalues[i] != (size_t) 0 ) {
            fprintf(stderr, "Error in %s: size_values[%d]=%zu, and should be equal to %zu.\n",
                    __func__, i, svalues[i], (size_t) 0);
            ret = -1;
        } else if ( i != world_rank && svalues[i] < (size_t) (world_size * (2 * 13 * sizeof(char) + sizeof(int)) + 13 * 3 * sizeof(char) + sizeof(int)) ) {
            fprintf(stderr, "Error in %s: size_values[%d]=%zu, and should be >= %zu.\n",
                    __func__, i, svalues[i], (size_t) (world_size * (2 * 13 * sizeof(char) + sizeof(int)) + 13 * 3 * sizeof(char) + sizeof(int)));
            ret = -1;
        }
    }
    /* Check One-to-all COLL values */
    pvar_o2a_read(session, &count, &size);
    if( count < (size_t) 2 ) {
        fprintf(stderr, "Error in %s: count_o2a=%zu, and should be >= %zu.\n",
                __func__, count, (size_t) 2);
        ret = -1;
    }
    if( size < (size_t) ((world_size - 1) * 13 * 2 * sizeof(char)) ) {
        fprintf(stderr, "Error in %s: size_o2a=%zu, and should be >= %zu.\n",
                __func__, size, (size_t) ((world_size - 1) * 13 * 2 * sizeof(char)));
        ret = -1;
    }
    /* Check All-to-one COLL values */
    pvar_a2o_read(session, &count, &size);
    if( count < (size_t) 2 ) {
        fprintf(stderr, "Error in %s: count_a2o=%zu, and should be >= %zu.\n",
                __func__, count, (size_t) 2);
        ret = -1;
    }
    if( size < (size_t) ((world_size - 1) * (13 * sizeof(char) + sizeof(int))) ) {
        fprintf(stderr, "Error in %s: size_a2o=%zu, and should be >= %zu.\n",
                __func__, size,
                (size_t) ((world_size - 1) * (13 * sizeof(char) + sizeof(int))));
        ret = -1;
    }
    /* Check All-to-all COLL values */
    pvar_a2a_read(session, &count, &size);
    if( count < (size_t) (world_size * 4) ) {
        fprintf(stderr, "Error in %s: count_a2a=%zu, and should be >= %zu.\n",
                __func__, count, (size_t) (world_size * 4));
        ret = -1;
    }
    if( size < (size_t) (world_size * (world_size - 1) * (2 * 13 * sizeof(char) + sizeof(int))) ) {
        fprintf(stderr, "Error in %s: size_a2a=%zu, and should be >= %zu.\n",
                __func__, size,
                (size_t) (world_size * (world_size - 1) * (2 * 13 * sizeof(char) + sizeof(int))));
        ret = -1;
    }
    if( MPI_SUCCESS == ret ) {
        fprintf(stdout, "Check COLL...[ OK ]\n");
    } else {
        fprintf(stdout, "Check COLL...[FAIL]\n");
    }
    /* Keep old PML values */
    pvar_pml_read(session, old_cvalues, old_svalues);
    /* Free arrays */
    free(cvalues);
    return ret;
}

int main(int argc, char* argv[])
{
    int size, i, n, to, from, world_rank;
    MPI_T_pvar_session session;
    MPI_Status status;
    char s1[20], s2[20];
    strncpy(s1, "hello world!", 13);

    MPI_Init(NULL, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    
    pvar_all_init(&session, size);

    /* first phase: exchange size times data with everyone in
       MPI_COMM_WORLD with collective operations.  This phase comes
       first in order to ease the prediction of messages exchanged of
       each kind.
    */
    char*coll_buff = malloc(2 * size * 13 * sizeof(char));
    char*coll_recv_buff = coll_buff + size * 13;
    int sum_ranks;
    for( n = 0; n < size; ++n ) {
        /* Allgather */
        memset(coll_buff, 0, size * 13 * sizeof(char));
        MPI_Allgather(s1, 13, MPI_CHAR, coll_buff, 13, MPI_CHAR, MPI_COMM_WORLD);
        for( i = 0; i < size; ++i ) {
            if( strncmp(s1, &coll_buff[i * 13], 13) ) {
                fprintf(stderr, "Error in Allgather check: received \"%s\" instead of "
                        "\"hello world!\" from %d.\n", &coll_buff[i * 13], i);
                MPI_Abort(MPI_COMM_WORLD, -1);
            }
        }
        /* Scatter */
        MPI_Scatter(coll_buff, 13, MPI_CHAR, s2, 13, MPI_CHAR, n, MPI_COMM_WORLD);
        if( strncmp(s1, s2, 13) ) {
            fprintf(stderr, "Error in Scatter check: received \"%s\" instead of "
                    "\"hello world!\" from %d.\n", s2, n);
            MPI_Abort(MPI_COMM_WORLD, -1);
        }
        /* Allreduce */
        MPI_Allreduce(&world_rank, &sum_ranks, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
        if( sum_ranks != ((size - 1) * size / 2) ) {
            fprintf(stderr, "Error in Allreduce check: sum_ranks=%d instead of %d.\n",
                    sum_ranks, (size - 1) * size / 2);
            MPI_Abort(MPI_COMM_WORLD, -1);
        }
        /* Alltoall */
        memset(coll_recv_buff, 0, size * 13 * sizeof(char));
        MPI_Alltoall(coll_buff, 13, MPI_CHAR, coll_recv_buff, 13, MPI_CHAR, MPI_COMM_WORLD);
        for( i = 0; i < size; ++i ) {
            if( strncmp(s1, &coll_recv_buff[i * 13], 13) ) {
                fprintf(stderr, "Error in Alltoall check: received \"%s\" instead of "
                        "\"hello world!\" from %d.\n", &coll_recv_buff[i * 13], i);
                MPI_Abort(MPI_COMM_WORLD, -1);
            }
        }
        /* Bcast */
        if( n == world_rank ) {
            MPI_Bcast(s1, 13, MPI_CHAR, n, MPI_COMM_WORLD);
        } else {
            MPI_Bcast(s2, 13, MPI_CHAR, n, MPI_COMM_WORLD);
            if( strncmp(s1, s2, 13) ) {
                fprintf(stderr, "Error in Bcast check: received \"%s\" instead of "
                        "\"hello world!\" from %d.\n", s2, n);
                MPI_Abort(MPI_COMM_WORLD, -1);
            }
        }
        /* Barrier */
        MPI_Barrier(MPI_COMM_WORLD);
        /* Gather */
        memset(coll_buff, 0, size * 13 * sizeof(char));
        MPI_Gather(s1, 13, MPI_CHAR, coll_buff, 13, MPI_CHAR, n, MPI_COMM_WORLD);
        if( n == world_rank ) {
            for( i = 0; i < size; ++i ) {
                if( strncmp(s1, &coll_buff[i * 13], 13) ) {
                    fprintf(stderr, "Error in Gather check: received \"%s\" instead of "
                            "\"hello world!\" from %d.\n", &coll_buff[i * 13], i);
                    MPI_Abort(MPI_COMM_WORLD, -1);
                }
            }
        }
        /* Reduce */
        MPI_Reduce(&world_rank, &sum_ranks, 1, MPI_INT, MPI_SUM, n, MPI_COMM_WORLD);
        if( n == world_rank ) {
            if( sum_ranks != ((size - 1) * size / 2) ) {
                fprintf(stderr, "Error in Reduce check: sum_ranks=%d instead of %d.\n",
                        sum_ranks, (size - 1) * size / 2);
                MPI_Abort(MPI_COMM_WORLD, -1);
            }
        }        
    }
    free(coll_buff);
    if( -1 == pvar_coll_check(session, size, world_rank) ) MPI_Abort(MPI_COMM_WORLD, -1);

    /* second phase: exchange size times data with everyone except self
       in MPI_COMM_WORLD with Send/Recv */    
    for( n = 0; n < size; ++n ) {
        for( i = 0; i < size - 1; ++i ) {
            to = (world_rank+1+i)%size;
            from = (world_rank+size-1-i)%size;
            if(world_rank < to){
                MPI_Send(s1, 13, MPI_CHAR, to, world_rank, MPI_COMM_WORLD);
                MPI_Recv(s2, 13, MPI_CHAR, from, from, MPI_COMM_WORLD, &status);
            } else {
                MPI_Recv(s2, 13, MPI_CHAR, from, from, MPI_COMM_WORLD, &status);
                MPI_Send(s1, 13, MPI_CHAR, to, world_rank, MPI_COMM_WORLD);
            }
            if( strncmp(s2, "hello world!", 13) ) {
                fprintf(stderr, "Error in PML check: s2=\"%s\" instead of \"hello world!\".\n",
                        s2);
                MPI_Abort(MPI_COMM_WORLD, -1);
            }
        }
    }    
    if( -1 == pvar_pml_check(session, size, world_rank) ) MPI_Abort(MPI_COMM_WORLD, -1);

    /* third phase: exchange size times data with everyone, including self, in
       MPI_COMM_WORLD with RMA opertations */
    char win_buff[20];
    MPI_Win win;
    MPI_Win_create(win_buff, 20, sizeof(char), MPI_INFO_NULL, MPI_COMM_WORLD, &win);
    for( n = 0; n < size; ++n ) {
        for( i = 0; i < size; ++i ) {
            MPI_Win_lock(MPI_LOCK_EXCLUSIVE, i, 0, win);
            MPI_Put(s1, 13, MPI_CHAR, i, 0, 13, MPI_CHAR, win);
            MPI_Win_unlock(i, win);
        }
        MPI_Win_lock(MPI_LOCK_EXCLUSIVE, world_rank, 0, win);
        if( strncmp(win_buff, "hello world!", 13) ) {
            fprintf(stderr, "Error in OSC check: win_buff=\"%s\" instead of \"hello world!\".\n",
                    win_buff);
            MPI_Abort(MPI_COMM_WORLD, -1);
        }
        MPI_Win_unlock(world_rank, win);
        for( i = 0; i < size; ++i ) {
            MPI_Win_lock(MPI_LOCK_EXCLUSIVE, i, 0, win);
            MPI_Get(s2, 13, MPI_CHAR, i, 0, 13, MPI_CHAR, win);
            MPI_Win_unlock(i, win);
            if( strncmp(s2, "hello world!", 13) ) {
                fprintf(stderr, "Error in OSC check: s2=\"%s\" instead of \"hello world!\".\n",
                        s2);
                MPI_Abort(MPI_COMM_WORLD, -1);
            }
        }
    }
    MPI_Win_free(&win);
    if( -1 == pvar_osc_check(session, size, world_rank) ) MPI_Abort(MPI_COMM_WORLD, -1);
    
    pvar_all_finalize(&session);

    MPI_Finalize();

    return EXIT_SUCCESS;
}
