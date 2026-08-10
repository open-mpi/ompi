/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * MINLOC/MAXLOC accumulate with a derived origin datatype.
 *
 * The osc/ucx accumulate path stages the target data in a temporary buffer
 * and reduces the origin into it.  When the origin datatype is derived, that
 * reduction used to walk the origin through an iovec list, which only works
 * if every element happens to start on an iovec segment boundary.  The three
 * pair datatype shapes below all have to work:
 *
 *   - trailing padding, no internal hole (MPI_DOUBLE_INT, MPI_LONG_INT,
 *     MPI_LONG_DOUBLE_INT): one segment per element, with gaps in between
 *   - internal hole, no trailing padding (MPI_SHORT_INT): consecutive
 *     elements are adjacent in memory, so segments merge across element
 *     boundaries
 *   - neither (MPI_FLOAT_INT, MPI_2INT): one single contiguous segment
 *
 * Each shape is exercised with a contiguous derived origin and with a strided
 * (MPI_Type_vector) origin, for several counts, and through the blocking,
 * request based and fetching entry points.  Every byte of the origin that the
 * datatype does not select is poisoned, and the poison value wins MAXLOC, so
 * any stray read shows up in the result.
 *
 * Run with at least two ranks.  With no arguments every case runs; the
 * optional arguments "type count stride" restrict the run to one case, which
 * is useful when a failing case takes the whole job down with it.
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define POISON_BYTE 0xAA
#define POISON_VAL  9000 /* beats every legal contribution under MAXLOC */
#define POISON_LOC  424242
#define WIN_INIT_VAL (-9000)
#define WIN_INIT_LOC (-1)
#define UNTOUCHED_VAL (-7000)
#define UNTOUCHED_LOC (-77)

#define MAX_COUNT 8
#define STRIDE 2

/* The C structs matching the MPI pair datatypes. */
typedef struct { short v; int loc; } short_int_t;
typedef struct { int v; int loc; } two_int_t;
typedef struct { long v; int loc; } long_int_t;
typedef struct { float v; int loc; } float_int_t;
typedef struct { double v; int loc; } double_int_t;
typedef struct { long double v; int loc; } long_double_int_t;

#define DEFINE_PAIR_ACCESSORS(tag, type)                      \
    static void tag##_set(void *p, long v, int loc) {         \
        type *e = (type *) p;                                 \
        e->v = v;                                             \
        e->loc = loc;                                         \
    }                                                         \
    static void tag##_get(const void *p, long *v, int *loc) { \
        const type *e = (const type *) p;                     \
        *v = (long) e->v;                                     \
        *loc = e->loc;                                        \
    }

DEFINE_PAIR_ACCESSORS(short_int, short_int_t)
DEFINE_PAIR_ACCESSORS(two_int, two_int_t)
DEFINE_PAIR_ACCESSORS(long_int, long_int_t)
DEFINE_PAIR_ACCESSORS(float_int, float_int_t)
DEFINE_PAIR_ACCESSORS(double_int, double_int_t)
DEFINE_PAIR_ACCESSORS(long_double_int, long_double_int_t)

typedef struct {
    const char *name;
    MPI_Datatype dt;
    void (*set)(void *p, long v, int loc);
    void (*get)(const void *p, long *v, int *loc);
} pair_type_t;

static pair_type_t types[6];

static void init_types(void)
{
    pair_type_t t[] = {
        {"MPI_SHORT_INT", MPI_SHORT_INT, short_int_set, short_int_get},
        {"MPI_2INT", MPI_2INT, two_int_set, two_int_get},
        {"MPI_LONG_INT", MPI_LONG_INT, long_int_set, long_int_get},
        {"MPI_FLOAT_INT", MPI_FLOAT_INT, float_int_set, float_int_get},
        {"MPI_DOUBLE_INT", MPI_DOUBLE_INT, double_int_set, double_int_get},
        {"MPI_LONG_DOUBLE_INT", MPI_LONG_DOUBLE_INT, long_double_int_set,
         long_double_int_get},
    };

    memcpy(types, t, sizeof(t));
}

/*
 * The value rank r contributes for element k.  Distinct per rank and per
 * element, and small enough to be exact in a short and in a float.
 */
static long contrib_val(int k, int rank)
{
    return 1000 + 10 * k + rank;
}

static int check_elem(const pair_type_t *type, const char *what, int count,
                      const char *mode, int k, const void *got, long exp_v,
                      int exp_loc)
{
    long got_v;
    int got_loc;

    type->get(got, &got_v, &got_loc);
    if (got_v == exp_v && got_loc == exp_loc) {
        return 0;
    }

    fprintf(stderr,
            "FAIL %s %s count=%d origin=%s element %d: got (v=%ld,loc=%d), "
            "expected (v=%ld,loc=%d)%s\n",
            type->name, what, count, mode, k, got_v, got_loc, exp_v, exp_loc,
            got_v == POISON_VAL ? " [read a poisoned datatype gap]" : "");
    return 1;
}

static void win_init(const pair_type_t *type, char *winbuf, int count,
                     MPI_Aint extent)
{
    int k;

    for (k = 0; k < count; k++) {
        type->set(winbuf + (size_t) k * extent, WIN_INIT_VAL, WIN_INIT_LOC);
    }
}

/*
 * One (type, count, origin shape) case.  "sf" is the origin's element stride
 * factor: 1 builds a contiguous derived type, anything larger builds a
 * strided vector with sf - 1 poisoned elements between the real ones.
 */
static int run_case(const pair_type_t *type, int count, int sf, MPI_Win win,
                    char *winbuf, int rank, int size)
{
    const char *mode = (sf == 1) ? "contig" : "vector";
    MPI_Aint lb, extent;
    MPI_Datatype origin_dt;
    MPI_Request req;
    char *origin, *result;
    int errs = 0;
    int k;

    MPI_Type_get_extent(type->dt, &lb, &extent);

    /* Poison every byte, then write the real elements. */
    origin = malloc((size_t) count * sf * extent);
    result = malloc((size_t) count * extent);
    memset(origin, POISON_BYTE, (size_t) count * sf * extent);
    memset(result, POISON_BYTE, (size_t) count * extent);
    for (k = 0; k < count * sf; k++) {
        type->set(origin + (size_t) k * extent, POISON_VAL, POISON_LOC);
    }
    for (k = 0; k < count; k++) {
        type->set(origin + (size_t) k * sf * extent, contrib_val(k, rank), rank);
    }
    for (k = 0; k < count; k++) {
        type->set(result + (size_t) k * extent, UNTOUCHED_VAL, UNTOUCHED_LOC);
    }

    MPI_Type_vector(count, 1, sf, type->dt, &origin_dt);
    MPI_Type_commit(&origin_dt);

    /* --- MPI_Accumulate: every rank contributes, rank size-1 wins. --- */
    if (rank == 0) {
        win_init(type, winbuf, count, extent);
    }
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Win_lock(MPI_LOCK_SHARED, 0, 0, win);
    MPI_Accumulate(origin, 1, origin_dt, 0, 0, count, type->dt, MPI_MAXLOC, win);
    MPI_Win_unlock(0, win);
    MPI_Barrier(MPI_COMM_WORLD);
    if (rank == 0) {
        for (k = 0; k < count; k++) {
            errs += check_elem(type, "MPI_Accumulate", count, mode, k,
                               winbuf + (size_t) k * extent,
                               contrib_val(k, size - 1), size - 1);
        }
        win_init(type, winbuf, count, extent);
    }

    /* --- MPI_Raccumulate: same, through the request based path. --- */
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Win_lock(MPI_LOCK_SHARED, 0, 0, win);
    MPI_Raccumulate(origin, 1, origin_dt, 0, 0, count, type->dt, MPI_MAXLOC, win,
                    &req);
    MPI_Wait(&req, MPI_STATUS_IGNORE);
    MPI_Win_unlock(0, win);
    MPI_Barrier(MPI_COMM_WORLD);
    if (rank == 0) {
        for (k = 0; k < count; k++) {
            errs += check_elem(type, "MPI_Raccumulate", count, mode, k,
                               winbuf + (size_t) k * extent,
                               contrib_val(k, size - 1), size - 1);
        }
        win_init(type, winbuf, count, extent);
    }

    /*
     * --- MPI_Get_accumulate: only the last rank contributes, so both the
     * window and the fetched data are deterministic. ---
     */
    MPI_Barrier(MPI_COMM_WORLD);
    if (rank == size - 1) {
        MPI_Win_lock(MPI_LOCK_SHARED, 0, 0, win);
        MPI_Get_accumulate(origin, 1, origin_dt, result, count, type->dt, 0, 0,
                           count, type->dt, MPI_MAXLOC, win);
        MPI_Win_unlock(0, win);
        for (k = 0; k < count; k++) {
            errs += check_elem(type, "MPI_Get_accumulate result", count, mode, k,
                               result + (size_t) k * extent, WIN_INIT_VAL,
                               WIN_INIT_LOC);
        }
    }
    MPI_Barrier(MPI_COMM_WORLD);
    if (rank == 0) {
        for (k = 0; k < count; k++) {
            errs += check_elem(type, "MPI_Get_accumulate target", count, mode, k,
                               winbuf + (size_t) k * extent,
                               contrib_val(k, size - 1), size - 1);
        }
    }

    MPI_Type_free(&origin_dt);
    free(origin);
    free(result);
    MPI_Barrier(MPI_COMM_WORLD);

    return errs;
}

int main(int argc, char **argv)
{
    static const int counts[] = {1, 3, MAX_COUNT};
    const char *only_type = NULL;
    int only_count = 0, only_sf = 0;
    int rank, size, errs = 0, total = 0;
    MPI_Aint max_extent = 0, lb, extent;
    MPI_Win win;
    char *winbuf;
    size_t ti, ci;
    int sf;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (argc > 1) {
        only_type = argv[1];
    }
    if (argc > 2) {
        only_count = atoi(argv[2]);
    }
    if (argc > 3) {
        only_sf = atoi(argv[3]);
    }

    if (size < 2) {
        if (rank == 0) {
            fprintf(stderr, "this test needs at least 2 ranks\n");
        }
        MPI_Finalize();
        return 1;
    }

    init_types();
    for (ti = 0; ti < sizeof(types) / sizeof(types[0]); ti++) {
        MPI_Type_get_extent(types[ti].dt, &lb, &extent);
        if (extent > max_extent) {
            max_extent = extent;
        }
    }

    MPI_Win_allocate(rank == 0 ? MAX_COUNT * max_extent : 0, 1, MPI_INFO_NULL,
                     MPI_COMM_WORLD, &winbuf, &win);

    for (ti = 0; ti < sizeof(types) / sizeof(types[0]); ti++) {
        if (only_type != NULL && strcmp(only_type, types[ti].name) != 0) {
            continue;
        }
        for (ci = 0; ci < sizeof(counts) / sizeof(counts[0]); ci++) {
            if (only_count != 0 && only_count != counts[ci]) {
                continue;
            }
            for (sf = 1; sf <= STRIDE; sf++) {
                if (only_sf != 0 && only_sf != sf) {
                    continue;
                }
                errs += run_case(&types[ti], counts[ci], sf, win, winbuf, rank,
                                 size);
            }
        }
    }

    MPI_Allreduce(&errs, &total, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    if (rank == 0) {
        if (total == 0) {
            printf("PASS: derived origin MAXLOC accumulate\n");
        } else {
            printf("FAIL: derived origin MAXLOC accumulate, %d errors\n", total);
        }
    }

    MPI_Win_free(&win);
    MPI_Finalize();
    return total ? 1 : 0;
}
