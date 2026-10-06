/*
 * Copyright (c) 2026      Stony Brook University.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * MPI_Allreduce with a non-commutative operation must combine the
 * contributions in rank order: x_0 op x_1 op ... op x_{n-1}.  The
 * operation here is the product of 2x2 integer matrices, which is
 * associative but not commutative.  Run it once per allreduce algorithm
 * (e.g., with coll_tuned_allreduce_algorithm) to check each of them.
 */

#include <mpi.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint32_t a, b, c, d;
} mat_t;

/* inout = in * inout, as MPI requires for user operations */
static void matmul(void *in, void *inout, int *len, MPI_Datatype *dtype)
{
    mat_t *x = (mat_t *) in, *y = (mat_t *) inout;
    (void) dtype;
    for (int i = 0; i < *len; ++i) {
        mat_t r;
        r.a = x[i].a * y[i].a + x[i].b * y[i].c;
        r.b = x[i].a * y[i].b + x[i].b * y[i].d;
        r.c = x[i].c * y[i].a + x[i].d * y[i].c;
        r.d = x[i].c * y[i].b + x[i].d * y[i].d;
        y[i] = r;
    }
}

static mat_t contribution(int rank, int i)
{
    mat_t m = {1u + (uint32_t) rank, (uint32_t) (i % 7), (uint32_t) (rank % 3) + 1u,
               1u + (uint32_t) (i % 5)};
    return m;
}

int main(int argc, char *argv[])
{
    int rank, size, errors = 0, all_errors = 0;
    /* small and larger vectors, to cover algorithms that segment */
    const int counts[] = {1, 7, 1000, 100000};
    MPI_Datatype mat_type;
    MPI_Op op;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    MPI_Type_contiguous(4, MPI_UINT32_T, &mat_type);
    MPI_Type_commit(&mat_type);
    MPI_Op_create(matmul, 0, &op);

    for (size_t k = 0; k < sizeof(counts) / sizeof(counts[0]); ++k) {
        int count = counts[k];
        mat_t *in = malloc(count * sizeof(mat_t));
        mat_t *out = malloc(count * sizeof(mat_t));
        for (int i = 0; i < count; ++i) {
            in[i] = contribution(rank, i);
        }

        for (int inplace = 0; inplace < 2; ++inplace) {
            if (inplace) {
                memcpy(out, in, count * sizeof(mat_t));
                MPI_Allreduce(MPI_IN_PLACE, out, count, mat_type, op, MPI_COMM_WORLD);
            } else {
                MPI_Allreduce(in, out, count, mat_type, op, MPI_COMM_WORLD);
            }
            for (int i = 0; i < count; ++i) {
                /* expected = x_0 * x_1 * ... * x_{size-1} */
                mat_t expected = contribution(size - 1, i);
                int one = 1;
                for (int r = size - 2; r >= 0; --r) {
                    mat_t x = contribution(r, i);
                    matmul(&x, &expected, &one, NULL);
                }
                if (0 != memcmp(&expected, &out[i], sizeof(mat_t))) {
                    fprintf(stderr,
                            "ERROR: rank %d count %d%s element %d: got {%u %u %u %u}, "
                            "expected {%u %u %u %u}\n",
                            rank, count, inplace ? " (in place)" : "", i, out[i].a, out[i].b,
                            out[i].c, out[i].d, expected.a, expected.b, expected.c, expected.d);
                    errors++;
                    break;
                }
            }
        }
        free(in);
        free(out);
    }

    MPI_Allreduce(&errors, &all_errors, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    MPI_Op_free(&op);
    MPI_Type_free(&mat_type);
    MPI_Finalize();
    return (0 == all_errors) ? 0 : 1;
}
