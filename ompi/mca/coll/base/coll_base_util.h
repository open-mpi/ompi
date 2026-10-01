/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2022 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2024      NVIDIA CORPORATION. All rights reserved.
 * Copyright (c) 2025      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#ifndef MCA_COLL_BASE_UTIL_EXPORT_H
#define MCA_COLL_BASE_UTIL_EXPORT_H

#include "ompi_config.h"

#include "opal/util/bit_ops.h"

#include "mpi.h"
#include "ompi/mca/mca.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/request/request.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/op/op.h"
#include "ompi/mca/pml/pml.h"

BEGIN_C_DECLS

/**
 * The largest array we need to track collective temporary memory. Right now
 * the record is for ialltoallw, for the array of send and receive types,
 * count and displacements.
 */
#define OMPI_REQ_NB_RELEASE_ARRAYS 7

/**
 * Request structure to be returned by non-blocking
 * collective operations.
 */
struct ompi_coll_base_nbc_request_t {
    ompi_request_t super;
    union {
        ompi_request_complete_fn_t req_complete_cb;
        ompi_request_free_fn_t req_free;
    } cb;
    void *req_complete_cb_data;
    struct {
        union {
            struct {
                ompi_op_t *op;
                ompi_datatype_t *datatype;
            } op;
            struct {
                ompi_datatype_t *stype;
                ompi_datatype_t *rtype;
            } types;
            struct {
                opal_object_t *objs[2];
            } objs;
            struct {
                ompi_datatype_t * const *stypes;
                ompi_datatype_t * const *rtypes;
                int scount;
                int rcount;
            } vecs;
        } refcounted;
        void* release_arrays[OMPI_REQ_NB_RELEASE_ARRAYS];
    } data;
};

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_coll_base_nbc_request_t);

static inline int32_t
ompi_coll_base_nbc_reserve_tags(ompi_communicator_t* comm, int32_t reserve)
{
    int32_t tag, old_tag;
    assert( reserve > 0 );
  reread_tag:  /* In case we fail to atomically update the tag */
    tag = old_tag = comm->c_nbc_tag;
    if ((tag - reserve) < MCA_COLL_BASE_TAG_NONBLOCKING_END) {
        tag = MCA_COLL_BASE_TAG_NONBLOCKING_BASE;
    }
    if( !OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_32(&comm->c_nbc_tag, &old_tag, tag - reserve) ) {
        goto reread_tag;
    }
    return tag;
}

/**
 * Append an array to a request object to be freed upon completion
 * of the associated operation.
 * The request object must be of type ompi_coll_base_nbc_request_t.
 */
__opal_attribute_always_inline__ static inline int
ompi_coll_base_append_array_to_release(struct ompi_request_t *req, void *array_ptr)
{
    int i, ret = OMPI_SUCCESS;
    struct ompi_coll_base_nbc_request_t *request = (struct ompi_coll_base_nbc_request_t *)req;

    /*
     * important sanity check - doing steps below on a non-libnbc request can lead
     * to difficult to debug memory corruption problems
     */
    assert(request->super.req_type == OMPI_REQUEST_COLL);

    for(i = 0; i < OMPI_REQ_NB_RELEASE_ARRAYS; i++ ) {
        if (NULL == request->data.release_arrays[i]) {
            break;
        }
    }

    if (OMPI_REQ_NB_RELEASE_ARRAYS > i) {
        request->data.release_arrays[i] = array_ptr;
        ++i;
        if (OMPI_REQ_NB_RELEASE_ARRAYS > i) {
            request->data.release_arrays[i] = NULL;
        }
    } else {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
    }

    return ret;
}

typedef struct ompi_coll_base_nbc_request_t ompi_coll_base_nbc_request_t;

/*
 * Structure to store an available module
 */
struct mca_coll_base_avail_coll_t {
    opal_list_item_t super;

    int ac_priority;
    mca_coll_base_module_t *ac_module;
    const char * ac_component_name;
};
typedef struct mca_coll_base_avail_coll_t mca_coll_base_avail_coll_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_coll_base_avail_coll_t);

/**
 * A MPI_like function doing a send and a receive simultaneously.
 * Posts a irecv, does a send, then gets irecv completion.
 */
int ompi_coll_base_sendrecv_actual( const void* sendbuf, size_t scount,
                                    ompi_datatype_t* sdatatype,
                                    int dest, int stag,
                                    void* recvbuf, size_t rcount,
                                    ompi_datatype_t* rdatatype,
                                    int source, int rtag,
                                    struct ompi_communicator_t* comm,
                                    ompi_status_public_t* status );


/**
 * A wrapper around ompi_coll_base_sendrecv_actual, with an optimized
 * path for self-directed send/recv.
 */
static inline int
ompi_coll_base_sendrecv( void* sendbuf, size_t scount, ompi_datatype_t* sdatatype,
                          int dest, int stag,
                          void* recvbuf, size_t rcount, ompi_datatype_t* rdatatype,
                          int source, int rtag,
                          struct ompi_communicator_t* comm,
                          ompi_status_public_t* status, int myid )
{
    if ((dest == source) && (source == myid)) {
        return (int) ompi_datatype_sndrcv(sendbuf, (int32_t) scount, sdatatype,
                                          recvbuf, (int32_t) rcount, rdatatype);
    }
    return ompi_coll_base_sendrecv_actual (sendbuf, scount, sdatatype,
                                           dest, stag,
                                           recvbuf, rcount, rdatatype,
                                           source, rtag, comm, status);
}

/**
 * Convert binary to negabinary (base -2) without loops.
 * Based on: https://stackoverflow.com/questions/37637781/
 *
 * The transform is a bijection for every int32 input (the round-trip
 * ompi_coll_negabinary_to_binary() is the identity), but the Bine
 * algorithms only use patterns whose decoded value fits in the number
 * of bits in use.  For bin > 0x55555555 (0x55555555 is the largest
 * usable value) the resulting pattern cannot be used, so that is an
 * error.
 */
static inline int ompi_coll_binary_to_negabinary(int32_t bin, uint32_t *out)
{
    if (bin > 0x55555555) {
        return MPI_ERR_ARG;
    }
    const uint32_t mask = 0xAAAAAAAA;
    *out = (mask + (uint32_t) bin) ^ mask;
    return MPI_SUCCESS;
}

/**
 * Convert negabinary back to binary.
 * Inverse of ompi_coll_binary_to_negabinary.
 */
static inline int32_t ompi_coll_negabinary_to_binary(uint32_t neg) {
    const uint32_t mask = 0xAAAAAAAA;
    return (mask ^ neg) - mask;
}

/**
 * Mathematical modulo (always non-negative).  C's % can return
 * negative values.  Returns MPI_ERR_ARG if b == 0.
 */
static inline int ompi_coll_mod(int a, int b, int *res)
{
    if (OPAL_UNLIKELY(0 == b)) {
        return MPI_ERR_ARG;
    }
    int r = a % b;
    *res = r < 0 ? r + b : r;
    return MPI_SUCCESS;
}

/**
 * Returns if the given value is a power of two.
 */
static inline int ompi_coll_is_power_of_two(int value)
{
    return value > 0 && (value & (value - 1)) == 0;
}

/* Maximum number of steps a BINE collective can ever require.  MPI
 * communicator sizes are bounded by INT_MAX and BINE runs on
 * power-of-two sizes, so ceil(log2(size)) <= 30.  Used to size
 * per-step arrays.
 */
#define BINE_MAX_STEPS 30

/* J(n) = (2^n - (-1)^n) / 3, the n-th Jacobsthal number (exact division).
 * J(n) exceeds INT_MAX when n >= 33, so the result always fits in an int
 * within the valid input range: 0 <= n < 33.  Returns MPI_ERR_ARG for any
 * n outside that range. 
 */
static inline int ompi_coll_bine_jacobsthal(int n, int *res)
{
    if (OPAL_UNLIKELY(n < 0 || n >= 33)) {
        return MPI_ERR_ARG;
    }
    uint64_t p = UINT64_C(1) << n;
    *res = (int) ((n & 1) ? (p + 1) / 3 : (p - 1) / 3);
    return MPI_SUCCESS;
}

/* largest signed value representable in nbits-bit negabinary.
 * e = nbits (even) / nbits + 1 (odd), and (2^e - 1) / 3 exceeds INT_MAX
 * when e >= 33, i.e. nbits >= 33.  Callers derive nbits from
 * opal_cube_dim(size), bounded by BINE_MAX_STEPS, so nbits <= 31 in
 * practice.  Valid input range: 0 <= nbits < 33.  Returns MPI_ERR_ARG for
 * any nbits outside that range. 
 */
static inline int ompi_coll_bine_largest_negabinary(int nbits, int *res)
{
    if (OPAL_UNLIKELY(nbits < 0 || nbits >= 33)) {
        return MPI_ERR_ARG;
    }
    uint32_t e = (nbits & 1) ? (uint32_t) nbits + 1 : (uint32_t) nbits;
    *res = (int) ((((uint64_t) 1 << e) - 1) / 3);
    return MPI_SUCCESS;
}

/* smallest signed value representable in nbits-bit negabinary.
 * -2 * largest(nbits - 1) exceeds INT_MIN when nbits >= 32 (nbits = 32
 * would multiply 1431655765 by -2).  Valid input range: 0 < nbits < 32.
 * Returns MPI_ERR_ARG for any nbits outside that range. 
 */
static inline int ompi_coll_bine_smallest_negabinary(int nbits, int *res)
{
    if (OPAL_UNLIKELY(nbits < 1 || nbits >= 32)) {
        return MPI_ERR_ARG;
    }
    int tmp;
    int err = ompi_coll_bine_largest_negabinary(nbits - 1, &tmp);
    if (OPAL_UNLIKELY(MPI_SUCCESS != err)) {
        return err;
    }
    *res = -2 * tmp;
    return MPI_SUCCESS;
}

/* check if x is within the representable range of signed nbits-bit negabinary
 * Valid input range: 0 < nbits < 32 (matches the smallest/largest bounds).
 * An out-of-range nbits is treated as "not in range" (returns 0). 
 */
static inline int ompi_coll_bine_in_range(int x, uint32_t nbits)
{
    int lowest, largest;
    int err = ompi_coll_bine_smallest_negabinary((int) nbits, &lowest);
    if (OPAL_UNLIKELY(MPI_SUCCESS != err)) {
        return 0;
    }
    err = ompi_coll_bine_largest_negabinary((int) nbits, &largest);
    if (OPAL_UNLIKELY(MPI_SUCCESS != err)) {
        return 0;
    }
    return x >= lowest && x <= largest;
}

/*
 *Try to produce a Bine-usable negabinary pattern for bin at the given
 * bit width.  Returns false when bin does not fit in nbits bits.
 */
static inline int ompi_coll_bine_try_negabinary(int32_t bin, uint32_t nbits, uint32_t *out)
{
    if (!ompi_coll_bine_in_range(bin, nbits)) {
        return 0;
    }
    return (MPI_SUCCESS == ompi_coll_binary_to_negabinary(bin, out));
}

/* get the negabinary representation of a rank, selecting between two possible encodings */
static inline uint32_t ompi_coll_bine_get_rank_negabinary_representation(uint32_t num_ranks,
                                                                         uint32_t rank)
{
    if (OPAL_UNLIKELY(num_ranks == 0 || num_ranks > INT_MAX || rank >= num_ranks)) {
        return UINT32_MAX;
    }

    if (num_ranks == 1) {
        return 0;
    }

    uint32_t nba = 0, nbb = 0;
    bool nba_ok = false, nbb_ok = false;
    int num_bits = opal_cube_dim(num_ranks);
    if (OPAL_UNLIKELY(num_bits < 0 || num_bits > 31)) {
        return UINT32_MAX;
    }

    if (rank % 2) {
        nba_ok = ompi_coll_bine_try_negabinary(rank, (uint32_t) num_bits, &nba);
        nbb_ok = ompi_coll_bine_try_negabinary(rank - num_ranks, (uint32_t) num_bits, &nbb);
    } else {
        nba_ok = ompi_coll_bine_try_negabinary(-rank, (uint32_t) num_bits, &nba);
        nbb_ok = ompi_coll_bine_try_negabinary(-rank + num_ranks, (uint32_t) num_bits, &nbb);
    }

    assert(nba_ok || nbb_ok);

    if (nba_ok && !nbb_ok) {
        return nba;
    } else if (!nba_ok && nbb_ok) {
        return nbb;
    } else { // Check MSB
        if (nba & (UINT32_C(0x80000000) >> (32 - num_bits))) {
            return nba;
        } else {
            return nbb;
        }
    }
}

/* reverse the order of all bits in a 32-bit value */
static inline uint32_t ompi_coll_bine_reverse(uint32_t x)
{
    x = ((x >> 1) & 0x55555555u) | ((x & 0x55555555u) << 1);
    x = ((x >> 2) & 0x33333333u) | ((x & 0x33333333u) << 2);
    x = ((x >> 4) & 0x0f0f0f0fu) | ((x & 0x0f0f0f0fu) << 4);
    x = ((x >> 8) & 0x00ff00ffu) | ((x & 0x00ff00ffu) << 8);
    x = ((x >> 16) & 0xffffu) | ((x & 0xffffu) << 16);
    return x;
}

/* convert negabinary value to nu (step) order via Gray-code-like transform */
static inline uint32_t ompi_coll_bine_nb_to_nu(uint32_t nb, uint32_t size)
{
    return ompi_coll_bine_reverse(nb ^ (nb >> 1)) >> (32 - (uint32_t) opal_cube_dim((int) size));
}

/* get the nu (step) value for a given rank, selecting the minimal encoding */
static inline uint32_t ompi_coll_bine_get_nu(uint32_t rank, uint32_t size)
{
    uint32_t nba = 0, nbb = 0;
    bool nba_ok = false, nbb_ok = false;
    int num_bits = opal_cube_dim((int) size);
    if (rank % 2) {
        nba_ok = ompi_coll_bine_try_negabinary(rank, (uint32_t) num_bits, &nba);
        nbb_ok = ompi_coll_bine_try_negabinary(rank - size, (uint32_t) num_bits, &nbb);
    } else {
        nba_ok = ompi_coll_bine_try_negabinary(-rank, (uint32_t) num_bits, &nba);
        nbb_ok = ompi_coll_bine_try_negabinary(-rank + size, (uint32_t) num_bits, &nbb);
    }
    assert(nba_ok || nbb_ok);

    if (nba_ok && !nbb_ok) {
        return ompi_coll_bine_nb_to_nu(nba, size);
    } else if (!nba_ok && nbb_ok) {
        return ompi_coll_bine_nb_to_nu(nbb, size);
    } else { // Check MSB
        int nu_a = ompi_coll_bine_nb_to_nu(nba, size);
        int nu_b = ompi_coll_bine_nb_to_nu(nbb, size);
        if (nu_a < nu_b) {
            return nu_a;
        } else {
            return nu_b;
        }
    }
}

/* remap a rank to its bine tree position; returns -1 on invalid input */
static inline int ompi_coll_bine_remap_rank(uint32_t num_ranks, uint32_t rank)
{
    uint32_t remap_rank = ompi_coll_bine_get_rank_negabinary_representation(num_ranks, rank);
    if (OPAL_UNLIKELY(remap_rank == UINT32_MAX)) {
        return -1;
    }

    if (num_ranks == 1) {
        return 0;
    }

    remap_rank = remap_rank ^ (remap_rank >> 1);
    int num_bits = opal_cube_dim((int) num_ranks);
    remap_rank = ompi_coll_bine_reverse(remap_rank) >> (32 - num_bits);
    return remap_rank;
}

/* get the sender rank for a given receiver in a bine tree */
static inline int ompi_coll_bine_get_sender(uint32_t num_ranks, uint32_t rank, uint32_t *sender)
{
    if (OPAL_UNLIKELY(0 == num_ranks)) {
        *sender = rank;
        return MPI_ERR_ARG;
    }

    uint32_t r = rank;
    for (uint32_t depth = 0; depth < BINE_MAX_STEPS; depth++) {
        int remap = ompi_coll_bine_remap_rank(num_ranks, r);
        if (OPAL_UNLIKELY(remap < 0)) {
            *sender = rank;
            return MPI_ERR_ARG;
        }
        if (remap == (int) rank) {
            *sender = r;
            return MPI_SUCCESS;
        }
        r = (uint32_t) remap;
    }
    *sender = rank;
    return MPI_ERR_ARG;
}

/*
 * Computes the destination rank for a given process in a bine
 * algorithm step.
 *
 * This function calculates the rank to which a process will communicate
 * based on the bine algorithm, ensuring the result is within the valid
 * range of ranks.
 *
 * rank: The rank of the current process.
 * step: The current step in the bine algorithm, in [0, BINE_MAX_STEPS).
 * comm_sz: The total number of processes in the communicator; a positive
 *          power of two.
 * dest: (out) The destination rank after applying the bine algorithm, a
 *       value in [0, comm_sz - 1].
 *
 * Returns MPI_SUCCESS on success, or an MPI error code.
 */
static inline int ompi_coll_bine_pi(int rank, int step, int comm_sz, int *dest)
{
    *dest = -1;
    if (OPAL_UNLIKELY(comm_sz <= 0)) {
        return MPI_ERR_ARG;
    }

    int rho;
    int err = ompi_coll_bine_jacobsthal(step + 1, &rho);
    if (OPAL_UNLIKELY(MPI_SUCCESS != err)) {
        return err;
    }

    if (step & 1) {
        rho = -rho;
    }

    if ((rank & 1) == 0) {
        *dest = (rank + rho) % comm_sz; // Even rank
    } else {
        *dest = (rank - rho) % comm_sz; // Odd rank
    }

    if (*dest < 0) {
        *dest += comm_sz; // Adjust for negative ranks
    }

    return MPI_SUCCESS;
}

/* recursive helper to build permutation mapping for bine allgather */
static inline int ompi_coll_bine_get_permutation_aux(int rank, int step, const int n_steps,
                                                     const int adj_size, int *bitmap, int offset)
{
    *(bitmap + rank) = offset;
    if (step >= n_steps)
        return MPI_SUCCESS;

    int peer;
    int err = MPI_SUCCESS;

    for (int s = step; s < n_steps; s++) {
        err = ompi_coll_bine_pi(rank, s, adj_size, &peer);
        if (OPAL_UNLIKELY(MPI_SUCCESS != err)) {
            return err;
        }
        err = ompi_coll_bine_get_permutation_aux(peer, s + 1, n_steps, adj_size, bitmap,
                                                 offset + (1 << (n_steps - s - 1)));
        if (OPAL_UNLIKELY(MPI_SUCCESS != err)) {
            return err;
        }
    }
    return MPI_SUCCESS;
}

/* compute permutation for bine allgather block reordering */
static inline int ompi_coll_bine_get_permutation(int rank, int step, const int n_steps,
                                                 const int adj_size, int *bitmap, int offset)
{
    if (step >= n_steps)
        return MPI_SUCCESS;

    int peer;
    int err = ompi_coll_bine_pi(rank, step, adj_size, &peer);
    if (OPAL_UNLIKELY(MPI_SUCCESS != err)) {
        return err;
    }
    return ompi_coll_bine_get_permutation_aux(peer, step + 1, n_steps, adj_size, bitmap, offset);
}

/*
 * Reorders blocks in a buffer according to a given permutation.
 *
 * buffer: The buffer containing the blocks to reorder.
 * block_size: The size of each block in bytes.
 * block_permutation: The permutation of the blocks.
 * num_blocks: The number of blocks in the buffer.
 *
 * Returns MPI_SUCCESS on success, or an error code.
 */
static inline int ompi_coll_bine_reorder_blocks(void *buffer, size_t block_size,
                                                int *block_permutation, int num_blocks)
{
    if (OPAL_UNLIKELY(buffer == NULL || block_permutation == NULL || num_blocks <= 0)) {
        return MPI_ERR_ARG;
    }

    char *buf = (char *) buffer;
    void *temp = malloc(block_size);
    if (NULL == temp) {
        return MPI_ERR_NO_MEM;
    }
    
    char *visited = (char *) calloc(num_blocks, 1);
    if (NULL == visited) {
        free(temp);
        return MPI_ERR_NO_MEM;
    }

    for (int i = 0; i < num_blocks; ++i) {
        // Skip if the block is already in its correct position or visited
        if (visited[i] == 1 || block_permutation[i] == i) {
            continue;
        }

        int current = i;
        // Save the current block to temp (start of the cycle)
        memcpy(temp, buf + current * block_size, block_size);

        // Follow the cycle and place each block in its final position
        if (block_permutation[current] < 0 || block_permutation[current] >= num_blocks) {
            free(temp);
            free(visited);
            return MPI_ERR_ARG;
        }

        while (visited[block_permutation[current]] != 1) {
            int next = block_permutation[current];
            if (next < 0 || next >= num_blocks) {
                free(temp);
                free(visited);
                return MPI_ERR_ARG;
            }
            memcpy(buf + current * block_size, buf + next * block_size, block_size);
            visited[current] = 1;
            current = next;
        }

        // Place the saved block in its final position
        memcpy(buf + current * block_size, temp, block_size);
        visited[current] = 1; // Mark the last block as visited
    }

    free(temp);
    free(visited);

    return MPI_SUCCESS;
}

/* Function to calculate a Mersenne number (2^n - 1): the mask with bits
 * 0..n-1 set.  The shift 1 << n fits a uint64_t for n <= 62; the value is
 * exact in the uint32_t return only for n <= 32, which is what callers use
 * (the reduce-scatter remap clears the current highest bit via mersenne(k + 1)).
 * Valid input range: 0 <= n <= 62.  Returns MPI_ERR_ARG for any n outside
 * that range.
 */
static inline int ompi_coll_mersenne(int n, uint32_t *res)
{
    if (OPAL_UNLIKELY(n < 0 || n > 62)) {
        return MPI_ERR_ARG;
    }
    uint64_t v = ((uint64_t) 1 << n) - 1;
    *res = (uint32_t) v;
    return MPI_SUCCESS;
}

/* remap bits using a distance-doubling transform for bine reduce-scatter */
static inline int ompi_coll_bine_remap_distance_doubling(uint32_t num, int *remapped)
{
    *remapped = 0;
    if (OPAL_UNLIKELY(0 == num)) {
        return MPI_SUCCESS;
    }
    if (OPAL_UNLIKELY(num >= (uint32_t) INT_MAX)) { // (int) num + 1 must stay positive in an int
        return MPI_ERR_ARG;
    }

    while (num > 0) {
        int k = opal_cube_dim((int) num + 1) - 1; // Find the position of the highest set bit
        *remapped ^= (0x1 << k);          // Set the k-th bit in the remapped number
        uint32_t mask;
        int err = ompi_coll_mersenne(k + 1, &mask); // 2^(k+1) - 1 clears bit k, flips bits 0..k-1
        if (OPAL_UNLIKELY(MPI_SUCCESS != err)) {
            *remapped = 0;
            return err;
        }
        num ^= mask;
    }
    return MPI_SUCCESS;
}

/**
 * ompi_mirror_perm: Returns mirror permutation of nbits low-order bits
 *                   of x [*].
 * [*] Warren Jr., Henry S. Hacker's Delight (2ed). 2013.
 *     Chapter 7. Rearranging Bits and Bytes.
 */
unsigned int ompi_mirror_perm(unsigned int x, int nbits);

/*
 * ompi_rounddown: Rounds a number down to nearest multiple.
 *     rounddown(10,4) = 8, rounddown(6,3) = 6, rounddown(14,3) = 12
 */
int ompi_rounddown(int num, int factor);

/**
 * If necessary, retain op and store it in the
 * request object, which should be of type ompi_coll_base_nbc_request_t
 * (will be cast internally).
 */
int ompi_coll_base_retain_op( ompi_request_t *request,
                              ompi_op_t *op,
                              ompi_datatype_t *type);

/**
 * If necessary, retain the datatypes and store them in the
 * request object, which should be of type ompi_coll_base_nbc_request_t
 * (will be cast internally).
 */
int ompi_coll_base_retain_datatypes( ompi_request_t *request,
                                     ompi_datatype_t *stype,
                                     ompi_datatype_t *rtype);

/**
 * If necessary, retain the datatypes and store them in the
 * request object, which should be of type ompi_coll_base_nbc_request_t
 * (will be cast internally).
 */
int ompi_coll_base_retain_datatypes_w( ompi_request_t *request,
                                       ompi_datatype_t * const stypes[],
                                       ompi_datatype_t * const rtypes[],
                                       bool use_topo);

/**
 * If necessary, set callback to free extra memory regions
 * set in release_arrays.  Not set if a callback is already
 * associated with the request.
 */
int ompi_coll_base_add_release_arrays_cb(ompi_request_t *request);

/* File reading function */
int ompi_coll_base_file_getnext_long(FILE *fptr, int *fileline, long* val);
int ompi_coll_base_file_getnext_size_t(FILE *fptr, int *fileline, size_t* val);
int ompi_coll_base_file_getnext_string(FILE *fptr, int *fileline, char** val);
/* peek at the next valid token to see if it begins with the expected value. If yes
 * eat the value, otherwise put it back into the file.
 */
int ompi_coll_base_file_peek_next_char_is(FILE *fptr, int *fileline, int expected);
int ompi_coll_base_file_peek_next_char_isdigit(FILE *fptr);

/* Miscellaneous function */
const char* mca_coll_base_colltype_to_str(int collid);
int mca_coll_base_name_to_colltype(const char* name);

END_C_DECLS
#endif /* MCA_COLL_BASE_UTIL_EXPORT_H */
