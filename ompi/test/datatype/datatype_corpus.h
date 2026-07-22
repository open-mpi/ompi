/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * A reusable corpus of committed MPI datatypes plus, for each, an independent
 * by-hand pack/unpack reference.  A single definition of each datatype and its
 * hand-written baseline can be shared by any number of tests instead of every
 * test re-deriving the constructions.
 *
 * The corpus is intentionally consumer-agnostic: it describes each datatype
 * (name, handles, by-hand references, shape traits) but knows nothing about how
 * a given consumer selects, drives, or reports on the datatypes.  A consumer
 * that wants to run a subset applies its own policy on top of the entries (for
 * example by name or by the shape traits below).
 *
 * Everything here uses only the public MPI API so the corpus keeps working in a
 * standalone (installed-MPI) build; consumers that need Open MPI internals pull
 * those in themselves.  (For the older OMPI-internal grab-bag of à-la-carte
 * builders returning caller-freed ompi_datatype_t handles, see ddt_lib.[hc];
 * the two libraries deliberately sit at different API layers.)
 */

#ifndef OMPI_TEST_DATATYPE_CORPUS_H
#define OMPI_TEST_DATATYPE_CORPUS_H

#include "mpi.h"

#include <stddef.h>
#include <stdint.h>

/*
 * Move @count instances of the datatype between a strided user buffer and a
 * contiguous packed buffer.  For a pack, @src is the strided user layout and
 * @dst the packed stream; for an unpack the two are swapped.
 */
typedef void (*datatype_byhand_fn_t)(void *dst, const void *src, int count);

/*
 * Shape traits so a consumer can select a slice of the corpus (e.g. only the
 * mixed-type datatypes that exercise the optimizer's type-promotion path) or
 * decide how hard to exercise a given entry.  A single datatype may carry
 * several of these.
 */
typedef enum {
    DT_TRAIT_CONTIGUOUS  = 1u << 0,
    DT_TRAIT_HAS_GAPS    = 1u << 1,
    DT_TRAIT_NESTED_LOOP = 1u << 2,
    DT_TRAIT_RESIZED     = 1u << 3,
    DT_TRAIT_MIXED_TYPES = 1u << 4, /* distinct basic types in one region -> promotion */
    DT_TRAIT_SINGLE_ITER = 1u << 5,
    DT_TRAIT_NEG_EXTENT  = 1u << 6,
    DT_TRAIT_ZERO_EXTENT = 1u << 7,
    DT_TRAIT_OVERLAP     = 1u << 8,
    DT_TRAIT_PAIR        = 1u << 9, /* send_type != recv_type */
} datatype_trait_t;

/* One datatype in the corpus plus everything needed to exercise and verify it. */
typedef struct {
    const char          *name;          /* stable, unique; used for output and selection */
    MPI_Datatype         send_type;     /* committed */
    MPI_Datatype         recv_type;     /* == send_type unless DT_TRAIT_PAIR */
    datatype_byhand_fn_t pack_byhand;   /* reference packer, or NULL */
    datatype_byhand_fn_t unpack_byhand; /* reference unpacker, or NULL */
    unsigned int         traits;        /* OR of datatype_trait_t */
} datatype_corpus_entry_t;

typedef struct {
    datatype_corpus_entry_t *entries;
    size_t                   count;
} datatype_corpus_t;

/*
 * Build and commit every datatype in the corpus.  Must be called after
 * MPI_Init().  Aborts the job (MPI_Abort) on any construction failure.  The
 * returned corpus and its datatypes are valid until datatype_corpus_finalize().
 */
datatype_corpus_t *datatype_corpus_init(void);

/* Free every committed datatype and the corpus bookkeeping itself. */
void datatype_corpus_finalize(datatype_corpus_t *corpus);

#endif /* OMPI_TEST_DATATYPE_CORPUS_H */
