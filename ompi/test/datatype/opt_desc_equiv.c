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
 * opt_desc / desc byte-equivalence check.
 *
 * The datatype optimizer rewrites a committed type's canonical descriptor
 * (opal_datatype_t::desc) into an optimized one (::opt_desc) that the engine
 * uses on the hot path.  For a homogeneous pack/unpack the two must be
 * interchangeable to the byte: walking either descriptor has to read and write
 * exactly the same bytes.  A bug in the optimizer breaks that silently -- no
 * crash, just a wrong stream for one particular datatype shape.
 *
 * For every datatype in the shared corpus this test packs (and unpacks) once
 * through the real opt_desc and once through the canonical desc, driving the
 * walk at several fragment sizes so the descriptor is stopped and resumed at
 * every awkward interior offset, and requires the results to be identical.
 * When the corpus supplies a by-hand reference it is used as a third,
 * independent oracle.  Any mismatch aborts the job with a nonzero status, so
 * the test gates CI through "make check".
 *
 * The baseline pass is obtained by temporarily aliasing opt_desc to desc for
 * the datatype under test, so both passes share the identical prepare/pack code
 * path and differ only in the descriptor walked.  This is a single-threaded
 * test operating on datatypes it owns, and the original opt_desc is restored
 * immediately afterwards.
 */

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/datatype/ompi_datatype.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_datatype_prototypes.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "datatype_corpus.h"

/* Fragment sizes (bytes) used to chop the packed stream so the descriptor walk
 * has to pause and resume at varied interior boundaries.  The engine advances
 * the pack/unpack in whole basic-element units, so a fragment must be at least
 * as large as the widest basic type in the corpus (8 bytes: double / int64) to
 * guarantee forward progress; smaller fragments are not a meaningful resume
 * point because the walk never stops below element granularity.  The chosen
 * sizes are deliberately not multiples of the datatype extents so successive
 * calls resume on different element boundaries.  0 is a sentinel for "one shot"
 * (the whole stream in a single call). */
static const size_t fragment_sizes[] = {12, 16, 40, 4096, 0};

static opal_datatype_t *opal_of(MPI_Datatype type)
{
    return &((ompi_datatype_t *) type)->super;
}

/*
 * Force a prepared convertor onto the generic interpreter walking the canonical
 * desc instead of the optimizer's opt_desc.  The optimized fast-path movers read
 * pData->opt_desc (and a mover-specific stack) directly, so the baseline cannot
 * be produced by swapping descriptors on those movers; it must run the general
 * mover over a freshly rebuilt generic stack.  Clearing CONVERTOR_NO_OP makes
 * opal_convertor_{pack,unpack} dispatch to fAdvance, and clearing the datatype
 * CONTIGUOUS flag makes opal_convertor_set_position_nocheck build the generic
 * (element-count) stack the general mover expects.
 */
static void force_desc_baseline(opal_convertor_t *conv, MPI_Datatype type, int is_pack)
{
    opal_datatype_t *odt = opal_of(type);
    size_t pos = 0;

    conv->use_desc = &odt->desc;
    conv->fAdvance = is_pack ? opal_pack_general : opal_unpack_general;
    conv->flags &= ~(OPAL_DATATYPE_FLAG_CONTIGUOUS | CONVERTOR_NO_OP | CONVERTOR_COMPLETED);
    opal_convertor_set_position_nocheck(conv, &pos);
}

/*
 * Pack @count instances of @type from @buf into @out (capacity @outcap),
 * issuing successive calls whose iovec is capped at @frag bytes (@frag == 0
 * means one unbounded call).  When @baseline is set the pack runs the generic
 * interpreter over the canonical desc; otherwise it runs the natural (optimized)
 * path.  Returns the total number of packed bytes.
 */
static size_t pack_stream(MPI_Datatype type, int count, void *buf, char *out, size_t outcap,
                          size_t frag, int baseline)
{
    opal_convertor_t *conv = opal_convertor_create(opal_local_arch, 0);
    size_t total = 0;
    int complete = 0;

    opal_convertor_prepare_for_send(conv, opal_of(type), (size_t) count, buf);
    if (baseline) {
        force_desc_baseline(conv, type, 1);
    }

    while (!complete) {
        struct iovec iov;
        uint32_t iov_count = 1;
        size_t max_data = 0;
        size_t want = (0 == frag) ? (outcap - total) : frag;

        if (want > (outcap - total)) {
            want = outcap - total;
        }
        iov.iov_base = out + total;
        iov.iov_len = want;
        complete = opal_convertor_pack(conv, &iov, &iov_count, &max_data);
        total += max_data;
        if ((0 == max_data) && (0 == complete)) {
            fprintf(stderr, "pack made no progress (frag=%zu)\n", frag);
            MPI_Abort(MPI_COMM_WORLD, MPI_ERR_INTERN);
        }
    }
    OBJ_RELEASE(conv);
    return total;
}

/*
 * Unpack @packed_len bytes from @in into @count instances of @type in @buf,
 * capping each call's iovec at @frag bytes.
 */
static void unpack_stream(MPI_Datatype type, int count, void *buf, char *in, size_t packed_len,
                          size_t frag, int baseline)
{
    opal_convertor_t *conv = opal_convertor_create(opal_local_arch, 0);
    size_t total = 0;
    int complete = 0;

    opal_convertor_prepare_for_recv(conv, opal_of(type), (size_t) count, buf);
    if (baseline) {
        force_desc_baseline(conv, type, 0);
    }

    while (!complete && (total < packed_len)) {
        struct iovec iov;
        uint32_t iov_count = 1;
        size_t max_data = 0;
        size_t want = (0 == frag) ? (packed_len - total) : frag;

        if (want > (packed_len - total)) {
            want = packed_len - total;
        }
        iov.iov_base = in + total;
        iov.iov_len = want;
        complete = opal_convertor_unpack(conv, &iov, &iov_count, &max_data);
        total += max_data;
        if ((0 == max_data) && (0 == complete)) {
            fprintf(stderr, "unpack made no progress (frag=%zu)\n", frag);
            MPI_Abort(MPI_COMM_WORLD, MPI_ERR_INTERN);
        }
    }
    OBJ_RELEASE(conv);
}

static void fill_pattern(unsigned char *buf, size_t len, unsigned int seed)
{
    for (size_t i = 0; i < len; ++i) {
        buf[i] = (unsigned char) ((i * 131u + seed) & 0xff);
    }
}

static int failures = 0;

static void report(const char *name, const char *what, size_t frag, size_t offset)
{
    fprintf(stderr, "MISMATCH: datatype '%s' %s differs (fragment=%zu, first byte offset=%zu)\n",
            name, what, frag, offset);
    ++failures;
}

static size_t first_diff(const char *a, const char *b, size_t len)
{
    for (size_t i = 0; i < len; ++i) {
        if (a[i] != b[i]) {
            return i;
        }
    }
    return len;
}

static void check_entry(const datatype_corpus_entry_t *entry)
{
    MPI_Datatype type = entry->send_type;
    opal_datatype_t *odt = opal_of(type);
    const int count = 7; /* a few instances so loop boundaries are crossed */
    size_t type_size = odt->size;
    size_t span, packed_len_opt;
    MPI_Aint true_lb, true_extent, lb, extent;
    ptrdiff_t lo = 0, hi = 0;
    unsigned char *src_raw, *dst_opt_raw, *dst_desc_raw;
    unsigned char *src, *dst_opt, *dst_desc; /* strided bases (raw - lo) */
    char *packed_opt, *packed_desc, *packed_byhand;

    if (0 == type_size) {
        return;
    }

    /*
     * The datatype's instances may stride forward, backward (negative extent) or
     * overlap (zero extent).  Compute the byte range an instance touches with the
     * true extent, then the union over all @count instances, and anchor the
     * strided buffers so instance i lands at src + true_lb + i*extent for every i
     * regardless of sign.  For the common lb == 0, positive-extent datatypes this
     * reduces to lo == 0 (base == raw).
     */
    MPI_Type_get_true_extent(type, &true_lb, &true_extent);
    MPI_Type_get_extent(type, &lb, &extent);
    (void) lb; /* only the stride (extent) and true byte span matter here */
    for (int i = 0; i < count; ++i) {
        ptrdiff_t s = (ptrdiff_t) true_lb + (ptrdiff_t) i * (ptrdiff_t) extent;
        ptrdiff_t e = s + (ptrdiff_t) true_extent;

        if (0 == i) {
            lo = s;
            hi = e;
        } else {
            if (s < lo) {
                lo = s;
            }
            if (e > hi) {
                hi = e;
            }
        }
    }
    span = (size_t) (hi - lo);
    if (span < type_size * (size_t) count) {
        span = type_size * (size_t) count;
    }

    src_raw = (unsigned char *) malloc(span);
    dst_opt_raw = (unsigned char *) malloc(span);
    dst_desc_raw = (unsigned char *) malloc(span);
    packed_opt = (char *) malloc(type_size * (size_t) count);
    packed_desc = (char *) malloc(type_size * (size_t) count);
    packed_byhand = (char *) malloc(type_size * (size_t) count);
    if ((NULL == src_raw) || (NULL == dst_opt_raw) || (NULL == dst_desc_raw)
        || (NULL == packed_opt) || (NULL == packed_desc) || (NULL == packed_byhand)) {
        fprintf(stderr, "opt_desc_equiv: out of memory\n");
        MPI_Abort(MPI_COMM_WORLD, MPI_ERR_NO_MEM);
    }
    src = src_raw - lo;
    dst_opt = dst_opt_raw - lo;
    dst_desc = dst_desc_raw - lo;

    fill_pattern(src_raw, span, 0x5a);

    /* Reference by-hand pack (content oracle), if available. */
    if (NULL != entry->pack_byhand) {
        entry->pack_byhand(packed_byhand, src, count);
    }

    for (size_t f = 0; f < sizeof(fragment_sizes) / sizeof(fragment_sizes[0]); ++f) {
        size_t frag = fragment_sizes[f];
        size_t packed_len_desc, off;

        /* ---- PACK through the natural (optimized) path ---- */
        memset(packed_opt, 0xcc, type_size * (size_t) count);
        packed_len_opt = pack_stream(type, count, src, packed_opt, type_size * (size_t) count, frag,
                                     0);

        /* ---- PACK through the generic interpreter over the canonical desc ---- */
        memset(packed_desc, 0x33, type_size * (size_t) count);
        packed_len_desc = pack_stream(type, count, src, packed_desc,
                                      type_size * (size_t) count, frag, 1);

        if (packed_len_opt != packed_len_desc) {
            report(entry->name, "packed length", frag, 0);
        } else {
            off = first_diff(packed_opt, packed_desc, packed_len_opt);
            if (off != packed_len_opt) {
                report(entry->name, "packed bytes (opt vs desc)", frag, off);
            }
        }

        if ((NULL != entry->pack_byhand) && (packed_len_opt == type_size * (size_t) count)) {
            off = first_diff(packed_opt, packed_byhand, packed_len_opt);
            if (off != packed_len_opt) {
                report(entry->name, "packed bytes (opt vs by-hand)", frag, off);
            }
        }

        /* ---- UNPACK equivalence: same stream, optimized path vs generic/desc ---- */
        fill_pattern(dst_opt_raw, span, 0xa5);
        memcpy(dst_desc_raw, dst_opt_raw, span);

        unpack_stream(type, count, dst_opt, packed_opt, packed_len_opt, frag, 0);
        unpack_stream(type, count, dst_desc, packed_opt, packed_len_opt, frag, 1);

        off = first_diff((char *) dst_opt_raw, (char *) dst_desc_raw, span);
        if (off != span) {
            report(entry->name, "unpacked bytes (opt vs desc)", frag, off);
        }
    }

    free(src_raw);
    free(dst_opt_raw);
    free(dst_desc_raw);
    free(packed_opt);
    free(packed_desc);
    free(packed_byhand);
}

int main(int argc, char *argv[])
{
    datatype_corpus_t *corpus;

    MPI_Init(&argc, &argv);

    corpus = datatype_corpus_init();
    for (size_t i = 0; i < corpus->count; ++i) {
        check_entry(&corpus->entries[i]);
    }
    datatype_corpus_finalize(corpus);

    if (0 != failures) {
        fprintf(stderr, "opt_desc_equiv: %d mismatch(es) detected\n", failures);
        MPI_Abort(MPI_COMM_WORLD, MPI_ERR_OTHER);
    }
    printf("SUCCESS: opt_desc and desc are byte-equivalent across the datatype corpus\n");

    MPI_Finalize();
    return 0;
}
