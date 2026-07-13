/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for the ompi/datatype layer (ompi_datatype_create_*.c,
 * ompi_datatype_module.c, ompi_datatype_args.c).  Exercised through the
 * public MPI datatype constructor API after a full singleton MPI_Init;
 * each MPI_Type_* call dispatches into the corresponding
 * ompi_datatype_create_* function.
 *
 * Note: the library is compiled with -DNDEBUG, so assert() is a no-op
 * here -- all verification must go through test_verify().
 */

#include "ompi_config.h"

#include <string.h>
#include <stdlib.h>

#include "support.h"

#include "ompi/datatype/ompi_datatype.h"
#include "ompi/proc/proc.h"

#include "mpi.h"

static void check_committed_size(const char *what, MPI_Datatype t, MPI_Aint expect_size);

static void test_contiguous(void);
static void test_vector(void);
static void test_indexed(void);
static void test_struct(void);
static void test_subarray(void);
static void test_darray(void);
static void test_dup_and_name(void);
static void test_envelope_contents(void);
static void test_get_contents_c(void);
static void test_match_size(void);
static void test_resized(void);
static void test_pack_unpack(void);
static void test_pack_external(void);
static void test_get_elements(void);
static void test_pack_description(void);

int main(int argc, char *argv[])
{
    test_init("ompi datatype");

    int rc = MPI_Init(&argc, &argv);
    test_verify("MPI_Init succeeds", MPI_SUCCESS == rc);

    /* Route MPI errors back as return codes instead of aborting.  Errors in
     * the datatype functions are not attached to a handle, so they invoke the
     * default communicator's error handler (MPI_COMM_WORLD in MPI-3 compat
     * mode); with MPI_ERRORS_RETURN a failure -- e.g. MPI_Type_match_size
     * finding no match -- surfaces through test_verify instead of aborting
     * this launcher-less singleton. */
    MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN);

    test_contiguous();
    test_vector();
    test_indexed();
    test_struct();
    test_subarray();
    test_darray();
    test_dup_and_name();
    test_envelope_contents();
    test_get_contents_c();
    test_match_size();
    test_resized();
    test_pack_unpack();
    test_pack_external();
    test_get_elements();
    test_pack_description();

    int r = test_finalize();
    MPI_Finalize();
    return r;
}

/* ------------------------------------------------------------------ */

/* Commit a derived type, verify its packed size, then free it. */
static void check_committed_size(const char *what, MPI_Datatype t, MPI_Aint expect_size)
{
    int rc = MPI_Type_commit(&t);
    test_verify(what, MPI_SUCCESS == rc);

    int size = -1;
    MPI_Type_size(t, &size);
    test_verify("type packed size matches", (int) expect_size == size);

    MPI_Aint lb = 0, extent = 0;
    rc = MPI_Type_get_extent(t, &lb, &extent);
    test_verify("Type_get_extent succeeds", MPI_SUCCESS == rc);

    MPI_Type_free(&t);
    test_verify("Type_free sets handle to MPI_DATATYPE_NULL", MPI_DATATYPE_NULL == t);
}

/* ------------------------------------------------------------------ */

static void test_contiguous(void)
{
    MPI_Datatype t = MPI_DATATYPE_NULL;
    int rc = MPI_Type_contiguous(4, MPI_INT, &t);
    test_verify("Type_contiguous succeeds", MPI_SUCCESS == rc);
    check_committed_size("contiguous commit", t, 4 * sizeof(int));
}

/* ------------------------------------------------------------------ */

static void test_vector(void)
{
    /* 2 blocks of 3 ints, stride 5 ints */
    MPI_Datatype t = MPI_DATATYPE_NULL;
    int rc = MPI_Type_vector(2, 3, 5, MPI_INT, &t);
    test_verify("Type_vector succeeds", MPI_SUCCESS == rc);
    check_committed_size("vector commit", t, 2 * 3 * sizeof(int));

    MPI_Datatype h = MPI_DATATYPE_NULL;
    rc = MPI_Type_create_hvector(2, 3, 5 * sizeof(int), MPI_INT, &h);
    test_verify("Type_create_hvector succeeds", MPI_SUCCESS == rc);
    check_committed_size("hvector commit", h, 2 * 3 * sizeof(int));
}

/* ------------------------------------------------------------------ */

static void test_indexed(void)
{
    int blens[2] = {1, 2};
    int displs[2] = {0, 3};

    MPI_Datatype t = MPI_DATATYPE_NULL;
    int rc = MPI_Type_indexed(2, blens, displs, MPI_INT, &t);
    test_verify("Type_indexed succeeds", MPI_SUCCESS == rc);
    check_committed_size("indexed commit", t, 3 * sizeof(int));

    MPI_Aint hdispls[2] = {0, 3 * sizeof(int)};
    MPI_Datatype h = MPI_DATATYPE_NULL;
    rc = MPI_Type_create_hindexed(2, blens, hdispls, MPI_INT, &h);
    test_verify("Type_create_hindexed succeeds", MPI_SUCCESS == rc);
    check_committed_size("hindexed commit", h, 3 * sizeof(int));

    MPI_Datatype b = MPI_DATATYPE_NULL;
    rc = MPI_Type_create_indexed_block(2, 2, displs, MPI_INT, &b);
    test_verify("Type_create_indexed_block succeeds", MPI_SUCCESS == rc);
    check_committed_size("indexed_block commit", b, 2 * 2 * sizeof(int));

    MPI_Datatype hb = MPI_DATATYPE_NULL;
    rc = MPI_Type_create_hindexed_block(2, 2, hdispls, MPI_INT, &hb);
    test_verify("Type_create_hindexed_block succeeds", MPI_SUCCESS == rc);
    check_committed_size("hindexed_block commit", hb, 2 * 2 * sizeof(int));
}

/* ------------------------------------------------------------------ */

static void test_struct(void)
{
    int blens[2] = {1, 1};
    MPI_Aint displs[2] = {0, sizeof(int)};
    MPI_Datatype types[2] = {MPI_INT, MPI_CHAR};

    MPI_Datatype t = MPI_DATATYPE_NULL;
    int rc = MPI_Type_create_struct(2, blens, displs, types, &t);
    test_verify("Type_create_struct succeeds", MPI_SUCCESS == rc);
    check_committed_size("struct commit", t, sizeof(int) + sizeof(char));
}

/* ------------------------------------------------------------------ */

static void test_subarray(void)
{
    int sizes[2] = {4, 4};
    int subsizes[2] = {2, 2};
    int starts[2] = {1, 1};

    MPI_Datatype t = MPI_DATATYPE_NULL;
    int rc = MPI_Type_create_subarray(2, sizes, subsizes, starts,
                                      MPI_ORDER_C, MPI_INT, &t);
    test_verify("Type_create_subarray succeeds", MPI_SUCCESS == rc);
    check_committed_size("subarray commit", t, 2 * 2 * sizeof(int));
}

/* ------------------------------------------------------------------ */

static void test_darray(void)
{
    /* singleton: 1 process, so the local block is the whole array */
    int gsizes[1] = {8};
    int distribs[1] = {MPI_DISTRIBUTE_BLOCK};
    int dargs[1] = {MPI_DISTRIBUTE_DFLT_DARG};
    int psizes[1] = {1};

    MPI_Datatype t = MPI_DATATYPE_NULL;
    int rc = MPI_Type_create_darray(1, 0, 1, gsizes, distribs, dargs, psizes,
                                    MPI_ORDER_C, MPI_INT, &t);
    test_verify("Type_create_darray succeeds", MPI_SUCCESS == rc);
    check_committed_size("darray commit", t, 8 * sizeof(int));
}

/* ------------------------------------------------------------------ */

static void test_dup_and_name(void)
{
    MPI_Datatype t = MPI_DATATYPE_NULL;
    MPI_Type_contiguous(2, MPI_DOUBLE, &t);
    MPI_Type_commit(&t);

    MPI_Datatype dup = MPI_DATATYPE_NULL;
    int rc = MPI_Type_dup(t, &dup);
    test_verify("Type_dup succeeds", MPI_SUCCESS == rc);

    int dsize = -1, osize = -1;
    MPI_Type_size(dup, &dsize);
    MPI_Type_size(t, &osize);
    test_verify("dup has same size as original", dsize == osize);

    rc = MPI_Type_set_name(dup, "mydup");
    test_verify("Type_set_name succeeds", MPI_SUCCESS == rc);
    char name[MPI_MAX_OBJECT_NAME];
    int len = -1;
    MPI_Type_get_name(dup, name, &len);
    test_verify("Type_get_name round-trips", 0 == strcmp(name, "mydup"));

    MPI_Type_free(&dup);
    MPI_Type_free(&t);
}

/* ------------------------------------------------------------------ */

/* Free any returned constituent types that are derived (predefined
 * types must not be freed). */
static void free_returned_types(MPI_Datatype *types, int n)
{
    for (int i = 0; i < n; ++i) {
        int ni, na, nt, comb;
        MPI_Type_get_envelope(types[i], &ni, &na, &nt, &comb);
        if (MPI_COMBINER_NAMED != comb) {
            MPI_Type_free(&types[i]);
        }
    }
}

/*
 * Round-trip a committed derived type through get_envelope + get_contents,
 * verifying the reported combiner.  Exercises the decode side of
 * ompi_datatype_args.c for every combiner.  Output arrays are intentionally
 * oversized (valid per the MPI standard).
 */
static void check_envelope(const char *what, MPI_Datatype t, int expect_combiner)
{
    int ni = -1, na = -1, nt = -1, combiner = -1;
    int rc = MPI_Type_get_envelope(t, &ni, &na, &nt, &combiner);
    test_verify(what, MPI_SUCCESS == rc && expect_combiner == combiner);

    int ints[32];
    MPI_Aint addrs[32];
    MPI_Datatype types[32];
    rc = MPI_Type_get_contents(t, 32, 32, 32, ints, addrs, types);
    test_verify("get_contents succeeds", MPI_SUCCESS == rc);
    if (MPI_SUCCESS == rc) {
        free_returned_types(types, nt);
    }

    /* Also exercise the large-count form (MPI_Type_get_contents_c) with
     * oversized output arrays: it shares the same loop-bound fix, where the
     * constituent-datatype fix-up must iterate over the actual count, not the
     * caller-provided max_datatypes. */
    int cints[32];
    MPI_Aint caddrs[32];
    MPI_Count ccounts[32];
    MPI_Datatype ctypes[32];
    rc = MPI_Type_get_contents_c(t, 32, 32, 32, 32, cints, caddrs, ccounts, ctypes);
    test_verify("get_contents_c succeeds", MPI_SUCCESS == rc);
    if (MPI_SUCCESS == rc) {
        free_returned_types(ctypes, nt);
    }
}

static void test_envelope_contents(void)
{
    /* named predefined type */
    int ni, na, nt, combiner = -1;
    MPI_Type_get_envelope(MPI_INT, &ni, &na, &nt, &combiner);
    test_verify("predefined type combiner is MPI_COMBINER_NAMED",
                MPI_COMBINER_NAMED == combiner);

    int blens[2] = {1, 2};
    int displs[2] = {0, 3};
    MPI_Aint hdispls[2] = {0, 3 * sizeof(int)};
    MPI_Datatype memb[2] = {MPI_INT, MPI_CHAR};
    int sizes[2] = {4, 4}, subsizes[2] = {2, 2}, starts[2] = {0, 0};
    int gsizes[1] = {4}, distribs[1] = {MPI_DISTRIBUTE_BLOCK};
    int dargs[1] = {MPI_DISTRIBUTE_DFLT_DARG}, psizes[1] = {1};

    MPI_Datatype t = MPI_DATATYPE_NULL;

    MPI_Type_contiguous(4, MPI_INT, &t); MPI_Type_commit(&t);
    check_envelope("contiguous combiner", t, MPI_COMBINER_CONTIGUOUS);
    MPI_Type_free(&t);

    MPI_Type_vector(2, 3, 5, MPI_INT, &t); MPI_Type_commit(&t);
    check_envelope("vector combiner", t, MPI_COMBINER_VECTOR);
    MPI_Type_free(&t);

    MPI_Type_create_hvector(2, 3, 20, MPI_INT, &t); MPI_Type_commit(&t);
    check_envelope("hvector combiner", t, MPI_COMBINER_HVECTOR);
    MPI_Type_free(&t);

    MPI_Type_indexed(2, blens, displs, MPI_INT, &t); MPI_Type_commit(&t);
    check_envelope("indexed combiner", t, MPI_COMBINER_INDEXED);
    MPI_Type_free(&t);

    MPI_Type_create_hindexed(2, blens, hdispls, MPI_INT, &t); MPI_Type_commit(&t);
    check_envelope("hindexed combiner", t, MPI_COMBINER_HINDEXED);
    MPI_Type_free(&t);

    MPI_Type_create_indexed_block(2, 2, displs, MPI_INT, &t); MPI_Type_commit(&t);
    check_envelope("indexed_block combiner", t, MPI_COMBINER_INDEXED_BLOCK);
    MPI_Type_free(&t);

    MPI_Type_create_hindexed_block(2, 2, hdispls, MPI_INT, &t); MPI_Type_commit(&t);
    check_envelope("hindexed_block combiner", t, MPI_COMBINER_HINDEXED_BLOCK);
    MPI_Type_free(&t);

    MPI_Type_create_struct(2, blens, hdispls, memb, &t); MPI_Type_commit(&t);
    check_envelope("struct combiner", t, MPI_COMBINER_STRUCT);
    MPI_Type_free(&t);

    MPI_Type_create_subarray(2, sizes, subsizes, starts, MPI_ORDER_C, MPI_INT, &t);
    MPI_Type_commit(&t);
    check_envelope("subarray combiner", t, MPI_COMBINER_SUBARRAY);
    MPI_Type_free(&t);

    MPI_Type_create_darray(1, 0, 1, gsizes, distribs, dargs, psizes,
                           MPI_ORDER_C, MPI_INT, &t);
    MPI_Type_commit(&t);
    check_envelope("darray combiner", t, MPI_COMBINER_DARRAY);
    MPI_Type_free(&t);

    MPI_Type_create_resized(MPI_INT, 0, 8, &t); MPI_Type_commit(&t);
    check_envelope("resized combiner", t, MPI_COMBINER_RESIZED);
    MPI_Type_free(&t);

    MPI_Datatype base = MPI_DATATYPE_NULL;
    MPI_Type_contiguous(2, MPI_INT, &base); MPI_Type_commit(&base);
    MPI_Type_dup(base, &t); MPI_Type_commit(&t);
    check_envelope("dup combiner", t, MPI_COMBINER_DUP);
    MPI_Type_free(&t);
    MPI_Type_free(&base);
}

/* ------------------------------------------------------------------ */

/*
 * Exercise the large-count contents path explicitly.  A datatype built with
 * an "_c" constructor stores its count in array_of_large_counts (not
 * array_of_integers), so MPI_Type_get_contents_c must return that value.  The
 * output arrays are oversized, which also covers the loop-bound fix on the _c
 * path (the constituent-datatype fix-up must iterate over the actual count,
 * not the caller-provided max_datatypes).
 */
static void test_get_contents_c(void)
{
    MPI_Datatype t = MPI_DATATYPE_NULL;
    int rc = MPI_Type_contiguous_c(7, MPI_INT, &t);
    test_verify("Type_contiguous_c succeeds", MPI_SUCCESS == rc && MPI_DATATYPE_NULL != t);
    MPI_Type_commit(&t);

    MPI_Count ni = -1, na = -1, nc = -1, nt = -1;
    int combiner = -1;
    MPI_Type_get_envelope_c(t, &ni, &na, &nc, &nt, &combiner);
    test_verify("get_envelope_c reports CONTIGUOUS", MPI_COMBINER_CONTIGUOUS == combiner);
    test_verify("get_envelope_c reports one large count", 1 == nc);
    test_verify("get_envelope_c reports one constituent datatype", 1 == nt);

    int cints[32];
    MPI_Aint caddrs[32];
    MPI_Count ccounts[32];
    MPI_Datatype ctypes[32];
    rc = MPI_Type_get_contents_c(t, 32, 32, 32, 32, cints, caddrs, ccounts, ctypes);
    test_verify("get_contents_c succeeds", MPI_SUCCESS == rc);
    test_verify("get_contents_c returns the large count in array_of_large_counts",
                7 == ccounts[0]);
    test_verify("get_contents_c returns the constituent datatype", MPI_INT == ctypes[0]);
    if (MPI_SUCCESS == rc) {
        free_returned_types(ctypes, (int) nt);
    }

    MPI_Type_free(&t);
}

/* ------------------------------------------------------------------ */

/*
 * MPI_Type_match_size maps a (typeclass, size) pair to an *intrinsic Fortran*
 * predefined datatype: the C binding forces the Fortran language flag, so it
 * resolves Fortran intrinsic types regardless of the calling language.  It
 * must return a scalar intrinsic of the requested size -- never a C type, an
 * "unavailable" type, or a composite type such as MPI_2REAL.
 *
 * This test asserts the behavior that is correct for the build as configured
 * (it is not a workaround to make the test pass):
 *
 *   - With the Fortran bindings built, each request must succeed and return
 *     the expected scalar intrinsic of the requested size.
 *   - With --disable-mpi-fortran the intrinsic types are unavailable, so per
 *     MPI-5.0 19.1 ("erroneous to specify a size not supported by the
 *     compiler") each request must fail (MPI_ERRORS_RETURN is set in main()).
 */
static void test_match_size(void)
{
    MPI_Datatype t;
    int rc, size;

    t = MPI_DATATYPE_NULL;
    rc = MPI_Type_match_size(MPI_TYPECLASS_REAL, (int) sizeof(double), &t);
#if OMPI_BUILD_FORTRAN_BINDINGS
    test_verify("Type_match_size(REAL, sizeof(double)) succeeds",
                MPI_SUCCESS == rc && MPI_DATATYPE_NULL != t);
    test_verify("Type_match_size(REAL, sizeof(double)) returns a scalar real intrinsic (not a composite)",
                MPI_REAL8 == t || MPI_DOUBLE_PRECISION == t);
    size = -1;
    MPI_Type_size(t, &size);
    test_verify("matched real type has the requested size", (int) sizeof(double) == size);
#else
    test_verify("Type_match_size(REAL, sizeof(double)) fails without Fortran support",
                MPI_SUCCESS != rc);
#endif

    t = MPI_DATATYPE_NULL;
    rc = MPI_Type_match_size(MPI_TYPECLASS_INTEGER, (int) sizeof(int), &t);
#if OMPI_BUILD_FORTRAN_BINDINGS
    test_verify("Type_match_size(INTEGER, sizeof(int)) succeeds",
                MPI_SUCCESS == rc && MPI_DATATYPE_NULL != t);
    test_verify("Type_match_size(INTEGER, sizeof(int)) returns a scalar integer intrinsic",
                MPI_INTEGER == t || MPI_INTEGER4 == t);
    size = -1;
    MPI_Type_size(t, &size);
    test_verify("matched integer type has the requested size", (int) sizeof(int) == size);
#else
    test_verify("Type_match_size(INTEGER, sizeof(int)) fails without Fortran support",
                MPI_SUCCESS != rc);
#endif

    t = MPI_DATATYPE_NULL;
    rc = MPI_Type_match_size(MPI_TYPECLASS_COMPLEX, (int) (2 * sizeof(float)), &t);
#if OMPI_BUILD_FORTRAN_BINDINGS
    test_verify("Type_match_size(COMPLEX, 2*sizeof(float)) succeeds",
                MPI_SUCCESS == rc && MPI_DATATYPE_NULL != t);
    test_verify("Type_match_size(COMPLEX, 2*sizeof(float)) returns a scalar complex intrinsic",
                MPI_COMPLEX == t || MPI_COMPLEX8 == t);
    size = -1;
    MPI_Type_size(t, &size);
    test_verify("matched complex type has the requested size", (int) (2 * sizeof(float)) == size);
#else
    test_verify("Type_match_size(COMPLEX, 2*sizeof(float)) fails without Fortran support",
                MPI_SUCCESS != rc);
#endif
}

/* ------------------------------------------------------------------ */

static void test_resized(void)
{
    MPI_Datatype t = MPI_DATATYPE_NULL;
    int rc = MPI_Type_create_resized(MPI_INT, 0, 8, &t);
    test_verify("Type_create_resized succeeds", MPI_SUCCESS == rc);
    MPI_Type_commit(&t);

    MPI_Aint lb = -1, extent = -1;
    MPI_Type_get_extent(t, &lb, &extent);
    test_verify("resized lb is 0", 0 == lb);
    test_verify("resized extent is 8", 8 == extent);

    int size = -1;
    MPI_Type_size(t, &size);
    test_verify("resized size is unchanged (sizeof int)", (int) sizeof(int) == size);

    MPI_Type_free(&t);
}

/* ------------------------------------------------------------------ */

static void test_pack_unpack(void)
{
    int in[3] = {11, 22, 33};
    int packsize = -1;
    int rc = MPI_Pack_size(3, MPI_INT, MPI_COMM_SELF, &packsize);
    test_verify("Pack_size succeeds", MPI_SUCCESS == rc);
    test_verify("pack size is at least 3 ints", packsize >= (int) (3 * sizeof(int)));

    char buf[128];
    int pos = 0;
    rc = MPI_Pack(in, 3, MPI_INT, buf, sizeof(buf), &pos, MPI_COMM_SELF);
    test_verify("Pack succeeds", MPI_SUCCESS == rc);
    test_verify("pack advanced the position", pos > 0);

    int out[3] = {0, 0, 0};
    int pos2 = 0;
    rc = MPI_Unpack(buf, sizeof(buf), &pos2, out, 3, MPI_INT, MPI_COMM_SELF);
    test_verify("Unpack succeeds", MPI_SUCCESS == rc);
    test_verify("unpacked data matches", 11 == out[0] && 22 == out[1] && 33 == out[2]);
}

/* ------------------------------------------------------------------ */

static void test_pack_external(void)
{
    int in[3] = {7, 8, 9};
    MPI_Aint extsize = -1;
    int rc = MPI_Pack_external_size("external32", 3, MPI_INT, &extsize);
    test_verify("Pack_external_size succeeds", MPI_SUCCESS == rc);
    test_verify("external32 size is 3 * 4 bytes", 12 == extsize);

    char buf[128];
    MPI_Aint pos = 0;
    rc = MPI_Pack_external("external32", in, 3, MPI_INT, buf, sizeof(buf), &pos);
    test_verify("Pack_external succeeds", MPI_SUCCESS == rc);

    int out[3] = {0, 0, 0};
    MPI_Aint pos2 = 0;
    rc = MPI_Unpack_external("external32", buf, sizeof(buf), &pos2, out, 3, MPI_INT);
    test_verify("Unpack_external succeeds", MPI_SUCCESS == rc);
    test_verify("external32 round-trip matches", 7 == out[0] && 8 == out[1] && 9 == out[2]);
}

/* ------------------------------------------------------------------ */

/* MPI_Get_elements / MPI_Get_count need a status; obtain one from a
 * self MPI_Sendrecv on MPI_COMM_SELF (valid single-process). */
static void test_get_elements(void)
{
    int sbuf[6] = {1, 2, 3, 4, 5, 6};
    int rbuf[6] = {0};
    MPI_Status status;
    int rc = MPI_Sendrecv(sbuf, 6, MPI_INT, 0, 0,
                          rbuf, 6, MPI_INT, 0, 0, MPI_COMM_SELF, &status);
    test_verify("self Sendrecv succeeds", MPI_SUCCESS == rc);

    int count = -1;
    rc = MPI_Get_count(&status, MPI_INT, &count);
    test_verify("Get_count succeeds", MPI_SUCCESS == rc);
    test_verify("Get_count returns 6", 6 == count);

    int elems = -1;
    rc = MPI_Get_elements(&status, MPI_INT, &elems);
    test_verify("Get_elements succeeds", MPI_SUCCESS == rc);
    test_verify("Get_elements returns 6", 6 == elems);

    MPI_Count elems_x = -1;
    rc = MPI_Get_elements_x(&status, MPI_INT, &elems_x);
    test_verify("Get_elements_x succeeds", MPI_SUCCESS == rc);
    test_verify("Get_elements_x returns 6", 6 == elems_x);
}

/* ------------------------------------------------------------------ */

/*
 * Exercise the datatype serialization machinery in ompi_datatype_args.c
 * (get_pack_description -> create_from_packed_description, i.e. the
 * encode/decode path normally used to ship derived types between ranks).
 * This is driven single-process by packing a type's description and
 * reconstructing it locally against the local proc.
 */
static void roundtrip_pack(const char *what, MPI_Datatype t)
{
    ompi_datatype_t *dt = (ompi_datatype_t *) t;

    size_t len = ompi_datatype_pack_description_length(dt);
    test_verify("pack_description_length is non-zero", len > 0);

    const void *packed = NULL;
    int rc = ompi_datatype_get_pack_description(dt, &packed);
    test_verify(what, OMPI_SUCCESS == rc && NULL != packed);
    if (OMPI_SUCCESS != rc || NULL == packed) {
        return;
    }

    /* create_from_packed_description advances the buffer pointer, so work
     * on a mutable copy. */
    void *buf = malloc(len);
    memcpy(buf, packed, len);
    void *ptr = buf;

    ompi_datatype_t *recon =
        ompi_datatype_create_from_packed_description(&ptr, ompi_proc_local());
    test_verify("create_from_packed_description succeeds", NULL != recon);
    if (NULL != recon) {
        int sz_orig = -1, sz_recon = -1;
        MPI_Type_size(t, &sz_orig);
        MPI_Type_size((MPI_Datatype) recon, &sz_recon);
        test_verify("reconstructed type has the same size", sz_orig == sz_recon);

        MPI_Datatype mr = (MPI_Datatype) recon;
        MPI_Type_free(&mr);
    }
    free(buf);
}

static void test_pack_description(void)
{
    int blens[2] = {1, 2};
    int displs[2] = {0, 3};
    MPI_Aint hdispls[2] = {0, 3 * sizeof(int)};
    MPI_Datatype memb[2] = {MPI_INT, MPI_DOUBLE};

    MPI_Datatype t = MPI_DATATYPE_NULL;

    MPI_Type_contiguous(4, MPI_INT, &t); MPI_Type_commit(&t);
    roundtrip_pack("get_pack_description (contiguous) succeeds", t);
    MPI_Type_free(&t);

    MPI_Type_vector(2, 3, 5, MPI_DOUBLE, &t); MPI_Type_commit(&t);
    roundtrip_pack("get_pack_description (vector) succeeds", t);
    MPI_Type_free(&t);

    MPI_Type_indexed(2, blens, displs, MPI_INT, &t); MPI_Type_commit(&t);
    roundtrip_pack("get_pack_description (indexed) succeeds", t);
    MPI_Type_free(&t);

    MPI_Type_create_struct(2, blens, hdispls, memb, &t); MPI_Type_commit(&t);
    roundtrip_pack("get_pack_description (struct) succeeds", t);
    MPI_Type_free(&t);

    MPI_Type_create_hvector(2, 3, 20, MPI_INT, &t); MPI_Type_commit(&t);
    roundtrip_pack("get_pack_description (hvector) succeeds", t);
    MPI_Type_free(&t);

    MPI_Type_create_hindexed(2, blens, hdispls, MPI_INT, &t); MPI_Type_commit(&t);
    roundtrip_pack("get_pack_description (hindexed) succeeds", t);
    MPI_Type_free(&t);

    MPI_Type_create_indexed_block(2, 2, displs, MPI_INT, &t); MPI_Type_commit(&t);
    roundtrip_pack("get_pack_description (indexed_block) succeeds", t);
    MPI_Type_free(&t);

    MPI_Type_create_hindexed_block(2, 2, hdispls, MPI_INT, &t); MPI_Type_commit(&t);
    roundtrip_pack("get_pack_description (hindexed_block) succeeds", t);
    MPI_Type_free(&t);

    {
        int sizes[2] = {4, 4}, subsizes[2] = {2, 2}, starts[2] = {0, 0};
        MPI_Type_create_subarray(2, sizes, subsizes, starts, MPI_ORDER_C, MPI_INT, &t);
        MPI_Type_commit(&t);
        roundtrip_pack("get_pack_description (subarray) succeeds", t);
        MPI_Type_free(&t);
    }

    MPI_Type_create_resized(MPI_INT, 0, 16, &t); MPI_Type_commit(&t);
    roundtrip_pack("get_pack_description (resized) succeeds", t);
    MPI_Type_free(&t);

    {
        /* nested: a contiguous of a vector exercises recursive packing */
        MPI_Datatype base = MPI_DATATYPE_NULL;
        MPI_Type_vector(2, 2, 3, MPI_DOUBLE, &base); MPI_Type_commit(&base);
        MPI_Type_contiguous(3, base, &t); MPI_Type_commit(&t);
        roundtrip_pack("get_pack_description (nested) succeeds", t);
        MPI_Type_free(&t);
        MPI_Type_free(&base);
    }
}
