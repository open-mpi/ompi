/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sun Microsystems Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/datatype/ompi_datatype.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/proc/proc.h"

#include <stdlib.h>
#include <string.h>

#include <poll.h>

static int get_extents(ompi_datatype_t * type, OPAL_PTRDIFF_TYPE *lb, OPAL_PTRDIFF_TYPE *extent, OPAL_PTRDIFF_TYPE *true_lb, OPAL_PTRDIFF_TYPE *true_extent) {
    int ret;

    ret = ompi_datatype_get_extent(type, lb, extent);
    if (MPI_SUCCESS != ret) return ret;
    ret = ompi_datatype_get_true_extent(type, true_lb, true_extent);
    if (MPI_SUCCESS != ret) return ret;

    return 0;
}

int
main(int argc, char* argv[])
{
    size_t packed_ddt_len;
    const void *packed_ddt;
    void *payload, *ptr;
    struct ompi_datatype_t *unpacked_dt;
    int ret = 0;
    int         blen[4];
    OPAL_PTRDIFF_TYPE    disp[4];
    ompi_datatype_t *newType, *types[4], *struct_type, *vec_type;
    OPAL_PTRDIFF_TYPE    old_lb, old_extent, old_true_lb, old_true_extent;
    OPAL_PTRDIFF_TYPE    lb, extent, true_lb, true_extent;

    /* make ompi_proc_local () work ... */
    struct ompi_proc_t dummy_proc;
    ompi_proc_local_proc = &dummy_proc;


    int _dbg = 0;
    while (_dbg) poll(NULL, 0, 1);

    ompi_datatype_init();

    /**
     *
     *                 TEST 1
     *
     */

    /* Basic test... */
    printf("---> Basic test with MPI_INT\n");

    packed_ddt_len = ompi_datatype_pack_description_length(&ompi_mpi_int.dt);
    ptr = payload = malloc(packed_ddt_len);
    ret = ompi_datatype_get_pack_description(&ompi_mpi_int.dt, &packed_ddt);
    if (ret != 0) goto cleanup;

    memcpy(payload, packed_ddt, packed_ddt_len);
    unpacked_dt = ompi_datatype_create_from_packed_description(&payload,
                                                               ompi_proc_local());
    free(ptr);
    if (unpacked_dt == &ompi_mpi_int32_t.dt) {
        printf("\tPASSED\n");
    } else {
        printf("\tFAILED: datatypes don't match\n");
        ret = 1;
        goto cleanup;
    }

    /**
     *
     *                 TEST 2
     *
     */

    printf("---> Simple test using a struct and few predefined datatype (4 * MPI_INT).\n");
    blen[0] = 1;         blen[1] = 2;        blen[2] = 3;        blen[3] = 4;
    disp[0] = 0;         disp[1] = 4;        disp[2] = 8;        disp[3] = 12;
    types[0] = &ompi_mpi_int.dt; types[1] = &ompi_mpi_int.dt; types[2] = &ompi_mpi_int.dt; types[3] = &ompi_mpi_int.dt;
    ret = ompi_datatype_create_struct( 4, blen, disp, types, &struct_type );
    if (ret != 0) goto cleanup;

    {
        int count = 4;
        const int* a_i[2] = {&count, blen};
        ret = ompi_datatype_set_args( struct_type, count + 1, a_i, count, disp,
                                      count, types, MPI_COMBINER_STRUCT);
        if (ret != 0) goto cleanup;
    }

    ret = ompi_datatype_commit(&struct_type);
    if (ret != 0) goto cleanup;

    ret = get_extents(struct_type, &old_lb, &old_extent, &old_true_lb, &old_true_extent);
    if (ret != 0) goto cleanup;

    packed_ddt_len = ompi_datatype_pack_description_length(struct_type);
    ptr = payload = malloc(packed_ddt_len);
    ret = ompi_datatype_get_pack_description(struct_type, &packed_ddt);
    if (ret != 0) goto cleanup;

    memcpy(payload, packed_ddt, packed_ddt_len);

    unpacked_dt = ompi_datatype_create_from_packed_description(&payload, ompi_proc_local());
    free(ptr);
    if (unpacked_dt == NULL) {
        printf("\tFAILED: could not unpack datatype\n");
        ret = 1;
        goto cleanup;
    } else {
        ret = get_extents(unpacked_dt, &lb, &extent, &true_lb, &true_extent);
        if (ret != 0) goto cleanup;

        if (old_lb != lb || old_extent != extent ||
            old_true_lb != true_lb || old_true_extent != extent) {
            printf("\tFAILED: datatypes don't match\n");
            ret = 1;
            goto cleanup;
        }
        printf("\tPASSED\n");
    }
    ret = ompi_datatype_destroy(&struct_type);
    if (ret != 0) goto cleanup;

    ret = ompi_datatype_destroy(&unpacked_dt);
    if (ret != 0) goto cleanup;

    /**
     *
     *                 TEST 3
     *
     */

    printf("---> Less Basic test with MPI_Type_vector\n");

    ret = ompi_datatype_create_vector(2, 1, 1, &ompi_mpi_int.dt, &vec_type);
    if (ret != 0) goto cleanup;

    {
        int count = 2;
        int blocklength = 1;
        int stride = 1;
        const int* a_i[3] = {&count, &blocklength, &stride};
        ompi_datatype_t * type = &ompi_mpi_int.dt;
        ret = ompi_datatype_set_args(vec_type, 3, a_i, 0, NULL, 1, &type, MPI_COMBINER_VECTOR );
        if (ret != 0) goto cleanup;
    }

    ret = ompi_datatype_commit(&vec_type);
    if (ret != 0) goto cleanup;

    ret = get_extents(vec_type, &old_lb, &old_extent, &old_true_lb, &old_true_extent);
    if (ret != 0) goto cleanup;

    packed_ddt_len = ompi_datatype_pack_description_length(vec_type);
    ptr = payload = malloc(packed_ddt_len);
    ret = ompi_datatype_get_pack_description(vec_type, &packed_ddt);
    if (ret != 0) goto cleanup;

    memcpy(payload, packed_ddt, packed_ddt_len);

    unpacked_dt = ompi_datatype_create_from_packed_description(&payload,
                                                          ompi_proc_local());
    free(ptr);
    if (unpacked_dt == NULL) {
        printf("\tFAILED: could not unpack datatype\n");
        ret = 1;
        goto cleanup;
    } else {
        ret = get_extents(unpacked_dt, &lb, &extent, &true_lb, &true_extent);
        if (ret != 0) goto cleanup;

        if (old_lb != lb || old_extent != extent ||
            old_true_lb != true_lb || old_true_extent != extent) {
            printf("\tFAILED: datatypes don't match\n");
            ret = 1;
            goto cleanup;
        }
        printf("\tPASSED\n");
    }
    ret = ompi_datatype_destroy(&vec_type);
    if (ret != 0) goto cleanup;

    ret = ompi_datatype_destroy(&unpacked_dt);
    if (ret != 0) goto cleanup;

    /**
     *
     *                 TEST 4
     *
     */

    printf("---> Test with MPI_Type_create_indexed_block\n");

    blen[0] = 0;
    blen[1] = 20*sizeof(double);

    ret = ompi_datatype_create_indexed_block(2, 10, blen, &ompi_mpi_double.dt, &newType);
    if (ret != 0) goto cleanup;

    {
        int count = 2;
        int blocklength = 10;
        const int* a_i[3] = {&count, &blocklength, blen};
        ompi_datatype_t * oldtype = &ompi_mpi_double.dt;
        ompi_datatype_set_args( newType, 2 + count, a_i, 0, NULL, 1, &oldtype,
                                MPI_COMBINER_INDEXED_BLOCK );
        if (ret != 0) goto cleanup;
    }

    ret = ompi_datatype_commit(&newType);
    if (ret != 0) goto cleanup;

    ret = get_extents(newType, &old_lb, &old_extent, &old_true_lb, &old_true_extent);
    if (ret != 0) goto cleanup;

    packed_ddt_len = ompi_datatype_pack_description_length(newType);

    ptr = payload = malloc(packed_ddt_len);
    ret = ompi_datatype_get_pack_description(newType, &packed_ddt);
    if (ret != 0) goto cleanup;

    memcpy(payload, packed_ddt, packed_ddt_len);
    unpacked_dt = ompi_datatype_create_from_packed_description(&payload,
                                                               ompi_proc_local());
    free(ptr);
    if (unpacked_dt == NULL) {
        printf("\tFAILED: could not unpack datatype\n");
        ret = 1;
        goto cleanup;
    } else {
        ret = get_extents(unpacked_dt, &lb, &extent, &true_lb, &true_extent);
        if (ret != 0) goto cleanup;

        if (old_lb != lb || old_extent != extent ||
            old_true_lb != true_lb || old_true_extent != extent) {
            printf("\tFAILED: datatypes don't match\n");
            ret = 1;
            goto cleanup;
        }
        printf("\tPASSED\n");
    }
    ret = ompi_datatype_destroy(&newType);
    if (ret != 0) goto cleanup;

    ret = ompi_datatype_destroy(&unpacked_dt);
    if (ret != 0) goto cleanup;

    /**
     *
     *                 TEST 5
     *
     */

    printf("---> Advanced test with hindexed\n");

    blen[0] = 10;
    blen[1] = 10;
    disp[0] = 0;
    disp[1] = 20*sizeof(double);

    ret = ompi_datatype_create_hindexed(2, blen, disp, &ompi_mpi_double.dt, &newType);
    if (ret != 0) goto cleanup;

    {
        int count = 2;
        const int* a_i[2] = {&count, blen};
        ompi_datatype_t * oldtype = &ompi_mpi_double.dt;
        ret = ompi_datatype_set_args( newType, count + 1, a_i, count, disp,
                                      1, &oldtype, MPI_COMBINER_HINDEXED );
        if (ret != 0) goto cleanup;
    }

    ret = ompi_datatype_commit(&newType);
    if (ret != 0) goto cleanup;

    ret = get_extents(newType, &old_lb, &old_extent, &old_true_lb, &old_true_extent);
    if (ret != 0) goto cleanup;

    packed_ddt_len = ompi_datatype_pack_description_length(newType);
    ptr = payload = malloc(packed_ddt_len);
    ret = ompi_datatype_get_pack_description(newType, &packed_ddt);
    if (ret != 0) goto cleanup;
    memcpy(payload, packed_ddt, packed_ddt_len);
    unpacked_dt = ompi_datatype_create_from_packed_description(&payload,
                                                          ompi_proc_local());
    free(ptr);
    if (unpacked_dt == NULL) {
        printf("\tFAILED: could not unpack datatype\n");
        ret = 1;
        goto cleanup;
    } else {
        ret = get_extents(unpacked_dt, &lb, &extent, &true_lb, &true_extent);
        if (ret != 0) goto cleanup;

        if (old_lb != lb || old_extent != extent ||
            old_true_lb != true_lb || old_true_extent != extent) {
            printf("\tFAILED: datatypes don't match\n");
            ret = 1;
            goto cleanup;
        }
        printf("\tPASSED\n");
    }
    ret = ompi_datatype_destroy(&newType);
    if (ret != 0) goto cleanup;

    newType = unpacked_dt;  /* save it for later */

    /**
     *
     *                 TEST 6
     *
     */

    printf("---> Even more advanced test using the previous type and struct\n");
    blen[0] = 11;
    blen[1] = 2;
    disp[0] = 0;
    disp[1] = 64;
    types[0] = &ompi_mpi_int.dt;
    types[1] = newType;
    ret = ompi_datatype_create_struct( 2, blen, disp, types, &struct_type );
    if (ret != 0) goto cleanup;

    {
        int count = 2;
        const int* a_i[2] = {&count, blen};
        ret = ompi_datatype_set_args( struct_type, count + 1, a_i, count, disp,
                                      count, types, MPI_COMBINER_STRUCT );
        if (ret != 0) goto cleanup;
    }

    ret = ompi_datatype_commit(&struct_type);
    if (ret != 0) goto cleanup;

    ret = ompi_datatype_destroy(&newType);
    if (ret != 0) goto cleanup;

    ret = get_extents(struct_type, &old_lb, &old_extent, &old_true_lb, &old_true_extent);
    if (ret != 0) goto cleanup;

    packed_ddt_len = ompi_datatype_pack_description_length(struct_type);
    ptr = payload = malloc(packed_ddt_len);
    ret = ompi_datatype_get_pack_description(struct_type, &packed_ddt);
    if (ret != 0) goto cleanup;
    memcpy(payload, packed_ddt, packed_ddt_len);

    unpacked_dt = ompi_datatype_create_from_packed_description(&payload,
                                                          ompi_proc_local());
    free(ptr);
    if (unpacked_dt == NULL) {
        printf("\tFAILED: could not unpack datatype\n");
        ret = 1;
        goto cleanup;
    } else {
        ret = get_extents(unpacked_dt, &lb, &extent, &true_lb, &true_extent);
        if (ret != 0) goto cleanup;

        if (old_lb != lb || old_extent != extent ||
            old_true_lb != true_lb || old_true_extent != extent) {
            printf("\tFAILED: datatypes don't match\n");
            ret = 1;
            goto cleanup;
        }
        printf("\tPASSED\n");
    }
    ret = ompi_datatype_destroy(&struct_type);
    if (ret != 0) goto cleanup;

    ret = ompi_datatype_destroy(&unpacked_dt);
    if (ret != 0) goto cleanup;


 cleanup:
    ompi_datatype_finalize();

    return ret;
}
