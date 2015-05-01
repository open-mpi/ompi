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

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

static int get_extents(MPI_Datatype type, MPI_Aint *lb, MPI_Aint *extent, MPI_Aint *true_lb, MPI_Aint *true_extent) {
    int ret;

    ret = MPI_Type_get_extent(type, lb, extent);
    if (MPI_SUCCESS != ret) return ret;
    ret = MPI_Type_get_true_extent(type, true_lb, true_extent);
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
    MPI_Aint    disp[4];
    MPI_Datatype newType, types[4], struct_type, vec_type;
    MPI_Aint    old_lb, old_extent, old_true_lb, old_true_extent;
    MPI_Aint    lb, extent, true_lb, true_extent;

    MPI_Init(&argc, &argv);

    /**
     *
     *                 TEST 1
     *
     */

    /* Basic test... */
    printf("---> Basic test with MPI_INT\n");

    packed_ddt_len = ompi_datatype_pack_description_length(MPI_INT);
    ptr = payload = malloc(packed_ddt_len);
    ret = ompi_datatype_get_pack_description(MPI_INT, &packed_ddt);
    if (ret != 0) goto cleanup;

    memcpy(payload, packed_ddt, packed_ddt_len);
    unpacked_dt = ompi_datatype_create_from_packed_description(&payload,
                                                               ompi_proc_local());
    free(ptr);
    if (unpacked_dt == MPI_INT32_T) {
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
    types[0] = MPI_INT; types[1] = MPI_INT; types[2] = MPI_INT; types[3] = MPI_INT;
    ret = MPI_Type_create_struct( 4, blen, disp, types, &struct_type );
    if (ret != 0) goto cleanup;

    ret = MPI_Type_commit(&struct_type);
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
    ret = MPI_Type_free(&struct_type);
    if (ret != 0) goto cleanup;

    ret = MPI_Type_free(&unpacked_dt);
    if (ret != 0) goto cleanup;

    /**
     *
     *                 TEST 3
     *
     */

    printf("---> Less Basic test with MPI_Type_vector\n");

    ret = MPI_Type_vector(2, 1, 1, MPI_INT, &vec_type);
    if (ret != 0) goto cleanup;

    ret = MPI_Type_commit(&vec_type);
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
    ret = MPI_Type_free(&vec_type);
    if (ret != 0) goto cleanup;

    ret = MPI_Type_free(&unpacked_dt);
    if (ret != 0) goto cleanup;

    /**
     *
     *                 TEST 4
     *
     */

    printf("---> Test with MPI_Type_create_indexed_block\n");

    blen[0] = 0;
    blen[1] = 20*sizeof(double);

    ret = MPI_Type_create_indexed_block(2, 10, blen, MPI_DOUBLE, &newType);
    if (ret != 0) goto cleanup;

    ret = MPI_Type_commit(&newType);
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
    ret = MPI_Type_free(&newType);
    if (ret != 0) goto cleanup;

    ret = MPI_Type_free(&unpacked_dt);
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

    ret = MPI_Type_create_hindexed(2, blen, disp, MPI_DOUBLE, &newType);
    if (ret != 0) goto cleanup;

    ret = MPI_Type_commit(&newType);
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
    ret = MPI_Type_free(&newType);
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
    types[0] = MPI_INT;
    types[1] = newType;
    ret = MPI_Type_create_struct( 2, blen, disp, types, &struct_type );
    if (ret != 0) goto cleanup;

    ret = MPI_Type_commit(&struct_type);
    if (ret != 0) goto cleanup;

    ret = MPI_Type_free(&newType);
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
    ret = MPI_Type_free(&struct_type);
    if (ret != 0) goto cleanup;

    ret = MPI_Type_free(&unpacked_dt);
    if (ret != 0) goto cleanup;


 cleanup:
    MPI_Finalize();

    return ret;
}
