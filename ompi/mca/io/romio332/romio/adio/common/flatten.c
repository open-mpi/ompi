/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *   Copyright (C) 1997 University of Chicago.
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"

#ifdef MPL_USE_DBG_LOGGING
#define FLATTEN_DEBUG 1
#endif

static ADIOI_Flatlist_node *flatlist_node_new(MPI_Datatype datatype, MPI_Count count)
{
    ADIOI_Flatlist_node *flat;
    flat = ADIOI_Malloc(sizeof(ADIOI_Flatlist_node));

    flat->type = datatype;
    flat->blocklens = NULL;
    flat->indices = NULL;
    flat->lb_idx = flat->ub_idx = -1;
    flat->refct = 1;
    flat->count = count;
    flat->flag = 0;

    flat->blocklens = (ADIO_Offset *) ADIOI_Calloc(flat->count * 2, sizeof(ADIO_Offset));
    flat->indices = flat->blocklens + flat->count;
    return flat;
}

void ADIOI_Optimize_flattened(ADIOI_Flatlist_node * flat_type);
/* flatten datatype and add it to Flatlist */
ADIOI_Flatlist_node *ADIOI_Flatten_datatype(MPI_Datatype datatype)
{
#ifdef HAVE_MPIR_TYPE_FLATTEN
    MPI_Aint flatten_idx;
#endif
    MPI_Count flat_count, curr_index = 0;
    int is_contig, flag;
    ADIOI_Flatlist_node *flat;

    if (ADIOI_Flattened_type_keyval == MPI_KEYVAL_INVALID) {
        /* ADIOI_End_call will take care of cleanup */
        MPI_Type_create_keyval(ADIOI_Flattened_type_copy,
                               ADIOI_Flattened_type_delete, &ADIOI_Flattened_type_keyval, NULL);
    }

    /* check if necessary to flatten. */

    /* has it already been flattened? */
    MPI_Type_get_attr(datatype, ADIOI_Flattened_type_keyval, &flat, &flag);
    if (flag) {
#ifdef FLATTEN_DEBUG
        DBG_FPRINTF(stderr, "ADIOI_Flatten_datatype:: found datatype %#X\n", datatype);
#endif
        return flat;
    }

    /* is it entirely contiguous? */
    ADIOI_Datatype_iscontig(datatype, &is_contig);

#ifdef FLATTEN_DEBUG
    DBG_FPRINTF(stderr, "ADIOI_Flatten_datatype:: is_contig %#X\n", is_contig);
#endif
    /* it would be great if ADIOI_Count_contiguous_blocks and the rest of the
     * flattening code operated on the built-in named types, but
     * it recursively processes types, stopping when it hits a named type. So
     * we will do the little bit of work that named types require right here,
     * and avoid touching the scary flattening code. */

    if (is_contig)
        flat_count = 1;
    else {
        flat_count = ADIOI_Count_contiguous_blocks(datatype, &curr_index);
    }
    /* flatten and add to datatype */
    flat = flatlist_node_new(datatype, flat_count);
    if (is_contig) {
        MPI_Type_size_x(datatype, &(flat->blocklens[0]));
        flat->indices[0] = 0;
    } else {

        curr_index = 0;
#ifdef HAVE_MPIR_TYPE_FLATTEN
        flatten_idx = (MPI_Aint) flat->count;
        MPIR_Type_flatten(datatype, flat->indices, flat->blocklens, &flatten_idx);
#ifdef FLATTEN_DEBUG
        DBG_FPRINTF(stderr, "ADIOI_Flatten_datatype:: MPIR_Type_flatten\n");
#endif
#else
        ADIOI_Flatten(datatype, flat, 0, &curr_index);
#ifdef FLATTEN_DEBUG
        DBG_FPRINTF(stderr, "ADIOI_Flatten_datatype:: ADIOI_Flatten\n");
#endif

        ADIOI_Optimize_flattened(flat);
#endif
/* debug */
#ifdef FLATTEN_DEBUG
        {
            int i;
            for (i = 0; i < flat->count; i++)
                DBG_FPRINTF(stderr,
                            "ADIOI_Flatten_datatype:: i %#X, blocklens %#llX, indices %#llX\n", i,
                            flat->blocklens[i], flat->indices[i]
);
        }
#endif
    }
    MPI_Type_set_attr(datatype, ADIOI_Flattened_type_keyval, flat);
    return flat;

}

/* ADIOI_Flatten()
 *
 * Assumption: input datatype is not a basic!!!!
 */
void ADIOI_Flatten(MPI_Datatype datatype, ADIOI_Flatlist_node * flat,
                   ADIO_Offset st_offset, MPI_Count * curr_index)
{
    int k, m, n, is_hindexed_block = 0;
    int lb_updated = 0;
    int combiner, old_combiner, old_is_contig;
    int nints, nadds, ntypes, old_nints, old_nadds, old_ntypes;
    /* By using ADIO_Offset we preserve +/- sign and
     * avoid >2G integer arithmetic problems */
    ADIO_Offset top_count;
    MPI_Count i, j, old_size, prev_index, basic_num, num, nonzeroth;
    MPI_Aint old_extent;        /* Assume extents are non-negative */
    int *ints;
    MPI_Aint *adds;             /* Make no assumptions about +/- sign on these */
    MPI_Datatype *types;
    MPI_Type_get_envelope(datatype, &nints, &nadds, &ntypes, &combiner);
    ints = (int *) ADIOI_Malloc((nints + 1) * sizeof(int));
    adds = (MPI_Aint *) ADIOI_Malloc((nadds + 1) * sizeof(MPI_Aint));
    types = (MPI_Datatype *) ADIOI_Malloc((ntypes + 1) * sizeof(MPI_Datatype));
    MPI_Type_get_contents(datatype, nints, nadds, ntypes, ints, adds, types);

#ifdef FLATTEN_DEBUG
    DBG_FPRINTF(stderr, "ADIOI_Flatten:: st_offset %#llX, curr_index %#llX\n", st_offset,
                *curr_index);
    DBG_FPRINTF(stderr, "ADIOI_Flatten:: nints %#X, nadds %#X, ntypes %#X\n", nints, nadds, ntypes);
    for (i = 0; i < nints; ++i) {
        DBG_FPRINTF(stderr, "ADIOI_Flatten:: ints[%lld]=%#X\n", i, ints[i]);
    }
    for (i = 0; i < nadds; ++i) {
        DBG_FPRINTF(stderr, "ADIOI_Flatten:: adds[%lld]=" MPI_AINT_FMT_HEX_SPEC "\n", i, adds[i]);
    }
    for (i = 0; i < ntypes; ++i) {
        DBG_FPRINTF(stderr, "ADIOI_Flatten:: types[%lld]=%#llX\n", i,
                    (unsigned long long) (unsigned long) types[i]);
    }
#endif
    /* Chapter 4, page 83: when processing datatypes, note this item from the
     * standard:
     Most datatype constructors have replication count or block length
     arguments.  Allowed values are non-negative integers. If the value is
     zero, no elements are generated in the type map and there is no effect
     on datatype bounds or extent.  */

    switch (combiner) {
#ifdef MPIIMPL_HAVE_MPI_COMBINER_DUP
        case MPI_COMBINER_DUP:
#ifdef FLATTEN_DEBUG
            DBG_FPRINTF(stderr, "ADIOI_Flatten:: MPI_COMBINER_DUP\n");
#endif
            MPI_Type_get_envelope(types[0], &old_nints, &old_nadds, &old_ntypes, &old_combiner);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);
            if ((old_combiner != MPI_COMBINER_NAMED) && (!old_is_contig))
                ADIOI_Flatten(types[0], flat, st_offset, curr_index);
            break;
#endif
#ifdef MPIIMPL_HAVE_MPI_COMBINER_SUBARRAY
        case MPI_COMBINER_SUBARRAY:
            {
                int dims = ints[0];
                MPI_Datatype stype;
#ifdef FLATTEN_DEBUG
                DBG_FPRINTF(stderr, "ADIOI_Flatten:: MPI_COMBINER_SUBARRAY\n");
#endif

                ADIO_Type_create_subarray(dims, &ints[1],       /* sizes */
                                          &ints[dims + 1],      /* subsizes */
                                          &ints[2 * dims + 1],  /* starts */
                                          ints[3 * dims + 1],   /* order */
                                          types[0],     /* type */
                                          &stype);
                ADIOI_Flatten(stype, flat, st_offset, curr_index);
                MPI_Type_free(&stype);
            }
            break;
#endif
#ifdef MPIIMPL_HAVE_MPI_COMBINER_DARRAY
        case MPI_COMBINER_DARRAY:
            {
                int dims = ints[2];
                MPI_Datatype dtype;
#ifdef FLATTEN_DEBUG
                DBG_FPRINTF(stderr, "ADIOI_Flatten:: MPI_COMBINER_DARRAY\n");
#endif

                ADIO_Type_create_darray(ints[0],        /* size */
                                        ints[1],        /* rank */
                                        dims, &ints[3], /* gsizes */
                                        &ints[dims + 3],        /* distribs */
                                        &ints[2 * dims + 3],    /* dargs */
                                        &ints[3 * dims + 3],    /* psizes */
                                        ints[4 * dims + 3],     /* order */
                                        types[0], &dtype);
#ifdef FLATTEN_DEBUG
                DBG_FPRINTF(stderr,
                            "ADIOI_Flatten:: MPI_COMBINER_DARRAY <ADIOI_Flatten(dtype, flat->indices[%#X] %#llX, flat->blocklens[%#X] %#llX, st_offset %#llX, curr_index %#llX);\n",
                            0, flat->indices[0], 0, flat->blocklens[0], st_offset, *curr_index);
#endif
                ADIOI_Flatten(dtype, flat, st_offset, curr_index);
#ifdef FLATTEN_DEBUG
                DBG_FPRINTF(stderr,
                            "ADIOI_Flatten:: MPI_COMBINER_DARRAY >ADIOI_Flatten(dtype, flat->indices[%#X] %#llX, flat->blocklens[%#X] %#llX, st_offset %#llX, curr_index %#llX);\n",
                            0, flat->indices[0], 0, flat->blocklens[0], st_offset, *curr_index);
#endif
                MPI_Type_free(&dtype);
            }
            break;
#endif
        case MPI_COMBINER_CONTIGUOUS:
#ifdef FLATTEN_DEBUG
            DBG_FPRINTF(stderr, "ADIOI_Flatten:: MPI_COMBINER_CONTIGUOUS\n");
#endif
            top_count = ints[0];
            MPI_Type_get_envelope(types[0], &old_nints, &old_nadds, &old_ntypes, &old_combiner);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            prev_index = *curr_index;
            if ((old_combiner != MPI_COMBINER_NAMED) && (!old_is_contig))
                ADIOI_Flatten(types[0], flat, st_offset, curr_index);

            if (prev_index == *curr_index) {
/* simplest case, made up of basic or contiguous types */
                j = *curr_index;
                flat->indices[j] = st_offset;
                MPI_Type_size_x(types[0], &old_size);
                flat->blocklens[j] = top_count * old_size;
#ifdef FLATTEN_DEBUG
                DBG_FPRINTF(stderr,
                            "ADIOI_Flatten:: simple flat->indices[%#llX] %#llX, flat->blocklens[%#llX] %#llX\n",
                            j, flat->indices[j], j, flat->blocklens[j]);
#endif
                (*curr_index)++;
            } else {
/* made up of noncontiguous derived types */
                j = *curr_index;
                num = *curr_index - prev_index;

/* The noncontiguous types have to be replicated count times */
                MPI_Type_extent(types[0], &old_extent);
                for (m = 1; m < top_count; m++) {
                    for (i = 0; i < num; i++) {
                        flat->indices[j] =
                            flat->indices[j - num] + ADIOI_AINT_CAST_TO_OFFSET old_extent;
                        flat->blocklens[j] = flat->blocklens[j - num];
#ifdef FLATTEN_DEBUG
                        DBG_FPRINTF(stderr,
                                    "ADIOI_Flatten:: derived flat->indices[%#llX] %#llX, flat->blocklens[%#llX] %#llX\n",
                                    j, flat->indices[j], j, flat->blocklens[j]);
#endif
                        j++;
                    }
                }
                *curr_index = j;
            }
            break;

        case MPI_COMBINER_VECTOR:
#ifdef FLATTEN_DEBUG
            DBG_FPRINTF(stderr, "ADIOI_Flatten:: MPI_COMBINER_VECTOR\n");
#endif
            top_count = ints[0];
            MPI_Type_get_envelope(types[0], &old_nints, &old_nadds, &old_ntypes, &old_combiner);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            prev_index = *curr_index;
            if ((old_combiner != MPI_COMBINER_NAMED) && (!old_is_contig))
                ADIOI_Flatten(types[0], flat, st_offset, curr_index);

            if (prev_index == *curr_index) {
/* simplest case, vector of basic or contiguous types */
                /* By using ADIO_Offset we preserve +/- sign and
                 * avoid >2G integer arithmetic problems */
                ADIO_Offset blocklength = ints[1], stride = ints[2];
                j = *curr_index;
                flat->indices[j] = st_offset;
                MPI_Type_size_x(types[0], &old_size);
                flat->blocklens[j] = blocklength * old_size;
                for (i = j + 1; i < j + top_count; i++) {
                    flat->indices[i] = flat->indices[i - 1] + stride * old_size;
                    flat->blocklens[i] = flat->blocklens[j];
                }
                *curr_index = i;
            } else {
/* vector of noncontiguous derived types */
                /* By using ADIO_Offset we preserve +/- sign and
                 * avoid >2G integer arithmetic problems */
                ADIO_Offset blocklength = ints[1], stride = ints[2];

                j = *curr_index;
                num = *curr_index - prev_index;

/* The noncontiguous types have to be replicated blocklen times
   and then strided. Replicate the first one. */
                MPI_Type_extent(types[0], &old_extent);
                for (m = 1; m < blocklength; m++) {
                    for (i = 0; i < num; i++) {
                        flat->indices[j] =
                            flat->indices[j - num] + ADIOI_AINT_CAST_TO_OFFSET old_extent;
                        flat->blocklens[j] = flat->blocklens[j - num];
                        j++;
                    }
                }
                *curr_index = j;

/* Now repeat with strides. */
                num = *curr_index - prev_index;
                for (i = 1; i < top_count; i++) {
                    for (m = 0; m < num; m++) {
                        flat->indices[j] =
                            flat->indices[j - num] + stride * ADIOI_AINT_CAST_TO_OFFSET old_extent;
                        flat->blocklens[j] = flat->blocklens[j - num];
                        j++;
                    }
                }
                *curr_index = j;
            }
            break;

        case MPI_COMBINER_HVECTOR:
        case MPI_COMBINER_HVECTOR_INTEGER:
#ifdef FLATTEN_DEBUG
            DBG_FPRINTF(stderr, "ADIOI_Flatten:: MPI_COMBINER_HVECTOR_INTEGER\n");
#endif
            top_count = ints[0];
            MPI_Type_get_envelope(types[0], &old_nints, &old_nadds, &old_ntypes, &old_combiner);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            prev_index = *curr_index;
            if ((old_combiner != MPI_COMBINER_NAMED) && (!old_is_contig))
                ADIOI_Flatten(types[0], flat, st_offset, curr_index);

            if (prev_index == *curr_index) {
/* simplest case, vector of basic or contiguous types */
                /* By using ADIO_Offset we preserve +/- sign and
                 * avoid >2G integer arithmetic problems */
                ADIO_Offset blocklength = ints[1];
                j = *curr_index;
                flat->indices[j] = st_offset;
                MPI_Type_size_x(types[0], &old_size);
                flat->blocklens[j] = blocklength * old_size;
                for (i = j + 1; i < j + top_count; i++) {
                    flat->indices[i] = flat->indices[i - 1] + adds[0];
                    flat->blocklens[i] = flat->blocklens[j];
                }
                *curr_index = i;
            } else {
/* vector of noncontiguous derived types */
                /* By using ADIO_Offset we preserve +/- sign and
                 * avoid >2G integer arithmetic problems */
                ADIO_Offset blocklength = ints[1];

                j = *curr_index;
                num = *curr_index - prev_index;

/* The noncontiguous types have to be replicated blocklen times
   and then strided. Replicate the first one. */
                MPI_Type_extent(types[0], &old_extent);
                for (m = 1; m < blocklength; m++) {
                    for (i = 0; i < num; i++) {
                        flat->indices[j] =
                            flat->indices[j - num] + ADIOI_AINT_CAST_TO_OFFSET old_extent;
                        flat->blocklens[j] = flat->blocklens[j - num];
                        j++;
                    }
                }
                *curr_index = j;

/* Now repeat with strides. */
                num = *curr_index - prev_index;
                for (i = 1; i < top_count; i++) {
                    for (m = 0; m < num; m++) {
                        flat->indices[j] = flat->indices[j - num] + adds[0];
                        flat->blocklens[j] = flat->blocklens[j - num];
                        j++;
                    }
                }
                *curr_index = j;
            }
            break;

        case MPI_COMBINER_INDEXED:
#ifdef FLATTEN_DEBUG
            DBG_FPRINTF(stderr, "ADIOI_Flatten:: MPI_COMBINER_INDEXED\n");
#endif
            top_count = ints[0];
            MPI_Type_get_envelope(types[0], &old_nints, &old_nadds, &old_ntypes, &old_combiner);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);
            MPI_Type_extent(types[0], &old_extent);

            prev_index = *curr_index;
            if ((old_combiner != MPI_COMBINER_NAMED) && (!old_is_contig)) {
                /* By using ADIO_Offset we preserve +/- sign and
                 * avoid >2G integer arithmetic problems */
                ADIO_Offset stride = ints[top_count + 1];
                ADIOI_Flatten(types[0], flat,
                              st_offset + stride * ADIOI_AINT_CAST_TO_OFFSET old_extent,
                              curr_index);
            }

            if (prev_index == *curr_index) {
/* simplest case, indexed type made up of basic or contiguous types */
                j = *curr_index;
                for (i = j, nonzeroth = i; i < j + top_count; i++) {
                    /* By using ADIO_Offset we preserve +/- sign and
                     * avoid >2G integer arithmetic problems */
                    ADIO_Offset blocklength = ints[1 + i - j], stride = ints[top_count + 1 + i - j];
                    if (blocklength > 0) {
                        flat->indices[nonzeroth] =
                            st_offset + stride * ADIOI_AINT_CAST_TO_OFFSET old_extent;
                        flat->blocklens[nonzeroth] =
                            blocklength * ADIOI_AINT_CAST_TO_OFFSET old_extent;
                        nonzeroth++;
                    } else {
                        flat->count--;  /* don't count/consider any zero-length blocklens */
                    }
                }
                *curr_index = i;
            } else {
/* indexed type made up of noncontiguous derived types */

                j = *curr_index;
                num = *curr_index - prev_index;
                basic_num = num;

/* The noncontiguous types have to be replicated blocklens[i] times
   and then strided. Replicate the first one. */
                for (m = 1; m < ints[1]; m++) {
                    for (i = 0, nonzeroth = j; i < num; i++) {
                        if (flat->blocklens[j - num] > 0) {
                            flat->indices[nonzeroth] =
                                flat->indices[nonzeroth - num] +
                                ADIOI_AINT_CAST_TO_OFFSET old_extent;
                            flat->blocklens[nonzeroth] = flat->blocklens[nonzeroth - num];
                            j++;
                            nonzeroth++;
                        } else {
                            flat->count--;
                        }
                    }
                }
                *curr_index = j;

/* Now repeat with strides. */
                for (i = 1; i < top_count; i++) {
                    num = *curr_index - prev_index;
                    prev_index = *curr_index;
                    for (m = 0, nonzeroth = j; m < basic_num; m++) {
                        /* By using ADIO_Offset we preserve +/- sign and
                         * avoid >2G integer arithmetic problems */
                        ADIO_Offset stride = ints[top_count + 1 + i] - ints[top_count + i];
                        if (flat->blocklens[j - num] > 0) {
                            flat->indices[nonzeroth] =
                                flat->indices[j - num] +
                                stride * ADIOI_AINT_CAST_TO_OFFSET old_extent;
                            flat->blocklens[nonzeroth] = flat->blocklens[j - num];
                            j++;
                            nonzeroth++;
                        } else {
                            flat->count--;
                        }
                    }
                    *curr_index = j;
                    for (m = 1; m < ints[1 + i]; m++) {
                        for (k = 0, nonzeroth = j; k < basic_num; k++) {
                            if (flat->blocklens[j - basic_num] > 0) {
                                flat->indices[nonzeroth] =
                                    flat->indices[j - basic_num] +
                                    ADIOI_AINT_CAST_TO_OFFSET old_extent;
                                flat->blocklens[nonzeroth] = flat->blocklens[j - basic_num];
                                j++;
                                nonzeroth++;
                            } else {
                                flat->count--;
                            }
                        }
                    }
                    *curr_index = j;
                }
            }
            break;

#if defined HAVE_DECL_MPI_COMBINER_HINDEXED_BLOCK && HAVE_DECL_MPI_COMBINER_HINDEXED_BLOCK
        case MPI_COMBINER_HINDEXED_BLOCK:
            is_hindexed_block = 1;
            /* deliberate fall-through */
            MPL_FALLTHROUGH;
#endif
        case MPI_COMBINER_INDEXED_BLOCK:
#ifdef FLATTEN_DEBUG
            DBG_FPRINTF(stderr, "ADIOI_Flatten:: MPI_COMBINER_INDEXED_BLOCK\n");
#endif
            top_count = ints[0];
            MPI_Type_get_envelope(types[0], &old_nints, &old_nadds, &old_ntypes, &old_combiner);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);
            MPI_Type_extent(types[0], &old_extent);

            prev_index = *curr_index;
            if ((old_combiner != MPI_COMBINER_NAMED) && (!old_is_contig)) {
                /* By using ADIO_Offset we preserve +/- sign and
                 * avoid >2G integer arithmetic problems */
                ADIO_Offset stride = ints[1 + 1];
                if (is_hindexed_block) {
                    ADIOI_Flatten(types[0], flat, st_offset + adds[0], curr_index);
                } else {
                    ADIOI_Flatten(types[0], flat,
                                  st_offset + stride * ADIOI_AINT_CAST_TO_OFFSET old_extent,
                                  curr_index);
                }
            }

            if (prev_index == *curr_index) {
/* simplest case, indexed type made up of basic or contiguous types */
                j = *curr_index;
                for (i = j; i < j + top_count; i++) {
                    /* By using ADIO_Offset we preserve +/- sign and
                     * avoid >2G integer arithmetic problems */
                    ADIO_Offset blocklength = ints[1];
                    if (is_hindexed_block) {
                        flat->indices[i] = st_offset + adds[i - j];
                    } else {
                        ADIO_Offset stride = ints[1 + 1 + i - j];
                        flat->indices[i] = st_offset +
                            stride * ADIOI_AINT_CAST_TO_OFFSET old_extent;
                    }
                    flat->blocklens[i] = blocklength * ADIOI_AINT_CAST_TO_OFFSET old_extent;
                }
                *curr_index = i;
            } else {
/* vector of noncontiguous derived types */

                j = *curr_index;
                num = *curr_index - prev_index;

/* The noncontiguous types have to be replicated blocklens[i] times
   and then strided. Replicate the first one. */
                for (m = 1; m < ints[1]; m++) {
                    for (i = 0; i < num; i++) {
                        if (is_hindexed_block) {
                            /* this is the one place the hindexed case uses the
                             * extent of a type */
                            MPI_Type_extent(types[0], &old_extent);
                        }
                        flat->indices[j] = flat->indices[j - num] +
                            ADIOI_AINT_CAST_TO_OFFSET old_extent;
                        flat->blocklens[j] = flat->blocklens[j - num];
                        j++;
                    }
                }
                *curr_index = j;

/* Now repeat with strides. */
                num = *curr_index - prev_index;
                for (i = 1; i < top_count; i++) {
                    for (m = 0; m < num; m++) {
                        if (is_hindexed_block) {
                            flat->indices[j] = flat->indices[j - num] + adds[i] - adds[i - 1];
                        } else {
                            /* By using ADIO_Offset we preserve +/- sign and
                             * avoid >2G integer arithmetic problems */
                            ADIO_Offset stride = ints[2 + i] - ints[1 + i];
                            flat->indices[j] = flat->indices[j - num] +
                                stride * ADIOI_AINT_CAST_TO_OFFSET old_extent;
                        }
                        flat->blocklens[j] = flat->blocklens[j - num];
                        j++;
                    }
                }
                *curr_index = j;
            }
            break;

        case MPI_COMBINER_HINDEXED:
        case MPI_COMBINER_HINDEXED_INTEGER:
#ifdef FLATTEN_DEBUG
            DBG_FPRINTF(stderr, "ADIOI_Flatten:: MPI_COMBINER_HINDEXED_INTEGER\n");
#endif
            top_count = ints[0];
            MPI_Type_get_envelope(types[0], &old_nints, &old_nadds, &old_ntypes, &old_combiner);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            prev_index = *curr_index;
            if ((old_combiner != MPI_COMBINER_NAMED) && (!old_is_contig)) {
                ADIOI_Flatten(types[0], flat, st_offset + adds[0], curr_index);
            }

            if (prev_index == *curr_index) {
/* simplest case, indexed type made up of basic or contiguous types */
                j = *curr_index;
                MPI_Type_size_x(types[0], &old_size);
                for (i = j, nonzeroth = j; i < j + top_count; i++) {
                    if (ints[1 + i - j] > 0) {
                        /* By using ADIO_Offset we preserve +/- sign and
                         * avoid >2G integer arithmetic problems */
                        ADIO_Offset blocklength = ints[1 + i - j];
                        flat->indices[nonzeroth] = st_offset + adds[i - j];
                        flat->blocklens[nonzeroth] = blocklength * old_size;
                        nonzeroth++;
                    } else {
                        flat->count--;
                    }
                }
                *curr_index = i;
            } else {
/* indexed type made up of noncontiguous derived types */

                j = *curr_index;
                num = *curr_index - prev_index;
                basic_num = num;

/* The noncontiguous types have to be replicated blocklens[i] times
   and then strided. Replicate the first one. */
                MPI_Type_extent(types[0], &old_extent);
                for (m = 1; m < ints[1]; m++) {
                    for (i = 0, nonzeroth = j; i < num; i++) {
                        if (flat->blocklens[j - num] > 0) {
                            flat->indices[nonzeroth] =
                                flat->indices[j - num] + ADIOI_AINT_CAST_TO_OFFSET old_extent;
                            flat->blocklens[nonzeroth] = flat->blocklens[j - num];
                            j++;
                            nonzeroth++;
                        } else {
                            flat->count--;
                        }
                    }
                }
                *curr_index = j;

/* Now repeat with strides. */
                for (i = 1; i < top_count; i++) {
                    num = *curr_index - prev_index;
                    prev_index = *curr_index;
                    for (m = 0, nonzeroth = j; m < basic_num; m++) {
                        if (flat->blocklens[j - num] > 0) {
                            flat->indices[nonzeroth] =
                                flat->indices[j - num] + adds[i] - adds[i - 1];
                            flat->blocklens[nonzeroth] = flat->blocklens[j - num];
                            j++;
                            nonzeroth++;
                        } else {
                            flat->count--;
                        }
                    }
                    *curr_index = j;
                    for (m = 1; m < ints[1 + i]; m++) {
                        for (k = 0, nonzeroth = j; k < basic_num; k++) {
                            if (flat->blocklens[j - basic_num] > 0) {
                                flat->indices[nonzeroth] =
                                    flat->indices[j - basic_num] +
                                    ADIOI_AINT_CAST_TO_OFFSET old_extent;
                                flat->blocklens[nonzeroth] = flat->blocklens[j - basic_num];
                                j++;
                                nonzeroth++;
                            }
                        }
                    }
                    *curr_index = j;
                }
            }
            break;

        case MPI_COMBINER_STRUCT:
        case MPI_COMBINER_STRUCT_INTEGER:
#ifdef FLATTEN_DEBUG
            DBG_FPRINTF(stderr, "ADIOI_Flatten:: MPI_COMBINER_STRUCT_INTEGER\n");
#endif
            top_count = ints[0];
            for (n = 0; n < top_count; n++) {
                MPI_Type_get_envelope(types[n], &old_nints, &old_nadds, &old_ntypes, &old_combiner);
                ADIOI_Datatype_iscontig(types[n], &old_is_contig);

                prev_index = *curr_index;
                if ((old_combiner != MPI_COMBINER_NAMED) && (!old_is_contig))
                    ADIOI_Flatten(types[n], flat, st_offset + adds[n], curr_index);

                if (prev_index == *curr_index) {
/* simplest case, current type is basic or contiguous types */
                    /* By using ADIO_Offset we preserve +/- sign and
                     * avoid >2G integer arithmetic problems */
                    if (ints[1 + n] > 0 || types[n] == MPI_LB || types[n] == MPI_UB) {
                        ADIO_Offset blocklength = ints[1 + n];
                        j = *curr_index;
                        flat->indices[j] = st_offset + adds[n];
                        MPI_Type_size_x(types[n], &old_size);
                        flat->blocklens[j] = blocklength * old_size;
                        if (types[n] == MPI_LB)
                            flat->lb_idx = j;
                        if (types[n] == MPI_UB)
                            flat->ub_idx = j;
#ifdef FLATTEN_DEBUG
                        DBG_FPRINTF(stderr,
                                    "ADIOI_Flatten:: simple adds[%#X] " MPI_AINT_FMT_HEX_SPEC
                                    ", flat->indices[%#llX] %#llX, flat->blocklens[%#llX] %#llX\n",
                                    n, adds[n], j, flat->indices[j], j, flat->blocklens[j]);
#endif
                        (*curr_index)++;
                    } else {
                        flat->count--;  /* don't count/consider any zero-length
                                         * blocklens */
                    }
                } else {
/* current type made up of noncontiguous derived types */

                    j = *curr_index;
                    num = *curr_index - prev_index;

/* The current type has to be replicated blocklens[n] times */
                    MPI_Type_extent(types[n], &old_extent);
                    for (m = 1; m < ints[1 + n]; m++) {
                        for (i = 0; i < num; i++) {
                            flat->indices[j] =
                                flat->indices[j - num] + ADIOI_AINT_CAST_TO_OFFSET old_extent;
                            flat->blocklens[j] = flat->blocklens[j - num];
#ifdef FLATTEN_DEBUG
                            DBG_FPRINTF(stderr,
                                        "ADIOI_Flatten:: simple old_extent " MPI_AINT_FMT_HEX_SPEC
                                        ", flat->indices[%#llX] %#llX, flat->blocklens[%#llX] %#llX\n",
                                        old_extent, j, flat->indices[j], j, flat->blocklens[j]);
#endif
                            j++;
                        }
                    }
                    *curr_index = j;
                }
            }
            break;

        case MPI_COMBINER_RESIZED:
#ifdef FLATTEN_DEBUG
            DBG_FPRINTF(stderr, "ADIOI_Flatten:: MPI_COMBINER_RESIZED\n");
#endif

            /* This is done similar to a type_struct with an lb, datatype, ub */

            /* handle the Lb */
            j = *curr_index;
            /* when we process resized types, we (recursively) process the lower
             * bound, the type being resized, then the upper bound.  In the
             * resized-of-resized case, we might find ourselves updating the upper
             * bound based on the inner type, but the lower bound based on the
             * upper type.  check both lb and ub to prevent mixing updates */
            if (flat->lb_idx == -1 && flat->ub_idx == -1) {
                flat->indices[j] = st_offset + adds[0];
                /* this zero-length blocklens[] element, unlike elsewhere in the
                 * flattening code, is correct and is used to indicate a lower bound
                 * marker */
                flat->blocklens[j] = 0;
                flat->lb_idx = *curr_index;
                lb_updated = 1;

#ifdef FLATTEN_DEBUG
                DBG_FPRINTF(stderr,
                            "ADIOI_Flatten:: simple adds[%#X] " MPI_AINT_FMT_HEX_SPEC
                            ", flat->indices[%#llX] %#llX, flat->blocklens[%#llX] %#llX\n", 0,
                            adds[0], j, flat->indices[j], j, flat->blocklens[j]);
#endif

                (*curr_index)++;
            } else {
                /* skipped over this chunk because something else higher-up in the
                 * type construction set this for us already */
                flat->count--;
                st_offset -= adds[0];
            }

            /* handle the datatype */

            MPI_Type_get_envelope(types[0], &old_nints, &old_nadds, &old_ntypes, &old_combiner);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            if ((old_combiner != MPI_COMBINER_NAMED) && (!old_is_contig)) {
                ADIOI_Flatten(types[0], flat, st_offset + adds[0], curr_index);
            } else {
                /* current type is basic or contiguous */
                j = *curr_index;
                flat->indices[j] = st_offset;
                MPI_Type_size_x(types[0], &old_size);
                flat->blocklens[j] = old_size;

#ifdef FLATTEN_DEBUG
                DBG_FPRINTF(stderr,
                            "ADIOI_Flatten:: simple adds[%#X] " MPI_AINT_FMT_HEX_SPEC
                            ", flat->indices[%#llX] %#llX, flat->blocklens[%#llX] %#llX\n", 0,
                            adds[0], j, flat->indices[j], j, flat->blocklens[j]);
#endif

                (*curr_index)++;
            }

            /* take care of the extent as a UB */
            /* see note above about mixing updates for why we check lb and ub */
            if ((flat->lb_idx == -1 && flat->ub_idx == -1) || lb_updated) {
                j = *curr_index;
                flat->indices[j] = st_offset + adds[0] + adds[1];
                /* again, zero-element ok: an upper-bound marker explicitly set by the
                 * constructor of this resized type */
                flat->blocklens[j] = 0;
                flat->ub_idx = *curr_index;
            } else {
                /* skipped over this chunk because something else higher-up in the
                 * type construction set this for us already */
                flat->count--;
                (*curr_index)--;
            }

#ifdef FLATTEN_DEBUG
            DBG_FPRINTF(stderr,
                        "ADIOI_Flatten:: simple adds[%#X] " MPI_AINT_FMT_HEX_SPEC
                        ", flat->indices[%#llX] %#llX, flat->blocklens[%#llX] %#llX\n", 1, adds[1],
                        j, flat->indices[j], j, flat->blocklens[j]);
#endif

            (*curr_index)++;

            break;

        default:
            /* TODO: FIXME (requires changing prototypes to return errors...) */
            DBG_FPRINTF(stderr, "Error: Unsupported datatype passed to ADIOI_Flatten\n");
            MPI_Abort(MPI_COMM_WORLD, 1);
    }

    for (i = 0; i < ntypes; i++) {
        MPI_Type_get_envelope(types[i], &old_nints, &old_nadds, &old_ntypes, &old_combiner);
        if (old_combiner != MPI_COMBINER_NAMED)
            MPI_Type_free(types + i);
    }

    ADIOI_Free(ints);
    ADIOI_Free(adds);
    ADIOI_Free(types);

#ifdef FLATTEN_DEBUG
    DBG_FPRINTF(stderr, "ADIOI_Flatten:: return st_offset %#llX, curr_index %#llX\n", st_offset,
                *curr_index);
#endif

}

/********************************************************/

/* ADIOI_Count_contiguous_blocks
 *
 * Returns number of contiguous blocks in type, and also updates
 * curr_index to reflect the space for the additional blocks.
 *
 * ASSUMES THAT TYPE IS NOT A BASIC!!!
 */
MPI_Count ADIOI_Count_contiguous_blocks(MPI_Datatype datatype, MPI_Count * curr_index)
{
    int i, n;
    MPI_Count count = 0, prev_index, num, basic_num;
    int top_count, combiner, old_combiner, old_is_contig;
    int nints, nadds, ntypes, old_nints, old_nadds, old_ntypes;
    int *ints;
    MPI_Aint *adds;             /* Make no assumptions about +/- sign on these */
    MPI_Datatype *types;

    MPI_Type_get_envelope(datatype, &nints, &nadds, &ntypes, &combiner);
    ints = (int *) ADIOI_Malloc((nints + 1) * sizeof(int));
    adds = (MPI_Aint *) ADIOI_Malloc((nadds + 1) * sizeof(MPI_Aint));
    types = (MPI_Datatype *) ADIOI_Malloc((ntypes + 1) * sizeof(MPI_Datatype));
    MPI_Type_get_contents(datatype, nints, nadds, ntypes, ints, adds, types);

    switch (combiner) {
#ifdef MPIIMPL_HAVE_MPI_COMBINER_DUP
        case MPI_COMBINER_DUP:
            MPI_Type_get_envelope(types[0], &old_nints, &old_nadds, &old_ntypes, &old_combiner);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);
            if ((old_combiner != MPI_COMBINER_NAMED) && (!old_is_contig))
                count = ADIOI_Count_contiguous_blocks(types[0], curr_index);
            else {
                count = 1;
                (*curr_index)++;
            }
            break;
#endif
#ifdef MPIIMPL_HAVE_MPI_COMBINER_SUBARRAY
        case MPI_COMBINER_SUBARRAY:
            {
                int dims = ints[0];
                MPI_Datatype stype;

                ADIO_Type_create_subarray(dims, &ints[1],       /* sizes */
                                          &ints[dims + 1],      /* subsizes */
                                          &ints[2 * dims + 1],  /* starts */
                                          ints[3 * dims + 1],   /* order */
                                          types[0],     /* type */
                                          &stype);
                count = ADIOI_Count_contiguous_blocks(stype, curr_index);
                /* curr_index will have already been updated; just pass
                 * count back up.
                 */
                MPI_Type_free(&stype);

            }
            break;
#endif
#ifdef MPIIMPL_HAVE_MPI_COMBINER_DARRAY
        case MPI_COMBINER_DARRAY:
            {
                int dims = ints[2];
                MPI_Datatype dtype;

                ADIO_Type_create_darray(ints[0],        /* size */
                                        ints[1],        /* rank */
                                        dims, &ints[3], /* gsizes */
                                        &ints[dims + 3],        /* distribs */
                                        &ints[2 * dims + 3],    /* dargs */
                                        &ints[3 * dims + 3],    /* psizes */
                                        ints[4 * dims + 3],     /* order */
                                        types[0], &dtype);
                count = ADIOI_Count_contiguous_blocks(dtype, curr_index);
                /* curr_index will have already been updated; just pass
                 * count back up.
                 */
                MPI_Type_free(&dtype);
            }
            break;
#endif
        case MPI_COMBINER_CONTIGUOUS:
            top_count = ints[0];
            MPI_Type_get_envelope(types[0], &old_nints, &old_nadds, &old_ntypes, &old_combiner);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            prev_index = *curr_index;
            if ((old_combiner != MPI_COMBINER_NAMED) && (!old_is_contig))
                count = ADIOI_Count_contiguous_blocks(types[0], curr_index);
            else
                count = 1;

            if (prev_index == *curr_index)
/* simplest case, made up of basic or contiguous types */
                (*curr_index)++;
            else {
/* made up of noncontiguous derived types */
                num = *curr_index - prev_index;
                count *= top_count;
                *curr_index += (top_count - 1) * num;
            }
            break;

        case MPI_COMBINER_VECTOR:
        case MPI_COMBINER_HVECTOR:
        case MPI_COMBINER_HVECTOR_INTEGER:
            top_count = ints[0];
            MPI_Type_get_envelope(types[0], &old_nints, &old_nadds, &old_ntypes, &old_combiner);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            prev_index = *curr_index;
            if ((old_combiner != MPI_COMBINER_NAMED) && (!old_is_contig))
                count = ADIOI_Count_contiguous_blocks(types[0], curr_index);
            else
                count = 1;

            if (prev_index == *curr_index) {
/* simplest case, vector of basic or contiguous types */
                count = top_count;
                *curr_index += count;
            } else {
/* vector of noncontiguous derived types */
                num = *curr_index - prev_index;

/* The noncontiguous types have to be replicated blocklen times
   and then strided. */
                count *= ints[1] * top_count;

/* First one */
                *curr_index += (ints[1] - 1) * num;

/* Now repeat with strides. */
                num = *curr_index - prev_index;
                *curr_index += (top_count - 1) * num;
            }
            break;

        case MPI_COMBINER_INDEXED:
        case MPI_COMBINER_HINDEXED:
        case MPI_COMBINER_HINDEXED_INTEGER:
            top_count = ints[0];
            MPI_Type_get_envelope(types[0], &old_nints, &old_nadds, &old_ntypes, &old_combiner);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            prev_index = *curr_index;
            if ((old_combiner != MPI_COMBINER_NAMED) && (!old_is_contig))
                count = ADIOI_Count_contiguous_blocks(types[0], curr_index);
            else
                count = 1;

            if (prev_index == *curr_index) {
/* simplest case, indexed type made up of basic or contiguous types */
                count = top_count;
                *curr_index += count;
            } else {
/* indexed type made up of noncontiguous derived types */
                basic_num = *curr_index - prev_index;

/* The noncontiguous types have to be replicated blocklens[i] times
   and then strided. */
                *curr_index += (ints[1] - 1) * basic_num;
                count *= ints[1];

/* Now repeat with strides. */
                for (i = 1; i < top_count; i++) {
                    count += ints[1 + i] * basic_num;
                    *curr_index += ints[1 + i] * basic_num;
                }
            }
            break;

#if defined HAVE_DECL_MPI_COMBINER_HINDEXED_BLOCK && HAVE_DECL_MPI_COMBINER_HINDEXED_BLOCK
        case MPI_COMBINER_HINDEXED_BLOCK:
#endif
        case MPI_COMBINER_INDEXED_BLOCK:
            top_count = ints[0];
            MPI_Type_get_envelope(types[0], &old_nints, &old_nadds, &old_ntypes, &old_combiner);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            prev_index = *curr_index;
            if ((old_combiner != MPI_COMBINER_NAMED) && (!old_is_contig))
                count = ADIOI_Count_contiguous_blocks(types[0], curr_index);
            else
                count = 1;

            if (prev_index == *curr_index) {
/* simplest case, indexed type made up of basic or contiguous types */
                count = top_count;
                *curr_index += count;
            } else {
/* indexed type made up of noncontiguous derived types */
                basic_num = *curr_index - prev_index;

/* The noncontiguous types have to be replicated blocklens[i] times
   and then strided. */
                *curr_index += (ints[1] - 1) * basic_num;
                count *= ints[1];

/* Now repeat with strides. */
                *curr_index += (top_count - 1) * count;
                count *= top_count;
            }
            break;

        case MPI_COMBINER_STRUCT:
        case MPI_COMBINER_STRUCT_INTEGER:
            top_count = ints[0];
            count = 0;
            for (n = 0; n < top_count; n++) {
                MPI_Type_get_envelope(types[n], &old_nints, &old_nadds, &old_ntypes, &old_combiner);
                ADIOI_Datatype_iscontig(types[n], &old_is_contig);

                prev_index = *curr_index;
                if ((old_combiner != MPI_COMBINER_NAMED) && (!old_is_contig))
                    count += ADIOI_Count_contiguous_blocks(types[n], curr_index);

                if (prev_index == *curr_index) {
/* simplest case, current type is basic or contiguous types */
                    count++;
                    (*curr_index)++;
                } else {
/* current type made up of noncontiguous derived types */
/* The current type has to be replicated blocklens[n] times */

                    num = *curr_index - prev_index;
                    count += (ints[1 + n] - 1) * num;
                    (*curr_index) += (ints[1 + n] - 1) * num;
                }
            }
            break;

        case MPI_COMBINER_RESIZED:
            /* treat it as a struct with lb, type, ub */

            /* add 2 for lb and ub */
            (*curr_index) += 2;
            count += 2;

            /* add for datatype */
            MPI_Type_get_envelope(types[0], &old_nints, &old_nadds, &old_ntypes, &old_combiner);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            if ((old_combiner != MPI_COMBINER_NAMED) && (!old_is_contig)) {
                count += ADIOI_Count_contiguous_blocks(types[0], curr_index);
            } else {
                /* basic or contiguous type */
                count++;
                (*curr_index)++;
            }
            break;

        default:
            /* TODO: FIXME */
            DBG_FPRINTF(stderr,
                        "Error: Unsupported datatype passed to ADIOI_Count_contiguous_blocks, combiner = %d\n",
                        combiner);
            MPI_Abort(MPI_COMM_WORLD, 1);
    }

    for (i = 0; i < ntypes; i++) {
        MPI_Type_get_envelope(types[i], &old_nints, &old_nadds, &old_ntypes, &old_combiner);
        if (old_combiner != MPI_COMBINER_NAMED)
            MPI_Type_free(types + i);
    }

    ADIOI_Free(ints);
    ADIOI_Free(adds);
    ADIOI_Free(types);
    return count;
}


/****************************************************************/

/* ADIOI_Optimize_flattened()
 *
 * Scans the blocks of a flattened type and merges adjacent blocks
 * together, resulting in a shorter blocklist (and thus fewer
 * contiguous operations).
 *
 * NOTE: a further optimization would be to remove zero length blocks. However,
 * the first and last blocks must remain as zero length first or last block
 * indicates UB and LB.  Furthermore, once the "zero length blocklen" fix
 * went in, the flattened representation should no longer have zero-length
 * blocks except for UB and LB markers.
 */
void ADIOI_Optimize_flattened(ADIOI_Flatlist_node * flat_type)
{
    int i, j, opt_blocks;
    ADIO_Offset *opt_blocklens;
    ADIO_Offset *opt_indices;

    opt_blocks = 1;

    for (j = -1, i = 0; i < flat_type->count; i++) {
        /* save number of noncontiguous blocks in opt_blocks */
        if (i < flat_type->count - 1 &&
            (flat_type->indices[i] + flat_type->blocklens[i] != flat_type->indices[i + 1]))
            opt_blocks++;

        /* Check if any of the displacements is negative */
        if (flat_type->blocklens[i] > 0 && flat_type->indices[i] < 0)
            flat_type->flag |= ADIOI_TYPE_NEGATIVE;

        if (flat_type->blocklens[i] == 0)       /* skip zero-length block */
            continue;
        else if (j == -1) {
            j = i;      /* set j the first non-zero-length block index */
            continue;
        }

        /* Check if displacements are in a monotonic nondecreasing order */
        if (flat_type->indices[j] > flat_type->indices[i])
            flat_type->flag |= ADIOI_TYPE_DECREASE;

        /* Check for overlapping regions */
        if (flat_type->indices[j] + flat_type->blocklens[j] > flat_type->indices[i])
            flat_type->flag |= ADIOI_TYPE_OVERLAP;

        j = i;  /* j is the previous non-zero-length block index */
    }

    /* if we can't reduce the number of blocks, quit now */
    if (opt_blocks == flat_type->count)
        return;

    opt_blocklens = (ADIO_Offset *) ADIOI_Calloc(opt_blocks * 2, sizeof(ADIO_Offset));
    opt_indices = opt_blocklens + opt_blocks;

    /* fill in new blocklists */
    opt_blocklens[0] = flat_type->blocklens[0];
    opt_indices[0] = flat_type->indices[0];
    j = 0;
    for (i = 0; i < (flat_type->count - 1); i++) {
        if ((flat_type->indices[i] + flat_type->blocklens[i] == flat_type->indices[i + 1]))
            opt_blocklens[j] += flat_type->blocklens[i + 1];
        else {
            j++;
            opt_indices[j] = flat_type->indices[i + 1];
            opt_blocklens[j] = flat_type->blocklens[i + 1];
        }
    }
    flat_type->count = opt_blocks;
    ADIOI_Free(flat_type->blocklens);
    flat_type->blocklens = opt_blocklens;
    flat_type->indices = opt_indices;
    return;
}

int ADIOI_Flattened_type_keyval = MPI_KEYVAL_INVALID;

int ADIOI_Flattened_type_copy(MPI_Datatype oldtype,
                              int type_keyval, void *extra_state, void *attribute_val_in,
                              void *attribute_val_out, int *flag)
{
    ADIOI_Flatlist_node *node = (ADIOI_Flatlist_node *) attribute_val_in;
    if (node != NULL)
        node->refct++;
    *(ADIOI_Flatlist_node **) attribute_val_out = node;
    *flag = 1;  /* attribute copied to new communicator */
    return MPI_SUCCESS;
}

int ADIOI_Flattened_type_delete(MPI_Datatype datatype,
                                int type_keyval, void *attribute_val, void *extra_state)
{
    ADIOI_Flatlist_node *node = (ADIOI_Flatlist_node *) attribute_val;
    ADIOI_Assert(node != NULL);
    node->refct--;

    if (node->refct <= 0) {
        ADIOI_Free(node->blocklens);
        ADIOI_Free(node);
    }

    return MPI_SUCCESS;
}

ADIOI_Flatlist_node *ADIOI_Flatten_and_find(MPI_Datatype datatype)
{
    ADIOI_Flatlist_node *node;
    int flag = 0;

    if (ADIOI_Flattened_type_keyval == MPI_KEYVAL_INVALID) {
        /* ADIOI_End_call will take care of cleanup */
        MPI_Type_create_keyval(ADIOI_Flattened_type_copy,
                               ADIOI_Flattened_type_delete, &ADIOI_Flattened_type_keyval, NULL);
    }

    MPI_Type_get_attr(datatype, ADIOI_Flattened_type_keyval, &node, &flag);
    if (flag == 0) {
        node = ADIOI_Flatten_datatype(datatype);
    }

    return node;
}
