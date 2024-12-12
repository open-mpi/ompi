/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "adio.h"
#include "adio_extern.h"

#ifdef MPL_USE_DBG_LOGGING
#define FLATTEN_DEBUG 1
#endif

static MPI_Count ADIOI_Count_contiguous_blocks(MPI_Datatype, MPI_Count *);
static void ADIOI_Flatten(MPI_Datatype, ADIOI_Flatlist_node *, ADIO_Offset, MPI_Count *);
 
int ADIOI_Flattened_type_keyval = MPI_KEYVAL_INVALID;

static int ADIOI_Flattened_type_copy(MPI_Datatype oldtype,
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

static int ADIOI_Flattened_type_delete(MPI_Datatype datatype,
                                       int type_keyval, void *attribute_val, void *extra_state)
{
    ADIOI_Flatlist_node *node = (ADIOI_Flatlist_node *) attribute_val;
    ADIOI_Assert(node != NULL);
    node->refct--;

    if (node->refct <= 0) {
        if (node->blocklens) {
            ADIOI_Free(node->blocklens);
        }
        ADIOI_Free(node);
    }

    return MPI_SUCCESS;
}

static ADIOI_Flatlist_node *ADIOI_Flatten_datatype(MPI_Datatype datatype);

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

#ifdef HAVE_MPIX_TYPE_IOV
ADIOI_Flatlist_node *ADIOI_Flatten_datatype(MPI_Datatype datatype)
{
    MPI_Count type_size;
    ADIOI_Flatlist_node *flat;

    MPI_Type_size_x(datatype, &type_size);

    if (type_size == 0) {
        /* copy to flatlist */
        flat = ADIOI_Malloc(sizeof(ADIOI_Flatlist_node));
        flat->count = 0;
        flat->blocklens = NULL;
        flat->indices = NULL;
        flat->refct = 1;
        flat->flag = 0;
    } else {
        MPI_Count num_iovs, actual;

        MPIX_Type_iov_len(datatype, type_size, &num_iovs, &actual);
        assert(num_iovs > 0);
        assert(actual == type_size);

        MPIX_Iov *iovs;
        iovs = ADIOI_Malloc(num_iovs * sizeof(MPIX_Iov));
        assert(iovs);

        MPIX_Type_iov(datatype, 0, iovs, num_iovs, &actual);
        assert(actual == num_iovs);

        /* copy to flatlist */
        flat = ADIOI_Malloc(sizeof(ADIOI_Flatlist_node));
        flat->count = num_iovs;
        flat->blocklens = (ADIO_Offset *) ADIOI_Malloc(flat->count * 2 * sizeof(ADIO_Offset));
        flat->indices = flat->blocklens + flat->count;
        flat->refct = 1;

        for (MPI_Count i = 0; i < num_iovs; i++) {
            flat->indices[i] = (ADIO_Offset) iovs[i].iov_base;
            flat->blocklens[i] = (ADIO_Offset) iovs[i].iov_len;
        }

        ADIOI_Free(iovs);

        /* update flags */
        flat->flag = 0;
        for (MPI_Count i = 0; i < flat->count; i++) {
            /* Check if any of the displacements is negative */
            if (flat->indices[i] < 0) {
                flat->flag |= ADIOI_TYPE_NEGATIVE;
            }

            if (i > 0) {
                MPI_Count j = i - 1;
                /* Check if displacements are in a monotonic nondecreasing order */
                if (flat->indices[j] > flat->indices[i]) {
                    flat->flag |= ADIOI_TYPE_DECREASE;
                }

                /* Check for overlapping regions */
                if (flat->indices[j] + flat->blocklens[j] > flat->indices[i]) {
                    flat->flag |= ADIOI_TYPE_OVERLAP;
                }
            }
        }
    }

    /* cache it to attribute */
    MPI_Type_set_attr(datatype, ADIOI_Flattened_type_keyval, flat);

    return flat;
}

#else

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

/*
 * I don't really expect this to ever trigger, but without the below safety
 * valve, the design relies on the Count function coming out >= whatever
 * the Flatten function comes up with.  There are enough differences between
 * the two that it's hard to be positive this will always be true.  So every
 * time something's added to flat's arrays, let's make sure they're big enough
 * and re-alloc if not.
 */
static void flatlist_node_grow(ADIOI_Flatlist_node * flat, int idx)
{
    if (idx >= flat->count) {
        ADIO_Offset *new_blocklens;
        ADIO_Offset *new_indices;
        int new_count = (flat->count * 1.25 + 4);
        new_blocklens = (ADIO_Offset *) ADIOI_Calloc(new_count * 2, sizeof(ADIO_Offset));
        new_indices = new_blocklens + new_count;
        if (flat->count) {
            memcpy(new_blocklens, flat->blocklens, flat->count * sizeof(ADIO_Offset));
            memcpy(new_indices, flat->indices, flat->count * sizeof(ADIO_Offset));
            ADIOI_Free(flat->blocklens);
        }
        flat->blocklens = new_blocklens;
        flat->indices = new_indices;
        flat->count = new_count;
    }
}

static void ADIOI_Optimize_flattened(ADIOI_Flatlist_node * flat_type);
/* flatten datatype and add it to Flatlist */
static ADIOI_Flatlist_node *ADIOI_Flatten_datatype(MPI_Datatype datatype)
{
    MPI_Count flat_count, curr_index = 0;
    int is_contig;
    ADIOI_Flatlist_node *flat;

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
        ADIOI_Flatten(datatype, flat, 0, &curr_index);
#ifdef FLATTEN_DEBUG
        DBG_FPRINTF(stderr, "ADIOI_Flatten_datatype:: ADIOI_Flatten\n");
#endif

/*
 * Setting flat->count to curr_index, since curr_index is the most fundamentally
 * correct updated value that represents what's in the indices/blocklens arrays.
 * It would be nice if the counter function and the flatten function were in sync,
 * but the numerous cases that decrement flat->count in the flatten function show
 * that syncing them is a hack, and as long as the counter doesn't under-count
 * it's good enough.
 */
        flat->count = curr_index;

        ADIOI_Optimize_flattened(flat);
/* debug */
#ifdef FLATTEN_DEBUG
        {
            int i;
            for (i = 0; i < flat->count; i++) {
                DBG_FPRINTF(stderr,
                            "ADIOI_Flatten_datatype:: i %#X, blocklens %#llX, indices %#llX\n", i,
                            (long long) flat->blocklens[i], (long long) flat->indices[i]);
            }
        }
#endif
    }
    MPI_Type_set_attr(datatype, ADIOI_Flattened_type_keyval, flat);
    return flat;

}

#if MPI_VERSION >= 4
static inline int downcast_i(MPI_Count count_value)
{
    int int_value = (int) count_value;
    ADIOI_Assert((int_value == count_value));
    return int_value;
}

static inline MPI_Aint downcast_a(MPI_Count count_value)
{
    MPI_Aint aint_value = (MPI_Aint) count_value;
    ADIOI_Assert((aint_value == count_value));
    return aint_value;
}
#endif

static void ADIOI_Type_decode(MPI_Datatype datatype, int *combiner,
                              int *nints, int *nadds, int *ntypes,
                              int **ints, MPI_Aint ** adds, MPI_Datatype ** types)
{
#if MPI_VERSION >= 4
    MPI_Count nints_c, nadds_c, ncnts_c, ntypes_c;
    int *ints_c;
    MPI_Aint *adds_c;
    MPI_Count *cnts_c;
    MPI_Datatype *types_c;

    MPI_Type_get_envelope_c(datatype, &nints_c, &nadds_c, &ncnts_c, &ntypes_c, combiner);

    ints_c = (int *) ADIOI_Malloc((nints_c + 1) * sizeof(int));
    adds_c = (MPI_Aint *) ADIOI_Malloc((nadds_c + 1) * sizeof(MPI_Aint));
    cnts_c = (MPI_Count *) ADIOI_Malloc((ncnts_c + 1) * sizeof(MPI_Count));
    types_c = (MPI_Datatype *) ADIOI_Malloc((ntypes_c + 1) * sizeof(MPI_Datatype));

    switch (*combiner) {
        case MPI_COMBINER_NAMED:
        case MPI_COMBINER_F90_INTEGER:
        case MPI_COMBINER_F90_REAL:
        case MPI_COMBINER_F90_COMPLEX:
            break;
        default:
            MPI_Type_get_contents_c(datatype,
                                    nints_c, nadds_c, ncnts_c, ntypes_c,
                                    ints_c, adds_c, cnts_c, types_c);
            break;
    }

    *nints = downcast_i(nints_c);
    *nadds = downcast_i(nadds_c);
    *ntypes = downcast_i(ntypes_c);

    if (ncnts_c > 0) {
        switch (*combiner) {
            case MPI_COMBINER_CONTIGUOUS:
            case MPI_COMBINER_VECTOR:
            case MPI_COMBINER_INDEXED:
            case MPI_COMBINER_INDEXED_BLOCK:
                *nints += downcast_i(ncnts_c);
                break;
            case MPI_COMBINER_HVECTOR:
                *nints += downcast_i(ncnts_c) - 1;
                *nadds += 1;
                break;
            case MPI_COMBINER_HINDEXED:
            case MPI_COMBINER_HINDEXED_BLOCK:
                *nints += downcast_i(cnts_c[0]);
                *nadds += downcast_i(cnts_c[0]);
                break;
            case MPI_COMBINER_STRUCT:
                *nints += downcast_i(cnts_c[0]);
                *nadds += downcast_i(cnts_c[0]);
                break;
            case MPI_COMBINER_SUBARRAY:
            case MPI_COMBINER_DARRAY:
                *nints += downcast_i(ncnts_c);
                break;
            case MPI_COMBINER_RESIZED:
                *nadds += downcast_i(ncnts_c);
                break;
            default:
                break;
        }
    }

    *ints = ints_c;
    *adds = adds_c;
    *types = types_c;

    if (ncnts_c > 0) {
        MPI_Count k, n, ip = 0, ap = 0, iq = 0, cq = 0;

        *ints = (int *) ADIOI_Malloc((*nints + 1) * sizeof(int));
        *adds = (MPI_Aint *) ADIOI_Malloc((*nadds + 1) * sizeof(MPI_Aint));

        switch (*combiner) {
            case MPI_COMBINER_CONTIGUOUS:
            case MPI_COMBINER_VECTOR:
            case MPI_COMBINER_INDEXED:
            case MPI_COMBINER_INDEXED_BLOCK:
                n = ncnts_c;
                for (k = 0; k < n; k++)
                    (*ints)[ip++] = downcast_i(cnts_c[cq++]);
                break;
            case MPI_COMBINER_HVECTOR:
                (*ints)[ip++] = downcast_i(cnts_c[cq++]);       /* count */
                (*ints)[ip++] = downcast_i(cnts_c[cq++]);       /* blocklength */
                (*adds)[ap++] = downcast_a(cnts_c[cq++]);       /* stride */
                break;
            case MPI_COMBINER_HINDEXED:
                n = cnts_c[0];
                (*ints)[ip++] = downcast_i(cnts_c[cq++]);       /* count */
                for (k = 0; k < n; k++)
                    (*ints)[ip++] = downcast_i(cnts_c[cq++]);   /* blocklengths */
                for (k = 0; k < n; k++)
                    (*adds)[ap++] = downcast_a(cnts_c[cq++]);   /* displacement */
                break;
            case MPI_COMBINER_HINDEXED_BLOCK:
                n = cnts_c[0];
                (*ints)[ip++] = downcast_i(cnts_c[cq++]);       /* count */
                (*ints)[ip++] = downcast_i(cnts_c[cq++]);       /* blocklength */
                for (k = 0; k < n; k++)
                    (*adds)[ap++] = downcast_a(cnts_c[cq++]);   /* displacements */
                break;
            case MPI_COMBINER_STRUCT:
                n = cnts_c[0];
                (*ints)[ip++] = downcast_i(cnts_c[cq++]);       /* count */
                for (k = 0; k < n; k++)
                    (*ints)[ip++] = downcast_i(cnts_c[cq++]);   /* blocklengths */
                for (k = 0; k < n; k++)
                    (*adds)[ap++] = downcast_a(cnts_c[cq++]);   /* displacements */
                break;
            case MPI_COMBINER_SUBARRAY:
                n = ints_c[0];
                (*ints)[ip++] = ints_c[iq++];   /* ndims */
                for (k = 0; k < n; k++)
                    (*ints)[ip++] = downcast_i(cnts_c[cq++]);   /* sizes */
                for (k = 0; k < n; k++)
                    (*ints)[ip++] = downcast_i(cnts_c[cq++]);   /* subsizes */
                for (k = 0; k < n; k++)
                    (*ints)[ip++] = downcast_i(cnts_c[cq++]);   /* starts */
                (*ints)[ip++] = ints_c[iq++];   /* order */
                break;
            case MPI_COMBINER_DARRAY:
                n = ints_c[2];
                (*ints)[ip++] = ints_c[iq++];   /* size */
                (*ints)[ip++] = ints_c[iq++];   /* rank */
                (*ints)[ip++] = ints_c[iq++];   /* ndims */
                for (k = 0; k < n; k++)
                    (*ints)[ip++] = downcast_i(cnts_c[cq++]);   /* gsizes */
                for (k = 0; k < n; k++)
                    (*ints)[ip++] = ints_c[iq++];       /* distribs */
                for (k = 0; k < n; k++)
                    (*ints)[ip++] = ints_c[iq++];       /* dargs */
                for (k = 0; k < n; k++)
                    (*ints)[ip++] = ints_c[iq++];       /* psizes */
                (*ints)[ip++] = ints_c[iq++];   /* order */
                break;
            case MPI_COMBINER_RESIZED:
                (*adds)[ap++] = downcast_a(cnts_c[cq++]);       /* lb */
                (*adds)[ap++] = downcast_a(cnts_c[cq++]);       /* extent */
                break;
            default:
                break;
        }
        ADIOI_Assert((iq == nints_c));
        ADIOI_Assert((cq == ncnts_c));

        ADIOI_Free(ints_c);
        ADIOI_Free(adds_c);
    }
    ADIOI_Free(cnts_c);
#else
    MPI_Type_get_envelope(datatype, nints, nadds, ntypes, combiner);
    *ints = (int *) ADIOI_Malloc((*nints + 1) * sizeof(int));
    *adds = (MPI_Aint *) ADIOI_Malloc((*nadds + 1) * sizeof(MPI_Aint));
    *types = (MPI_Datatype *) ADIOI_Malloc((*ntypes + 1) * sizeof(MPI_Datatype));
    MPI_Type_get_contents(datatype, *nints, *nadds, *ntypes, *ints, *adds, *types);
#endif
}

/* ADIOI_Flatten()
 *
 * Assumption: input datatype is not a basic!!!!
 */
static void ADIOI_Flatten(MPI_Datatype datatype, ADIOI_Flatlist_node * flat,
                          ADIO_Offset st_offset, MPI_Count * curr_index)
{
    int k, m, n, is_hindexed_block = 0;
    int lb_updated = 0;
    int combiner, old_is_predef, old_is_contig;
    int nints, nadds, ntypes;
    /* By using ADIO_Offset we preserve +/- sign and
     * avoid >2G integer arithmetic problems */
    ADIO_Offset top_count;
    MPI_Count i, j, old_size, prev_index, basic_num, num, nonzeroth;
    MPI_Aint lb, old_extent;    /* Assume extents are non-negative */
    int *ints;
    MPI_Aint *adds;             /* Make no assumptions about +/- sign on these */
    MPI_Datatype *types;

    ADIOI_Type_decode(datatype, &combiner, &nints, &nadds, &ntypes, &ints, &adds, &types);

#ifdef FLATTEN_DEBUG
    DBG_FPRINTF(stderr, "ADIOI_Flatten:: st_offset %#llX, curr_index %#llX\n",
                (long long) st_offset, (long long) *curr_index);
    DBG_FPRINTF(stderr, "ADIOI_Flatten:: nints %#X, nadds %#X, ntypes %#X\n", nints, nadds, ntypes);
    for (i = 0; i < nints; ++i) {
        DBG_FPRINTF(stderr, "ADIOI_Flatten:: ints[%lld]=%#X\n", (long long) i, ints[i]);
    }
    for (i = 0; i < nadds; ++i) {
        DBG_FPRINTF(stderr, "ADIOI_Flatten:: adds[%lld]=" MPI_AINT_FMT_HEX_SPEC "\n",
                    (long long) i, adds[i]);
    }
    for (i = 0; i < ntypes; ++i) {
        DBG_FPRINTF(stderr, "ADIOI_Flatten:: types[%lld]=%#llX\n", (long long) i,
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
            ADIOI_Type_ispredef(types[0], &old_is_predef);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);
            if ((!old_is_predef) && (!old_is_contig))
                ADIOI_Flatten(types[0], flat, st_offset, curr_index);
            break;
#endif
#ifdef MPIIMPL_HAVE_MPI_COMBINER_SUBARRAY
        case MPI_COMBINER_SUBARRAY:
            if (ints[0] > 0) {
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
            if (ints[2] > 0) {
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
                            0, (long long) flat->indices[0], 0, (long long) flat->blocklens[0],
                            (long long) st_offset, (long long) *curr_index);
#endif
                ADIOI_Flatten(dtype, flat, st_offset, curr_index);
#ifdef FLATTEN_DEBUG
                DBG_FPRINTF(stderr,
                            "ADIOI_Flatten:: MPI_COMBINER_DARRAY >ADIOI_Flatten(dtype, flat->indices[%#X] %#llX, flat->blocklens[%#X] %#llX, st_offset %#llX, curr_index %#llX);\n",
                            0, (long long) flat->indices[0], 0, (long long) flat->blocklens[0],
                            (long long) st_offset, (long long) *curr_index);
#endif
                MPI_Type_free(&dtype);
            }
            break;
#endif
        case MPI_COMBINER_CONTIGUOUS:
#ifdef FLATTEN_DEBUG
            DBG_FPRINTF(stderr, "ADIOI_Flatten:: MPI_COMBINER_CONTIGUOUS\n");
#endif
            if (ints[0] == 0)
                break;

            top_count = ints[0];
            ADIOI_Type_ispredef(types[0], &old_is_predef);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            prev_index = *curr_index;
            if ((!old_is_predef) && (!old_is_contig))
                ADIOI_Flatten(types[0], flat, st_offset, curr_index);

            if (prev_index == *curr_index) {
/* simplest case, made up of basic or contiguous types */
                j = *curr_index;
                flatlist_node_grow(flat, j);
                flat->indices[j] = st_offset;
                MPI_Type_size_x(types[0], &old_size);
                flat->blocklens[j] = top_count * old_size;
#ifdef FLATTEN_DEBUG
                DBG_FPRINTF(stderr,
                            "ADIOI_Flatten:: simple flat->indices[%#llX] %#llX, flat->blocklens[%#llX] %#llX\n",
                            (long long) j, (long long) flat->indices[j],
                            (long long) j, (long long) flat->blocklens[j]);
#endif
                (*curr_index)++;
            } else {
/* made up of noncontiguous derived types */
                j = *curr_index;
                num = *curr_index - prev_index;

/* The noncontiguous types have to be replicated count times */
                MPI_Type_get_extent(types[0], &lb, &old_extent);
                for (m = 1; m < top_count; m++) {
                    for (i = 0; i < num; i++) {
                        flatlist_node_grow(flat, j);
                        flat->indices[j] =
                            flat->indices[j - num] + ADIOI_AINT_CAST_TO_OFFSET old_extent;
                        flat->blocklens[j] = flat->blocklens[j - num];
#ifdef FLATTEN_DEBUG
                        DBG_FPRINTF(stderr,
                                    "ADIOI_Flatten:: derived flat->indices[%#llX] %#llX, flat->blocklens[%#llX] %#llX\n",
                                    (long long) j, (long long) flat->indices[j],
                                    (long long) j, (long long) flat->blocklens[j]);
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
            if (ints[0] == 0)
                break;

            top_count = ints[0];
            ADIOI_Type_ispredef(types[0], &old_is_predef);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            prev_index = *curr_index;
            if ((!old_is_predef) && (!old_is_contig))
                ADIOI_Flatten(types[0], flat, st_offset, curr_index);

            if (prev_index == *curr_index) {
/* simplest case, vector of basic or contiguous types */
                /* By using ADIO_Offset we preserve +/- sign and
                 * avoid >2G integer arithmetic problems */
                ADIO_Offset blocklength = ints[1], stride = ints[2];
                j = *curr_index;
                flatlist_node_grow(flat, j);
                flat->indices[j] = st_offset;
                MPI_Type_size_x(types[0], &old_size);
                flat->blocklens[j] = blocklength * old_size;
                for (i = j + 1; i < j + top_count; i++) {
                    flatlist_node_grow(flat, i);
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
                MPI_Type_get_extent(types[0], &lb, &old_extent);
                for (m = 1; m < blocklength; m++) {
                    for (i = 0; i < num; i++) {
                        flatlist_node_grow(flat, j);
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
                        flatlist_node_grow(flat, j);
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
#if MPI_VERSION < 3
        case MPI_COMBINER_HVECTOR_INTEGER:
#ifdef FLATTEN_DEBUG
            DBG_FPRINTF(stderr, "ADIOI_Flatten:: MPI_COMBINER_HVECTOR_INTEGER\n");
#endif
#endif
            if (ints[0] == 0)
                break;

            top_count = ints[0];
            ADIOI_Type_ispredef(types[0], &old_is_predef);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            prev_index = *curr_index;
            if ((!old_is_predef) && (!old_is_contig))
                ADIOI_Flatten(types[0], flat, st_offset, curr_index);

            if (prev_index == *curr_index) {
/* simplest case, vector of basic or contiguous types */
                /* By using ADIO_Offset we preserve +/- sign and
                 * avoid >2G integer arithmetic problems */
                ADIO_Offset blocklength = ints[1];
                j = *curr_index;
                flatlist_node_grow(flat, j);
                flat->indices[j] = st_offset;
                MPI_Type_size_x(types[0], &old_size);
                flat->blocklens[j] = blocklength * old_size;
                for (i = j + 1; i < j + top_count; i++) {
                    flatlist_node_grow(flat, i);
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
                MPI_Type_get_extent(types[0], &lb, &old_extent);
                for (m = 1; m < blocklength; m++) {
                    for (i = 0; i < num; i++) {
                        flatlist_node_grow(flat, j);
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
                        flatlist_node_grow(flat, j);
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
            if (ints[0] == 0)
                break;

            top_count = ints[0];
            ADIOI_Type_ispredef(types[0], &old_is_predef);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);
            MPI_Type_get_extent(types[0], &lb, &old_extent);

            prev_index = *curr_index;
            if ((!old_is_predef) && (!old_is_contig)) {
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
                        flatlist_node_grow(flat, nonzeroth);
                        flat->indices[nonzeroth] =
                            st_offset + stride * ADIOI_AINT_CAST_TO_OFFSET old_extent;
                        flat->blocklens[nonzeroth] =
                            blocklength * ADIOI_AINT_CAST_TO_OFFSET old_extent;
                        nonzeroth++;
                    }
                }
                *curr_index = nonzeroth;
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
                            flatlist_node_grow(flat, nonzeroth);
                            flat->indices[nonzeroth] =
                                flat->indices[nonzeroth - num] +
                                ADIOI_AINT_CAST_TO_OFFSET old_extent;
                            flat->blocklens[nonzeroth] = flat->blocklens[nonzeroth - num];
                            j++;
                            nonzeroth++;
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
                            flatlist_node_grow(flat, nonzeroth);
                            flat->indices[nonzeroth] =
                                flat->indices[j - num] +
                                stride * ADIOI_AINT_CAST_TO_OFFSET old_extent;
                            flat->blocklens[nonzeroth] = flat->blocklens[j - num];
                            j++;
                            nonzeroth++;
                        }
                    }
                    *curr_index = j;
                    for (m = 1; m < ints[1 + i]; m++) {
                        for (k = 0, nonzeroth = j; k < basic_num; k++) {
                            if (flat->blocklens[j - basic_num] > 0) {
                                flatlist_node_grow(flat, nonzeroth);
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

#if defined HAVE_DECL_MPI_COMBINER_HINDEXED_BLOCK && HAVE_DECL_MPI_COMBINER_HINDEXED_BLOCK
        case MPI_COMBINER_HINDEXED_BLOCK:
            is_hindexed_block = 1;
#endif
            /* fall through */
        case MPI_COMBINER_INDEXED_BLOCK:
#ifdef FLATTEN_DEBUG
            DBG_FPRINTF(stderr, "ADIOI_Flatten:: MPI_COMBINER_INDEXED_BLOCK\n");
#endif
            if (ints[0] == 0)
                break;

            top_count = ints[0];
            ADIOI_Type_ispredef(types[0], &old_is_predef);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);
            MPI_Type_get_extent(types[0], &lb, &old_extent);

            prev_index = *curr_index;
            if ((!old_is_predef) && (!old_is_contig)) {
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
                        flatlist_node_grow(flat, i);
                        flat->indices[i] = st_offset + adds[i - j];
                    } else {
                        ADIO_Offset stride = ints[1 + 1 + i - j];
                        flatlist_node_grow(flat, i);
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
                            MPI_Type_get_extent(types[0], &lb, &old_extent);
                        }
                        flatlist_node_grow(flat, j);
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
                            flatlist_node_grow(flat, j);
                            flat->indices[j] = flat->indices[j - num] + adds[i] - adds[i - 1];
                        } else {
                            /* By using ADIO_Offset we preserve +/- sign and
                             * avoid >2G integer arithmetic problems */
                            ADIO_Offset stride = ints[2 + i] - ints[1 + i];
                            flatlist_node_grow(flat, j);
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
#if MPI_VERSION < 3
        case MPI_COMBINER_HINDEXED_INTEGER:
#ifdef FLATTEN_DEBUG
            DBG_FPRINTF(stderr, "ADIOI_Flatten:: MPI_COMBINER_HINDEXED_INTEGER\n");
#endif
#endif
            if (ints[0] == 0)
                break;

            top_count = ints[0];
            ADIOI_Type_ispredef(types[0], &old_is_predef);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            prev_index = *curr_index;
            if ((!old_is_predef) && (!old_is_contig)) {
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
                        flatlist_node_grow(flat, nonzeroth);
                        flat->indices[nonzeroth] = st_offset + adds[i - j];
                        flat->blocklens[nonzeroth] = blocklength * old_size;
                        nonzeroth++;
                    }
                }
                *curr_index = nonzeroth;
            } else {
/* indexed type made up of noncontiguous derived types */

                j = *curr_index;
                num = *curr_index - prev_index;
                basic_num = num;

/* The noncontiguous types have to be replicated blocklens[i] times
   and then strided. Replicate the first one. */
                MPI_Type_get_extent(types[0], &lb, &old_extent);
                for (m = 1; m < ints[1]; m++) {
                    for (i = 0, nonzeroth = j; i < num; i++) {
                        if (flat->blocklens[j - num] > 0) {
                            flatlist_node_grow(flat, nonzeroth);
                            flat->indices[nonzeroth] =
                                flat->indices[j - num] + ADIOI_AINT_CAST_TO_OFFSET old_extent;
                            flat->blocklens[nonzeroth] = flat->blocklens[j - num];
                            j++;
                            nonzeroth++;
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
                            flatlist_node_grow(flat, nonzeroth);
                            flat->indices[nonzeroth] =
                                flat->indices[j - num] + adds[i] - adds[i - 1];
                            flat->blocklens[nonzeroth] = flat->blocklens[j - num];
                            j++;
                            nonzeroth++;
                        }
                    }
                    *curr_index = j;
                    for (m = 1; m < ints[1 + i]; m++) {
                        for (k = 0, nonzeroth = j; k < basic_num; k++) {
                            if (flat->blocklens[j - basic_num] > 0) {
                                flatlist_node_grow(flat, nonzeroth);
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
#if MPI_VERSION < 3
        case MPI_COMBINER_STRUCT_INTEGER:
#ifdef FLATTEN_DEBUG
            DBG_FPRINTF(stderr, "ADIOI_Flatten:: MPI_COMBINER_STRUCT_INTEGER\n");
#endif
#endif
            if (ints[0] == 0)
                break;

            top_count = ints[0];
            for (n = 0; n < top_count; n++) {
                ADIOI_Type_ispredef(types[n], &old_is_predef);
                ADIOI_Datatype_iscontig(types[n], &old_is_contig);

                prev_index = *curr_index;
                if ((!old_is_predef) && (!old_is_contig))
                    ADIOI_Flatten(types[n], flat, st_offset + adds[n], curr_index);

                if (prev_index == *curr_index) {
/* simplest case, current type is basic or contiguous types */
                    /* By using ADIO_Offset we preserve +/- sign and
                     * avoid >2G integer arithmetic problems */
                    if (ints[1 + n] > 0) {
                        ADIO_Offset blocklength = ints[1 + n];
                        j = *curr_index;
                        flatlist_node_grow(flat, j);
                        flat->indices[j] = st_offset + adds[n];
                        MPI_Type_size_x(types[n], &old_size);
                        flat->blocklens[j] = blocklength * old_size;
#ifdef FLATTEN_DEBUG
                        DBG_FPRINTF(stderr,
                                    "ADIOI_Flatten:: simple adds[%#X] " MPI_AINT_FMT_HEX_SPEC
                                    ", flat->indices[%#llX] %#llX, flat->blocklens[%#llX] %#llX\n",
                                    n, adds[n], j, flat->indices[j], j, flat->blocklens[j]);
#endif
                        (*curr_index)++;
                    }
                } else {
/* current type made up of noncontiguous derived types */

                    j = *curr_index;
                    num = *curr_index - prev_index;

/* The current type has to be replicated blocklens[n] times */
                    MPI_Type_get_extent(types[n], &lb, &old_extent);
                    for (m = 1; m < ints[1 + n]; m++) {
                        for (i = 0; i < num; i++) {
                            flatlist_node_grow(flat, j);
                            flat->indices[j] =
                                flat->indices[j - num] + ADIOI_AINT_CAST_TO_OFFSET old_extent;
                            flat->blocklens[j] = flat->blocklens[j - num];
#ifdef FLATTEN_DEBUG
                            DBG_FPRINTF(stderr,
                                        "ADIOI_Flatten:: simple old_extent " MPI_AINT_FMT_HEX_SPEC
                                        ", flat->indices[%#llX] %#llX, flat->blocklens[%#llX] %#llX\n",
                                        old_extent, (long long) j, (long long) flat->indices[j],
                                        (long long) j, (long long) flat->blocklens[j]);
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
                flatlist_node_grow(flat, j);
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
                            adds[0], (long long) j, (long long) flat->indices[j],
                            (long long) j, (long long) flat->blocklens[j]);
#endif

                (*curr_index)++;
            } else {
                /* skipped over this chunk because something else higher-up in the
                 * type construction set this for us already */
                st_offset -= adds[0];
            }

            /* handle the datatype */

            ADIOI_Type_ispredef(types[0], &old_is_predef);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            if ((!old_is_predef) && (!old_is_contig)) {
                ADIOI_Flatten(types[0], flat, st_offset + adds[0], curr_index);
            } else {
                /* current type is basic or contiguous */
                j = *curr_index;
                flatlist_node_grow(flat, j);
                flat->indices[j] = st_offset;
                MPI_Type_size_x(types[0], &old_size);
                flat->blocklens[j] = old_size;

#ifdef FLATTEN_DEBUG
                DBG_FPRINTF(stderr,
                            "ADIOI_Flatten:: simple adds[%#X] " MPI_AINT_FMT_HEX_SPEC
                            ", flat->indices[%#llX] %#llX, flat->blocklens[%#llX] %#llX\n", 0,
                            adds[0], (long long) j, (long long) flat->indices[j],
                            (long long) j, (long long) flat->blocklens[j]);
#endif

                (*curr_index)++;
            }

            /* take care of the extent as a UB */
            /* see note above about mixing updates for why we check lb and ub */
            if ((flat->lb_idx == -1 && flat->ub_idx == -1) || lb_updated) {
                j = *curr_index;
                flatlist_node_grow(flat, j);
                flat->indices[j] = st_offset + adds[0] + adds[1];
                /* again, zero-element ok: an upper-bound marker explicitly set by the
                 * constructor of this resized type */
                flat->blocklens[j] = 0;
                flat->ub_idx = *curr_index;
            } else {
                /* skipped over this chunk because something else higher-up in the
                 * type construction set this for us already */
                (*curr_index)--;
            }

#ifdef FLATTEN_DEBUG
            DBG_FPRINTF(stderr,
                        "ADIOI_Flatten:: simple adds[%#X] " MPI_AINT_FMT_HEX_SPEC
                        ", flat->indices[%#llX] %#llX, flat->blocklens[%#llX] %#llX\n", 1, adds[1],
                        (long long) j, (long long) flat->indices[j],
                        (long long) j, (long long) flat->blocklens[j]);
#endif

            (*curr_index)++;

            break;

        default:
            /* TODO: FIXME (requires changing prototypes to return errors...) */
            DBG_FPRINTF(stderr, "Error: Unsupported datatype passed to ADIOI_Flatten\n");
            MPI_Abort(MPI_COMM_WORLD, 1);
    }

    for (i = 0; i < ntypes; i++) {
        ADIOI_Type_dispose(types + i);
    }

    ADIOI_Free(ints);
    ADIOI_Free(adds);
    ADIOI_Free(types);

#ifdef FLATTEN_DEBUG
    DBG_FPRINTF(stderr, "ADIOI_Flatten:: return st_offset %#llX, curr_index %#llX\n",
                (long long) st_offset, (long long) *curr_index);
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
static MPI_Count ADIOI_Count_contiguous_blocks(MPI_Datatype datatype, MPI_Count * curr_index)
{
    int i, n;
    MPI_Count count = 0, prev_index, num, basic_num;
    int top_count, combiner, old_is_predef, old_is_contig;
    int nints, nadds, ntypes;
    int *ints;
    MPI_Aint *adds;             /* Make no assumptions about +/- sign on these */
    MPI_Datatype *types;

    ADIOI_Type_decode(datatype, &combiner, &nints, &nadds, &ntypes, &ints, &adds, &types);

    switch (combiner) {
#ifdef MPIIMPL_HAVE_MPI_COMBINER_DUP
        case MPI_COMBINER_DUP:
            ADIOI_Type_ispredef(types[0], &old_is_predef);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);
            if ((!old_is_predef) && (!old_is_contig))
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
            ADIOI_Type_ispredef(types[0], &old_is_predef);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            prev_index = *curr_index;
            if ((!old_is_predef) && (!old_is_contig))
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
#if MPI_VERSION < 3
        case MPI_COMBINER_HVECTOR_INTEGER:
#endif
            top_count = ints[0];
            ADIOI_Type_ispredef(types[0], &old_is_predef);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            prev_index = *curr_index;
            if ((!old_is_predef) && (!old_is_contig))
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
#if MPI_VERSION < 3
        case MPI_COMBINER_HINDEXED_INTEGER:
#endif
            top_count = ints[0];
            ADIOI_Type_ispredef(types[0], &old_is_predef);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            prev_index = *curr_index;
            if ((!old_is_predef) && (!old_is_contig))
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
            ADIOI_Type_ispredef(types[0], &old_is_predef);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            prev_index = *curr_index;
            if ((!old_is_predef) && (!old_is_contig))
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
#if MPI_VERSION < 3
        case MPI_COMBINER_STRUCT_INTEGER:
#endif
            top_count = ints[0];
            count = 0;
            for (n = 0; n < top_count; n++) {
                ADIOI_Type_ispredef(types[n], &old_is_predef);
                ADIOI_Datatype_iscontig(types[n], &old_is_contig);

                prev_index = *curr_index;
                if ((!old_is_predef) && (!old_is_contig))
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
            ADIOI_Type_ispredef(types[0], &old_is_predef);
            ADIOI_Datatype_iscontig(types[0], &old_is_contig);

            if ((!old_is_predef) && (!old_is_contig)) {
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
        ADIOI_Type_dispose(types + i);
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
static void ADIOI_Optimize_flattened(ADIOI_Flatlist_node * flat_type)
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

#endif /* HAVE_MPIX_TYPE_IOV */
