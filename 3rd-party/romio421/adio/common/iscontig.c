/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "adio.h"

#if defined(MPICH)
/* MPICH also provides this routine */
void MPIR_Datatype_iscontig(MPI_Datatype datatype, int *flag);

void ADIOI_Datatype_iscontig(MPI_Datatype datatype, int *flag)
{
    MPIR_Datatype_iscontig(datatype, flag);

    /* if the datatype is reported as contiguous, check if the true_lb is
     * non-zero, and if so, mark the datatype as noncontiguous */
    if (*flag) {
        MPI_Aint true_extent, true_lb;

        MPI_Type_get_true_extent(datatype, &true_lb, &true_extent);

        if (true_lb > 0)
            *flag = 0;
    }
}

#elif (defined(MPIHP) && defined(HAVE_MPI_INFO))
/* i.e. HPMPI 1.4 only */

int hpmp_dtiscontig(MPI_Datatype datatype);

void ADIOI_Datatype_iscontig(MPI_Datatype datatype, int *flag)
{
    *flag = hpmp_dtiscontig(datatype);
}

#elif (defined(MPISGI) && !defined(NO_MPI_SGI_type_is_contig))

int MPI_SGI_type_is_contig(MPI_Datatype datatype);

void ADIOI_Datatype_iscontig(MPI_Datatype datatype, int *flag)
{
    MPI_Aint displacement, extent;
    MPI_Type_get_extent(datatype, &distplacement, &extent);

    /* SGI's MPI_SGI_type_is_contig() returns true for indexed
     * datatypes with holes at the beginning, which causes
     * problems with ROMIO's use of this function.
     */
    *flag = MPI_SGI_type_is_contig(datatype) && (displacement == 0);
}

#elif defined(OMPI_BUILDING) && OMPI_BUILDING

/* void ADIOI_Datatype_iscontig(MPI_Datatype datatype, int *flag) is defined
 * and implemented in OpenMPI itself */

#else

void ADIOI_Datatype_iscontig(MPI_Datatype datatype, int *flag)
{
    int combiner;

    ADIOI_Type_get_combiner(datatype, &combiner);

    switch (combiner) {
        case MPI_COMBINER_NAMED:
            *flag = 1;
            break;
#ifdef MPIIMPL_HAVE_MPI_COMBINER_DUP
        case MPI_COMBINER_DUP:
#endif
        case MPI_COMBINER_CONTIGUOUS:
            {
                int *ints;
                MPI_Aint *adds;
                MPI_Count *cnts;
                MPI_Datatype *types;
#if MPI_VERSION >= 4
                MPI_Count nints, nadds, ncnts, ntypes;
                MPI_Type_get_envelope_c(datatype, &nints, &nadds, &ncnts, &ntypes, &combiner);
#else
                int nints, nadds, ncnts = 0, ntypes;
                MPI_Type_get_envelope(datatype, &nints, &nadds, &ntypes, &combiner);
#endif
                ints = (int *) ADIOI_Malloc((nints + 1) * sizeof(int));
                adds = (MPI_Aint *) ADIOI_Malloc((nadds + 1) * sizeof(MPI_Aint));
                cnts = (MPI_Count *) ADIOI_Malloc((ncnts + 1) * sizeof(MPI_Count));
                types = (MPI_Datatype *) ADIOI_Malloc((ntypes + 1) * sizeof(MPI_Datatype));
#if MPI_VERSION >= 4
                MPI_Type_get_contents_c(datatype, nints, nadds, ncnts, ntypes, ints, adds, cnts,
                                        types);
#else
                MPI_Type_get_contents(datatype, nints, nadds, ntypes, ints, adds, types);
#endif
                ADIOI_Datatype_iscontig(types[0], flag);

                ADIOI_Type_dispose(types);
                ADIOI_Free(ints);
                ADIOI_Free(adds);
                ADIOI_Free(types);
            }
            break;
        case MPI_COMBINER_F90_INTEGER:
        case MPI_COMBINER_F90_REAL:
        case MPI_COMBINER_F90_COMPLEX:
            *flag = 1;
            break;
        default:
            *flag = 0;
            break;
    }

    /* This function needs more work. It should check for contiguity
     * in other cases as well. */
}
#endif
