/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: iscontig.c,v 1.7 2003/01/07 21:31:16 thakur Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#ifdef MPISGI
#include "mpisgi2.h"
#endif

#if (defined(MPICH) || defined(MPICH2))
/* MPICH2 also provides this routine */
void MPIR_Datatype_iscontig(MPI_Datatype datatype, int *flag);

void ADIOI_Datatype_iscontig(MPI_Datatype datatype, int *flag)
{
    MPIR_Datatype_iscontig(datatype, flag);
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
    *flag = MPI_SGI_type_is_contig(datatype);
}

#else

void ADIOI_Datatype_iscontig(MPI_Datatype datatype, int *flag)
{
    int nints, nadds, ntypes, combiner;
    int *ints, ni, na, nt, cb;
    MPI_Aint *adds;
    MPI_Datatype *types;

    MPI_Type_get_envelope(datatype, &nints, &nadds, &ntypes, &combiner);

    switch (combiner) {
    case MPI_COMBINER_NAMED:
	*flag = 1;
	break;
    case MPI_COMBINER_CONTIGUOUS:
	ints = (int *) ADIOI_Malloc((nints+1)*sizeof(int));
	adds = (MPI_Aint *) ADIOI_Malloc((nadds+1)*sizeof(MPI_Aint));
	types = (MPI_Datatype *) ADIOI_Malloc((ntypes+1)*sizeof(MPI_Datatype));
	MPI_Type_get_contents(datatype, nints, nadds, ntypes, ints,
			      adds, types); 
	ADIOI_Datatype_iscontig(types[0], flag);

#ifndef MPISGI
/* There is a bug in SGI's impl. of MPI_Type_get_contents. It doesn't
   return new datatypes. Therefore no need to free. */
	MPI_Type_get_envelope(types[0], &ni, &na, &nt, &cb);
	if (cb != MPI_COMBINER_NAMED) MPI_Type_free(types);
#endif

	ADIOI_Free(ints);
	ADIOI_Free(adds);
	ADIOI_Free(types);
	break;
    default:
	*flag = 0;
	break;
    }

    /* This function needs more work. It should check for contiguity 
       in other cases as well.*/
}
#endif
