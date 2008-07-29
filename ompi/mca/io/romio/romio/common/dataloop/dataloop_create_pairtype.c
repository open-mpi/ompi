/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "dataloop.h"

#define PAIRTYPE_CONTENTS(mt1_,ut1_,mt2_,ut2_)				\
    {									\
	struct { ut1_ a; ut2_ b; } foo;					\
	disps[0] = 0;							\
	disps[1] = (MPI_Aint) ((char *) &foo.b - (char *) &foo.a);	\
	types[0] = mt1_;						\
	types[1] = mt2_;						\
    }

/*@
   Dataloop_create_pairtype - create dataloop for a pairtype

   Arguments:
+  MPI_Datatype type - the pairtype
.  DLOOP_Dataloop **output_dataloop_ptr
.  int output_dataloop_size
.  int output_dataloop_depth
-  int flag

.N Errors
.N Returns 0 on success, -1 on failure.

   Note:
   This function simply creates the appropriate input parameters for
   use with Dataloop_create_struct and then calls that function.

   This same function could be used to create dataloops for any type
   that actually consists of two distinct elements.
@*/
int PREPEND_PREFIX(Dataloop_create_pairtype)(MPI_Datatype type,
					     DLOOP_Dataloop **dlp_p,
					     int *dlsz_p,
					     int *dldepth_p,
					     int flag)
{
    int blocks[2] = { 1, 1 };
    MPI_Aint disps[2];
    MPI_Datatype types[2];

    DLOOP_Assert(type == MPI_FLOAT_INT || type == MPI_DOUBLE_INT ||
		 type == MPI_LONG_INT || type == MPI_SHORT_INT ||
		 type == MPI_LONG_DOUBLE_INT || type == MPI_2INT);

    switch(type) {
	case MPI_FLOAT_INT:
	    PAIRTYPE_CONTENTS(MPI_FLOAT, float, MPI_INT, int);
	    break;
	case MPI_DOUBLE_INT:
	    PAIRTYPE_CONTENTS(MPI_DOUBLE, double, MPI_INT, int);
	    break;
	case MPI_LONG_INT:
	    PAIRTYPE_CONTENTS(MPI_LONG, long, MPI_INT, int);
	    break;
	case MPI_SHORT_INT:
	    PAIRTYPE_CONTENTS(MPI_SHORT, short, MPI_INT, int);
	    break;
	case MPI_LONG_DOUBLE_INT:
	    PAIRTYPE_CONTENTS(MPI_LONG_DOUBLE, long double, MPI_INT, int);
	    break;
	case MPI_2INT:
	    PAIRTYPE_CONTENTS(MPI_INT, int, MPI_INT, int);
	    break;
    }

    return PREPEND_PREFIX(Dataloop_create_struct)(2,
						  blocks,
						  disps,
						  types,
						  dlp_p,
						  dlsz_p,
						  dldepth_p,
						  flag);
}
