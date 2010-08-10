/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stddef.h>
#include <stdio.h>

#include "ompi/constants.h"
#include "opal/datatype/opal_convertor_internal.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/util/output.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"

#include "mpi.h"

#if OPAL_ENABLE_DEBUG
#include "opal/mca/base/mca_base_param.h"
/* These are used within test/datatype/position.c -- and therefore not static */
int ompi_unpack_debug   = 0;
int ompi_pack_debug     = 0;
int ompi_copy_debug     = 0;
int ompi_position_debug = 0;
#endif  /* OPAL_ENABLE_DEBUG */

/* by default the debuging is turned off */
int ompi_datatype_dfd = -1;
OMPI_DECLSPEC union dt_elem_desc ompi_datatype_predefined_elem_desc[2 * OMPI_DATATYPE_MPI_MAX_PREDEFINED];

/**
 * This is the number of predefined datatypes. It is different than the MAX_PREDEFINED
 * as it include all the optional datatypes (such as MPI_INTEGER?, MPI_REAL?).
 */
int32_t ompi_datatype_number_of_predefined_data = 0;

/*
 * The following initialization of C, C++ and Fortran types is fairly complex,
 * based on the OPAL-datatypes.
 *   ompi_datatypes.h
 *       \-------> ompi_datatypes_internal.h   (Macros defining type-number and initalization)
 *   opal_datatypes.h
 *       \-------> opal_datatypes_internal.h   (Macros defining type-number and initalization)
 *
 * The Macros in the OMPI Layer differ in that:
 *   Additionally to OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE, we have a OMPI_DATATYPE_INIT_PREDEFINED,
 *   for all available types (getting rid of duplication of the name.
 * Of course initialization of ompi_mpi_datatype_null includes the string-name NULL,
 * which CPP converts to ((void*)0), which we do not really want...
 */
ompi_predefined_datatype_t ompi_mpi_datatype_null =
    {
        {
            OPAL_DATATYPE_INITIALIZER_NULL(OMPI_DATATYPE_FLAG_PREDEFINED),
            OMPI_DATATYPE_EMPTY_DATA(NULL),
        },
        {0, } /* padding */
    };

ompi_predefined_datatype_t ompi_mpi_unavailable =    OMPI_DATATYPE_INIT_PREDEFINED (UNAVAILABLE, 0);

ompi_predefined_datatype_t ompi_mpi_lb =             OMPI_DATATYPE_INIT_PREDEFINED (LB, 0);
ompi_predefined_datatype_t ompi_mpi_ub =             OMPI_DATATYPE_INIT_PREDEFINED (UB, 0);
ompi_predefined_datatype_t ompi_mpi_char =           OMPI_DATATYPE_INIT_PREDEFINED (CHAR, OMPI_DATATYPE_FLAG_DATA_C);
ompi_predefined_datatype_t ompi_mpi_signed_char =    OMPI_DATATYPE_INIT_PREDEFINED (SIGNED_CHAR, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_INT );
ompi_predefined_datatype_t ompi_mpi_unsigned_char =  OMPI_DATATYPE_INIT_PREDEFINED (UNSIGNED_CHAR, OMPI_DATATYPE_FLAG_DATA_C);
ompi_predefined_datatype_t ompi_mpi_byte =           OMPI_DATATYPE_INIT_PREDEFINED (BYTE, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_INT );
ompi_predefined_datatype_t ompi_mpi_short =          OMPI_DATATYPE_INIT_PREDEFINED (SHORT, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_INT );
ompi_predefined_datatype_t ompi_mpi_unsigned_short = OMPI_DATATYPE_INIT_PREDEFINED (UNSIGNED_SHORT, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_INT );
ompi_predefined_datatype_t ompi_mpi_int =            OMPI_DATATYPE_INIT_PREDEFINED (INT, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_INT );
ompi_predefined_datatype_t ompi_mpi_unsigned =       OMPI_DATATYPE_INIT_PREDEFINED (UNSIGNED_INT, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_INT );
ompi_predefined_datatype_t ompi_mpi_long =           OMPI_DATATYPE_INIT_PREDEFINED (LONG, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_INT );
ompi_predefined_datatype_t ompi_mpi_unsigned_long =  OMPI_DATATYPE_INIT_PREDEFINED (UNSIGNED_LONG, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_INT );
#if HAVE_LONG_LONG
ompi_predefined_datatype_t ompi_mpi_long_long_int =  OMPI_DATATYPE_INIT_PREDEFINED (LONG_LONG, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_INT );
ompi_predefined_datatype_t ompi_mpi_unsigned_long_long = OMPI_DATATYPE_INIT_PREDEFINED (UNSIGNED_LONG_LONG, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_INT );
#else
ompi_predefined_datatype_t ompi_mpi_long_long_int =  OMPI_DATATYPE_INIT_UNAVAILABLE (LONG_LONG, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_INT );
ompi_predefined_datatype_t ompi_mpi_unsigned_long_long = OMPI_DATATYPE_INIT_UNAVAILABLE (UNSIGNED_LONG_LONG, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_INT);
#endif  /* HAVE_LONG_LONG */
ompi_predefined_datatype_t ompi_mpi_float =          OMPI_DATATYPE_INIT_PREDEFINED (FLOAT, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_FLOAT );
ompi_predefined_datatype_t ompi_mpi_double =         OMPI_DATATYPE_INIT_PREDEFINED (DOUBLE, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_FLOAT );
#if HAVE_LONG_DOUBLE
ompi_predefined_datatype_t ompi_mpi_long_double =    OMPI_DATATYPE_INIT_PREDEFINED (LONG_DOUBLE, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_FLOAT );
#else
ompi_predefined_datatype_t ompi_mpi_long_double =    OMPI_DATATYPE_INIT_UNAVAILABLE (LONG_DOUBLE, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_FLOAT );
#endif  /* HAVE_LONG_DOUBLE */
#if defined(OPAL_ALIGNMENT_WCHAR) && OPAL_ALIGNMENT_WCHAR != 0
ompi_predefined_datatype_t ompi_mpi_wchar =          OMPI_DATATYPE_INIT_PREDEFINED (WCHAR, OMPI_DATATYPE_FLAG_DATA_C );
#else
ompi_predefined_datatype_t ompi_mpi_wchar =          OMPI_DATATYPE_INIT_UNAVAILABLE (WCHAR, OMPI_DATATYPE_FLAG_DATA_C );
#endif /* OPAL_ALIGNMENT_WCHAR */
ompi_predefined_datatype_t ompi_mpi_packed =         OMPI_DATATYPE_INIT_PREDEFINED (PACKED, 0 );

/*
 * C++ / C99 datatypes
 */
ompi_predefined_datatype_t ompi_mpi_cxx_bool =       OMPI_DATATYPE_INIT_PREDEFINED (BOOL, OMPI_DATATYPE_FLAG_DATA_CPP);
ompi_predefined_datatype_t ompi_mpi_cxx_cplex =      OMPI_DATATYPE_INIT_DEFER (COMPLEX, OMPI_DATATYPE_FLAG_DATA_CPP | OMPI_DATATYPE_FLAG_DATA_COMPLEX );
ompi_predefined_datatype_t ompi_mpi_cxx_dblcplex =   OMPI_DATATYPE_INIT_DEFER (DOUBLE_COMPLEX, OMPI_DATATYPE_FLAG_DATA_CPP | OMPI_DATATYPE_FLAG_DATA_COMPLEX );
#if HAVE_LONG_DOUBLE
ompi_predefined_datatype_t ompi_mpi_cxx_ldblcplex =  OMPI_DATATYPE_INIT_DEFER (LONG_DOUBLE_COMPLEX, OMPI_DATATYPE_FLAG_DATA_CPP | OMPI_DATATYPE_FLAG_DATA_COMPLEX );
#else
ompi_predefined_datatype_t ompi_mpi_cxx_ldblcplex =  OMPI_DATATYPE_INIT_UNAVAILABLE (LONG_DOUBLE_COMPLEX, OMPI_DATATYPE_FLAG_DATA_CPP | OMPI_DATATYPE_FLAG_DATA_COMPLEX );
#endif  /* HAVE_LONG_DOUBLE */

/*
 * Fortran datatypes
 */
ompi_predefined_datatype_t ompi_mpi_logical =        OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN (INT, LOGICAL, OMPI_SIZEOF_FORTRAN_LOGICAL, OMPI_ALIGNMENT_FORTRAN_LOGICAL, 0 );
ompi_predefined_datatype_t ompi_mpi_character =      OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN (INT, CHARACTER, 1, OPAL_ALIGNMENT_CHAR, 0 );
ompi_predefined_datatype_t ompi_mpi_integer =        OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN (INT, INTEGER, OMPI_SIZEOF_FORTRAN_INTEGER, OMPI_ALIGNMENT_FORTRAN_INTEGER, OMPI_DATATYPE_FLAG_DATA_INT );
ompi_predefined_datatype_t ompi_mpi_real =           OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN (FLOAT, REAL, OMPI_SIZEOF_FORTRAN_REAL, OMPI_ALIGNMENT_FORTRAN_REAL, OMPI_DATATYPE_FLAG_DATA_FLOAT );
ompi_predefined_datatype_t ompi_mpi_dblprec =        OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN (FLOAT, DOUBLE_PRECISION, OMPI_SIZEOF_FORTRAN_DOUBLE_PRECISION, OMPI_ALIGNMENT_FORTRAN_DOUBLE_PRECISION, OMPI_DATATYPE_FLAG_DATA_FLOAT );
ompi_predefined_datatype_t ompi_mpi_cplex =          OMPI_DATATYPE_INIT_DEFER (COMPLEX, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_COMPLEX );
#if OMPI_HAVE_F90_DOUBLE_COMPLEX || OMPI_HAVE_FORTRAN_DOUBLE_COMPLEX
/* We do not configure-check for alignment of F90 types ... Alignment of F77 OMPI_ALIGNMENT_FORTRAN_COMPLEX has to suffice */
ompi_predefined_datatype_t ompi_mpi_dblcplex =       OMPI_DATATYPE_INIT_DEFER (DOUBLE_COMPLEX, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_COMPLEX );
#else
ompi_predefined_datatype_t ompi_mpi_dblcplex =       OMPI_DATATYPE_INIT_UNAVAILABLE (DOUBLE_COMPLEX, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_COMPLEX );
#endif

/* In Fortran, there does not exist a type LONG DOUBLE COMPLEX, but DOUBLE COMPLEX(KIND=8) may require this */
#if HAVE_LONG_DOUBLE && ( OMPI_HAVE_F90_DOUBLE_COMPLEX || OMPI_HAVE_FORTRAN_DOUBLE_COMPLEX )
/* We do not configure-check for alignment of F90 types ... Alignment of F77 OMPI_ALIGNMENT_FORTRAN_COMPLEX has to suffice */
ompi_predefined_datatype_t ompi_mpi_ldblcplex =      OMPI_DATATYPE_INIT_DEFER (LONG_DOUBLE_COMPLEX, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_COMPLEX );
#else
ompi_predefined_datatype_t ompi_mpi_ldblcplex =      OMPI_DATATYPE_INIT_UNAVAILABLE (LONG_DOUBLE_COMPLEX, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_COMPLEX );
#endif

/* Aggregate struct datatypes are not const */
ompi_predefined_datatype_t ompi_mpi_float_int =      OMPI_DATATYPE_INIT_DEFER (FLOAT_INT, OMPI_DATATYPE_FLAG_DATA_C );
ompi_predefined_datatype_t ompi_mpi_double_int =     OMPI_DATATYPE_INIT_DEFER (DOUBLE_INT, OMPI_DATATYPE_FLAG_DATA_C );
#if HAVE_LONG_DOUBLE
ompi_predefined_datatype_t ompi_mpi_longdbl_int =    OMPI_DATATYPE_INIT_DEFER (LONG_DOUBLE_INT, OMPI_DATATYPE_FLAG_DATA_C );
#else
ompi_predefined_datatype_t ompi_mpi_longdbl_int =    OMPI_DATATYPE_INIT_UNAVAILABLE (LONG_DOUBLE_INT, OMPI_DATATYPE_FLAG_DATA_C );
#endif  /* HAVE_LONG_DOUBLE */

ompi_predefined_datatype_t ompi_mpi_2int =           OMPI_DATATYPE_INIT_DEFER (2INT, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_INT );
ompi_predefined_datatype_t ompi_mpi_short_int =      OMPI_DATATYPE_INIT_DEFER (SHORT_INT, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_INT );
ompi_predefined_datatype_t ompi_mpi_long_int =       OMPI_DATATYPE_INIT_DEFER (LONG_INT, OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_INT );

ompi_predefined_datatype_t ompi_mpi_2integer =       OMPI_DATATYPE_INIT_DEFER (2INTEGER, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_INT );
ompi_predefined_datatype_t ompi_mpi_2real =          OMPI_DATATYPE_INIT_DEFER (2REAL, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_FLOAT );
ompi_predefined_datatype_t ompi_mpi_2dblprec =       OMPI_DATATYPE_INIT_DEFER (2DBLPREC, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_FLOAT );

ompi_predefined_datatype_t ompi_mpi_2cplex =         OMPI_DATATYPE_INIT_DEFER (2COMPLEX, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_COMPLEX );
ompi_predefined_datatype_t ompi_mpi_2dblcplex =      OMPI_DATATYPE_INIT_DEFER (2DOUBLE_COMPLEX, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_COMPLEX );

/* For each of these we figure out, whether it is available -- otherwise it's set to unavailable */
#if OMPI_HAVE_FORTRAN_LOGICAL1
ompi_predefined_datatype_t ompi_mpi_logical1 =       OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN (INT, LOGICAL1, OMPI_SIZEOF_FORTRAN_LOGICAL1, OMPI_ALIGNMENT_FORTRAN_LOGICAL1, 0);
#else
ompi_predefined_datatype_t ompi_mpi_logical1 =       OMPI_DATATYPE_INIT_UNAVAILABLE (LOGICAL1, OMPI_DATATYPE_FLAG_DATA_FORTRAN );
#endif
#if OMPI_HAVE_FORTRAN_LOGICAL2
ompi_predefined_datatype_t ompi_mpi_logical2 =       OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN (INT, LOGICAL2, OMPI_SIZEOF_FORTRAN_LOGICAL2, OMPI_ALIGNMENT_FORTRAN_LOGICAL2, 0);
#else
ompi_predefined_datatype_t ompi_mpi_logical2 =       OMPI_DATATYPE_INIT_UNAVAILABLE (LOGICAL2, OMPI_DATATYPE_FLAG_DATA_FORTRAN );
#endif
#if OMPI_HAVE_FORTRAN_LOGICAL4
ompi_predefined_datatype_t ompi_mpi_logical4 =       OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN (INT, LOGICAL4, OMPI_SIZEOF_FORTRAN_LOGICAL4, OMPI_ALIGNMENT_FORTRAN_LOGICAL4, 0);
#else
ompi_predefined_datatype_t ompi_mpi_logical4 =       OMPI_DATATYPE_INIT_UNAVAILABLE (LOGICAL4, OMPI_DATATYPE_FLAG_DATA_FORTRAN );
#endif
#if OMPI_HAVE_FORTRAN_LOGICAL8
ompi_predefined_datatype_t ompi_mpi_logical8 =       OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN (INT, LOGICAL8, OMPI_SIZEOF_FORTRAN_LOGICAL8, OMPI_ALIGNMENT_FORTRAN_LOGICAL8, 0);
#else
ompi_predefined_datatype_t ompi_mpi_logical8 =       OMPI_DATATYPE_INIT_UNAVAILABLE (LOGICAL8, OMPI_DATATYPE_FLAG_DATA_FORTRAN );
#endif
#if OMPI_HAVE_FORTRAN_REAL2
ompi_predefined_datatype_t ompi_mpi_real2 =          OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN (FLOAT, REAL2, OMPI_SIZEOF_FORTRAN_REAL2, OMPI_ALIGNMENT_FORTRAN_REAL2, OMPI_DATATYPE_FLAG_DATA_FLOAT);
#else
ompi_predefined_datatype_t ompi_mpi_real2 =          OMPI_DATATYPE_INIT_UNAVAILABLE (REAL2, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_FLOAT);
#endif
#if OMPI_HAVE_FORTRAN_REAL4
ompi_predefined_datatype_t ompi_mpi_real4 =          OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN (FLOAT, REAL4, OMPI_SIZEOF_FORTRAN_REAL4, OMPI_ALIGNMENT_FORTRAN_REAL4, OMPI_DATATYPE_FLAG_DATA_FLOAT);
#else
ompi_predefined_datatype_t ompi_mpi_real4 =          OMPI_DATATYPE_INIT_UNAVAILABLE (REAL4, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_FLOAT);
#endif
#if OMPI_HAVE_FORTRAN_REAL8
ompi_predefined_datatype_t ompi_mpi_real8 =          OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN (FLOAT, REAL8, OMPI_SIZEOF_FORTRAN_REAL8, OMPI_ALIGNMENT_FORTRAN_REAL8, OMPI_DATATYPE_FLAG_DATA_FLOAT);
#else
ompi_predefined_datatype_t ompi_mpi_real8 =          OMPI_DATATYPE_INIT_UNAVAILABLE (REAL8, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_FLOAT);
#endif
#if OMPI_HAVE_FORTRAN_REAL16
ompi_predefined_datatype_t ompi_mpi_real16 =         OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN (FLOAT, REAL16, OMPI_SIZEOF_FORTRAN_REAL16, OMPI_ALIGNMENT_FORTRAN_REAL16, OMPI_DATATYPE_FLAG_DATA_FLOAT);
#else
ompi_predefined_datatype_t ompi_mpi_real16 =         OMPI_DATATYPE_INIT_UNAVAILABLE (REAL16, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_FLOAT);
#endif

#if OMPI_HAVE_FORTRAN_INTEGER1
ompi_predefined_datatype_t ompi_mpi_integer1 =       OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN (INT, INTEGER1, OMPI_SIZEOF_FORTRAN_INTEGER1, OMPI_ALIGNMENT_FORTRAN_INTEGER1, OMPI_DATATYPE_FLAG_DATA_INT);
#else
ompi_predefined_datatype_t ompi_mpi_integer1 =       OMPI_DATATYPE_INIT_UNAVAILABLE (INTEGER1, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_INT);
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
ompi_predefined_datatype_t ompi_mpi_integer2 =       OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN (INT, INTEGER2, OMPI_SIZEOF_FORTRAN_INTEGER2, OMPI_ALIGNMENT_FORTRAN_INTEGER2, OMPI_DATATYPE_FLAG_DATA_INT);
#else
ompi_predefined_datatype_t ompi_mpi_integer2 =       OMPI_DATATYPE_INIT_UNAVAILABLE (INTEGER2, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_INT);
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
ompi_predefined_datatype_t ompi_mpi_integer4 =       OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN (INT, INTEGER4, OMPI_SIZEOF_FORTRAN_INTEGER4, OMPI_ALIGNMENT_FORTRAN_INTEGER4, OMPI_DATATYPE_FLAG_DATA_INT);
#else
ompi_predefined_datatype_t ompi_mpi_integer4 =       OMPI_DATATYPE_INIT_UNAVAILABLE (INTEGER4, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_INT);
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
ompi_predefined_datatype_t ompi_mpi_integer8 =       OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN (INT, INTEGER8, OMPI_SIZEOF_FORTRAN_INTEGER8, OMPI_ALIGNMENT_FORTRAN_INTEGER8, OMPI_DATATYPE_FLAG_DATA_INT);
#else
ompi_predefined_datatype_t ompi_mpi_integer8 =       OMPI_DATATYPE_INIT_UNAVAILABLE (INTEGER8, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_INT);
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
ompi_predefined_datatype_t ompi_mpi_integer16 =      OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN (INT, INTEGER16, OMPI_SIZEOF_FORTRAN_INTEGER16, OMPI_ALIGNMENT_FORTRAN_INTEGER16, OMPI_DATATYPE_FLAG_DATA_INT);
#else
ompi_predefined_datatype_t ompi_mpi_integer16 =      OMPI_DATATYPE_INIT_UNAVAILABLE (INTEGER8, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_INT);
#endif

#if OMPI_HAVE_FORTRAN_COMPLEX8
ompi_predefined_datatype_t ompi_mpi_complex8 =       OMPI_DATATYPE_INIT_DEFER (COMPLEX8, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_COMPLEX);
#else
ompi_predefined_datatype_t ompi_mpi_complex8 =       OMPI_DATATYPE_INIT_UNAVAILABLE (COMPLEX8, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_COMPLEX);
#endif
#if OMPI_HAVE_FORTRAN_COMPLEX16
ompi_predefined_datatype_t ompi_mpi_complex16 =      OMPI_DATATYPE_INIT_DEFER (COMPLEX16, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_COMPLEX);
#else
ompi_predefined_datatype_t ompi_mpi_complex16 =      OMPI_DATATYPE_INIT_UNAVAILABLE (COMPLEX16, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_COMPLEX);
#endif
#if OMPI_HAVE_FORTRAN_COMPLEX32
ompi_predefined_datatype_t ompi_mpi_complex32 =      OMPI_DATATYPE_INIT_DEFER (COMPLEX32, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_COMPLEX);
#else
ompi_predefined_datatype_t ompi_mpi_complex32 =      OMPI_DATATYPE_INIT_UNAVAILABLE (COMPLEX32, OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_COMPLEX);
#endif


/*
 * NOTE: The order of this array *MUST* match what is listed in
 * opal_datatype_internal.h and ompi_datatype_internal.h
 * Everything referring to types/ids should be ORDERED as in ompi_datatype_basicDatatypes array.
 */
const ompi_datatype_t* ompi_datatype_basicDatatypes[OMPI_DATATYPE_MPI_MAX_PREDEFINED] = {
    &ompi_mpi_lb.dt,                   /*  0 */
    &ompi_mpi_ub.dt,
    &ompi_mpi_char.dt,
    &ompi_mpi_signed_char.dt,
    &ompi_mpi_unsigned_char.dt,
    &ompi_mpi_byte.dt,                 /*  5 */
    &ompi_mpi_short.dt,
    &ompi_mpi_unsigned_short.dt,
    &ompi_mpi_int.dt,
    &ompi_mpi_unsigned.dt,
    &ompi_mpi_long.dt,                 /* 10 */
    &ompi_mpi_unsigned_long.dt,
    &ompi_mpi_long_long_int.dt,
    &ompi_mpi_unsigned_long_long.dt,
    &ompi_mpi_float.dt,
    &ompi_mpi_double.dt,               /* 15 */
    &ompi_mpi_long_double.dt,
    &ompi_mpi_complex8.dt,
    &ompi_mpi_complex16.dt,
    &ompi_mpi_complex32.dt,
    &ompi_mpi_wchar.dt,                /* 20 */
    &ompi_mpi_packed.dt,

    /* C++ / C99 datatypes */
    &ompi_mpi_cxx_bool.dt,

    /* Fortran datatypes */
    &ompi_mpi_logical.dt,
    &ompi_mpi_character.dt,
    &ompi_mpi_integer.dt,              /* 25 */
    &ompi_mpi_real.dt,
    &ompi_mpi_dblprec.dt,
    &ompi_mpi_cplex.dt,
    &ompi_mpi_dblcplex.dt,
    &ompi_mpi_ldblcplex.dt,            /* 30 */

    /* Structure types, based on two basic types */
    &ompi_mpi_2int.dt,
    &ompi_mpi_2integer.dt,
    &ompi_mpi_2real.dt,
    &ompi_mpi_2dblprec.dt,
    &ompi_mpi_2cplex.dt,               /* 35 */
    &ompi_mpi_2dblcplex.dt,
    &ompi_mpi_float_int.dt,
    &ompi_mpi_double_int.dt,
    &ompi_mpi_longdbl_int.dt,
    &ompi_mpi_long_int.dt,             /* 40 */
    &ompi_mpi_short_int.dt,
    &ompi_mpi_unavailable.dt
};

opal_pointer_array_t ompi_datatype_f_to_c_table;

#define COPY_DATA_DESC( PDST, PSRC )                                                 \
    do {                                                                             \
        (PDST)->super.flags    = (PSRC)->super.flags;                                \
        (PDST)->super.id       = (PSRC)->super.id;                                   \
        (PDST)->super.bdt_used = (PSRC)->super.bdt_used;                             \
        (PDST)->super.size     = (PSRC)->super.size;                                 \
        (PDST)->super.true_lb  = (PSRC)->super.true_lb;                              \
        (PDST)->super.true_ub  = (PSRC)->super.true_ub;                              \
        (PDST)->super.lb       = (PSRC)->super.lb;                                   \
        (PDST)->super.ub       = (PSRC)->super.ub;                                   \
        (PDST)->super.align    = (PSRC)->super.align;                                \
        (PDST)->super.nbElems  = (PSRC)->super.nbElems;                              \
        (PDST)->super.desc     = (PSRC)->super.desc;                                 \
        (PDST)->super.opt_desc = (PSRC)->super.opt_desc;                             \
        (PDST)->packed_description = (PSRC)->packed_description;                     \
        (PSRC)->packed_description = NULL;                                           \
        memcpy( (PDST)->super.btypes, (PSRC)->super.btypes,                          \
                OPAL_DATATYPE_MAX_PREDEFINED * sizeof(uint32_t) );                   \
    } while(0)

#define DECLARE_MPI2_COMPOSED_STRUCT_DDT( PDATA, MPIDDT, MPIDDTNAME, type1, type2, MPIType1, MPIType2, FLAGS) \
    do {                                                                             \
        struct { type1 v1; type2 v2; } s[2];                                         \
        ompi_datatype_t *types[2], *ptype;                                           \
        int bLength[2] = {1, 1};                                                     \
        OPAL_PTRDIFF_TYPE base, displ[2];                                            \
                                                                                     \
        types[0] = (ompi_datatype_t*)ompi_datatype_basicDatatypes[MPIType1];         \
        types[1] = (ompi_datatype_t*)ompi_datatype_basicDatatypes[MPIType2];         \
        base = (OPAL_PTRDIFF_TYPE)(&(s[0]));                                         \
        displ[0] = (OPAL_PTRDIFF_TYPE)(&(s[0].v1));                                  \
        displ[0] -= base;                                                            \
        displ[1] = (OPAL_PTRDIFF_TYPE)(&(s[0].v2));                                  \
        displ[1] -= base;                                                            \
                                                                                     \
        ompi_datatype_create_struct( 2, bLength, displ, types, &ptype );             \
        displ[0] = (OPAL_PTRDIFF_TYPE)(&(s[1]));                                     \
        displ[0] -= base;                                                            \
        if( displ[0] != (displ[1] + (OPAL_PTRDIFF_TYPE)sizeof(type2)) )              \
            ptype->super.ub = displ[0];  /* force a new extent for the datatype */   \
        ptype->super.flags |= (FLAGS);                                               \
        ptype->id = MPIDDT;                                                          \
        ompi_datatype_commit( &ptype );                                              \
        COPY_DATA_DESC( PDATA, ptype );                                              \
        (PDATA)->super.flags &= ~OPAL_DATATYPE_FLAG_PREDEFINED;                      \
        (PDATA)->super.flags |= OMPI_DATATYPE_FLAG_PREDEFINED;                       \
        ptype->super.desc.desc = NULL;                                               \
        ptype->super.opt_desc.desc = NULL;                                           \
        OBJ_RELEASE( ptype );                                                        \
        strncpy( (PDATA)->super.name, MPIDDTNAME, MPI_MAX_OBJECT_NAME );             \
    } while(0)

#define DECLARE_MPI2_COMPOSED_BLOCK_DDT( PDATA, MPIDDT, MPIDDTNAME, MPIType, FLAGS ) \
    do {                                                                             \
        ompi_datatype_t *ptype;                                                      \
        ompi_datatype_create_contiguous( 2, ompi_datatype_basicDatatypes[MPIType], &ptype );   \
        ptype->super.flags |= (FLAGS);                                               \
        ptype->super.id = (MPIDDT);                                                  \
        ompi_datatype_commit( &ptype );                                              \
        COPY_DATA_DESC( (PDATA), ptype );                                            \
        (PDATA)->super.flags &= ~OPAL_DATATYPE_FLAG_PREDEFINED;                      \
        (PDATA)->super.flags |= OMPI_DATATYPE_FLAG_PREDEFINED;                       \
        ptype->super.desc.desc = NULL;                                               \
        ptype->super.opt_desc.desc = NULL;                                           \
        OBJ_RELEASE( ptype );                                                        \
        strncpy( (PDATA)->super.name, (MPIDDTNAME), MPI_MAX_OBJECT_NAME );           \
    } while(0)

#define DECLARE_MPI_SYNONYM_DDT( PDATA, MPIDDTNAME, PORIGDDT)                        \
    do {                                                                             \
        /* just memcpy as it's easier this way */                                    \
        memcpy( (PDATA), (PORIGDDT), sizeof(ompi_datatype_t) );                      \
        strncpy( (PDATA)->name, MPIDDTNAME, MPI_MAX_OBJECT_NAME );                   \
        /* forget the language flag */                                               \
        (PDATA)->super.flags &= ~OMPI_DATATYPE_FLAG_DATA_LANGUAGE;                   \
        (PDATA)->super.flags &= ~OPAL_DATATYPE_FLAG_PREDEFINED;                      \
        (PDATA)->super.flags |= OMPI_DATATYPE_FLAG_PREDEFINED;                       \
    } while(0)


int32_t ompi_datatype_init( void )
{
    int32_t i;

    for( i = OMPI_DATATYPE_FIRST_TYPE; i < OMPI_DATATYPE_MPI_MAX_PREDEFINED; i++ ) {
        const ompi_datatype_t* datatype = (ompi_datatype_t*)ompi_datatype_basicDatatypes[i];

        datatype->super.desc.desc[0].elem.common.flags = OPAL_DATATYPE_FLAG_PREDEFINED |
                                                         OPAL_DATATYPE_FLAG_DATA |
                                                         OPAL_DATATYPE_FLAG_CONTIGUOUS;
        datatype->super.desc.desc[0].elem.common.type  = i;
        datatype->super.desc.desc[0].elem.count        = 1;
        datatype->super.desc.desc[0].elem.disp         = 0;
        datatype->super.desc.desc[0].elem.extent       = datatype->super.size;

        datatype->super.desc.desc[1].end_loop.common.flags    = 0;
        datatype->super.desc.desc[1].end_loop.common.type     = OPAL_DATATYPE_END_LOOP;
        datatype->super.desc.desc[1].end_loop.items           = 1;
        datatype->super.desc.desc[1].end_loop.first_elem_disp = datatype->super.desc.desc[0].elem.disp;
        datatype->super.desc.desc[1].end_loop.size            = datatype->super.size;

        /* Check if the data contain gaps */
        if( (datatype->super.ub - datatype->super.lb) == (OPAL_PTRDIFF_TYPE)datatype->super.size ) {
            datatype->super.desc.desc[0].elem.common.flags |= OPAL_DATATYPE_FLAG_NO_GAPS;
        }
    }

    /* Create the f2c translation table */
    OBJ_CONSTRUCT(&ompi_datatype_f_to_c_table, opal_pointer_array_t);
    if( OPAL_SUCCESS != opal_pointer_array_init(&ompi_datatype_f_to_c_table,
                                                0, OMPI_FORTRAN_HANDLE_MAX, 64)) {
        return OMPI_ERROR;
    }
    /* All temporary datatypes created on the following statement will get registered
     * on the f2c table. But as they get destroyed they will (hopefully) get unregistered
     * so later when we start registering the real datatypes they will get the index
     * in mpif.h
     */

    /* the complex datatypes (float, double and long double) */
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_cplex.dt, OMPI_DATATYPE_COMPLEX, "MPI_COMPLEX",
                                      float, float, OMPI_DATATYPE_MPI_FLOAT, OMPI_DATATYPE_MPI_FLOAT,
                                      OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_FLOAT );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_dblcplex.dt, OMPI_DATATYPE_DOUBLE_COMPLEX, "MPI_DOUBLE_COMPLEX",
                                      double, double, OMPI_DATATYPE_MPI_DOUBLE, OMPI_DATATYPE_MPI_DOUBLE,
                                      OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_FLOAT );
#if HAVE_LONG_DOUBLE
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_ldblcplex.dt, OMPI_DATATYPE_LONG_DOUBLE_COMPLEX, "MPI_LONG_DOUBLE_COMPLEX",
                                      long double, long double, OMPI_DATATYPE_MPI_LONG_DOUBLE, OMPI_DATATYPE_MPI_LONG_DOUBLE,
                                      OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_FLOAT );
#endif  /* HAVE_LONG_DOUBLE */

    /* Now the predefined MPI2 datatypes (they should last forever!) */
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( &ompi_mpi_2int.dt, OMPI_DATATYPE_2INT, "MPI_2INT",
                                     OMPI_DATATYPE_MPI_INT,
                                     OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_INT );
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( &ompi_mpi_2integer.dt, OMPI_DATATYPE_2INTEGER, "MPI_2INTEGER",
                                     OMPI_DATATYPE_MPI_INT,
                                     OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_INT);
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( &ompi_mpi_2real.dt, OMPI_DATATYPE_2REAL, "MPI_2REAL",
                                     OMPI_DATATYPE_MPI_FLOAT,
                                     OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_FLOAT );
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( &ompi_mpi_2dblprec.dt, OMPI_DATATYPE_2DBLPREC, "MPI_2DOUBLE_PRECISION",
                                     OMPI_DATATYPE_MPI_DOUBLE,
                                     OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_FLOAT );
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( &ompi_mpi_2cplex.dt, OMPI_DATATYPE_2COMPLEX, "MPI_2COMPLEX",
                                     OMPI_DATATYPE_MPI_COMPLEX,
                                     OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_COMPLEX );
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( &ompi_mpi_2dblcplex.dt, OMPI_DATATYPE_2DOUBLE_COMPLEX, "MPI_2DOUBLE_COMPLEX",
                                     OMPI_DATATYPE_MPI_DOUBLE_COMPLEX,
                                     OMPI_DATATYPE_FLAG_DATA_FORTRAN | OMPI_DATATYPE_FLAG_DATA_COMPLEX );

    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_float_int.dt, OMPI_DATATYPE_FLOAT_INT, "MPI_FLOAT_INT",
                                      float, int,
                                      OMPI_DATATYPE_MPI_FLOAT, OMPI_DATATYPE_MPI_INT,
                                      OMPI_DATATYPE_FLAG_DATA_C );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_double_int.dt, OMPI_DATATYPE_DOUBLE_INT, "MPI_DOUBLE_INT",
                                      double, int,
                                      OMPI_DATATYPE_MPI_DOUBLE, OMPI_DATATYPE_MPI_INT,
                                      OMPI_DATATYPE_FLAG_DATA_C );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_long_int.dt, OMPI_DATATYPE_LONG_INT, "MPI_LONG_INT",
                                      long, int,
                                      OMPI_DATATYPE_MPI_LONG, OMPI_DATATYPE_MPI_INT,
                                      OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_INT );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_short_int.dt, OMPI_DATATYPE_SHORT_INT, "MPI_SHORT_INT",
                                      short, int,
                                      OMPI_DATATYPE_MPI_SHORT, OMPI_DATATYPE_MPI_INT,
                                      OMPI_DATATYPE_FLAG_DATA_C | OMPI_DATATYPE_FLAG_DATA_INT );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_longdbl_int.dt, OMPI_DATATYPE_LONG_DOUBLE_INT, "MPI_LONG_DOUBLE_INT",
                                      long double, int,
                                      OMPI_DATATYPE_MPI_LONG_DOUBLE, OMPI_DATATYPE_MPI_INT,
                                      OMPI_DATATYPE_FLAG_DATA_C );


    /* Copy the desc pointer from the <OMPI_DATATYPE_MPI_MAX_PREDEFINED datatypes to
       the synonym types */

    /* C++ complex types */
    DECLARE_MPI_SYNONYM_DDT( &ompi_mpi_cxx_cplex.dt, "MPI_CXX_COMPLEX", &ompi_mpi_cplex.dt );
    ompi_mpi_cxx_cplex.dt.super.flags |= OMPI_DATATYPE_FLAG_DATA_CPP | OMPI_DATATYPE_FLAG_DATA_COMPLEX;
    DECLARE_MPI_SYNONYM_DDT( &ompi_mpi_cxx_dblcplex.dt, "MPI_CXX_DOUBLE_COMPLEX", &ompi_mpi_dblcplex.dt );
    ompi_mpi_cxx_dblcplex.dt.super.flags |= OMPI_DATATYPE_FLAG_DATA_CPP | OMPI_DATATYPE_FLAG_DATA_COMPLEX;
#if HAVE_LONG_DOUBLE
    DECLARE_MPI_SYNONYM_DDT( &ompi_mpi_cxx_ldblcplex.dt, "MPI_CXX_LONG_DOUBLE_COMPLEX", &ompi_mpi_ldblcplex.dt );
    ompi_mpi_cxx_ldblcplex.dt.super.flags |= OMPI_DATATYPE_FLAG_DATA_CPP | OMPI_DATATYPE_FLAG_DATA_COMPLEX;
#endif  /* HAVE_LONG_DOUBLE */


    /* Start to populate the f2c index translation table */

    /* The order of the data registration should be the same as the
     * one in the mpif.h file. Any modification here should be
     * reflected there !!!  Do the Fortran types first so that mpif.h
     * can have consecutive, dense numbers. */ 

    /* This macro makes everything significantly easier to read below.
       All hail the moog!  :-) */

#define MOOG(name)                                                                   \
    {                                                                                \
        ompi_mpi_##name.dt.d_f_to_c_index =                                          \
            opal_pointer_array_add(&ompi_datatype_f_to_c_table, &ompi_mpi_##name);   \
        if( ompi_datatype_number_of_predefined_data < (ompi_mpi_##name).dt.d_f_to_c_index ) \
            ompi_datatype_number_of_predefined_data = (ompi_mpi_##name).dt.d_f_to_c_index; \
    }

    /*
     * This MUST match the order of ompi/include/mpif-common.h
     * Any change will break binary compatibility of Fortran programs.
     */
    MOOG(datatype_null);
    MOOG(byte);
    MOOG(packed);
    MOOG(ub);
    MOOG(lb);
    MOOG(character);
    MOOG(logical);
    MOOG(integer);
    MOOG(integer1);
    MOOG(integer2);
    MOOG(integer4);
    MOOG(integer8);
    MOOG(integer16);
    MOOG(real);
    MOOG(real4);
    MOOG(real8);
    MOOG(real16);
    MOOG(dblprec);
    MOOG(cplex);
    MOOG(complex8);
    MOOG(complex16);
    MOOG(complex32);
    MOOG(dblcplex);
    MOOG(2real);
    MOOG(2dblprec);
    MOOG(2integer);
    MOOG(2cplex);
    MOOG(2dblcplex);
    MOOG(real2);
    MOOG(logical1);
    MOOG(logical2);
    MOOG(logical4);
    MOOG(logical8);

    /* Now the C types */

    MOOG(wchar);
    MOOG(char);
    MOOG(unsigned_char);
    MOOG(signed_char);
    MOOG(short);
    MOOG(unsigned_short);
    MOOG(int);
    MOOG(unsigned);
    MOOG(long);
    MOOG(unsigned_long);
    MOOG(long_long_int);
    MOOG(unsigned_long_long);

    MOOG(float);
    MOOG(double);
    MOOG(long_double);

    MOOG(float_int);
    MOOG(double_int);
    MOOG(longdbl_int);
    MOOG(long_int);
    MOOG(2int);
    MOOG(short_int);

    /* C++ types */

    MOOG(cxx_bool);
    MOOG(cxx_cplex);
    MOOG(cxx_dblcplex);
    MOOG(cxx_ldblcplex);

    for( i = 0; i < ompi_mpi_cxx_ldblcplex.dt.d_f_to_c_index; i++ ) {
        opal_datatype_t* datatype = (opal_datatype_t*)opal_pointer_array_get_item(&ompi_datatype_f_to_c_table, i );

        if( (datatype->ub - datatype->lb) == (OPAL_PTRDIFF_TYPE)datatype->size ) {
            datatype->flags |= OPAL_DATATYPE_FLAG_NO_GAPS;
        } else {
            datatype->flags &= ~OPAL_DATATYPE_FLAG_NO_GAPS;
        }
    }

    ompi_datatype_default_convertors_init();
    return OMPI_SUCCESS;
}


int32_t ompi_datatype_finalize( void )
{
    /* As the synonyms are just copies of the internal data we should not free them.
     * Anyway they are over the limit of OMPI_DATATYPE_MPI_MAX_PREDEFINED so they will never get freed.
     */

    /* As they are statically allocated they cannot be released.
     * XXX But we can call OBJ_DESTRUCT, just to free all internally allocated ressources.
     */
#if 0
    int i;

    for( i = 0; i < OMPI_DATATYPE_MAX_PREDEFINED; i++ ) {
        OBJ_DESTRUCT( ompi_datatype_basicDatatypes[i] );
    }
#endif /* 0 */

    /* Get rid of the Fortran2C translation table */
    OBJ_DESTRUCT(&ompi_datatype_f_to_c_table);

#if defined(VERBOSE)
    if( ompi_datatype_dfd != -1 )
        opal_output_close( ompi_datatype_dfd );
    ompi_datatype_dfd = -1;
#endif  /* VERBOSE */

    /* release the local convertors (external32 and local) */
    ompi_datatype_default_convertors_fini();

    opal_datatype_finalize();

    return OMPI_SUCCESS;
}


#if OPAL_ENABLE_DEBUG
/*
 * Set a breakpoint to this function in your favorite debugger
 * to make it stop on all pack and unpack errors.
 */
int ompi_datatype_safeguard_pointer_debug_breakpoint( const void* actual_ptr, int length,
                                                 const void* initial_ptr,
                                                 const ompi_datatype_t* pData,
                                                 int count )
{
    return 0;
}
#endif  /* OPAL_ENABLE_DEBUG */


/********************************************************
 * Data dumping functions
 ********************************************************/

static int _ompi_dump_data_flags( unsigned short usflags, char* ptr, size_t length )
{
    int index = 0;
    if( length < 22 ) return 0;
    /* The lower-level part is the responsibility of opal_datatype_dump_data_flags */
    index += opal_datatype_dump_data_flags (usflags, ptr, length);

    /* Which kind of datatype is that */
    switch( usflags & OMPI_DATATYPE_FLAG_DATA_LANGUAGE ) {
    case OMPI_DATATYPE_FLAG_DATA_C:
        ptr[12] = ' '; ptr[13] = 'C'; ptr[14] = ' '; break;
    case OMPI_DATATYPE_FLAG_DATA_CPP:
        ptr[12] = 'C'; ptr[13] = 'P'; ptr[14] = 'P'; break;
    case OMPI_DATATYPE_FLAG_DATA_FORTRAN:
        ptr[12] = 'F'; ptr[13] = '7'; ptr[14] = '7'; break;
    default:
        if( usflags & OMPI_DATATYPE_FLAG_PREDEFINED ) {
            ptr[12] = 'E'; ptr[13] = 'R'; ptr[14] = 'R'; break;
        }
    }
    switch( usflags & OMPI_DATATYPE_FLAG_DATA_TYPE ) {
    case OMPI_DATATYPE_FLAG_DATA_INT:
        ptr[17] = 'I'; ptr[18] = 'N'; ptr[19] = 'T'; break;
    case OMPI_DATATYPE_FLAG_DATA_FLOAT:
        ptr[17] = 'F'; ptr[18] = 'L'; ptr[19] = 'T'; break;
    case OMPI_DATATYPE_FLAG_DATA_COMPLEX:
        ptr[17] = 'C'; ptr[18] = 'P'; ptr[19] = 'L'; break;
    default:
        if( usflags & OMPI_DATATYPE_FLAG_PREDEFINED ) {
            ptr[17] = 'E'; ptr[18] = 'R'; ptr[19] = 'R'; break;
        }
    }
    return index;
}


void ompi_datatype_dump( const ompi_datatype_t* pData )
{
    size_t length;
    int index = 0;
    char* buffer;

    length = pData->super.opt_desc.used + pData->super.desc.used;
    length = length * 100 + 500;
    buffer = (char*)malloc( length );
    index += snprintf( buffer, length - index,
                       "Datatype %p[%s] id %d size %ld align %d opal_id %d length %d used %d\n"
                       "true_lb %ld true_ub %ld (true_extent %ld) lb %ld ub %ld (extent %ld)\n"
                       "nbElems %d loops %d flags %X (",
                     (void*)pData, pData->name, pData->id,
                     (long)pData->super.size, (int)pData->super.align, pData->super.id, (int)pData->super.desc.length, (int)pData->super.desc.used,
                     (long)pData->super.true_lb, (long)pData->super.true_ub, (long)(pData->super.true_ub - pData->super.true_lb),
                     (long)pData->super.lb, (long)pData->super.ub, (long)(pData->super.ub - pData->super.lb),
                     (int)pData->super.nbElems, (int)pData->super.btypes[OPAL_DATATYPE_LOOP], (int)pData->super.flags );
    /* dump the flags */
    if( ompi_datatype_is_predefined(pData) )
        index += snprintf( buffer + index, length - index, "predefined " );
    else {
        if( pData->super.flags & OPAL_DATATYPE_FLAG_COMMITED ) index += snprintf( buffer + index, length - index, "commited " );
        if( pData->super.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS) index += snprintf( buffer + index, length - index, "contiguous " );
    }
    index += snprintf( buffer + index, length - index, ")" );
    index += _ompi_dump_data_flags( pData->super.flags, buffer + index, length - index );
    {
        index += snprintf( buffer + index, length - index, "\n   contain " );
        index += opal_datatype_contain_basic_datatypes( &pData->super, buffer + index, length - index );
        index += snprintf( buffer + index, length - index, "\n" );
    }
    if( (pData->super.opt_desc.desc != pData->super.desc.desc) && (NULL != pData->super.opt_desc.desc) ) {
        /* If the data is already committed print everything including the last
         * fake DT_END_LOOP entry.
         */
        index += opal_datatype_dump_data_desc( pData->super.desc.desc, pData->super.desc.used + 1, buffer + index, length - index );
        index += snprintf( buffer + index, length - index, "Optimized description \n" );
        index += opal_datatype_dump_data_desc( pData->super.opt_desc.desc, pData->super.opt_desc.used + 1, buffer + index, length - index );
    } else {
        index += opal_datatype_dump_data_desc( pData->super.desc.desc, pData->super.desc.used, buffer + index, length - index );
        index += snprintf( buffer + index, length - index, "No optimized description\n" );
    }
    buffer[index] = '\0';  /* make sure we end the string with 0 */
    opal_output( 0, "%s\n", buffer );

    ompi_datatype_print_args ( pData );

    free(buffer);
}
