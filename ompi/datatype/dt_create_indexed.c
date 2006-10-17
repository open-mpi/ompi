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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/datatype/datatype.h"

/* We try to merge together data that are contiguous */
int32_t ompi_ddt_create_indexed( int count, const int* pBlockLength, const int* pDisp,
			         const ompi_datatype_t* oldType, ompi_datatype_t** newType )
{
    ompi_datatype_t* pdt;
    int i, dLength, endat, disp;
    ptrdiff_t extent = oldType->ub - oldType->lb;

    if( 0 == count ) {
        *newType = ompi_ddt_create( 0 );
        ompi_ddt_add( *newType, &ompi_mpi_datatype_null, 0, 0, 0);
        return OMPI_SUCCESS;
    }

    disp = pDisp[0];
    dLength = pBlockLength[0];
    endat = disp + dLength;
    if( 1 >= count ) {
        pdt = ompi_ddt_create( oldType->desc.used + 2 );
        /* multiply by count to make it zero if count is zero */
        ompi_ddt_add( pdt, oldType, count * dLength, disp * extent, extent );
    } else {
        pdt = ompi_ddt_create( count * (2 + oldType->desc.used) );
        for( i = 1; i < count; i++ ) {
            if( endat == pDisp[i] ) {
                /* contiguous with the previsious */
                dLength += pBlockLength[i];
                endat += pBlockLength[i];
            } else {
                ompi_ddt_add( pdt, oldType, dLength, disp * extent, extent );
                disp = pDisp[i];
                dLength = pBlockLength[i];
                endat = disp + pBlockLength[i];
            }
        }
        ompi_ddt_add( pdt, oldType, dLength, disp * extent, extent );
    }

    *newType = pdt;
    return OMPI_SUCCESS;
}

int32_t ompi_ddt_create_hindexed( int count, const int* pBlockLength, const MPI_Aint* pDisp,
                                  const ompi_datatype_t* oldType, ompi_datatype_t** newType )
{
    ompi_datatype_t* pdt;
    int i, dLength;
    ptrdiff_t extent = oldType->ub - oldType->lb;
    ptrdiff_t disp, endat;

    if( 0 == count ) {
        *newType = ompi_ddt_create( 0 );
        ompi_ddt_add( *newType, &ompi_mpi_datatype_null, 0, 0, 0);
        return OMPI_SUCCESS;
    }

    pdt = ompi_ddt_create( count * (2 + oldType->desc.used) );
    disp = pDisp[0];
    dLength = pBlockLength[0];
    endat = disp + dLength * extent;
    if( 1 >= count ) {
        pdt = ompi_ddt_create( oldType->desc.used + 2 );
        /* multiply by count to make it zero if count is zero */
        ompi_ddt_add( pdt, oldType, count * dLength, disp, extent );
    } else {
        for( i = 1; i < count; i++ ) {
            if( endat == pDisp[i] ) {
                /* contiguous with the previsious */
                dLength += pBlockLength[i];
                endat += pBlockLength[i] * extent;
            } else {
                ompi_ddt_add( pdt, oldType, dLength, disp, extent );
                disp = pDisp[i];
                dLength = pBlockLength[i];
                endat = disp + pBlockLength[i] * extent;
            }
        }
        ompi_ddt_add( pdt, oldType, dLength, disp, extent );
    }
    *newType = pdt;
    return OMPI_SUCCESS;
}

int32_t ompi_ddt_create_indexed_block( int count, int bLength, const int* pDisp,
                                       const ompi_datatype_t* oldType, ompi_datatype_t** newType )
{
   ompi_datatype_t* pdt;
   int i, dLength, endat, disp;
   ptrdiff_t extent = oldType->ub - oldType->lb;

   if( (count == 0) || (bLength == 0) ) {
      *newType = ompi_ddt_create(1);
      if( 0 == count )
          ompi_ddt_add( *newType, &ompi_mpi_datatype_null, 0, 0, 0 );
      else
          ompi_ddt_add( *newType, oldType, 0, pDisp[0] * extent, extent );
      return OMPI_SUCCESS;
   }
   pdt = ompi_ddt_create( count * (2 + oldType->desc.used) );
   disp = pDisp[0];
   dLength = bLength;
   endat = disp + dLength;
   for( i = 1; i < count; i++ ) {
      if( endat == pDisp[i] ) {
         /* contiguous with the previsious */
         dLength += bLength;
         endat += bLength;
      } else {
         ompi_ddt_add( pdt, oldType, dLength, disp * extent, extent );
         disp = pDisp[i];
         dLength = bLength;
         endat = disp + bLength;
      }
   }
   ompi_ddt_add( pdt, oldType, dLength, disp * extent, extent );

   *newType = pdt;
   return OMPI_SUCCESS;
}
