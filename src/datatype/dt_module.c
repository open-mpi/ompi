/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "datatype.h"
#include "datatype_internal.h"

int lam_ddt_init( void )
{
   int i;

   for( i = 0; i < DT_MAX_PREDEFINED; i++ ) {
      basicDatatypes[i].desc.desc = (dt_elem_desc_t*)malloc(sizeof(dt_elem_desc_t));
      basicDatatypes[i].desc.desc->flags  = DT_FLAG_BASIC | DT_FLAG_CONTIGUOUS;
      basicDatatypes[i].desc.desc->type   = i;
      basicDatatypes[i].desc.desc->count  = 1;
      basicDatatypes[i].desc.desc->disp   = 0;
      basicDatatypes[i].desc.desc->extent = basicDatatypes[i].size;
      basicDatatypes[i].desc.length       = 1;
      basicDatatypes[i].desc.used         = 1;
      basicDatatypes[i].btypes[i]         = 1;
   }

   return 0;
}

int lam_ddt_finalize( void )
{
   int i;

   for( i =0; i < DT_MAX_PREDEFINED; i++ ) {
      free( basicDatatypes[i].desc.desc );
      basicDatatypes[i].desc.desc   = NULL;
      basicDatatypes[i].desc.length = 0;
      basicDatatypes[i].desc.used   = 0;
   }
   return 0;
}
