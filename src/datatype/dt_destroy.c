/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "datatype.h"
#include "datatype_internal.h"

int lam_ddt_destroy( dt_desc_t** dt )
{
   dt_desc_t* pData = *dt;

   if( pData->flags & DT_FLAG_FOREVER )
      return LAM_ERROR;

   OBJ_RELEASE( pData );
   *dt = NULL;
   return 0;
}
