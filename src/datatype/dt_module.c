/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "datatype.h"
#include "datatype_internal.h"

struct _c_l {
   char c;
   long l;
};

struct _c_d {
   char c;
   double d;
};

struct _c_ll {
   char c;
   long long ll;
};

struct _c_ld {
   char c;
   long double ld;
};

struct _c_f {
   char c;
   float f;
};

int dt_load( void )
{
   /* we have to compute the correct alignement for several types of basic datatypes */
   struct _c_f  c_f;
   struct _c_l c_l;
   struct _c_d c_d;
   struct _c_ll c_ll;
   struct _c_ld c_ld;
   int i;

   basicDatatypes[DT_FLOAT].align = (char*)&(c_f.f) - (char*)&(c_f);
   basicDatatypes[DT_LONG].align = (char*)&(c_l.l) - (char*)&(c_l);
   basicDatatypes[DT_DOUBLE].align = (char*)&(c_d.d) - (char*)&(c_d);
   basicDatatypes[DT_LONG_DOUBLE].align = (char*)&(c_ld.ld) - (char*)&(c_ld);
   basicDatatypes[DT_LONG_LONG].align = (char*)&(c_ll.ll) - (char*)&(c_ll);

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

int dt_unload( void )
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
