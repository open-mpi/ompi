/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "datatype.h"
#include "limits.h"

/* other fields starting after bdt_used (index of DT_LOOP should be ONE) */
#define EMPTY_DATA NULL, "", {0, 0, NULL}, {0, 0, NULL}, NULL, { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
#define BASEOBJ_DATA { NULL, 1 }
dt_desc_t basicDatatypes[] = {
         /*super         size                 true_lb  true_ub              align                      lb ub                    flags                           id                 nbElems  bdt_used                others fields */
/*0x00*/ { BASEOBJ_DATA, 0,                   0,       0,                   0,                         0, 0,                   DT_FLAG_BASIC,                   DT_LOOP,           1,       (1<<DT_LOOP),           EMPTY_DATA },
/*0x01*/ { BASEOBJ_DATA, 0,                   0,       0,                   0,                         0, 0,                   DT_FLAG_BASIC | DT_FLAG_USER_LB, DT_LB,             1,       (1<<DT_LB),             EMPTY_DATA },
/*0x02*/ { BASEOBJ_DATA, 0,                   0,       0,                   0,                         0, 0,                   DT_FLAG_BASIC | DT_FLAG_USER_UB, DT_UB,             1,       (1<<DT_UB),             EMPTY_DATA },
/* now the real basic datatypes */                                                                                                                                           
/*0x03*/ { BASEOBJ_DATA, 1,                   0,       1,                   1,                         0, 1,                   DT_FLAG_BASIC | DT_FLAG_DATA,    DT_SPACE,          1,       (1<<DT_SPACE),          EMPTY_DATA },
/*0x04*/ { BASEOBJ_DATA, 1,                   0,       1,                   LAM_ALIGNMENT_CHAR,        0, 1,                   DT_FLAG_BASIC | DT_FLAG_DATA,    DT_CHAR,           1,       (1<<DT_CHAR),           EMPTY_DATA },
/*0x05*/ { BASEOBJ_DATA, 1,                   0,       1,                   LAM_ALIGNMENT_CHAR,        0, 1,                   DT_FLAG_BASIC | DT_FLAG_DATA,    DT_BYTE,           1,       (1<<DT_BYTE),           EMPTY_DATA },
/*0x06*/ { BASEOBJ_DATA, sizeof(short),       0,       sizeof(short),       LAM_ALIGNMENT_SHORT,       0, sizeof(short),       DT_FLAG_BASIC | DT_FLAG_DATA,    DT_SHORT,          1,       (1<<DT_SHORT),          EMPTY_DATA },
/*0x07*/ { BASEOBJ_DATA, sizeof(int),         0,       sizeof(int),         LAM_ALIGNMENT_INT,         0, sizeof(int),         DT_FLAG_BASIC | DT_FLAG_DATA,    DT_INT,            1,       (1<<DT_INT),            EMPTY_DATA },
/*0x08*/ { BASEOBJ_DATA, sizeof(float),       0,       sizeof(float),       LAM_ALIGNMENT_FLOAT,       0, sizeof(float),       DT_FLAG_BASIC | DT_FLAG_DATA,    DT_FLOAT,          1,       (1<<DT_FLOAT),          EMPTY_DATA },
/*0x09*/ { BASEOBJ_DATA, sizeof(long),        0,       sizeof(long),        LAM_ALIGNMENT_LONG,        0, sizeof(long),        DT_FLAG_BASIC | DT_FLAG_DATA,    DT_LONG,           1,       (1<<DT_LONG),           EMPTY_DATA },
/*0x0A*/ { BASEOBJ_DATA, sizeof(double),      0,       sizeof(double),      LAM_ALIGNMENT_DOUBLE,      0, sizeof(double),      DT_FLAG_BASIC | DT_FLAG_DATA,    DT_DOUBLE,         1,       (1<<DT_DOUBLE),         EMPTY_DATA },
/*0x0B*/ { BASEOBJ_DATA, sizeof(long long),   0,       sizeof(long long),   LAM_ALIGNMENT_LONG_LONG,   0, sizeof(long long),   DT_FLAG_BASIC | DT_FLAG_DATA,    DT_LONG_LONG,      1,       (1<<DT_LONG_LONG),      EMPTY_DATA },
/*0x0C*/ { BASEOBJ_DATA, sizeof(long double), 0,       sizeof(long double), LAM_ALIGNMENT_LONG_DOUBLE, 0, sizeof(long double), DT_FLAG_BASIC | DT_FLAG_DATA,    DT_LONG_DOUBLE,    1,       (1<<DT_LONG_DOUBLE),    EMPTY_DATA },
/*0x0D*/ { BASEOBJ_DATA, 2 * sizeof(float),   0,       2 * sizeof(float),   2 * LAM_ALIGNMENT_FLOAT,   0, 2 * sizeof(float),   DT_FLAG_BASIC | DT_FLAG_DATA,    DT_COMPLEX_FLOAT,  1,       (1<<DT_COMPLEX_FLOAT),  EMPTY_DATA },
/*0x0E*/ { BASEOBJ_DATA, 2 * sizeof(double),  0,       2 * sizeof(double),  2 * LAM_ALIGNMENT_DOUBLE,  0, 2 * sizeof(double),  DT_FLAG_BASIC | DT_FLAG_DATA,    DT_COMPLEX_DOUBLE, 1,       (1<<DT_COMPLEX_DOUBLE), EMPTY_DATA },
/*0x0F*/ { BASEOBJ_DATA, 0,                   0,       0,                   0,                         0, 0,                   DT_FLAG_BASIC,                   DT_END_LOOP,       1,       (1<<DT_END_LOOP),       EMPTY_DATA },
};

static char* basicDatatypeNames[] = { "loop", "lb", "ub", "space", "char", "byte", "short", "int", "float",
                                      "long", "double", "long_long", "long_double", "cfloat", "cdouble", "end_loop", "unknown" };

typedef struct __internal_keep_ptr {
   struct __internal_keep_ptr* next;
} internal_dt_desc_t;

static internal_dt_desc_t* __free_dt_desc = NULL;

static void __get_free_dt_struct(  dt_desc_t* pData )
{
   int i;

   pData->size            = 0;
   pData->id              = 0;
   pData->nbElems         = 0;
   pData->bdt_used        = 0;
   for( i = 0; i < DT_MAX_PREDEFINED; i++ )
      pData->btypes[i]    = 0;
   pData->btypes[DT_LOOP] = 1;

   pData->opt_desc.desc   = NULL;
   pData->opt_desc.length = 0;
   pData->opt_desc.used   = 0;
   pData->args            = NULL;
   pData->align           = 1;
   pData->flags           = DT_FLAG_CONTIGUOUS;
   pData->true_lb         = LONG_MAX;
   pData->true_ub         = LONG_MIN;
   pData->lb              = LONG_MAX;
   pData->ub              = LONG_MIN;
}
OBJ_CLASS_INSTANCE(lam_datatype_t, lam_object_t, __get_free_dt_struct, lam_ddt_destroy );

dt_desc_t* lam_ddt_create( int expectedSize )
{
   dt_desc_t* pdt = (dt_desc_t*)OBJ_NEW(lam_datatype_t);

   if( expectedSize == -1 ) expectedSize = DT_INCREASE_STACK;
   pdt->desc.length = expectedSize;
   pdt->desc.used   = 0;
   pdt->desc.desc   = (dt_elem_desc_t*)calloc(pdt->desc.length, sizeof(dt_elem_desc_t));
   return pdt;
}

int lam_ddt_create_resized( dt_desc_t* oldType, long lb, long extent, dt_desc_t** newType )
{
   lam_ddt_duplicate( oldType, newType );
   (*newType)->lb = lb;
   (*newType)->ub = lb + extent;
   return 0;
}

int lam_ddt_commit( dt_desc_t** data )
{
   dt_desc_t* pData = (dt_desc_t*)*data;

   if( pData->flags & DT_FLAG_COMMITED ) return -1;
   pData->flags |= DT_FLAG_COMMITED;
   /* If the data is contiguous is useless to generate an optimized version. */
   if( pData->size != (pData->true_ub - pData->true_lb) )
      (void)dt_optimize_short( pData, 1, &(pData->opt_desc) );
   return 0;
}

static void _dump_data_flags( unsigned short usflags )
{
   char flags[12] = "-----------";

   if( usflags & DT_FLAG_DESTROYED )                flags[0]  = 'd';
   if( usflags & DT_FLAG_COMMITED )                 flags[1]  = 'c';
   if( usflags & DT_FLAG_CONTIGUOUS )               flags[2]  = 'C';
   if( usflags & DT_FLAG_OVERLAP )                  flags[3]  = 'o';
   if( usflags & DT_FLAG_USER_LB )                  flags[4]  = 'l';
   if( usflags & DT_FLAG_USER_UB )                  flags[5]  = 'u';
   if( usflags & DT_FLAG_FOREVER )                  flags[6]  = 'F';
   if( usflags & DT_FLAG_IN_LOOP )                  flags[7]  = 'L';
   if( usflags & DT_FLAG_DATA )                     flags[8]  = 'D';
   if( usflags & DT_FLAG_INITIAL )                  flags[9]  = 'I';
   if( (usflags & DT_FLAG_BASIC) == DT_FLAG_BASIC ) flags[10] = 'B';
   flags[11] = 0;
   printf( "%s\t", flags );
}

int __dump_data_desc( dt_elem_desc_t* pDesc, int nbElems )
{
   char* dtName;
   int i;

   for( i = 0; i < nbElems; i++ ) {
      if( pDesc->type > DT_MAX_PREDEFINED ) dtName = basicDatatypeNames[DT_MAX_PREDEFINED];
      else dtName = basicDatatypeNames[pDesc->type];
      _dump_data_flags( pDesc->flags );
      if( pDesc->type == DT_LOOP )
         printf( "%15s %d times the next %d elements extent %d\n", dtName,
                 pDesc->count, (int)pDesc->disp, pDesc->extent );
      else
         printf( "%15s count %d disp 0x%lx (%ld) extent %d\n", dtName,
                 pDesc->count, pDesc->disp, pDesc->disp, pDesc->extent );
      pDesc++;
   }
   return 0;
}

void __dt_contain_basic_datatypes( dt_desc_t* pData )
{
   int i, mask = 1;

   if( pData->flags & DT_FLAG_USER_LB ) printf( "lb " );
   if( pData->flags & DT_FLAG_USER_UB ) printf( "ub " );
   for( i = 0; i < DT_MAX_PREDEFINED; i++ ) {
      if( pData->bdt_used & mask )
         printf( "%s ", basicDatatypeNames[i] );
      mask <<= 1;
   }
}

void lam_ddt_dump( dt_desc_t* data )
{
   dt_desc_t* pData = (dt_desc_t*)data;

   printf( "Datatype %p size %d align %d id %d length %d used %d\n\
   true_lb %ld true_ub %ld (true_extent %ld) lb %ld ub %ld (extent %ld)\n\
   nbElems %d loops %d flags %X (",
           pData, pData->size, pData->align, pData->id, pData->desc.length, pData->desc.used,
           pData->true_lb, pData->true_ub, pData->true_ub - pData->true_lb,
           pData->lb, pData->ub, pData->ub - pData->lb,
           pData->nbElems, pData->btypes[DT_LOOP], pData->flags );
   /* dump the flags */
   if( pData->flags == DT_FLAG_BASIC ) printf( "basic datatype " );
   else {
      if( pData->flags & DT_FLAG_DESTROYED ) printf( "destroyed " );
      if( pData->flags & DT_FLAG_COMMITED ) printf( "commited " );
      if( pData->flags & DT_FLAG_CONTIGUOUS) printf( "contiguous " );
   }
   printf( ")" ); _dump_data_flags( pData->flags );
   printf( "\n   contain " ); __dt_contain_basic_datatypes( pData ); printf( "\n" );
   __dump_data_desc( pData->desc.desc, pData->desc.used );
   if( pData->opt_desc.desc != NULL ) {
      printf( "Optimized description \n" );
      __dump_data_desc( pData->opt_desc.desc, pData->opt_desc.used );
   }
}
#define DUMP_TYPE( TYPENAME, TYPE ) \
static int dump_##TYPENAME( unsigned int count, \
                            char* from, unsigned int from_len, long from_extent, \
                            char* to, unsigned int to_len, long to_extent, \
                            int* used )                                 \
{ \
   int remote_type_size = sizeof(TYPE), res = 1; \
   if( (remote_type_size * count) > from_len ) { \
      count = from_len / remote_type_size; \
      if( (count * remote_type_size) != from_len ) { \
         printf( "oops should I keep this data somewhere (excedent %d bytes)?\n", \
                 from_len - (count * remote_type_size) ); \
         res = -1; \
      } \
      printf( "correct: %s count %d from %p with length %d to %p space %d\n", \
              #TYPE, count, from, from_len, to, to_len );      \
   } else \
      printf( "         %s count %d from %p with length %d to %p space %d\n", \
              #TYPE, count, from, from_len, to, to_len );      \
 \
   *used = count * to_extent; \
   return res * count; \
}

DUMP_TYPE( char, char );
DUMP_TYPE( short, short );
DUMP_TYPE( int, int );
DUMP_TYPE( float, float );
DUMP_TYPE( long, long );
DUMP_TYPE( double, double );
DUMP_TYPE( long_long, long long );
DUMP_TYPE( long_double, long double );
DUMP_TYPE( complex_float, complex_float_t );
DUMP_TYPE( complex_double, complex_double_t );

static convertor_t* pDumpConv = NULL;

static conversion_fct_t dump_functions[] = {
   (conversion_fct_t)NULL,                 /* DT_LOOP           */
   (conversion_fct_t)NULL,                 /* DT_LB             */
   (conversion_fct_t)NULL,                 /* DT_UB             */
   (conversion_fct_t)NULL,                 /* DT_SPACE          */
   (conversion_fct_t)dump_char,            /* DT_CHAR           */
   (conversion_fct_t)dump_char,            /* DT_BYTE           */
   (conversion_fct_t)dump_short,           /* DT_SHORT          */
   (conversion_fct_t)dump_int,             /* DT_INT            */
   (conversion_fct_t)dump_float,           /* DT_FLOAT          */
   (conversion_fct_t)dump_long,            /* DT_LONG           */
   (conversion_fct_t)dump_double,          /* DT_DOUBLE         */
   (conversion_fct_t)dump_long_long,       /* DT_LONG_LONG      */
   (conversion_fct_t)dump_long_double,     /* DT_LONG_DOUBLE    */
   (conversion_fct_t)dump_complex_float,   /* DT_COMPLEX_FLOAT  */
   (conversion_fct_t)dump_complex_double,  /* DT_COMPLEX_DOUBLE */
};

void lam_ddt_dump_complete( dt_desc_t* data )
{
   dt_desc_t* pData = (dt_desc_t*)data;
   struct iovec fake = { (void*)0, 0 };

   printf( "Datatype %p size %d align %d id %d length %d used %d\n\
   true_lb %ld true_ub %ld (true_extent %ld) lb %ld ub %ld (extent %ld)\n\
   nbElems %d loops %d flags %X (",
           pData, pData->size, pData->align, pData->id, pData->desc.length, pData->desc.used,
           pData->true_lb, pData->true_ub, pData->true_ub - pData->true_lb,
           pData->lb, pData->ub, pData->ub - pData->lb,
           pData->nbElems, pData->btypes[DT_LOOP], pData->flags );

   /* dump the flags */
   if( pData->flags == DT_FLAG_BASIC ) printf( "basic datatype " );
   else {
      if( pData->flags & DT_FLAG_DESTROYED ) printf( "destroyed " );
      if( pData->flags & DT_FLAG_COMMITED ) printf( "commited " );
      if( pData->flags & DT_FLAG_CONTIGUOUS) printf( "contiguous " );
      if( pData->flags & DT_FLAG_OVERLAP ) printf( "overlap " );
   }
   printf( ")\n   contain " ); __dt_contain_basic_datatypes( pData );
   printf( "\n{\n" );
   if( pDumpConv == NULL ) {
      pDumpConv = lam_convertor_create( 0, 0 );
   }
   convertor_init_for_recv( pDumpConv, 0, pData, 1, NULL );
   pDumpConv->pFunctions = dump_functions;

   fake.iov_len = pData->size;
   convertor_unpack( pDumpConv, &fake, 1 );
   printf( "}\n" );
}
