/* -*- Mode: C; c-basic-offset:4 ; -*- */

#ifndef DATATYPE_INTERNAL_H_HAS_BEEN_INCLUDED
#define DATATYPE_INTERNAL_H_HAS_BEEN_INCLUDED

#if defined(VERBOSE)
#  define DUMP_STACK( PSTACK, STACK_POS, PDESC, NAME ) \
     dump_stack( (PSTACK), (STACK_POS), (PDESC), (NAME) )
#  if defined(__GNUC__)
#    define DUMP(ARGS...)            printf(ARGS)
#  else
#    if defined(ACCEPT_C99)
#      define DUMP( ARGS... )        printf(__VA_ARGS__)
#    else
#      define DUMP                   printf
#    endif  /* ACCEPT_C99 */
#  endif  /* __GNUC__ */
#else
#  define DUMP_STACK( PSTACK, STACK_POS, PDESC, NAME )
#  if defined(__GNUC__)
#    define DUMP(ARGS...)
#  else
#    if defined(ACCEPT_C99)
#      define DUMP(ARGS...)
#    else
       static void DUMP() { /* empty hopefully removed by the compiler */ }
#    endif  /* ACCEPT_C99 */
#  endif  /* __GNUC__ */
#endif  /* VERBOSE */

typedef struct {
      float r;
      float i;
} complex_float_t;

typedef struct {
      double r;
      double i;
} complex_double_t;

extern dt_desc_t basicDatatypes[DT_MAX_PREDEFINED];

/* mcaros to play with the flags */
#define REMOVE_FLAG( INT_VALUE, FLAG )  (INT_VALUE) = (INT_VALUE) ^ (FLAG)
#define SET_FLAG( INT_VALUE, FLAG )     (INT_VALUE) = (INT_VALUE) | (FLAG)
#define UNSET_FLAG( INT_VALUE, FLAG)    (INT_VALUE) = (INT_VALUE) & (~(FLAG))

#define REMOVE_CONTIGUOUS_FLAG( INT_VALUE )  REMOVE_FLAG(INT_VALUE, DT_FLAG_CONTIGUOUS)
#define SET_CONTIGUOUS_FLAG( INT_VALUE )     SET_FLAG(INT_VALUE, DT_FLAG_CONTIGUOUS)
#define UNSET_CONTIGUOUS_FLAG( INT_VALUE )   UNSET_FLAG(INT_VALUE, DT_FLAG_CONTIGUOUS)

#if defined(__GNUC__)
#define LMAX(A,B)  ({ long _a = (A), _b = (B); (_a < _b ? _b : _a); })
#define LMIN(A,B)  ({ long _a = (A), _b = (B); (_a < _b ? _a : _b); })
#define IMAX(A,B)  ({ int _a = (A), _b = (B); (_a < _b ? _b : _a); })
#define IMIN(A,B)  ({ int _a = (A), _b = (B); (_a < _b ? _a : _b); })
#else
static inline long LMAX( long a, long b ) { return ( a < b ? b : a ); }
static inline long LMIN( long a, long b ) { return ( a < b ? a : b ); }
static inline int  IMAX( int a, int b ) { return ( a < b ? b : a ); }
static inline int  IMIN( int a, int b ) { return ( a < b ? a : b ); }
#endif  /* __GNU__ */

extern conversion_fct_t copy_functions[DT_MAX_PREDEFINED];

extern void dump_stack( dt_stack_t* pStack, int stack_pos, dt_elem_desc_t* pDesc, char* name );
#define SAVE_STACK( PSTACK, INDEX, COUNT, DISP, END_LOOP) \
do { \
   (PSTACK)->index    = (INDEX); \
   (PSTACK)->count    = (COUNT); \
   (PSTACK)->disp     = (DISP); \
   (PSTACK)->end_loop = (END_LOOP); \
} while(0)

#define PUSH_STACK( PSTACK, STACK_POS, INDEX, COUNT, DISP, END_LOOP) \
do { \
   dt_stack_t* pTempStack = (PSTACK) + 1; \
   SAVE_STACK( pTempStack, (INDEX), (COUNT), (DISP), (END_LOOP) );  \
   (STACK_POS)++; \
   (PSTACK) = pTempStack; \
} while(0)

#define MEMCPY( DST, SRC, BLENGTH ) memcpy( (DST), (SRC), (BLENGTH) )

#ifdef USELESS
#define MEMCPY_LIMIT 1

#define MEMCPY( DST, SRC, BLENGTH ) \
do { \
   if( (BLENGTH) < (MEMCPY_LIMIT) ) { \
      long mask = sizeof(int) - 1; \
      char *dst = (char*)(DST), *src = (char*)(SRC); \
      int i; \
      if( ((long)(DST) & mask) == ((long)(SRC) & mask) ) { \
         int *idst = (int*)((long)(DST) & (~mask)); \
         int *isrc = (int*)((long)(SRC) & (~mask)); \
         for( i = 0; i < ((long)(DST) & mask); i++ ) { \
            *dst = *src; dst++; src++; \
         } \
         if( ((char*)idst) != dst ) { \
            idst++; isrc++; \
         } \
         for( i = 0; i < ((BLENGTH) >> 2); i++ ) { \
            *idst = *isrc; idst++; isrc++; \
         } \
      } else { \
         for( i = 0; i < (BLENGTH); i++ ) { \
            *dst = *src; dst++; src++; \
         } \
      } \
   } else \
      memcpy( (DST), (SRC), (BLENGTH) ); \
} while(0)
#endif  /* USELESS */

#endif  /* DATATYPE_INTERNAL_H_HAS_BEEN_INCLUDED */
