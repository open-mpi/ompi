#include "ompi_config.h"
#if !defined(NDEBUG)
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

typedef struct __mem_allocator smemAllocator_t;

#define VALIDATOR 0xdeadbeaf
/* #define EXTREME_DEBUG */
#ifdef EXTREME_DEBUG
#define ZEROS_FOLLOW 1024
#else
#define ZEROS_FOLLOW 0
#endif  /* EXTREME_DEBUG */
#define VALUE_CHAR   0xbe

struct __mem_allocator {
  smemAllocator_t* prev;
  smemAllocator_t* next;
  unsigned int lineno;
  unsigned int validator;
  char* pFileName;
  unsigned int length;
  int index;
};

static int __alloc_index = 0;
static smemAllocator_t* pdllmem = NULL;

#ifdef EXTREME_DEBUG
static int __overflow_detection( smemAllocator_t* pTemp )
{
   int i;
   unsigned char* pchar = ((char*)&pTemp[1]) + pTemp->length;

   for( i = 0; i < ZEROS_FOLLOW; i++ ) {
      if( pchar[i] != VALUE_CHAR ) {
         printf( "buffer overflow detected at position %d(%p) on memory allocated in %s at line %d (pointer %p size %d) \n",
                 i, pchar + i, pTemp->pFileName, pTemp->lineno, ((char*)pTemp) + sizeof(smemAllocator_t), pTemp->length );
         return -1;
      }
   }
   return 0;
}
#endif  /* EXTREME_DEBUG */

void* ftmpi_malloc( size_t size, char* file, int lineno )
{
   size_t totalLength = 0;
   smemAllocator_t* pTemp;

   totalLength = sizeof( smemAllocator_t ) + size + ZEROS_FOLLOW;
   pTemp = (smemAllocator_t*)malloc( totalLength );
   if( pTemp == NULL ) return NULL;
   pTemp->length = size;
   pTemp->validator = VALIDATOR;
   pTemp->pFileName = file;
   pTemp->lineno = lineno;
   pTemp->index = __alloc_index++;

   if( pdllmem == NULL ) {
      pdllmem = pTemp;
      pTemp->next = pTemp;
      pTemp->prev = pTemp;
   } else {
      pdllmem->prev->next = pTemp;
      pTemp->prev = pdllmem->prev;
      pTemp->next = pdllmem;
      pdllmem->prev = pTemp;
   }
#ifdef EXTREME_DEBUG
   memset( ((char*)pTemp) + sizeof(smemAllocator_t) + pTemp->length, VALUE_CHAR, ZEROS_FOLLOW );
#endif  /* EXTREME_DEBUG */
   return (void*)&pTemp[1];
}

void* ftmpi_calloc( size_t number, size_t size, char* file, int lineno )
{
   size_t totalLength = 0;
   smemAllocator_t* pTemp;

   totalLength = sizeof( smemAllocator_t ) + number * size + ZEROS_FOLLOW;
   pTemp = (smemAllocator_t*)calloc( 1, totalLength );
   pTemp->length = number * size;
   pTemp->validator = VALIDATOR;
   pTemp->pFileName = file;
   pTemp->lineno = lineno;
   pTemp->index = __alloc_index++;

   if( pdllmem == NULL ) {
      pdllmem = pTemp;
      pTemp->next = pTemp;
      pTemp->prev = pTemp;
   } else {
      pdllmem->prev->next = pTemp;
      pTemp->prev = pdllmem->prev;
      pTemp->next = pdllmem;
      pdllmem->prev = pTemp;
   }
#ifdef EXTREME_DEBUG
   memset( ((char*)pTemp) + sizeof(smemAllocator_t) + pTemp->length, VALUE_CHAR, ZEROS_FOLLOW );
#endif  /* EXTREME_DEBUG */
   return (void*)&pTemp[1];
}

void* ftmpi_realloc( void* ptr, size_t size, char* file, int lineno )
{
   size_t totalLength = 0;
   smemAllocator_t* pTemp = (smemAllocator_t*)((char*)ptr - sizeof(smemAllocator_t));
   smemAllocator_t *prev, *next, *old;
  
   prev = pTemp->prev;
   next = pTemp->next;
   old = pTemp;

#ifdef EXTREME_DEBUG
   if( __overflow_detection( pTemp ) != 0 ) assert(0);
#endif  /* EXTREME_DEBUG */
   totalLength = sizeof( smemAllocator_t ) + size + ZEROS_FOLLOW;
   pTemp = (smemAllocator_t*)realloc( pTemp, totalLength );
   pTemp->length = size;
   pTemp->validator = VALIDATOR;
   pTemp->pFileName = file;
   pTemp->lineno = lineno;
   pTemp->index = __alloc_index++;

   if( pdllmem == old ) {
      if( prev == old ) {
         assert( next == old );
         prev = next = pTemp;
      }
      pdllmem = pTemp;
   }
   prev->next = pTemp;
   next->prev = pTemp;
   pTemp->prev = prev;
   pTemp->next = next;
#ifdef EXTREME_DEBUG
   memset( ((char*)pTemp) + sizeof(smemAllocator_t) + pTemp->length, VALUE_CHAR, ZEROS_FOLLOW );
#endif  /* EXTREME_DEBUG */
   return (void*)&pTemp[1];
}

void ftmpi_free( void* ptr, char* file, int lineno )
{
   smemAllocator_t* pTemp = (smemAllocator_t*)((char*)ptr - sizeof(smemAllocator_t));

   /* remove it from the allocated memory */
#ifdef EXTREME_DEBUG
   if( __overflow_detection( pTemp ) != 0 ) assert(0);
#endif  /* EXTREME_DEBUG */
   assert( pTemp->prev->next == pTemp );
   assert( pTemp->next->prev == pTemp );
   pTemp->prev->next = pTemp->next;
   pTemp->next->prev = pTemp->prev;
   if( pTemp == pdllmem ) {
      if( pTemp->next == pTemp ) {
         assert( pTemp->prev == pTemp );
         pdllmem = NULL;
      } else {
         pdllmem = pTemp->next;
      }
   }
   assert( pTemp->validator == VALIDATOR );
   pTemp->validator = 0;
   /* keep this informations to see where the memory has been freed */
   pTemp->pFileName = file;
   pTemp->lineno = lineno;
  
   /* And now really free the memory */
   free( pTemp );
}

void ftmpi_display_memory_usage( void )
{
   smemAllocator_t* pTemp = pdllmem;
   int totalSize = 0, chunks = 0;

   if( pTemp == NULL ) return;
   printf( ">> BEGIN MEMORY USAGE and TRACE\n" );
   do {
#ifdef EXTREME_DEBUG
      if( __overflow_detection( pTemp ) != 0 ) assert(0);
#endif  /* EXTREME_DEBUG */
      printf( "Allocate %d bytes in file %s at line %d pointer %p index %d\n",
              pTemp->length, pTemp->pFileName, pTemp->lineno,
              ((char*)pTemp + sizeof(smemAllocator_t)), pTemp->index );
      chunks++;
      totalSize += pTemp->length;
      pTemp = pTemp->next;
   } while( pTemp != pdllmem );
   printf( "Allocated %d chunks of memory with the total size of %d bytes\n",
           chunks, totalSize );
   printf( ">> END MEMORY USAGE and TRACE\n" );
}

#endif  /* NDEBUG */
