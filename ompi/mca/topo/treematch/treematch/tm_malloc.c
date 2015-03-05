#include "uthash.h"
#include <stdio.h>
#include "tm_verbose.h"
#include "tm_malloc.h"

#define EXTRA_BYTE 100

typedef signed char  byte;


/* static int verbose_level = ERROR;*/

typedef struct _hash_t {
    void *key;            /* we'll use this field as the key */
    size_t size;
    UT_hash_handle hh; /* makes this structure hashable */
}hash_t;

static hash_t *size_hash = NULL;
static char extra_data[EXTRA_BYTE];

static void save_size(void *ptr, size_t size);
static size_t retreive_size(void *someaddr);
static void init_extra_data(void);

void save_size(void *ptr, size_t size) {
  hash_t *elem;
  elem = (hash_t*) malloc(sizeof(hash_t));
  elem -> key = ptr;
  elem -> size = size;
  if(get_verbose_level() >= DEBUG)
    printf("Storing (%p,%ld)\n",ptr,size);
  HASH_ADD_PTR( size_hash, key, elem );
}


size_t retreive_size(void *someaddr){
  size_t res;
  hash_t *elem = NULL;
  HASH_FIND_PTR(size_hash, &someaddr, elem);
  if(!elem){
    fprintf(stderr,"cannot find ptr %p to free!\n",someaddr);
    return 0;
  }

  res  = elem->size;
  if(get_verbose_level()>=DEBUG)
    printf("Retreiving (%p,%ld)\n",someaddr, res);

  HASH_DEL( size_hash, elem);
  return res;
}

void my_mem_check(void){
    hash_t  *s;
    int nb_errors = 0;
    for(s=size_hash; s != NULL; s=s->hh.next) {
      if(get_verbose_level()>=ERROR)
        printf("pointer %p of size %ld has not been freed!\n", s->key, s->size);
	nb_errors ++;
    }

    if(get_verbose_level() >= INFO)
      printf ("Number of errors in managing memory: %d\n",nb_errors);
}

void init_extra_data(void){
  static int done = 0;
  int i;

  if(done)
    return;

  srandom(0);

  for( i = 0 ; i < EXTRA_BYTE; i++)
    extra_data[i] = (char) random() % 256;

  done = 1;
}


void *my_malloc(size_t size, char *file, int line){
  byte *ptr;
  init_extra_data();

  size+=2*EXTRA_BYTE;
  ptr = malloc(size);

  if(get_verbose_level()>=DEBUG)
    printf("my_malloc of size %ld: %p (%s: %d)\n",size-2*EXTRA_BYTE,ptr,file,line);

  save_size(ptr,size);

  memcpy(ptr, extra_data, EXTRA_BYTE);
  memcpy(ptr + size - EXTRA_BYTE, extra_data, EXTRA_BYTE);


  if(get_verbose_level()>=DEBUG)
    printf("my_malloc returning: %p\n",ptr+EXTRA_BYTE);

  return (void *)(ptr + EXTRA_BYTE);
}

void *my_calloc(size_t count, size_t size, char *file, int line){
  byte *ptr;
  size_t full_size;

  init_extra_data();

  full_size = count * size + 2 * EXTRA_BYTE;

  ptr = malloc(full_size);
  bzero(ptr,full_size);
  save_size(ptr, full_size);

  if(get_verbose_level()>=DEBUG)
    printf("my_calloc of size %ld: %p (%s: %d)\n",full_size-2*EXTRA_BYTE,ptr, file, line);


  memcpy(ptr, extra_data, EXTRA_BYTE);
  memcpy(ptr + full_size - EXTRA_BYTE, extra_data, EXTRA_BYTE);

  if(get_verbose_level()>=DEBUG)
    printf("my_calloc returning: %p\n",ptr+EXTRA_BYTE);

  return (void *)(ptr+EXTRA_BYTE);
}

void my_free(void *ptr){
  byte *original_ptr = ((byte *)ptr) - EXTRA_BYTE;
  size_t size;

  if(!ptr)
    return;

  size = retreive_size(original_ptr);

  if((bcmp(original_ptr ,extra_data, EXTRA_BYTE)) && ((get_verbose_level()>=ERROR))){
    fprintf(stderr,"cannot find special string ***before*** %p!\n",ptr);
    fprintf(stderr,"memory is probably corrupted here!\n");
  }

  if((bcmp(original_ptr + size -EXTRA_BYTE ,extra_data, EXTRA_BYTE)) && ((get_verbose_level()>=ERROR))){
    fprintf(stderr,"cannot find special string ***after*** %p!\n",ptr);
    fprintf(stderr,"memory is probably corrupted here!\n");
  }

  if(get_verbose_level()>=DEBUG)
    printf("my_free freeing: %p\n",original_ptr);


  free(original_ptr);
}



