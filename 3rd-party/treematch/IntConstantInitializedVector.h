#ifndef INTEGER_CONSTANT_INITIALIZED_VECTOR
#define INTEGER_CONSTANT_INITIALIZED_VECTOR

#include "ompi_config.h"

typedef struct int_CIVector_
{
  int init_value, size, top, *to, *from, *vec;
} int_CIVector;

OMPI_HIDDEN int tm_intCIV_isInitialized(int_CIVector * v, int i);
OMPI_HIDDEN void tm_intCIV_init(int_CIVector * v, int size, int init_value);
OMPI_HIDDEN int tm_intCIV_set(int_CIVector * v, int i, int val);
OMPI_HIDDEN int tm_intCIV_get(int_CIVector * v, int i);

#endif /*INTEGER_CONSTANT_INITIALIZED_VECTOR*/
