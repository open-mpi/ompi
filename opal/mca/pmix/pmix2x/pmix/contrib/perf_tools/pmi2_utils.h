#ifndef PMI2_UTILS_H
#define PMI2_UTILS_H

#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>

char *pmi_encode(const void *val, size_t vallen);
uint8_t *pmi_decode (const char *data, size_t *retlen);

#endif // PMI2_UTILS_H
