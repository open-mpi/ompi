#ifndef  OPAL_UTIL_ASSERT_H
#define  OPAL_UTIL_ASSERT_H

#include "opal/include/opal_config.h"
#include "opal/prefetch.h"

#include <assert.h>
#include <stdio.h>
#include <unistd.h>

OPAL_DECLSPEC extern bool opal_assert_core_enabled;

#define OPAL_ASSERT_INTERNAL(cond, cond_str) {                   \
  if(OPAL_UNLIKELY((false == ((bool) (cond))))) {                \
      if(OPAL_LIKELY(true == opal_assert_core_enabled)) {        \
          assert(cond);                                          \
      }                                                          \
      else {                                                     \
          fprintf(stderr, "ERROR: %s:%d: %s(): Assertion  \'%s\' failed.\n", __FILE__, __LINE__, __func__, cond_str);              \
          fprintf(stderr, "WARNING: No core file will be generated.\n");                                                           \
          fprintf(stderr, "If you would like to generate a core file, please rerun with: \'--mca opal_enable_assert_core 1\'\n");  \
          fprintf(stderr, "Exiting.\n");  \
          fflush(stderr);                 \
          _exit(1);                       \
      }                                   \
  }                                       \
}

#define OPAL_ASSERT(cond) {               \
    OPAL_ASSERT_INTERNAL(cond, #cond)     \
}

#endif
