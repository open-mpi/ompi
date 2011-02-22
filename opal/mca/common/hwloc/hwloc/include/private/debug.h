/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2010 INRIA
 * Copyright © 2009 Université Bordeaux 1
 * See COPYING in top-level directory.
 */

/* The configuration file */

#ifndef HWLOC_DEBUG_H
#define HWLOC_DEBUG_H

#include <private/config.h>

#ifdef HWLOC_DEBUG
#define hwloc_debug(s, ...) fprintf(stderr, s, ##__VA_ARGS__)
#define hwloc_debug_bitmap(fmt, bitmap) do { \
  char *s= hwloc_bitmap_printf_value(bitmap); \
  fprintf(stderr, fmt, s); \
  free(s); \
} while (0)
#define hwloc_debug_1arg_bitmap(fmt, arg1, bitmap) do { \
  char *s= hwloc_bitmap_printf_value(bitmap); \
  fprintf(stderr, fmt, arg1, s); \
  free(s); \
} while (0)
#define hwloc_debug_2args_bitmap(fmt, arg1, arg2, bitmap) do { \
  char *s= hwloc_bitmap_printf_value(bitmap); \
  fprintf(stderr, fmt, arg1, arg2, s); \
  free(s); \
} while (0)
#else
#define hwloc_debug(s, ...) do { } while(0)
#define hwloc_debug_bitmap(s, bitmap) do { } while(0)
#define hwloc_debug_1arg_bitmap(s, arg1, bitmap) do { } while(0)
#define hwloc_debug_2args_bitmap(s, arg1, arg2, bitmap) do { } while(0)
#endif

#endif /* HWLOC_DEBUG_H */
