/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

/* The configuration file */

#ifndef HWLOC_DEBUG_H
#define HWLOC_DEBUG_H

#include <private/config.h>

#ifdef HWLOC_DEBUG
#define hwloc_debug(s, ...) fprintf(stderr, s, ##__VA_ARGS__)
#define hwloc_debug_cpuset(fmt, cpuset) do { \
  char *s= hwloc_cpuset_printf_value(cpuset); \
  fprintf(stderr, fmt, s); \
  free(s); \
} while (0)
#define hwloc_debug_1arg_cpuset(fmt, arg1, cpuset) do { \
  char *s= hwloc_cpuset_printf_value(cpuset); \
  fprintf(stderr, fmt, arg1, s); \
  free(s); \
} while (0)
#define hwloc_debug_2args_cpuset(fmt, arg1, arg2, cpuset) do { \
  char *s= hwloc_cpuset_printf_value(cpuset); \
  fprintf(stderr, fmt, arg1, arg2, s); \
  free(s); \
} while (0)
#else
#define hwloc_debug(s, ...) do { } while(0)
#define hwloc_debug_cpuset(s, cpuset) do { } while(0)
#define hwloc_debug_1arg_cpuset(s, arg1, cpuset) do { } while(0)
#define hwloc_debug_2args_cpuset(s, arg1, arg2, cpuset) do { } while(0)
#endif

#endif /* HWLOC_DEBUG_H */
