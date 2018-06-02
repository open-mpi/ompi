/*
 * Copyright © 2011 Université Bordeaux
 * Copyright © 2016 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_AIX_SYS_SYSTEMCFG_H
#define HWLOC_PORT_AIX_SYS_SYSTEMCFG_H

struct {
  int dcache_size;
  int dcache_asc;
  int dcache_line;
  int icache_size;
  int icache_asc;
  int icache_line;
  int L2_cache_size;
  int L2_cache_asc;
  int cache_attrib;
} _system_configuration;

#define __power_pc() 1
#define __power_4() 1
#define __power_5() 1
#define __power_6() 1
#define __power_7() 1

/* hide Linux' host disabling _SC_LARGE_PAGESIZE */
#undef HAVE_DECL__SC_LARGE_PAGESIZE
#define HAVE_DECL__SC_LARGE_PAGESIZE 1
#undef _SC_LARGE_PAGESIZE
#define _SC_LARGE_PAGESIZE 33

#endif /* HWLOC_PORT_AIX_SYS_SYSTEMCFG_H */
