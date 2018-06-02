/*
 * Copyright © 2010-2018 Inria.  All rights reserved.
 * Copyright © 2010-2013 Université Bordeaux
 * Copyright © 2010-2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 *
 *
 * This backend is only used when the operating system does not export
 * the necessary hardware topology information to user-space applications.
 * Currently, only the FreeBSD backend relies on this x86 backend.
 *
 * Other backends such as Linux have their own way to retrieve various
 * pieces of hardware topology information from the operating system
 * on various architectures, without having to use this x86-specific code.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <private/private.h>
#include <private/debug.h>
#include <private/misc.h>

#include <private/cpuid-x86.h>

#include <sys/types.h>
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
#ifdef HAVE_VALGRIND_VALGRIND_H
#include <valgrind/valgrind.h>
#endif

struct hwloc_x86_backend_data_s {
  unsigned nbprocs;
  hwloc_bitmap_t apicid_set;
  int apicid_unique;
  char *src_cpuiddump_path;
  int is_knl;
};

/************************************
 * Management of cpuid dump as input
 */

struct cpuiddump {
  unsigned nr;
  struct cpuiddump_entry {
    unsigned inmask; /* which of ine[abcd]x are set on input */
    unsigned ineax;
    unsigned inebx;
    unsigned inecx;
    unsigned inedx;
    unsigned outeax;
    unsigned outebx;
    unsigned outecx;
    unsigned outedx;
  } *entries;
};

static void
cpuiddump_free(struct cpuiddump *cpuiddump)
{
  if (cpuiddump->nr)
    free(cpuiddump->entries);
  free(cpuiddump);
}

static struct cpuiddump *
cpuiddump_read(const char *dirpath, unsigned idx)
{
  struct cpuiddump *cpuiddump;
  struct cpuiddump_entry *cur;
  FILE *file;
  char line[128];
  unsigned nr;

  cpuiddump = malloc(sizeof(*cpuiddump));
  if (!cpuiddump) {
    fprintf(stderr, "Failed to allocate cpuiddump for PU #%u, ignoring cpuiddump.\n", idx);
    goto out;
  }

 {
  size_t filenamelen = strlen(dirpath) + 15;
  HWLOC_VLA(char, filename, filenamelen);
  snprintf(filename, filenamelen, "%s/pu%u", dirpath, idx);
  file = fopen(filename, "r");
  if (!file) {
    fprintf(stderr, "Could not read dumped cpuid file %s, ignoring cpuiddump.\n", filename);
    goto out_with_dump;
  }
 }

  nr = 0;
  while (fgets(line, sizeof(line), file))
    nr++;
  cpuiddump->entries = malloc(nr * sizeof(struct cpuiddump_entry));
  if (!cpuiddump->entries) {
    fprintf(stderr, "Failed to allocate %u cpuiddump entries for PU #%u, ignoring cpuiddump.\n", nr, idx);
    goto out_with_file;
  }

  fseek(file, 0, SEEK_SET);
  cur = &cpuiddump->entries[0];
  nr = 0;
  while (fgets(line, sizeof(line), file)) {
    if (*line == '#')
      continue;
    if (sscanf(line, "%x %x %x %x %x => %x %x %x %x",
	      &cur->inmask,
	      &cur->ineax, &cur->inebx, &cur->inecx, &cur->inedx,
	      &cur->outeax, &cur->outebx, &cur->outecx, &cur->outedx) == 9) {
      cur++;
      nr++;
    }
  }

  cpuiddump->nr = nr;
  fclose(file);
  return cpuiddump;

 out_with_file:
  fclose(file);
 out_with_dump:
  free(cpuiddump);
 out:
  return NULL;
}

static void
cpuiddump_find_by_input(unsigned *eax, unsigned *ebx, unsigned *ecx, unsigned *edx, struct cpuiddump *cpuiddump)
{
  unsigned i;

  for(i=0; i<cpuiddump->nr; i++) {
    struct cpuiddump_entry *entry = &cpuiddump->entries[i];
    if ((entry->inmask & 0x1) && *eax != entry->ineax)
      continue;
    if ((entry->inmask & 0x2) && *ebx != entry->inebx)
      continue;
    if ((entry->inmask & 0x4) && *ecx != entry->inecx)
      continue;
    if ((entry->inmask & 0x8) && *edx != entry->inedx)
      continue;
    *eax = entry->outeax;
    *ebx = entry->outebx;
    *ecx = entry->outecx;
    *edx = entry->outedx;
    return;
  }

  fprintf(stderr, "Couldn't find %x,%x,%x,%x in dumped cpuid, returning 0s.\n",
	  *eax, *ebx, *ecx, *edx);
  *eax = 0;
  *ebx = 0;
  *ecx = 0;
  *edx = 0;
}

static void cpuid_or_from_dump(unsigned *eax, unsigned *ebx, unsigned *ecx, unsigned *edx, struct cpuiddump *src_cpuiddump)
{
  if (src_cpuiddump) {
    cpuiddump_find_by_input(eax, ebx, ecx, edx, src_cpuiddump);
  } else {
    hwloc_x86_cpuid(eax, ebx, ecx, edx);
  }
}

/*******************************
 * Core detection routines and structures
 */

#define has_topoext(features) ((features)[6] & (1 << 22))
#define has_x2apic(features) ((features)[4] & (1 << 21))

struct cacheinfo {
  hwloc_obj_cache_type_t type;
  unsigned level;
  unsigned nbthreads_sharing;
  unsigned cacheid;

  unsigned linesize;
  unsigned linepart;
  int inclusive;
  int ways;
  unsigned sets;
  unsigned long size;
};

struct procinfo {
  unsigned present;
  unsigned apicid;
  unsigned packageid;
  unsigned nodeid;
  unsigned unitid;
  unsigned threadid;
  unsigned coreid;
  unsigned *otherids;
  unsigned levels;
  unsigned numcaches;
  struct cacheinfo *cache;
  char cpuvendor[13];
  char cpumodel[3*4*4+1];
  unsigned cpustepping;
  unsigned cpumodelnumber;
  unsigned cpufamilynumber;
};

enum cpuid_type {
  intel,
  amd,
  zhaoxin,
  unknown
};

static void fill_amd_cache(struct procinfo *infos, unsigned level, hwloc_obj_cache_type_t type, unsigned nbthreads_sharing, unsigned cpuid)
{
  struct cacheinfo *cache, *tmpcaches;
  unsigned cachenum;
  unsigned long size = 0;

  if (level == 1)
    size = ((cpuid >> 24)) << 10;
  else if (level == 2)
    size = ((cpuid >> 16)) << 10;
  else if (level == 3)
    size = ((cpuid >> 18)) << 19;
  if (!size)
    return;

  tmpcaches = realloc(infos->cache, (infos->numcaches+1)*sizeof(*infos->cache));
  if (!tmpcaches)
    /* failed to allocated, ignore that cache */
    return;
  infos->cache = tmpcaches;
  cachenum = infos->numcaches++;

  cache = &infos->cache[cachenum];

  cache->type = type;
  cache->level = level;
  cache->nbthreads_sharing = nbthreads_sharing;
  cache->linesize = cpuid & 0xff;
  cache->linepart = 0;
  cache->inclusive = 0; /* old AMD (K8-K10) supposed to have exclusive caches */

  if (level == 1) {
    cache->ways = (cpuid >> 16) & 0xff;
    if (cache->ways == 0xff)
      /* Fully associative */
      cache->ways = -1;
  } else {
    static const unsigned ways_tab[] = { 0, 1, 2, 0, 4, 0, 8, 0, 16, 0, 32, 48, 64, 96, 128, -1 };
    unsigned ways = (cpuid >> 12) & 0xf;
    cache->ways = ways_tab[ways];
  }
  cache->size = size;
  cache->sets = 0;

  hwloc_debug("cache L%u t%u linesize %u ways %d size %luKB\n", cache->level, cache->nbthreads_sharing, cache->linesize, cache->ways, cache->size >> 10);
}

/* Fetch information from the processor itself thanks to cpuid and store it in
 * infos for summarize to analyze them globally */
static void look_proc(struct hwloc_backend *backend, struct procinfo *infos, unsigned highest_cpuid, unsigned highest_ext_cpuid, unsigned *features, enum cpuid_type cpuid_type, struct cpuiddump *src_cpuiddump)
{
  struct hwloc_x86_backend_data_s *data = backend->private_data;
  unsigned eax, ebx, ecx = 0, edx;
  unsigned cachenum;
  struct cacheinfo *cache;
  unsigned regs[4];
  unsigned legacy_max_log_proc; /* not valid on Intel processors with > 256 threads, or when cpuid 0x80000008 is supported */
  unsigned legacy_log_proc_id;
  unsigned _model, _extendedmodel, _family, _extendedfamily;

  infos->present = 1;

  /* Get apicid, legacy_max_log_proc, packageid, legacy_log_proc_id from cpuid 0x01 */
  eax = 0x01;
  cpuid_or_from_dump(&eax, &ebx, &ecx, &edx, src_cpuiddump);
  infos->apicid = ebx >> 24;
  if (edx & (1 << 28))
    legacy_max_log_proc = 1 << hwloc_flsl(((ebx >> 16) & 0xff) - 1);
  else
    legacy_max_log_proc = 1;
  hwloc_debug("APIC ID 0x%02x legacy_max_log_proc %u\n", infos->apicid, legacy_max_log_proc);
  infos->packageid = infos->apicid / legacy_max_log_proc;
  legacy_log_proc_id = infos->apicid % legacy_max_log_proc;
  hwloc_debug("phys %u legacy thread %u\n", infos->packageid, legacy_log_proc_id);

  /* Get cpu model/family/stepping numbers from same cpuid */
  _model          = (eax>>4) & 0xf;
  _extendedmodel  = (eax>>16) & 0xf;
  _family         = (eax>>8) & 0xf;
  _extendedfamily = (eax>>20) & 0xff;
  if ((cpuid_type == intel || cpuid_type == amd) && _family == 0xf) {
    infos->cpufamilynumber = _family + _extendedfamily;
  } else {
    infos->cpufamilynumber = _family;
  }
  if ((cpuid_type == intel && (_family == 0x6 || _family == 0xf))
      || (cpuid_type == amd && _family == 0xf)
      || (cpuid_type == zhaoxin && (_family == 0x6 || _family == 0x7))) {
    infos->cpumodelnumber = _model + (_extendedmodel << 4);
  } else {
    infos->cpumodelnumber = _model;
  }
  infos->cpustepping = eax & 0xf;

  if (cpuid_type == intel && infos->cpufamilynumber == 0x6 &&
      (infos->cpumodelnumber == 0x57 || infos->cpumodelnumber == 0x85))
    data->is_knl = 1; /* KNM is the same as KNL */

  /* Get cpu vendor string from cpuid 0x00 */
  memset(regs, 0, sizeof(regs));
  regs[0] = 0;
  cpuid_or_from_dump(&regs[0], &regs[1], &regs[3], &regs[2], src_cpuiddump);
  memcpy(infos->cpuvendor, regs+1, 4*3);
  /* infos was calloc'ed, already ends with \0 */

  /* Get cpu model string from cpuid 0x80000002-4 */
  if (highest_ext_cpuid >= 0x80000004) {
    memset(regs, 0, sizeof(regs));
    regs[0] = 0x80000002;
    cpuid_or_from_dump(&regs[0], &regs[1], &regs[2], &regs[3], src_cpuiddump);
    memcpy(infos->cpumodel, regs, 4*4);
    regs[0] = 0x80000003;
    cpuid_or_from_dump(&regs[0], &regs[1], &regs[2], &regs[3], src_cpuiddump);
    memcpy(infos->cpumodel + 4*4, regs, 4*4);
    regs[0] = 0x80000004;
    cpuid_or_from_dump(&regs[0], &regs[1], &regs[2], &regs[3], src_cpuiddump);
    memcpy(infos->cpumodel + 4*4*2, regs, 4*4);
    /* infos was calloc'ed, already ends with \0 */
  }

  /* Get core/thread information from cpuid 0x80000008
   * (not supported on Intel)
   */
  if (cpuid_type != intel && cpuid_type != zhaoxin && highest_ext_cpuid >= 0x80000008) {
    unsigned max_nbcores;
    unsigned max_nbthreads;
    unsigned coreidsize;
    unsigned logprocid;
    eax = 0x80000008;
    cpuid_or_from_dump(&eax, &ebx, &ecx, &edx, src_cpuiddump);
    coreidsize = (ecx >> 12) & 0xf;
    hwloc_debug("core ID size: %u\n", coreidsize);
    if (!coreidsize) {
      max_nbcores = (ecx & 0xff) + 1;
    } else
      max_nbcores = 1 << coreidsize;
    hwloc_debug("Thus max # of cores: %u\n", max_nbcores);
    /* Still no multithreaded AMD */
    max_nbthreads = 1 ;
    hwloc_debug("and max # of threads: %u\n", max_nbthreads);
    /* legacy_max_log_proc is deprecated, it can be smaller than max_nbcores,
     * which is the maximum number of cores that the processor could theoretically support
     * (see "Multiple Core Calculation" in the AMD CPUID specification).
     * Recompute packageid/threadid/coreid accordingly.
     */
    infos->packageid = infos->apicid / max_nbcores;
    logprocid = infos->apicid % max_nbcores;
    infos->threadid = logprocid % max_nbthreads;
    infos->coreid = logprocid / max_nbthreads;
    hwloc_debug("this is thread %u of core %u\n", infos->threadid, infos->coreid);
  }

  infos->numcaches = 0;
  infos->cache = NULL;

  /* Get apicid, nodeid, unitid from cpuid 0x8000001e
   * and cache information from cpuid 0x8000001d
   * (AMD topology extension)
   */
  if (cpuid_type != intel && cpuid_type != zhaoxin && has_topoext(features)) {
    unsigned apic_id, node_id, nodes_per_proc;

    /* the code below doesn't want any other cache yet */
    assert(!infos->numcaches);

    eax = 0x8000001e;
    cpuid_or_from_dump(&eax, &ebx, &ecx, &edx, src_cpuiddump);
    infos->apicid = apic_id = eax;

    if (infos->cpufamilynumber == 0x16) {
      /* ecx is reserved */
      node_id = 0;
      nodes_per_proc = 1;
    } else {
      node_id = ecx & 0xff;
      nodes_per_proc = ((ecx >> 8) & 7) + 1;
    }
    infos->nodeid = node_id;
    if ((infos->cpufamilynumber == 0x15 && nodes_per_proc > 2)
	|| (infos->cpufamilynumber == 0x17 && nodes_per_proc > 4)) {
      hwloc_debug("warning: undefined nodes_per_proc value %u, assuming it means %u\n", nodes_per_proc, nodes_per_proc);
    }

    if (infos->cpufamilynumber <= 0x16) { /* topoext appeared in 0x15 and compute-units were only used in 0x15 and 0x16 */
      unsigned unit_id, cores_per_unit;
      infos->unitid = unit_id = ebx & 0xff;
      cores_per_unit = ((ebx >> 8) & 0xff) + 1;
      hwloc_debug("topoext %08x, %u nodes, node %u, %u cores in unit %u\n", apic_id, nodes_per_proc, node_id, cores_per_unit, unit_id);
      /* coreid and unitid are package-wide (core 0-15 and unit 0-7 on 16-core 2-NUMAnode processor).
       * The Linux kernel reduces theses to NUMA-node-wide (by applying %core_per_node and %unit_per node respectively).
       * It's not clear if we should do this as well.
       */
    } else {
      unsigned core_id, threads_per_core;
      infos->coreid = core_id = ebx & 0xff;
      threads_per_core = ((ebx >> 8) & 0xff) + 1;
      hwloc_debug("topoext %08x, %u nodes, node %u, %u threads in core %u\n", apic_id, nodes_per_proc, node_id, threads_per_core, core_id);
    }

    for (cachenum = 0; ; cachenum++) {
      eax = 0x8000001d;
      ecx = cachenum;
      cpuid_or_from_dump(&eax, &ebx, &ecx, &edx, src_cpuiddump);
      if ((eax & 0x1f) == 0)
	break;
      infos->numcaches++;
    }

    cache = infos->cache = malloc(infos->numcaches * sizeof(*infos->cache));
    if (cache) {
     for (cachenum = 0; ; cachenum++) {
      unsigned long linesize, linepart, ways, sets;
      eax = 0x8000001d;
      ecx = cachenum;
      cpuid_or_from_dump(&eax, &ebx, &ecx, &edx, src_cpuiddump);

      if ((eax & 0x1f) == 0)
	break;
      switch (eax & 0x1f) {
      case 1: cache->type = HWLOC_OBJ_CACHE_DATA; break;
      case 2: cache->type = HWLOC_OBJ_CACHE_INSTRUCTION; break;
      default: cache->type = HWLOC_OBJ_CACHE_UNIFIED; break;
      }

      cache->level = (eax >> 5) & 0x7;
      /* Note: actually number of cores */
      cache->nbthreads_sharing = ((eax >> 14) &  0xfff) + 1;

      cache->linesize = linesize = (ebx & 0xfff) + 1;
      cache->linepart = linepart = ((ebx >> 12) & 0x3ff) + 1;
      ways = ((ebx >> 22) & 0x3ff) + 1;

      if (eax & (1 << 9))
	/* Fully associative */
	cache->ways = -1;
      else
	cache->ways = ways;
      cache->sets = sets = ecx + 1;
      cache->size = linesize * linepart * ways * sets;
      cache->inclusive = edx & 0x2;

      hwloc_debug("cache %u L%u%c t%u linesize %lu linepart %lu ways %lu sets %lu, size %luKB\n",
		  cachenum, cache->level,
		  cache->type == HWLOC_OBJ_CACHE_DATA ? 'd' : cache->type == HWLOC_OBJ_CACHE_INSTRUCTION ? 'i' : 'u',
		  cache->nbthreads_sharing, linesize, linepart, ways, sets, cache->size >> 10);

      cache++;
     }
    } else {
     infos->numcaches = 0;
    }
  } else {
    /* If there's no topoext,
     * get cache information from cpuid 0x80000005 and 0x80000006
     * (not supported on Intel)
     */
    if (cpuid_type != intel && cpuid_type != zhaoxin && highest_ext_cpuid >= 0x80000005) {
      eax = 0x80000005;
      cpuid_or_from_dump(&eax, &ebx, &ecx, &edx, src_cpuiddump);
      fill_amd_cache(infos, 1, HWLOC_OBJ_CACHE_DATA, 1, ecx); /* private L1d */
      fill_amd_cache(infos, 1, HWLOC_OBJ_CACHE_INSTRUCTION, 1, edx); /* private L1i */
    }
    if (cpuid_type != intel && cpuid_type != zhaoxin && highest_ext_cpuid >= 0x80000006) {
      eax = 0x80000006;
      cpuid_or_from_dump(&eax, &ebx, &ecx, &edx, src_cpuiddump);
      if (ecx & 0xf000)
	/* This is actually supported on Intel but LinePerTag isn't returned in bits 8-11.
	 * Could be useful if some Intels (at least before Core micro-architecture)
	 * support this leaf without leaf 0x4.
	 */
	fill_amd_cache(infos, 2, HWLOC_OBJ_CACHE_UNIFIED, 1, ecx); /* private L2u */
      if (edx & 0xf000)
	fill_amd_cache(infos, 3, HWLOC_OBJ_CACHE_UNIFIED, legacy_max_log_proc, edx); /* package-wide L3u */
    }
  }

  /* Get thread/core + cache information from cpuid 0x04
   * (not supported on AMD)
   */
  if (cpuid_type != amd && highest_cpuid >= 0x04) {
    unsigned max_nbcores;
    unsigned max_nbthreads;
    unsigned level;
    struct cacheinfo *tmpcaches;
    unsigned oldnumcaches = infos->numcaches; /* in case we got caches above */

    for (cachenum = 0; ; cachenum++) {
      eax = 0x04;
      ecx = cachenum;
      cpuid_or_from_dump(&eax, &ebx, &ecx, &edx, src_cpuiddump);

      hwloc_debug("cache %u type %u\n", cachenum, eax & 0x1f);
      if ((eax & 0x1f) == 0)
	break;
      level = (eax >> 5) & 0x7;
      if (data->is_knl && level == 3)
	/* KNL reports wrong L3 information (size always 0, cpuset always the entire machine, ignore it */
	break;
      infos->numcaches++;

      if (!cachenum) {
	/* by the way, get thread/core information from the first cache */
	max_nbcores = ((eax >> 26) & 0x3f) + 1;
	max_nbthreads = legacy_max_log_proc / max_nbcores;
	hwloc_debug("thus %u threads\n", max_nbthreads);
	infos->threadid = legacy_log_proc_id % max_nbthreads;
	infos->coreid = legacy_log_proc_id / max_nbthreads;
	hwloc_debug("this is thread %u of core %u\n", infos->threadid, infos->coreid);
      }
    }

    tmpcaches = realloc(infos->cache, infos->numcaches * sizeof(*infos->cache));
    if (tmpcaches) {
     infos->cache = tmpcaches;
     cache = &infos->cache[oldnumcaches];

     for (cachenum = 0; ; cachenum++) {
      unsigned long linesize, linepart, ways, sets;
      eax = 0x04;
      ecx = cachenum;
      cpuid_or_from_dump(&eax, &ebx, &ecx, &edx, src_cpuiddump);

      if ((eax & 0x1f) == 0)
	break;
      level = (eax >> 5) & 0x7;
      if (data->is_knl && level == 3)
	/* KNL reports wrong L3 information (size always 0, cpuset always the entire machine, ignore it */
	break;
      switch (eax & 0x1f) {
      case 1: cache->type = HWLOC_OBJ_CACHE_DATA; break;
      case 2: cache->type = HWLOC_OBJ_CACHE_INSTRUCTION; break;
      default: cache->type = HWLOC_OBJ_CACHE_UNIFIED; break;
      }

      cache->level = level;
      cache->nbthreads_sharing = ((eax >> 14) & 0xfff) + 1;

      cache->linesize = linesize = (ebx & 0xfff) + 1;
      cache->linepart = linepart = ((ebx >> 12) & 0x3ff) + 1;
      ways = ((ebx >> 22) & 0x3ff) + 1;
      if (eax & (1 << 9))
        /* Fully associative */
        cache->ways = -1;
      else
        cache->ways = ways;
      cache->sets = sets = ecx + 1;
      cache->size = linesize * linepart * ways * sets;
      cache->inclusive = edx & 0x2;

      hwloc_debug("cache %u L%u%c t%u linesize %lu linepart %lu ways %lu sets %lu, size %luKB\n",
		  cachenum, cache->level,
		  cache->type == HWLOC_OBJ_CACHE_DATA ? 'd' : cache->type == HWLOC_OBJ_CACHE_INSTRUCTION ? 'i' : 'u',
		  cache->nbthreads_sharing, linesize, linepart, ways, sets, cache->size >> 10);
      cache++;
     }
    }
  }

  /* Get package/core/thread information from cpuid 0x0b
   * (Intel x2APIC)
   */
  if ((cpuid_type == intel || cpuid_type == zhaoxin) && highest_cpuid >= 0x0b && has_x2apic(features)) {
    unsigned level, apic_nextshift, apic_number, apic_type, apic_id = 0, apic_shift = 0, id;
    for (level = 0; ; level++) {
      ecx = level;
      eax = 0x0b;
      cpuid_or_from_dump(&eax, &ebx, &ecx, &edx, src_cpuiddump);
      if (!eax && !ebx)
        break;
    }
    if (level) {
      infos->otherids = malloc(level * sizeof(*infos->otherids));
      if (infos->otherids) {
       infos->levels = level;
       for (level = 0; ; level++) {
	ecx = level;
	eax = 0x0b;
	cpuid_or_from_dump(&eax, &ebx, &ecx, &edx, src_cpuiddump);
	if (!eax && !ebx)
	  break;
	apic_nextshift = eax & 0x1f;
	apic_number = ebx & 0xffff;
	apic_type = (ecx & 0xff00) >> 8;
	apic_id = edx;
	id = (apic_id >> apic_shift) & ((1 << (apic_nextshift - apic_shift)) - 1);
	hwloc_debug("x2APIC %08x %u: nextshift %u num %2u type %u id %2u\n", apic_id, level, apic_nextshift, apic_number, apic_type, id);
	infos->apicid = apic_id;
	infos->otherids[level] = UINT_MAX;
	switch (apic_type) {
	case 1:
	  infos->threadid = id;
	  /* apic_number is the actual number of threads per core */
	  break;
	case 2:
	  infos->coreid = id;
	  /* apic_number is the actual number of threads per package */
	  break;
	default:
	  hwloc_debug("x2APIC %u: unknown type %u\n", level, apic_type);
	  infos->otherids[level] = apic_id >> apic_shift;
	  break;
	}
	apic_shift = apic_nextshift;
      }
      infos->apicid = apic_id;
      infos->packageid = apic_id >> apic_shift;
      hwloc_debug("x2APIC remainder: %u\n", infos->packageid);
      hwloc_debug("this is thread %u of core %u\n", infos->threadid, infos->coreid);
     }
    }
  }

  /* Now that we have all info, compute cacheids and apply quirks */
  for (cachenum = 0; cachenum < infos->numcaches; cachenum++) {
    cache = &infos->cache[cachenum];

    /* default cacheid value */
    cache->cacheid = infos->apicid / cache->nbthreads_sharing;

    if (cpuid_type == amd) {
      /* AMD quirks */
      if (infos->cpufamilynumber == 0x17
	  && cache->level == 3 && cache->nbthreads_sharing == 6) {
	/* AMD family 0x17 always shares L3 between 8 APIC ids,
	 * even when only 6 APIC ids are enabled and reported in nbthreads_sharing
	 * (on 24-core CPUs).
	 */
	cache->cacheid = infos->apicid / 8;

      } else if (infos->cpufamilynumber== 0x10 && infos->cpumodelnumber == 0x9
	  && cache->level == 3
	  && (cache->ways == -1 || (cache->ways % 2 == 0)) && cache->nbthreads_sharing >= 8) {
	/* Fix AMD family 0x10 model 0x9 (Magny-Cours) with 8 or 12 cores.
	 * The L3 (and its associativity) is actually split into two halves).
	 */
	if (cache->nbthreads_sharing == 16)
	  cache->nbthreads_sharing = 12; /* nbthreads_sharing is a power of 2 but the processor actually has 8 or 12 cores */
	cache->nbthreads_sharing /= 2;
	cache->size /= 2;
	if (cache->ways != -1)
	  cache->ways /= 2;
	/* AMD Magny-Cours 12-cores processor reserve APIC ids as AAAAAABBBBBB....
	 * among first L3 (A), second L3 (B), and unexisting cores (.).
	 * On multi-socket servers, L3 in non-first sockets may have APIC id ranges
	 * such as [16-21] that are not aligned on multiple of nbthreads_sharing (6).
	 * That means, we can't just compare apicid/nbthreads_sharing to identify siblings.
	 */
	cache->cacheid = (infos->apicid % legacy_max_log_proc) / cache->nbthreads_sharing /* cacheid within the package */
	  + 2 * (infos->apicid / legacy_max_log_proc); /* add 2 caches per previous package */

      } else if (infos->cpufamilynumber == 0x15
		 && (infos->cpumodelnumber == 0x1 /* Bulldozer */ || infos->cpumodelnumber == 0x2 /* Piledriver */)
		 && cache->level == 3 && cache->nbthreads_sharing == 6) {
	/* AMD Bulldozer and Piledriver 12-core processors have same APIC ids as Magny-Cours below,
	 * but we can't merge the checks because the original nbthreads_sharing must be exactly 6 here.
	 */
	cache->cacheid = (infos->apicid % legacy_max_log_proc) / cache->nbthreads_sharing /* cacheid within the package */
	  + 2 * (infos->apicid / legacy_max_log_proc); /* add 2 cache per previous package */
      }
    }
  }

  if (hwloc_bitmap_isset(data->apicid_set, infos->apicid))
    data->apicid_unique = 0;
  else
    hwloc_bitmap_set(data->apicid_set, infos->apicid);
}

static void
hwloc_x86_add_cpuinfos(hwloc_obj_t obj, struct procinfo *info, int replace)
{
  char number[8];
  if (info->cpuvendor[0])
    hwloc__add_info_nodup(&obj->infos, &obj->infos_count, "CPUVendor", info->cpuvendor, replace);
  snprintf(number, sizeof(number), "%u", info->cpufamilynumber);
  hwloc__add_info_nodup(&obj->infos, &obj->infos_count, "CPUFamilyNumber", number, replace);
  snprintf(number, sizeof(number), "%u", info->cpumodelnumber);
  hwloc__add_info_nodup(&obj->infos, &obj->infos_count, "CPUModelNumber", number, replace);
  if (info->cpumodel[0]) {
    const char *c = info->cpumodel;
    while (*c == ' ')
      c++;
    hwloc__add_info_nodup(&obj->infos, &obj->infos_count, "CPUModel", c, replace);
  }
  snprintf(number, sizeof(number), "%u", info->cpustepping);
  hwloc__add_info_nodup(&obj->infos, &obj->infos_count, "CPUStepping", number, replace);
}

/* Analyse information stored in infos, and build/annotate topology levels accordingly */
static void summarize(struct hwloc_backend *backend, struct procinfo *infos, int fulldiscovery)
{
  struct hwloc_topology *topology = backend->topology;
  struct hwloc_x86_backend_data_s *data = backend->private_data;
  unsigned nbprocs = data->nbprocs;
  hwloc_bitmap_t complete_cpuset = hwloc_bitmap_alloc();
  unsigned i, j, l, level;
  int one = -1;
  hwloc_bitmap_t remaining_cpuset;
  int gotnuma = 0;

  for (i = 0; i < nbprocs; i++)
    if (infos[i].present) {
      hwloc_bitmap_set(complete_cpuset, i);
      one = i;
    }

  if (one == -1) {
    hwloc_bitmap_free(complete_cpuset);
    return;
  }

  remaining_cpuset = hwloc_bitmap_alloc();

  /* Ideally, when fulldiscovery=0, we could add any object that doesn't exist yet.
   * But what if the x86 and the native backends disagree because one is buggy? Which one to trust?
   * We only add missing caches, and annotate other existing objects for now.
   */

  if (hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_PACKAGE)) {
    /* Look for packages */
    hwloc_obj_t package;

    hwloc_bitmap_copy(remaining_cpuset, complete_cpuset);
    while ((i = hwloc_bitmap_first(remaining_cpuset)) != (unsigned) -1) {
      if (fulldiscovery) {
	unsigned packageid = infos[i].packageid;
	hwloc_bitmap_t package_cpuset = hwloc_bitmap_alloc();

	for (j = i; j < nbprocs; j++) {
	  if (infos[j].packageid == packageid) {
	    hwloc_bitmap_set(package_cpuset, j);
	    hwloc_bitmap_clr(remaining_cpuset, j);
	  }
	}
	package = hwloc_alloc_setup_object(topology, HWLOC_OBJ_PACKAGE, packageid);
	package->cpuset = package_cpuset;

	hwloc_x86_add_cpuinfos(package, &infos[i], 0);

	hwloc_debug_1arg_bitmap("os package %u has cpuset %s\n",
				packageid, package_cpuset);
	hwloc_insert_object_by_cpuset(topology, package);

      } else {
	/* Annotate packages previously-existing packages */
	hwloc_bitmap_t set = hwloc_bitmap_alloc();
	hwloc_bitmap_set(set, i);
	package = hwloc_get_next_obj_covering_cpuset_by_type(topology, set, HWLOC_OBJ_PACKAGE, NULL);
	hwloc_bitmap_free(set);
	if (package) {
	  /* Found package above that PU, annotate if no such attribute yet */
	  hwloc_x86_add_cpuinfos(package, &infos[i], 1);
	  hwloc_bitmap_andnot(remaining_cpuset, remaining_cpuset, package->cpuset);
	} else {
	  /* No package, annotate the root object */
	  hwloc_x86_add_cpuinfos(hwloc_get_root_obj(topology), &infos[i], 1);
	  break;
	}
      }
    }
  }

  /* Look for Numa nodes inside packages (cannot be filtered-out) */
  if (fulldiscovery) {
    hwloc_bitmap_t node_cpuset;
    hwloc_obj_t node;

    /* FIXME: if there's memory inside the root object, divide it into NUMA nodes? */

    hwloc_bitmap_copy(remaining_cpuset, complete_cpuset);
    while ((i = hwloc_bitmap_first(remaining_cpuset)) != (unsigned) -1) {
      unsigned packageid = infos[i].packageid;
      unsigned nodeid = infos[i].nodeid;

      if (nodeid == (unsigned)-1) {
        hwloc_bitmap_clr(remaining_cpuset, i);
	continue;
      }

      node_cpuset = hwloc_bitmap_alloc();
      for (j = i; j < nbprocs; j++) {
	if (infos[j].nodeid == (unsigned) -1) {
	  hwloc_bitmap_clr(remaining_cpuset, j);
	  continue;
	}

        if (infos[j].packageid == packageid && infos[j].nodeid == nodeid) {
          hwloc_bitmap_set(node_cpuset, j);
          hwloc_bitmap_clr(remaining_cpuset, j);
        }
      }
      node = hwloc_alloc_setup_object(topology, HWLOC_OBJ_NUMANODE, nodeid);
      node->cpuset = node_cpuset;
      node->nodeset = hwloc_bitmap_alloc();
      hwloc_bitmap_set(node->nodeset, nodeid);
      hwloc_debug_1arg_bitmap("os node %u has cpuset %s\n",
          nodeid, node_cpuset);
      hwloc_insert_object_by_cpuset(topology, node);
      gotnuma++;
    }
  }

  if (hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_GROUP)) {
    /* Look for Compute units inside packages */
    if (fulldiscovery) {
      hwloc_bitmap_t unit_cpuset;
      hwloc_obj_t unit;

      hwloc_bitmap_copy(remaining_cpuset, complete_cpuset);
      while ((i = hwloc_bitmap_first(remaining_cpuset)) != (unsigned) -1) {
	unsigned packageid = infos[i].packageid;
	unsigned unitid = infos[i].unitid;

	if (unitid == (unsigned)-1) {
	  hwloc_bitmap_clr(remaining_cpuset, i);
	  continue;
	}

	unit_cpuset = hwloc_bitmap_alloc();
	for (j = i; j < nbprocs; j++) {
	  if (infos[j].unitid == (unsigned) -1) {
	    hwloc_bitmap_clr(remaining_cpuset, j);
	    continue;
	  }

	  if (infos[j].packageid == packageid && infos[j].unitid == unitid) {
	    hwloc_bitmap_set(unit_cpuset, j);
	    hwloc_bitmap_clr(remaining_cpuset, j);
	  }
	}
	unit = hwloc_alloc_setup_object(topology, HWLOC_OBJ_GROUP, unitid);
	unit->cpuset = unit_cpuset;
	unit->subtype = strdup("ComputeUnit");
	unit->attr->group.kind = HWLOC_GROUP_KIND_AMD_COMPUTE_UNIT;
	hwloc_debug_1arg_bitmap("os unit %u has cpuset %s\n",
				unitid, unit_cpuset);
	hwloc_insert_object_by_cpuset(topology, unit);
      }
    }

    /* Look for unknown objects */
    if (infos[one].otherids) {
      for (level = infos[one].levels-1; level <= infos[one].levels-1; level--) {
	if (infos[one].otherids[level] != UINT_MAX) {
	  hwloc_bitmap_t unknown_cpuset;
	  hwloc_obj_t unknown_obj;

	  hwloc_bitmap_copy(remaining_cpuset, complete_cpuset);
	  while ((i = hwloc_bitmap_first(remaining_cpuset)) != (unsigned) -1) {
	    unsigned unknownid = infos[i].otherids[level];

	    unknown_cpuset = hwloc_bitmap_alloc();
	    for (j = i; j < nbprocs; j++) {
	      if (infos[j].otherids[level] == unknownid) {
		hwloc_bitmap_set(unknown_cpuset, j);
		hwloc_bitmap_clr(remaining_cpuset, j);
	      }
	    }
	    unknown_obj = hwloc_alloc_setup_object(topology, HWLOC_OBJ_GROUP, unknownid);
	    unknown_obj->cpuset = unknown_cpuset;
	    unknown_obj->attr->group.kind = HWLOC_GROUP_KIND_INTEL_X2APIC_UNKNOWN;
	    unknown_obj->attr->group.subkind = level;
	    hwloc_debug_2args_bitmap("os unknown%u %u has cpuset %s\n",
				     level, unknownid, unknown_cpuset);
	    hwloc_insert_object_by_cpuset(topology, unknown_obj);
	  }
	}
      }
    }
  }

  if (hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_CORE)) {
    /* Look for cores */
    if (fulldiscovery) {
      hwloc_bitmap_t core_cpuset;
      hwloc_obj_t core;

      hwloc_bitmap_copy(remaining_cpuset, complete_cpuset);
      while ((i = hwloc_bitmap_first(remaining_cpuset)) != (unsigned) -1) {
	unsigned packageid = infos[i].packageid;
	unsigned nodeid = infos[i].nodeid;
	unsigned coreid = infos[i].coreid;

	if (coreid == (unsigned) -1) {
	  hwloc_bitmap_clr(remaining_cpuset, i);
	  continue;
	}

	core_cpuset = hwloc_bitmap_alloc();
	for (j = i; j < nbprocs; j++) {
	  if (infos[j].coreid == (unsigned) -1) {
	    hwloc_bitmap_clr(remaining_cpuset, j);
	    continue;
	  }

	  if (infos[j].packageid == packageid && infos[j].nodeid == nodeid && infos[j].coreid == coreid) {
	    hwloc_bitmap_set(core_cpuset, j);
	    hwloc_bitmap_clr(remaining_cpuset, j);
	  }
	}
	core = hwloc_alloc_setup_object(topology, HWLOC_OBJ_CORE, coreid);
	core->cpuset = core_cpuset;
	hwloc_debug_1arg_bitmap("os core %u has cpuset %s\n",
				coreid, core_cpuset);
	hwloc_insert_object_by_cpuset(topology, core);
      }
    }
  }

  /* Look for PUs (cannot be filtered-out) */
  if (fulldiscovery) {
    hwloc_debug("%s", "\n\n * CPU cpusets *\n\n");
    for (i=0; i<nbprocs; i++)
      if(infos[i].present) { /* Only add present PU. We don't know if others actually exist */
       struct hwloc_obj *obj = hwloc_alloc_setup_object(topology, HWLOC_OBJ_PU, i);
       obj->cpuset = hwloc_bitmap_alloc();
       hwloc_bitmap_only(obj->cpuset, i);
       hwloc_debug_1arg_bitmap("PU %u has cpuset %s\n", i, obj->cpuset);
       hwloc_insert_object_by_cpuset(topology, obj);
     }
  }

  /* Look for caches */
  /* First find max level */
  level = 0;
  for (i = 0; i < nbprocs; i++)
    for (j = 0; j < infos[i].numcaches; j++)
      if (infos[i].cache[j].level > level)
        level = infos[i].cache[j].level;
  while (level > 0) {
    hwloc_obj_cache_type_t type;
    HWLOC_BUILD_ASSERT(HWLOC_OBJ_CACHE_DATA == HWLOC_OBJ_CACHE_UNIFIED+1);
    HWLOC_BUILD_ASSERT(HWLOC_OBJ_CACHE_INSTRUCTION == HWLOC_OBJ_CACHE_DATA+1);
    for (type = HWLOC_OBJ_CACHE_UNIFIED; type <= HWLOC_OBJ_CACHE_INSTRUCTION; type++) {
      /* Look for caches of that type at level level */
      hwloc_obj_type_t otype;
      hwloc_obj_t cache;

      otype = hwloc_cache_type_by_depth_type(level, type);
      if (otype == HWLOC_OBJ_TYPE_NONE)
	continue;
      if (!hwloc_filter_check_keep_object_type(topology, otype))
	continue;

      hwloc_bitmap_copy(remaining_cpuset, complete_cpuset);
      while ((i = hwloc_bitmap_first(remaining_cpuset)) != (unsigned) -1) {
	hwloc_bitmap_t puset;

	for (l = 0; l < infos[i].numcaches; l++) {
	  if (infos[i].cache[l].level == level && infos[i].cache[l].type == type)
	    break;
	}
	if (l == infos[i].numcaches) {
	  /* no cache Llevel of that type in i */
	  hwloc_bitmap_clr(remaining_cpuset, i);
	  continue;
	}

	puset = hwloc_bitmap_alloc();
	hwloc_bitmap_set(puset, i);
	cache = hwloc_get_next_obj_covering_cpuset_by_type(topology, puset, otype, NULL);
	hwloc_bitmap_free(puset);

	if (cache) {
	  /* Found cache above that PU, annotate if no such attribute yet */
	  if (!hwloc_obj_get_info_by_name(cache, "Inclusive"))
	    hwloc_obj_add_info(cache, "Inclusive", infos[i].cache[l].inclusive ? "1" : "0");
	  hwloc_bitmap_andnot(remaining_cpuset, remaining_cpuset, cache->cpuset);
	} else {
	  /* Add the missing cache */
	  hwloc_bitmap_t cache_cpuset;
	  unsigned packageid = infos[i].packageid;
	  unsigned cacheid = infos[i].cache[l].cacheid;
	  /* Now look for others sharing it */
	  cache_cpuset = hwloc_bitmap_alloc();
	  for (j = i; j < nbprocs; j++) {
	    unsigned l2;
	    for (l2 = 0; l2 < infos[j].numcaches; l2++) {
	      if (infos[j].cache[l2].level == level && infos[j].cache[l2].type == type)
		break;
	    }
	    if (l2 == infos[j].numcaches) {
	      /* no cache Llevel of that type in j */
	      hwloc_bitmap_clr(remaining_cpuset, j);
	      continue;
	    }
	    if (infos[j].packageid == packageid && infos[j].cache[l2].cacheid == cacheid) {
	      hwloc_bitmap_set(cache_cpuset, j);
	      hwloc_bitmap_clr(remaining_cpuset, j);
	    }
	  }
	  cache = hwloc_alloc_setup_object(topology, otype, HWLOC_UNKNOWN_INDEX);
	  cache->attr->cache.depth = level;
	  cache->attr->cache.size = infos[i].cache[l].size;
	  cache->attr->cache.linesize = infos[i].cache[l].linesize;
	  cache->attr->cache.associativity = infos[i].cache[l].ways;
	  cache->attr->cache.type = infos[i].cache[l].type;
	  cache->cpuset = cache_cpuset;
	  hwloc_obj_add_info(cache, "Inclusive", infos[i].cache[l].inclusive ? "1" : "0");
	  hwloc_debug_2args_bitmap("os L%u cache %u has cpuset %s\n",
				   level, cacheid, cache_cpuset);
	  hwloc_insert_object_by_cpuset(topology, cache);
	}
      }
    }
    level--;
  }

  /* FIXME: if KNL and L2 disabled, add tiles instead of L2 */

  hwloc_bitmap_free(remaining_cpuset);
  hwloc_bitmap_free(complete_cpuset);

  if (gotnuma)
    topology->support.discovery->numa = 1;
}

static int
look_procs(struct hwloc_backend *backend, struct procinfo *infos, int fulldiscovery,
	   unsigned highest_cpuid, unsigned highest_ext_cpuid, unsigned *features, enum cpuid_type cpuid_type,
	   int (*get_cpubind)(hwloc_topology_t topology, hwloc_cpuset_t set, int flags),
	   int (*set_cpubind)(hwloc_topology_t topology, hwloc_const_cpuset_t set, int flags))
{
  struct hwloc_x86_backend_data_s *data = backend->private_data;
  struct hwloc_topology *topology = backend->topology;
  unsigned nbprocs = data->nbprocs;
  hwloc_bitmap_t orig_cpuset = NULL;
  hwloc_bitmap_t set = NULL;
  unsigned i;

  if (!data->src_cpuiddump_path) {
    orig_cpuset = hwloc_bitmap_alloc();
    if (get_cpubind(topology, orig_cpuset, HWLOC_CPUBIND_STRICT)) {
      hwloc_bitmap_free(orig_cpuset);
      return -1;
    }
    set = hwloc_bitmap_alloc();
  }

  for (i = 0; i < nbprocs; i++) {
    struct cpuiddump *src_cpuiddump = NULL;
    if (data->src_cpuiddump_path) {
      src_cpuiddump = cpuiddump_read(data->src_cpuiddump_path, i);
      if (!src_cpuiddump)
	continue;
    } else {
      hwloc_bitmap_only(set, i);
      hwloc_debug("binding to CPU%u\n", i);
      if (set_cpubind(topology, set, HWLOC_CPUBIND_STRICT)) {
	hwloc_debug("could not bind to CPU%u: %s\n", i, strerror(errno));
	continue;
      }
    }

    look_proc(backend, &infos[i], highest_cpuid, highest_ext_cpuid, features, cpuid_type, src_cpuiddump);

    if (data->src_cpuiddump_path) {
      cpuiddump_free(src_cpuiddump);
    }
  }

  if (!data->src_cpuiddump_path) {
    set_cpubind(topology, orig_cpuset, 0);
    hwloc_bitmap_free(set);
    hwloc_bitmap_free(orig_cpuset);
  }

  if (!data->apicid_unique)
    fulldiscovery = 0;
  else
    summarize(backend, infos, fulldiscovery);
  return 0;
}

#if defined HWLOC_FREEBSD_SYS && defined HAVE_CPUSET_SETID
#include <sys/param.h>
#include <sys/cpuset.h>
typedef cpusetid_t hwloc_x86_os_state_t;
static void hwloc_x86_os_state_save(hwloc_x86_os_state_t *state, struct cpuiddump *src_cpuiddump)
{
  if (!src_cpuiddump) {
    /* temporary make all cpus available during discovery */
    cpuset_getid(CPU_LEVEL_CPUSET, CPU_WHICH_PID, -1, state);
    cpuset_setid(CPU_WHICH_PID, -1, 0);
  }
}
static void hwloc_x86_os_state_restore(hwloc_x86_os_state_t *state, struct cpuiddump *src_cpuiddump)
{
  if (!src_cpuiddump) {
    /* restore initial cpuset */
    cpuset_setid(CPU_WHICH_PID, -1, *state);
  }
}
#else /* !defined HWLOC_FREEBSD_SYS || !defined HAVE_CPUSET_SETID */
typedef void * hwloc_x86_os_state_t;
static void hwloc_x86_os_state_save(hwloc_x86_os_state_t *state __hwloc_attribute_unused, struct cpuiddump *src_cpuiddump __hwloc_attribute_unused) { }
static void hwloc_x86_os_state_restore(hwloc_x86_os_state_t *state __hwloc_attribute_unused, struct cpuiddump *src_cpuiddump __hwloc_attribute_unused) { }
#endif /* !defined HWLOC_FREEBSD_SYS || !defined HAVE_CPUSET_SETID */

/* GenuineIntel */
#define INTEL_EBX ('G' | ('e'<<8) | ('n'<<16) | ('u'<<24))
#define INTEL_EDX ('i' | ('n'<<8) | ('e'<<16) | ('I'<<24))
#define INTEL_ECX ('n' | ('t'<<8) | ('e'<<16) | ('l'<<24))

/* AuthenticAMD */
#define AMD_EBX ('A' | ('u'<<8) | ('t'<<16) | ('h'<<24))
#define AMD_EDX ('e' | ('n'<<8) | ('t'<<16) | ('i'<<24))
#define AMD_ECX ('c' | ('A'<<8) | ('M'<<16) | ('D'<<24))

/* (Zhaoxin) CentaurHauls */
#define ZX_EBX ('C' | ('e'<<8) | ('n'<<16) | ('t'<<24))
#define ZX_EDX ('a' | ('u'<<8) | ('r'<<16) | ('H'<<24))
#define ZX_ECX ('a' | ('u'<<8) | ('l'<<16) | ('s'<<24))
/* (Zhaoxin) Shanghai */
#define SH_EBX (' ' | (' '<<8) | ('S'<<16) | ('h'<<24))
#define SH_EDX ('a' | ('n'<<8) | ('g'<<16) | ('h'<<24))
#define SH_ECX ('a' | ('i'<<8) | (' '<<16) | (' '<<24))

/* fake cpubind for when nbprocs=1 and no binding support */
static int fake_get_cpubind(hwloc_topology_t topology __hwloc_attribute_unused,
			    hwloc_cpuset_t set __hwloc_attribute_unused,
			    int flags __hwloc_attribute_unused)
{
  return 0;
}
static int fake_set_cpubind(hwloc_topology_t topology __hwloc_attribute_unused,
			    hwloc_const_cpuset_t set __hwloc_attribute_unused,
			    int flags __hwloc_attribute_unused)
{
  return 0;
}

static
int hwloc_look_x86(struct hwloc_backend *backend, int fulldiscovery)
{
  struct hwloc_x86_backend_data_s *data = backend->private_data;
  unsigned nbprocs = data->nbprocs;
  unsigned eax, ebx, ecx = 0, edx;
  unsigned i;
  unsigned highest_cpuid;
  unsigned highest_ext_cpuid;
  /* This stores cpuid features with the same indexing as Linux */
  unsigned features[10] = { 0 };
  struct procinfo *infos = NULL;
  enum cpuid_type cpuid_type = unknown;
  hwloc_x86_os_state_t os_state;
  struct hwloc_binding_hooks hooks;
  struct hwloc_topology_support support;
  struct hwloc_topology_membind_support memsupport __hwloc_attribute_unused;
  int (*get_cpubind)(hwloc_topology_t topology, hwloc_cpuset_t set, int flags) = NULL;
  int (*set_cpubind)(hwloc_topology_t topology, hwloc_const_cpuset_t set, int flags) = NULL;
  struct cpuiddump *src_cpuiddump = NULL;
  int ret = -1;

  if (data->src_cpuiddump_path) {
    /* just read cpuid from the dump */
    src_cpuiddump = cpuiddump_read(data->src_cpuiddump_path, 0);
    if (!src_cpuiddump)
      goto out;

  } else {
    /* otherwise check if binding works */
    memset(&hooks, 0, sizeof(hooks));
    support.membind = &memsupport;
    hwloc_set_native_binding_hooks(&hooks, &support);
    if (hooks.get_thisthread_cpubind && hooks.set_thisthread_cpubind) {
      get_cpubind = hooks.get_thisthread_cpubind;
      set_cpubind = hooks.set_thisthread_cpubind;
    } else if (hooks.get_thisproc_cpubind && hooks.set_thisproc_cpubind) {
      /* FIXME: if called by a multithreaded program, we will restore the original process binding
       * for each thread instead of their own original thread binding.
       * See issue #158.
       */
      get_cpubind = hooks.get_thisproc_cpubind;
      set_cpubind = hooks.set_thisproc_cpubind;
    } else {
      /* we need binding support if there are multiple PUs */
      if (nbprocs > 1)
	goto out;
      get_cpubind = fake_get_cpubind;
      set_cpubind = fake_set_cpubind;
    }
  }

  if (!src_cpuiddump && !hwloc_have_x86_cpuid())
    goto out;

  infos = calloc(nbprocs, sizeof(struct procinfo));
  if (NULL == infos)
    goto out;
  for (i = 0; i < nbprocs; i++) {
    infos[i].nodeid = (unsigned) -1;
    infos[i].packageid = (unsigned) -1;
    infos[i].unitid = (unsigned) -1;
    infos[i].coreid = (unsigned) -1;
    infos[i].threadid = (unsigned) -1;
  }

  eax = 0x00;
  cpuid_or_from_dump(&eax, &ebx, &ecx, &edx, src_cpuiddump);
  highest_cpuid = eax;
  if (ebx == INTEL_EBX && ecx == INTEL_ECX && edx == INTEL_EDX)
    cpuid_type = intel;
  else if (ebx == AMD_EBX && ecx == AMD_ECX && edx == AMD_EDX)
    cpuid_type = amd;
  else if ((ebx == ZX_EBX && ecx == ZX_ECX && edx == ZX_EDX)
	   || (ebx == SH_EBX && ecx == SH_ECX && edx == SH_EDX))
    cpuid_type = zhaoxin;

  hwloc_debug("highest cpuid %x, cpuid type %u\n", highest_cpuid, cpuid_type);
  if (highest_cpuid < 0x01) {
      goto out_with_infos;
  }

  eax = 0x01;
  cpuid_or_from_dump(&eax, &ebx, &ecx, &edx, src_cpuiddump);
  features[0] = edx;
  features[4] = ecx;

  eax = 0x80000000;
  cpuid_or_from_dump(&eax, &ebx, &ecx, &edx, src_cpuiddump);
  highest_ext_cpuid = eax;

  hwloc_debug("highest extended cpuid %x\n", highest_ext_cpuid);

  if (highest_cpuid >= 0x7) {
    eax = 0x7;
    ecx = 0;
    cpuid_or_from_dump(&eax, &ebx, &ecx, &edx, src_cpuiddump);
    features[9] = ebx;
  }

  if (cpuid_type != intel && highest_ext_cpuid >= 0x80000001) {
    eax = 0x80000001;
    cpuid_or_from_dump(&eax, &ebx, &ecx, &edx, src_cpuiddump);
    features[1] = edx;
    features[6] = ecx;
  }

  hwloc_x86_os_state_save(&os_state, src_cpuiddump);

  ret = look_procs(backend, infos, fulldiscovery,
		   highest_cpuid, highest_ext_cpuid, features, cpuid_type,
		   get_cpubind, set_cpubind);
  if (!ret)
    /* success, we're done */
    goto out_with_os_state;

  if (nbprocs == 1) {
    /* only one processor, no need to bind */
    look_proc(backend, &infos[0], highest_cpuid, highest_ext_cpuid, features, cpuid_type, src_cpuiddump);
    summarize(backend, infos, fulldiscovery);
    ret = 0;
  }

out_with_os_state:
  hwloc_x86_os_state_restore(&os_state, src_cpuiddump);

out_with_infos:
  if (NULL != infos) {
    for (i = 0; i < nbprocs; i++) {
      free(infos[i].cache);
      free(infos[i].otherids);
    }
    free(infos);
  }

out:
  if (src_cpuiddump)
    cpuiddump_free(src_cpuiddump);
  return ret;
}

static int
hwloc_x86_discover(struct hwloc_backend *backend)
{
  struct hwloc_x86_backend_data_s *data = backend->private_data;
  struct hwloc_topology *topology = backend->topology;
  int alreadypus = 0;
  int ret;

#if HAVE_DECL_RUNNING_ON_VALGRIND
  if (RUNNING_ON_VALGRIND && !data->src_cpuiddump_path) {
    fprintf(stderr, "hwloc x86 backend cannot work under Valgrind, disabling.\n"
	    "May be reenabled by dumping CPUIDs with hwloc-gather-cpuid\n"
	    "and reloading them under Valgrind with HWLOC_CPUID_PATH.\n");
    return 0;
  }
#endif

  if (data->src_cpuiddump_path) {
    assert(data->nbprocs > 0); /* enforced by hwloc_x86_component_instantiate() */
    topology->support.discovery->pu = 1;
  } else {
    int nbprocs = hwloc_fallback_nbprocessors(topology);
    if (nbprocs >= 1)
      topology->support.discovery->pu = 1;
    else
      nbprocs = 1;
    data->nbprocs = (unsigned) nbprocs;
  }

  if (topology->levels[0][0]->cpuset) {
    /* somebody else discovered things */
    if (topology->nb_levels == 2 && topology->level_nbobjects[1] == data->nbprocs) {
      /* only PUs were discovered, as much as we would, complete the topology with everything else */
      alreadypus = 1;
      goto fulldiscovery;
    }

    /* several object types were added, we can't easily complete, just do partial discovery */
    hwloc_topology_reconnect(topology, 0);
    ret = hwloc_look_x86(backend, 0);
    if (ret)
      hwloc_obj_add_info(topology->levels[0][0], "Backend", "x86");
    return 0;
  } else {
    /* topology is empty, initialize it */
    hwloc_alloc_root_sets(topology->levels[0][0]);
  }

fulldiscovery:
  if (hwloc_look_x86(backend, 1) < 0) {
    /* if failed, create PUs */
    if (!alreadypus)
      hwloc_setup_pu_level(topology, data->nbprocs);
  }

  hwloc_obj_add_info(topology->levels[0][0], "Backend", "x86");

  if (!data->src_cpuiddump_path) { /* CPUID dump works for both x86 and x86_64 */
#ifdef HAVE_UNAME
    hwloc_add_uname_info(topology, NULL); /* we already know is_thissystem() is true */
#else
    /* uname isn't available, manually setup the "Architecture" info */
#ifdef HWLOC_X86_64_ARCH
    hwloc_obj_add_info(topology->levels[0][0], "Architecture", "x86_64");
#else
    hwloc_obj_add_info(topology->levels[0][0], "Architecture", "x86");
#endif
#endif
  }

  return 1;
}

static int
hwloc_x86_check_cpuiddump_input(const char *src_cpuiddump_path, hwloc_bitmap_t set)
{
#if !(defined HWLOC_WIN_SYS && !defined __MINGW32__) /* needs a lot of work */
  struct dirent *dirent;
  DIR *dir;
  FILE *file;
  char line [32];

  dir = opendir(src_cpuiddump_path);
  if (!dir)
    return -1;

  char path[strlen(src_cpuiddump_path) + strlen("/hwloc-cpuid-info") + 1];
  sprintf(path, "%s/hwloc-cpuid-info", src_cpuiddump_path);
  file = fopen(path, "r");
  if (!file) {
    fprintf(stderr, "Couldn't open dumped cpuid summary %s\n", path);
    goto out_with_dir;
  }
  if (!fgets(line, sizeof(line), file)) {
    fprintf(stderr, "Found read dumped cpuid summary in %s\n", path);
    fclose(file);
    goto out_with_dir;
  }
  fclose(file);
  if (strcmp(line, "Architecture: x86\n")) {
    fprintf(stderr, "Found non-x86 dumped cpuid summary in %s: %s\n", path, line);
    goto out_with_dir;
  }

  while ((dirent = readdir(dir)) != NULL) {
    if (!strncmp(dirent->d_name, "pu", 2)) {
      char *end;
      unsigned long idx = strtoul(dirent->d_name+2, &end, 10);
      if (!*end)
	hwloc_bitmap_set(set, idx);
      else
	fprintf(stderr, "Ignoring invalid dirent `%s' in dumped cpuid directory `%s'\n",
		dirent->d_name, src_cpuiddump_path);
    }
  }
  closedir(dir);

  if (hwloc_bitmap_iszero(set)) {
    fprintf(stderr, "Did not find any valid pu%%u entry in dumped cpuid directory `%s'\n",
	    src_cpuiddump_path);
    return -1;
  } else if (hwloc_bitmap_last(set) != hwloc_bitmap_weight(set) - 1) {
    /* The x86 backends enforces contigous set of PUs starting at 0 so far */
    fprintf(stderr, "Found non-contigous pu%%u range in dumped cpuid directory `%s'\n",
	    src_cpuiddump_path);
    return -1;
  }

  return 0;

out_with_dir:
  closedir(dir);
#endif /* HWLOC_WIN_SYS & !__MINGW32__ needs a lot of work */
  return -1;
}

static void
hwloc_x86_backend_disable(struct hwloc_backend *backend)
{
  struct hwloc_x86_backend_data_s *data = backend->private_data;
  hwloc_bitmap_free(data->apicid_set);
  free(data->src_cpuiddump_path);
  free(data);
}

static struct hwloc_backend *
hwloc_x86_component_instantiate(struct hwloc_disc_component *component,
				const void *_data1 __hwloc_attribute_unused,
				const void *_data2 __hwloc_attribute_unused,
				const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;
  struct hwloc_x86_backend_data_s *data;
  const char *src_cpuiddump_path;

  backend = hwloc_backend_alloc(component);
  if (!backend)
    goto out;

  data = malloc(sizeof(*data));
  if (!data) {
    errno = ENOMEM;
    goto out_with_backend;
  }

  backend->private_data = data;
  backend->discover = hwloc_x86_discover;
  backend->disable = hwloc_x86_backend_disable;

  /* default values */
  data->is_knl = 0;
  data->apicid_set = hwloc_bitmap_alloc();
  data->apicid_unique = 1;
  data->src_cpuiddump_path = NULL;

  src_cpuiddump_path = getenv("HWLOC_CPUID_PATH");
  if (src_cpuiddump_path) {
    hwloc_bitmap_t set = hwloc_bitmap_alloc();
    if (!hwloc_x86_check_cpuiddump_input(src_cpuiddump_path, set)) {
      backend->is_thissystem = 0;
      data->src_cpuiddump_path = strdup(src_cpuiddump_path);
      assert(!hwloc_bitmap_iszero(set)); /* enforced by hwloc_x86_check_cpuiddump_input() */
      data->nbprocs = hwloc_bitmap_weight(set);
    } else {
      fprintf(stderr, "Ignoring dumped cpuid directory.\n");
    }
    hwloc_bitmap_free(set);
  }

  return backend;

 out_with_backend:
  free(backend);
 out:
  return NULL;
}

static struct hwloc_disc_component hwloc_x86_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_CPU,
  "x86",
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  hwloc_x86_component_instantiate,
  45, /* between native and no_os */
  1,
  NULL
};

const struct hwloc_component hwloc_x86_component = {
  HWLOC_COMPONENT_ABI,
  NULL, NULL,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_x86_disc_component
};
