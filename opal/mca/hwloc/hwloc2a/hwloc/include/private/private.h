/*
 * Copyright © 2009      CNRS
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009-2012 Université Bordeaux
 * Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
 *
 * See COPYING in top-level directory.
 */

/* Internal types and helpers. */


#ifdef HWLOC_INSIDE_PLUGIN
/*
 * these declarations are internal only, they are not available to plugins
 * (many functions below are internal static symbols).
 */
#error This file should not be used in plugins
#endif


#ifndef HWLOC_PRIVATE_H
#define HWLOC_PRIVATE_H

#include <private/autogen/config.h>
#include <hwloc.h>
#include <hwloc/bitmap.h>
#include <private/components.h>
#include <private/debug.h>
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif
#include <string.h>

struct hwloc_topology {
  unsigned nb_levels;                                   /* Number of horizontal levels */
  unsigned nb_levels_allocated;                         /* Number of levels allocated and zeroed in level_nbobjects and levels below */
  unsigned *level_nbobjects;                            /* Number of objects on each horizontal level */
  struct hwloc_obj ***levels;                           /* Direct access to levels, levels[l = 0 .. nblevels-1][0..level_nbobjects[l]] */
  unsigned long flags;
  int type_depth[HWLOC_OBJ_TYPE_MAX];
  enum hwloc_type_filter_e type_filter[HWLOC_OBJ_TYPE_MAX];
  int is_thissystem;
  int is_loaded;
  int modified;                                         /* >0 if objects were added/removed recently, which means a reconnect is needed */
  hwloc_pid_t pid;                                      /* Process ID the topology is view from, 0 for self */
  void *userdata;
  uint64_t next_gp_index;

  void *adopted_shmem_addr;
  size_t adopted_shmem_length;

#define HWLOC_NR_SLEVELS 4
#define HWLOC_SLEVEL_BRIDGE 0
#define HWLOC_SLEVEL_PCIDEV 1
#define HWLOC_SLEVEL_OSDEV 2
#define HWLOC_SLEVEL_MISC 3
  /* order must match negative depth, it's asserted in setup_defaults() */
#define HWLOC_SLEVEL_FROM_DEPTH(x) (HWLOC_TYPE_DEPTH_BRIDGE-(x))
#define HWLOC_SLEVEL_TO_DEPTH(x) (HWLOC_TYPE_DEPTH_BRIDGE-(x))
  struct hwloc_special_level_s {
    unsigned nbobjs;
    struct hwloc_obj **objs;
    struct hwloc_obj *first, *last; /* Temporarily used while listing object before building the objs array */
  } slevels[HWLOC_NR_SLEVELS];

  int pci_nonzero_domains;
  int need_pci_belowroot_apply_locality;
  struct hwloc_backend *get_pci_busid_cpuset_backend;

  int pci_has_forced_locality;
  unsigned pci_forced_locality_nr;
  struct hwloc_pci_forced_locality_s {
    unsigned domain;
    unsigned bus_first, bus_last;
    hwloc_bitmap_t cpuset;
  } * pci_forced_locality;

  struct hwloc_binding_hooks {
    int (*set_thisproc_cpubind)(hwloc_topology_t topology, hwloc_const_cpuset_t set, int flags);
    int (*get_thisproc_cpubind)(hwloc_topology_t topology, hwloc_cpuset_t set, int flags);
    int (*set_thisthread_cpubind)(hwloc_topology_t topology, hwloc_const_cpuset_t set, int flags);
    int (*get_thisthread_cpubind)(hwloc_topology_t topology, hwloc_cpuset_t set, int flags);
    int (*set_proc_cpubind)(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_cpuset_t set, int flags);
    int (*get_proc_cpubind)(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_cpuset_t set, int flags);
#ifdef hwloc_thread_t
    int (*set_thread_cpubind)(hwloc_topology_t topology, hwloc_thread_t tid, hwloc_const_cpuset_t set, int flags);
    int (*get_thread_cpubind)(hwloc_topology_t topology, hwloc_thread_t tid, hwloc_cpuset_t set, int flags);
#endif

    int (*get_thisproc_last_cpu_location)(hwloc_topology_t topology, hwloc_cpuset_t set, int flags);
    int (*get_thisthread_last_cpu_location)(hwloc_topology_t topology, hwloc_cpuset_t set, int flags);
    int (*get_proc_last_cpu_location)(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_cpuset_t set, int flags);

    int (*set_thisproc_membind)(hwloc_topology_t topology, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags);
    int (*get_thisproc_membind)(hwloc_topology_t topology, hwloc_nodeset_t nodeset, hwloc_membind_policy_t * policy, int flags);
    int (*set_thisthread_membind)(hwloc_topology_t topology, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags);
    int (*get_thisthread_membind)(hwloc_topology_t topology, hwloc_nodeset_t nodeset, hwloc_membind_policy_t * policy, int flags);
    int (*set_proc_membind)(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags);
    int (*get_proc_membind)(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_nodeset_t nodeset, hwloc_membind_policy_t * policy, int flags);
    int (*set_area_membind)(hwloc_topology_t topology, const void *addr, size_t len, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags);
    int (*get_area_membind)(hwloc_topology_t topology, const void *addr, size_t len, hwloc_nodeset_t nodeset, hwloc_membind_policy_t * policy, int flags);
    int (*get_area_memlocation)(hwloc_topology_t topology, const void *addr, size_t len, hwloc_nodeset_t nodeset, int flags);
    /* This has to return the same kind of pointer as alloc_membind, so that free_membind can be used on it */
    void *(*alloc)(hwloc_topology_t topology, size_t len);
    /* alloc_membind has to always succeed if !(flags & HWLOC_MEMBIND_STRICT).
     * see hwloc_alloc_or_fail which is convenient for that.  */
    void *(*alloc_membind)(hwloc_topology_t topology, size_t len, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags);
    int (*free_membind)(hwloc_topology_t topology, void *addr, size_t len);

    int (*get_allowed_resources)(hwloc_topology_t topology);
  } binding_hooks;

  struct hwloc_topology_support support;

  void (*userdata_export_cb)(void *reserved, struct hwloc_topology *topology, struct hwloc_obj *obj);
  void (*userdata_import_cb)(struct hwloc_topology *topology, struct hwloc_obj *obj, const char *name, const void *buffer, size_t length);
  int userdata_not_decoded;

  struct hwloc_internal_distances_s {
    hwloc_obj_type_t type;
    /* add union hwloc_obj_attr_u if we ever support groups */
    unsigned nbobjs;
    uint64_t *indexes; /* array of OS or GP indexes before we can convert them into objs. */
    uint64_t *values; /* distance matrices, ordered according to the above indexes/objs array.
                       * distance from i to j is stored in slot i*nbnodes+j.
                       */
    unsigned long kind;

    /* objects are currently stored in physical_index order */
    hwloc_obj_t *objs; /* array of objects */
    int objs_are_valid; /* set to 1 if the array objs is still valid, 0 if needs refresh */

    struct hwloc_internal_distances_s *prev, *next;
  } *first_dist, *last_dist;

  int grouping;
  int grouping_verbose;
  unsigned grouping_nbaccuracies;
  float grouping_accuracies[5];
  int grouping_next_subkind;

  /* list of enabled backends. */
  struct hwloc_backend * backends;
  unsigned backend_excludes;

  /* memory allocator for topology objects */
  struct hwloc_tma * tma;
};

extern void hwloc_alloc_obj_cpusets(hwloc_obj_t obj);
extern void hwloc_setup_pu_level(struct hwloc_topology *topology, unsigned nb_pus);
extern int hwloc_get_sysctlbyname(const char *name, int64_t *n);
extern int hwloc_get_sysctl(int name[], unsigned namelen, int *n);
extern unsigned hwloc_fallback_nbprocessors(struct hwloc_topology *topology);

extern int hwloc__object_cpusets_compare_first(hwloc_obj_t obj1, hwloc_obj_t obj2);
extern void hwloc__reorder_children(hwloc_obj_t parent);

extern void hwloc_topology_setup_defaults(struct hwloc_topology *topology);
extern void hwloc_topology_clear(struct hwloc_topology *topology);

extern void hwloc_pci_discovery_init(struct hwloc_topology *topology);
extern void hwloc_pci_discovery_prepare(struct hwloc_topology *topology);
extern void hwloc_pci_discovery_exit(struct hwloc_topology *topology);

/* Look for an object matching complete cpuset exactly, or insert one.
 * Return NULL on failure.
 * Return a good fallback (object above) on failure to insert.
 */
extern hwloc_obj_t hwloc_find_insert_io_parent_by_complete_cpuset(struct hwloc_topology *topology, hwloc_cpuset_t cpuset);

/* Move PCI objects currently attached to the root object ot their actual location.
 * Called by the core at the end of hwloc_topology_load().
 * Prior to this call, all PCI objects may be found below the root object.
 * After this call and a reconnect of levels, all PCI objects are available through levels.
 */
extern int hwloc_pci_belowroot_apply_locality(struct hwloc_topology *topology);

HWLOC_DECLSPEC extern const char * hwloc_pci_class_string(unsigned short class_id);

extern void hwloc__add_info(struct hwloc_obj_info_s **infosp, unsigned *countp, const char *name, const char *value);
extern char ** hwloc__find_info_slot(struct hwloc_obj_info_s **infosp, unsigned *countp, const char *name);
extern void hwloc__move_infos(struct hwloc_obj_info_s **dst_infosp, unsigned *dst_countp, struct hwloc_obj_info_s **src_infosp, unsigned *src_countp);
extern void hwloc__free_infos(struct hwloc_obj_info_s *infos, unsigned count);

/* set native OS binding hooks */
extern void hwloc_set_native_binding_hooks(struct hwloc_binding_hooks *hooks, struct hwloc_topology_support *support);
/* set either native OS binding hooks (if thissystem), or dummy ones */
extern void hwloc_set_binding_hooks(struct hwloc_topology *topology);

#if defined(HWLOC_LINUX_SYS)
extern void hwloc_set_linuxfs_hooks(struct hwloc_binding_hooks *binding_hooks, struct hwloc_topology_support *support);
#endif /* HWLOC_LINUX_SYS */

#if defined(HWLOC_BGQ_SYS)
extern void hwloc_set_bgq_hooks(struct hwloc_binding_hooks *binding_hooks, struct hwloc_topology_support *support);
#endif /* HWLOC_BGQ_SYS */

#ifdef HWLOC_SOLARIS_SYS
extern void hwloc_set_solaris_hooks(struct hwloc_binding_hooks *binding_hooks, struct hwloc_topology_support *support);
#endif /* HWLOC_SOLARIS_SYS */

#ifdef HWLOC_AIX_SYS
extern void hwloc_set_aix_hooks(struct hwloc_binding_hooks *binding_hooks, struct hwloc_topology_support *support);
#endif /* HWLOC_AIX_SYS */

#ifdef HWLOC_WIN_SYS
extern void hwloc_set_windows_hooks(struct hwloc_binding_hooks *binding_hooks, struct hwloc_topology_support *support);
#endif /* HWLOC_WIN_SYS */

#ifdef HWLOC_DARWIN_SYS
extern void hwloc_set_darwin_hooks(struct hwloc_binding_hooks *binding_hooks, struct hwloc_topology_support *support);
#endif /* HWLOC_DARWIN_SYS */

#ifdef HWLOC_FREEBSD_SYS
extern void hwloc_set_freebsd_hooks(struct hwloc_binding_hooks *binding_hooks, struct hwloc_topology_support *support);
#endif /* HWLOC_FREEBSD_SYS */

#ifdef HWLOC_NETBSD_SYS
extern void hwloc_set_netbsd_hooks(struct hwloc_binding_hooks *binding_hooks, struct hwloc_topology_support *support);
#endif /* HWLOC_NETBSD_SYS */

#ifdef HWLOC_HPUX_SYS
extern void hwloc_set_hpux_hooks(struct hwloc_binding_hooks *binding_hooks, struct hwloc_topology_support *support);
#endif /* HWLOC_HPUX_SYS */

extern int hwloc_look_hardwired_fujitsu_k(struct hwloc_topology *topology);
extern int hwloc_look_hardwired_fujitsu_fx10(struct hwloc_topology *topology);
extern int hwloc_look_hardwired_fujitsu_fx100(struct hwloc_topology *topology);

/* Insert uname-specific names/values in the object infos array.
 * If cached_uname isn't NULL, it is used as a struct utsname instead of recalling uname.
 * Any field that starts with \0 is ignored.
 */
extern void hwloc_add_uname_info(struct hwloc_topology *topology, void *cached_uname);

/* Free obj and its attributes assuming it's not linked to a parent and doesn't have any child */
extern void hwloc_free_unlinked_object(hwloc_obj_t obj);

/* Free obj and its children, assuming it's not linked to a parent */
extern void hwloc_free_object_and_children(hwloc_obj_t obj);

/* Free obj, its next siblings, and their children, assuming they're not linked to a parent */
extern void hwloc_free_object_siblings_and_children(hwloc_obj_t obj);

/* This can be used for the alloc field to get allocated data that can be freed by free() */
void *hwloc_alloc_heap(hwloc_topology_t topology, size_t len);

/* This can be used for the alloc field to get allocated data that can be freed by munmap() */
void *hwloc_alloc_mmap(hwloc_topology_t topology, size_t len);

/* This can be used for the free_membind field to free data using free() */
int hwloc_free_heap(hwloc_topology_t topology, void *addr, size_t len);

/* This can be used for the free_membind field to free data using munmap() */
int hwloc_free_mmap(hwloc_topology_t topology, void *addr, size_t len);

/* Allocates unbound memory or fail, depending on whether STRICT is requested
 * or not */
static __hwloc_inline void *
hwloc_alloc_or_fail(hwloc_topology_t topology, size_t len, int flags)
{
  if (flags & HWLOC_MEMBIND_STRICT)
    return NULL;
  return hwloc_alloc(topology, len);
}

extern void hwloc_internal_distances_init(hwloc_topology_t topology);
extern void hwloc_internal_distances_prepare(hwloc_topology_t topology);
extern void hwloc_internal_distances_destroy(hwloc_topology_t topology);
extern int hwloc_internal_distances_dup(hwloc_topology_t new, hwloc_topology_t old);
extern void hwloc_internal_distances_refresh(hwloc_topology_t topology);
extern int hwloc_internal_distances_add(hwloc_topology_t topology, unsigned nbobjs, hwloc_obj_t *objs, uint64_t *values, unsigned long kind, unsigned long flags);
extern int hwloc_internal_distances_add_by_index(hwloc_topology_t topology, hwloc_obj_type_t type, unsigned nbobjs, uint64_t *indexes, uint64_t *values, unsigned long kind, unsigned long flags);
extern void hwloc_internal_distances_invalidate_cached_objs(hwloc_topology_t topology);

#ifdef HAVE_USELOCALE
#include "locale.h"
#ifdef HAVE_XLOCALE_H
#include "xlocale.h"
#endif
#define hwloc_localeswitch_declare locale_t __old_locale = (locale_t)0, __new_locale
#define hwloc_localeswitch_init() do {                     \
  __new_locale = newlocale(LC_ALL_MASK, "C", (locale_t)0); \
  if (__new_locale != (locale_t)0)                         \
    __old_locale = uselocale(__new_locale);                \
} while (0)
#define hwloc_localeswitch_fini() do { \
  if (__new_locale != (locale_t)0) {   \
    uselocale(__old_locale);           \
    freelocale(__new_locale);          \
  }                                    \
} while(0)
#else /* HAVE_USELOCALE */
#if __HWLOC_HAVE_ATTRIBUTE_UNUSED
#define hwloc_localeswitch_declare int __dummy_nolocale __hwloc_attribute_unused
#define hwloc_localeswitch_init()
#else
#define hwloc_localeswitch_declare int __dummy_nolocale
#define hwloc_localeswitch_init() (void)__dummy_nolocale
#endif
#define hwloc_localeswitch_fini()
#endif /* HAVE_USELOCALE */

#if !HAVE_DECL_FABSF
#define fabsf(f) fabs((double)(f))
#endif

#if HAVE_DECL__SC_PAGE_SIZE
#define hwloc_getpagesize() sysconf(_SC_PAGE_SIZE)
#elif HAVE_DECL__SC_PAGESIZE
#define hwloc_getpagesize() sysconf(_SC_PAGESIZE)
#elif defined HAVE_GETPAGESIZE
#define hwloc_getpagesize() getpagesize()
#else
#undef hwloc_getpagesize
#endif

/* encode src buffer into target buffer.
 * targsize must be at least 4*((srclength+2)/3)+1.
 * target will be 0-terminated.
 */
extern int hwloc_encode_to_base64(const char *src, size_t srclength, char *target, size_t targsize);
/* decode src buffer into target buffer.
 * src is 0-terminated.
 * targsize must be at least srclength*3/4+1 (srclength not including \0)
 * but only srclength*3/4 characters will be meaningful
 * (the next one may be partially written during decoding, but it should be ignored).
 */
extern int hwloc_decode_from_base64(char const *src, char *target, size_t targsize);

/* Check whether needle matches the beginning of haystack, at least n, and up
 * to a colon or \0 */
extern int hwloc_namecoloncmp(const char *haystack, const char *needle, size_t n);

#ifdef HWLOC_HAVE_ATTRIBUTE_FORMAT
# if HWLOC_HAVE_ATTRIBUTE_FORMAT
#  define __hwloc_attribute_format(type, str, arg)  __attribute__((__format__(type, str, arg)))
# else
#  define __hwloc_attribute_format(type, str, arg)
# endif
#else
# define __hwloc_attribute_format(type, str, arg)
#endif

#define hwloc_memory_size_printf_value(_size, _verbose) \
  ((_size) < (10ULL<<20) || _verbose ? (((_size)>>9)+1)>>1 : (_size) < (10ULL<<30) ? (((_size)>>19)+1)>>1 : (_size) < (10ULL<<40) ? (((_size)>>29)+1)>>1 : (((_size)>>39)+1)>>1)
#define hwloc_memory_size_printf_unit(_size, _verbose) \
  ((_size) < (10ULL<<20) || _verbose ? "KB" : (_size) < (10ULL<<30) ? "MB" : (_size) < (10ULL<<40) ? "GB" : "TB")

/* On some systems, snprintf returns the size of written data, not the actually
 * required size.  hwloc_snprintf always report the actually required size. */
extern int hwloc_snprintf(char *str, size_t size, const char *format, ...) __hwloc_attribute_format(printf, 3, 4);

extern void hwloc_obj_add_info_nodup(hwloc_obj_t obj, const char *name, const char *value, int nodup);

/* Return the name of the currently running program, if supported.
 * If not NULL, must be freed by the caller.
 */
extern char * hwloc_progname(struct hwloc_topology *topology);

#define HWLOC_BITMAP_EQUAL 0       /* Bitmaps are equal */
#define HWLOC_BITMAP_INCLUDED 1    /* First bitmap included in second */
#define HWLOC_BITMAP_CONTAINS 2    /* First bitmap contains second */
#define HWLOC_BITMAP_INTERSECTS 3  /* Bitmaps intersect without any inclusion */
#define HWLOC_BITMAP_DIFFERENT  4  /* Bitmaps do not intersect */

/** \brief Compare bitmaps \p bitmap1 and \p bitmap2 from an inclusion point of view.
 */
HWLOC_DECLSPEC int hwloc_bitmap_compare_inclusion(hwloc_const_bitmap_t bitmap1, hwloc_const_bitmap_t bitmap2) __hwloc_attribute_pure;

/* obj->attr->group.kind internal values.
 * the core will keep the highest ones when merging two groups.
 */
#define HWLOC_GROUP_KIND_NONE                           0 /* user can use subkind */
#define HWLOC_GROUP_KIND_DISTANCE                       1 /* subkind is round of adding these groups during distance based grouping */
#define HWLOC_GROUP_KIND_IO                             2 /* no subkind */
#define HWLOC_GROUP_KIND_WINDOWS_RELATIONSHIP_UNKNOWN   3 /* no subkind */
#define HWLOC_GROUP_KIND_WINDOWS_PROCESSOR_GROUP        4 /* no subkind */
#define HWLOC_GROUP_KIND_AIX_SDL_UNKNOWN                5 /* subkind is SDL level */
#define HWLOC_GROUP_KIND_INTEL_X2APIC_UNKNOWN           6 /* subkind is x2APIC unknown level */
#define HWLOC_GROUP_KIND_S390_BOOK                      7 /* no subkind */
#define HWLOC_GROUP_KIND_INTEL_SUBNUMA_CLUSTER          8 /* no subkind */
#define HWLOC_GROUP_KIND_AMD_COMPUTE_UNIT               9 /* no subkind */
#define HWLOC_GROUP_KIND_SYNTHETIC                      10 /* subkind is group depth within synthetic description */

/* memory allocator for topology objects */
struct hwloc_tma {
  void * (*malloc)(struct hwloc_tma *, size_t);
  void *data;
  int dontfree; /* when set, free() or realloc() cannot be used, and tma->malloc() cannot fail */
};

static __hwloc_inline void *
hwloc_tma_malloc(struct hwloc_tma *tma,
                 size_t size)
{
  if (tma) {
    return tma->malloc(tma, size);
  } else {
    return malloc(size);
  }
}

static __hwloc_inline void *
hwloc_tma_calloc(struct hwloc_tma *tma,
                 size_t size)
{
  char *ptr = hwloc_tma_malloc(tma, size);
  if (ptr)
    memset(ptr, 0, size);
  return ptr;
}

static __hwloc_inline char *
hwloc_tma_strdup(struct hwloc_tma *tma,
                 const char *src)
{
  size_t len = strlen(src);
  char *ptr = hwloc_tma_malloc(tma, len+1);
  if (ptr)
    memcpy(ptr, src, len+1);
  return ptr;
}

/* bitmap allocator to be used inside hwloc */
extern hwloc_bitmap_t hwloc_bitmap_tma_dup(struct hwloc_tma *tma, hwloc_const_bitmap_t old);

extern int hwloc__topology_dup(hwloc_topology_t *newp, hwloc_topology_t old, struct hwloc_tma *tma);
extern void hwloc__topology_disadopt(hwloc_topology_t  topology);

#endif /* HWLOC_PRIVATE_H */
