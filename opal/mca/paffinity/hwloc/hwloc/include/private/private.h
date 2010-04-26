/*
 * Copyright © 2009      CNRS, INRIA, Université Bordeaux 1
 * Copyright © 2009-2010 Cisco Systems, Inc.  All rights reserved.
 *
 * See COPYING in top-level directory.
 */

/* Internal types and helpers. */

#ifndef HWLOC_PRIVATE_H
#define HWLOC_PRIVATE_H

#include <private/config.h>
#include <hwloc.h>
#include <hwloc/cpuset.h>
#include <private/debug.h>

#include <string.h>

#ifdef HWLOC_HAVE_ATTRIBUTE_FORMAT
# if HWLOC_HAVE_ATTRIBUTE_FORMAT
#  define __hwloc_attribute_format(type, str, arg)  __attribute__((__format__(type, str, arg)))
# else
#  define __hwloc_attribute_format(type, str, arg)
# endif
#else
# define __hwloc_attribute_format(type, str, arg)
#endif

enum hwloc_ignore_type_e {
  HWLOC_IGNORE_TYPE_NEVER = 0,
  HWLOC_IGNORE_TYPE_KEEP_STRUCTURE,
  HWLOC_IGNORE_TYPE_ALWAYS
};

/* Maximal value of an object type */
#define HWLOC_OBJ_TYPE_MAX (HWLOC_OBJ_MISC+1)
#define HWLOC_DEPTH_MAX 128

typedef enum hwloc_backend_e {
  HWLOC_BACKEND_NONE,
  HWLOC_BACKEND_SYNTHETIC,
#ifdef HWLOC_LINUX_SYS
  HWLOC_BACKEND_SYSFS,
#endif
#ifdef HWLOC_HAVE_XML
  HWLOC_BACKEND_XML,
#endif
  /* This value is only here so that we can end the enum list without
     a comma (thereby preventing compiler warnings) */
  HWLOC_BACKEND_MAX
} hwloc_backend_t;

struct hwloc_topology {
  unsigned nb_levels;					/* Number of horizontal levels */
  unsigned level_nbobjects[HWLOC_DEPTH_MAX]; 		/* Number of objects on each horizontal level */
  struct hwloc_obj **levels[HWLOC_DEPTH_MAX];		/* Direct access to levels, levels[l = 0 .. nblevels-1][0..level_nbobjects[l]] */
  unsigned long flags;
  int type_depth[HWLOC_OBJ_TYPE_MAX];
  enum hwloc_ignore_type_e ignored_types[HWLOC_OBJ_TYPE_MAX];
  int is_thissystem;
  int is_loaded;
  hwloc_pid_t pid;                                      /* Process ID the topology is view from, 0 for self */

  int (*set_thisproc_cpubind)(hwloc_topology_t topology, hwloc_const_cpuset_t set, int policy);
  int (*get_thisproc_cpubind)(hwloc_topology_t topology, hwloc_cpuset_t set, int policy);
  int (*set_thisthread_cpubind)(hwloc_topology_t topology, hwloc_const_cpuset_t set, int policy);
  int (*get_thisthread_cpubind)(hwloc_topology_t topology, hwloc_cpuset_t set, int policy);
  int (*set_proc_cpubind)(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_cpuset_t set, int policy);
  int (*get_proc_cpubind)(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_cpuset_t set, int policy);
#ifdef hwloc_thread_t
  int (*set_thread_cpubind)(hwloc_topology_t topology, hwloc_thread_t tid, hwloc_const_cpuset_t set, int policy);
  int (*get_thread_cpubind)(hwloc_topology_t topology, hwloc_thread_t tid, hwloc_cpuset_t set, int policy);
#endif

  struct hwloc_topology_support support;

  hwloc_backend_t backend_type;
  union hwloc_backend_params_u {
#ifdef HWLOC_LINUX_SYS
    struct hwloc_backend_params_sysfs_s {
      /* sysfs backend parameters */
      int root_fd; /* The file descriptor for the file system root, used when browsing, e.g., Linux' sysfs and procfs. */
    } sysfs;
#endif /* HWLOC_LINUX_SYS */
#if defined(HWLOC_OSF_SYS) || defined(HWLOC_COMPILE_PORTS)
    struct hwloc_backend_params_osf {
      int nbnodes;
    } osf;
#endif /* HWLOC_OSF_SYS */
#ifdef HWLOC_HAVE_XML
    struct hwloc_backend_params_xml_s {
      /* xml backend parameters */
      void *doc;
    } xml;
#endif /* HWLOC_HAVE_XML */
    struct hwloc_backend_params_synthetic_s {
      /* synthetic backend parameters */
#define HWLOC_SYNTHETIC_MAX_DEPTH 128
      unsigned arity[HWLOC_SYNTHETIC_MAX_DEPTH];
      hwloc_obj_type_t type[HWLOC_SYNTHETIC_MAX_DEPTH];
      unsigned id[HWLOC_SYNTHETIC_MAX_DEPTH];
      unsigned depth[HWLOC_SYNTHETIC_MAX_DEPTH]; /* For cache/misc */
    } synthetic;
  } backend_params;
};


extern void hwloc_setup_pu_level(struct hwloc_topology *topology, unsigned nb_pus);
extern void hwloc_setup_misc_level_from_distances(struct hwloc_topology *topology, unsigned nbobjs, struct hwloc_obj **objs, unsigned *_distances/*[nbnobjs][nbobjs]*/);
extern int hwloc_get_sysctlbyname(const char *name, int *n);
extern int hwloc_get_sysctl(int name[], unsigned namelen, int *n);
extern unsigned hwloc_fallback_nbprocessors(struct hwloc_topology *topology);

#if defined(HWLOC_LINUX_SYS)
extern void hwloc_look_linux(struct hwloc_topology *topology);
extern void hwloc_set_linux_hooks(struct hwloc_topology *topology);
extern int hwloc_backend_sysfs_init(struct hwloc_topology *topology, const char *fsroot_path);
extern void hwloc_backend_sysfs_exit(struct hwloc_topology *topology);
#endif /* HWLOC_LINUX_SYS */

#ifdef HWLOC_HAVE_XML
extern int hwloc_backend_xml_init(struct hwloc_topology *topology, const char *xmlpath);
extern void hwloc_look_xml(struct hwloc_topology *topology);
extern void hwloc_backend_xml_exit(struct hwloc_topology *topology);
#endif /* HWLOC_HAVE_XML */

#ifdef HWLOC_SOLARIS_SYS
extern void hwloc_look_solaris(struct hwloc_topology *topology);
extern void hwloc_set_solaris_hooks(struct hwloc_topology *topology);
#endif /* HWLOC_SOLARIS_SYS */

#ifdef HWLOC_AIX_SYS
extern void hwloc_look_aix(struct hwloc_topology *topology);
extern void hwloc_set_aix_hooks(struct hwloc_topology *topology);
#endif /* HWLOC_AIX_SYS */

#ifdef HWLOC_OSF_SYS
extern void hwloc_look_osf(struct hwloc_topology *topology);
extern void hwloc_set_osf_hooks(struct hwloc_topology *topology);
#endif /* HWLOC_OSF_SYS */

#ifdef HWLOC_WIN_SYS
extern void hwloc_look_windows(struct hwloc_topology *topology);
extern void hwloc_set_windows_hooks(struct hwloc_topology *topology);
#endif /* HWLOC_WIN_SYS */

#ifdef HWLOC_DARWIN_SYS
extern void hwloc_look_darwin(struct hwloc_topology *topology);
extern void hwloc_set_darwin_hooks(struct hwloc_topology *topology);
#endif /* HWLOC_DARWIN_SYS */

#ifdef HWLOC_FREEBSD_SYS
extern void hwloc_look_freebsd(struct hwloc_topology *topology);
extern void hwloc_set_freebsd_hooks(struct hwloc_topology *topology);
#endif /* HWLOC_FREEBSD_SYS */

#ifdef HWLOC_HPUX_SYS
extern void hwloc_look_hpux(struct hwloc_topology *topology);
extern void hwloc_set_hpux_hooks(struct hwloc_topology *topology);
#endif /* HWLOC_HPUX_SYS */

extern void hwloc_look_x86(struct hwloc_topology *topology, unsigned nbprocs);

extern int hwloc_backend_synthetic_init(struct hwloc_topology *topology, const char *description);
extern void hwloc_backend_synthetic_exit(struct hwloc_topology *topology);
extern void hwloc_look_synthetic (struct hwloc_topology *topology);

/*
 * Add an object to the topology.
 * It is sorted along the tree of other objects according to the inclusion of
 * cpusets, to eventually be added as a child of the smallest object including
 * this object.
 *
 * If the cpuset is empty, the type of the object (and maybe some attributes)
 * must be enough to find where to insert the object. This is especially true
 * for NUMA nodes with memory and no CPUs.
 *
 * The given object should not have children.
 *
 * This shall only be called before levels are built.
 */
extern void hwloc_insert_object_by_cpuset(struct hwloc_topology *topology, hwloc_obj_t obj);

/*
 * Insert an object somewhere in the topology.
 *
 * It is added as the last child of the given parent.
 * The cpuset is completely ignored, so strange objects such as I/O devices should
 * preferably be inserted with this.
 *
 * The given object may have children.
 *
 * Remember to call topology_connect() afterwards to fix handy pointers.
 */
extern void hwloc_insert_object_by_parent(struct hwloc_topology *topology, hwloc_obj_t parent, hwloc_obj_t obj);

/** \brief Return a locally-allocated stringified cpuset for printf-like calls. */
static inline char *
hwloc_cpuset_printf_value(hwloc_const_cpuset_t cpuset)
{
  char *buf;
  hwloc_cpuset_asprintf(&buf, cpuset);
  return buf;
}

static inline struct hwloc_obj *
hwloc_alloc_setup_object(hwloc_obj_type_t type, signed idx)
{
  struct hwloc_obj *obj = malloc(sizeof(*obj));
  memset(obj, 0, sizeof(*obj));
  obj->type = type;
  obj->os_index = idx;
  obj->os_level = -1;
  obj->attr = malloc(sizeof(*obj->attr));
  memset(obj->attr, 0, sizeof(*obj->attr));
  /* do not allocate the cpuset here, let the caller do it */
  return obj;
}

extern void free_object(hwloc_obj_t obj);

#define hwloc_object_cpuset_from_array(l, _value, _array, _max) do {	\
		struct hwloc_obj *__l = (l);				\
		unsigned int *__a = (_array);				\
		int k;							\
		__l->cpuset = hwloc_cpuset_alloc();			\
		for(k=0; k<_max; k++)					\
			if (__a[k] == _value)				\
				hwloc_cpuset_set(__l->cpuset, k);	\
	} while (0)

/* Configures an array of NUM objects of type TYPE with physical IDs OSPHYSIDS
 * and for which processors have ID PROC_PHYSIDS, and add them to the topology.
 * */
static inline void
hwloc_setup_level(int procid_max, unsigned num, unsigned *osphysids, unsigned *proc_physids, struct hwloc_topology *topology, hwloc_obj_type_t type)
{
  struct hwloc_obj *obj;
  unsigned j;

  hwloc_debug("%d %s\n", num, hwloc_obj_type_string(type));

  for (j = 0; j < num; j++)
    {
      obj = hwloc_alloc_setup_object(type, osphysids[j]);
      hwloc_object_cpuset_from_array(obj, j, proc_physids, procid_max);
      hwloc_debug_2args_cpuset("%s %d has cpuset %s\n",
		 hwloc_obj_type_string(type),
		 j, obj->cpuset);
      hwloc_insert_object_by_cpuset(topology, obj);
    }
  hwloc_debug("%s", "\n");
}

#endif /* HWLOC_PRIVATE_H */
