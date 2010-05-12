/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * Copyright © 2009 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/config.h>
#include <hwloc.h>
#include <hwloc/linux.h>
#include <private/private.h>
#include <private/debug.h>

#include <limits.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <assert.h>
#include <dirent.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sched.h>
#include <pthread.h>

#ifndef HWLOC_HAVE_CPU_SET
/* libc doesn't have support for sched_setaffinity, build system call
 * ourselves: */
#    include <linux/unistd.h>
#    ifndef __NR_sched_setaffinity
#       ifdef __i386__
#         define __NR_sched_setaffinity 241
#       elif defined(__x86_64__)
#         define __NR_sched_setaffinity 203
#       elif defined(__ia64__)
#         define __NR_sched_setaffinity 1231
#       elif defined(__hppa__)
#         define __NR_sched_setaffinity 211
#       elif defined(__alpha__)
#         define __NR_sched_setaffinity 395
#       elif defined(__s390__)
#         define __NR_sched_setaffinity 239
#       elif defined(__sparc__)
#         define __NR_sched_setaffinity 261
#       elif defined(__m68k__)
#         define __NR_sched_setaffinity 311
#       elif defined(__powerpc__) || defined(__ppc__) || defined(__PPC__) || defined(__powerpc64__) || defined(__ppc64__)
#         define __NR_sched_setaffinity 222
#       elif defined(__arm__)
#         define __NR_sched_setaffinity 241
#       elif defined(__cris__)
#         define __NR_sched_setaffinity 241
/*#       elif defined(__mips__)
  #         define __NR_sched_setaffinity TODO (32/64/nabi) */
#       else
#         warning "don't know the syscall number for sched_setaffinity on this architecture, will not support binding"
#         define sched_setaffinity(pid, lg, mask) (errno = ENOSYS, -1)
#       endif
#    endif
#    ifndef sched_setaffinity
       _syscall3(int, sched_setaffinity, pid_t, pid, unsigned int, lg, const void *, mask);
#    endif
#    ifndef __NR_sched_getaffinity
#       ifdef __i386__
#         define __NR_sched_getaffinity 242
#       elif defined(__x86_64__)
#         define __NR_sched_getaffinity 204
#       elif defined(__ia64__)
#         define __NR_sched_getaffinity 1232
#       elif defined(__hppa__)
#         define __NR_sched_getaffinity 212
#       elif defined(__alpha__)
#         define __NR_sched_getaffinity 396
#       elif defined(__s390__)
#         define __NR_sched_getaffinity 240
#       elif defined(__sparc__)
#         define __NR_sched_getaffinity 260
#       elif defined(__m68k__)
#         define __NR_sched_getaffinity 312
#       elif defined(__powerpc__) || defined(__ppc__) || defined(__PPC__) || defined(__powerpc64__) || defined(__ppc64__)
#         define __NR_sched_getaffinity 223
#       elif defined(__arm__)
#         define __NR_sched_getaffinity 242
#       elif defined(__cris__)
#         define __NR_sched_getaffinity 242
/*#       elif defined(__mips__)
  #         define __NR_sched_getaffinity TODO (32/64/nabi) */
#       else
#         warning "don't know the syscall number for sched_getaffinity on this architecture, will not support getting binding"
#         define sched_getaffinity(pid, lg, mask) (errno = ENOSYS, -1)
#       endif
#    endif
#    ifndef sched_getaffinity
       _syscall3(int, sched_getaffinity, pid_t, pid, unsigned int, lg, void *, mask);
#    endif
#endif

#ifdef HAVE_OPENAT
/* Use our own filesystem functions if we have openat */

static FILE *
hwloc_fopenat(const char *path, const char *mode, int fsroot_fd)
{
  int fd;
  const char *relative_path;

  if (fsroot_fd < 0) {
    errno = EBADF;
    return NULL;
  }
  if (strcmp(mode, "r")) {
    errno = ENOTSUP;
    return NULL;
  }

  /* Skip leading slashes.  */
  for (relative_path = path; *relative_path == '/'; relative_path++);

  fd = openat (fsroot_fd, relative_path, O_RDONLY);
  if (fd == -1)
    return NULL;

  return fdopen(fd, mode);
}

static int
hwloc_accessat(const char *path, int mode, int fsroot_fd)
{
  const char *relative_path;

  if (fsroot_fd < 0) {
    errno = EBADF;
    return -1;
  }

  /* Skip leading slashes.  */
  for (relative_path = path; *relative_path == '/'; relative_path++);

  return faccessat(fsroot_fd, relative_path, mode, 0);
}

static DIR*
hwloc_opendirat(const char *path, int fsroot_fd)
{
  int dir_fd;
  const char *relative_path;

  /* Skip leading slashes.  */
  for (relative_path = path; *relative_path == '/'; relative_path++);

  dir_fd = openat(fsroot_fd, relative_path, O_RDONLY | O_DIRECTORY);
  if (dir_fd < 0)
    return NULL;

  return fdopendir(dir_fd);
}

#endif /* HAVE_OPENAT */

/* Static inline version of fopen so that we can use openat if we have
   it, but still preserve compiler parameter checking */
static inline FILE *
hwloc_fopen(const char *p, const char *m, int d __hwloc_attribute_unused)
{ 
#ifdef HAVE_OPENAT
    return hwloc_fopenat(p, m, d);
#else
    return fopen(p, m); 
#endif
}

/* Static inline version of access so that we can use openat if we have
   it, but still preserve compiler parameter checking */
static inline int 
hwloc_access(const char *p, int m, int d __hwloc_attribute_unused)
{ 
#ifdef HAVE_OPENAT
    return hwloc_accessat(p, m, d);
#else
    return access(p, m); 
#endif
}

/* Static inline version of opendir so that we can use openat if we have
   it, but still preserve compiler parameter checking */
static inline DIR *
hwloc_opendir(const char *p, int d __hwloc_attribute_unused)
{ 
#ifdef HAVE_OPENAT
    return hwloc_opendirat(p, d);
#else
    return opendir(p); 
#endif
}

int
hwloc_linux_set_tid_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, pid_t tid, hwloc_const_cpuset_t hwloc_set)
{
  /* TODO Kerrighed: Use
   * int migrate (pid_t pid, int destination_node);
   * int migrate_self (int destination_node);
   * int thread_migrate (int thread_id, int destination_node);
   */

  /* The resulting binding is always strict */

#if defined(HWLOC_HAVE_CPU_SET_S) && !defined(HWLOC_HAVE_OLD_SCHED_SETAFFINITY) && CPU_SETSIZE < HWLOC_NBMAXCPUS
  cpu_set_t *plinux_set;
  unsigned cpu;
  size_t setsize = CPU_ALLOC_SIZE(HWLOC_NBMAXCPUS);
  int err;

  plinux_set = CPU_ALLOC(HWLOC_NBMAXCPUS);

  CPU_ZERO_S(setsize, plinux_set);
  hwloc_cpuset_foreach_begin(cpu, hwloc_set)
    CPU_SET_S(cpu, setsize, plinux_set);
  hwloc_cpuset_foreach_end();

  err = sched_setaffinity(tid, setsize, plinux_set);

  CPU_FREE(plinux_set);
  return err;
#elif defined(HWLOC_HAVE_CPU_SET)
  cpu_set_t linux_set;
  unsigned cpu;

  CPU_ZERO(&linux_set);
  hwloc_cpuset_foreach_begin(cpu, hwloc_set)
    CPU_SET(cpu, &linux_set);
  hwloc_cpuset_foreach_end();

#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
  return sched_setaffinity(tid, &linux_set);
#else /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
  return sched_setaffinity(tid, sizeof(linux_set), &linux_set);
#endif /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
#else /* !CPU_SET */
  unsigned long mask = hwloc_cpuset_to_ulong(hwloc_set);

#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
  return sched_setaffinity(tid, (void*) &mask);
#else /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
  return sched_setaffinity(tid, sizeof(mask), (void*) &mask);
#endif /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
#endif /* !CPU_SET */
}

int
hwloc_linux_get_tid_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, pid_t tid, hwloc_cpuset_t hwloc_set)
{
  int err;
  /* TODO Kerrighed */

#if defined(HWLOC_HAVE_CPU_SET_S) && !defined(HWLOC_HAVE_OLD_SCHED_SETAFFINITY) && CPU_SETSIZE < HWLOC_NBMAXCPUS
  cpu_set_t *plinux_set;
  unsigned cpu;
  size_t setsize = CPU_ALLOC_SIZE(HWLOC_NBMAXCPUS);

  plinux_set = CPU_ALLOC(HWLOC_NBMAXCPUS);

  err = sched_getaffinity(tid, setsize, plinux_set);

  if (err < 0) {
    CPU_FREE(plinux_set);
    return -1;
  }

  hwloc_cpuset_zero(hwloc_set);
  for(cpu=0; cpu<HWLOC_NBMAXCPUS; cpu++)
    if (CPU_ISSET_S(cpu, setsize, plinux_set))
      hwloc_cpuset_set(hwloc_set, cpu);

  CPU_FREE(plinux_set);
#elif defined(HWLOC_HAVE_CPU_SET)
  cpu_set_t linux_set;
  unsigned cpu;

#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
  err = sched_getaffinity(tid, &linux_set);
#else /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
  err = sched_getaffinity(tid, sizeof(linux_set), &linux_set);
#endif /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
  if (err < 0)
    return -1;

  hwloc_cpuset_zero(hwloc_set);
  for(cpu=0; cpu<CPU_SETSIZE; cpu++)
    if (CPU_ISSET(cpu, &linux_set))
      hwloc_cpuset_set(hwloc_set, cpu);
#else /* !CPU_SET */
  unsigned long mask;

#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
  err = sched_getaffinity(tid, (void*) &mask);
#else /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
  err = sched_getaffinity(tid, sizeof(mask), (void*) &mask);
#endif /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
  if (err < 0)
    return -1;

  hwloc_cpuset_from_ulong(hwloc_set, mask);
#endif /* !CPU_SET */

  return 0;
}

/* Get the array of tids of a process from the task directory in /proc */
static int
hwloc_linux_get_proc_tids(DIR *taskdir, unsigned *nr_tidsp, pid_t ** tidsp)
{
  struct dirent *dirent;
  unsigned nr_tids = 0;
  unsigned max_tids = 32;
  pid_t *tids;
  struct stat sb;

  /* take the number of links as a good estimate for the number of tids */
  if (fstat(dirfd(taskdir), &sb) == 0)
    max_tids = sb.st_nlink;

  tids = malloc(max_tids*sizeof(pid_t));
  if (!tids) {
    errno = ENOMEM;
    return -1;
  }

  rewinddir(taskdir);

  while ((dirent = readdir(taskdir)) != NULL) {
    if (nr_tids == max_tids) {
      pid_t *newtids;
      max_tids += 8;
      newtids = realloc(tids, max_tids*sizeof(pid_t));
      if (!newtids) {
        free(tids);
        errno = ENOMEM;
        return -1;
      }
      tids = newtids;
    }
    if (!strcmp(dirent->d_name, ".") || !strcmp(dirent->d_name, ".."))
      continue;
    tids[nr_tids++] = atoi(dirent->d_name);
  }

  *nr_tidsp = nr_tids;
  *tidsp = tids;
  return 0;
}

/* Callbacks for binding each process sub-tid */
typedef int (*hwloc_linux_foreach_proc_tid_cb_t)(hwloc_topology_t topology, pid_t tid, void *data, int idx, int policy);

static int
hwloc_linux_foreach_proc_tid_set_cpubind_cb(hwloc_topology_t topology, pid_t tid, void *data, int idx __hwloc_attribute_unused, int policy __hwloc_attribute_unused)
{
  hwloc_cpuset_t cpuset = data;
  return hwloc_linux_set_tid_cpubind(topology, tid, cpuset);
}

static int
hwloc_linux_foreach_proc_tid_get_cpubind_cb(hwloc_topology_t topology, pid_t tid, void *data, int idx, int policy)
{
  hwloc_cpuset_t *cpusets = data;
  hwloc_cpuset_t cpuset = cpusets[0];
  hwloc_cpuset_t tidset = cpusets[1];

  if (hwloc_linux_get_tid_cpubind(topology, tid, tidset))
    return -1;

  /* reset the cpuset on first iteration */
  if (!idx)
    hwloc_cpuset_zero(cpuset);

  if (policy & HWLOC_CPUBIND_STRICT) {
    /* if STRICT, we want all threads to have the same binding */
    if (!idx) {
      /* this is the first thread, copy its binding */
      hwloc_cpuset_copy(cpuset, tidset);
    } else if (!hwloc_cpuset_isequal(cpuset, tidset)) {
      /* this is not the first thread, and it's binding is different */
      errno = EXDEV;
      return -1;
    }
  } else {
    /* if not STRICT, just OR all thread bindings */
    hwloc_cpuset_or(cpuset, cpuset, tidset);
  }
  return 0;
}

/* Call the callback for each process tid. */
static int
hwloc_linux_foreach_proc_tid(hwloc_topology_t topology,
			     pid_t pid, hwloc_linux_foreach_proc_tid_cb_t cb,
			     void *data, int policy)
{
  char taskdir_path[128];
  DIR *taskdir;
  pid_t *tids, *newtids;
  unsigned i, nr, newnr;
  int err;

  if (pid)
    snprintf(taskdir_path, sizeof(taskdir_path), "/proc/%u/task", (unsigned) pid);
  else
    snprintf(taskdir_path, sizeof(taskdir_path), "/proc/self/task");

  taskdir = opendir(taskdir_path);
  if (!taskdir) {
    errno = ENOSYS;
    err = -1;
    goto out;
  }

  /* read the current list of threads */
  err = hwloc_linux_get_proc_tids(taskdir, &nr, &tids);
  if (err < 0)
    goto out_with_dir;

 retry:
  /* apply the callback to all threads */
  for(i=0; i<nr; i++) {
    err = cb(topology, tids[i], data, i, policy);
    if (err < 0)
      goto out_with_tids;
  }

  /* re-read the list of thread and retry if it changed in the meantime */
  err = hwloc_linux_get_proc_tids(taskdir, &newnr, &newtids);
  if (err < 0)
    goto out_with_tids;
  if (newnr != nr || memcmp(newtids, tids, nr*sizeof(pid_t))) {
    free(tids);
    tids = newtids;
    nr = newnr;
    goto retry;
  }

  err = 0;
  free(newtids);
 out_with_tids:
  free(tids);
 out_with_dir:
  closedir(taskdir);
 out:
  return err;
}

static int
hwloc_linux_set_pid_cpubind(hwloc_topology_t topology, pid_t pid, hwloc_const_cpuset_t hwloc_set, int policy)
{
  return hwloc_linux_foreach_proc_tid(topology, pid,
				      hwloc_linux_foreach_proc_tid_set_cpubind_cb,
				      (void*) hwloc_set, policy);
}

static int
hwloc_linux_get_pid_cpubind(hwloc_topology_t topology, pid_t pid, hwloc_cpuset_t hwloc_set, int policy)
{
  hwloc_cpuset_t tidset = hwloc_cpuset_alloc();
  hwloc_cpuset_t cpusets[2] = { hwloc_set, tidset };
  int ret;
  ret = hwloc_linux_foreach_proc_tid(topology, pid,
					 hwloc_linux_foreach_proc_tid_get_cpubind_cb,
					 (void*) cpusets, policy);
  hwloc_cpuset_free(tidset);
  return ret;
}

static int
hwloc_linux_set_proc_cpubind(hwloc_topology_t topology, pid_t pid, hwloc_const_cpuset_t hwloc_set, int policy)
{
  if (pid == 0)
    pid = topology->pid;
  if (policy & HWLOC_CPUBIND_THREAD)
    return hwloc_linux_set_tid_cpubind(topology, pid, hwloc_set);
  else
    return hwloc_linux_set_pid_cpubind(topology, pid, hwloc_set, policy);
}

static int
hwloc_linux_get_proc_cpubind(hwloc_topology_t topology, pid_t pid, hwloc_cpuset_t hwloc_set, int policy)
{
  if (pid == 0)
    pid = topology->pid;
  if (policy & HWLOC_CPUBIND_THREAD)
    return hwloc_linux_get_tid_cpubind(topology, pid, hwloc_set);
  else
    return hwloc_linux_get_pid_cpubind(topology, pid, hwloc_set, policy);
}

static int
hwloc_linux_set_thisproc_cpubind(hwloc_topology_t topology, hwloc_const_cpuset_t hwloc_set, int policy)
{
  return hwloc_linux_set_pid_cpubind(topology, topology->pid, hwloc_set, policy);
}

static int
hwloc_linux_get_thisproc_cpubind(hwloc_topology_t topology, hwloc_cpuset_t hwloc_set, int policy)
{
  return hwloc_linux_get_pid_cpubind(topology, topology->pid, hwloc_set, policy);
}

static int
hwloc_linux_set_thisthread_cpubind(hwloc_topology_t topology, hwloc_const_cpuset_t hwloc_set, int policy __hwloc_attribute_unused)
{
  if (topology->pid) {
    errno = ENOSYS;
    return -1;
  }
  return hwloc_linux_set_tid_cpubind(topology, 0, hwloc_set);
}

static int
hwloc_linux_get_thisthread_cpubind(hwloc_topology_t topology, hwloc_cpuset_t hwloc_set, int policy __hwloc_attribute_unused)
{
  if (topology->pid) {
    errno = ENOSYS;
    return -1;
  }
  return hwloc_linux_get_tid_cpubind(topology, 0, hwloc_set);
}

#if HAVE_DECL_PTHREAD_SETAFFINITY_NP
#pragma weak pthread_setaffinity_np

static int
hwloc_linux_set_thread_cpubind(hwloc_topology_t topology, pthread_t tid, hwloc_const_cpuset_t hwloc_set, int policy __hwloc_attribute_unused)
{
  int err;

  if (topology->pid) {
    errno = ENOSYS;
    return -1;
  }

  if (tid == pthread_self())
    return hwloc_linux_set_tid_cpubind(topology, 0, hwloc_set);

  if (!pthread_setaffinity_np) {
    /* ?! Application uses set_thread_cpubind, but doesn't link against libpthread ?! */
    errno = ENOSYS;
    return -1;
  }
  /* TODO Kerrighed: Use
   * int migrate (pid_t pid, int destination_node);
   * int migrate_self (int destination_node);
   * int thread_migrate (int thread_id, int destination_node);
   */

#ifdef HWLOC_HAVE_CPU_SET
  /* Use a separate block so that we can define specific variable
     types here */
  {
     cpu_set_t linux_set;
     unsigned cpu;

     CPU_ZERO(&linux_set);
     hwloc_cpuset_foreach_begin(cpu, hwloc_set)
         CPU_SET(cpu, &linux_set);
     hwloc_cpuset_foreach_end();

#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
     err = pthread_setaffinity_np(tid, &linux_set);
#else /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
     err = pthread_setaffinity_np(tid, sizeof(linux_set), &linux_set);
#endif /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
  }
#else /* CPU_SET */
  /* Use a separate block so that we can define specific variable
     types here */
  {
      unsigned long mask = hwloc_cpuset_to_ulong(hwloc_set);

#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
      err = pthread_setaffinity_np(tid, (void*) &mask);
#else /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
      err = pthread_setaffinity_np(tid, sizeof(mask), (void*) &mask);
#endif /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
  }
#endif /* CPU_SET */

  if (err) {
    errno = err;
    return -1;
  }
  return 0;
}
#endif /* HAVE_DECL_PTHREAD_SETAFFINITY_NP */

#if HAVE_DECL_PTHREAD_GETAFFINITY_NP
#pragma weak pthread_getaffinity_np

static int
hwloc_linux_get_thread_cpubind(hwloc_topology_t topology, pthread_t tid, hwloc_cpuset_t hwloc_set, int policy __hwloc_attribute_unused)
{
  int err;

  if (topology->pid) {
    errno = ENOSYS;
    return -1;
  }

  if (tid == pthread_self())
    return hwloc_linux_get_tid_cpubind(topology, 0, hwloc_set);

  if (!pthread_getaffinity_np) {
    /* ?! Application uses get_thread_cpubind, but doesn't link against libpthread ?! */
    errno = ENOSYS;
    return -1;
  }
  /* TODO Kerrighed */

#ifdef HWLOC_HAVE_CPU_SET
  /* Use a separate block so that we can define specific variable
     types here */
  {
     cpu_set_t linux_set;
     unsigned cpu;

#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
     err = pthread_getaffinity_np(tid, &linux_set);
#else /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
     err = pthread_getaffinity_np(tid, sizeof(linux_set), &linux_set);
#endif /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
     if (err) {
        errno = err;
        return -1;
     }

     hwloc_cpuset_zero(hwloc_set);
     for(cpu=0; cpu<CPU_SETSIZE; cpu++)
       if (CPU_ISSET(cpu, &linux_set))
	 hwloc_cpuset_set(hwloc_set, cpu);
  }
#else /* CPU_SET */
  /* Use a separate block so that we can define specific variable
     types here */
  {
      unsigned long mask;

#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
      err = pthread_getaffinity_np(tid, (void*) &mask);
#else /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
      err = pthread_getaffinity_np(tid, sizeof(mask), (void*) &mask);
#endif /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
      if (err) {
        errno = err;
        return -1;
      }

     hwloc_cpuset_from_ulong(hwloc_set, mask);
  }
#endif /* CPU_SET */

  return 0;
}
#endif /* HAVE_DECL_PTHREAD_GETAFFINITY_NP */

int
hwloc_backend_sysfs_init(struct hwloc_topology *topology, const char *fsroot_path __hwloc_attribute_unused)
{
#ifdef HAVE_OPENAT
  int root;

  assert(topology->backend_type == HWLOC_BACKEND_NONE);

  if (!fsroot_path)
    fsroot_path = "/";

  root = open(fsroot_path, O_RDONLY | O_DIRECTORY);
  if (root < 0)
    return -1;

  if (strcmp(fsroot_path, "/"))
    topology->is_thissystem = 0;

  topology->backend_params.sysfs.root_fd = root;
#else
  topology->backend_params.sysfs.root_fd = -1;
#endif
  topology->backend_type = HWLOC_BACKEND_SYSFS;
  return 0;
}

void
hwloc_backend_sysfs_exit(struct hwloc_topology *topology)
{
  assert(topology->backend_type == HWLOC_BACKEND_SYSFS);
#ifdef HAVE_OPENAT
  close(topology->backend_params.sysfs.root_fd);
#endif
  topology->backend_type = HWLOC_BACKEND_NONE;
}

static int
hwloc_parse_sysfs_unsigned(const char *mappath, unsigned *value, int fsroot_fd)
{
  char string[11];
  FILE * fd;

  fd = hwloc_fopen(mappath, "r", fsroot_fd);
  if (!fd) {
    *value = -1;
    return -1;
  }

  if (!fgets(string, 11, fd)) {
    *value = -1;
    return -1;
  }
  *value = strtoul(string, NULL, 10);

  fclose(fd);

  return 0;
}


/* kernel cpumaps are composed of an array of 32bits cpumasks */
#define KERNEL_CPU_MASK_BITS 32
#define KERNEL_CPU_MAP_LEN (KERNEL_CPU_MASK_BITS/4+2)
#define MAX_KERNEL_CPU_MASK ((HWLOC_NBMAXCPUS+KERNEL_CPU_MASK_BITS-1)/KERNEL_CPU_MASK_BITS)

int
hwloc_linux_parse_cpumap_file(FILE *file, hwloc_cpuset_t set)
{
  unsigned long maps[MAX_KERNEL_CPU_MASK];
  unsigned long map;
  int nr_maps = 0;
  int n;

  int i;

  /* reset to zero first */
  hwloc_cpuset_zero(set);

  /* parse the whole mask */
  while (fscanf(file, "%lx,", &map) == 1) /* read one kernel cpu mask and the ending comma */
    {
      if (nr_maps == MAX_KERNEL_CPU_MASK)
        break; /* too many cpumasks in this cpumap */

      if (!map && !nr_maps)
	/* ignore the first map if it's empty */
	continue;

      memmove(&maps[1], &maps[0], nr_maps*sizeof(*maps));
      maps[0] = map;
      nr_maps++;
    }

  /* check that the map can be stored in our cpuset */
  n = nr_maps*KERNEL_CPU_MASK_BITS;
  if (n > HWLOC_NBMAXCPUS)
    n = HWLOC_NBMAXCPUS;

  /* convert into a set */
  for(i=0; i<n; i++)
    if (maps[i/KERNEL_CPU_MASK_BITS] & 1<<(i%KERNEL_CPU_MASK_BITS))
      hwloc_cpuset_set(set, i);

  return 0;
}

static hwloc_cpuset_t
hwloc_parse_cpumap(const char *mappath, int fsroot_fd)
{
  hwloc_cpuset_t set;
  FILE * file;

  file = hwloc_fopen(mappath, "r", fsroot_fd);
  if (!file)
    return NULL;

  set = hwloc_cpuset_alloc();
  hwloc_linux_parse_cpumap_file(file, set);

  fclose(file);
  return set;
}

static char *
hwloc_strdup_mntpath(const char *escapedpath, size_t length)
{
  char *path = malloc(length+1);
  const char *src = escapedpath, *tmp = src;
  char *dst = path;

  while ((tmp = strchr(src, '\\')) != NULL) {
    strncpy(dst, src, tmp-src);
    dst += tmp-src;
    if (!strncmp(tmp+1, "040", 3))
      *dst = ' ';
    else if (!strncmp(tmp+1, "011", 3))
      *dst = '	';
    else if (!strncmp(tmp+1, "012", 3))
      *dst = '\n';
    else
      *dst = '\\';
    dst++;
    src = tmp+4;
  }

  strcpy(dst, src);

  return path;
}

static void
hwloc_find_linux_cpuset_mntpnt(char **cgroup_mntpnt, char **cpuset_mntpnt, int fsroot_fd)
{
#define PROC_MOUNT_LINE_LEN 128
  char line[PROC_MOUNT_LINE_LEN];
  FILE *fd;

  *cgroup_mntpnt = NULL;
  *cpuset_mntpnt = NULL;

  /* ideally we should use setmntent, getmntent, hasmntopt and endmntent,
   * but they do not support fsroot_fd.
   */

  fd = hwloc_fopen("/proc/mounts", "r", fsroot_fd);
  if (!fd)
    return;

  while (fgets(line, sizeof(line), fd)) {
    char *path;
    char *type;
    char *tmp;

    /* path is after first field and a space */
    tmp = strchr(line, ' ');
    if (!tmp)
      continue;
    path = tmp+1;

    /* type is after path, which may not contain spaces since the kernel escaped them to \040
     * (see the manpage of getmntent) */
    tmp = strchr(path, ' ');
    if (!tmp)
      continue;
    type = tmp+1;
    /* mark the end of path to ease upcoming strdup */
    *tmp = '\0';

    if (!strncmp(type, "cpuset ", 7)) {
      /* found a cpuset mntpnt */
      hwloc_debug("Found cpuset mount point on %s\n", path);
      *cpuset_mntpnt = hwloc_strdup_mntpath(path, type-path);
      break;
    } else if (!strncmp(type, "cgroup ", 7)) {
      /* found a cgroup mntpnt */
      char *opt, *opts;

      /* find options */
      tmp = strchr(type, ' ');
      if (!tmp)
	continue;
      opts = tmp+1;

      /* find "cpuset" option */
      while ((opt = strsep(&opts, ",")) && strcmp(opt, "cpuset"))
        ; /* continue */
      if (!opt)
	continue;

      hwloc_debug("Found cgroup/cpuset mount point on %s\n", path);
      *cgroup_mntpnt = hwloc_strdup_mntpath(path, type-path);
      break;
    }
  }

  fclose(fd);
}

/*
 * Linux cpusets may be managed directly or through cgroup.
 * If cgroup is used, tasks get a /proc/pid/cgroup which may contain a
 * single line %d:cpuset:<name>. If cpuset are used they get /proc/pid/cpuset
 * containing <name>.
 */
static char *
hwloc_read_linux_cpuset_name(int fsroot_fd, hwloc_pid_t pid)
{
#define CPUSET_NAME_LEN 128
  char cpuset_name[CPUSET_NAME_LEN];
  FILE *fd;
  char *tmp;

  /* check whether a cgroup-cpuset is enabled */
  if (!pid)
    fd = hwloc_fopen("/proc/self/cgroup", "r", fsroot_fd);
  else {
    char path[] = "/proc/XXXXXXXXXX/cgroup";
    snprintf(path, sizeof(path), "/proc/%d/cgroup", pid);
    fd = hwloc_fopen(path, "r", fsroot_fd);
  }
  if (fd) {
    /* find a cpuset line */
#define CGROUP_LINE_LEN 256
    char line[CGROUP_LINE_LEN];
    while (fgets(line, sizeof(line), fd)) {
      char *end, *colon = strchr(line, ':');
      if (!colon)
	continue;
      if (strncmp(colon, ":cpuset:", 8))
	continue;

      /* found a cgroup-cpuset line, return the name */
      fclose(fd);
      end = strchr(colon, '\n');
      if (end)
	*end = '\0';
      hwloc_debug("Found cgroup-cpuset %s\n", colon+8);
      return strdup(colon+8);
    }
    fclose(fd);
  }

  /* check whether a cpuset is enabled */
  if (!pid)
    fd = hwloc_fopen("/proc/self/cpuset", "r", fsroot_fd);
  else {
    char path[] = "/proc/XXXXXXXXXX/cpuset";
    snprintf(path, sizeof(path), "/proc/%d/cpuset", pid);
    fd = hwloc_fopen(path, "r", fsroot_fd);
  }
  if (!fd) {
    /* found nothing */
    hwloc_debug("%s", "No cgroup or cpuset found\n");
    return NULL;
  }

  /* found a cpuset, return the name */
  tmp = fgets(cpuset_name, sizeof(cpuset_name), fd);
  fclose(fd);
  if (!tmp)
    return NULL;
  tmp = strchr(cpuset_name, '\n');
  if (tmp)
    *tmp = '\0';
  hwloc_debug("Found cpuset %s\n", cpuset_name);
  return strdup(cpuset_name);
}

/*
 * Then, the cpuset description is available from either the cgroup or
 * the cpuset filesystem (usually mounted in / or /dev) where there
 * are cgroup<name>/cpuset.{cpus,mems} or cpuset<name>/{cpus,mems} files.
 */
static char *
hwloc_read_linux_cpuset_mask(const char *cgroup_mntpnt, const char *cpuset_mntpnt, const char *cpuset_name, const char *attr_name, int fsroot_fd)
{
#define CPUSET_FILENAME_LEN 256
  char cpuset_filename[CPUSET_FILENAME_LEN];
  FILE *fd;
  char *info = NULL, *tmp;
  ssize_t ssize;
  size_t size;

  if (cgroup_mntpnt) {
    /* try to read the cpuset from cgroup */
    snprintf(cpuset_filename, CPUSET_FILENAME_LEN, "%s%s/cpuset.%s", cgroup_mntpnt, cpuset_name, attr_name);
    hwloc_debug("Trying to read cgroup file <%s>\n", cpuset_filename);
    fd = hwloc_fopen(cpuset_filename, "r", fsroot_fd);
    if (fd)
      goto gotfile;
  } else if (cpuset_mntpnt) {
    /* try to read the cpuset directly */
    snprintf(cpuset_filename, CPUSET_FILENAME_LEN, "%s%s/%s", cpuset_mntpnt, cpuset_name, attr_name);
    hwloc_debug("Trying to read cpuset file <%s>\n", cpuset_filename);
    fd = hwloc_fopen(cpuset_filename, "r", fsroot_fd);
    if (fd)
      goto gotfile;
  }

  /* found no cpuset description, ignore it */
  hwloc_debug("Couldn't find cpuset <%s> description, ignoring\n", cpuset_name);
  goto out;

gotfile:
  ssize = getline(&info, &size, fd);
  fclose(fd);
  if (ssize < 0)
    goto out;
  if (!info)
    goto out;

  tmp = strchr(info, '\n');
  if (tmp)
    *tmp = '\0';

out:
  return info;
}

static void
hwloc_admin_disable_set_from_cpuset(struct hwloc_topology *topology,
				    const char *cgroup_mntpnt, const char *cpuset_mntpnt, const char *cpuset_name,
				    const char *attr_name,
				    hwloc_cpuset_t admin_enabled_cpus_set)
{
  char *cpuset_mask;
  char *current, *comma, *tmp;
  int prevlast, nextfirst, nextlast; /* beginning/end of enabled-segments */

  cpuset_mask = hwloc_read_linux_cpuset_mask(cgroup_mntpnt, cpuset_mntpnt, cpuset_name,
					     attr_name, topology->backend_params.sysfs.root_fd);
  if (!cpuset_mask)
    return;

  hwloc_debug("found cpuset %s: %s\n", attr_name, cpuset_mask);

  current = cpuset_mask;
  prevlast = -1;

  while (1) {
    /* save a pointer to the next comma and erase it to simplify things */
    comma = strchr(current, ',');
    if (comma)
      *comma = '\0';

    /* find current enabled-segment bounds */
    nextfirst = strtoul(current, &tmp, 0);
    if (*tmp == '-')
      nextlast = strtoul(tmp+1, NULL, 0);
    else
      nextlast = nextfirst;
    if (prevlast+1 <= nextfirst-1) {
      hwloc_debug("%s [%d:%d] excluded by cpuset\n", attr_name, prevlast+1, nextfirst-1);
      hwloc_cpuset_clr_range(admin_enabled_cpus_set, prevlast+1, nextfirst-1);
    }

    /* switch to next enabled-segment */
    prevlast = nextlast;
    if (!comma)
      break;
    current = comma+1;
  }

  /* disable after last enabled-segment */
  nextfirst = HWLOC_NBMAXCPUS;
  if (prevlast+1 <= nextfirst-1) {
    hwloc_debug("%s [%d:%d] excluded by cpuset\n", attr_name, prevlast+1, nextfirst-1);
    hwloc_cpuset_clr_range(admin_enabled_cpus_set, prevlast+1, nextfirst-1);
  }

  free(cpuset_mask);
}

static void
hwloc_get_procfs_meminfo_info(struct hwloc_topology *topology,
			     const char *path, struct hwloc_obj_memory_s *memory)
{
  char string[64];
  FILE *fd;

  if (topology->is_thissystem) {
    memory->page_types_len = 2;
    memory->page_types = malloc(2*sizeof(*memory->page_types));
    memset(memory->page_types, 0, 2*sizeof(*memory->page_types));

  /* Try to get the hugepage size from sysconf in case we fail to get it from /proc/meminfo later */
#ifdef HAVE__SC_LARGE_PAGESIZE
    memory->page_types[1].size = sysconf(_SC_LARGE_PAGESIZE);
#endif
    memory->page_types[0].size = getpagesize();
  }

  fd = hwloc_fopen(path, "r", topology->backend_params.sysfs.root_fd);
  if (!fd)
    return;

  while (fgets(string, sizeof(string), fd) && *string != '\0')
    {
      unsigned long long number;
      if (sscanf(string, "MemTotal: %llu kB", &number) == 1)
	memory->local_memory = number << 10;
      else if (memory->page_types && sscanf(string, "Hugepagesize: %llu", &number) == 1)
	memory->page_types[1].size = number << 10;
      else if (memory->page_types && sscanf(string, "HugePages_Free: %llu", &number) == 1)
	memory->page_types[1].count = number;
    }

  if (memory->page_types)
    memory->page_types[0].count = (memory->local_memory - memory->page_types[1].count*memory->page_types[1].size) / memory->page_types[0].size;

  fclose(fd);
}

#define SYSFS_NUMA_NODE_PATH_LEN 128

static void
hwloc_sysfs_node_meminfo_info(struct hwloc_topology *topology,
			     const char *syspath, int node,
			     struct hwloc_obj_memory_s *memory)
{
  char path[SYSFS_NUMA_NODE_PATH_LEN];
  char string[64];
  FILE *fd;

  sprintf(path, "%s/node%d/meminfo", syspath, node);
  fd = hwloc_fopen(path, "r", topology->backend_params.sysfs.root_fd);
  if (!fd)
    return;

  if (topology->is_thissystem) {
    memory->page_types_len = 2;
    memory->page_types = malloc(2*sizeof(*memory->page_types));
    memset(memory->page_types, 0, 2*sizeof(*memory->page_types));
  }

  while (fgets(string, sizeof(string), fd) && *string != '\0')
    {
      unsigned long long number;
      if (sscanf(string, "Node %d MemTotal: %llu kB", &node, &number) == 2)
	memory->local_memory = number << 10;
      else if (memory->page_types && sscanf(string, "Node %d HugePages_Free: %llu kB", &node, &number) == 2)
	memory->page_types[1].count = number;
    }

  if (memory->page_types) {
    /* hwloc_get_procfs_meminfo_info must have been called earlier */
    memory->page_types[1].size = topology->levels[0][0]->memory.page_types[1].size;
    memory->page_types[0].size = getpagesize();
    memory->page_types[0].count = (memory->local_memory - memory->page_types[1].count*memory->page_types[1].size) / memory->page_types[0].size;
  }

  fclose(fd);
}

static void
hwloc_parse_node_distance(const char *distancepath, unsigned nbnodes, unsigned *distances, int fsroot_fd)
{
  char string[4096]; /* enough for hundreds of nodes */
  char *tmp, *next;
  FILE * fd;

  fd = hwloc_fopen(distancepath, "r", fsroot_fd);
  if (!fd)
    return;

  if (!fgets(string, sizeof(string), fd))
    return;

  tmp = string;
  while (tmp) {
    unsigned distance = strtoul(tmp, &next, 0);
    if (next == tmp)
      break;
    *distances = distance;
    distances++;
    nbnodes--;
    if (!nbnodes)
      break;
    tmp = next+1;
  }

  fclose(fd);
}

static void
look_sysfsnode(struct hwloc_topology *topology, const char *path, unsigned *found)
{
  unsigned osnode;
  unsigned nbnodes = 1;
  DIR *dir;
  struct dirent *dirent;
  hwloc_obj_t node;

  *found = 0;

  dir = hwloc_opendir(path, topology->backend_params.sysfs.root_fd);
  if (dir)
    {
      while ((dirent = readdir(dir)) != NULL)
	{
	  unsigned long numnode;
	  if (strncmp(dirent->d_name, "node", 4))
	    continue;
	  numnode = strtoul(dirent->d_name+4, NULL, 0);
	  if (nbnodes < numnode+1)
	    nbnodes = numnode+1;
	}
      closedir(dir);
    }

  if (nbnodes <= 1)
    return;

  /* For convenience, put these declarations inside a block.  Saves us
     from a bunch of mallocs, particularly with the 2D array. */
  {
      hwloc_obj_t nodes[nbnodes];
      unsigned distances[nbnodes][nbnodes];
      for (osnode=0; osnode < nbnodes; osnode++) {
          char nodepath[SYSFS_NUMA_NODE_PATH_LEN];
          hwloc_cpuset_t cpuset;

          sprintf(nodepath, "%s/node%u/cpumap", path, osnode);
          cpuset = hwloc_parse_cpumap(nodepath, topology->backend_params.sysfs.root_fd);
          if (!cpuset)
              continue;

          node = hwloc_alloc_setup_object(HWLOC_OBJ_NODE, osnode);
          node->cpuset = cpuset;
          node->nodeset = hwloc_cpuset_alloc();
          hwloc_cpuset_set(node->nodeset, osnode);

          hwloc_sysfs_node_meminfo_info(topology, path, osnode, &node->memory);

          hwloc_debug_1arg_cpuset("os node %u has cpuset %s\n",
                                  osnode, node->cpuset);
          hwloc_insert_object_by_cpuset(topology, node);
          nodes[osnode] = node;

          sprintf(nodepath, "%s/node%u/distance", path, osnode);
          hwloc_parse_node_distance(nodepath, nbnodes, distances[osnode], topology->backend_params.sysfs.root_fd);
      }

      hwloc_setup_misc_level_from_distances(topology, nbnodes, nodes, (unsigned*) distances);
  }

  *found = nbnodes;
}

/* Look at Linux' /sys/devices/system/cpu/cpu%d/topology/ */
static void
look_sysfscpu(struct hwloc_topology *topology, const char *path)
{
  hwloc_cpuset_t cpuset; /* Set of cpus for which we have topology information */
#define CPU_TOPOLOGY_STR_LEN 128
  char str[CPU_TOPOLOGY_STR_LEN];
  DIR *dir;
  int i,j;
  FILE *fd;

  cpuset = hwloc_cpuset_alloc();

  /* fill the cpuset of interesting cpus */
  dir = hwloc_opendir(path, topology->backend_params.sysfs.root_fd);
  if (dir) {
    struct dirent *dirent;
    while ((dirent = readdir(dir)) != NULL) {
      unsigned long cpu;
      char online[2];

      if (strncmp(dirent->d_name, "cpu", 3))
	continue;
      cpu = strtoul(dirent->d_name+3, NULL, 0);

      if (cpu >= HWLOC_NBMAXCPUS)
        continue;

      /* Maybe we don't have topology information but at least it exists */
      hwloc_cpuset_set(topology->levels[0][0]->complete_cpuset, cpu);

      /* check whether this processor is online */
      sprintf(str, "%s/cpu%lu/online", path, cpu);
      fd = hwloc_fopen(str, "r", topology->backend_params.sysfs.root_fd);
      if (fd) {
	if (fgets(online, sizeof(online), fd)) {
	  fclose(fd);
	  if (atoi(online)) {
	    hwloc_debug("os proc %lu is online\n", cpu);
	  } else {
	    hwloc_debug("os proc %lu is offline\n", cpu);
            hwloc_cpuset_clr(topology->levels[0][0]->online_cpuset, cpu);
	  }
	} else {
	  fclose(fd);
	}
      }

      /* check whether the kernel exports topology information for this cpu */
      sprintf(str, "%s/cpu%lu/topology", path, cpu);
      if (hwloc_access(str, X_OK, topology->backend_params.sysfs.root_fd) < 0 && errno == ENOENT) {
	hwloc_debug("os proc %lu has no accessible %s/cpu%lu/topology\n",
		   cpu, path, cpu);
	continue;
      }

      hwloc_cpuset_set(cpuset, cpu);
    }
    closedir(dir);
  }

  topology->support.discovery->pu = 1;
  hwloc_debug_1arg_cpuset("found %d cpu topologies, cpuset %s\n",
	     hwloc_cpuset_weight(cpuset), cpuset);

  hwloc_cpuset_foreach_begin(i, cpuset)
    {
      struct hwloc_obj *socket, *core, *thread;
      hwloc_cpuset_t socketset, coreset, threadset;
      unsigned mysocketid, mycoreid;

      /* look at the socket */
      mysocketid = 0; /* shut-up the compiler */
      sprintf(str, "%s/cpu%d/topology/physical_package_id", path, i);
      hwloc_parse_sysfs_unsigned(str, &mysocketid, topology->backend_params.sysfs.root_fd);

      sprintf(str, "%s/cpu%d/topology/core_siblings", path, i);
      socketset = hwloc_parse_cpumap(str, topology->backend_params.sysfs.root_fd);
      if (socketset && hwloc_cpuset_weight(socketset) >= 1) {
        if (hwloc_cpuset_first(socketset) == i) {
          /* first cpu in this socket, add the socket */
          socket = hwloc_alloc_setup_object(HWLOC_OBJ_SOCKET, mysocketid);
          socket->cpuset = socketset;
          hwloc_debug_1arg_cpuset("os socket %u has cpuset %s\n",
                     mysocketid, socketset);
          hwloc_insert_object_by_cpuset(topology, socket);
        } else
          hwloc_cpuset_free(socketset);
      }

      /* look at the core */
      mycoreid = 0; /* shut-up the compiler */
      sprintf(str, "%s/cpu%d/topology/core_id", path, i);
      hwloc_parse_sysfs_unsigned(str, &mycoreid, topology->backend_params.sysfs.root_fd);

      sprintf(str, "%s/cpu%d/topology/thread_siblings", path, i);
      coreset = hwloc_parse_cpumap(str, topology->backend_params.sysfs.root_fd);
      if (coreset && hwloc_cpuset_weight(coreset) >= 1) {
        if (hwloc_cpuset_first(coreset) == i) {
          core = hwloc_alloc_setup_object(HWLOC_OBJ_CORE, mycoreid);
          core->cpuset = coreset;
          hwloc_debug_1arg_cpuset("os core %u has cpuset %s\n",
                     mycoreid, coreset);
          hwloc_insert_object_by_cpuset(topology, core);
        } else
          hwloc_cpuset_free(coreset);
      }

      /* look at the thread */
      threadset = hwloc_cpuset_alloc();
      hwloc_cpuset_cpu(threadset, i);

      /* add the thread */
      thread = hwloc_alloc_setup_object(HWLOC_OBJ_PU, i);
      thread->cpuset = threadset;
      hwloc_debug_1arg_cpuset("thread %d has cpuset %s\n",
		 i, threadset);
      hwloc_insert_object_by_cpuset(topology, thread);

      /* look at the caches */
      for(j=0; j<10; j++) {
#define SHARED_CPU_MAP_STRLEN 128
	char mappath[SHARED_CPU_MAP_STRLEN];
	char str2[20]; /* enough for a level number (one digit) or a type (Data/Instruction/Unified) */
	struct hwloc_obj *cache;
	hwloc_cpuset_t cacheset;
	unsigned long kB = 0;
	int depth; /* 0 for L1, .... */

	/* get the cache level depth */
	sprintf(mappath, "%s/cpu%d/cache/index%d/level", path, i, j);
	fd = hwloc_fopen(mappath, "r", topology->backend_params.sysfs.root_fd);
	if (fd) {
	  if (fgets(str2,sizeof(str2), fd))
	    depth = strtoul(str2, NULL, 10)-1;
	  else
	    continue;
	  fclose(fd);
	} else
	  continue;

	/* ignore Instruction caches */
	sprintf(mappath, "%s/cpu%d/cache/index%d/type", path, i, j);
	fd = hwloc_fopen(mappath, "r", topology->backend_params.sysfs.root_fd);
	if (fd) {
	  if (fgets(str2, sizeof(str2), fd)) {
	    fclose(fd);
	    if (!strncmp(str2, "Instruction", 11))
	      continue;
	  } else {
	    fclose(fd);
	    continue;
	  }
	} else
	  continue;

	/* get the cache size */
	sprintf(mappath, "%s/cpu%d/cache/index%d/size", path, i, j);
	fd = hwloc_fopen(mappath, "r", topology->backend_params.sysfs.root_fd);
	if (fd) {
	  if (fgets(str2,sizeof(str2), fd))
	    kB = atol(str2); /* in kB */
	  fclose(fd);
	}

	sprintf(mappath, "%s/cpu%d/cache/index%d/shared_cpu_map", path, i, j);
	cacheset = hwloc_parse_cpumap(mappath, topology->backend_params.sysfs.root_fd);
        if (cacheset) {
          if (hwloc_cpuset_weight(cacheset) < 1)
            /* mask is wrong (happens on ia64), assumes it's not shared */
            hwloc_cpuset_cpu(cacheset, i);

          if (hwloc_cpuset_first(cacheset) == i) {
            /* first cpu in this cache, add the cache */
            cache = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, -1);
            cache->attr->cache.size = kB << 10;
            cache->attr->cache.depth = depth+1;
            cache->cpuset = cacheset;
            hwloc_debug_1arg_cpuset("cache depth %d has cpuset %s\n",
                       depth, cacheset);
            hwloc_insert_object_by_cpuset(topology, cache);
          } else
            hwloc_cpuset_free(cacheset);
        }
      }
    }
  hwloc_cpuset_foreach_end();

  hwloc_cpuset_free(cpuset);
}


/* Look at Linux' /proc/cpuinfo */
#      define PROCESSOR	"processor"
#      define PHYSID "physical id"
#      define COREID "core id"
static int
look_cpuinfo(struct hwloc_topology *topology, const char *path,
	     hwloc_cpuset_t online_cpuset)
{
  FILE *fd;
  char str[strlen(PHYSID)+1+9+1+1];
  char *endptr;
  unsigned proc_physids[HWLOC_NBMAXCPUS];
  unsigned osphysids[HWLOC_NBMAXCPUS];
  unsigned proc_coreids[HWLOC_NBMAXCPUS];
  unsigned oscoreids[HWLOC_NBMAXCPUS];
  unsigned proc_osphysids[HWLOC_NBMAXCPUS];
  unsigned core_osphysids[HWLOC_NBMAXCPUS];
  unsigned procid_max=0;
  unsigned numprocs=0;
  unsigned numsockets=0;
  unsigned numcores=0;
  unsigned long physid;
  unsigned long coreid;
  unsigned long processor = (unsigned long) -1;
  unsigned i;
  hwloc_cpuset_t cpuset;
  hwloc_obj_t obj;

  for (i = 0; i < HWLOC_NBMAXCPUS; i++) {
    proc_physids[i] = -1;
    osphysids[i] = -1;
    proc_coreids[i] = -1;
    oscoreids[i] = -1;
    proc_osphysids[i] = -1;
    core_osphysids[i] = -1;
  }

  if (!(fd=hwloc_fopen(path,"r", topology->backend_params.sysfs.root_fd)))
    {
      hwloc_debug("%s", "could not open /proc/cpuinfo\n");
      return -1;
    }

  cpuset = hwloc_cpuset_alloc();
  /* Just record information and count number of sockets and cores */

  hwloc_debug("%s", "\n\n * Topology extraction from /proc/cpuinfo *\n\n");
  while (fgets(str,sizeof(str),fd)!=NULL)
    {
#      define getprocnb_begin(field, var)		     \
      if ( !strncmp(field,str,strlen(field)))	     \
	{						     \
	char *c = strchr(str, ':')+1;		     \
	var = strtoul(c,&endptr,0);			     \
	if (endptr==c)							\
	  {								\
            hwloc_debug("%s", "no number in "field" field of /proc/cpuinfo\n"); \
            hwloc_cpuset_free(cpuset);					\
            return -1;							\
	  }								\
	else if (var==ULONG_MAX)						\
	  {								\
            hwloc_debug("%s", "too big "field" number in /proc/cpuinfo\n"); \
            hwloc_cpuset_free(cpuset);					\
            return -1;							\
	  }								\
	hwloc_debug(field " %lu\n", var)
#      define getprocnb_end()			\
      }
      getprocnb_begin(PROCESSOR,processor);
      hwloc_cpuset_set(cpuset, processor);

      obj = hwloc_alloc_setup_object(HWLOC_OBJ_PU, processor);
      obj->cpuset = hwloc_cpuset_alloc();
      hwloc_cpuset_cpu(obj->cpuset, processor);

      hwloc_debug_2args_cpuset("cpu %u (os %lu) has cpuset %s\n",
		 numprocs, processor, obj->cpuset);
      numprocs++;
      hwloc_insert_object_by_cpuset(topology, obj);

      getprocnb_end() else
      getprocnb_begin(PHYSID,physid);
      proc_osphysids[processor]=physid;
      for (i=0; i<numsockets; i++)
	if (physid == osphysids[i])
	  break;
      proc_physids[processor]=i;
      hwloc_debug("%lu on socket %u (%lx)\n", processor, i, physid);
      if (i==numsockets)
	osphysids[(numsockets)++] = physid;
      getprocnb_end() else
      getprocnb_begin(COREID,coreid);
      for (i=0; i<numcores; i++)
	if (coreid == oscoreids[i] && proc_osphysids[processor] == core_osphysids[i])
	  break;
      proc_coreids[processor]=i;
      if (i==numcores)
	{
	  core_osphysids[numcores] = proc_osphysids[processor];
	  oscoreids[numcores] = coreid;
	  (numcores)++;
	}
      getprocnb_end()
	if (str[strlen(str)-1]!='\n')
	  {
            /* ignore end of line */
	    if (fscanf(fd,"%*[^\n]") == EOF)
	      break;
	    getc(fd);
	  }
    }
  fclose(fd);

  if (processor == (unsigned long) -1) {
    hwloc_cpuset_free(cpuset);
    return -1;
  }

  topology->support.discovery->pu = 1;
  /* setup the final number of procs */
  procid_max = processor + 1;
  hwloc_cpuset_copy(online_cpuset, cpuset);
  hwloc_cpuset_free(cpuset);

  hwloc_debug("%u online processors found, with id max %u\n", numprocs, procid_max);
  hwloc_debug_cpuset("online processor cpuset: %s\n", online_cpuset);

  hwloc_debug("%s", "\n * Topology summary *\n");
  hwloc_debug("%u processors (%u max id)\n", numprocs, procid_max);

  hwloc_debug("%u sockets\n", numsockets);
  if (numsockets>0)
    hwloc_setup_level(procid_max, numsockets, osphysids, proc_physids, topology, HWLOC_OBJ_SOCKET);

  hwloc_debug("%u cores\n", numcores);
  if (numcores>0)
    hwloc_setup_level(procid_max, numcores, oscoreids, proc_coreids, topology, HWLOC_OBJ_CORE);

  return 0;
}

static void
hwloc__get_dmi_info(struct hwloc_topology *topology,
		   char **dmi_board_vendor, char **dmi_board_name)
{
#define DMI_BOARD_STRINGS_LEN 50
  char dmi_line[DMI_BOARD_STRINGS_LEN];
  char *tmp;
  FILE *fd;

  dmi_line[0] = '\0';
  fd = hwloc_fopen("/sys/class/dmi/id/board_vendor", "r", topology->backend_params.sysfs.root_fd);
  if (fd) {
    tmp = fgets(dmi_line, DMI_BOARD_STRINGS_LEN, fd);
    fclose (fd);
    if (tmp && dmi_line[0] != '\0') {
      tmp = strchr(dmi_line, '\n');
      if (tmp)
	*tmp = '\0';
      *dmi_board_vendor = strdup(dmi_line);
      hwloc_debug("found DMI board vendor '%s'\n", *dmi_board_vendor);
    }
  }

  dmi_line[0] = '\0';
  fd = hwloc_fopen("/sys/class/dmi/id/board_name", "r", topology->backend_params.sysfs.root_fd);
  if (fd) {
    tmp = fgets(dmi_line, DMI_BOARD_STRINGS_LEN, fd);
    fclose (fd);
    if (tmp && dmi_line[0] != '\0') {
      tmp = strchr(dmi_line, '\n');
      if (tmp)
	*tmp = '\0';
      *dmi_board_name = strdup(dmi_line);
      hwloc_debug("found DMI board name '%s'\n", *dmi_board_name);
    }
  }
}

void
hwloc_look_linux(struct hwloc_topology *topology)
{
  DIR *nodes_dir;
  unsigned nbnodes;
  char *cpuset_mntpnt, *cgroup_mntpnt, *cpuset_name;
  int err;

  /* Gather the list of admin-disabled cpus and mems */
  hwloc_find_linux_cpuset_mntpnt(&cgroup_mntpnt, &cpuset_mntpnt, topology->backend_params.sysfs.root_fd);
  if (cgroup_mntpnt || cpuset_mntpnt) {
    cpuset_name = hwloc_read_linux_cpuset_name(topology->backend_params.sysfs.root_fd, topology->pid);
    if (cpuset_name) {
      hwloc_admin_disable_set_from_cpuset(topology, cgroup_mntpnt, cpuset_mntpnt, cpuset_name, "cpus", topology->levels[0][0]->allowed_cpuset);
      hwloc_admin_disable_set_from_cpuset(topology, cgroup_mntpnt, cpuset_mntpnt, cpuset_name, "mems", topology->levels[0][0]->allowed_nodeset);
      free(cpuset_name);
    }
    free(cgroup_mntpnt);
    free(cpuset_mntpnt);
  }

  nodes_dir = hwloc_opendir("/proc/nodes", topology->backend_params.sysfs.root_fd);
  if (nodes_dir) {
    /* Kerrighed */
    struct dirent *dirent;
    char path[128];
    hwloc_obj_t machine;
    hwloc_cpuset_t machine_online_set;

    /* replace top-level object type with SYSTEM and add some MACHINE underneath */

    topology->levels[0][0]->type = HWLOC_OBJ_SYSTEM;
    topology->levels[0][0]->name = strdup("Kerrighed");

    /* No cpuset support for now.  */
    /* No sys support for now.  */
    while ((dirent = readdir(nodes_dir)) != NULL) {
      unsigned long node;
      if (strncmp(dirent->d_name, "node", 4))
	continue;
      machine_online_set = hwloc_cpuset_alloc();
      node = strtoul(dirent->d_name+4, NULL, 0);
      snprintf(path, sizeof(path), "/proc/nodes/node%lu/cpuinfo", node);
      err = look_cpuinfo(topology, path, machine_online_set);
      if (err < 0)
        continue;
      hwloc_cpuset_or(topology->levels[0][0]->online_cpuset, topology->levels[0][0]->online_cpuset, machine_online_set);
      machine = hwloc_alloc_setup_object(HWLOC_OBJ_MACHINE, node);
      machine->cpuset = machine_online_set;
      machine->attr->machine.dmi_board_name = NULL;
      machine->attr->machine.dmi_board_vendor = NULL;
      hwloc_debug_1arg_cpuset("machine number %lu has cpuset %s\n",
		 node, machine_online_set);
      hwloc_insert_object_by_cpuset(topology, machine);

      snprintf(path, sizeof(path), "/proc/nodes/node%lu/meminfo", node);
      /* Get the machine memory attributes */
      hwloc_get_procfs_meminfo_info(topology, path, &machine->memory);

      /* Gather DMI info */
      /* FIXME: get the right DMI info of each machine */
      hwloc__get_dmi_info(topology,
			 &machine->attr->machine.dmi_board_vendor,
			 &machine->attr->machine.dmi_board_name);
    }
    closedir(nodes_dir);
  } else {
    /* Get the machine memory attributes */
    hwloc_get_procfs_meminfo_info(topology, "/proc/meminfo", &topology->levels[0][0]->memory);

    /* Gather NUMA information. Must be after hwloc_get_procfs_meminfo_info so that the hugepage size is known */
    look_sysfsnode(topology, "/sys/devices/system/node", &nbnodes);

    /* if we found some numa nodes, the machine object has no local memory */
    if (nbnodes) {
      topology->levels[0][0]->memory.local_memory = 0;
      if (topology->levels[0][0]->memory.page_types) {
        topology->levels[0][0]->memory.page_types[0].count = 0;
        topology->levels[0][0]->memory.page_types[1].count = 0;
      }
    }

    /* Gather the list of cpus now */
    if (getenv("HWLOC_LINUX_USE_CPUINFO")
	|| hwloc_access("/sys/devices/system/cpu/cpu0/topology", R_OK, topology->backend_params.sysfs.root_fd) < 0) {
	/* revert to reading cpuinfo only if /sys/.../topology unavailable (before 2.6.16) */
      err = look_cpuinfo(topology, "/proc/cpuinfo", topology->levels[0][0]->online_cpuset);
      if (err < 0) {
        if (topology->is_thissystem)
          hwloc_setup_pu_level(topology, hwloc_fallback_nbprocessors(topology));
        else
          /* fsys-root but not this system, no way, assume there's just 1
           * processor :/ */
          hwloc_setup_pu_level(topology, 1);
      }
    } else {
      look_sysfscpu(topology, "/sys/devices/system/cpu");
    }

    /* Gather DMI info */
    hwloc__get_dmi_info(topology,
		       &topology->levels[0][0]->attr->machine.dmi_board_vendor,
		       &topology->levels[0][0]->attr->machine.dmi_board_name);
  }
}

void
hwloc_set_linux_hooks(struct hwloc_topology *topology)
{
  topology->set_thisthread_cpubind = hwloc_linux_set_thisthread_cpubind;
  topology->get_thisthread_cpubind = hwloc_linux_get_thisthread_cpubind;
  topology->set_thisproc_cpubind = hwloc_linux_set_thisproc_cpubind;
  topology->get_thisproc_cpubind = hwloc_linux_get_thisproc_cpubind;
  topology->set_proc_cpubind = hwloc_linux_set_proc_cpubind;
  topology->get_proc_cpubind = hwloc_linux_get_proc_cpubind;
#if HAVE_DECL_PTHREAD_SETAFFINITY_NP
  topology->set_thread_cpubind = hwloc_linux_set_thread_cpubind;
#endif /* HAVE_DECL_PTHREAD_SETAFFINITY_NP */
#if HAVE_DECL_PTHREAD_GETAFFINITY_NP
  topology->get_thread_cpubind = hwloc_linux_get_thread_cpubind;
#endif /* HAVE_DECL_PTHREAD_GETAFFINITY_NP */
}

/* TODO mbind, setpolicy */
