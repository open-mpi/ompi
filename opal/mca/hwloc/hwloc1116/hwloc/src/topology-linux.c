/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009-2013, 2015 Université Bordeaux
 * Copyright © 2009-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright © 2015 Intel, Inc.  All rights reserved.
 * Copyright © 2010 IBM
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <hwloc/linux.h>
#include <private/misc.h>
#include <private/private.h>
#include <private/misc.h>
#include <private/debug.h>

#include <limits.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <assert.h>
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HWLOC_HAVE_LIBUDEV
#include <libudev.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <sched.h>
#include <pthread.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <mntent.h>
#if defined HWLOC_HAVE_SET_MEMPOLICY || defined HWLOC_HAVE_MBIND || defined HWLOC_HAVE_MOVE_PAGES
#define migratepages migrate_pages /* workaround broken migratepages prototype in numaif.h before libnuma 2.0.2 */
#include <numaif.h>
#endif

struct hwloc_linux_backend_data_s {
  char *root_path; /* NULL if unused */
  int root_fd; /* The file descriptor for the file system root, used when browsing, e.g., Linux' sysfs and procfs. */
  int is_real_fsroot; /* Boolean saying whether root_fd points to the real filesystem root of the system */
#ifdef HWLOC_HAVE_LIBUDEV
  struct udev *udev; /* Global udev context */
#endif
  char *dumped_hwdata_dirname;
  enum {
    HWLOC_LINUX_ARCH_X86, /* x86 32 or 64bits, including k1om (KNC) */
    HWLOC_LINUX_ARCH_IA64,
    HWLOC_LINUX_ARCH_ARM,
    HWLOC_LINUX_ARCH_POWER,
    HWLOC_LINUX_ARCH_UNKNOWN
  } arch;
  int is_knl;
  int is_amd_with_CU;
  struct utsname utsname; /* fields contain \0 when unknown */
  unsigned fallback_nbprocessors;
  unsigned pagesize;

  int deprecated_classlinks_model; /* -2 if never tried, -1 if unknown, 0 if new (device contains class/name), 1 if old (device contains class:name) */
  int mic_need_directlookup; /* if not tried yet, 0 if not needed, 1 if needed */
  unsigned mic_directlookup_id_max; /* -1 if not tried yet, 0 if none to lookup, maxid+1 otherwise */
};



/***************************
 * Misc Abstraction layers *
 ***************************/

#if !(defined HWLOC_HAVE_SCHED_SETAFFINITY) && (defined HWLOC_HAVE_SYSCALL)
/* libc doesn't have support for sched_setaffinity, make system call
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
#      define sched_setaffinity(pid, lg, mask) syscall(__NR_sched_setaffinity, pid, lg, mask)
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
#      define sched_getaffinity(pid, lg, mask) (syscall(__NR_sched_getaffinity, pid, lg, mask) < 0 ? -1 : 0)
#    endif
#endif

/* Added for ntohl() */
#include <arpa/inet.h>

#ifdef HAVE_OPENAT
/* Use our own filesystem functions if we have openat */

static const char *
hwloc_checkat(const char *path, int fsroot_fd)
{
  const char *relative_path;
  if (fsroot_fd < 0) {
    errno = EBADF;
    return NULL;
  }

  /* Skip leading slashes.  */
  for (relative_path = path; *relative_path == '/'; relative_path++);

  return relative_path;
}

static int
hwloc_openat(const char *path, int fsroot_fd)
{
  const char *relative_path;

  relative_path = hwloc_checkat(path, fsroot_fd);
  if (!relative_path)
    return -1;

  return openat (fsroot_fd, relative_path, O_RDONLY);
}

static FILE *
hwloc_fopenat(const char *path, const char *mode, int fsroot_fd)
{
  int fd;

  if (strcmp(mode, "r")) {
    errno = ENOTSUP;
    return NULL;
  }

  fd = hwloc_openat (path, fsroot_fd);
  if (fd == -1)
    return NULL;

  return fdopen(fd, mode);
}

static int
hwloc_accessat(const char *path, int mode, int fsroot_fd)
{
  const char *relative_path;

  relative_path = hwloc_checkat(path, fsroot_fd);
  if (!relative_path)
    return -1;

  return faccessat(fsroot_fd, relative_path, mode, 0);
}

static int
hwloc_fstatat(const char *path, struct stat *st, int flags, int fsroot_fd)
{
  const char *relative_path;

  relative_path = hwloc_checkat(path, fsroot_fd);
  if (!relative_path)
    return -1;

  return fstatat(fsroot_fd, relative_path, st, flags);
}

static DIR*
hwloc_opendirat(const char *path, int fsroot_fd)
{
  int dir_fd;
  const char *relative_path;

  relative_path = hwloc_checkat(path, fsroot_fd);
  if (!relative_path)
    return NULL;

  dir_fd = openat(fsroot_fd, relative_path, O_RDONLY | O_DIRECTORY);
  if (dir_fd < 0)
    return NULL;

  return fdopendir(dir_fd);
}

#endif /* HAVE_OPENAT */

/* Static inline version of fopen so that we can use openat if we have
   it, but still preserve compiler parameter checking */
static __hwloc_inline int
hwloc_open(const char *p, int d __hwloc_attribute_unused)
{
#ifdef HAVE_OPENAT
    return hwloc_openat(p, d);
#else
    return open(p, O_RDONLY);
#endif
}

static __hwloc_inline FILE *
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
static __hwloc_inline int
hwloc_access(const char *p, int m, int d __hwloc_attribute_unused)
{
#ifdef HAVE_OPENAT
    return hwloc_accessat(p, m, d);
#else
    return access(p, m);
#endif
}

static __hwloc_inline int
hwloc_stat(const char *p, struct stat *st, int d __hwloc_attribute_unused)
{
#ifdef HAVE_OPENAT
    return hwloc_fstatat(p, st, 0, d);
#else
    return stat(p, st);
#endif
}

static __hwloc_inline int
hwloc_lstat(const char *p, struct stat *st, int d __hwloc_attribute_unused)
{
#ifdef HAVE_OPENAT
    return hwloc_fstatat(p, st, AT_SYMLINK_NOFOLLOW, d);
#else
    return lstat(p, st);
#endif
}

/* Static inline version of opendir so that we can use openat if we have
   it, but still preserve compiler parameter checking */
static __hwloc_inline DIR *
hwloc_opendir(const char *p, int d __hwloc_attribute_unused)
{
#ifdef HAVE_OPENAT
    return hwloc_opendirat(p, d);
#else
    return opendir(p);
#endif
}


/*****************************************
 ******* Helpers for reading files *******
 *****************************************/

static __hwloc_inline int
hwloc_read_path_by_length(const char *path, char *string, size_t length, int fsroot_fd)
{
  int fd, ret;

  fd = hwloc_open(path, fsroot_fd);
  if (fd < 0)
    return -1;

  ret = read(fd, string, length-1); /* read -1 to put the ending \0 */
  close(fd);

  if (ret <= 0)
    return -1;

  string[ret] = 0;

  return 0;
}

static __hwloc_inline int
hwloc_read_path_as_int(const char *path, int *value, int fsroot_fd)
{
  char string[11];
  if (hwloc_read_path_by_length(path, string, sizeof(string), fsroot_fd) < 0)
    return -1;
  *value = atoi(string);
  return 0;
}

static __hwloc_inline int
hwloc_read_path_as_uint(const char *path, unsigned *value, int fsroot_fd)
{
  char string[11];
  if (hwloc_read_path_by_length(path, string, sizeof(string), fsroot_fd) < 0)
    return -1;
  *value = (unsigned) strtoul(string, NULL, 10);
  return 0;
}

/* Read everything from fd and save it into a newly allocated buffer
 * returned in bufferp. Use sizep as a default buffer size, and returned
 * the actually needed size in sizep.
 */
static __hwloc_inline int
hwloc__read_fd(int fd, char **bufferp, size_t *sizep)
{
  char *buffer;
  size_t toread, filesize, totalread;
  ssize_t ret;

  toread = filesize = *sizep;

  /* Alloc and read +1 so that we get EOF on 2^n without reading once more */
  buffer = malloc(filesize+1);
  if (!buffer)
    return -1;

  ret = read(fd, buffer, toread+1);
  if (ret < 0) {
    free(buffer);
    return -1;
  }

  totalread = (size_t) ret;

  if (totalread < toread + 1)
    /* Normal case, a single read got EOF */
    goto done;

  /* Unexpected case, must extend the buffer and read again.
   * Only occurs on first invocation and if the kernel ever uses multiple page for a single mask.
   */
  do {
    char *tmp;

    toread = filesize;
    filesize *= 2;

    tmp = realloc(buffer, filesize+1);
    if (!tmp) {
      free(buffer);
      return -1;
    }
    buffer = tmp;

    ret = read(fd, buffer+toread+1, toread);
    if (ret < 0) {
      free(buffer);
      return -1;
    }

    totalread += ret;
  } while ((size_t) ret == toread);

 done:
  buffer[totalread] = '\0';
  *bufferp = buffer;
  *sizep = filesize;
  return 0;
}

/* kernel cpumaps are composed of an array of 32bits cpumasks */
#define KERNEL_CPU_MASK_BITS 32
#define KERNEL_CPU_MAP_LEN (KERNEL_CPU_MASK_BITS/4+2)

static __hwloc_inline int
hwloc__read_fd_as_cpumask(int fd, hwloc_bitmap_t set)
{
  static size_t _filesize = 0; /* will be dynamically initialized to hwloc_get_pagesize(), and increased later if needed */
  size_t filesize;
  unsigned long *maps;
  unsigned long map;
  int nr_maps = 0;
  static int _nr_maps_allocated = 8; /* Only compute the power-of-two above the kernel cpumask size once.
				      * Actually, it may increase multiple times if first read cpumaps start with zeroes.
				      */
  int nr_maps_allocated = _nr_maps_allocated;
  char *buffer, *tmpbuf;
  int i;

  /* Kernel sysfs files are usually at most one page. 4kB may contain 455 32-bit
   * masks (followed by comma), enough for 14k PUs. So allocate a page by default for now.
   *
   * If we ever need a larger buffer, we'll realloc() the buffer during the first
   * invocation of this function so that others directly allocate the right size
   * (all cpumask files have the exact same size).
   */
  filesize = _filesize;
  if (!filesize)
    filesize = hwloc_getpagesize();
  if (hwloc__read_fd(fd, &buffer, &filesize) < 0)
    return -1;
  /* Only update the static value with the final one,
   * to avoid sharing intermediate values that we modify,
   * in case there's ever multiple concurrent calls.
   */
  _filesize = filesize;

  maps = malloc(nr_maps_allocated * sizeof(*maps));
  if (!maps) {
    free(buffer);
    return -1;
  }

  /* reset to zero first */
  hwloc_bitmap_zero(set);

  /* parse the whole mask */
  tmpbuf = buffer;
  while (sscanf(tmpbuf, "%lx", &map) == 1) {
    /* read one kernel cpu mask and the ending comma */
    if (nr_maps == nr_maps_allocated) {
      unsigned long *tmp = realloc(maps, 2*nr_maps_allocated * sizeof(*maps));
      if (!tmp) {
	free(buffer);
	free(maps);
	return -1;
      }
      maps = tmp;
      nr_maps_allocated *= 2;
    }

    tmpbuf = strchr(tmpbuf, ',');
    if (!tmpbuf) {
      maps[nr_maps++] = map;
      break;
    } else
      tmpbuf++;

    if (!map && !nr_maps)
      /* ignore the first map if it's empty */
      continue;

    maps[nr_maps++] = map;
  }

  free(buffer);

  /* convert into a set */
#if KERNEL_CPU_MASK_BITS == HWLOC_BITS_PER_LONG
  for(i=0; i<nr_maps; i++)
    hwloc_bitmap_set_ith_ulong(set, i, maps[nr_maps-1-i]);
#else
  for(i=0; i<(nr_maps+1)/2; i++) {
    unsigned long mask;
    mask = maps[nr_maps-2*i-1];
    if (2*i+1<nr_maps)
      mask |= maps[nr_maps-2*i-2] << KERNEL_CPU_MASK_BITS;
    hwloc_bitmap_set_ith_ulong(set, i, mask);
  }
#endif

  free(maps);

  /* Only update the static value with the final one,
   * to avoid sharing intermediate values that we modify,
   * in case there's ever multiple concurrent calls.
   */
  if (nr_maps_allocated > _nr_maps_allocated)
    _nr_maps_allocated = nr_maps_allocated;
  return 0;
}

static __hwloc_inline int
hwloc__read_path_as_cpumask(const char *maskpath, hwloc_bitmap_t set, int fsroot_fd)
{
  int fd, err;
  fd = hwloc_open(maskpath, fsroot_fd);
  if (fd < 0)
    return -1;
  err = hwloc__read_fd_as_cpumask(fd, set);
  close(fd);
  return err;
}

static __hwloc_inline hwloc_bitmap_t
hwloc__alloc_read_path_as_cpumask(const char *maskpath, int fsroot_fd)
{
  hwloc_bitmap_t set;
  int err;
  set = hwloc_bitmap_alloc();
  if (!set)
    return NULL;
  err = hwloc__read_path_as_cpumask(maskpath, set, fsroot_fd);
  if (err < 0) {
    hwloc_bitmap_free(set);
    return NULL;
  } else
    return set;
}

/* set must be full on input */
static __hwloc_inline int
hwloc__read_fd_as_cpulist(int fd, hwloc_bitmap_t set)
{
  /* Kernel sysfs files are usually at most one page.
   * But cpulists can be of very different sizes depending on the fragmentation,
   * so don't bother remember the actual read size between invocations.
   * We don't have many invocations anyway.
   */
  size_t filesize = hwloc_getpagesize();
  char *buffer, *current, *comma, *tmp;
  int prevlast, nextfirst, nextlast; /* beginning/end of enabled-segments */

  if (hwloc__read_fd(fd, &buffer, &filesize) < 0)
    return -1;

  current = buffer;
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
    if (prevlast+1 <= nextfirst-1)
      hwloc_bitmap_clr_range(set, prevlast+1, nextfirst-1);

    /* switch to next enabled-segment */
    prevlast = nextlast;
    if (!comma)
      break;
    current = comma+1;
  }

  hwloc_bitmap_clr_range(set, prevlast+1, -1);
  free(buffer);
  return 0;
}


/*****************************
 ******* CpuBind Hooks *******
 *****************************/

int
hwloc_linux_set_tid_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, pid_t tid __hwloc_attribute_unused, hwloc_const_bitmap_t hwloc_set __hwloc_attribute_unused)
{
  /* TODO Kerrighed: Use
   * int migrate (pid_t pid, int destination_node);
   * int migrate_self (int destination_node);
   * int thread_migrate (int thread_id, int destination_node);
   */

  /* The resulting binding is always strict */

#if defined(HWLOC_HAVE_CPU_SET_S) && !defined(HWLOC_HAVE_OLD_SCHED_SETAFFINITY)
  cpu_set_t *plinux_set;
  unsigned cpu;
  int last;
  size_t setsize;
  int err;

  last = hwloc_bitmap_last(hwloc_set);
  if (last == -1) {
    errno = EINVAL;
    return -1;
  }

  setsize = CPU_ALLOC_SIZE(last+1);
  plinux_set = CPU_ALLOC(last+1);

  CPU_ZERO_S(setsize, plinux_set);
  hwloc_bitmap_foreach_begin(cpu, hwloc_set)
    CPU_SET_S(cpu, setsize, plinux_set);
  hwloc_bitmap_foreach_end();

  err = sched_setaffinity(tid, setsize, plinux_set);

  CPU_FREE(plinux_set);
  return err;
#elif defined(HWLOC_HAVE_CPU_SET)
  cpu_set_t linux_set;
  unsigned cpu;

  CPU_ZERO(&linux_set);
  hwloc_bitmap_foreach_begin(cpu, hwloc_set)
    CPU_SET(cpu, &linux_set);
  hwloc_bitmap_foreach_end();

#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
  return sched_setaffinity(tid, &linux_set);
#else /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
  return sched_setaffinity(tid, sizeof(linux_set), &linux_set);
#endif /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
#elif defined(HWLOC_HAVE_SYSCALL)
  unsigned long mask = hwloc_bitmap_to_ulong(hwloc_set);

#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
  return sched_setaffinity(tid, (void*) &mask);
#else /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
  return sched_setaffinity(tid, sizeof(mask), (void*) &mask);
#endif /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
#else /* !SYSCALL */
  errno = ENOSYS;
  return -1;
#endif /* !SYSCALL */
}

#if defined(HWLOC_HAVE_CPU_SET_S) && !defined(HWLOC_HAVE_OLD_SCHED_SETAFFINITY)
/*
 * On some kernels, sched_getaffinity requires the output size to be larger
 * than the kernel cpu_set size (defined by CONFIG_NR_CPUS).
 * Try sched_affinity on ourself until we find a nr_cpus value that makes
 * the kernel happy.
 */
static int
hwloc_linux_find_kernel_nr_cpus(hwloc_topology_t topology)
{
  static int _nr_cpus = -1;
  int nr_cpus = _nr_cpus;
  int fd;

  if (nr_cpus != -1)
    /* already computed */
    return nr_cpus;

  if (topology->levels[0][0]->complete_cpuset)
    /* start with a nr_cpus that may contain the whole topology */
    nr_cpus = hwloc_bitmap_last(topology->levels[0][0]->complete_cpuset) + 1;
  if (nr_cpus <= 0)
    /* start from scratch, the topology isn't ready yet (complete_cpuset is missing (-1) or empty (0))*/
    nr_cpus = 1;

  fd = open("/sys/devices/system/cpu/possible", O_RDONLY); /* binding only supported in real fsroot, no need for data->root_fd */
  if (fd >= 0) {
    hwloc_bitmap_t possible_bitmap = hwloc_bitmap_alloc_full();
    if (hwloc__read_fd_as_cpulist(fd, possible_bitmap) == 0) {
      int max_possible = hwloc_bitmap_last(possible_bitmap);
      hwloc_debug_bitmap("possible CPUs are %s\n", possible_bitmap);

      if (nr_cpus < max_possible + 1)
        nr_cpus = max_possible + 1;
    }
    close(fd);
    hwloc_bitmap_free(possible_bitmap);
  }

  while (1) {
    cpu_set_t *set = CPU_ALLOC(nr_cpus);
    size_t setsize = CPU_ALLOC_SIZE(nr_cpus);
    int err = sched_getaffinity(0, setsize, set); /* always works, unless setsize is too small */
    CPU_FREE(set);
    nr_cpus = setsize * 8; /* that's the value that was actually tested */
    if (!err)
      /* Found it. Only update the static value with the final one,
       * to avoid sharing intermediate values that we modify,
       * in case there's ever multiple concurrent calls.
       */
      return _nr_cpus = nr_cpus;
    nr_cpus *= 2;
  }
}
#endif

int
hwloc_linux_get_tid_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, pid_t tid __hwloc_attribute_unused, hwloc_bitmap_t hwloc_set __hwloc_attribute_unused)
{
  int err __hwloc_attribute_unused;
  /* TODO Kerrighed */

#if defined(HWLOC_HAVE_CPU_SET_S) && !defined(HWLOC_HAVE_OLD_SCHED_SETAFFINITY)
  cpu_set_t *plinux_set;
  unsigned cpu;
  int last;
  size_t setsize;
  int kernel_nr_cpus;

  /* find the kernel nr_cpus so as to use a large enough cpu_set size */
  kernel_nr_cpus = hwloc_linux_find_kernel_nr_cpus(topology);
  setsize = CPU_ALLOC_SIZE(kernel_nr_cpus);
  plinux_set = CPU_ALLOC(kernel_nr_cpus);

  err = sched_getaffinity(tid, setsize, plinux_set);

  if (err < 0) {
    CPU_FREE(plinux_set);
    return -1;
  }

  last = -1;
  if (topology->levels[0][0]->complete_cpuset)
    last = hwloc_bitmap_last(topology->levels[0][0]->complete_cpuset);
  if (last == -1)
    /* round the maximal support number, the topology isn't ready yet (complete_cpuset is missing or empty)*/
    last = kernel_nr_cpus-1;

  hwloc_bitmap_zero(hwloc_set);
  for(cpu=0; cpu<=(unsigned) last; cpu++)
    if (CPU_ISSET_S(cpu, setsize, plinux_set))
      hwloc_bitmap_set(hwloc_set, cpu);

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

  hwloc_bitmap_zero(hwloc_set);
  for(cpu=0; cpu<CPU_SETSIZE; cpu++)
    if (CPU_ISSET(cpu, &linux_set))
      hwloc_bitmap_set(hwloc_set, cpu);
#elif defined(HWLOC_HAVE_SYSCALL)
  unsigned long mask;

#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
  err = sched_getaffinity(tid, (void*) &mask);
#else /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
  err = sched_getaffinity(tid, sizeof(mask), (void*) &mask);
#endif /* HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
  if (err < 0)
    return -1;

  hwloc_bitmap_from_ulong(hwloc_set, mask);
#else /* !SYSCALL */
  errno = ENOSYS;
  return -1;
#endif /* !SYSCALL */

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

/* Per-tid callbacks */
typedef int (*hwloc_linux_foreach_proc_tid_cb_t)(hwloc_topology_t topology, pid_t tid, void *data, int idx);

static int
hwloc_linux_foreach_proc_tid(hwloc_topology_t topology,
			     pid_t pid, hwloc_linux_foreach_proc_tid_cb_t cb,
			     void *data)
{
  char taskdir_path[128];
  DIR *taskdir;
  pid_t *tids, *newtids;
  unsigned i, nr, newnr, failed = 0, failed_errno = 0;
  unsigned retrynr = 0;
  int err;

  if (pid)
    snprintf(taskdir_path, sizeof(taskdir_path), "/proc/%u/task", (unsigned) pid);
  else
    snprintf(taskdir_path, sizeof(taskdir_path), "/proc/self/task");

  taskdir = opendir(taskdir_path);
  if (!taskdir) {
    if (errno == ENOENT)
      errno = EINVAL;
    err = -1;
    goto out;
  }

  /* read the current list of threads */
  err = hwloc_linux_get_proc_tids(taskdir, &nr, &tids);
  if (err < 0)
    goto out_with_dir;

 retry:
  /* apply the callback to all threads */
  failed=0;
  for(i=0; i<nr; i++) {
    err = cb(topology, tids[i], data, i);
    if (err < 0) {
      failed++;
      failed_errno = errno;
    }
  }

  /* re-read the list of thread */
  err = hwloc_linux_get_proc_tids(taskdir, &newnr, &newtids);
  if (err < 0)
    goto out_with_tids;
  /* retry if the list changed in the meantime, or we failed for *some* threads only.
   * if we're really unlucky, all threads changed but we got the same set of tids. no way to support this.
   */
  if (newnr != nr || memcmp(newtids, tids, nr*sizeof(pid_t)) || (failed && failed != nr)) {
    free(tids);
    tids = newtids;
    nr = newnr;
    if (++retrynr > 10) {
      /* we tried 10 times, it didn't work, the application is probably creating/destroying many threads, stop trying */
      errno = EAGAIN;
      err = -1;
      goto out_with_tids;
    }
    goto retry;
  } else {
    free(newtids);
  }

  /* if all threads failed, return the last errno. */
  if (failed) {
    err = -1;
    errno = failed_errno;
    goto out_with_tids;
  }

  err = 0;
 out_with_tids:
  free(tids);
 out_with_dir:
  closedir(taskdir);
 out:
  return err;
}

/* Per-tid proc_set_cpubind callback and caller.
 * Callback data is a hwloc_bitmap_t. */
static int
hwloc_linux_foreach_proc_tid_set_cpubind_cb(hwloc_topology_t topology, pid_t tid, void *data, int idx __hwloc_attribute_unused)
{
  return hwloc_linux_set_tid_cpubind(topology, tid, (hwloc_bitmap_t) data);
}

static int
hwloc_linux_set_pid_cpubind(hwloc_topology_t topology, pid_t pid, hwloc_const_bitmap_t hwloc_set, int flags __hwloc_attribute_unused)
{
  return hwloc_linux_foreach_proc_tid(topology, pid,
				      hwloc_linux_foreach_proc_tid_set_cpubind_cb,
				      (void*) hwloc_set);
}

/* Per-tid proc_get_cpubind callback data, callback function and caller */
struct hwloc_linux_foreach_proc_tid_get_cpubind_cb_data_s {
  hwloc_bitmap_t cpuset;
  hwloc_bitmap_t tidset;
  int flags;
};

static int
hwloc_linux_foreach_proc_tid_get_cpubind_cb(hwloc_topology_t topology, pid_t tid, void *_data, int idx)
{
  struct hwloc_linux_foreach_proc_tid_get_cpubind_cb_data_s *data = _data;
  hwloc_bitmap_t cpuset = data->cpuset;
  hwloc_bitmap_t tidset = data->tidset;
  int flags = data->flags;

  if (hwloc_linux_get_tid_cpubind(topology, tid, tidset))
    return -1;

  /* reset the cpuset on first iteration */
  if (!idx)
    hwloc_bitmap_zero(cpuset);

  if (flags & HWLOC_CPUBIND_STRICT) {
    /* if STRICT, we want all threads to have the same binding */
    if (!idx) {
      /* this is the first thread, copy its binding */
      hwloc_bitmap_copy(cpuset, tidset);
    } else if (!hwloc_bitmap_isequal(cpuset, tidset)) {
      /* this is not the first thread, and it's binding is different */
      errno = EXDEV;
      return -1;
    }
  } else {
    /* if not STRICT, just OR all thread bindings */
    hwloc_bitmap_or(cpuset, cpuset, tidset);
  }
  return 0;
}

static int
hwloc_linux_get_pid_cpubind(hwloc_topology_t topology, pid_t pid, hwloc_bitmap_t hwloc_set, int flags)
{
  struct hwloc_linux_foreach_proc_tid_get_cpubind_cb_data_s data;
  hwloc_bitmap_t tidset = hwloc_bitmap_alloc();
  int ret;

  data.cpuset = hwloc_set;
  data.tidset = tidset;
  data.flags = flags;
  ret = hwloc_linux_foreach_proc_tid(topology, pid,
				     hwloc_linux_foreach_proc_tid_get_cpubind_cb,
				     (void*) &data);
  hwloc_bitmap_free(tidset);
  return ret;
}

static int
hwloc_linux_set_proc_cpubind(hwloc_topology_t topology, pid_t pid, hwloc_const_bitmap_t hwloc_set, int flags)
{
  if (pid == 0)
    pid = topology->pid;
  if (flags & HWLOC_CPUBIND_THREAD)
    return hwloc_linux_set_tid_cpubind(topology, pid, hwloc_set);
  else
    return hwloc_linux_set_pid_cpubind(topology, pid, hwloc_set, flags);
}

static int
hwloc_linux_get_proc_cpubind(hwloc_topology_t topology, pid_t pid, hwloc_bitmap_t hwloc_set, int flags)
{
  if (pid == 0)
    pid = topology->pid;
  if (flags & HWLOC_CPUBIND_THREAD)
    return hwloc_linux_get_tid_cpubind(topology, pid, hwloc_set);
  else
    return hwloc_linux_get_pid_cpubind(topology, pid, hwloc_set, flags);
}

static int
hwloc_linux_set_thisproc_cpubind(hwloc_topology_t topology, hwloc_const_bitmap_t hwloc_set, int flags)
{
  return hwloc_linux_set_pid_cpubind(topology, topology->pid, hwloc_set, flags);
}

static int
hwloc_linux_get_thisproc_cpubind(hwloc_topology_t topology, hwloc_bitmap_t hwloc_set, int flags)
{
  return hwloc_linux_get_pid_cpubind(topology, topology->pid, hwloc_set, flags);
}

static int
hwloc_linux_set_thisthread_cpubind(hwloc_topology_t topology, hwloc_const_bitmap_t hwloc_set, int flags __hwloc_attribute_unused)
{
  if (topology->pid) {
    errno = ENOSYS;
    return -1;
  }
  return hwloc_linux_set_tid_cpubind(topology, 0, hwloc_set);
}

static int
hwloc_linux_get_thisthread_cpubind(hwloc_topology_t topology, hwloc_bitmap_t hwloc_set, int flags __hwloc_attribute_unused)
{
  if (topology->pid) {
    errno = ENOSYS;
    return -1;
  }
  return hwloc_linux_get_tid_cpubind(topology, 0, hwloc_set);
}

#if HAVE_DECL_PTHREAD_SETAFFINITY_NP
#pragma weak pthread_setaffinity_np
#pragma weak pthread_self

static int
hwloc_linux_set_thread_cpubind(hwloc_topology_t topology, pthread_t tid, hwloc_const_bitmap_t hwloc_set, int flags __hwloc_attribute_unused)
{
  int err;

  if (topology->pid) {
    errno = ENOSYS;
    return -1;
  }

  if (!pthread_self) {
    /* ?! Application uses set_thread_cpubind, but doesn't link against libpthread ?! */
    errno = ENOSYS;
    return -1;
  }
  if (tid == pthread_self())
    return hwloc_linux_set_tid_cpubind(topology, 0, hwloc_set);

  if (!pthread_setaffinity_np) {
    errno = ENOSYS;
    return -1;
  }
  /* TODO Kerrighed: Use
   * int migrate (pid_t pid, int destination_node);
   * int migrate_self (int destination_node);
   * int thread_migrate (int thread_id, int destination_node);
   */

#if defined(HWLOC_HAVE_CPU_SET_S) && !defined(HWLOC_HAVE_OLD_SCHED_SETAFFINITY)
  /* Use a separate block so that we can define specific variable
     types here */
  {
     cpu_set_t *plinux_set;
     unsigned cpu;
     int last;
     size_t setsize;

     last = hwloc_bitmap_last(hwloc_set);
     if (last == -1) {
       errno = EINVAL;
       return -1;
     }

     setsize = CPU_ALLOC_SIZE(last+1);
     plinux_set = CPU_ALLOC(last+1);

     CPU_ZERO_S(setsize, plinux_set);
     hwloc_bitmap_foreach_begin(cpu, hwloc_set)
         CPU_SET_S(cpu, setsize, plinux_set);
     hwloc_bitmap_foreach_end();

     err = pthread_setaffinity_np(tid, setsize, plinux_set);

     CPU_FREE(plinux_set);
  }
#elif defined(HWLOC_HAVE_CPU_SET)
  /* Use a separate block so that we can define specific variable
     types here */
  {
     cpu_set_t linux_set;
     unsigned cpu;

     CPU_ZERO(&linux_set);
     hwloc_bitmap_foreach_begin(cpu, hwloc_set)
         CPU_SET(cpu, &linux_set);
     hwloc_bitmap_foreach_end();

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
      unsigned long mask = hwloc_bitmap_to_ulong(hwloc_set);

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
#pragma weak pthread_self

static int
hwloc_linux_get_thread_cpubind(hwloc_topology_t topology, pthread_t tid, hwloc_bitmap_t hwloc_set, int flags __hwloc_attribute_unused)
{
  int err;

  if (topology->pid) {
    errno = ENOSYS;
    return -1;
  }

  if (!pthread_self) {
    /* ?! Application uses set_thread_cpubind, but doesn't link against libpthread ?! */
    errno = ENOSYS;
    return -1;
  }
  if (tid == pthread_self())
    return hwloc_linux_get_tid_cpubind(topology, 0, hwloc_set);

  if (!pthread_getaffinity_np) {
    errno = ENOSYS;
    return -1;
  }
  /* TODO Kerrighed */

#if defined(HWLOC_HAVE_CPU_SET_S) && !defined(HWLOC_HAVE_OLD_SCHED_SETAFFINITY)
  /* Use a separate block so that we can define specific variable
     types here */
  {
     cpu_set_t *plinux_set;
     unsigned cpu;
     int last;
     size_t setsize;

     last = hwloc_bitmap_last(topology->levels[0][0]->complete_cpuset);
     assert (last != -1);

     setsize = CPU_ALLOC_SIZE(last+1);
     plinux_set = CPU_ALLOC(last+1);

     err = pthread_getaffinity_np(tid, setsize, plinux_set);
     if (err) {
        CPU_FREE(plinux_set);
        errno = err;
        return -1;
     }

     hwloc_bitmap_zero(hwloc_set);
     for(cpu=0; cpu<=(unsigned) last; cpu++)
       if (CPU_ISSET_S(cpu, setsize, plinux_set))
	 hwloc_bitmap_set(hwloc_set, cpu);

     CPU_FREE(plinux_set);
  }
#elif defined(HWLOC_HAVE_CPU_SET)
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

     hwloc_bitmap_zero(hwloc_set);
     for(cpu=0; cpu<CPU_SETSIZE; cpu++)
       if (CPU_ISSET(cpu, &linux_set))
	 hwloc_bitmap_set(hwloc_set, cpu);
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

     hwloc_bitmap_from_ulong(hwloc_set, mask);
  }
#endif /* CPU_SET */

  return 0;
}
#endif /* HAVE_DECL_PTHREAD_GETAFFINITY_NP */

int
hwloc_linux_get_tid_last_cpu_location(hwloc_topology_t topology __hwloc_attribute_unused, pid_t tid, hwloc_bitmap_t set)
{
  /* read /proc/pid/stat.
   * its second field contains the command name between parentheses,
   * and the command itself may contain parentheses,
   * so read the whole line and find the last closing parenthesis to find the third field.
   */
  char buf[1024] = "";
  char name[64];
  char *tmp;
  int fd, i, err;

  if (!tid) {
#ifdef SYS_gettid
    tid = syscall(SYS_gettid);
#else
    errno = ENOSYS;
    return -1;
#endif
  }

  snprintf(name, sizeof(name), "/proc/%lu/stat", (unsigned long) tid);
  fd = open(name, O_RDONLY); /* no fsroot for real /proc */
  if (fd < 0) {
    errno = ENOSYS;
    return -1;
  }
  err = read(fd, buf, sizeof(buf)-1); /* read -1 to put the ending \0 */
  close(fd);
  if (err <= 0) {
    errno = ENOSYS;
    return -1;
  }
  buf[err-1] = '\0';

  tmp = strrchr(buf, ')');
  if (!tmp) {
    errno = ENOSYS;
    return -1;
  }
  /* skip ') ' to find the actual third argument */
  tmp += 2;

  /* skip 35 fields */
  for(i=0; i<36; i++) {
    tmp = strchr(tmp, ' ');
    if (!tmp) {
      errno = ENOSYS;
      return -1;
    }
    /* skip the ' ' itself */
    tmp++;
  }

  /* read the last cpu in the 38th field now */
  if (sscanf(tmp, "%d ", &i) != 1) {
    errno = ENOSYS;
    return -1;
  }

  hwloc_bitmap_only(set, i);
  return 0;
}

/* Per-tid proc_get_last_cpu_location callback data, callback function and caller */
struct hwloc_linux_foreach_proc_tid_get_last_cpu_location_cb_data_s {
  hwloc_bitmap_t cpuset;
  hwloc_bitmap_t tidset;
};

static int
hwloc_linux_foreach_proc_tid_get_last_cpu_location_cb(hwloc_topology_t topology, pid_t tid, void *_data, int idx)
{
  struct hwloc_linux_foreach_proc_tid_get_last_cpu_location_cb_data_s *data = _data;
  hwloc_bitmap_t cpuset = data->cpuset;
  hwloc_bitmap_t tidset = data->tidset;

  if (hwloc_linux_get_tid_last_cpu_location(topology, tid, tidset))
    return -1;

  /* reset the cpuset on first iteration */
  if (!idx)
    hwloc_bitmap_zero(cpuset);

  hwloc_bitmap_or(cpuset, cpuset, tidset);
  return 0;
}

static int
hwloc_linux_get_pid_last_cpu_location(hwloc_topology_t topology, pid_t pid, hwloc_bitmap_t hwloc_set, int flags __hwloc_attribute_unused)
{
  struct hwloc_linux_foreach_proc_tid_get_last_cpu_location_cb_data_s data;
  hwloc_bitmap_t tidset = hwloc_bitmap_alloc();
  int ret;

  data.cpuset = hwloc_set;
  data.tidset = tidset;
  ret = hwloc_linux_foreach_proc_tid(topology, pid,
				     hwloc_linux_foreach_proc_tid_get_last_cpu_location_cb,
				     &data);
  hwloc_bitmap_free(tidset);
  return ret;
}

static int
hwloc_linux_get_proc_last_cpu_location(hwloc_topology_t topology, pid_t pid, hwloc_bitmap_t hwloc_set, int flags)
{
  if (pid == 0)
    pid = topology->pid;
  if (flags & HWLOC_CPUBIND_THREAD)
    return hwloc_linux_get_tid_last_cpu_location(topology, pid, hwloc_set);
  else
    return hwloc_linux_get_pid_last_cpu_location(topology, pid, hwloc_set, flags);
}

static int
hwloc_linux_get_thisproc_last_cpu_location(hwloc_topology_t topology, hwloc_bitmap_t hwloc_set, int flags)
{
  return hwloc_linux_get_pid_last_cpu_location(topology, topology->pid, hwloc_set, flags);
}

static int
hwloc_linux_get_thisthread_last_cpu_location(hwloc_topology_t topology, hwloc_bitmap_t hwloc_set, int flags __hwloc_attribute_unused)
{
  if (topology->pid) {
    errno = ENOSYS;
    return -1;
  }
  return hwloc_linux_get_tid_last_cpu_location(topology, 0, hwloc_set);
}



/***************************
 ****** Membind hooks ******
 ***************************/

#if defined HWLOC_HAVE_SET_MEMPOLICY || defined HWLOC_HAVE_MBIND
static int
hwloc_linux_membind_policy_from_hwloc(int *linuxpolicy, hwloc_membind_policy_t policy, int flags)
{
  switch (policy) {
  case HWLOC_MEMBIND_DEFAULT:
  case HWLOC_MEMBIND_FIRSTTOUCH:
    *linuxpolicy = MPOL_DEFAULT;
    break;
  case HWLOC_MEMBIND_BIND:
    if (flags & HWLOC_MEMBIND_STRICT)
      *linuxpolicy = MPOL_BIND;
    else
      *linuxpolicy = MPOL_PREFERRED;
    break;
  case HWLOC_MEMBIND_INTERLEAVE:
    *linuxpolicy = MPOL_INTERLEAVE;
    break;
  /* TODO: next-touch when (if?) patch applied upstream */
  default:
    errno = ENOSYS;
    return -1;
  }
  return 0;
}

static int
hwloc_linux_membind_mask_from_nodeset(hwloc_topology_t topology __hwloc_attribute_unused,
				      hwloc_const_nodeset_t nodeset,
				      unsigned *max_os_index_p, unsigned long **linuxmaskp)
{
  unsigned max_os_index = 0; /* highest os_index + 1 */
  unsigned long *linuxmask;
  unsigned i;
  hwloc_nodeset_t linux_nodeset = NULL;

  if (hwloc_bitmap_isfull(nodeset)) {
    linux_nodeset = hwloc_bitmap_alloc();
    hwloc_bitmap_only(linux_nodeset, 0);
    nodeset = linux_nodeset;
  }

  max_os_index = hwloc_bitmap_last(nodeset);
  if (max_os_index == (unsigned) -1)
    max_os_index = 0;
  /* add 1 to convert the last os_index into a max_os_index,
   * and round up to the nearest multiple of BITS_PER_LONG */
  max_os_index = (max_os_index + 1 + HWLOC_BITS_PER_LONG - 1) & ~(HWLOC_BITS_PER_LONG - 1);

  linuxmask = calloc(max_os_index/HWLOC_BITS_PER_LONG, sizeof(long));
  if (!linuxmask) {
    hwloc_bitmap_free(linux_nodeset);
    errno = ENOMEM;
    return -1;
  }

  for(i=0; i<max_os_index/HWLOC_BITS_PER_LONG; i++)
    linuxmask[i] = hwloc_bitmap_to_ith_ulong(nodeset, i);

  if (linux_nodeset)
    hwloc_bitmap_free(linux_nodeset);

  *max_os_index_p = max_os_index;
  *linuxmaskp = linuxmask;
  return 0;
}

static void
hwloc_linux_membind_mask_to_nodeset(hwloc_topology_t topology __hwloc_attribute_unused,
				    hwloc_nodeset_t nodeset,
				    unsigned max_os_index, const unsigned long *linuxmask)
{
  unsigned i;

#ifdef HWLOC_DEBUG
  /* max_os_index comes from hwloc_linux_find_kernel_max_numnodes() so it's a multiple of HWLOC_BITS_PER_LONG */
  assert(!(max_os_index%HWLOC_BITS_PER_LONG));
#endif

  hwloc_bitmap_zero(nodeset);
  for(i=0; i<max_os_index/HWLOC_BITS_PER_LONG; i++)
    hwloc_bitmap_set_ith_ulong(nodeset, i, linuxmask[i]);
}
#endif /* HWLOC_HAVE_SET_MEMPOLICY || HWLOC_HAVE_MBIND */

#ifdef HWLOC_HAVE_MBIND
static int
hwloc_linux_set_area_membind(hwloc_topology_t topology, const void *addr, size_t len, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags)
{
  unsigned max_os_index; /* highest os_index + 1 */
  unsigned long *linuxmask;
  size_t remainder;
  int linuxpolicy;
  unsigned linuxflags = 0;
  int err;

  remainder = (uintptr_t) addr & (hwloc_getpagesize()-1);
  addr = (char*) addr - remainder;
  len += remainder;

  err = hwloc_linux_membind_policy_from_hwloc(&linuxpolicy, policy, flags);
  if (err < 0)
    return err;

  if (linuxpolicy == MPOL_DEFAULT)
    /* Some Linux kernels don't like being passed a set */
    return mbind((void *) addr, len, linuxpolicy, NULL, 0, 0);

  err = hwloc_linux_membind_mask_from_nodeset(topology, nodeset, &max_os_index, &linuxmask);
  if (err < 0)
    goto out;

  if (flags & HWLOC_MEMBIND_MIGRATE) {
#ifdef MPOL_MF_MOVE
    linuxflags = MPOL_MF_MOVE;
    if (flags & HWLOC_MEMBIND_STRICT)
      linuxflags |= MPOL_MF_STRICT;
#else
    if (flags & HWLOC_MEMBIND_STRICT) {
      errno = ENOSYS;
      goto out_with_mask;
    }
#endif
  }

  err = mbind((void *) addr, len, linuxpolicy, linuxmask, max_os_index+1, linuxflags);
  if (err < 0)
    goto out_with_mask;

  free(linuxmask);
  return 0;

 out_with_mask:
  free(linuxmask);
 out:
  return -1;
}

static void *
hwloc_linux_alloc_membind(hwloc_topology_t topology, size_t len, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags)
{
  void *buffer;
  int err;

  buffer = hwloc_alloc_mmap(topology, len);
  if (!buffer)
    return NULL;

  err = hwloc_linux_set_area_membind(topology, buffer, len, nodeset, policy, flags);
  if (err < 0 && policy & HWLOC_MEMBIND_STRICT) {
    munmap(buffer, len);
    return NULL;
  }

  return buffer;
}
#endif /* HWLOC_HAVE_MBIND */

#ifdef HWLOC_HAVE_SET_MEMPOLICY
static int
hwloc_linux_set_thisthread_membind(hwloc_topology_t topology, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags)
{
  unsigned max_os_index; /* highest os_index + 1 */
  unsigned long *linuxmask;
  int linuxpolicy;
  int err;

  err = hwloc_linux_membind_policy_from_hwloc(&linuxpolicy, policy, flags);
  if (err < 0)
    return err;

  if (linuxpolicy == MPOL_DEFAULT)
    /* Some Linux kernels don't like being passed a set */
    return set_mempolicy(linuxpolicy, NULL, 0);

  err = hwloc_linux_membind_mask_from_nodeset(topology, nodeset, &max_os_index, &linuxmask);
  if (err < 0)
    goto out;

  if (flags & HWLOC_MEMBIND_MIGRATE) {
#ifdef HWLOC_HAVE_MIGRATE_PAGES
    unsigned long *fullmask = malloc(max_os_index/HWLOC_BITS_PER_LONG * sizeof(long));
    if (fullmask) {
      memset(fullmask, 0xf, max_os_index/HWLOC_BITS_PER_LONG * sizeof(long));
      err = migrate_pages(0, max_os_index+1, fullmask, linuxmask);
      free(fullmask);
    } else
      err = -1;
    if (err < 0 && (flags & HWLOC_MEMBIND_STRICT))
      goto out_with_mask;
#else
    errno = ENOSYS;
    goto out_with_mask;
#endif
  }

  err = set_mempolicy(linuxpolicy, linuxmask, max_os_index+1);
  if (err < 0)
    goto out_with_mask;

  free(linuxmask);
  return 0;

 out_with_mask:
  free(linuxmask);
 out:
  return -1;
}

/*
 * On some kernels, get_mempolicy requires the output size to be larger
 * than the kernel MAX_NUMNODES (defined by CONFIG_NODES_SHIFT).
 * Try get_mempolicy on ourself until we find a max_os_index value that
 * makes the kernel happy.
 */
static int
hwloc_linux_find_kernel_max_numnodes(hwloc_topology_t topology __hwloc_attribute_unused)
{
  static int _max_numnodes = -1, max_numnodes;
  int linuxpolicy;

  if (_max_numnodes != -1)
    /* already computed */
    return _max_numnodes;

  /* start with a single ulong, it's the minimal and it's enough for most machines */
  max_numnodes = HWLOC_BITS_PER_LONG;
  while (1) {
    unsigned long *mask = malloc(max_numnodes / HWLOC_BITS_PER_LONG * sizeof(long));
    int err = get_mempolicy(&linuxpolicy, mask, max_numnodes, 0, 0);
    free(mask);
    if (!err || errno != EINVAL)
      /* Found it. Only update the static value with the final one,
       * to avoid sharing intermediate values that we modify,
       * in case there's ever multiple concurrent calls.
       */
      return _max_numnodes = max_numnodes;
    max_numnodes *= 2;
  }
}

static int
hwloc_linux_membind_policy_to_hwloc(int linuxpolicy, hwloc_membind_policy_t *policy)
{
  switch (linuxpolicy) {
  case MPOL_DEFAULT:
    *policy = HWLOC_MEMBIND_FIRSTTOUCH;
    return 0;
  case MPOL_PREFERRED:
  case MPOL_BIND:
    *policy = HWLOC_MEMBIND_BIND;
    return 0;
  case MPOL_INTERLEAVE:
    *policy = HWLOC_MEMBIND_INTERLEAVE;
    return 0;
  default:
    errno = EINVAL;
    return -1;
  }
}

static int
hwloc_linux_get_thisthread_membind(hwloc_topology_t topology, hwloc_nodeset_t nodeset, hwloc_membind_policy_t *policy, int flags __hwloc_attribute_unused)
{
  unsigned max_os_index;
  unsigned long *linuxmask;
  int linuxpolicy;
  int err;

  max_os_index = hwloc_linux_find_kernel_max_numnodes(topology);

  linuxmask = malloc(max_os_index/HWLOC_BITS_PER_LONG * sizeof(long));
  if (!linuxmask) {
    errno = ENOMEM;
    goto out;
  }

  err = get_mempolicy(&linuxpolicy, linuxmask, max_os_index, 0, 0);
  if (err < 0)
    goto out_with_mask;

  if (linuxpolicy == MPOL_DEFAULT) {
    hwloc_bitmap_copy(nodeset, hwloc_topology_get_topology_nodeset(topology));
  } else {
    hwloc_linux_membind_mask_to_nodeset(topology, nodeset, max_os_index, linuxmask);
  }

  err = hwloc_linux_membind_policy_to_hwloc(linuxpolicy, policy);
  if (err < 0)
    goto out_with_mask;

  free(linuxmask);
  return 0;

 out_with_mask:
  free(linuxmask);
 out:
  return -1;
}

static int
hwloc_linux_get_area_membind(hwloc_topology_t topology, const void *addr, size_t len, hwloc_nodeset_t nodeset, hwloc_membind_policy_t *policy, int flags __hwloc_attribute_unused)
{
  unsigned max_os_index;
  unsigned long *linuxmask, *globallinuxmask;
  int linuxpolicy, globallinuxpolicy = 0;
  int mixed = 0;
  int full = 0;
  int first = 1;
  int pagesize = hwloc_getpagesize();
  char *tmpaddr;
  int err;
  unsigned i;

  max_os_index = hwloc_linux_find_kernel_max_numnodes(topology);

  linuxmask = malloc(max_os_index/HWLOC_BITS_PER_LONG * sizeof(long));
  if (!linuxmask) {
    errno = ENOMEM;
    goto out;
  }
  globallinuxmask = calloc(max_os_index/HWLOC_BITS_PER_LONG, sizeof(long));
  if (!globallinuxmask) {
    errno = ENOMEM;
    goto out_with_masks;
  }

  for(tmpaddr = (char *)((unsigned long)addr & ~(pagesize-1));
      tmpaddr < (char *)addr + len;
      tmpaddr += pagesize) {
    err = get_mempolicy(&linuxpolicy, linuxmask, max_os_index, tmpaddr, MPOL_F_ADDR);
    if (err < 0)
      goto out_with_masks;

    /* use the first found policy. if we find a different one later, set mixed to 1 */
    if (first)
      globallinuxpolicy = linuxpolicy;
    else if (globallinuxpolicy != linuxpolicy)
      mixed = 1;

    /* agregate masks, and set full to 1 if we ever find DEFAULT */
    if (full || linuxpolicy == MPOL_DEFAULT) {
      full = 1;
    } else {
      for(i=0; i<max_os_index/HWLOC_BITS_PER_LONG; i++)
        globallinuxmask[i] |= linuxmask[i];
    }

    first = 0;
  }

  if (mixed) {
    *policy = HWLOC_MEMBIND_MIXED;
  } else {
    err = hwloc_linux_membind_policy_to_hwloc(linuxpolicy, policy);
    if (err < 0)
      goto out_with_masks;
  }

  if (full) {
    hwloc_bitmap_copy(nodeset, hwloc_topology_get_topology_nodeset(topology));
  } else {
    hwloc_linux_membind_mask_to_nodeset(topology, nodeset, max_os_index, globallinuxmask);
  }

  free(globallinuxmask);
  free(linuxmask);
  return 0;

 out_with_masks:
  free(globallinuxmask);
  free(linuxmask);
 out:
  return -1;
}

#endif /* HWLOC_HAVE_SET_MEMPOLICY */

#ifdef HWLOC_HAVE_MOVE_PAGES
static int
hwloc_linux_get_area_memlocation(hwloc_topology_t topology __hwloc_attribute_unused, const void *addr, size_t len, hwloc_nodeset_t nodeset, int flags __hwloc_attribute_unused)
{
  unsigned offset;
  unsigned long count;
  void **pages;
  int *status;
  int pagesize = hwloc_getpagesize();
  int ret;
  unsigned i;

  offset = ((unsigned long) addr) & (pagesize-1);
  addr = ((char*) addr) - offset;
  len += offset;
  count = (len + pagesize-1)/pagesize;
  pages = malloc(count*sizeof(*pages));
  status = malloc(count*sizeof(*status));
  if (!pages || !status) {
    ret = -1;
    goto out_with_pages;
  }

  for(i=0; i<count; i++)
    pages[i] = ((char*)addr) + i*pagesize;

  ret = move_pages(0, count, pages, NULL, status, 0);
  if (ret  < 0)
    goto out_with_pages;

  hwloc_bitmap_zero(nodeset);
  for(i=0; i<count; i++)
    if (status[i] >= 0)
      hwloc_bitmap_set(nodeset, status[i]);
  ret = 0;

 out_with_pages:
  free(pages);
  free(status);
  return ret;
}
#endif /* HWLOC_HAVE_MOVE_PAGES */

static void hwloc_linux__get_allowed_resources(hwloc_topology_t topology, const char *root_path, int root_fd, char **cpuset_namep);

static int hwloc_linux_get_allowed_resources_hook(hwloc_topology_t topology)
{
  const char *fsroot_path;
  char *cpuset_name;
  int root_fd = -1;

  fsroot_path = getenv("HWLOC_FSROOT");
  if (!fsroot_path)
    fsroot_path = "/";

#ifdef HAVE_OPENAT
  root_fd = open(fsroot_path, O_RDONLY | O_DIRECTORY);
  if (root_fd < 0)
    goto out;
#else
  if (strcmp(fsroot_path, "/")) {
    errno = ENOSYS;
    goto out;
  }
#endif

  /* we could also error-out if the current topology doesn't actually match the system,
   * at least for PUs and NUMA nodes. But it would increase the overhead of loading XMLs.
   *
   * Just trust the user when he sets THISSYSTEM=1. It enables hacky
   * tests such as restricting random XML or synthetic to the current
   * machine (uses the default cgroup).
   */

  hwloc_linux__get_allowed_resources(topology, fsroot_path, root_fd, &cpuset_name);
  if (cpuset_name) {
    hwloc_obj_add_info(topology->levels[0][0], "LinuxCgroup", cpuset_name);
    free(cpuset_name);
  }
  if (root_fd != -1)
    close(root_fd);

 out:
  return -1;
}

void
hwloc_set_linuxfs_hooks(struct hwloc_binding_hooks *hooks,
			struct hwloc_topology_support *support __hwloc_attribute_unused)
{
  hooks->set_thisthread_cpubind = hwloc_linux_set_thisthread_cpubind;
  hooks->get_thisthread_cpubind = hwloc_linux_get_thisthread_cpubind;
  hooks->set_thisproc_cpubind = hwloc_linux_set_thisproc_cpubind;
  hooks->get_thisproc_cpubind = hwloc_linux_get_thisproc_cpubind;
  hooks->set_proc_cpubind = hwloc_linux_set_proc_cpubind;
  hooks->get_proc_cpubind = hwloc_linux_get_proc_cpubind;
#if HAVE_DECL_PTHREAD_SETAFFINITY_NP
  hooks->set_thread_cpubind = hwloc_linux_set_thread_cpubind;
#endif /* HAVE_DECL_PTHREAD_SETAFFINITY_NP */
#if HAVE_DECL_PTHREAD_GETAFFINITY_NP
  hooks->get_thread_cpubind = hwloc_linux_get_thread_cpubind;
#endif /* HAVE_DECL_PTHREAD_GETAFFINITY_NP */
  hooks->get_thisthread_last_cpu_location = hwloc_linux_get_thisthread_last_cpu_location;
  hooks->get_thisproc_last_cpu_location = hwloc_linux_get_thisproc_last_cpu_location;
  hooks->get_proc_last_cpu_location = hwloc_linux_get_proc_last_cpu_location;
#ifdef HWLOC_HAVE_SET_MEMPOLICY
  hooks->set_thisthread_membind = hwloc_linux_set_thisthread_membind;
  hooks->get_thisthread_membind = hwloc_linux_get_thisthread_membind;
  hooks->get_area_membind = hwloc_linux_get_area_membind;
#endif /* HWLOC_HAVE_SET_MEMPOLICY */
#ifdef HWLOC_HAVE_MBIND
  hooks->set_area_membind = hwloc_linux_set_area_membind;
#ifdef HWLOC_HAVE_MOVE_PAGES
  hooks->get_area_memlocation = hwloc_linux_get_area_memlocation;
#endif /* HWLOC_HAVE_MOVE_PAGES */
  hooks->alloc_membind = hwloc_linux_alloc_membind;
  hooks->alloc = hwloc_alloc_mmap;
  hooks->free_membind = hwloc_free_mmap;
  support->membind->firsttouch_membind = 1;
  support->membind->bind_membind = 1;
  support->membind->interleave_membind = 1;
#endif /* HWLOC_HAVE_MBIND */
#if (defined HWLOC_HAVE_MIGRATE_PAGES) || ((defined HWLOC_HAVE_MBIND) && (defined MPOL_MF_MOVE))
  support->membind->migrate_membind = 1;
#endif
  hooks->get_allowed_resources = hwloc_linux_get_allowed_resources_hook;
}


/*******************************************
 *** Misc Helpers for Topology Discovery ***
 *******************************************/

/* cpuinfo array */
struct hwloc_linux_cpuinfo_proc {
  /* set during hwloc_linux_parse_cpuinfo */
  unsigned long Pproc;
  /* set during hwloc_linux_parse_cpuinfo or -1 if unknown*/
  long Pcore, Ppkg;
  /* set later, or -1 if unknown */
  long Lcore, Lpkg;

  /* custom info, set during hwloc_linux_parse_cpuinfo */
  struct hwloc_obj_info_s *infos;
  unsigned infos_count;
};

/* deprecated but still needed in hwloc/linux.h for backward compat */
int
hwloc_linux_parse_cpumap_file(FILE *file, hwloc_bitmap_t set)
{
  unsigned long *maps;
  unsigned long map;
  int nr_maps = 0;
  static int _nr_maps_allocated = 8; /* Only compute the power-of-two above the kernel cpumask size once.
				      * Actually, it may increase multiple times if first read cpumaps start with zeroes.
				      */
  int nr_maps_allocated = _nr_maps_allocated;
  int i;

  maps = malloc(nr_maps_allocated * sizeof(*maps));
  if (!maps)
    return -1;

  /* reset to zero first */
  hwloc_bitmap_zero(set);

  /* parse the whole mask */
  while (fscanf(file, "%lx,", &map) == 1) /* read one kernel cpu mask and the ending comma */
    {
      if (nr_maps == nr_maps_allocated) {
	unsigned long *tmp = realloc(maps, 2*nr_maps_allocated * sizeof(*maps));
	if (!tmp) {
	  free(maps);
	  return -1;
	}
	maps = tmp;
	nr_maps_allocated *= 2;
      }

      if (!map && !nr_maps)
	/* ignore the first map if it's empty */
	continue;

      maps[nr_maps++] = map;
    }

  /* convert into a set */
#if KERNEL_CPU_MASK_BITS == HWLOC_BITS_PER_LONG
  for(i=0; i<nr_maps; i++)
    hwloc_bitmap_set_ith_ulong(set, i, maps[nr_maps-1-i]);
#else
  for(i=0; i<(nr_maps+1)/2; i++) {
    unsigned long mask;
    mask = maps[nr_maps-2*i-1];
    if (2*i+1<nr_maps)
      mask |= maps[nr_maps-2*i-2] << KERNEL_CPU_MASK_BITS;
    hwloc_bitmap_set_ith_ulong(set, i, mask);
  }
#endif

  free(maps);

  /* Only update the static value with the final one,
   * to avoid sharing intermediate values that we modify,
   * in case there's ever multiple concurrent calls.
   */
  if (nr_maps_allocated > _nr_maps_allocated)
    _nr_maps_allocated = nr_maps_allocated;
  return 0;
}

static void
hwloc_find_linux_cpuset_mntpnt(char **cgroup_mntpnt, char **cpuset_mntpnt, const char *root_path)
{
  char *mount_path;
  struct mntent mntent;
  FILE *fd;
  int err;
  size_t bufsize;
  char *buf;

  *cgroup_mntpnt = NULL;
  *cpuset_mntpnt = NULL;

  if (root_path) {
    /* setmntent() doesn't support openat(), so use the root_path directly */
    err = asprintf(&mount_path, "%s/proc/mounts", root_path);
    if (err < 0)
      return;
    fd = setmntent(mount_path, "r");
    free(mount_path);
  } else {
    fd = setmntent("/proc/mounts", "r");
  }
  if (!fd)
    return;

  /* getmntent_r() doesn't actually report an error when the buffer
   * is too small. It just silently truncates things. So we can't
   * dynamically resize things.
   *
   * Linux limits mount type, string, and options to one page each.
   * getmntent() limits the line size to 4kB.
   * so use 4*pagesize to be far above both.
   */
  bufsize = hwloc_getpagesize()*4;
  buf = malloc(bufsize);

  while (getmntent_r(fd, &mntent, buf, bufsize)) {
    if (!strcmp(mntent.mnt_type, "cpuset")) {
      hwloc_debug("Found cpuset mount point on %s\n", mntent.mnt_dir);
      *cpuset_mntpnt = strdup(mntent.mnt_dir);
      break;
    } else if (!strcmp(mntent.mnt_type, "cgroup")) {
      /* found a cgroup mntpnt */
      char *opt, *opts = mntent.mnt_opts;
      int cpuset_opt = 0;
      int noprefix_opt = 0;
      /* look at options */
      while ((opt = strsep(&opts, ",")) != NULL) {
	if (!strcmp(opt, "cpuset"))
	  cpuset_opt = 1;
	else if (!strcmp(opt, "noprefix"))
	  noprefix_opt = 1;
      }
      if (!cpuset_opt)
	continue;
      if (noprefix_opt) {
	hwloc_debug("Found cgroup emulating a cpuset mount point on %s\n", mntent.mnt_dir);
	*cpuset_mntpnt = strdup(mntent.mnt_dir);
      } else {
	hwloc_debug("Found cgroup/cpuset mount point on %s\n", mntent.mnt_dir);
	*cgroup_mntpnt = strdup(mntent.mnt_dir);
      }
      break;
    }
  }

  free(buf);
  endmntent(fd);
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
  FILE *file;
  int err;
  char *tmp;

  /* check whether a cgroup-cpuset is enabled */
  if (!pid)
    file = hwloc_fopen("/proc/self/cgroup", "r", fsroot_fd);
  else {
    char path[] = "/proc/XXXXXXXXXX/cgroup";
    snprintf(path, sizeof(path), "/proc/%d/cgroup", pid);
    file = hwloc_fopen(path, "r", fsroot_fd);
  }
  if (file) {
    /* find a cpuset line */
#define CGROUP_LINE_LEN 256
    char line[CGROUP_LINE_LEN];
    while (fgets(line, sizeof(line), file)) {
      char *end, *colon = strchr(line, ':');
      if (!colon)
	continue;
      if (strncmp(colon, ":cpuset:", 8))
	continue;

      /* found a cgroup-cpuset line, return the name */
      fclose(file);
      end = strchr(colon, '\n');
      if (end)
	*end = '\0';
      hwloc_debug("Found cgroup-cpuset %s\n", colon+8);
      return strdup(colon+8);
    }
    fclose(file);
  }

  /* check whether a cpuset is enabled */
  if (!pid)
    err = hwloc_read_path_by_length("/proc/self/cpuset", cpuset_name, sizeof(cpuset_name), fsroot_fd);
  else {
    char path[] = "/proc/XXXXXXXXXX/cpuset";
    snprintf(path, sizeof(path), "/proc/%d/cpuset", pid);
    err = hwloc_read_path_by_length(path, cpuset_name, sizeof(cpuset_name), fsroot_fd);
  }
  if (err < 0) {
    /* found nothing */
    hwloc_debug("%s", "No cgroup or cpuset found\n");
    return NULL;
  }

  /* found a cpuset, return the name */
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
static void
hwloc_admin_disable_set_from_cpuset(int root_fd,
				    const char *cgroup_mntpnt, const char *cpuset_mntpnt, const char *cpuset_name,
				    const char *attr_name,
				    hwloc_bitmap_t admin_enabled_cpus_set)
{
#define CPUSET_FILENAME_LEN 256
  char cpuset_filename[CPUSET_FILENAME_LEN];
  int fd;
  int err;

  if (cgroup_mntpnt) {
    /* try to read the cpuset from cgroup */
    snprintf(cpuset_filename, CPUSET_FILENAME_LEN, "%s%s/cpuset.%s", cgroup_mntpnt, cpuset_name, attr_name);
    hwloc_debug("Trying to read cgroup file <%s>\n", cpuset_filename);
  } else if (cpuset_mntpnt) {
    /* try to read the cpuset directly */
    snprintf(cpuset_filename, CPUSET_FILENAME_LEN, "%s%s/%s", cpuset_mntpnt, cpuset_name, attr_name);
    hwloc_debug("Trying to read cpuset file <%s>\n", cpuset_filename);
  }

  fd = hwloc_open(cpuset_filename, root_fd);
  if (fd < 0) {
    /* found no cpuset description, ignore it */
    hwloc_debug("Couldn't find cpuset <%s> description, ignoring\n", cpuset_name);
    return;
  }

  err = hwloc__read_fd_as_cpulist(fd, admin_enabled_cpus_set);
  close(fd);

  if (err < 0)
    hwloc_bitmap_fill(admin_enabled_cpus_set);
  else
    hwloc_debug_bitmap("cpuset includes %s\n", admin_enabled_cpus_set);
}

static void
hwloc_parse_meminfo_info(struct hwloc_linux_backend_data_s *data,
			 const char *path,
			 uint64_t *local_memory,
			 uint64_t *meminfo_hugepages_count,
			 uint64_t *meminfo_hugepages_size,
			 int onlytotal)
{
  char *tmp;
  char buffer[4096];
  unsigned long long number;

  if (hwloc_read_path_by_length(path, buffer, sizeof(buffer), data->root_fd) < 0)
    return;

  tmp = strstr(buffer, "MemTotal: "); /* MemTotal: %llu kB */
  if (tmp) {
    number = strtoull(tmp+10, NULL, 10);
    *local_memory = number << 10;

    if (onlytotal)
      return;

    tmp = strstr(tmp, "Hugepagesize: "); /* Hugepagesize: %llu */
    if (tmp) {
      number = strtoull(tmp+14, NULL, 10);
      *meminfo_hugepages_size = number << 10;

      tmp = strstr(tmp, "HugePages_Free: "); /* HugePages_Free: %llu */
      if (tmp) {
	number = strtoull(tmp+16, NULL, 10);
	*meminfo_hugepages_count = number;
      }
    }
  }
}

#define SYSFS_NUMA_NODE_PATH_LEN 128

static void
hwloc_parse_hugepages_info(struct hwloc_linux_backend_data_s *data,
			   const char *dirpath,
			   struct hwloc_obj_memory_s *memory,
			   uint64_t *remaining_local_memory)
{
  DIR *dir;
  struct dirent *dirent;
  unsigned long index_ = 1;
  char line[64];
  char path[SYSFS_NUMA_NODE_PATH_LEN];

  dir = hwloc_opendir(dirpath, data->root_fd);
  if (dir) {
    while ((dirent = readdir(dir)) != NULL) {
      if (strncmp(dirent->d_name, "hugepages-", 10))
        continue;
      memory->page_types[index_].size = strtoul(dirent->d_name+10, NULL, 0) * 1024ULL;
      sprintf(path, "%s/%s/nr_hugepages", dirpath, dirent->d_name);
      if (!hwloc_read_path_by_length(path, line, sizeof(line), data->root_fd)) {
	/* these are the actual total amount of huge pages */
	memory->page_types[index_].count = strtoull(line, NULL, 0);
	*remaining_local_memory -= memory->page_types[index_].count * memory->page_types[index_].size;
	index_++;
      }
    }
    closedir(dir);
    memory->page_types_len = index_;
  }
}

static void
hwloc_get_kerrighed_node_meminfo_info(struct hwloc_topology *topology,
				      struct hwloc_linux_backend_data_s *data,
				      unsigned long node, struct hwloc_obj_memory_s *memory)
{
  char path[128];
  uint64_t meminfo_hugepages_count, meminfo_hugepages_size = 0;

  if (topology->is_thissystem) {
    memory->page_types_len = 2;
    memory->page_types = malloc(2*sizeof(*memory->page_types));
    memset(memory->page_types, 0, 2*sizeof(*memory->page_types));
    /* Try to get the hugepage size from sysconf in case we fail to get it from /proc/meminfo later */
#ifdef HAVE__SC_LARGE_PAGESIZE
    memory->page_types[1].size = sysconf(_SC_LARGE_PAGESIZE);
#endif
    memory->page_types[0].size = data->pagesize;
  }

  snprintf(path, sizeof(path), "/proc/nodes/node%lu/meminfo", node);
  hwloc_parse_meminfo_info(data, path,
			   &memory->local_memory,
			   &meminfo_hugepages_count, &meminfo_hugepages_size,
			   memory->page_types == NULL);

  if (memory->page_types) {
    uint64_t remaining_local_memory = memory->local_memory;
    if (meminfo_hugepages_size) {
      memory->page_types[1].size = meminfo_hugepages_size;
      memory->page_types[1].count = meminfo_hugepages_count;
      remaining_local_memory -= meminfo_hugepages_count * meminfo_hugepages_size;
    } else {
      memory->page_types_len = 1;
    }
    memory->page_types[0].count = remaining_local_memory / memory->page_types[0].size;
  }
}

static void
hwloc_get_procfs_meminfo_info(struct hwloc_topology *topology,
			      struct hwloc_linux_backend_data_s *data,
			      struct hwloc_obj_memory_s *memory)
{
  uint64_t meminfo_hugepages_count, meminfo_hugepages_size = 0;
  struct stat st;
  int has_sysfs_hugepages = 0;
  const char *pagesize_env = getenv("HWLOC_DEBUG_PAGESIZE");
  int types = 2;
  int err;

  err = hwloc_stat("/sys/kernel/mm/hugepages", &st, data->root_fd);
  if (!err) {
    types = 1 + st.st_nlink-2;
    has_sysfs_hugepages = 1;
  }

  if (topology->is_thissystem || pagesize_env) {
    /* we cannot report any page_type info unless we have the page size.
     * we'll take it either from the system if local, or from the debug env variable
     */
    memory->page_types_len = types;
    memory->page_types = calloc(types, sizeof(*memory->page_types));
  }

  if (topology->is_thissystem) {
    /* Get the page and hugepage sizes from sysconf */
#if HAVE_DECL__SC_LARGE_PAGESIZE
    memory->page_types[1].size = sysconf(_SC_LARGE_PAGESIZE);
#endif
    memory->page_types[0].size = data->pagesize; /* might be overwritten later by /proc/meminfo or sysfs */
  }

  hwloc_parse_meminfo_info(data, "/proc/meminfo",
			   &memory->local_memory,
			   &meminfo_hugepages_count, &meminfo_hugepages_size,
			   memory->page_types == NULL);

  if (memory->page_types) {
    uint64_t remaining_local_memory = memory->local_memory;
    if (has_sysfs_hugepages) {
      /* read from node%d/hugepages/hugepages-%skB/nr_hugepages */
      hwloc_parse_hugepages_info(data, "/sys/kernel/mm/hugepages", memory, &remaining_local_memory);
    } else {
      /* use what we found in meminfo */
      if (meminfo_hugepages_size) {
        memory->page_types[1].size = meminfo_hugepages_size;
        memory->page_types[1].count = meminfo_hugepages_count;
        remaining_local_memory -= meminfo_hugepages_count * meminfo_hugepages_size;
      } else {
        memory->page_types_len = 1;
      }
    }

    if (pagesize_env) {
      /* We cannot get the pagesize if not thissystem, use the env-given one to experience the code during make check */
      memory->page_types[0].size = strtoull(pagesize_env, NULL, 10);
      /* If failed, use 4kB */
      if (!memory->page_types[0].size)
	memory->page_types[0].size = 4096;
    }
    assert(memory->page_types[0].size); /* from sysconf if local or from the env */
    /* memory->page_types[1].size from sysconf if local, or from /proc/meminfo, or from sysfs,
     * may be 0 if no hugepage support in the kernel */

    memory->page_types[0].count = remaining_local_memory / memory->page_types[0].size;
  }
}

static void
hwloc_sysfs_node_meminfo_info(struct hwloc_topology *topology,
			      struct hwloc_linux_backend_data_s *data,
			      const char *syspath, int node,
			      struct hwloc_obj_memory_s *memory)
{
  char path[SYSFS_NUMA_NODE_PATH_LEN];
  char meminfopath[SYSFS_NUMA_NODE_PATH_LEN];
  uint64_t meminfo_hugepages_count = 0;
  uint64_t meminfo_hugepages_size = 0;
  struct stat st;
  int has_sysfs_hugepages = 0;
  int types = 2;
  int err;

  sprintf(path, "%s/node%d/hugepages", syspath, node);
  err = hwloc_stat(path, &st, data->root_fd);
  if (!err) {
    types = 1 + st.st_nlink-2;
    has_sysfs_hugepages = 1;
  }

  if (topology->is_thissystem) {
    memory->page_types_len = types;
    memory->page_types = malloc(types*sizeof(*memory->page_types));
    memset(memory->page_types, 0, types*sizeof(*memory->page_types));
  }

  sprintf(meminfopath, "%s/node%d/meminfo", syspath, node);
  hwloc_parse_meminfo_info(data, meminfopath,
			   &memory->local_memory,
			   &meminfo_hugepages_count, NULL /* no hugepage size in node-specific meminfo */,
			   memory->page_types == NULL);

  if (memory->page_types) {
    uint64_t remaining_local_memory = memory->local_memory;
    if (has_sysfs_hugepages) {
      /* read from node%d/hugepages/hugepages-%skB/nr_hugepages */
      hwloc_parse_hugepages_info(data, path, memory, &remaining_local_memory);
    } else {
      /* get hugepage size from machine-specific meminfo since there is no size in node-specific meminfo,
       * hwloc_get_procfs_meminfo_info must have been called earlier */
      meminfo_hugepages_size = topology->levels[0][0]->memory.page_types[1].size;
      /* use what we found in meminfo */
      if (meminfo_hugepages_size) {
        memory->page_types[1].count = meminfo_hugepages_count;
        memory->page_types[1].size = meminfo_hugepages_size;
        remaining_local_memory -= meminfo_hugepages_count * meminfo_hugepages_size;
      } else {
        memory->page_types_len = 1;
      }
    }
    /* update what's remaining as normal pages */
    memory->page_types[0].size = data->pagesize;
    memory->page_types[0].count = remaining_local_memory / memory->page_types[0].size;
  }
}

static int
hwloc_parse_nodes_distances(const char *path, unsigned nbnodes, unsigned *indexes, float *distances, int fsroot_fd)
{
  size_t len = (10+1)*nbnodes;
  float *curdist = distances;
  char *string;
  unsigned i;

  string = malloc(len); /* space-separated %d */
  if (!string)
    goto out;

  for(i=0; i<nbnodes; i++) {
    unsigned osnode = indexes[i];
    char distancepath[SYSFS_NUMA_NODE_PATH_LEN];
    char *tmp, *next;
    unsigned found;

    /* Linux nodeX/distance file contains distance from X to other localities (from ACPI SLIT table or so),
     * store them in slots X*N...X*N+N-1 */
    sprintf(distancepath, "%s/node%u/distance", path, osnode);
    if (hwloc_read_path_by_length(distancepath, string, len, fsroot_fd) < 0)
      goto out_with_string;

    tmp = string;
    found = 0;
    while (tmp) {
      unsigned distance = strtoul(tmp, &next, 0); /* stored as a %d */
      if (next == tmp)
	break;
      *curdist = (float) distance;
      curdist++;
      found++;
      if (found == nbnodes)
	break;
      tmp = next+1;
    }
    if (found != nbnodes)
      goto out_with_string;
  }

  free(string);
  return 0;

 out_with_string:
  free(string);
 out:
  return -1;
}

static void
hwloc__get_dmi_id_one_info(struct hwloc_linux_backend_data_s *data,
			   hwloc_obj_t obj,
			   char *path, unsigned pathlen,
			   const char *dmi_name, const char *hwloc_name)
{
  char dmi_line[64];

  strcpy(path+pathlen, dmi_name);
  if (hwloc_read_path_by_length(path, dmi_line, sizeof(dmi_line), data->root_fd) < 0)
    return;

  if (dmi_line[0] != '\0') {
    char *tmp = strchr(dmi_line, '\n');
    if (tmp)
      *tmp = '\0';
    hwloc_debug("found %s '%s'\n", hwloc_name, dmi_line);
    hwloc_obj_add_info(obj, hwloc_name, dmi_line);
  }
}

static void
hwloc__get_dmi_id_info(struct hwloc_linux_backend_data_s *data, hwloc_obj_t obj)
{
  char path[128];
  unsigned pathlen;
  DIR *dir;

  strcpy(path, "/sys/devices/virtual/dmi/id");
  dir = hwloc_opendir(path, data->root_fd);
  if (dir) {
    pathlen = 27;
  } else {
    strcpy(path, "/sys/class/dmi/id");
    dir = hwloc_opendir(path, data->root_fd);
    if (dir)
      pathlen = 17;
    else
      return;
  }
  closedir(dir);

  path[pathlen++] = '/';

  hwloc__get_dmi_id_one_info(data, obj, path, pathlen, "product_name", "DMIProductName");
  hwloc__get_dmi_id_one_info(data, obj, path, pathlen, "product_version", "DMIProductVersion");
  hwloc__get_dmi_id_one_info(data, obj, path, pathlen, "product_serial", "DMIProductSerial");
  hwloc__get_dmi_id_one_info(data, obj, path, pathlen, "product_uuid", "DMIProductUUID");
  hwloc__get_dmi_id_one_info(data, obj, path, pathlen, "board_vendor", "DMIBoardVendor");
  hwloc__get_dmi_id_one_info(data, obj, path, pathlen, "board_name", "DMIBoardName");
  hwloc__get_dmi_id_one_info(data, obj, path, pathlen, "board_version", "DMIBoardVersion");
  hwloc__get_dmi_id_one_info(data, obj, path, pathlen, "board_serial", "DMIBoardSerial");
  hwloc__get_dmi_id_one_info(data, obj, path, pathlen, "board_asset_tag", "DMIBoardAssetTag");
  hwloc__get_dmi_id_one_info(data, obj, path, pathlen, "chassis_vendor", "DMIChassisVendor");
  hwloc__get_dmi_id_one_info(data, obj, path, pathlen, "chassis_type", "DMIChassisType");
  hwloc__get_dmi_id_one_info(data, obj, path, pathlen, "chassis_version", "DMIChassisVersion");
  hwloc__get_dmi_id_one_info(data, obj, path, pathlen, "chassis_serial", "DMIChassisSerial");
  hwloc__get_dmi_id_one_info(data, obj, path, pathlen, "chassis_asset_tag", "DMIChassisAssetTag");
  hwloc__get_dmi_id_one_info(data, obj, path, pathlen, "bios_vendor", "DMIBIOSVendor");
  hwloc__get_dmi_id_one_info(data, obj, path, pathlen, "bios_version", "DMIBIOSVersion");
  hwloc__get_dmi_id_one_info(data, obj, path, pathlen, "bios_date", "DMIBIOSDate");
  hwloc__get_dmi_id_one_info(data, obj, path, pathlen, "sys_vendor", "DMISysVendor");
}

struct hwloc_firmware_dmi_mem_device_header {
  unsigned char type;
  unsigned char length;
  unsigned char handle[2];
  unsigned char phy_mem_handle[2];
  unsigned char mem_err_handle[2];
  unsigned char tot_width[2];
  unsigned char dat_width[2];
  unsigned char size[2];
  unsigned char ff;
  unsigned char dev_set;
  unsigned char dev_loc_str_num;
  unsigned char bank_loc_str_num;
  unsigned char mem_type;
  unsigned char type_detail[2];
  unsigned char speed[2];
  unsigned char manuf_str_num;
  unsigned char serial_str_num;
  unsigned char asset_tag_str_num;
  unsigned char part_num_str_num;
  /* don't include the following fields since we don't need them,
   * some old implementations may miss them.
   */
};

static int check_dmi_entry(const char *buffer)
{
  /* reject empty strings */
  if (!*buffer)
    return 0;
  /* reject strings of spaces (at least Dell use this for empty memory slots) */
  if (strspn(buffer, " ") == strlen(buffer))
    return 0;
  return 1;
}

static void
hwloc__get_firmware_dmi_memory_info_one(struct hwloc_topology *topology,
					unsigned idx, const char *path, FILE *fd,
					struct hwloc_firmware_dmi_mem_device_header *header)
{
  unsigned slen;
  char buffer[256]; /* enough for memory device strings, or at least for each of them */
  unsigned foff; /* offset in raw file */
  unsigned boff; /* offset in buffer read from raw file */
  unsigned i;
  struct hwloc_obj_info_s *infos = NULL;
  unsigned infos_count = 0;
  hwloc_obj_t misc;
  int foundinfo = 0;

  hwloc__add_info(&infos, &infos_count, "Type", "MemoryModule");

  /* start after the header */
  foff = header->length;
  i = 1;
  while (1) {
    /* read one buffer */
    if (fseek(fd, foff, SEEK_SET) < 0)
      break;
    if (!fgets(buffer, sizeof(buffer), fd))
      break;
    /* read string at the beginning of the buffer */
    boff = 0;
    while (1) {
      /* stop on empty string */
      if (!buffer[boff])
        goto done;
      /* stop if this string goes to the end of the buffer */
      slen = strlen(buffer+boff);
      if (boff + slen+1 == sizeof(buffer))
        break;
      /* string didn't get truncated, should be OK */
      if (i == header->manuf_str_num) {
	if (check_dmi_entry(buffer+boff)) {
	  hwloc__add_info(&infos, &infos_count, "Vendor", buffer+boff);
	  foundinfo = 1;
	}
      }	else if (i == header->serial_str_num) {
	if (check_dmi_entry(buffer+boff)) {
	  hwloc__add_info(&infos, &infos_count, "SerialNumber", buffer+boff);
	  foundinfo = 1;
	}
      } else if (i == header->asset_tag_str_num) {
	if (check_dmi_entry(buffer+boff)) {
	  hwloc__add_info(&infos, &infos_count, "AssetTag", buffer+boff);
	  foundinfo = 1;
	}
      } else if (i == header->part_num_str_num) {
	if (check_dmi_entry(buffer+boff)) {
	  hwloc__add_info(&infos, &infos_count, "PartNumber", buffer+boff);
	  foundinfo = 1;
	}
      } else if (i == header->dev_loc_str_num) {
	if (check_dmi_entry(buffer+boff)) {
	  hwloc__add_info(&infos, &infos_count, "DeviceLocation", buffer+boff);
	  /* only a location, not an actual info about the device */
	}
      } else if (i == header->bank_loc_str_num) {
	if (check_dmi_entry(buffer+boff)) {
	  hwloc__add_info(&infos, &infos_count, "BankLocation", buffer+boff);
	  /* only a location, not an actual info about the device */
	}
      } else {
	goto done;
      }
      /* next string in buffer */
      boff += slen+1;
      i++;
    }
    /* couldn't read a single full string from that buffer, we're screwed */
    if (!boff) {
      fprintf(stderr, "hwloc could read a DMI firmware entry #%u in %s\n",
	      i, path);
      break;
    }
    /* reread buffer after previous string */
    foff += boff;
  }

done:
  if (!foundinfo) {
    /* found no actual info about the device. if there's only location info, the slot may be empty */
    goto out_with_infos;
  }

  misc = hwloc_alloc_setup_object(HWLOC_OBJ_MISC, idx);
  if (!misc)
    goto out_with_infos;

  hwloc__move_infos(&misc->infos, &misc->infos_count, &infos, &infos_count);
  /* FIXME: find a way to identify the corresponding NUMA node and attach these objects there.
   * but it means we need to parse DeviceLocation=DIMM_B4 but these vary significantly
   * with the vendor, and it's hard to be 100% sure 'B' is second socket.
   * Examples at http://sourceforge.net/p/edac-utils/code/HEAD/tree/trunk/src/etc/labels.db
   * or https://github.com/grondo/edac-utils/blob/master/src/etc/labels.db
   */
  hwloc_insert_object_by_parent(topology, hwloc_get_root_obj(topology), misc);
  return;

 out_with_infos:
  hwloc__free_infos(infos, infos_count);
}

static void
hwloc__get_firmware_dmi_memory_info(struct hwloc_topology *topology,
				    struct hwloc_linux_backend_data_s *data)
{
  char path[128];
  unsigned i;

  for(i=0; ; i++) {
    FILE *fd;
    struct hwloc_firmware_dmi_mem_device_header header;
    int err;

    snprintf(path, sizeof(path), "/sys/firmware/dmi/entries/17-%u/raw", i);
    fd = hwloc_fopen(path, "r", data->root_fd);
    if (!fd)
      break;

    err = fread(&header, sizeof(header), 1, fd);
    if (err != 1) {
      fclose(fd);
      break;
    }
    if (header.length < sizeof(header)) {
      /* invalid, or too old entry/spec that doesn't contain what we need */
      fclose(fd);
      break;
    }

    hwloc__get_firmware_dmi_memory_info_one(topology, i, path, fd, &header);

    fclose(fd);
  }
}


/***********************************
 ****** Device tree Discovery ******
 ***********************************/

/* Reads the entire file and returns bytes read if bytes_read != NULL
 * Returned pointer can be freed by using free().  */
static void *
hwloc_read_raw(const char *p, const char *p1, size_t *bytes_read, int root_fd)
{
  char fname[256];
  char *ret = NULL;
  struct stat fs;
  int file = -1;

  snprintf(fname, sizeof(fname), "%s/%s", p, p1);

  file = hwloc_open(fname, root_fd);
  if (-1 == file) {
      goto out_no_close;
  }
  if (fstat(file, &fs)) {
    goto out;
  }

  ret = (char *) malloc(fs.st_size);
  if (NULL != ret) {
    ssize_t cb = read(file, ret, fs.st_size);
    if (cb == -1) {
      free(ret);
      ret = NULL;
    } else {
      if (NULL != bytes_read)
        *bytes_read = cb;
    }
  }

 out:
  close(file);
 out_no_close:
  return ret;
}

/* Reads the entire file and returns it as a 0-terminated string
 * Returned pointer can be freed by using free().  */
static char *
hwloc_read_str(const char *p, const char *p1, int root_fd)
{
  size_t cb = 0;
  char *ret = hwloc_read_raw(p, p1, &cb, root_fd);
  if ((NULL != ret) && (0 < cb) && (0 != ret[cb-1])) {
    char *tmp = realloc(ret, cb + 1);
    if (!tmp) {
      free(ret);
      return NULL;
    }
    ret = tmp;
    ret[cb] = 0;
  }
  return ret;
}

/* Reads first 32bit bigendian value */
static ssize_t
hwloc_read_unit32be(const char *p, const char *p1, uint32_t *buf, int root_fd)
{
  size_t cb = 0;
  uint32_t *tmp = hwloc_read_raw(p, p1, &cb, root_fd);
  if (sizeof(*buf) != cb) {
    errno = EINVAL;
    free(tmp); /* tmp is either NULL or contains useless things */
    return -1;
  }
  *buf = htonl(*tmp);
  free(tmp);
  return sizeof(*buf);
}

typedef struct {
  unsigned int n, allocated;
  struct {
    hwloc_bitmap_t cpuset;
    uint32_t phandle;
    uint32_t l2_cache;
    char *name;
  } *p;
} device_tree_cpus_t;

static void
add_device_tree_cpus_node(device_tree_cpus_t *cpus, hwloc_bitmap_t cpuset,
    uint32_t l2_cache, uint32_t phandle, const char *name)
{
  if (cpus->n == cpus->allocated) {
    void *tmp;
    unsigned allocated;
    if (!cpus->allocated)
      allocated = 64;
    else
      allocated = 2 * cpus->allocated;
    tmp = realloc(cpus->p, allocated * sizeof(cpus->p[0]));
    if (!tmp)
      return; /* failed to realloc, ignore this entry */
    cpus->p = tmp;
    cpus->allocated = allocated;
  }
  cpus->p[cpus->n].phandle = phandle;
  cpus->p[cpus->n].cpuset = (NULL == cpuset)?NULL:hwloc_bitmap_dup(cpuset);
  cpus->p[cpus->n].l2_cache = l2_cache;
  cpus->p[cpus->n].name = strdup(name);
  ++cpus->n;
}

/* Walks over the cache list in order to detect nested caches and CPU mask for each */
static int
look_powerpc_device_tree_discover_cache(device_tree_cpus_t *cpus,
    uint32_t phandle, unsigned int *level, hwloc_bitmap_t cpuset)
{
  unsigned int i;
  int ret = -1;
  if ((NULL == level) || (NULL == cpuset) || phandle == (uint32_t) -1)
    return ret;
  for (i = 0; i < cpus->n; ++i) {
    if (phandle != cpus->p[i].l2_cache)
      continue;
    if (NULL != cpus->p[i].cpuset) {
      hwloc_bitmap_or(cpuset, cpuset, cpus->p[i].cpuset);
      ret = 0;
    } else {
      ++(*level);
      if (0 == look_powerpc_device_tree_discover_cache(cpus,
            cpus->p[i].phandle, level, cpuset))
        ret = 0;
    }
  }
  return ret;
}

static void
try__add_cache_from_device_tree_cpu(struct hwloc_topology *topology,
				    unsigned int level, hwloc_obj_cache_type_t type,
				    uint32_t cache_line_size, uint32_t cache_size, uint32_t cache_sets,
				    hwloc_bitmap_t cpuset)
{
  struct hwloc_obj *c = NULL;

  if (0 == cache_size)
    return;

  c = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, -1);
  c->attr->cache.depth = level;
  c->attr->cache.linesize = cache_line_size;
  c->attr->cache.size = cache_size;
  c->attr->cache.type = type;
  if (cache_sets == 1)
    /* likely wrong, make it unknown */
    cache_sets = 0;
  if (cache_sets && cache_line_size)
    c->attr->cache.associativity = cache_size / (cache_sets * cache_line_size);
  else
    c->attr->cache.associativity = 0;
  c->cpuset = hwloc_bitmap_dup(cpuset);
  hwloc_debug_2args_bitmap("cache (%s) depth %d has cpuset %s\n",
			   type == HWLOC_OBJ_CACHE_UNIFIED ? "unified" : (type == HWLOC_OBJ_CACHE_DATA ? "data" : "instruction"),
			   level, c->cpuset);
  hwloc_insert_object_by_cpuset(topology, c);
}

static void
try_add_cache_from_device_tree_cpu(struct hwloc_topology *topology,
				   struct hwloc_linux_backend_data_s *data,
				   const char *cpu, unsigned int level, hwloc_bitmap_t cpuset)
{
  /* d-cache-block-size - ignore */
  /* d-cache-line-size - to read, in bytes */
  /* d-cache-sets - ignore */
  /* d-cache-size - to read, in bytes */
  /* i-cache, same for instruction */
  /* cache-unified only exist if data and instruction caches are unified */
  /* d-tlb-sets - ignore */
  /* d-tlb-size - ignore, always 0 on power6 */
  /* i-tlb-*, same */
  uint32_t d_cache_line_size = 0, d_cache_size = 0, d_cache_sets = 0;
  uint32_t i_cache_line_size = 0, i_cache_size = 0, i_cache_sets = 0;
  char unified_path[1024];
  struct stat statbuf;
  int unified;

  snprintf(unified_path, sizeof(unified_path), "%s/cache-unified", cpu);
  unified = (hwloc_stat(unified_path, &statbuf, data->root_fd) == 0);

  hwloc_read_unit32be(cpu, "d-cache-line-size", &d_cache_line_size,
      data->root_fd);
  hwloc_read_unit32be(cpu, "d-cache-size", &d_cache_size,
      data->root_fd);
  hwloc_read_unit32be(cpu, "d-cache-sets", &d_cache_sets,
      data->root_fd);
  hwloc_read_unit32be(cpu, "i-cache-line-size", &i_cache_line_size,
      data->root_fd);
  hwloc_read_unit32be(cpu, "i-cache-size", &i_cache_size,
      data->root_fd);
  hwloc_read_unit32be(cpu, "i-cache-sets", &i_cache_sets,
      data->root_fd);

  if (!unified)
    try__add_cache_from_device_tree_cpu(topology, level, HWLOC_OBJ_CACHE_INSTRUCTION,
					i_cache_line_size, i_cache_size, i_cache_sets, cpuset);
  try__add_cache_from_device_tree_cpu(topology, level, unified ? HWLOC_OBJ_CACHE_UNIFIED : HWLOC_OBJ_CACHE_DATA,
				      d_cache_line_size, d_cache_size, d_cache_sets, cpuset);
}

/*
 * Discovers L1/L2/L3 cache information on IBM PowerPC systems for old kernels (RHEL5.*)
 * which provide NUMA nodes information without any details
 */
static void
look_powerpc_device_tree(struct hwloc_topology *topology,
			 struct hwloc_linux_backend_data_s *data)
{
  device_tree_cpus_t cpus;
  const char ofroot[] = "/proc/device-tree/cpus";
  unsigned int i;
  int root_fd = data->root_fd;
  DIR *dt = hwloc_opendir(ofroot, root_fd);
  struct dirent *dirent;

  if (NULL == dt)
    return;

  /* only works for Power so far, and not useful on ARM */
  if (data->arch != HWLOC_LINUX_ARCH_POWER)
    return;

  cpus.n = 0;
  cpus.p = NULL;
  cpus.allocated = 0;

  while (NULL != (dirent = readdir(dt))) {
    char cpu[256];
    char *device_type;
    uint32_t reg = -1, l2_cache = -1, phandle = -1;

    if ('.' == dirent->d_name[0])
      continue;

    snprintf(cpu, sizeof(cpu), "%s/%s", ofroot, dirent->d_name);

    device_type = hwloc_read_str(cpu, "device_type", root_fd);
    if (NULL == device_type)
      continue;

    hwloc_read_unit32be(cpu, "reg", &reg, root_fd);
    if (hwloc_read_unit32be(cpu, "next-level-cache", &l2_cache, root_fd) == -1)
      hwloc_read_unit32be(cpu, "l2-cache", &l2_cache, root_fd);
    if (hwloc_read_unit32be(cpu, "phandle", &phandle, root_fd) == -1)
      if (hwloc_read_unit32be(cpu, "ibm,phandle", &phandle, root_fd) == -1)
        hwloc_read_unit32be(cpu, "linux,phandle", &phandle, root_fd);

    if (0 == strcmp(device_type, "cache")) {
      add_device_tree_cpus_node(&cpus, NULL, l2_cache, phandle, dirent->d_name);
    }
    else if (0 == strcmp(device_type, "cpu")) {
      /* Found CPU */
      hwloc_bitmap_t cpuset = NULL;
      size_t cb = 0;
      uint32_t *threads = hwloc_read_raw(cpu, "ibm,ppc-interrupt-server#s", &cb, root_fd);
      uint32_t nthreads = cb / sizeof(threads[0]);

      if (NULL != threads) {
        cpuset = hwloc_bitmap_alloc();
        for (i = 0; i < nthreads; ++i) {
          if (hwloc_bitmap_isset(topology->levels[0][0]->complete_cpuset, ntohl(threads[i])))
            hwloc_bitmap_set(cpuset, ntohl(threads[i]));
        }
        free(threads);
      } else if ((unsigned int)-1 != reg) {
        /* Doesn't work on ARM because cpu "reg" do not start at 0.
	 * We know the first cpu "reg" is the lowest. The others are likely
	 * in order assuming the device-tree shows objects in order.
	 */
        cpuset = hwloc_bitmap_alloc();
        hwloc_bitmap_set(cpuset, reg);
      }

      if (NULL == cpuset) {
        hwloc_debug("%s has no \"reg\" property, skipping\n", cpu);
      } else {
        struct hwloc_obj *core = NULL;
        add_device_tree_cpus_node(&cpus, cpuset, l2_cache, phandle, dirent->d_name);

        /* Add core */
        core = hwloc_alloc_setup_object(HWLOC_OBJ_CORE, reg);
        core->cpuset = hwloc_bitmap_dup(cpuset);
        hwloc_insert_object_by_cpuset(topology, core);

        /* Add L1 cache */
        try_add_cache_from_device_tree_cpu(topology, data, cpu, 1, cpuset);

        hwloc_bitmap_free(cpuset);
      }
    }
    free(device_type);
  }
  closedir(dt);

  /* No cores and L2 cache were found, exiting */
  if (0 == cpus.n) {
    hwloc_debug("No cores and L2 cache were found in %s, exiting\n", ofroot);
    return;
  }

#ifdef HWLOC_DEBUG
  for (i = 0; i < cpus.n; ++i) {
    hwloc_debug("%i: %s  ibm,phandle=%08X l2_cache=%08X ",
      i, cpus.p[i].name, cpus.p[i].phandle, cpus.p[i].l2_cache);
    if (NULL == cpus.p[i].cpuset) {
      hwloc_debug("%s\n", "no cpuset");
    } else {
      hwloc_debug_bitmap("cpuset %s\n", cpus.p[i].cpuset);
    }
  }
#endif

  /* Scan L2/L3/... caches */
  for (i = 0; i < cpus.n; ++i) {
    unsigned int level = 2;
    hwloc_bitmap_t cpuset;
    /* Skip real CPUs */
    if (NULL != cpus.p[i].cpuset)
      continue;

    /* Calculate cache level and CPU mask */
    cpuset = hwloc_bitmap_alloc();
    if (0 == look_powerpc_device_tree_discover_cache(&cpus,
          cpus.p[i].phandle, &level, cpuset)) {
      char cpu[256];
      snprintf(cpu, sizeof(cpu), "%s/%s", ofroot, cpus.p[i].name);
      try_add_cache_from_device_tree_cpu(topology, data, cpu, level, cpuset);
    }
    hwloc_bitmap_free(cpuset);
  }

  /* Do cleanup */
  for (i = 0; i < cpus.n; ++i) {
    hwloc_bitmap_free(cpus.p[i].cpuset);
    free(cpus.p[i].name);
  }
  free(cpus.p);
}

/* Try to handle knl hwdata properties
 * Returns 0 on success and -1 otherwise */
static int hwloc_linux_try_handle_knl_hwdata_properties(hwloc_topology_t topology, struct hwloc_linux_backend_data_s *data, hwloc_obj_t *nodes, unsigned nbnodes)
{
  char *knl_cache_file;
  long long int cache_size = -1;
  int associativity = -1;
  int inclusiveness = -1;
  int line_size = -1;
  int version = 0;
  unsigned i;
  char buffer[512] = {0};
  char *data_beg = NULL;
  char memory_mode_str[32] = {0};
  char cluster_mode_str[32] = {0};

  if (asprintf(&knl_cache_file, "%s/knl_memoryside_cache", data->dumped_hwdata_dirname) < 0)
    return -1;

  hwloc_debug("Reading knl cache data from: %s\n", knl_cache_file);
  if (hwloc_read_path_by_length(knl_cache_file, buffer, sizeof(buffer), data->root_fd) < 0) {
    hwloc_debug("Unable to open KNL data file `%s' (%s)\n", knl_cache_file, strerror(errno));
    free(knl_cache_file);
    return -1;
  }
  free(knl_cache_file);

  data_beg = &buffer[0];

  /* file must start with version information */
  if (sscanf(data_beg, "version: %d", &version) != 1) {
    fprintf(stderr, "Invalid knl_memoryside_cache header, expected \"version: <int>\".\n");
    return -1;
  }

  while (1) {
    char *line_end = strstr(data_beg, "\n");
    if (!line_end)
        break;
    if (version >= 1) {
      if (!strncmp("cache_size:", data_beg, strlen("cache_size"))) {
          sscanf(data_beg, "cache_size: %lld", &cache_size);
          hwloc_debug("read cache_size=%lld\n", cache_size);
      } else if (!strncmp("line_size:", data_beg, strlen("line_size:"))) {
          sscanf(data_beg, "line_size: %d", &line_size);
          hwloc_debug("read line_size=%d\n", line_size);
      } else if (!strncmp("inclusiveness:", data_beg, strlen("inclusiveness:"))) {
          sscanf(data_beg, "inclusiveness: %d", &inclusiveness);
          hwloc_debug("read inclusiveness=%d\n", inclusiveness);
      } else if (!strncmp("associativity:", data_beg, strlen("associativity:"))) {
          sscanf(data_beg, "associativity: %d\n", &associativity);
          hwloc_debug("read associativity=%d\n", associativity);
      }
    }
    if (version >= 2) {
      if (!strncmp("cluster_mode:", data_beg, strlen("cluster_mode:"))) {
        sscanf(data_beg, "cluster_mode: %s\n", cluster_mode_str);
        hwloc_debug("read cluster_mode=%s\n", cluster_mode_str);
      } else if (!strncmp("memory_mode:", data_beg, strlen("memory_mode:"))) {
        sscanf(data_beg, "memory_mode: %s\n", memory_mode_str);
        hwloc_debug("read memory_mode=%s\n", memory_mode_str);
      }
    }

    data_beg = line_end + 1;
  }

  if (line_size == -1 || cache_size == -1 || associativity == -1 || inclusiveness == -1) {
    hwloc_debug("Incorrect file format line_size=%d cache_size=%lld associativity=%d inclusiveness=%d\n",
            line_size, cache_size, associativity, inclusiveness);
    return -1;
  }

  /* In file version 1 mcdram_cache is always non-zero.
   * In file version 2 mcdram cache can be zero in flat mode. We need to check and do not expose cache in flat mode. */
  if (cache_size > 0) {
    for(i=0; i<nbnodes; i++) {
      hwloc_obj_t cache;

      if (hwloc_bitmap_iszero(nodes[i]->cpuset))
        /* one L3 per DDR, none for MCDRAM nodes */
        continue;

      cache = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, -1);
      if (!cache)
	return -1;

      cache->attr->cache.depth = 3;
      cache->attr->cache.type = HWLOC_OBJ_CACHE_UNIFIED;
      cache->attr->cache.associativity = associativity;
      hwloc_obj_add_info(cache, "Inclusive", inclusiveness ? "1" : "0");
      cache->attr->cache.size = cache_size;
      cache->attr->cache.linesize = line_size;
      cache->cpuset = hwloc_bitmap_dup(nodes[i]->cpuset);
      hwloc_obj_add_info(cache, "Type", "MemorySideCache");
      hwloc_insert_object_by_cpuset(topology, cache);
    }
  }
  /* adding cluster and memory mode as properties of the machine */
  if (version >= 2) {
    hwloc_obj_add_info(topology->levels[0][0], "ClusterMode", cluster_mode_str);
    hwloc_obj_add_info(topology->levels[0][0], "MemoryMode", memory_mode_str);
  }

  return 0;
}



/**************************************
 ****** Sysfs Topology Discovery ******
 **************************************/

static int
look_sysfsnode(struct hwloc_topology *topology,
	       struct hwloc_linux_backend_data_s *data,
	       const char *path, unsigned *found)
{
  unsigned osnode;
  unsigned nbnodes = 0;
  DIR *dir;
  struct dirent *dirent;
  hwloc_bitmap_t nodeset;

  *found = 0;

  /* Get the list of nodes first */
  dir = hwloc_opendir(path, data->root_fd);
  if (dir)
    {
      nodeset = hwloc_bitmap_alloc();
      while ((dirent = readdir(dir)) != NULL)
	{
	  if (strncmp(dirent->d_name, "node", 4))
	    continue;
	  osnode = strtoul(dirent->d_name+4, NULL, 0);
	  hwloc_bitmap_set(nodeset, osnode);
	  nbnodes++;
	}
      closedir(dir);
    }
  else
    return -1;

  if (!nbnodes || (nbnodes == 1 && !data->is_knl)) { /* always keep NUMA for KNL, or configs might look too different */
    hwloc_bitmap_free(nodeset);
    return 0;
  }

  /* For convenience, put these declarations inside a block. */

  {
      hwloc_obj_t * nodes = calloc(nbnodes, sizeof(hwloc_obj_t));
      unsigned *indexes = calloc(nbnodes, sizeof(unsigned));
      float * distances = NULL;
      int failednodes = 0;
      unsigned index_;

      if (NULL == nodes || NULL == indexes) {
          free(nodes);
          free(indexes);
          hwloc_bitmap_free(nodeset);
          nbnodes = 0;
          goto out;
      }

      /* Unsparsify node indexes.
       * We'll need them later because Linux groups sparse distances
       * and keeps them in order in the sysfs distance files.
       * It'll simplify things in the meantime.
       */
      index_ = 0;
      hwloc_bitmap_foreach_begin (osnode, nodeset) {
	indexes[index_] = osnode;
	index_++;
      } hwloc_bitmap_foreach_end();
      hwloc_bitmap_free(nodeset);

#ifdef HWLOC_DEBUG
      hwloc_debug("%s", "NUMA indexes: ");
      for (index_ = 0; index_ < nbnodes; index_++) {
	hwloc_debug(" %u", indexes[index_]);
      }
      hwloc_debug("%s", "\n");
#endif

      /* Create NUMA objects */
      for (index_ = 0; index_ < nbnodes; index_++) {
          char nodepath[SYSFS_NUMA_NODE_PATH_LEN];
          hwloc_bitmap_t cpuset;
          hwloc_obj_t node, res_obj;
	  int annotate;

	  osnode = indexes[index_];

          sprintf(nodepath, "%s/node%u/cpumap", path, osnode);
          cpuset = hwloc__alloc_read_path_as_cpumask(nodepath, data->root_fd);
          if (!cpuset) {
	    /* This NUMA object won't be inserted, we'll ignore distances */
	    failednodes++;
	    continue;
	  }

	  node = hwloc_get_numanode_obj_by_os_index(topology, osnode);
	  annotate = (node != NULL);
	  if (!annotate) {
	    /* create a new node */
	    node = hwloc_alloc_setup_object(HWLOC_OBJ_NUMANODE, osnode);
	    node->cpuset = cpuset;
	    node->nodeset = hwloc_bitmap_alloc();
	    hwloc_bitmap_set(node->nodeset, osnode);
	  }
          hwloc_sysfs_node_meminfo_info(topology, data, path, osnode, &node->memory);

          hwloc_debug_1arg_bitmap("os node %u has cpuset %s\n",
                                  osnode, node->cpuset);

	  if (annotate) {
	    nodes[index_] = node;
	  } else {
	    res_obj = hwloc_insert_object_by_cpuset(topology, node);
	    if (node == res_obj) {
	      nodes[index_] = node;
	    } else {
	      /* We got merged somehow, could be a buggy BIOS reporting wrong NUMA node cpuset.
	       * This object disappeared, we'll ignore distances */
	      failednodes++;
	    }
	  }
      }

      if (!failednodes && data->is_knl)
        hwloc_linux_try_handle_knl_hwdata_properties(topology, data, nodes, nbnodes);

      if (failednodes) {
	/* failed to read/create some nodes, don't bother reading/fixing
	 * a distance matrix that would likely be wrong anyway.
	 */
	nbnodes -= failednodes;
      } else if (nbnodes > 1) {
	distances = malloc(nbnodes*nbnodes*sizeof(*distances));
      }

      if (NULL == distances) {
          free(nodes);
          free(indexes);
          goto out;
      }

      if (hwloc_parse_nodes_distances(path, nbnodes, indexes, distances, data->root_fd) < 0) {
	free(nodes);
	free(distances);
	free(indexes);
	goto out;
      }

      if (data->is_knl && distances) {
	char *env = getenv("HWLOC_KNL_NUMA_QUIRK");
	if (!(env && !atoi(env)) && nbnodes>=2) { /* SNC2 or SNC4, with 0 or 2/4 MCDRAM, and 0-4 DDR nodes */
	  unsigned i, j, closest;
	  for(i=0; i<nbnodes; i++) {
	    if (!hwloc_bitmap_iszero(nodes[i]->cpuset))
	      /* nodes with CPU, that's DDR, skip it */
	      continue;
	    hwloc_obj_add_info(nodes[i], "Type", "MCDRAM");

	    /* DDR is the closest node with CPUs */
	    closest = (unsigned)-1;
	    for(j=0; j<nbnodes; j++) {
	      if (j==i)
		continue;
	      if (hwloc_bitmap_iszero(nodes[j]->cpuset))
		/* nodes without CPU, that's another MCDRAM, skip it */
		continue;
	      if (closest == (unsigned)-1 || distances[i*nbnodes+j]<distances[i*nbnodes+closest])
		closest = j;
	    }
	    if (closest != (unsigned) -1) {
	      /* Add a Group for Cluster containing this MCDRAM + DDR */
	      hwloc_obj_t cluster = hwloc_alloc_setup_object(HWLOC_OBJ_GROUP, -1);
	      cluster->cpuset = hwloc_bitmap_dup(nodes[i]->cpuset);
	      cluster->nodeset = hwloc_bitmap_dup(nodes[i]->nodeset);
	      hwloc_bitmap_or(cluster->cpuset, cluster->cpuset, nodes[closest]->cpuset);
	      hwloc_bitmap_or(cluster->nodeset, cluster->nodeset, nodes[closest]->nodeset);
	      hwloc_obj_add_info(cluster, "Type", "Cluster");
	      hwloc_insert_object_by_cpuset(topology, cluster);
	    }
          }
	  /* drop the distance matrix, it contradicts the above NUMA layout groups */
	  free(distances);
          free(nodes);
          free(indexes);
          goto out;
	}
      }

      hwloc_distances_set(topology, HWLOC_OBJ_NUMANODE, nbnodes, indexes, nodes, distances, 0 /* OS cannot force */);
  }

 out:
  *found = nbnodes;
  return 0;
}

/* Look at Linux' /sys/devices/system/cpu/cpu%d/topology/ */
static int
look_sysfscpu(struct hwloc_topology *topology,
	      struct hwloc_linux_backend_data_s *data,
	      const char *path,
	      struct hwloc_linux_cpuinfo_proc * cpuinfo_Lprocs, unsigned cpuinfo_numprocs)
{
  hwloc_bitmap_t cpuset; /* Set of cpus for which we have topology information */
#define CPU_TOPOLOGY_STR_LEN 128
  char str[CPU_TOPOLOGY_STR_LEN];
  DIR *dir;
  int i,j;
  unsigned caches_added, merge_buggy_core_siblings;
  hwloc_obj_t packages = NULL; /* temporary list of packages before actual insert in the tree */
  int threadwithcoreid = data->is_amd_with_CU ? -1 : 0; /* -1 means we don't know yet if threads have their own coreids within thread_siblings */

  /* fill the cpuset of interesting cpus */
  dir = hwloc_opendir(path, data->root_fd);
  if (!dir)
    return -1;
  else {
    struct dirent *dirent;
    cpuset = hwloc_bitmap_alloc();

    while ((dirent = readdir(dir)) != NULL) {
      unsigned long cpu;
      char online[2];

      if (strncmp(dirent->d_name, "cpu", 3))
	continue;
      cpu = strtoul(dirent->d_name+3, NULL, 0);

      /* Maybe we don't have topology information but at least it exists */
      hwloc_bitmap_set(topology->levels[0][0]->complete_cpuset, cpu);

      /* check whether this processor is online */
      sprintf(str, "%s/cpu%lu/online", path, cpu);
      if (hwloc_read_path_by_length(str, online, sizeof(online), data->root_fd) == 0) {
	if (atoi(online)) {
	  hwloc_debug("os proc %lu is online\n", cpu);
	} else {
	  hwloc_debug("os proc %lu is offline\n", cpu);
          hwloc_bitmap_clr(topology->levels[0][0]->online_cpuset, cpu);
	}
      }

      /* check whether the kernel exports topology information for this cpu */
      sprintf(str, "%s/cpu%lu/topology", path, cpu);
      if (hwloc_access(str, X_OK, data->root_fd) < 0 && errno == ENOENT) {
	hwloc_debug("os proc %lu has no accessible %s/cpu%lu/topology\n",
		   cpu, path, cpu);
	continue;
      }

      hwloc_bitmap_set(cpuset, cpu);
    }
    closedir(dir);
  }

  topology->support.discovery->pu = 1;
  hwloc_debug_1arg_bitmap("found %d cpu topologies, cpuset %s\n",
	     hwloc_bitmap_weight(cpuset), cpuset);

  merge_buggy_core_siblings = (data->arch == HWLOC_LINUX_ARCH_X86);
  caches_added = 0;
  hwloc_bitmap_foreach_begin(i, cpuset) {
    hwloc_bitmap_t packageset, coreset, bookset, threadset;
    unsigned mypackageid, mycoreid, mybookid;
    int tmpint;

    /* look at the package */
    sprintf(str, "%s/cpu%d/topology/core_siblings", path, i);
    packageset = hwloc__alloc_read_path_as_cpumask(str, data->root_fd);
    if (packageset && hwloc_bitmap_first(packageset) == i) {
      /* first cpu in this package, add the package */
      struct hwloc_obj *package;

      mypackageid = (unsigned) -1;
      sprintf(str, "%s/cpu%d/topology/physical_package_id", path, i); /* contains %d at least up to 4.9 */
      if (hwloc_read_path_as_int(str, &tmpint, data->root_fd) == 0)
	mypackageid = (unsigned) tmpint;

      if (merge_buggy_core_siblings) {
	/* check for another package with same physical_package_id */
	hwloc_obj_t curpackage = packages;
	while (curpackage) {
	  if (curpackage->os_index == mypackageid) {
	    /* found another package with same physical_package_id but different core_siblings.
	     * looks like a buggy kernel on Intel Xeon E5 v3 processor with two rings.
	     * merge these core_siblings to extend the existing first package object.
	     */
	    static int reported = 0;
	    if (!reported && !hwloc_hide_errors()) {
	      char *a, *b;
	      hwloc_bitmap_asprintf(&a, curpackage->cpuset);
	      hwloc_bitmap_asprintf(&b, packageset);
	      fprintf(stderr, "****************************************************************************\n");
	      fprintf(stderr, "* hwloc %s has detected buggy sysfs package information: Two packages have\n", HWLOC_VERSION);
	      fprintf(stderr, "* the same physical package id %u but different core_siblings %s and %s\n",
		      mypackageid, a, b);
	      fprintf(stderr, "* hwloc is merging these packages into a single one assuming your Linux kernel\n");
	      fprintf(stderr, "* does not support this processor correctly.\n");
	      fprintf(stderr, "* You may hide this warning by setting HWLOC_HIDE_ERRORS=1 in the environment.\n");
	      fprintf(stderr, "*\n");
	      fprintf(stderr, "* If hwloc does not report the right number of packages,\n");
	      fprintf(stderr, "* please report this error message to the hwloc user's mailing list,\n");
	      fprintf(stderr, "* along with the output+tarball generated by the hwloc-gather-topology script.\n");
	      fprintf(stderr, "****************************************************************************\n");
	      reported = 1;
	      free(a);
	      free(b);
	    }
	    hwloc_bitmap_or(curpackage->cpuset, curpackage->cpuset, packageset);
	    goto package_done;
	  }
	  curpackage = curpackage->next_cousin;
	}
      }

      /* no package with same physical_package_id, create a new one */
      package = hwloc_alloc_setup_object(HWLOC_OBJ_PACKAGE, mypackageid);
      package->cpuset = packageset;
      hwloc_debug_1arg_bitmap("os package %u has cpuset %s\n",
			      mypackageid, packageset);
      /* add cpuinfo */
      if (cpuinfo_Lprocs) {
	for(j=0; j<(int) cpuinfo_numprocs; j++)
	  if ((int) cpuinfo_Lprocs[j].Pproc == i) {
	    hwloc__move_infos(&package->infos, &package->infos_count,
			      &cpuinfo_Lprocs[j].infos, &cpuinfo_Lprocs[j].infos_count);
	  }
      }
      /* insert in a temporary list in case we have to modify the cpuset by merging other core_siblings later.
       * we'll actually insert the tree at the end of the entire sysfs cpu loop.
       */
      package->next_cousin = packages;
      packages = package;

      packageset = NULL; /* don't free it */
    }
package_done:
    hwloc_bitmap_free(packageset);

    /* look at the core */
    sprintf(str, "%s/cpu%d/topology/thread_siblings", path, i);
    coreset = hwloc__alloc_read_path_as_cpumask(str, data->root_fd);

    if (coreset) {
      int gotcoreid = 0; /* to avoid reading the coreid twice */
      if (hwloc_bitmap_weight(coreset) > 1 && threadwithcoreid == -1) {
	/* check if this is hyper-threading or different coreids */
	unsigned siblingid, siblingcoreid;

	mycoreid = (unsigned) -1;
	sprintf(str, "%s/cpu%d/topology/core_id", path, i); /* contains %d at least up to 4.9 */
	if (hwloc_read_path_as_int(str, &tmpint, data->root_fd) == 0)
	  mycoreid = (unsigned) tmpint;
	gotcoreid = 1;

	siblingid = hwloc_bitmap_first(coreset);
	if (siblingid == (unsigned) i)
	  siblingid = hwloc_bitmap_next(coreset, i);
	siblingcoreid = (unsigned) -1;
	sprintf(str, "%s/cpu%u/topology/core_id", path, siblingid); /* contains %d at least up to 4.9 */
	if (hwloc_read_path_as_int(str, &tmpint, data->root_fd) == 0)
	  siblingcoreid = (unsigned) tmpint;
	threadwithcoreid = (siblingcoreid != mycoreid);
      }
      if (hwloc_bitmap_first(coreset) == i || threadwithcoreid) {
	/* regular core */
        struct hwloc_obj *core;

	if (!gotcoreid) {
	  mycoreid = (unsigned) -1;
	  sprintf(str, "%s/cpu%d/topology/core_id", path, i); /* contains %d at least up to 4.9 */
	  if (hwloc_read_path_as_int(str, &tmpint, data->root_fd) == 0)
	    mycoreid = (unsigned) tmpint;
	}

	core = hwloc_alloc_setup_object(HWLOC_OBJ_CORE, mycoreid);
	if (threadwithcoreid)
	  /* amd multicore compute-unit, create one core per thread */
	  hwloc_bitmap_only(coreset, i);
	core->cpuset = coreset;
        hwloc_debug_1arg_bitmap("os core %u has cpuset %s\n",
				mycoreid, core->cpuset);
        hwloc_insert_object_by_cpuset(topology, core);
	coreset = NULL; /* don't free it */
      }
      hwloc_bitmap_free(coreset);
    }

    /* look at the books */
    sprintf(str, "%s/cpu%d/topology/book_siblings", path, i);
    bookset = hwloc__alloc_read_path_as_cpumask(str, data->root_fd);
    if (bookset && hwloc_bitmap_first(bookset) == i) {
      struct hwloc_obj *book;

      mybookid = (unsigned) -1;
      sprintf(str, "%s/cpu%d/topology/book_id", path, i); /* contains %d at least up to 4.9 */
      if (hwloc_read_path_as_int(str, &tmpint, data->root_fd) == 0) {
	mybookid = (unsigned) tmpint;

	book = hwloc_alloc_setup_object(HWLOC_OBJ_GROUP, mybookid);
	book->cpuset = bookset;
	hwloc_debug_1arg_bitmap("os book %u has cpuset %s\n",
				mybookid, bookset);
	hwloc_obj_add_info(book, "Type", "Book");
	hwloc_insert_object_by_cpuset(topology, book);
	bookset = NULL; /* don't free it */
      }
      hwloc_bitmap_free(bookset);
    }

    {
      /* look at the thread */
      struct hwloc_obj *thread = hwloc_alloc_setup_object(HWLOC_OBJ_PU, i);
      threadset = hwloc_bitmap_alloc();
      hwloc_bitmap_only(threadset, i);
      thread->cpuset = threadset;
      hwloc_debug_1arg_bitmap("thread %d has cpuset %s\n",
			      i, threadset);
      hwloc_insert_object_by_cpuset(topology, thread);
    }

    /* look at the caches */
    for(j=0; j<10; j++) {
      char str2[20]; /* enough for a level number (one digit) or a type (Data/Instruction/Unified) */
      hwloc_bitmap_t cacheset;

      sprintf(str, "%s/cpu%d/cache/index%d/shared_cpu_map", path, i, j);
      cacheset = hwloc__alloc_read_path_as_cpumask(str, data->root_fd);
      if (cacheset) {
	if (hwloc_bitmap_iszero(cacheset)) {
	  hwloc_bitmap_t tmpset;
	  /* ia64 returning empty L3 and L2i? use the core set instead */
	  sprintf(str, "%s/cpu%d/topology/thread_siblings", path, i);
	  tmpset = hwloc__alloc_read_path_as_cpumask(str, data->root_fd);
	  /* only use it if we actually got something */
	  if (tmpset) {
	    hwloc_bitmap_free(cacheset);
	    cacheset = tmpset;
	  }
	}

	if (hwloc_bitmap_first(cacheset) == i) {
	  unsigned kB;
	  unsigned linesize;
	  unsigned sets, lines_per_tag;
	  unsigned depth; /* 1 for L1, .... */
	  hwloc_obj_cache_type_t type = HWLOC_OBJ_CACHE_UNIFIED; /* default */
	  struct hwloc_obj *cache;

	  /* get the cache level depth */
	  sprintf(str, "%s/cpu%d/cache/index%d/level", path, i, j); /* contains %u at least up to 4.9 */
	  if (hwloc_read_path_as_uint(str, &depth, data->root_fd) < 0) {
	    hwloc_bitmap_free(cacheset);
	    continue;
	  }

	  /* cache type */
	  sprintf(str, "%s/cpu%d/cache/index%d/type", path, i, j);
	  if (hwloc_read_path_by_length(str, str2, sizeof(str2), data->root_fd) == 0) {
	    if (!strncmp(str2, "Data", 4))
	      type = HWLOC_OBJ_CACHE_DATA;
	    else if (!strncmp(str2, "Unified", 7))
	      type = HWLOC_OBJ_CACHE_UNIFIED;
	    else if (!strncmp(str2, "Instruction", 11))
	      type = HWLOC_OBJ_CACHE_INSTRUCTION;
	    else {
	      hwloc_bitmap_free(cacheset);
	      continue;
	    }
	  } else {
	    hwloc_bitmap_free(cacheset);
	    continue;
	  }

	  /* get the cache size */
	  kB = 0;
	  sprintf(str, "%s/cpu%d/cache/index%d/size", path, i, j); /* contains %uK at least up to 4.9 */
	  hwloc_read_path_as_uint(str, &kB, data->root_fd);
	  /* KNL reports L3 with size=0 and full cpuset in cpuid.
	   * Let hwloc_linux_try_add_knl_mcdram_cache() detect it better.
	   */
	  if (!kB && depth == 2 && data->is_knl) {
	    hwloc_bitmap_free(cacheset);
	    continue;
	  }

	  /* get the line size */
	  linesize = 0;
	  sprintf(str, "%s/cpu%d/cache/index%d/coherency_line_size", path, i, j); /* contains %u at least up to 4.9 */
	  hwloc_read_path_as_uint(str, &linesize, data->root_fd);

	  /* get the number of sets and lines per tag.
	   * don't take the associativity directly in "ways_of_associativity" because
	   * some archs (ia64, ppc) put 0 there when fully-associative, while others (x86) put something like -1 there.
	   */
	  sets = 0;
	  sprintf(str, "%s/cpu%d/cache/index%d/number_of_sets", path, i, j); /* contains %u at least up to 4.9 */
	  hwloc_read_path_as_uint(str, &sets, data->root_fd);

	  lines_per_tag = 1;
	  sprintf(str, "%s/cpu%d/cache/index%d/physical_line_partition", path, i, j); /* contains %u at least up to 4.9 */
	  hwloc_read_path_as_uint(str, &lines_per_tag, data->root_fd);

	  /* first cpu in this cache, add the cache */
	  cache = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, -1);
	  cache->attr->cache.size = ((uint64_t)kB) << 10;
	  cache->attr->cache.depth = depth;
	  cache->attr->cache.linesize = linesize;
	  cache->attr->cache.type = type;
	  if (!linesize || !lines_per_tag || !sets)
	    cache->attr->cache.associativity = 0; /* unknown */
	  else if (sets == 1)
	    cache->attr->cache.associativity = 0; /* likely wrong, make it unknown */
	  else
	    cache->attr->cache.associativity = (kB << 10) / linesize / lines_per_tag / sets;
	  cache->cpuset = cacheset;
	  hwloc_debug_1arg_bitmap("cache depth %u has cpuset %s\n",
				  depth, cacheset);
	  hwloc_insert_object_by_cpuset(topology, cache);
	  cacheset = NULL; /* don't free it */
	  ++caches_added;
	}
      }
      hwloc_bitmap_free(cacheset);
    }
  } hwloc_bitmap_foreach_end();

  /* actually insert in the tree now that package cpusets have been fixed-up */
  while (packages) {
    hwloc_obj_t next = packages->next_cousin;
    packages->next_cousin = NULL;
    hwloc_insert_object_by_cpuset(topology, packages);
    packages = next;
  }

  if (0 == caches_added)
    look_powerpc_device_tree(topology, data);

  hwloc_bitmap_free(cpuset);

  return 0;
}



/****************************************
 ****** cpuinfo Topology Discovery ******
 ****************************************/

static int
hwloc_linux_parse_cpuinfo_x86(const char *prefix, const char *value,
			      struct hwloc_obj_info_s **infos, unsigned *infos_count,
			      int is_global __hwloc_attribute_unused)
{
  if (!strcmp("vendor_id", prefix)) {
    hwloc__add_info(infos, infos_count, "CPUVendor", value);
  } else if (!strcmp("model name", prefix)) {
    hwloc__add_info(infos, infos_count, "CPUModel", value);
  } else if (!strcmp("model", prefix)) {
    hwloc__add_info(infos, infos_count, "CPUModelNumber", value);
  } else if (!strcmp("cpu family", prefix)) {
    hwloc__add_info(infos, infos_count, "CPUFamilyNumber", value);
  } else if (!strcmp("stepping", prefix)) {
    hwloc__add_info(infos, infos_count, "CPUStepping", value);
  }
  return 0;
}

static int
hwloc_linux_parse_cpuinfo_ia64(const char *prefix, const char *value,
			       struct hwloc_obj_info_s **infos, unsigned *infos_count,
			       int is_global __hwloc_attribute_unused)
{
  if (!strcmp("vendor", prefix)) {
    hwloc__add_info(infos, infos_count, "CPUVendor", value);
  } else if (!strcmp("model name", prefix)) {
    hwloc__add_info(infos, infos_count, "CPUModel", value);
  } else if (!strcmp("model", prefix)) {
    hwloc__add_info(infos, infos_count, "CPUModelNumber", value);
  } else if (!strcmp("family", prefix)) {
    hwloc__add_info(infos, infos_count, "CPUFamilyNumber", value);
  }
  return 0;
}

static int
hwloc_linux_parse_cpuinfo_arm(const char *prefix, const char *value,
			      struct hwloc_obj_info_s **infos, unsigned *infos_count,
			      int is_global __hwloc_attribute_unused)
{
  if (!strcmp("Processor", prefix) /* old kernels with one Processor header */
      || !strcmp("model name", prefix) /* new kernels with one model name per core */) {
    hwloc__add_info(infos, infos_count, "CPUModel", value);
  } else if (!strcmp("CPU implementer", prefix)) {
    hwloc__add_info(infos, infos_count, "CPUImplementer", value);
  } else if (!strcmp("CPU architecture", prefix)) {
    hwloc__add_info(infos, infos_count, "CPUArchitecture", value);
  } else if (!strcmp("CPU variant", prefix)) {
    hwloc__add_info(infos, infos_count, "CPUVariant", value);
  } else if (!strcmp("CPU part", prefix)) {
    hwloc__add_info(infos, infos_count, "CPUPart", value);
  } else if (!strcmp("CPU revision", prefix)) {
    hwloc__add_info(infos, infos_count, "CPURevision", value);
  } else if (!strcmp("Hardware", prefix)) {
    hwloc__add_info(infos, infos_count, "HardwareName", value);
  } else if (!strcmp("Revision", prefix)) {
    hwloc__add_info(infos, infos_count, "HardwareRevision", value);
  } else if (!strcmp("Serial", prefix)) {
    hwloc__add_info(infos, infos_count, "HardwareSerial", value);
  }
  return 0;
}

static int
hwloc_linux_parse_cpuinfo_ppc(const char *prefix, const char *value,
			      struct hwloc_obj_info_s **infos, unsigned *infos_count,
			      int is_global)
{
  /* common fields */
  if (!strcmp("cpu", prefix)) {
    hwloc__add_info(infos, infos_count, "CPUModel", value);
  } else if (!strcmp("platform", prefix)) {
    hwloc__add_info(infos, infos_count, "PlatformName", value);
  } else if (!strcmp("model", prefix)) {
    hwloc__add_info(infos, infos_count, "PlatformModel", value);
  }
  /* platform-specific fields */
  else if (!strcasecmp("vendor", prefix)) {
    hwloc__add_info(infos, infos_count, "PlatformVendor", value);
  } else if (!strcmp("Board ID", prefix)) {
    hwloc__add_info(infos, infos_count, "PlatformBoardID", value);
  } else if (!strcmp("Board", prefix)
	     || !strcasecmp("Machine", prefix)) {
    /* machine and board are similar (and often more precise) than model above */
    char **valuep = hwloc__find_info_slot(infos, infos_count, "PlatformModel");
    if (*valuep)
      free(*valuep);
    *valuep = strdup(value);
  } else if (!strcasecmp("Revision", prefix)
	     || !strcmp("Hardware rev", prefix)) {
    hwloc__add_info(infos, infos_count, is_global ? "PlatformRevision" : "CPURevision", value);
  } else if (!strcmp("SVR", prefix)) {
    hwloc__add_info(infos, infos_count, "SystemVersionRegister", value);
  } else if (!strcmp("PVR", prefix)) {
    hwloc__add_info(infos, infos_count, "ProcessorVersionRegister", value);
  }
  /* don't match 'board*' because there's also "board l2" on some platforms */
  return 0;
}

/*
 * avr32: "chip type\t:"			=> OK
 * blackfin: "model name\t:"			=> OK
 * h8300: "CPU:"				=> OK
 * m68k: "CPU:"					=> OK
 * mips: "cpu model\t\t:"			=> OK
 * openrisc: "CPU:"				=> OK
 * sparc: "cpu\t\t:"				=> OK
 * tile: "model name\t:"			=> OK
 * unicore32: "Processor\t:"			=> OK
 * alpha: "cpu\t\t\t: Alpha" + "cpu model\t\t:"	=> "cpu" overwritten by "cpu model", no processor indexes
 * cris: "cpu\t\t:" + "cpu model\t:"		=> only "cpu"
 * frv: "CPU-Core:" + "CPU:"			=> only "CPU"
 * mn10300: "cpu core   :" + "model name :"	=> only "model name"
 * parisc: "cpu family\t:" + "cpu\t\t:"		=> only "cpu"
 *
 * not supported because of conflicts with other arch minor lines:
 * m32r: "cpu family\t:"			=> KO (adding "cpu family" would break "blackfin")
 * microblaze: "CPU-Family:"			=> KO
 * sh: "cpu family\t:" + "cpu type\t:"		=> KO
 * xtensa: "model\t\t:"				=> KO
 */
static int
hwloc_linux_parse_cpuinfo_generic(const char *prefix, const char *value,
				  struct hwloc_obj_info_s **infos, unsigned *infos_count,
				  int is_global __hwloc_attribute_unused)
{
  if (!strcmp("model name", prefix)
      || !strcmp("Processor", prefix)
      || !strcmp("chip type", prefix)
      || !strcmp("cpu model", prefix)
      || !strcasecmp("cpu", prefix)) {
    /* keep the last one, assume it's more precise than the first one.
     * we should have the Architecture keypair for basic information anyway.
     */
    char **valuep = hwloc__find_info_slot(infos, infos_count, "CPUModel");
    if (*valuep)
      free(*valuep);
    *valuep = strdup(value);
  }
  return 0;
}

/* Lprocs_p set to NULL unless returns > 0 */
static int
hwloc_linux_parse_cpuinfo(struct hwloc_linux_backend_data_s *data,
			  const char *path,
			  struct hwloc_linux_cpuinfo_proc ** Lprocs_p,
			  struct hwloc_obj_info_s **global_infos, unsigned *global_infos_count)
{
  FILE *fd;
  char *str = NULL;
  char *endptr;
  unsigned len;
  unsigned allocated_Lprocs = 0;
  struct hwloc_linux_cpuinfo_proc * Lprocs = NULL;
  unsigned numprocs = 0;
  int curproc = -1;
  int (*parse_cpuinfo_func)(const char *, const char *, struct hwloc_obj_info_s **, unsigned *, int) = NULL;

  if (!(fd=hwloc_fopen(path,"r", data->root_fd)))
    {
      hwloc_debug("could not open %s\n", path);
      return -1;
    }

#      define PROCESSOR	"processor"
#      define PACKAGEID "physical id" /* the longest one */
#      define COREID "core id"
  len = 128; /* vendor/model can be very long */
  str = malloc(len);
  hwloc_debug("\n\n * Topology extraction from %s *\n\n", path);
  while (fgets(str,len,fd)!=NULL) {
    unsigned long Ppkg, Pcore, Pproc;
    char *end, *dot, *prefix, *value;
    int noend = 0;

    /* remove the ending \n */
    end = strchr(str, '\n');
    if (end)
      *end = 0;
    else
      noend = 1;
    /* if empty line, skip and reset curproc */
    if (!*str) {
      curproc = -1;
      continue;
    }
    /* skip lines with no dot */
    dot = strchr(str, ':');
    if (!dot)
      continue;
    /* skip lines not starting with a letter */
    if ((*str > 'z' || *str < 'a')
	&& (*str > 'Z' || *str < 'A'))
      continue;

    /* mark the end of the prefix */
    prefix = str;
    end = dot;
    while (end[-1] == ' ' || end[-1] == '	') end--; /* need a strrspn() */
    *end = 0;
    /* find beginning of value, its end is already marked */
    value = dot+1 + strspn(dot+1, " 	");

    /* defines for parsing numbers */
#   define getprocnb_begin(field, var)					\
    if (!strcmp(field,prefix)) {					\
      var = strtoul(value,&endptr,0);					\
      if (endptr==value) {						\
	hwloc_debug("no number in "field" field of %s\n", path);	\
	goto err;							\
      } else if (var==ULONG_MAX) {					\
	hwloc_debug("too big "field" number in %s\n", path); 		\
	goto err;							\
      }									\
      hwloc_debug(field " %lu\n", var)
#   define getprocnb_end()						\
    }
    /* actually parse numbers */
    getprocnb_begin(PROCESSOR, Pproc);
    curproc = numprocs++;
    if (numprocs > allocated_Lprocs) {
      struct hwloc_linux_cpuinfo_proc * tmp;
      if (!allocated_Lprocs)
	allocated_Lprocs = 8;
      else
        allocated_Lprocs *= 2;
      tmp = realloc(Lprocs, allocated_Lprocs * sizeof(*Lprocs));
      if (!tmp)
	goto err;
      Lprocs = tmp;
    }
    Lprocs[curproc].Pproc = Pproc;
    Lprocs[curproc].Pcore = -1;
    Lprocs[curproc].Ppkg = -1;
    Lprocs[curproc].Lcore = -1;
    Lprocs[curproc].Lpkg = -1;
    Lprocs[curproc].infos = NULL;
    Lprocs[curproc].infos_count = 0;
    getprocnb_end() else
    getprocnb_begin(PACKAGEID, Ppkg);
    Lprocs[curproc].Ppkg = Ppkg;
    getprocnb_end() else
    getprocnb_begin(COREID, Pcore);
    Lprocs[curproc].Pcore = Pcore;
    getprocnb_end() else {

      /* architecture specific or default routine for parsing cpumodel */
      switch (data->arch) {
      case HWLOC_LINUX_ARCH_X86:
	parse_cpuinfo_func = hwloc_linux_parse_cpuinfo_x86;
	break;
      case HWLOC_LINUX_ARCH_ARM:
	parse_cpuinfo_func = hwloc_linux_parse_cpuinfo_arm;
	break;
      case HWLOC_LINUX_ARCH_POWER:
	parse_cpuinfo_func = hwloc_linux_parse_cpuinfo_ppc;
	break;
      case HWLOC_LINUX_ARCH_IA64:
	parse_cpuinfo_func = hwloc_linux_parse_cpuinfo_ia64;
	break;
      default:
	parse_cpuinfo_func = hwloc_linux_parse_cpuinfo_generic;
      }

      /* we can't assume that we already got a processor index line:
       * alpha/frv/h8300/m68k/microblaze/sparc have no processor lines at all, only a global entry.
       * tile has a global section with model name before the list of processor lines.
       */
      parse_cpuinfo_func(prefix, value,
			 curproc >= 0 ? &Lprocs[curproc].infos : global_infos,
			 curproc >= 0 ? &Lprocs[curproc].infos_count : global_infos_count,
			 curproc < 0);
    }

    if (noend) {
      /* ignore end of line */
      if (fscanf(fd,"%*[^\n]") == EOF)
	break;
      getc(fd);
    }
  }
  fclose(fd);
  free(str);

  *Lprocs_p = Lprocs;
  return numprocs;

 err:
  fclose(fd);
  free(str);
  free(Lprocs);
  *Lprocs_p = NULL;
  return -1;
}

static void
hwloc_linux_free_cpuinfo(struct hwloc_linux_cpuinfo_proc * Lprocs, unsigned numprocs,
			 struct hwloc_obj_info_s *global_infos, unsigned global_infos_count)
{
  if (Lprocs) {
    unsigned i;
    for(i=0; i<numprocs; i++) {
      hwloc__free_infos(Lprocs[i].infos, Lprocs[i].infos_count);
    }
    free(Lprocs);
  }
  hwloc__free_infos(global_infos, global_infos_count);
}

static int
look_cpuinfo(struct hwloc_topology *topology,
	     struct hwloc_linux_cpuinfo_proc * Lprocs,
	     unsigned numprocs, hwloc_bitmap_t online_cpuset)
{
  /* P for physical/OS index, L for logical (e.g. in we order we get them, not in the final hwloc logical order) */
  unsigned *Lcore_to_Pcore;
  unsigned *Lcore_to_Ppkg; /* needed because Lcore is equivalent to Pcore+Ppkg, not to Pcore alone */
  unsigned *Lpkg_to_Ppkg;
  unsigned numpkgs=0;
  unsigned numcores=0;
  unsigned long Lproc;
  unsigned missingpkg;
  unsigned missingcore;
  unsigned i,j;
  hwloc_bitmap_t cpuset;

  /* initialize misc arrays, there can be at most numprocs entries */
  Lcore_to_Pcore = malloc(numprocs * sizeof(*Lcore_to_Pcore));
  Lcore_to_Ppkg = malloc(numprocs * sizeof(*Lcore_to_Ppkg));
  Lpkg_to_Ppkg = malloc(numprocs * sizeof(*Lpkg_to_Ppkg));
  for (i = 0; i < numprocs; i++) {
    Lcore_to_Pcore[i] = -1;
    Lcore_to_Ppkg[i] = -1;
    Lpkg_to_Ppkg[i] = -1;
  }

  cpuset = hwloc_bitmap_alloc();

  /* create PU objects */
  for(Lproc=0; Lproc<numprocs; Lproc++) {
    unsigned long Pproc = Lprocs[Lproc].Pproc;
    hwloc_obj_t obj = hwloc_alloc_setup_object(HWLOC_OBJ_PU, Pproc);
    hwloc_bitmap_set(cpuset, Pproc);
    obj->cpuset = hwloc_bitmap_alloc();
    hwloc_bitmap_only(obj->cpuset, Pproc);
    hwloc_debug_2args_bitmap("cpu %lu (os %lu) has cpuset %s\n",
			     Lproc, Pproc, obj->cpuset);
    hwloc_insert_object_by_cpuset(topology, obj);
  }

  topology->support.discovery->pu = 1;
  hwloc_bitmap_copy(online_cpuset, cpuset);
  hwloc_bitmap_free(cpuset);

  hwloc_debug("%u online processors found\n", numprocs);
  hwloc_debug_bitmap("online processor cpuset: %s\n", online_cpuset);

  hwloc_debug("%s", "\n * Topology summary *\n");
  hwloc_debug("%u processors)\n", numprocs);

  /* fill Lprocs[].Lpkg and Lpkg_to_Ppkg */
  for(Lproc=0; Lproc<numprocs; Lproc++) {
    long Ppkg = Lprocs[Lproc].Ppkg;
    if (Ppkg != -1) {
      unsigned long Pproc = Lprocs[Lproc].Pproc;
      for (i=0; i<numpkgs; i++)
	if ((unsigned) Ppkg == Lpkg_to_Ppkg[i])
	  break;
      Lprocs[Lproc].Lpkg = i;
      hwloc_debug("%lu on package %u (%lx)\n", Pproc, i, Ppkg);
      if (i==numpkgs) {
	Lpkg_to_Ppkg[numpkgs] = Ppkg;
	numpkgs++;
      }
    }
  }
  /* Some buggy Linuxes don't provide numbers for processor 0, which makes us
   * provide bogus information. We should rather drop it. */
  missingpkg=0;
  for(j=0; j<numprocs; j++)
    if (Lprocs[j].Ppkg == -1) {
      missingpkg=1;
      break;
    }
  /* create package objects */
  hwloc_debug("%u pkgs%s\n", numpkgs, missingpkg ? ", but some missing package" : "");
  if (!missingpkg && numpkgs>0) {
    for (i = 0; i < numpkgs; i++) {
      struct hwloc_obj *obj = hwloc_alloc_setup_object(HWLOC_OBJ_PACKAGE, Lpkg_to_Ppkg[i]);
      int doneinfos = 0;
      obj->cpuset = hwloc_bitmap_alloc();
      for(j=0; j<numprocs; j++)
	if ((unsigned) Lprocs[j].Lpkg == i) {
	  hwloc_bitmap_set(obj->cpuset, Lprocs[j].Pproc);
	  if (!doneinfos) {
	    hwloc__move_infos(&obj->infos, &obj->infos_count, &Lprocs[j].infos, &Lprocs[j].infos_count);
	    doneinfos = 1;
	  }
	}
      hwloc_debug_1arg_bitmap("package %d has cpuset %s\n", i, obj->cpuset);
      hwloc_insert_object_by_cpuset(topology, obj);
    }
    hwloc_debug("%s", "\n");
  }

  /* fill Lprocs[].Lcore, Lcore_to_Ppkg and Lcore_to_Pcore */
  for(Lproc=0; Lproc<numprocs; Lproc++) {
    long Pcore = Lprocs[Lproc].Pcore;
    if (Pcore != -1) {
      for (i=0; i<numcores; i++)
	if ((unsigned) Pcore == Lcore_to_Pcore[i] && (unsigned) Lprocs[Lproc].Ppkg == Lcore_to_Ppkg[i])
	  break;
      Lprocs[Lproc].Lcore = i;
      if (i==numcores) {
	Lcore_to_Ppkg[numcores] = Lprocs[Lproc].Ppkg;
	Lcore_to_Pcore[numcores] = Pcore;
	numcores++;
      }
    }
  }
  /* Some buggy Linuxes don't provide numbers for processor 0, which makes us
   * provide bogus information. We should rather drop it. */
  missingcore=0;
  for(j=0; j<numprocs; j++)
    if (Lprocs[j].Pcore == -1) {
      missingcore=1;
      break;
    }
  /* create Core objects */
  hwloc_debug("%u cores%s\n", numcores, missingcore ? ", but some missing core" : "");
  if (!missingcore && numcores>0) {
    for (i = 0; i < numcores; i++) {
      struct hwloc_obj *obj = hwloc_alloc_setup_object(HWLOC_OBJ_CORE, Lcore_to_Pcore[i]);
      obj->cpuset = hwloc_bitmap_alloc();
      for(j=0; j<numprocs; j++)
	if ((unsigned) Lprocs[j].Lcore == i)
	  hwloc_bitmap_set(obj->cpuset, Lprocs[j].Pproc);
      hwloc_debug_1arg_bitmap("Core %d has cpuset %s\n", i, obj->cpuset);
      hwloc_insert_object_by_cpuset(topology, obj);
    }
    hwloc_debug("%s", "\n");
  }

  free(Lcore_to_Pcore);
  free(Lcore_to_Ppkg);
  free(Lpkg_to_Ppkg);
  return 0;
}



/*************************************
 ****** Main Topology Discovery ******
 *************************************/

static void
hwloc__linux_get_mic_sn(struct hwloc_topology *topology, struct hwloc_linux_backend_data_s *data)
{
  char line[64], *tmp, *end;
  if (hwloc_read_path_by_length("/proc/elog", line, sizeof(line), data->root_fd) < 0)
    return;
  if (strncmp(line, "Card ", 5))
    return;
  tmp = line + 5;
  end = strchr(tmp, ':');
  if (!end)
    return;
  *end = '\0';
  hwloc_obj_add_info(hwloc_get_root_obj(topology), "MICSerialNumber", tmp);
}

static void
hwloc_gather_system_info(struct hwloc_topology *topology,
			 struct hwloc_linux_backend_data_s *data)
{
  FILE *file;
  char line[128]; /* enough for utsname fields */
  const char *env;

  /* initialize to something sane, in case !is_thissystem and we can't find things in /proc/hwloc-nofile-info */
  memset(&data->utsname, 0, sizeof(data->utsname));
  data->fallback_nbprocessors = 1;
  data->pagesize = 4096;

  /* read thissystem info */
  if (topology->is_thissystem) {
    uname(&data->utsname);
    data->fallback_nbprocessors = hwloc_fallback_nbprocessors(topology);
    data->pagesize = hwloc_getpagesize();
  }

  /* overwrite with optional /proc/hwloc-nofile-info */
  file = hwloc_fopen("/proc/hwloc-nofile-info", "r", data->root_fd);
  if (file) {
    while (fgets(line, sizeof(line), file)) {
      char *tmp = strchr(line, '\n');
      if (!strncmp("OSName: ", line, 8)) {
	if (tmp)
	  *tmp = '\0';
	strncpy(data->utsname.sysname, line+8, sizeof(data->utsname.sysname));
	data->utsname.sysname[sizeof(data->utsname.sysname)-1] = '\0';
      } else if (!strncmp("OSRelease: ", line, 11)) {
	if (tmp)
	  *tmp = '\0';
	strncpy(data->utsname.release, line+11, sizeof(data->utsname.release));
	data->utsname.release[sizeof(data->utsname.release)-1] = '\0';
      } else if (!strncmp("OSVersion: ", line, 11)) {
	if (tmp)
	  *tmp = '\0';
	strncpy(data->utsname.version, line+11, sizeof(data->utsname.version));
	data->utsname.version[sizeof(data->utsname.version)-1] = '\0';
      } else if (!strncmp("HostName: ", line, 10)) {
	if (tmp)
	  *tmp = '\0';
	strncpy(data->utsname.nodename, line+10, sizeof(data->utsname.nodename));
	data->utsname.nodename[sizeof(data->utsname.nodename)-1] = '\0';
      } else if (!strncmp("Architecture: ", line, 14)) {
	if (tmp)
	  *tmp = '\0';
	strncpy(data->utsname.machine, line+14, sizeof(data->utsname.machine));
	data->utsname.machine[sizeof(data->utsname.machine)-1] = '\0';
      } else if (!strncmp("FallbackNbProcessors: ", line, 22)) {
	if (tmp)
	  *tmp = '\0';
	data->fallback_nbprocessors = atoi(line+22);
      } else if (!strncmp("PageSize: ", line, 10)) {
	if (tmp)
	 *tmp = '\0';
	data->pagesize = strtoull(line+10, NULL, 10);
      } else {
	hwloc_debug("ignored /proc/hwloc-nofile-info line %s\n", line);
	/* ignored */
      }
    }
    fclose(file);
  }

  env = getenv("HWLOC_DUMP_NOFILE_INFO");
  if (env && *env) {
    file = fopen(env, "w");
    if (file) {
      if (*data->utsname.sysname)
	fprintf(file, "OSName: %s\n", data->utsname.sysname);
      if (*data->utsname.release)
	fprintf(file, "OSRelease: %s\n", data->utsname.release);
      if (*data->utsname.version)
	fprintf(file, "OSVersion: %s\n", data->utsname.version);
      if (*data->utsname.nodename)
	fprintf(file, "HostName: %s\n", data->utsname.nodename);
      if (*data->utsname.machine)
	fprintf(file, "Architecture: %s\n", data->utsname.machine);
      fprintf(file, "FallbackNbProcessors: %u\n", data->fallback_nbprocessors);
      fprintf(file, "PageSize: %llu\n", (unsigned long long) data->pagesize);
      fclose(file);
    }
  }

  /* detect arch for quirks, using configure #defines if possible, or uname */
#if (defined HWLOC_X86_32_ARCH) || (defined HWLOC_X86_64_ARCH) /* does not cover KNC */
  if (topology->is_thissystem)
    data->arch = HWLOC_LINUX_ARCH_X86;
#endif
  if (data->arch == HWLOC_LINUX_ARCH_UNKNOWN && *data->utsname.machine) {
    if (!strcmp(data->utsname.machine, "x86_64")
	|| (data->utsname.machine[0] == 'i' && !strcmp(data->utsname.machine+2, "86"))
	|| !strcmp(data->utsname.machine, "k1om"))
      data->arch = HWLOC_LINUX_ARCH_X86;
    else if (!strncmp(data->utsname.machine, "arm", 3))
      data->arch = HWLOC_LINUX_ARCH_ARM;
    else if (!strncmp(data->utsname.machine, "ppc", 3)
	     || !strncmp(data->utsname.machine, "power", 5))
      data->arch = HWLOC_LINUX_ARCH_POWER;
    else if (!strcmp(data->utsname.machine, "ia64"))
      data->arch = HWLOC_LINUX_ARCH_IA64;
  }
}

/* returns 0 on success, -1 on non-match or error during hardwired load */
static int
hwloc_linux_try_hardwired_cpuinfo(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  struct hwloc_linux_backend_data_s *data = backend->private_data;

  if (getenv("HWLOC_NO_HARDWIRED_TOPOLOGY"))
    return -1;

  if (!strcmp(data->utsname.machine, "s64fx")) {
    char line[128];
    /* Fujistu K-computer, FX10, and FX100 use specific processors
     * whose Linux topology support is broken until 4.1 (acc455cffa75070d55e74fc7802b49edbc080e92and)
     * and existing machines will likely never be fixed by kernel upgrade.
     */

    /* /proc/cpuinfo starts with one of these lines:
     * "cpu             : Fujitsu SPARC64 VIIIfx"
     * "cpu             : Fujitsu SPARC64 XIfx"
     * "cpu             : Fujitsu SPARC64 IXfx"
     */
    if (hwloc_read_path_by_length("/proc/cpuinfo", line, sizeof(line), data->root_fd) < 0)
      return -1;

    if (strncmp(line, "cpu	", 4))
      return -1;

    if (strstr(line, "Fujitsu SPARC64 VIIIfx"))
      return hwloc_look_hardwired_fujitsu_k(topology);
    else if (strstr(line, "Fujitsu SPARC64 IXfx"))
      return hwloc_look_hardwired_fujitsu_fx10(topology);
    else if (strstr(line, "FUJITSU SPARC64 XIfx"))
      return hwloc_look_hardwired_fujitsu_fx100(topology);
  }
  return -1;
}

static void hwloc_linux__get_allowed_resources(hwloc_topology_t topology, const char *root_path, int root_fd, char **cpuset_namep)
{
  char *cpuset_mntpnt, *cgroup_mntpnt, *cpuset_name = NULL;
  hwloc_find_linux_cpuset_mntpnt(&cgroup_mntpnt, &cpuset_mntpnt, root_path);
  if (cgroup_mntpnt || cpuset_mntpnt) {
    cpuset_name = hwloc_read_linux_cpuset_name(root_fd, topology->pid);
    if (cpuset_name) {
      hwloc_admin_disable_set_from_cpuset(root_fd, cgroup_mntpnt, cpuset_mntpnt, cpuset_name, "cpus", topology->levels[0][0]->allowed_cpuset);
      hwloc_admin_disable_set_from_cpuset(root_fd, cgroup_mntpnt, cpuset_mntpnt, cpuset_name, "mems", topology->levels[0][0]->allowed_nodeset);
    }
    free(cgroup_mntpnt);
    free(cpuset_mntpnt);
  }
  *cpuset_namep = cpuset_name;
}

static int
hwloc_look_linuxfs(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  struct hwloc_linux_backend_data_s *data = backend->private_data;
  DIR *nodes_dir;
  unsigned nbnodes;
  char *cpuset_name;
  struct hwloc_linux_cpuinfo_proc * Lprocs = NULL;
  struct hwloc_obj_info_s *global_infos = NULL;
  unsigned global_infos_count = 0;
  int numprocs;
  int already_pus;
  int err;

  already_pus = (topology->levels[0][0]->complete_cpuset != NULL
		 && !hwloc_bitmap_iszero(topology->levels[0][0]->complete_cpuset));
  /* if there are PUs, still look at memory information
   * since x86 misses NUMA node information (unless the processor supports topoext)
   * memory size.
   */

  /* allocate root sets in case not done yet */
  hwloc_alloc_obj_cpusets(topology->levels[0][0]);

  /*********************************
   * Platform information for later
   */
  hwloc_gather_system_info(topology, data);

  /**********************
   * /proc/cpuinfo
   */
  numprocs = hwloc_linux_parse_cpuinfo(data, "/proc/cpuinfo", &Lprocs, &global_infos, &global_infos_count);
  if (numprocs < 0)
    numprocs = 0;

  /**************************
   * detect model for quirks
   */
  if (data->arch == HWLOC_LINUX_ARCH_X86 && numprocs > 0) {
      unsigned i;
      const char *cpuvendor = NULL, *cpufamilynumber = NULL, *cpumodelnumber = NULL;
      for(i=0; i<Lprocs[0].infos_count; i++) {
	if (!strcmp(Lprocs[0].infos[i].name, "CPUVendor")) {
	  cpuvendor = Lprocs[0].infos[i].value;
	} else if (!strcmp(Lprocs[0].infos[i].name, "CPUFamilyNumber")) {
	  cpufamilynumber = Lprocs[0].infos[i].value;
	} else if (!strcmp(Lprocs[0].infos[i].name, "CPUModelNumber")) {
	  cpumodelnumber = Lprocs[0].infos[i].value;
	}
      }
      if (cpuvendor && !strcmp(cpuvendor, "GenuineIntel")
	  && cpufamilynumber && !strcmp(cpufamilynumber, "6")
	  && cpumodelnumber && (!strcmp(cpumodelnumber, "87")
	  || !strcmp(cpumodelnumber, "133")))
	data->is_knl = 1;
      if (cpuvendor && !strcmp(cpuvendor, "AuthenticAMD")
	  && cpufamilynumber
	  && (!strcmp(cpufamilynumber, "21")
	      || !strcmp(cpufamilynumber, "22")))
	data->is_amd_with_CU = 1;
  }

  /**********************
   * Gather the list of admin-disabled cpus and mems
   */
  hwloc_linux__get_allowed_resources(topology, data->root_path, data->root_fd, &cpuset_name);

  nodes_dir = hwloc_opendir("/proc/nodes", data->root_fd);
  if (nodes_dir) {
    /* Kerrighed */
    struct dirent *dirent;
    char path[128];
    hwloc_obj_t machine;
    hwloc_bitmap_t machine_online_set;

    if (already_pus) {
      /* we don't support extending kerrighed topologies */
      free(cpuset_name);
      hwloc_linux_free_cpuinfo(Lprocs, numprocs, global_infos, global_infos_count);
      return 0;
    }

    /* replace top-level object type with SYSTEM and add some MACHINE underneath */

    topology->levels[0][0]->type = HWLOC_OBJ_SYSTEM;
    topology->levels[0][0]->name = strdup("Kerrighed");

    /* No cpuset support for now.  */
    /* No sys support for now.  */
    while ((dirent = readdir(nodes_dir)) != NULL) {
      struct hwloc_linux_cpuinfo_proc * machine_Lprocs = NULL;
      struct hwloc_obj_info_s *machine_global_infos = NULL;
      unsigned machine_global_infos_count = 0;
      int machine_numprocs = 0;
      unsigned long node;
      if (strncmp(dirent->d_name, "node", 4))
	continue;
      machine_online_set = hwloc_bitmap_alloc();
      node = strtoul(dirent->d_name+4, NULL, 0);
      snprintf(path, sizeof(path), "/proc/nodes/node%lu/cpuinfo", node);
      machine_numprocs = hwloc_linux_parse_cpuinfo(data, path, &machine_Lprocs, &machine_global_infos, &machine_global_infos_count);
      if (machine_numprocs < 0) {
	err = -1;
	machine_numprocs = 0;
      } else {
	err = look_cpuinfo(topology, machine_Lprocs, machine_numprocs, machine_online_set);
      }

      hwloc_linux_free_cpuinfo(machine_Lprocs, machine_numprocs, machine_global_infos, machine_global_infos_count);
      if (err < 0) {
        hwloc_bitmap_free(machine_online_set);
        continue;
      }
      hwloc_bitmap_or(topology->levels[0][0]->online_cpuset, topology->levels[0][0]->online_cpuset, machine_online_set);
      machine = hwloc_alloc_setup_object(HWLOC_OBJ_MACHINE, node);
      machine->cpuset = machine_online_set;
      hwloc_debug_1arg_bitmap("machine number %lu has cpuset %s\n",
		 node, machine_online_set);

      /* Get the machine memory attributes */
      hwloc_get_kerrighed_node_meminfo_info(topology, data, node, &machine->memory);

      /* Gather DMI info */
      /* FIXME: get the right DMI info of each machine */
      hwloc__get_dmi_id_info(data, machine);

      hwloc_insert_object_by_cpuset(topology, machine);
    }
    closedir(nodes_dir);
  } else {
    /*********************
     * Memory information
     */

    /* Get the machine memory attributes */
    hwloc_get_procfs_meminfo_info(topology, data, &topology->levels[0][0]->memory);

    /* Gather NUMA information. Must be after hwloc_get_procfs_meminfo_info so that the hugepage size is known */
    if (look_sysfsnode(topology, data, "/sys/bus/node/devices", &nbnodes) < 0)
      look_sysfsnode(topology, data, "/sys/devices/system/node", &nbnodes);

    /* if we found some numa nodes, the machine object has no local memory */
    if (nbnodes) {
      unsigned i;
      topology->levels[0][0]->memory.local_memory = 0;
      if (topology->levels[0][0]->memory.page_types)
        for(i=0; i<topology->levels[0][0]->memory.page_types_len; i++)
          topology->levels[0][0]->memory.page_types[i].count = 0;
    }

    /**********************
     * CPU information
     */

    /* Don't rediscover CPU resources if already done */
    if (already_pus)
      goto done;

    /* Gather the list of cpus now */
    err = hwloc_linux_try_hardwired_cpuinfo(backend);
    if (!err)
      goto done;

    /* setup root info */
    hwloc__move_infos(&hwloc_get_root_obj(topology)->infos, &hwloc_get_root_obj(topology)->infos_count,
		      &global_infos, &global_infos_count);

    if (getenv("HWLOC_LINUX_USE_CPUINFO")
	|| (hwloc_access("/sys/devices/system/cpu/cpu0/topology/core_siblings", R_OK, data->root_fd) < 0
	    && hwloc_access("/sys/devices/system/cpu/cpu0/topology/thread_siblings", R_OK, data->root_fd) < 0
	    && hwloc_access("/sys/bus/cpu/devices/cpu0/topology/thread_siblings", R_OK, data->root_fd) < 0
	    && hwloc_access("/sys/bus/cpu/devices/cpu0/topology/core_siblings", R_OK, data->root_fd) < 0)) {
	/* revert to reading cpuinfo only if /sys/.../topology unavailable (before 2.6.16)
	 * or not containing anything interesting */
      if (numprocs > 0)
	err = look_cpuinfo(topology, Lprocs, numprocs, topology->levels[0][0]->online_cpuset);
      else
	err = -1;
      if (err < 0)
	hwloc_setup_pu_level(topology, data->fallback_nbprocessors);
      look_powerpc_device_tree(topology, data);

    } else {
      /* sysfs */
      if (look_sysfscpu(topology, data, "/sys/bus/cpu/devices", Lprocs, numprocs) < 0)
        if (look_sysfscpu(topology, data, "/sys/devices/system/cpu", Lprocs, numprocs) < 0)
	  /* sysfs but we failed to read cpu topology, fallback */
	  hwloc_setup_pu_level(topology, data->fallback_nbprocessors);
    }

 done:

    /**********************
     * Misc
     */

    /* Gather DMI info */
    hwloc__get_dmi_id_info(data, topology->levels[0][0]);
    if (hwloc_topology_get_flags(topology) & (HWLOC_TOPOLOGY_FLAG_IO_DEVICES|HWLOC_TOPOLOGY_FLAG_WHOLE_IO))
      hwloc__get_firmware_dmi_memory_info(topology, data);
  }

  hwloc_obj_add_info(topology->levels[0][0], "Backend", "Linux");
  if (cpuset_name) {
    hwloc_obj_add_info(topology->levels[0][0], "LinuxCgroup", cpuset_name);
    free(cpuset_name);
  }

  hwloc__linux_get_mic_sn(topology, data);

  /* data->utsname was filled with real uname or \0, we can safely pass it */
  hwloc_add_uname_info(topology, &data->utsname);

  hwloc_linux_free_cpuinfo(Lprocs, numprocs, global_infos, global_infos_count);
  return 1;
}



/****************************************
 ***** Linux PCI backend callbacks ******
 ****************************************
 * Do not support changing the fsroot (use sysfs)
 */

static hwloc_obj_t
hwloc_linux_add_os_device(struct hwloc_backend *backend, struct hwloc_obj *pcidev, hwloc_obj_osdev_type_t type, const char *name)
{
  struct hwloc_topology *topology = backend->topology;
  struct hwloc_obj *obj = hwloc_alloc_setup_object(HWLOC_OBJ_OS_DEVICE, -1);
  obj->name = strdup(name);
  obj->logical_index = -1;
  obj->attr->osdev.type = type;

  hwloc_insert_object_by_parent(topology, pcidev, obj);
  /* insert_object_by_parent() doesn't merge during insert, so obj is still valid */

  return obj;
}

typedef void (*hwloc_linux_class_fillinfos_t)(struct hwloc_backend *backend, struct hwloc_obj *osdev, const char *osdevpath);

/* cannot be used in fsroot-aware code, would have to move to a per-topology variable */

static void
hwloc_linux_check_deprecated_classlinks_model(struct hwloc_linux_backend_data_s *data)
{
  int root_fd = data->root_fd;
  DIR *dir;
  struct dirent *dirent;
  char path[128];
  struct stat st;

  data->deprecated_classlinks_model = -1;

  dir = hwloc_opendir("/sys/class/net", root_fd);
  if (!dir)
    return;
  while ((dirent = readdir(dir)) != NULL) {
    if (!strcmp(dirent->d_name, ".") || !strcmp(dirent->d_name, "..") || !strcmp(dirent->d_name, "lo"))
      continue;
    snprintf(path, sizeof(path), "/sys/class/net/%s/device/net/%s", dirent->d_name, dirent->d_name);
    if (hwloc_stat(path, &st, root_fd) == 0) {
      data->deprecated_classlinks_model = 0;
      goto out;
    }
    snprintf(path, sizeof(path), "/sys/class/net/%s/device/net:%s", dirent->d_name, dirent->d_name);
    if (hwloc_stat(path, &st, root_fd) == 0) {
      data->deprecated_classlinks_model = 1;
      goto out;
    }
  }
out:
  closedir(dir);
}

/* class objects that are immediately below pci devices:
 * look for objects of the given classname below a sysfs (pcidev) directory
 */
static int
hwloc_linux_class_readdir(struct hwloc_backend *backend,
			  struct hwloc_obj *pcidev, const char *devicepath,
			  hwloc_obj_osdev_type_t type, const char *classname,
			  hwloc_linux_class_fillinfos_t fillinfo)
{
  struct hwloc_linux_backend_data_s *data = backend->private_data;
  int root_fd = data->root_fd;
  size_t classnamelen = strlen(classname);
  char path[256];
  DIR *dir;
  struct dirent *dirent;
  hwloc_obj_t obj;
  int res = 0, err;

  if (data->deprecated_classlinks_model == -2)
    hwloc_linux_check_deprecated_classlinks_model(data);

  if (data->deprecated_classlinks_model != 1) {
    /* modern sysfs: <device>/<class>/<name> */
    struct stat st;
    snprintf(path, sizeof(path), "%s/%s", devicepath, classname);

    /* some very host kernel (2.6.9/RHEL4) have <device>/<class> symlink without any way to find <name>.
     * make sure <device>/<class> is a directory to avoid this case.
     */
    err = hwloc_lstat(path, &st, root_fd);
    if (err < 0 || !S_ISDIR(st.st_mode))
      goto trydeprecated;

    dir = hwloc_opendir(path, root_fd);
    if (dir) {
      data->deprecated_classlinks_model = 0;
      while ((dirent = readdir(dir)) != NULL) {
	if (!strcmp(dirent->d_name, ".") || !strcmp(dirent->d_name, ".."))
	  continue;
	obj = hwloc_linux_add_os_device(backend, pcidev, type, dirent->d_name);
	if (fillinfo) {
	  snprintf(path, sizeof(path), "%s/%s/%s", devicepath, classname, dirent->d_name);
	  fillinfo(backend, obj, path);
	}
	res++;
      }
      closedir(dir);
      return res;
    }
  }

trydeprecated:
  if (data->deprecated_classlinks_model != 0) {
    /* deprecated sysfs: <device>/<class>:<name> */
    dir = hwloc_opendir(devicepath, root_fd);
    if (dir) {
      while ((dirent = readdir(dir)) != NULL) {
	if (strncmp(dirent->d_name, classname, classnamelen) || dirent->d_name[classnamelen] != ':')
	  continue;
	data->deprecated_classlinks_model = 1;
	obj = hwloc_linux_add_os_device(backend, pcidev, type, dirent->d_name + classnamelen+1);
	if (fillinfo) {
	  snprintf(path, sizeof(path), "%s/%s", devicepath, dirent->d_name);
	  fillinfo(backend, obj, path);
	}
	res++;
      }
      closedir(dir);
      return res;
    }
  }

  return 0;
}

/*
 * look for net objects below a pcidev in sysfs
 */
static void
hwloc_linux_net_class_fillinfos(struct hwloc_backend *backend,
				struct hwloc_obj *obj, const char *osdevpath)
{
  struct hwloc_linux_backend_data_s *data = backend->private_data;
  int root_fd = data->root_fd;
  struct stat st;
  char path[256];
  char address[128];
  snprintf(path, sizeof(path), "%s/address", osdevpath);
  if (!hwloc_read_path_by_length(path, address, sizeof(address), root_fd)) {
    char *eol = strchr(address, '\n');
    if (eol)
      *eol = 0;
    hwloc_obj_add_info(obj, "Address", address);
  }
  snprintf(path, sizeof(path), "%s/device/infiniband", osdevpath);
  if (!hwloc_stat(path, &st, root_fd)) {
    char hexid[16];
    snprintf(path, sizeof(path), "%s/dev_id", osdevpath);
    if (!hwloc_read_path_by_length(path, hexid, sizeof(hexid), root_fd)) {
      char *eoid;
      unsigned long port;
      port = strtoul(hexid, &eoid, 0);
      if (eoid != hexid) {
	char portstr[16];
	snprintf(portstr, sizeof(portstr), "%ld", port+1);
	hwloc_obj_add_info(obj, "Port", portstr);
      }
    }
  }
}

static int
hwloc_linux_lookup_net_class(struct hwloc_backend *backend,
			     struct hwloc_obj *pcidev, const char *pcidevpath)
{
  return hwloc_linux_class_readdir(backend, pcidev, pcidevpath, HWLOC_OBJ_OSDEV_NETWORK, "net", hwloc_linux_net_class_fillinfos);
}

/*
 * look for infiniband objects below a pcidev in sysfs
 */
static void
hwloc_linux_infiniband_class_fillinfos(struct hwloc_backend *backend,
				       struct hwloc_obj *obj, const char *osdevpath)
{
  struct hwloc_linux_backend_data_s *data = backend->private_data;
  int root_fd = data->root_fd;
  char path[256];
  char guidvalue[20];
  unsigned i,j;

  snprintf(path, sizeof(path), "%s/node_guid", osdevpath);
  if (!hwloc_read_path_by_length(path, guidvalue, sizeof(guidvalue), root_fd)) {
    size_t len;
    len = strspn(guidvalue, "0123456789abcdefx:");
    guidvalue[len] = '\0';
    hwloc_obj_add_info(obj, "NodeGUID", guidvalue);
  }

  snprintf(path, sizeof(path), "%s/sys_image_guid", osdevpath);
  if (!hwloc_read_path_by_length(path, guidvalue, sizeof(guidvalue), root_fd)) {
    size_t len;
    len = strspn(guidvalue, "0123456789abcdefx:");
    guidvalue[len] = '\0';
    hwloc_obj_add_info(obj, "SysImageGUID", guidvalue);
  }

  for(i=1; ; i++) {
    char statevalue[2];
    char lidvalue[11];
    char gidvalue[40];

    snprintf(path, sizeof(path), "%s/ports/%u/state", osdevpath, i);
    if (!hwloc_read_path_by_length(path, statevalue, sizeof(statevalue), root_fd)) {
      char statename[32];
      statevalue[1] = '\0'; /* only keep the first byte/digit */
      snprintf(statename, sizeof(statename), "Port%uState", i);
      hwloc_obj_add_info(obj, statename, statevalue);
    } else {
      /* no such port */
      break;
    }

    snprintf(path, sizeof(path), "%s/ports/%u/lid", osdevpath, i);
    if (!hwloc_read_path_by_length(path, lidvalue, sizeof(lidvalue), root_fd)) {
      char lidname[32];
      size_t len;
      len = strspn(lidvalue, "0123456789abcdefx");
      lidvalue[len] = '\0';
      snprintf(lidname, sizeof(lidname), "Port%uLID", i);
      hwloc_obj_add_info(obj, lidname, lidvalue);
    }

    snprintf(path, sizeof(path), "%s/ports/%u/lid_mask_count", osdevpath, i);
    if (!hwloc_read_path_by_length(path, lidvalue, sizeof(lidvalue), root_fd)) {
      char lidname[32];
      size_t len;
      len = strspn(lidvalue, "0123456789");
      lidvalue[len] = '\0';
      snprintf(lidname, sizeof(lidname), "Port%uLMC", i);
      hwloc_obj_add_info(obj, lidname, lidvalue);
    }

    for(j=0; ; j++) {
      snprintf(path, sizeof(path), "%s/ports/%u/gids/%u", osdevpath, i, j);
      if (!hwloc_read_path_by_length(path, gidvalue, sizeof(gidvalue), root_fd)) {
	char gidname[32];
	size_t len;
	len = strspn(gidvalue, "0123456789abcdefx:");
	gidvalue[len] = '\0';
	if (strncmp(gidvalue+20, "0000:0000:0000:0000", 19)) {
	  /* only keep initialized GIDs */
	  snprintf(gidname, sizeof(gidname), "Port%uGID%u", i, j);
	  hwloc_obj_add_info(obj, gidname, gidvalue);
	}
      } else {
	/* no such port */
	break;
      }
    }
  }
}

static int
hwloc_linux_lookup_openfabrics_class(struct hwloc_backend *backend,
				     struct hwloc_obj *pcidev, const char *pcidevpath)
{
  return hwloc_linux_class_readdir(backend, pcidev, pcidevpath, HWLOC_OBJ_OSDEV_OPENFABRICS, "infiniband", hwloc_linux_infiniband_class_fillinfos);
}

/* look for dma objects below a pcidev in sysfs */
static int
hwloc_linux_lookup_dma_class(struct hwloc_backend *backend,
			     struct hwloc_obj *pcidev, const char *pcidevpath)
{
  return hwloc_linux_class_readdir(backend, pcidev, pcidevpath, HWLOC_OBJ_OSDEV_DMA, "dma", NULL);
}

/* look for drm objects below a pcidev in sysfs */
static int
hwloc_linux_lookup_drm_class(struct hwloc_backend *backend,
			     struct hwloc_obj *pcidev, const char *pcidevpath)
{
  return hwloc_linux_class_readdir(backend, pcidev, pcidevpath, HWLOC_OBJ_OSDEV_GPU, "drm", NULL);

  /* we could look at the "graphics" class too, but it doesn't help for proprietary drivers either */

  /* GPU devices (even with a proprietary driver) seem to have a boot_vga field in their PCI device directory (since 2.6.30),
   * so we could create a OS device for each PCI devices with such a field.
   * boot_vga is actually created when class >> 8 == VGA (it contains 1 for boot vga device), so it's trivial anyway.
   */
}

/*
 * look for block objects below a pcidev in sysfs
 */

static void
hwloc_linux_block_class_fillinfos(struct hwloc_backend *backend,
				  struct hwloc_obj *obj, const char *osdevpath)
{
  struct hwloc_linux_backend_data_s *data = backend->private_data;
  int root_fd = data->root_fd;
  FILE *file;
  char path[256];
  char line[128];
  char vendor[64] = "";
  char model[64] = "";
  char serial[64] = "";
  char revision[64] = "";
  char blocktype[64] = "";
  unsigned major_id, minor_id;
  char *tmp;

  snprintf(path, sizeof(path), "%s/dev", osdevpath);
  if (hwloc_read_path_by_length(path, line, sizeof(line), root_fd) < 0)
    return;

  if (sscanf(line, "%u:%u", &major_id, &minor_id) != 2)
    return;
  tmp = strchr(line, '\n');
  if (tmp)
    *tmp = '\0';
  hwloc_obj_add_info(obj, "LinuxDeviceID", line);

#ifdef HWLOC_HAVE_LIBUDEV
  if (data->udev) {
    struct udev_device *dev;
    const char *prop;
    dev = udev_device_new_from_subsystem_sysname(data->udev, "block", obj->name);
    if (!dev)
      return;
    prop = udev_device_get_property_value(dev, "ID_VENDOR");
    if (prop) {
      strncpy(vendor, prop, sizeof(vendor));
      vendor[sizeof(vendor)-1] = '\0';
    }
    prop = udev_device_get_property_value(dev, "ID_MODEL");
    if (prop) {
      strncpy(model, prop, sizeof(model));
      model[sizeof(model)-1] = '\0';
    }
    prop = udev_device_get_property_value(dev, "ID_REVISION");
    if (prop) {
      strncpy(revision, prop, sizeof(revision));
      revision[sizeof(revision)-1] = '\0';
    }
    prop = udev_device_get_property_value(dev, "ID_SERIAL_SHORT");
    if (prop) {
      strncpy(serial, prop, sizeof(serial));
      serial[sizeof(serial)-1] = '\0';
    }
    prop = udev_device_get_property_value(dev, "ID_TYPE");
    if (prop) {
      strncpy(blocktype, prop, sizeof(blocktype));
      blocktype[sizeof(blocktype)-1] = '\0';
    }

    udev_device_unref(dev);
  } else
    /* fallback to reading files, works with any fsroot */
#endif
 {
  snprintf(path, sizeof(path), "/run/udev/data/b%u:%u", major_id, minor_id);
  file = hwloc_fopen(path, "r", root_fd);
  if (!file)
    return;

  while (NULL != fgets(line, sizeof(line), file)) {
    tmp = strchr(line, '\n');
    if (tmp)
      *tmp = '\0';
    if (!strncmp(line, "E:ID_VENDOR=", strlen("E:ID_VENDOR="))) {
      strncpy(vendor, line+strlen("E:ID_VENDOR="), sizeof(vendor));
      vendor[sizeof(vendor)-1] = '\0';
    } else if (!strncmp(line, "E:ID_MODEL=", strlen("E:ID_MODEL="))) {
      strncpy(model, line+strlen("E:ID_MODEL="), sizeof(model));
      model[sizeof(model)-1] = '\0';
    } else if (!strncmp(line, "E:ID_REVISION=", strlen("E:ID_REVISION="))) {
      strncpy(revision, line+strlen("E:ID_REVISION="), sizeof(revision));
      revision[sizeof(revision)-1] = '\0';
    } else if (!strncmp(line, "E:ID_SERIAL_SHORT=", strlen("E:ID_SERIAL_SHORT="))) {
      strncpy(serial, line+strlen("E:ID_SERIAL_SHORT="), sizeof(serial));
      serial[sizeof(serial)-1] = '\0';
    } else if (!strncmp(line, "E:ID_TYPE=", strlen("E:ID_TYPE="))) {
      strncpy(blocktype, line+strlen("E:ID_TYPE="), sizeof(blocktype));
      blocktype[sizeof(blocktype)-1] = '\0';
    }
  }
  fclose(file);
 }

  /* clear fake "ATA" vendor name */
  if (!strcasecmp(vendor, "ATA"))
    *vendor = '\0';
  /* overwrite vendor name from model when possible */
  if (!*vendor) {
    if (!strncasecmp(model, "wd", 2))
      strcpy(vendor, "Western Digital");
    else if (!strncasecmp(model, "st", 2))
      strcpy(vendor, "Seagate");
    else if (!strncasecmp(model, "samsung", 7))
      strcpy(vendor, "Samsung");
    else if (!strncasecmp(model, "sandisk", 7))
      strcpy(vendor, "SanDisk");
    else if (!strncasecmp(model, "toshiba", 7))
      strcpy(vendor, "Toshiba");
  }

  if (*vendor)
    hwloc_obj_add_info(obj, "Vendor", vendor);
  if (*model)
    hwloc_obj_add_info(obj, "Model", model);
  if (*revision)
    hwloc_obj_add_info(obj, "Revision", revision);
  if (*serial)
    hwloc_obj_add_info(obj, "SerialNumber", serial);

  if (!strcmp(blocktype, "disk"))
    hwloc_obj_add_info(obj, "Type", "Disk");
  else if (!strcmp(blocktype, "tape"))
    hwloc_obj_add_info(obj, "Type", "Tape");
  else if (!strcmp(blocktype, "cd") || !strcmp(blocktype, "floppy") || !strcmp(blocktype, "optical"))
    hwloc_obj_add_info(obj, "Type", "Removable Media Device");
  else /* generic, usb mass storage/rbc, usb mass storage/scsi */
    hwloc_obj_add_info(obj, "Type", "Other");
}

/* block class objects are in
 * host%d/target%d:%d:%d/%d:%d:%d:%d/
 * or
 * host%d/port-%d:%d/end_device-%d:%d/target%d:%d:%d/%d:%d:%d:%d/
 * or
 * ide%d/%d.%d/
 * below pci devices */
static int
hwloc_linux_lookup_host_block_class(struct hwloc_backend *backend,
				    struct hwloc_obj *pcidev, char *path, size_t pathlen)
{
  struct hwloc_linux_backend_data_s *data = backend->private_data;
  int root_fd = data->root_fd;
  DIR *hostdir, *portdir, *targetdir;
  struct dirent *hostdirent, *portdirent, *targetdirent;
  size_t hostdlen, portdlen, targetdlen;
  int dummy;
  int res = 0;

  hostdir = hwloc_opendir(path, root_fd);
  if (!hostdir)
    return 0;

  while ((hostdirent = readdir(hostdir)) != NULL) {
    if (sscanf(hostdirent->d_name, "port-%d:%d", &dummy, &dummy) == 2)
    {
      /* found host%d/port-%d:%d */
      path[pathlen] = '/';
      strcpy(&path[pathlen+1], hostdirent->d_name);
      pathlen += hostdlen = 1+strlen(hostdirent->d_name);
      portdir = hwloc_opendir(path, root_fd);
      if (!portdir)
	continue;
      while ((portdirent = readdir(portdir)) != NULL) {
	if (sscanf(portdirent->d_name, "end_device-%d:%d", &dummy, &dummy) == 2) {
	  /* found host%d/port-%d:%d/end_device-%d:%d */
	  path[pathlen] = '/';
	  strcpy(&path[pathlen+1], portdirent->d_name);
	  pathlen += portdlen = 1+strlen(portdirent->d_name);
	  res += hwloc_linux_lookup_host_block_class(backend, pcidev, path, pathlen);
	  /* restore parent path */
	  pathlen -= portdlen;
	  path[pathlen] = '\0';
	}
      }
      closedir(portdir);
      /* restore parent path */
      pathlen -= hostdlen;
      path[pathlen] = '\0';
      continue;
    } else if (sscanf(hostdirent->d_name, "target%d:%d:%d", &dummy, &dummy, &dummy) == 3) {
      /* found host%d/target%d:%d:%d */
      path[pathlen] = '/';
      strcpy(&path[pathlen+1], hostdirent->d_name);
      pathlen += hostdlen = 1+strlen(hostdirent->d_name);
      targetdir = hwloc_opendir(path, root_fd);
      if (!targetdir)
	continue;
      while ((targetdirent = readdir(targetdir)) != NULL) {
	if (sscanf(targetdirent->d_name, "%d:%d:%d:%d", &dummy, &dummy, &dummy, &dummy) != 4)
	  continue;
	/* found host%d/target%d:%d:%d/%d:%d:%d:%d */
	path[pathlen] = '/';
	strcpy(&path[pathlen+1], targetdirent->d_name);
	pathlen += targetdlen = 1+strlen(targetdirent->d_name);
	/* lookup block class for real */
	res += hwloc_linux_class_readdir(backend, pcidev, path, HWLOC_OBJ_OSDEV_BLOCK, "block", hwloc_linux_block_class_fillinfos);
	/* restore parent path */
	pathlen -= targetdlen;
	path[pathlen] = '\0';
      }
      closedir(targetdir);
      /* restore parent path */
      pathlen -= hostdlen;
      path[pathlen] = '\0';
    }
  }
  closedir(hostdir);

  return res;
}

static int
hwloc_linux_lookup_block_class(struct hwloc_backend *backend,
			       struct hwloc_obj *pcidev, const char *pcidevpath)
{
  struct hwloc_linux_backend_data_s *data = backend->private_data;
  int root_fd = data->root_fd;
  size_t pathlen;
  DIR *devicedir, *hostdir;
  struct dirent *devicedirent, *hostdirent;
  size_t devicedlen, hostdlen;
  char path[256];
  int dummy;
  int res = 0;

  strcpy(path, pcidevpath);
  pathlen = strlen(path);

  /* look for a direct block device here (such as NVMe, something without controller subdirs in the middle) */
  res += hwloc_linux_class_readdir(backend, pcidev, path,
				   HWLOC_OBJ_OSDEV_BLOCK, "block",
				   hwloc_linux_block_class_fillinfos);
  if (res)
    return res;
  /* otherwise try to find controller subdirectories */

  devicedir = hwloc_opendir(pcidevpath, root_fd);
  if (!devicedir)
    return 0;

  while ((devicedirent = readdir(devicedir)) != NULL) {
    if (sscanf(devicedirent->d_name, "ide%d", &dummy) == 1) {
      /* found ide%d */
      path[pathlen] = '/';
      strcpy(&path[pathlen+1], devicedirent->d_name);
      pathlen += devicedlen = 1+strlen(devicedirent->d_name);
      hostdir = hwloc_opendir(path, root_fd);
      if (!hostdir)
	continue;
      while ((hostdirent = readdir(hostdir)) != NULL) {
	if (sscanf(hostdirent->d_name, "%d.%d", &dummy, &dummy) == 2) {
	  /* found ide%d/%d.%d */
	  path[pathlen] = '/';
	  strcpy(&path[pathlen+1], hostdirent->d_name);
	  pathlen += hostdlen = 1+strlen(hostdirent->d_name);
	  /* lookup block class for real */
	  res += hwloc_linux_class_readdir(backend, pcidev, path, HWLOC_OBJ_OSDEV_BLOCK, "block", NULL);
	  /* restore parent path */
	  pathlen -= hostdlen;
	  path[pathlen] = '\0';
	}
      }
      closedir(hostdir);
      /* restore parent path */
      pathlen -= devicedlen;
      path[pathlen] = '\0';
    } else if (sscanf(devicedirent->d_name, "host%d", &dummy) == 1) {
      /* found host%d */
      path[pathlen] = '/';
      strcpy(&path[pathlen+1], devicedirent->d_name);
      pathlen += devicedlen = 1+strlen(devicedirent->d_name);
      res += hwloc_linux_lookup_host_block_class(backend, pcidev, path, pathlen);
      /* restore parent path */
      pathlen -= devicedlen;
      path[pathlen] = '\0';
    } else if (sscanf(devicedirent->d_name, "ata%d", &dummy) == 1) {
      /* found ata%d */
      path[pathlen] = '/';
      strcpy(&path[pathlen+1], devicedirent->d_name);
      pathlen += devicedlen = 1+strlen(devicedirent->d_name);
      hostdir = hwloc_opendir(path, root_fd);
      if (!hostdir)
	continue;
      while ((hostdirent = readdir(hostdir)) != NULL) {
	if (sscanf(hostdirent->d_name, "host%d", &dummy) == 1) {
	  /* found ata%d/host%d */
	  path[pathlen] = '/';
	  strcpy(&path[pathlen+1], hostdirent->d_name);
	  pathlen += hostdlen = 1+strlen(hostdirent->d_name);
	  /* lookup block class for real */
          res += hwloc_linux_lookup_host_block_class(backend, pcidev, path, pathlen);
	  /* restore parent path */
	  pathlen -= hostdlen;
	  path[pathlen] = '\0';
	}
      }
      closedir(hostdir);
      /* restore parent path */
      pathlen -= devicedlen;
      path[pathlen] = '\0';
    }
  }
  closedir(devicedir);

  return res;
}

static void
hwloc_linux_mic_class_fillinfos(struct hwloc_backend *backend,
				struct hwloc_obj *obj, const char *osdevpath)
{
  struct hwloc_linux_backend_data_s *data = backend->private_data;
  int root_fd = data->root_fd;
  char path[256];
  char family[64];
  char sku[64];
  char sn[64];
  char string[20];

  hwloc_obj_add_info(obj, "CoProcType", "MIC");

  snprintf(path, sizeof(path), "%s/family", osdevpath);
  if (!hwloc_read_path_by_length(path, family, sizeof(family), root_fd)) {
    char *eol = strchr(family, '\n');
    if (eol)
      *eol = 0;
    hwloc_obj_add_info(obj, "MICFamily", family);
  }

  snprintf(path, sizeof(path), "%s/sku", osdevpath);
  if (!hwloc_read_path_by_length(path, sku, sizeof(sku), root_fd)) {
    char *eol = strchr(sku, '\n');
    if (eol)
      *eol = 0;
    hwloc_obj_add_info(obj, "MICSKU", sku);
  }

  snprintf(path, sizeof(path), "%s/serialnumber", osdevpath);
  if (!hwloc_read_path_by_length(path, sn, sizeof(sn), root_fd)) {
    char *eol;
    eol = strchr(sn, '\n');
    if (eol)
      *eol = 0;
    hwloc_obj_add_info(obj, "MICSerialNumber", sn);
  }

  snprintf(path, sizeof(path), "%s/active_cores", osdevpath);
  if (!hwloc_read_path_by_length(path, string, sizeof(string), root_fd)) {
    unsigned long count = strtoul(string, NULL, 16);
    snprintf(string, sizeof(string), "%lu", count);
    hwloc_obj_add_info(obj, "MICActiveCores", string);
  }

  snprintf(path, sizeof(path), "%s/memsize", osdevpath);
  if (!hwloc_read_path_by_length(path, string, sizeof(string), root_fd)) {
    unsigned long count = strtoul(string, NULL, 16);
    snprintf(string, sizeof(string), "%lu", count);
    hwloc_obj_add_info(obj, "MICMemorySize", string);
  }
}

static int
hwloc_linux_lookup_mic_class(struct hwloc_backend *backend,
			     struct hwloc_obj *pcidev, const char *pcidevpath)
{
  return hwloc_linux_class_readdir(backend, pcidev, pcidevpath, HWLOC_OBJ_OSDEV_COPROC, "mic", hwloc_linux_mic_class_fillinfos);
}

static int
hwloc_linux_directlookup_mic_class(struct hwloc_backend *backend,
				   struct hwloc_obj *pcidev)
{
  struct hwloc_linux_backend_data_s *data = backend->private_data;
  int root_fd = data->root_fd;
  char path[256];
  struct stat st;
  hwloc_obj_t obj;
  unsigned idx;
  int res = 0;

  if (!data->mic_directlookup_id_max)
    /* already tried, nothing to do */
    return 0;

  if (data->mic_directlookup_id_max == (unsigned) -1) {
    /* never tried, find out the max id */
    DIR *dir;
    struct dirent *dirent;

    /* make sure we never do this lookup again */
    data->mic_directlookup_id_max = 0;

    /* read the entire class and find the max id of mic%u dirents */
    dir = hwloc_opendir("/sys/devices/virtual/mic", root_fd);
    if (!dir) {
      dir = hwloc_opendir("/sys/class/mic", root_fd);
      if (!dir)
	return 0;
    }
    while ((dirent = readdir(dir)) != NULL) {
      if (!strcmp(dirent->d_name, ".") || !strcmp(dirent->d_name, ".."))
	continue;
      if (sscanf(dirent->d_name, "mic%u", &idx) != 1)
	continue;
      if (idx >= data->mic_directlookup_id_max)
	data->mic_directlookup_id_max = idx+1;
    }
    closedir(dir);
  }

  /* now iterate over the mic ids and see if one matches our pcidev */
  for(idx=0; idx<data->mic_directlookup_id_max; idx++) {
    snprintf(path, sizeof(path), "/sys/class/mic/mic%u/pci_%02x:%02x.%02x",
	     idx, pcidev->attr->pcidev.bus,  pcidev->attr->pcidev.dev,  pcidev->attr->pcidev.func);
    if (hwloc_stat(path, &st, root_fd) < 0)
      continue;
    snprintf(path, sizeof(path), "mic%u", idx);
    obj = hwloc_linux_add_os_device(backend, pcidev, HWLOC_OBJ_OSDEV_COPROC, path);
    snprintf(path, sizeof(path), "/sys/class/mic/mic%u", idx);
    hwloc_linux_mic_class_fillinfos(backend, obj, path);
    res++;
  }

  return res;
}

/*
 * backend callback for inserting objects inside a pci device
 */
static int
hwloc_linux_backend_notify_new_object(struct hwloc_backend *backend, struct hwloc_backend *caller __hwloc_attribute_unused,
				      struct hwloc_obj *obj)
{
  struct hwloc_linux_backend_data_s *data = backend->private_data;
  char pcidevpath[256];
  int res = 0;

  /* this callback is only used in the libpci backend for now */
  assert(obj->type == HWLOC_OBJ_PCI_DEVICE);

  snprintf(pcidevpath, sizeof(pcidevpath), "/sys/bus/pci/devices/%04x:%02x:%02x.%01x/",
	   obj->attr->pcidev.domain, obj->attr->pcidev.bus,
	   obj->attr->pcidev.dev, obj->attr->pcidev.func);

  res += hwloc_linux_lookup_net_class(backend, obj, pcidevpath);
  res += hwloc_linux_lookup_openfabrics_class(backend, obj, pcidevpath);
  res += hwloc_linux_lookup_dma_class(backend, obj, pcidevpath);
  res += hwloc_linux_lookup_drm_class(backend, obj, pcidevpath);
  res += hwloc_linux_lookup_block_class(backend, obj, pcidevpath);

  if (data->mic_need_directlookup == -1) {
    struct stat st;
    if (hwloc_stat("/sys/class/mic/mic0", &st, data->root_fd) == 0
	&& hwloc_stat("/sys/class/mic/mic0/device/mic/mic0", &st, data->root_fd) == -1)
      /* hwloc_linux_lookup_mic_class will fail because pcidev sysfs directories
       * do not have mic/mic%u symlinks to mic devices (old mic driver).
       * if so, try from the mic class.
       */
      data->mic_need_directlookup = 1;
    else
      data->mic_need_directlookup = 0;
  }
  if (data->mic_need_directlookup)
    res += hwloc_linux_directlookup_mic_class(backend, obj);
  else
    res += hwloc_linux_lookup_mic_class(backend, obj, pcidevpath);

  return res;
}

/*
 * backend callback for retrieving the location of a pci device
 */
static int
hwloc_linux_backend_get_obj_cpuset(struct hwloc_backend *backend,
				   struct hwloc_backend *caller __hwloc_attribute_unused,
				   struct hwloc_obj *obj, hwloc_bitmap_t cpuset)
{
  struct hwloc_linux_backend_data_s *data = backend->private_data;
  char path[256];

  /* this callback is only used in the libpci backend for now */
  assert(obj->type == HWLOC_OBJ_PCI_DEVICE
	 || (obj->type == HWLOC_OBJ_BRIDGE && obj->attr->bridge.upstream_type == HWLOC_OBJ_BRIDGE_PCI));

  snprintf(path, sizeof(path), "/sys/bus/pci/devices/%04x:%02x:%02x.%01x/local_cpus",
	   obj->attr->pcidev.domain, obj->attr->pcidev.bus,
	   obj->attr->pcidev.dev, obj->attr->pcidev.func);
  if (!hwloc__read_path_as_cpumask(path, cpuset, data->root_fd)
      && !hwloc_bitmap_iszero(cpuset))
    return 0;
  return -1;
}



/*******************************
 ******* Linux component *******
 *******************************/

static void
hwloc_linux_backend_disable(struct hwloc_backend *backend)
{
  struct hwloc_linux_backend_data_s *data = backend->private_data;
#ifdef HAVE_OPENAT
  if (data->root_path)
    free(data->root_path);
  close(data->root_fd);
#endif
#ifdef HWLOC_HAVE_LIBUDEV
  if (data->udev)
    udev_unref(data->udev);
#endif
  free(data);
}

static struct hwloc_backend *
hwloc_linux_component_instantiate(struct hwloc_disc_component *component,
				  const void *_data1,
				  const void *_data2 __hwloc_attribute_unused,
				  const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;
  struct hwloc_linux_backend_data_s *data;
  const char * fsroot_path = _data1;
  int flags, root = -1;

  backend = hwloc_backend_alloc(component);
  if (!backend)
    goto out;

  data = malloc(sizeof(*data));
  if (!data) {
    errno = ENOMEM;
    goto out_with_backend;
  }

  backend->private_data = data;
  backend->flags = HWLOC_BACKEND_FLAG_NEED_LEVELS;
  backend->discover = hwloc_look_linuxfs;
  backend->get_obj_cpuset = hwloc_linux_backend_get_obj_cpuset;
  backend->notify_new_object = hwloc_linux_backend_notify_new_object;
  backend->disable = hwloc_linux_backend_disable;

  /* default values */
  data->arch = HWLOC_LINUX_ARCH_UNKNOWN;
  data->is_knl = 0;
  data->is_amd_with_CU = 0;
  data->is_real_fsroot = 1;
  data->root_path = NULL;
  if (!fsroot_path)
    fsroot_path = "/";

#ifdef HAVE_OPENAT
  root = open(fsroot_path, O_RDONLY | O_DIRECTORY);
  if (root < 0)
    goto out_with_data;

  if (strcmp(fsroot_path, "/")) {
    backend->is_thissystem = 0;
    data->is_real_fsroot = 0;
    data->root_path = strdup(fsroot_path);
  }

  /* Since this fd stays open after hwloc returns, mark it as
     close-on-exec so that children don't inherit it.  Stevens says
     that we should GETFD before we SETFD, so we do. */
  flags = fcntl(root, F_GETFD, 0);
  if (-1 == flags ||
      -1 == fcntl(root, F_SETFD, FD_CLOEXEC | flags)) {
      close(root);
      root = -1;
      goto out_with_data;
  }
#else
  if (strcmp(fsroot_path, "/")) {
    errno = ENOSYS;
    goto out_with_data;
  }
#endif
  data->root_fd = root;

#ifdef HWLOC_HAVE_LIBUDEV
  data->udev = NULL;
  if (data->is_real_fsroot) {
    data->udev = udev_new();
  }
#endif

  data->dumped_hwdata_dirname = getenv("HWLOC_DUMPED_HWDATA_DIR");
  if (!data->dumped_hwdata_dirname)
    data->dumped_hwdata_dirname = RUNSTATEDIR "/hwloc/";

  data->deprecated_classlinks_model = -2; /* never tried */
  data->mic_need_directlookup = -1; /* not initialized */
  data->mic_directlookup_id_max = -1; /* not initialized */

  return backend;

 out_with_data:
#ifdef HAVE_OPENAT
  if (data->root_path)
    free(data->root_path);
#endif
  free(data);
 out_with_backend:
  free(backend);
 out:
  return NULL;
}

static struct hwloc_disc_component hwloc_linux_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_CPU,
  "linux",
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  hwloc_linux_component_instantiate,
  50,
  NULL
};

const struct hwloc_component hwloc_linux_component = {
  HWLOC_COMPONENT_ABI,
  NULL, NULL,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_linux_disc_component
};




#ifdef HWLOC_HAVE_LINUXPCI

/***********************************
 ******* Linux PCI component *******
 ***********************************/

#define HWLOC_PCI_REVISION_ID 0x08
#define HWLOC_PCI_CAP_ID_EXP 0x10
#define HWLOC_PCI_CLASS_NOT_DEFINED 0x0000

static int
hwloc_look_linuxfs_pci(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  struct hwloc_backend *tmpbackend;
  hwloc_obj_t first_obj = NULL, last_obj = NULL;
  int root_fd = -1;
  DIR *dir;
  struct dirent *dirent;
  int res = 0;

  if (!(hwloc_topology_get_flags(topology) & (HWLOC_TOPOLOGY_FLAG_IO_DEVICES|HWLOC_TOPOLOGY_FLAG_WHOLE_IO)))
    return 0;

  if (hwloc_get_next_pcidev(topology, NULL)) {
    hwloc_debug("%s", "PCI objects already added, ignoring linuxpci backend.\n");
    return 0;
  }

  /* hackily find the linux backend to steal its fsroot */
  tmpbackend = topology->backends;
  while (tmpbackend) {
    if (tmpbackend->component == &hwloc_linux_disc_component) {
      root_fd = ((struct hwloc_linux_backend_data_s *) tmpbackend->private_data)->root_fd;
      hwloc_debug("linuxpci backend stole linux backend root_fd %d\n", root_fd);
      break;    }
    tmpbackend = tmpbackend->next;
  }
  /* take our own descriptor, either pointing to linux fsroot, or to / if not found */
  if (root_fd >= 0)
    root_fd = dup(root_fd);
  else
    root_fd = open("/", O_RDONLY | O_DIRECTORY);

  dir = hwloc_opendir("/sys/bus/pci/devices/", root_fd);
  if (!dir)
    goto out_with_rootfd;

  while ((dirent = readdir(dir)) != NULL) {
    unsigned domain, bus, dev, func;
    hwloc_obj_t obj;
    struct hwloc_pcidev_attr_s *attr;
    unsigned os_index;
    char path[64];
    char value[16];
    size_t ret;
    int fd;

    if (sscanf(dirent->d_name, "%04x:%02x:%02x.%01x", &domain, &bus, &dev, &func) != 4)
      continue;

    os_index = (domain << 20) + (bus << 12) + (dev << 4) + func;
    obj = hwloc_alloc_setup_object(HWLOC_OBJ_PCI_DEVICE, os_index);
    if (!obj)
      break;
    attr = &obj->attr->pcidev;

    attr->domain = domain;
    attr->bus = bus;
    attr->dev = dev;
    attr->func = func;

    /* default (unknown) values */
    attr->vendor_id = 0;
    attr->device_id = 0;
    attr->class_id = HWLOC_PCI_CLASS_NOT_DEFINED;
    attr->revision = 0;
    attr->subvendor_id = 0;
    attr->subdevice_id = 0;
    attr->linkspeed = 0;

    snprintf(path, sizeof(path), "/sys/bus/pci/devices/%s/vendor", dirent->d_name);
    if (!hwloc_read_path_by_length(path, value, sizeof(value), root_fd))
      attr->vendor_id = strtoul(value, NULL, 16);

    snprintf(path, sizeof(path), "/sys/bus/pci/devices/%s/device", dirent->d_name);
    if (!hwloc_read_path_by_length(path, value, sizeof(value), root_fd))
      attr->device_id = strtoul(value, NULL, 16);

    snprintf(path, sizeof(path), "/sys/bus/pci/devices/%s/class", dirent->d_name);
    if (!hwloc_read_path_by_length(path, value, sizeof(value), root_fd))
      attr->class_id = strtoul(value, NULL, 16) >> 8;

    snprintf(path, sizeof(path), "/sys/bus/pci/devices/%s/subsystem_vendor", dirent->d_name);
    if (!hwloc_read_path_by_length(path, value, sizeof(value), root_fd))
      attr->subvendor_id = strtoul(value, NULL, 16);

    snprintf(path, sizeof(path), "/sys/bus/pci/devices/%s/subsystem_device", dirent->d_name);
    if (!hwloc_read_path_by_length(path, value, sizeof(value), root_fd))
      attr->subdevice_id = strtoul(value, NULL, 16);

    snprintf(path, sizeof(path), "/sys/bus/pci/devices/%s/config", dirent->d_name);
    /* don't use hwloc_read_path_by_length() because we don't want the ending \0 */
    fd = hwloc_open(path, root_fd);
    if (fd >= 0) {
#define CONFIG_SPACE_CACHESIZE 256
      unsigned char config_space_cache[CONFIG_SPACE_CACHESIZE];
      unsigned offset;

      /* initialize the config space in case we fail to read it (missing permissions, etc). */
      memset(config_space_cache, 0xff, CONFIG_SPACE_CACHESIZE);
      ret = read(fd, config_space_cache, CONFIG_SPACE_CACHESIZE);
      (void) ret; /* we initialized config_space_cache in case we don't read enough, ignore the read length */
      close(fd);

      /* is this a bridge? */
      if (hwloc_pci_prepare_bridge(obj, config_space_cache) < 0)
	continue;

      /* get the revision */
      attr->revision = config_space_cache[HWLOC_PCI_REVISION_ID];

      /* try to get the link speed */
      offset = hwloc_pci_find_cap(config_space_cache, HWLOC_PCI_CAP_ID_EXP);
      if (offset > 0 && offset + 20 /* size of PCI express block up to link status */ <= CONFIG_SPACE_CACHESIZE)
	hwloc_pci_find_linkspeed(config_space_cache, offset, &attr->linkspeed);
    }

    if (first_obj)
      last_obj->next_sibling = obj;
    else
      first_obj = obj;
    last_obj = obj;
  }

  closedir(dir);

  dir = hwloc_opendir("/sys/bus/pci/slots/", root_fd);
  if (dir) {
    while ((dirent = readdir(dir)) != NULL) {
      char path[64];
      char buf[64];
      unsigned domain, bus, dev;
      if (dirent->d_name[0] == '.')
	continue;
      snprintf(path, sizeof(path), "/sys/bus/pci/slots/%s/address", dirent->d_name);
      if (!hwloc_read_path_by_length(path, buf, sizeof(buf), root_fd)
	  && sscanf(buf, "%x:%x:%x", &domain, &bus, &dev) == 3) {
	hwloc_obj_t obj = first_obj;
	while (obj) {
	  if (obj->attr->pcidev.domain == domain
	      && obj->attr->pcidev.bus == bus
	      && obj->attr->pcidev.dev == dev) {
	    hwloc_obj_add_info(obj, "PCISlot", dirent->d_name);
	  }
	  obj = obj->next_sibling;
	}
      }
    }
    closedir(dir);
  }

  res = hwloc_insert_pci_device_list(backend, first_obj);

 out_with_rootfd:
  close(root_fd);
  return res;
}

static struct hwloc_backend *
hwloc_linuxpci_component_instantiate(struct hwloc_disc_component *component,
				     const void *_data1 __hwloc_attribute_unused,
				     const void *_data2 __hwloc_attribute_unused,
				     const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;

  /* thissystem may not be fully initialized yet, we'll check flags in discover() */

  backend = hwloc_backend_alloc(component);
  if (!backend)
    return NULL;
  backend->flags = HWLOC_BACKEND_FLAG_NEED_LEVELS;
  backend->discover = hwloc_look_linuxfs_pci;
  return backend;
}

static struct hwloc_disc_component hwloc_linuxpci_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_MISC,
  "linuxpci",
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  hwloc_linuxpci_component_instantiate,
  19, /* after pci */
  NULL
};

const struct hwloc_component hwloc_linuxpci_component = {
  HWLOC_COMPONENT_ABI,
  NULL, NULL,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_linuxpci_disc_component
};

#endif /* HWLOC_HAVE_LINUXPCI */
