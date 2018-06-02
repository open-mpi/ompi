/*
 * Copyright © 2011-2018 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>
#include <hwloc/shmem.h>
#include <private/misc.h> /* for hwloc_getpagesize() */

#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>

#define EXIT_SKIP 77

/* must match the header defined in hwloc/shmem.c */
struct header {
  uint32_t header_version;
  uint32_t header_length;
  uint64_t mmap_address;
  uint64_t mmap_length;
};

static int adopt(int fd, unsigned long fileoffset, unsigned long mmap_address, unsigned long mmap_length, int synthetic_with_distances)
{
  static hwloc_topology_t adopted;
  char *xmlbuf;
  int xmlbuflen;
  char *origxmlbuf;
  struct hwloc_distances_s *distances;
  unsigned nr = 1;
  int err;
  int ret = EXIT_SKIP;

  err = lseek(fd, 0, SEEK_SET);
  assert(!err);

  printf(" reading XML dump\n");
  origxmlbuf = malloc(fileoffset);
  assert(origxmlbuf);
  err = read(fd, origxmlbuf, fileoffset);
  assert(err > 0);

  printf(" adopting from file at offset %lu with address 0x%lx len %lu\n", fileoffset, mmap_address, mmap_length);

  err = hwloc_shmem_topology_adopt(&adopted, fd, fileoffset, (void*)(uintptr_t)mmap_address, mmap_length, 0);
  if (err == -1 && errno == EBUSY) {
    /* may fail on 32bits and on some OS (e.g. darwin from time to time), and even on Linux/64bits if unlucky */
    fprintf(stderr, "Failed to shmem adopt, requested mapping is busy\n");
    goto out_with_origxmlbuf;
  }
  assert(!err);
  printf(" adopted OK\n");

  err = hwloc_distances_get_by_type(adopted, HWLOC_OBJ_NUMANODE, &nr, &distances, 0, 0);
  assert(!err);
  if (synthetic_with_distances) {
    assert(nr == 1);
    assert(distances->nbobjs == 3);
    assert(distances->kind == (HWLOC_DISTANCES_KIND_MEANS_LATENCY|HWLOC_DISTANCES_KIND_FROM_USER));
    hwloc_distances_release(adopted, distances);
    printf(" distances OK\n");
  }

  err = hwloc_topology_export_xmlbuffer(adopted, &xmlbuf, &xmlbuflen, 0);
  assert(!err);
  printf(" XML export %d bytes\n", xmlbuflen);
  assert((unsigned long) xmlbuflen < fileoffset);
  assert(!memcmp(origxmlbuf, xmlbuf, xmlbuflen));
  hwloc_free_xmlbuffer(adopted, xmlbuf);
  printf(" XML export is identical to original\n");

  hwloc_topology_destroy(adopted);
  printf(" destroyed\n");

  ret = EXIT_SUCCESS;

 out_with_origxmlbuf:
  free(origxmlbuf);
  return ret;
}

static unsigned long
find_mmap_addr(unsigned long length)
{
  unsigned long addr;
  void *tmp_mmap;
  int err;

  /* try to find a good address starting from something in the middle of the entire/full address space */
#if SIZEOF_VOID_P == 8
  addr = 0x8000000000000000UL;
#else
  addr = 0x80000000UL;
#endif
  printf("testing mmaps to find room for length %lu\n", length);

again:
  tmp_mmap = mmap((void*)(uintptr_t)addr, length, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_SHARED, -1, 0);
  if (tmp_mmap != MAP_FAILED) {
    err = munmap((void*)(uintptr_t)tmp_mmap, length);
    assert(!err);
    if (tmp_mmap == (void*)(uintptr_t) addr) {
      /* worked! */
      printf(" test mmap at 0x%lx succeeded, let's use that!\n", addr);
      return addr;
    }
    printf(" test mmap at 0x%lx returned another address\n", addr);
  } else
    printf(" test mmap at 0x%lx failed (errno %d)\n", addr, errno);
  /* couldn't map there, try again with a smaller address */
  addr >>= 3;
  if (addr)
    goto again;

  return 0;
}

static int test(hwloc_topology_t orig, const char *callname) {
  unsigned long forced_addr;
  unsigned long fileoffset;
  size_t shmem_length;
  int synthetic_with_distances = (hwloc_obj_get_info_by_name(hwloc_get_root_obj(orig), "SyntheticDescription") != NULL);
  char tmpname[] = "/tmp/hwloc_test_shmem.XXXXXX";
  char cmd[512];
  struct stat st;
  int fd, err;
  int ret = EXIT_SKIP;

  printf("opening temporary file\n");
  fd = mkstemp(tmpname);
  if (fd < 0) {
    perror("mkstemp");
    goto out;
  }
  printf("opened %s\n", tmpname);

  printf("exporting XML\n");
  err = hwloc_topology_export_xml(orig, tmpname, 0);
  assert(!err);
  err = stat(tmpname, &st);
  assert(!err);
  printf("exported %lu bytes\n", (unsigned long) st.st_size);
  fileoffset = st.st_size+1; /* skip a couple bytes to make sure the XML is don" */
  fileoffset = (fileoffset + hwloc_getpagesize() - 1) &~(hwloc_getpagesize() - 1);
  printf("will mmap at file offset %lu\n", fileoffset);

  err = hwloc_shmem_topology_get_length(orig, &shmem_length, 0);
  assert(!err);
  printf("need mmap length %lu\n", (unsigned long) shmem_length);

  forced_addr = find_mmap_addr((unsigned long) shmem_length);
  if (!forced_addr)
    goto out_with_fd;

  printf("write to shmem at address 0x%lx in file %s offset %lu\n", forced_addr, tmpname, fileoffset);
  err = hwloc_shmem_topology_write(orig, fd, fileoffset, (void*)(uintptr_t)forced_addr, shmem_length, 0);
  if (err == -1 && errno == EBUSY) {
    fprintf(stderr, "Failed to shmem write, requested mapping is busy\n");
    goto out_with_fd;
  }
  assert(!err);
  printf("wrote length %lu\n", (unsigned long) shmem_length);

  printf("adopting locally\n");
  ret = adopt(fd, fileoffset, forced_addr, shmem_length, synthetic_with_distances);
  assert(ret == EXIT_SUCCESS || ret == EXIT_SKIP);

  printf("adopting in other child process\n");
  snprintf(cmd, sizeof(cmd), "%s %s %lu 0x%lx %lu %d", callname, tmpname, fileoffset, forced_addr, (unsigned long) shmem_length, synthetic_with_distances);
  printf("running command %s\n", cmd);
  err = system(cmd);
  assert(WIFEXITED(err));
  printf("child process returned %d\n", WEXITSTATUS(err));
  assert(WEXITSTATUS(err) == EXIT_SUCCESS || WEXITSTATUS(err) == EXIT_SKIP);

  /* we caught errors above.
   * return SKIP if both returned SKIP. otherwise SUCCESS
   */
  if (WEXITSTATUS(err) == EXIT_SKIP && ret == EXIT_SKIP)
    ret = EXIT_SKIP;
  else
    ret = EXIT_SUCCESS;

 out_with_fd:
  close(fd);
  unlink(tmpname);
 out:
  return ret;
}

int main(int argc, char *argv[])
{
  static hwloc_topology_t orig;
  hwloc_obj_t nodes[3];
  uint64_t node_distances[9];
  unsigned i,j;
  int err, ret, ret2;

  if (argc > 1) {
    int fd;
    unsigned long forced_addr;
    unsigned long fileoffset;
    size_t shmem_length;
    int synthetic_with_distances;

    if (argc < 6) {
      printf("needs 5 arguments\n");
      return EXIT_FAILURE;
    }

    printf(" opening %s\n", argv[1]);
    fd = open(argv[1], O_RDONLY);
    if (fd < 0) {
      perror("open");
      return EXIT_FAILURE;
    }

    fileoffset = strtoul(argv[2], NULL, 0);
    forced_addr = strtoul(argv[3], NULL, 0);
    shmem_length = strtoul(argv[4], NULL, 0);
    synthetic_with_distances = atoi(argv[5]);

    ret = adopt(fd, fileoffset, forced_addr, shmem_length, synthetic_with_distances);
    close(fd);
    exit(ret);
  }

  printf("########################\n");
  printf("creating native topology\n");
  err = hwloc_topology_init(&orig);
  assert(!err);
  err = hwloc_topology_set_all_types_filter(orig, HWLOC_TYPE_FILTER_KEEP_ALL);
  assert(!err);
  err = hwloc_topology_load(orig);
  assert(!err);

  ret = test(orig, argv[0]);

  printf("destroying original\n");
  hwloc_topology_destroy(orig);

  printf("###############################################\n");
  printf("creating synthetic topo with distances topology\n");
  err = hwloc_topology_init(&orig);
  assert(!err);
  err = hwloc_topology_set_synthetic(orig, "node:3 core:2 pu:4");
  assert(!err);
  err = hwloc_topology_load(orig);
  assert(!err);

  printf("adding distance matrix\n");
  for(i=0; i<3; i++) {
    nodes[i] = hwloc_get_obj_by_type(orig, HWLOC_OBJ_NUMANODE, i);
    for(j=0; j<3; j++)
      node_distances[i*3+j] = (i == j ? 10 : 20);
  }
  err = hwloc_distances_add(orig, 3, nodes, node_distances,
                            HWLOC_DISTANCES_KIND_MEANS_LATENCY|HWLOC_DISTANCES_KIND_FROM_USER,
                            HWLOC_DISTANCES_ADD_FLAG_GROUP);
  assert(!err);

  ret2 = test(orig, argv[0]);

  printf("destroying original\n");
  hwloc_topology_destroy(orig);

  /* we caught errors above.
   * return SKIP if both returned SKIP. otherwise SUCCESS
   */
  if (ret == EXIT_SKIP && ret2 == EXIT_SKIP)
    ret = EXIT_SKIP;
  else
    ret = EXIT_SUCCESS;

  return ret;
}
