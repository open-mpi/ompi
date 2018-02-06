/*
 * Copyright © 2012-2017 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h> /* for HWLOC_WIN_SYS */
#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#include <errno.h>
#include <assert.h>

#ifndef HAVE_MKSTEMP
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
static inline int mkstemp(char *name)
{
  mktemp(name);
  return open(name, O_RDWR|O_CREAT, S_IRWXU);
}
#endif

/* mostly useful with valgrind, to check if backend cleanup properly */

static const char *get_backend_name(hwloc_topology_t topo)
{
  hwloc_obj_t root = hwloc_get_root_obj(topo);
  return hwloc_obj_get_info_by_name(root, "Backend");
}

static void assert_backend_name(hwloc_topology_t topo, const char *wanted)
{
  const char *found = get_backend_name(topo);
  int diff;
  assert(found);
  diff = strcmp(found, wanted);
  assert(!diff);
}

static void assert_foo_bar(hwloc_topology_t topo, int setornot)
{
  hwloc_obj_t root = hwloc_get_root_obj(topo);
  const char *found = hwloc_obj_get_info_by_name(root, "Foo");
  if (!setornot) {
    assert(!found);
  } else {
    int diff;
    assert(found);
    diff = strcmp(found, "Bar");
    assert(!diff);
  }
}

int main(void)
{
  hwloc_topology_t topology1, topology2;
  char *xmlbuf;
  int xmlbuflen;
  char xmlfile[] = "hwloc_backends.tmpxml.XXXXXX";
  char env[64];
  int xmlbufok = 0, xmlfileok = 0, xmlfilefd;
  const char *orig_backend_name;

  putenv("HWLOC_LIBXML_CLEANUP=1");

  printf("trying to export topology to XML buffer and file for later...\n");
  hwloc_topology_init(&topology1);
  hwloc_topology_load(topology1);
  orig_backend_name = get_backend_name(topology1);
  hwloc_obj_add_info(hwloc_get_root_obj(topology1), "Foo", "Bar");
  assert(hwloc_topology_is_thissystem(topology1));
  if (hwloc_topology_export_xmlbuffer(topology1, &xmlbuf, &xmlbuflen, 0) < 0)
    printf("XML buffer export failed (%s), ignoring\n", strerror(errno));
  else
    xmlbufok = 1;
  xmlfilefd = mkstemp(xmlfile);
  if (xmlfilefd < 0 || hwloc_topology_export_xml(topology1, xmlfile, 0) < 0)
    printf("XML file export failed (%s), ignoring\n", strerror(errno));
  else
    xmlfileok = 1;


  /* init+config+destroy without loading */
  printf("init...\n");
  hwloc_topology_init(&topology2);
  if (xmlfileok) {
    printf("switching to xml...\n");
    assert(!hwloc_topology_set_xml(topology2, xmlfile));
  }
  if (xmlbufok) {
    printf("switching to xmlbuffer...\n");
    assert(!hwloc_topology_set_xmlbuffer(topology2, xmlbuf, xmlbuflen));
  }
  printf("switching to synthetic...\n");
  hwloc_topology_set_synthetic(topology2, "pack:2 node:3 l1:2 pu:4");
  hwloc_topology_destroy(topology2);

  /* init+xml+load+destroy */
  if (xmlfileok) {
    printf("switching to xml and loading...\n");
    hwloc_topology_init(&topology2);
    assert(!hwloc_topology_set_xml(topology2, xmlfile));
    hwloc_topology_load(topology2);
    assert_backend_name(topology2, orig_backend_name);
    assert_foo_bar(topology2, 1);
    hwloc_topology_check(topology2);
    assert(!hwloc_topology_is_thissystem(topology2));
    hwloc_topology_destroy(topology2);
  }

  /* init+xmlbuf+load+destroy */
  if (xmlbufok) {
    printf("switching to xmlbuffer and loading...\n");
    hwloc_topology_init(&topology2);
    assert(!hwloc_topology_set_xmlbuffer(topology2, xmlbuf, xmlbuflen));
    hwloc_topology_load(topology2);
    assert_backend_name(topology2, orig_backend_name);
    assert_foo_bar(topology2, 1);
    hwloc_topology_check(topology2);
    assert(!hwloc_topology_is_thissystem(topology2));
    hwloc_topology_destroy(topology2);
  }

  /* init+synthetic+load+destroy */
  printf("switching to synthetic and loading...\n");
  hwloc_topology_init(&topology2);
  hwloc_topology_set_synthetic(topology2, "pack:2 node:3 l3i:2 pu:4");
  hwloc_topology_load(topology2);
  assert_backend_name(topology2, "Synthetic");
  assert_foo_bar(topology2, 0);
  assert(hwloc_get_nbobjs_by_type(topology2, HWLOC_OBJ_PU) == 2*3*2*4);
  hwloc_topology_check(topology2);
  assert(!hwloc_topology_is_thissystem(topology2));
  hwloc_topology_destroy(topology2);

  /* xmlenv+init+load+destroy */
  if (xmlfileok) {
    printf("switching to xml by env and loading...\n");
    snprintf(env, sizeof(env), "HWLOC_XMLFILE=%s", xmlfile);
    putenv(env);
    hwloc_topology_init(&topology2);
    hwloc_topology_load(topology2);
    assert_backend_name(topology2, orig_backend_name);
    assert_foo_bar(topology2, 1);
    hwloc_topology_check(topology2);
    assert(!hwloc_topology_is_thissystem(topology2));
    hwloc_topology_destroy(topology2);
  }

  /* syntheticenv+init+load+destroy, synthetic env overrides xml */
  printf("switching to synthetic by env and loading...\n");
  putenv("HWLOC_SYNTHETIC=node:3 pu:3");
  hwloc_topology_init(&topology2);
  hwloc_topology_load(topology2);
  assert_backend_name(topology2, "Synthetic");
  assert_foo_bar(topology2, 0);
  assert(hwloc_get_nbobjs_by_type(topology2, HWLOC_OBJ_PU) == 3*3);
  hwloc_topology_check(topology2);
  assert(!hwloc_topology_is_thissystem(topology2));
  hwloc_topology_destroy(topology2);

  /* componentsenv+init+load+destroy for testing defaults, overrides synthetic/xml/fsroot envs */
  printf("switching to default components by env and loading...\n");
  putenv("HWLOC_COMPONENTS=,"); /* don't set to empty since it means 'unset' on windows */
  hwloc_topology_init(&topology2);
  hwloc_topology_load(topology2);
  assert_backend_name(topology2, orig_backend_name);
  assert_foo_bar(topology2, 0);
  hwloc_topology_check(topology2);
  assert(hwloc_topology_is_thissystem(topology2));
  hwloc_topology_destroy(topology2);

  if (xmlbufok)
    hwloc_free_xmlbuffer(topology1, xmlbuf);
  if (xmlfilefd >= 0) {
    unlink(xmlfile);
    close(xmlfilefd);
  }
  hwloc_topology_destroy(topology1);

  return 0;
}
