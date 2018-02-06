/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2015 Inria.  All rights reserved.
 * Copyright © 2009-2011 Université Bordeaux
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <stdio.h>
#include <string.h>
#include <errno.h>

#include <private/autogen/config.h> /* for HWLOC_WIN_SYS */
#include <hwloc.h>

/* check the binding functions */
hwloc_topology_t topology;
const struct hwloc_topology_support *support;

static void result_set(const char *msg, int err, int supported)
{
  const char *errmsg = strerror(errno);
  if (err)
    printf("%-40s: %sFAILED (%d, %s)\n", msg, supported?"":"X", errno, errmsg);
  else
    printf("%-40s: OK\n", msg);
}

static void result_get(const char *msg, hwloc_const_bitmap_t expected, hwloc_const_bitmap_t result, int err, int supported)
{
  const char *errmsg = strerror(errno);
  if (err)
    printf("%-40s: %sFAILED (%d, %s)\n", msg, supported?"":"X", errno, errmsg);
  else if (!expected || hwloc_bitmap_isequal(expected, result))
    printf("%-40s: OK\n", msg);
  else {
    char *expected_s, *result_s;
    hwloc_bitmap_asprintf(&expected_s, expected);
    hwloc_bitmap_asprintf(&result_s, result);
    printf("%-40s: expected %s, got %s\n", msg, expected_s, result_s);
    free(expected_s);
    free(result_s);
  }
}

static void test(hwloc_const_bitmap_t cpuset, int flags)
{
  hwloc_bitmap_t new_cpuset = hwloc_bitmap_alloc();
  result_set("Bind this singlethreaded process", hwloc_set_cpubind(topology, cpuset, flags), support->cpubind->set_thisproc_cpubind || support->cpubind->set_thisthread_cpubind);
  result_get("Get  this singlethreaded process", cpuset, new_cpuset, hwloc_get_cpubind(topology, new_cpuset, flags), support->cpubind->get_thisproc_cpubind || support->cpubind->get_thisthread_cpubind);
  result_set("Bind this thread", hwloc_set_cpubind(topology, cpuset, flags | HWLOC_CPUBIND_THREAD), support->cpubind->set_thisthread_cpubind);
  result_get("Get  this thread", cpuset, new_cpuset, hwloc_get_cpubind(topology, new_cpuset, flags | HWLOC_CPUBIND_THREAD), support->cpubind->get_thisthread_cpubind);
  result_set("Bind this whole process", hwloc_set_cpubind(topology, cpuset, flags | HWLOC_CPUBIND_PROCESS), support->cpubind->set_thisproc_cpubind);
  result_get("Get  this whole process", cpuset, new_cpuset, hwloc_get_cpubind(topology, new_cpuset, flags | HWLOC_CPUBIND_PROCESS), support->cpubind->get_thisproc_cpubind);

#ifdef HWLOC_WIN_SYS
  result_set("Bind process", hwloc_set_proc_cpubind(topology, GetCurrentProcess(), cpuset, flags | HWLOC_CPUBIND_PROCESS), support->cpubind->set_proc_cpubind);
  result_get("Get  process", cpuset, new_cpuset, hwloc_get_proc_cpubind(topology, GetCurrentProcess(), new_cpuset, flags | HWLOC_CPUBIND_PROCESS), support->cpubind->get_proc_cpubind);
  result_set("Bind thread", hwloc_set_thread_cpubind(topology, GetCurrentThread(), cpuset, flags | HWLOC_CPUBIND_THREAD), support->cpubind->set_thread_cpubind);
  result_get("Get  thread", cpuset, new_cpuset, hwloc_get_thread_cpubind(topology, GetCurrentThread(), new_cpuset, flags | HWLOC_CPUBIND_THREAD), support->cpubind->get_thread_cpubind);
#else /* !HWLOC_WIN_SYS */
  result_set("Bind whole process", hwloc_set_proc_cpubind(topology, getpid(), cpuset, flags | HWLOC_CPUBIND_PROCESS), support->cpubind->set_proc_cpubind);
  result_get("Get  whole process", cpuset, new_cpuset, hwloc_get_proc_cpubind(topology, getpid(), new_cpuset, flags | HWLOC_CPUBIND_PROCESS), support->cpubind->get_proc_cpubind);
  result_set("Bind process", hwloc_set_proc_cpubind(topology, getpid(), cpuset, flags), support->cpubind->set_proc_cpubind);
  result_get("Get  process", cpuset, new_cpuset, hwloc_get_proc_cpubind(topology, getpid(), new_cpuset, flags), support->cpubind->get_proc_cpubind);
#ifdef hwloc_thread_t
  result_set("Bind thread", hwloc_set_thread_cpubind(topology, pthread_self(), cpuset, flags), support->cpubind->set_thread_cpubind);
  result_get("Get  thread", cpuset, new_cpuset, hwloc_get_thread_cpubind(topology, pthread_self(), new_cpuset, flags), support->cpubind->get_thread_cpubind);
#endif
#endif /* !HWLOC_WIN_SYS */
  printf("\n");
  hwloc_bitmap_free(new_cpuset);
}

static void testmem(hwloc_const_bitmap_t nodeset, hwloc_membind_policy_t policy, int flags, int expected)
{
  hwloc_bitmap_t new_nodeset = hwloc_bitmap_alloc();
  hwloc_membind_policy_t newpolicy;
  void *area;
  size_t area_size = 1024;

  result_set("Bind this singlethreaded process memory", hwloc_set_membind(topology, nodeset, policy, flags), (support->membind->set_thisproc_membind || support->membind->set_thisthread_membind) && expected);
  result_get("Get  this singlethreaded process memory", nodeset, new_nodeset, hwloc_get_membind(topology, new_nodeset, &newpolicy, flags), (support->membind->get_thisproc_membind || support->membind->get_thisthread_membind) && expected);

  result_set("Bind this thread memory", hwloc_set_membind(topology, nodeset, policy, flags | HWLOC_MEMBIND_THREAD), support->membind->set_thisproc_membind && expected);
  result_get("Get  this thread memory", nodeset, new_nodeset, hwloc_get_membind(topology, new_nodeset, &newpolicy, flags | HWLOC_MEMBIND_THREAD), support->membind->get_thisproc_membind && expected);

  result_set("Bind this whole process memory", hwloc_set_membind(topology, nodeset, policy, flags | HWLOC_MEMBIND_PROCESS), support->membind->set_thisproc_membind && expected);
  result_get("Get  this whole process memory", nodeset, new_nodeset, hwloc_get_membind(topology, new_nodeset, &newpolicy, flags | HWLOC_MEMBIND_PROCESS), support->membind->get_thisproc_membind && expected);

#ifdef HWLOC_WIN_SYS
  result_set("Bind process memory", hwloc_set_proc_membind(topology, GetCurrentProcess(), nodeset, policy, flags), support->membind->set_proc_membind && expected);
  result_get("Get  process memory", nodeset, new_nodeset, hwloc_get_proc_membind(topology, GetCurrentProcess(), new_nodeset, &newpolicy, flags), support->membind->get_proc_membind && expected);
#else /* !HWLOC_WIN_SYS */
  result_set("Bind process memory", hwloc_set_proc_membind(topology, getpid(), nodeset, policy, flags), support->membind->set_proc_membind && expected);
  result_get("Get  process memory", nodeset, new_nodeset, hwloc_get_proc_membind(topology, getpid(), new_nodeset, &newpolicy, flags), support->membind->get_proc_membind && expected);
#endif /* !HWLOC_WIN_SYS */

  result_set("Bind area", hwloc_set_area_membind(topology, &new_nodeset, sizeof(new_nodeset), nodeset, policy, flags), support->membind->set_area_membind && expected);
  result_get("Get  area", nodeset, new_nodeset, hwloc_get_area_membind(topology, &new_nodeset, sizeof(new_nodeset), new_nodeset, &newpolicy, flags), support->membind->get_area_membind && expected);

  if (!(flags & HWLOC_MEMBIND_MIGRATE)) {
    result_set("Alloc bound area", (area = hwloc_alloc_membind(topology, area_size, nodeset, policy, flags)) == NULL, (support->membind->alloc_membind && expected) || !(flags & HWLOC_MEMBIND_STRICT));
    if (area) {
      memset(area, 0, area_size);
      result_get("Get   bound area", nodeset, new_nodeset, hwloc_get_area_membind(topology, area, area_size, new_nodeset, &newpolicy, flags), support->membind->get_area_membind && expected);
      result_get("Free  bound area", NULL, NULL, hwloc_free(topology, area, area_size), support->membind->alloc_membind && expected);
    }

    result_set("Alloc bound area through policy", (area = hwloc_alloc_membind_policy(topology, area_size, nodeset, policy, flags)) == NULL, (support->membind->set_thisproc_membind && expected) || !(flags & HWLOC_MEMBIND_STRICT));
    if (area) {
      memset(area, 0, area_size);
      result_get("Get   bound area", nodeset, new_nodeset, hwloc_get_area_membind(topology, area, area_size, new_nodeset, &newpolicy, flags), support->membind->get_area_membind && expected);
      result_get("Free  bound area", NULL, NULL, hwloc_free(topology, area, area_size), support->membind->alloc_membind && expected);
    }
  }
  printf("\n");
  hwloc_bitmap_free(new_nodeset);
}

static void testmem2(hwloc_const_bitmap_t set, int flags)
{
  printf("  default\n");
  testmem(set, HWLOC_MEMBIND_DEFAULT, flags, 1);
  printf("  firsttouch\n");
  testmem(set, HWLOC_MEMBIND_FIRSTTOUCH, flags, support->membind->firsttouch_membind);
  printf("  bound\n");
  testmem(set, HWLOC_MEMBIND_BIND, flags, support->membind->bind_membind);
  printf("  interleave\n");
  testmem(set, HWLOC_MEMBIND_INTERLEAVE, flags, support->membind->interleave_membind);
  printf("  nexttouch\n");
  testmem(set, HWLOC_MEMBIND_NEXTTOUCH, flags, support->membind->nexttouch_membind);
}

static void testmem3(hwloc_const_bitmap_t set)
{
  testmem2(set, 0);
  printf("now strict\n\n");
  testmem2(set, HWLOC_MEMBIND_STRICT);
  printf("now migrate\n\n");
  testmem2(set, HWLOC_MEMBIND_MIGRATE);
  printf("now strictly migrate\n\n");
  testmem2(set, HWLOC_MEMBIND_STRICT | HWLOC_MEMBIND_MIGRATE);
}

int main(void)
{
  hwloc_bitmap_t set;
  hwloc_obj_t obj;
  char *str = NULL;

  hwloc_topology_init(&topology);
  hwloc_topology_load(topology);

  support = hwloc_topology_get_support(topology);

  obj = hwloc_get_root_obj(topology);
  set = hwloc_bitmap_dup(obj->cpuset);

  while (hwloc_bitmap_isequal(obj->cpuset, set)) {
    if (!obj->arity)
      break;
    obj = obj->children[0];
  }

  hwloc_bitmap_asprintf(&str, set);
  printf("system set is %s\n", str);
  free(str);

  test(set, 0);
  printf("now strict\n");
  test(set, HWLOC_CPUBIND_STRICT);

  hwloc_bitmap_free(set);
  set = hwloc_bitmap_dup(obj->cpuset);
  hwloc_bitmap_asprintf(&str, set);
  printf("obj set is %s\n", str);
  free(str);

  test(set, 0);
  printf("now strict\n");
  test(set, HWLOC_CPUBIND_STRICT);

  hwloc_bitmap_singlify(set);
  hwloc_bitmap_asprintf(&str, set);
  printf("singlified to %s\n", str);
  free(str);

  test(set, 0);
  printf("now strict\n");
  test(set, HWLOC_CPUBIND_STRICT);
  hwloc_bitmap_free(set);

  printf("\n\nmemory tests\n\n");
  printf("complete node set\n");
  set = hwloc_bitmap_dup(hwloc_get_root_obj(topology)->cpuset);
  hwloc_bitmap_asprintf(&str, set);
  printf("i.e. cpuset %s\n", str);
  free(str);
  testmem3(set);
  hwloc_bitmap_free(set);

  obj = hwloc_get_obj_by_type(topology, HWLOC_OBJ_NUMANODE, 0);
  assert(obj);
  set = hwloc_bitmap_dup(obj->cpuset);
  hwloc_bitmap_asprintf(&str, set);
  printf("cpuset set is %s\n", str);
  free(str);

  testmem3(set);

  obj = hwloc_get_obj_by_type(topology, HWLOC_OBJ_NUMANODE, 1);
  if (obj) {
    hwloc_bitmap_or(set, set, obj->cpuset);
    hwloc_bitmap_asprintf(&str, set);
    printf("cpuset set is %s\n", str);
    free(str);

    testmem3(set);
  }
  hwloc_bitmap_free(set);

  hwloc_topology_destroy(topology);
  return 0;
}
