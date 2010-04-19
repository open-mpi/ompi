/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

#include <stdio.h>
#include <string.h>
#include <errno.h>

#include <private/config.h>
#include <hwloc.h>
#include <private/config.h>

/* check the binding functions */
hwloc_topology_t topology;
const struct hwloc_topology_support *support;

static void result_set(const char *msg, int err, int supported)
{
  const char *errmsg = strerror(errno);
  if (err)
    printf("%-40s: FAILED (%d, %s)%s\n", msg, errno, errmsg, supported ? "" : " (expected)");
  else
    printf("%-40s: OK%s\n", msg, supported ? "" : " (unexpected)");
}

static void result_get(const char *msg, hwloc_const_cpuset_t expected, hwloc_const_cpuset_t result, int err, int supported)
{
  const char *errmsg = strerror(errno);
  if (err)
    printf("%-40s: FAILED (%d, %s)%s\n", msg, errno, errmsg, supported ? "" : " (expected)");
  else if (hwloc_cpuset_isequal(expected, result))
    printf("%-40s: OK%s\n", msg, supported ? "" : " (unexpected)");
  else {
    char *expected_s, *result_s;
    hwloc_cpuset_asprintf(&expected_s, expected);
    hwloc_cpuset_asprintf(&result_s, result);
    printf("%-40s: expected %s, got %s\n", msg, expected_s, result_s);
  }
}

static void test(hwloc_const_cpuset_t cpuset, int flags)
{
  hwloc_cpuset_t new_cpuset = hwloc_cpuset_alloc();
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
  hwloc_cpuset_free(new_cpuset);
}

int main(void)
{
  hwloc_cpuset_t set;
  hwloc_obj_t obj;
  char *str = NULL;

  hwloc_topology_init(&topology);
  hwloc_topology_load(topology);

  support = hwloc_topology_get_support(topology);

  obj = hwloc_get_root_obj(topology);
  set = hwloc_cpuset_dup(obj->cpuset);

  while (hwloc_cpuset_isequal(obj->cpuset, set)) {
    if (!obj->arity)
      break;
    obj = obj->children[0];
  }

  hwloc_cpuset_asprintf(&str, set);
  printf("system set is %s\n", str);
  free(str);

  test(set, 0);

  printf("now strict\n");
  test(set, HWLOC_CPUBIND_STRICT);

  hwloc_cpuset_free(set);
  set = hwloc_cpuset_dup(obj->cpuset);
  hwloc_cpuset_asprintf(&str, set);
  printf("obj set is %s\n", str);
  free(str);

  test(set, 0);

  printf("now strict\n");
  test(set, HWLOC_CPUBIND_STRICT);

  hwloc_cpuset_singlify(set);
  hwloc_cpuset_asprintf(&str, set);
  printf("singlified to %s\n", str);
  free(str);

  test(set, 0);

  printf("now strict\n");
  test(set, HWLOC_CPUBIND_STRICT);

  hwloc_cpuset_free(set);
  hwloc_topology_destroy(topology);
  return 0;
}
