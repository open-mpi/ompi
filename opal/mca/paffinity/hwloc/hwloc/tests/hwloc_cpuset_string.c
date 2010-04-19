/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

#include <private/config.h>
#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

/* check hwloc_cpuset_asprintf(), hwloc_obj_cpuset_snprintf() and hwloc_cpuset_from_string() */

int main(void)
{
  hwloc_topology_t topology;
  unsigned depth;
  char *string = NULL;
  int stringlen, len;
  hwloc_obj_t obj;
  hwloc_cpuset_t set;

  hwloc_topology_init(&topology);
  hwloc_topology_set_synthetic(topology, "6 5 4 3 2");
  hwloc_topology_load(topology);
  depth = hwloc_topology_get_depth(topology);

  obj = hwloc_get_root_obj(topology);
  stringlen = hwloc_cpuset_asprintf(&string, obj->cpuset);
  printf("system cpuset is %s\n", string);
  set = hwloc_cpuset_alloc();
  hwloc_cpuset_from_string(set, string);
  assert(hwloc_cpuset_isequal(set, obj->cpuset));
  hwloc_cpuset_free(set);
  printf("system cpuset converted back and forth, ok\n");

  printf("truncating system cpuset to NULL buffer\n");
  len = hwloc_obj_cpuset_snprintf(NULL, 0, 1, &obj);
  assert(len == stringlen);

  printf("truncating system cpuset to 0 char (no modification)\n");
  memset(string, 'X', 1);
  string[1] = 0;
  len = hwloc_obj_cpuset_snprintf(string, 0, 1, &obj);
  assert(len == stringlen);
  assert(string[0] == 'X');

  printf("truncating system cpuset to 1 char (empty string)\n");
  memset(string, 'X', 2);
  string[2] = 0;
  len = hwloc_obj_cpuset_snprintf(string, 1, 1, &obj);
  printf("got %s\n", string);
  assert(len == stringlen);
  assert(string[0] == 0);
  assert(string[1] == 'X');

  printf("truncating system cpuset to 10 chars (single 32bit subset except last char)\n");
  memset(string, 'X', 11);
  string[11] = 0;
  len = hwloc_obj_cpuset_snprintf(string, 10, 1, &obj);
  printf("got %s\n", string);
  assert(len == stringlen);
  assert(string[8] == 'f');
  assert(string[9] == 0);
  assert(string[10] == 'X');

  printf("truncating system cpuset to 11 chars (single 32bit subset)\n");
  memset(string, 'X', 12);
  string[12] = 0;
  len = hwloc_obj_cpuset_snprintf(string, 11, 1, &obj);
  printf("got %s\n", string);
  assert(len == stringlen);
  assert(string[9] == 'f');
  assert(string[10] == 0);
  assert(string[11] == 'X');

  printf("truncating system cpuset to 23 chars (two 32bit subsets with ending comma)\n");
  memset(string, 'X', 24);
  string[24] = 0;
  len = hwloc_obj_cpuset_snprintf(string, 23, 1, &obj);
  printf("got %s\n", string);
  assert(len == stringlen);
  assert(string[20] == 'f');
  assert(string[21] == ',');
  assert(string[22] == 0);
  assert(string[23] == 'X');

  printf("truncating system cpuset to 51 chars (truncate to four and a half 32bit subsets)\n");
  memset(string, 'X', 52);
  string[52] = 0;
  len = hwloc_obj_cpuset_snprintf(string, 51, 1, &obj);
  printf("got %s\n", string);
  assert(len == stringlen);
  assert(string[49] == 'f');
  assert(string[50] == 0);
  assert(string[51] == 'X');

  obj = hwloc_get_obj_by_depth(topology, depth-1, 0);
  hwloc_obj_cpuset_snprintf(string, stringlen+1, 1, &obj);
  printf("first cpu cpuset is %s\n", string);
  set = hwloc_cpuset_alloc();
  hwloc_cpuset_from_string(set, string);
  assert(hwloc_cpuset_isequal(set, obj->cpuset));
  hwloc_cpuset_free(set);
  printf("first cpu cpuset converted back and forth, ok\n");

  obj = hwloc_get_obj_by_depth(topology, depth-1, hwloc_get_nbobjs_by_depth(topology, depth-1) - 1);
  hwloc_obj_cpuset_snprintf(string, stringlen+1, 1, &obj);
  printf("last cpu cpuset is %s\n", string);
  set = hwloc_cpuset_alloc();
  hwloc_cpuset_from_string(set, string);
  assert(hwloc_cpuset_isequal(set, obj->cpuset));
  hwloc_cpuset_free(set);
  printf("last cpu cpuset converted back and forth, ok\n");

//  hwloc_cpuset_from_string(set, "1,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,2,4,8,10,20\n");
//  char *s = hwloc_cpuset_printf_value(&set);
//  printf("%s\n", s);
//  free(s);
//  will be truncated after ",4," since it's too large

  free(string);

  hwloc_topology_destroy(topology);

  return 0;
}
