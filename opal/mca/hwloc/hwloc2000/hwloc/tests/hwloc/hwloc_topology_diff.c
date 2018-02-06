/*
 * Copyright © 2013-2017 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

int main(void)
{
  hwloc_topology_t topo1, topo2, topo3;
  hwloc_topology_diff_t diff, diff2, tmpdiff;
  hwloc_obj_t obj;
  char *xmlbuffer;
  int xmlbuflen;
  char *refname;
  int err;

  putenv("HWLOC_LIBXML_CLEANUP=1");

  hwloc_topology_init(&topo1);
  hwloc_topology_load(topo1);
  printf("duplicate topo1 into topo2\n");
  hwloc_topology_dup(&topo2, topo1);

  printf("check that topo2 is identical\n");
  err = hwloc_topology_diff_build(topo1, topo2, 0, &diff);
  assert(err == 0);
  assert(!diff);

  printf("add a new info to topo2\n");
  obj = hwloc_get_root_obj(topo1);
  hwloc_obj_add_info(obj, "Foo", "Bar");
  printf("check that topo2 cannot be diff'ed from topo1\n");
  err = hwloc_topology_diff_build(topo1, topo2, 0, &diff);
  assert(err == 1);
  assert(diff->generic.type == HWLOC_TOPOLOGY_DIFF_TOO_COMPLEX);
  assert(diff->generic.next == NULL);
  assert(diff->too_complex.obj_depth == 0);
  assert(diff->too_complex.obj_index == 0);
  hwloc_topology_diff_destroy(diff);

  printf("add a similar info to topo1, and change memory sizes\n");
  obj = hwloc_get_root_obj(topo2);
  hwloc_obj_add_info(obj, "Foo", "Bar2");

  obj = hwloc_get_obj_by_type(topo2, HWLOC_OBJ_NUMANODE, 0);
  obj->attr->numanode.local_memory += 32*4096;

  printf("check that topo2 is now properly diff'ed\n");
  err = hwloc_topology_diff_build(topo1, topo2, 0, &diff);
  assert(err == 0);
  tmpdiff = diff;
  assert(tmpdiff->generic.type == HWLOC_TOPOLOGY_DIFF_OBJ_ATTR);
  assert(tmpdiff->obj_attr.obj_depth == 0);
  assert(tmpdiff->obj_attr.obj_index == 0);
  assert(tmpdiff->obj_attr.diff.generic.type == HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_INFO);
  err = strcmp(tmpdiff->obj_attr.diff.string.name, "Foo");
  assert(!err);
  err = strcmp(tmpdiff->obj_attr.diff.string.oldvalue, "Bar");
  assert(!err);
  err = strcmp(tmpdiff->obj_attr.diff.string.newvalue, "Bar2");
  assert(!err);
  tmpdiff = tmpdiff->generic.next;
  assert(tmpdiff->generic.type == HWLOC_TOPOLOGY_DIFF_OBJ_ATTR);
  assert(tmpdiff->obj_attr.obj_depth == hwloc_get_type_depth(topo1, HWLOC_OBJ_NUMANODE));
  assert(tmpdiff->obj_attr.obj_index == 0);
  assert(tmpdiff->obj_attr.diff.generic.type == HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_SIZE);
  assert(tmpdiff->obj_attr.diff.uint64.newvalue - tmpdiff->obj_attr.diff.uint64.oldvalue == 32*4096);
  assert(tmpdiff->generic.next == NULL);

  printf("apply the diff to new duplicate topo3 of topo1\n");
  hwloc_topology_dup(&topo3, topo1);
  err = hwloc_topology_diff_apply(topo3, diff, 0);
  assert(!err);
  printf("check that topo2 and topo3 are identical\n");
  err = hwloc_topology_diff_build(topo2, topo3, 0, &diff2);
  assert(err == 0);
  assert(!diff2);

  printf("apply the reverse diff to topo2\n");
  err = hwloc_topology_diff_apply(topo2, diff, HWLOC_TOPOLOGY_DIFF_APPLY_REVERSE);
  assert(!err);
  printf("check that topo2 and topo1 are identical\n");
  err = hwloc_topology_diff_build(topo1, topo2, 0, &diff2);
  assert(err == 0);
  assert(!diff2);

  printf("exporting and reloading diff from XML buffer without refname\n");
  err = hwloc_topology_diff_export_xmlbuffer(diff, NULL, &xmlbuffer, &xmlbuflen);
  assert(!err);
  hwloc_topology_diff_destroy(diff);
  err = hwloc_topology_diff_load_xmlbuffer(xmlbuffer, xmlbuflen, &diff2, &refname);
  assert(!err);
  assert(diff2);
  assert(!refname);
  assert(!err);
  hwloc_free_xmlbuffer(topo1, xmlbuffer);

  printf("exporting and reloading diff from XML buffer with refname\n");
  err = hwloc_topology_diff_export_xmlbuffer(diff2, "foobar", &xmlbuffer, &xmlbuflen);
  assert(!err);
  hwloc_topology_diff_destroy(diff2);
  err = hwloc_topology_diff_load_xmlbuffer(xmlbuffer, xmlbuflen, &diff, &refname);
  assert(!err);
  assert(diff);
  err = strcmp(refname, "foobar");
  assert(!err);
  free(refname);
  hwloc_free_xmlbuffer(topo1, xmlbuffer);

  tmpdiff = diff;
  assert(tmpdiff->generic.type == HWLOC_TOPOLOGY_DIFF_OBJ_ATTR);
  assert(tmpdiff->obj_attr.obj_depth == 0);
  assert(tmpdiff->obj_attr.obj_index == 0);
  assert(tmpdiff->obj_attr.diff.generic.type == HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_INFO);
  err = strcmp(tmpdiff->obj_attr.diff.string.name, "Foo");
  assert(!err);
  err = strcmp(tmpdiff->obj_attr.diff.string.oldvalue, "Bar");
  assert(!err);
  err = strcmp(tmpdiff->obj_attr.diff.string.newvalue, "Bar2");
  assert(!err);
  tmpdiff = tmpdiff->generic.next;
  assert(tmpdiff->generic.type == HWLOC_TOPOLOGY_DIFF_OBJ_ATTR);
  assert(tmpdiff->obj_attr.obj_depth == hwloc_get_type_depth(topo1, HWLOC_OBJ_NUMANODE));
  assert(tmpdiff->obj_attr.obj_index == 0);
  assert(tmpdiff->obj_attr.diff.generic.type == HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_SIZE);
  assert(tmpdiff->obj_attr.diff.uint64.newvalue - tmpdiff->obj_attr.diff.uint64.oldvalue == 32*4096);
  assert(tmpdiff->generic.next == NULL);

  printf("reapplying to topo2\n");
  err = hwloc_topology_diff_apply(topo2, diff, 0);
  assert(!err);
  printf("check that topo2 and topo3 are again identical\n");
  err = hwloc_topology_diff_build(topo2, topo3, 0, &diff2);
  assert(err == 0);
  assert(!diff2);

  hwloc_topology_diff_destroy(diff);

  printf("adding new key to the bottom of topo3\n");
  obj = hwloc_get_obj_by_type(topo3, HWLOC_OBJ_PU, 0);
  assert(obj);
  hwloc_obj_add_info(obj, "Bar", "Baz3");
  printf("check that diff fails at the last entry\n");
  err = hwloc_topology_diff_build(topo1, topo3, 0, &diff);
  assert(err == 1);
  assert(diff);
  tmpdiff = diff;
  assert(tmpdiff->generic.type == HWLOC_TOPOLOGY_DIFF_OBJ_ATTR);
  tmpdiff = tmpdiff->generic.next;
  assert(tmpdiff->generic.type == HWLOC_TOPOLOGY_DIFF_TOO_COMPLEX);
  tmpdiff = tmpdiff->generic.next;
  assert(tmpdiff->generic.type == HWLOC_TOPOLOGY_DIFF_OBJ_ATTR);
  assert(tmpdiff->generic.next == NULL);
  hwloc_topology_diff_destroy(diff);

  printf("adding similar key to topo1\n");
  obj = hwloc_get_obj_by_type(topo1, HWLOC_OBJ_PU, 0);
  assert(obj);
  hwloc_obj_add_info(obj, "Bar", "Baz1");
  printf("checking that topo3 diff fails to reverse apply to topo2\n");
  err = hwloc_topology_diff_build(topo1, topo3, 0, &diff);
  assert(err == 0);
  assert(diff);
  err = hwloc_topology_diff_apply(topo2, diff, HWLOC_TOPOLOGY_DIFF_APPLY_REVERSE);
  assert(err == -2);
  hwloc_topology_diff_destroy(diff);

  hwloc_topology_destroy(topo3);
  hwloc_topology_destroy(topo2);
  hwloc_topology_destroy(topo1);

  return 0;
}
