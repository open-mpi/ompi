/*
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009 Université Bordeaux
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

/* check that object userdata is properly initialized */

#define RANDOMSTRINGLENGTH 128
#define RANDOMSTRINGSHORTTESTS 5
#define RANDOMSTRINGLONGTESTS 9
static char *randomstring;

static void check(hwloc_topology_t topology)
{
  int depth, i;
  unsigned j;

  depth = hwloc_topology_get_depth(topology);
  for(i=0; i<depth; i++) {
    for(j=0; j<hwloc_get_nbobjs_by_depth(topology, i); j++) {
      assert(hwloc_get_obj_by_depth(topology, i, j)->userdata == NULL);
    }
  }
}

static void export_cb(void *reserved, hwloc_topology_t topo, hwloc_obj_t obj)
{
  char tmp[32];
  int err;
  unsigned i;

  sprintf(tmp, "%016llx", (unsigned long long) (uintptr_t) obj->userdata);
  err = hwloc_export_obj_userdata(reserved, topo, obj, "MyName", tmp, 16);
  assert(err >= 0);

  err = hwloc_export_obj_userdata(reserved, topo, obj, NULL, "", 0);
  assert(err >= 0);

  err = hwloc_export_obj_userdata_base64(reserved, topo, obj, NULL, "", 0);
  assert(err >= 0);

  for(i=0; i<RANDOMSTRINGSHORTTESTS; i++) {
    sprintf(tmp, "EncodedShort%u", i);
    err = hwloc_export_obj_userdata_base64(reserved, topo, obj, tmp, randomstring+i, i);
    assert(err >= 0);
  }
  for(i=0; i<RANDOMSTRINGLONGTESTS; i++) {
    sprintf(tmp, "EncodedLong%u", i);
    err = hwloc_export_obj_userdata_base64(reserved, topo, obj, tmp, randomstring+(i+1)/2, RANDOMSTRINGLENGTH-i);
    assert(err >= 0);
  }
}

static void import_cb(hwloc_topology_t topo __hwloc_attribute_unused, hwloc_obj_t obj, const char *name, const void *buffer, size_t length)
{
  int err;
  char tmp[17];
  uint64_t val;

  if (!name) {
    assert(!*(char*)buffer);

  } else if (!strcmp("MyName", name)) {
    assert(length == 16);
    memcpy(tmp, buffer, 16);
    tmp[16] = '\0';

    val = strtoull(buffer, NULL, 0);

    switch (val) {
    case 0x1:
      assert(obj->type == HWLOC_OBJ_MACHINE);
      obj->userdata = (void *)(uintptr_t) 0x4;
      break;
    case 0x2:
      assert(obj->type == HWLOC_OBJ_L2CACHE);
      obj->userdata = (void *)(uintptr_t) 0x5;
      break;
    case 0x3:
      assert(obj->type == HWLOC_OBJ_PU);
      obj->userdata = (void *)(uintptr_t) 0x6;
      break;
    default:
      assert(0);
    }

  } else if (!strncmp("EncodedShort", name, 12)) {
    unsigned i = atoi(name+12);
    assert(i <= RANDOMSTRINGSHORTTESTS-1);
    assert(i == (unsigned) length);
    err = memcmp(buffer, randomstring+i, length);
    assert(!err);

  } else if (!strncmp("EncodedLong", name, 11)) {
    unsigned i = atoi(name+11);
    assert(i <= RANDOMSTRINGLONGTESTS-1);
    assert(RANDOMSTRINGLENGTH-i == (unsigned) length);
    err = memcmp(buffer, randomstring+(i+1)/2, length);
    assert(!err);

  } else
    assert(0);
}

int main(void)
{
  hwloc_topology_t topology, reimport;
  hwloc_obj_t obj1, obj2, obj3;
  char *xmlbuf;
  int xmlbuflen;

  randomstring = malloc(RANDOMSTRINGLENGTH);
  /* keep it uninitialized, we want binary data */

  /* check the real topology */
  hwloc_topology_init(&topology);
  hwloc_topology_load(topology);
  check(topology);
  assert(hwloc_topology_get_userdata(topology) == NULL);
  hwloc_topology_destroy(topology);

  /* check a synthetic topology */
  hwloc_topology_init(&topology);
  hwloc_topology_set_userdata(topology, (void *)(uintptr_t)0x987654);
  hwloc_topology_set_synthetic(topology, "6 5 4 3 2");
  hwloc_topology_load(topology);
  check(topology);

  /* now place some userdata and see if importing/exporting works well */
  obj1 = hwloc_get_root_obj(topology);
  assert(obj1);
  obj1->userdata = (void *)(uintptr_t) 0x1;
  obj2 = hwloc_get_obj_by_depth(topology, 3, 13);
  assert(obj2);
  obj2->userdata = (void *)(uintptr_t) 0x2;
  obj3 = hwloc_get_obj_by_depth(topology, 5, 2*3*4*5*6-1);
  assert(obj3);
  obj3->userdata = (void *)(uintptr_t) 0x3;

  /* export/import without callback, we get nothing */
  hwloc_topology_export_xmlbuffer(topology, &xmlbuf, &xmlbuflen, 0);

  hwloc_topology_init(&reimport);
  hwloc_topology_set_xmlbuffer(reimport, xmlbuf, xmlbuflen);
  hwloc_topology_load(reimport);
  check(reimport); /* there should be no userdata */
  hwloc_topology_destroy(reimport);

  /* export/import with callback, we should get three userdata */
  hwloc_topology_set_userdata_export_callback(topology, export_cb);
  hwloc_topology_export_xmlbuffer(topology, &xmlbuf, &xmlbuflen, 0);

  hwloc_topology_init(&reimport);
  hwloc_topology_set_userdata_import_callback(reimport, import_cb);
  hwloc_topology_set_xmlbuffer(reimport, xmlbuf, xmlbuflen);
  hwloc_topology_load(reimport);
  obj1 = hwloc_get_root_obj(reimport);
  assert(obj1);
  assert(obj1->userdata == (void *)(uintptr_t) 0x4);
  obj2 = hwloc_get_obj_by_depth(reimport, 3, 13);
  assert(obj2);
  assert(obj2->userdata == (void *)(uintptr_t) 0x5);
  obj3 = hwloc_get_obj_by_depth(reimport, 5, 2*3*4*5*6-1);
  assert(obj3);
  assert(obj3->userdata == (void *)(uintptr_t) 0x6);
  hwloc_topology_destroy(reimport);

  assert(hwloc_topology_get_userdata(topology) == (void *)(uintptr_t)0x987654);
  hwloc_topology_destroy(topology);

  free(randomstring);
  return 0;
}
