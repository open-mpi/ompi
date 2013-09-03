/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2012 Inria.  All rights reserved.
 * Copyright © 2009-2011 Université Bordeaux 1
 * Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <private/xml.h>
#include <private/private.h>
#include <private/misc.h>
#include <private/debug.h>

int
hwloc__xml_verbose(void)
{
  static int first = 1;
  static int verbose = 0;
  if (first) {
    char *env = getenv("HWLOC_XML_VERBOSE");
    if (env)
      verbose = atoi(env);
    first = 0;
  }
  return verbose;
}

/*********************************
 ********* XML callbacks *********
 *********************************/

/* set when registering nolibxml and libxml components.
 * modifications protected by the components mutex.
 * read by the common XML code in topology-xml.c to jump to the right XML backend.
 */
static struct hwloc_xml_callbacks *hwloc_nolibxml_callbacks = NULL, *hwloc_libxml_callbacks = NULL;

void
hwloc_xml_callbacks_register(struct hwloc_xml_component *comp)
{
  if (!hwloc_nolibxml_callbacks)
    hwloc_nolibxml_callbacks = comp->nolibxml_callbacks;
  if (!hwloc_libxml_callbacks)
    hwloc_libxml_callbacks = comp->libxml_callbacks;
}

void
hwloc_xml_callbacks_reset(void)
{
  hwloc_nolibxml_callbacks = NULL;
  hwloc_libxml_callbacks = NULL;
}			       

/************************************************
 ********* XML import (common routines) *********
 ************************************************/

static void
hwloc__xml_import_object_attr(struct hwloc_topology *topology __hwloc_attribute_unused, struct hwloc_obj *obj,
			      const char *name, const char *value)
{
  if (!strcmp(name, "type")) {
    /* already handled */
    return;
  }

  else if (!strcmp(name, "os_level"))
    obj->os_level = strtoul(value, NULL, 10);
  else if (!strcmp(name, "os_index"))
    obj->os_index = strtoul(value, NULL, 10);
  else if (!strcmp(name, "cpuset")) {
    obj->cpuset = hwloc_bitmap_alloc();
    hwloc_bitmap_sscanf(obj->cpuset, value);
  } else if (!strcmp(name, "complete_cpuset")) {
    obj->complete_cpuset = hwloc_bitmap_alloc();
    hwloc_bitmap_sscanf(obj->complete_cpuset,value);
  } else if (!strcmp(name, "online_cpuset")) {
    obj->online_cpuset = hwloc_bitmap_alloc();
    hwloc_bitmap_sscanf(obj->online_cpuset, value);
  } else if (!strcmp(name, "allowed_cpuset")) {
    obj->allowed_cpuset = hwloc_bitmap_alloc();
    hwloc_bitmap_sscanf(obj->allowed_cpuset, value);
  } else if (!strcmp(name, "nodeset")) {
    obj->nodeset = hwloc_bitmap_alloc();
    hwloc_bitmap_sscanf(obj->nodeset, value);
  } else if (!strcmp(name, "complete_nodeset")) {
    obj->complete_nodeset = hwloc_bitmap_alloc();
    hwloc_bitmap_sscanf(obj->complete_nodeset, value);
  } else if (!strcmp(name, "allowed_nodeset")) {
    obj->allowed_nodeset = hwloc_bitmap_alloc();
    hwloc_bitmap_sscanf(obj->allowed_nodeset, value);
  } else if (!strcmp(name, "name"))
    obj->name = strdup(value);

  else if (!strcmp(name, "cache_size")) {
    unsigned long long lvalue = strtoull(value, NULL, 10);
    if (obj->type == HWLOC_OBJ_CACHE)
      obj->attr->cache.size = lvalue;
    else if (hwloc__xml_verbose())
      fprintf(stderr, "ignoring cache_size attribute for non-cache object type\n");
  }

  else if (!strcmp(name, "cache_linesize")) {
    unsigned long lvalue = strtoul(value, NULL, 10);
    if (obj->type == HWLOC_OBJ_CACHE)
      obj->attr->cache.linesize = lvalue;
    else if (hwloc__xml_verbose())
      fprintf(stderr, "ignoring cache_linesize attribute for non-cache object type\n");
  }

  else if (!strcmp(name, "cache_associativity")) {
    unsigned long lvalue = strtoul(value, NULL, 10);
    if (obj->type == HWLOC_OBJ_CACHE)
      obj->attr->cache.associativity = lvalue;
    else if (hwloc__xml_verbose())
      fprintf(stderr, "ignoring cache_associativity attribute for non-cache object type\n");
  }

  else if (!strcmp(name, "cache_type")) {
    unsigned long lvalue = strtoul(value, NULL, 10);
    if (obj->type == HWLOC_OBJ_CACHE) {
      if (lvalue == HWLOC_OBJ_CACHE_UNIFIED
	  || lvalue == HWLOC_OBJ_CACHE_DATA
	  || lvalue == HWLOC_OBJ_CACHE_INSTRUCTION)
	obj->attr->cache.type = (hwloc_obj_cache_type_t) lvalue;
      else
	fprintf(stderr, "ignoring invalid cache_type attribute %ld\n", lvalue);
    } else if (hwloc__xml_verbose())
      fprintf(stderr, "ignoring cache_type attribute for non-cache object type\n");
  }

  else if (!strcmp(name, "local_memory"))
    obj->memory.local_memory = strtoull(value, NULL, 10);

  else if (!strcmp(name, "depth")) {
    unsigned long lvalue = strtoul(value, NULL, 10);
    switch (obj->type) {
      case HWLOC_OBJ_CACHE:
	obj->attr->cache.depth = lvalue;
	break;
      case HWLOC_OBJ_GROUP:
	obj->attr->group.depth = lvalue;
	break;
      case HWLOC_OBJ_BRIDGE:
	obj->attr->bridge.depth = lvalue;
	break;
      default:
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring depth attribute for object type without depth\n");
	break;
    }
  }

  else if (!strcmp(name, "pci_busid")) {
    switch (obj->type) {
    case HWLOC_OBJ_PCI_DEVICE:
    case HWLOC_OBJ_BRIDGE: {
      unsigned domain, bus, dev, func;
      if (sscanf(value, "%04x:%02x:%02x.%01x",
		 &domain, &bus, &dev, &func) != 4) {
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring invalid pci_busid format string %s\n", value);
      } else {
	obj->attr->pcidev.domain = domain;
	obj->attr->pcidev.bus = bus;
	obj->attr->pcidev.dev = dev;
	obj->attr->pcidev.func = func;
      }
      break;
    }
    default:
      if (hwloc__xml_verbose())
	fprintf(stderr, "ignoring pci_busid attribute for non-PCI object\n");
      break;
    }
  }

  else if (!strcmp(name, "pci_type")) {
    switch (obj->type) {
    case HWLOC_OBJ_PCI_DEVICE:
    case HWLOC_OBJ_BRIDGE: {
      unsigned classid, vendor, device, subvendor, subdevice, revision;
      if (sscanf(value, "%04x [%04x:%04x] [%04x:%04x] %02x",
		 &classid, &vendor, &device, &subvendor, &subdevice, &revision) != 6) {
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring invalid pci_type format string %s\n", value);
      } else {
	obj->attr->pcidev.class_id = classid;
	obj->attr->pcidev.vendor_id = vendor;
	obj->attr->pcidev.device_id = device;
	obj->attr->pcidev.subvendor_id = subvendor;
	obj->attr->pcidev.subdevice_id = subdevice;
	obj->attr->pcidev.revision = revision;
      }
      break;
    }
    default:
      if (hwloc__xml_verbose())
	fprintf(stderr, "ignoring pci_type attribute for non-PCI object\n");
      break;
    }
  }

  else if (!strcmp(name, "pci_link_speed")) {
    switch (obj->type) {
    case HWLOC_OBJ_PCI_DEVICE:
    case HWLOC_OBJ_BRIDGE: {
      obj->attr->pcidev.linkspeed = (float) atof(value);
      break;
    }
    default:
      if (hwloc__xml_verbose())
	fprintf(stderr, "ignoring pci_link_speed attribute for non-PCI object\n");
      break;
    }
  }

  else if (!strcmp(name, "bridge_type")) {
    switch (obj->type) {
    case HWLOC_OBJ_BRIDGE: {
      unsigned upstream_type, downstream_type;
      if (sscanf(value, "%u-%u", &upstream_type, &downstream_type) != 2) {
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring invalid bridge_type format string %s\n", value);
      } else {
	obj->attr->bridge.upstream_type = (hwloc_obj_bridge_type_t) upstream_type;
	obj->attr->bridge.downstream_type = (hwloc_obj_bridge_type_t) downstream_type;
      };
      break;
    }
    default:
      if (hwloc__xml_verbose())
	fprintf(stderr, "ignoring bridge_type attribute for non-bridge object\n");
      break;
    }
  }

  else if (!strcmp(name, "bridge_pci")) {
    switch (obj->type) {
    case HWLOC_OBJ_BRIDGE: {
      unsigned domain, secbus, subbus;
      if (sscanf(value, "%04x:[%02x-%02x]",
		 &domain, &secbus, &subbus) != 3) {
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring invalid bridge_pci format string %s\n", value);
      } else {
	obj->attr->bridge.downstream.pci.domain = domain;
	obj->attr->bridge.downstream.pci.secondary_bus = secbus;
	obj->attr->bridge.downstream.pci.subordinate_bus = subbus;
      }
      break;
    }
    default:
      if (hwloc__xml_verbose())
	fprintf(stderr, "ignoring bridge_pci attribute for non-bridge object\n");
      break;
    }
  }

  else if (!strcmp(name, "osdev_type")) {
    switch (obj->type) {
    case HWLOC_OBJ_OS_DEVICE: {
      unsigned osdev_type;
      if (sscanf(value, "%u", &osdev_type) != 1) {
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring invalid osdev_type format string %s\n", value);
      } else
	obj->attr->osdev.type = (hwloc_obj_osdev_type_t) osdev_type;
      break;
    }
    default:
      if (hwloc__xml_verbose())
	fprintf(stderr, "ignoring osdev_type attribute for non-osdev object\n");
      break;
    }
  }




  /*************************
   * deprecated (from 1.0)
   */
  else if (!strcmp(name, "dmi_board_vendor")) {
    hwloc_obj_add_info(obj, "DMIBoardVendor", strdup(value));
  }
  else if (!strcmp(name, "dmi_board_name")) {
    hwloc_obj_add_info(obj, "DMIBoardName", strdup(value));
  }

  /*************************
   * deprecated (from 0.9)
   */
  else if (!strcmp(name, "memory_kB")) {
    unsigned long long lvalue = strtoull(value, NULL, 10);
    switch (obj->type) {
      case HWLOC_OBJ_CACHE:
	obj->attr->cache.size = lvalue << 10;
	break;
      case HWLOC_OBJ_NODE:
      case HWLOC_OBJ_MACHINE:
      case HWLOC_OBJ_SYSTEM:
	obj->memory.local_memory = lvalue << 10;
	break;
      default:
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring memory_kB attribute for object type without memory\n");
	break;
    }
  }
  else if (!strcmp(name, "huge_page_size_kB")) {
    unsigned long lvalue = strtoul(value, NULL, 10);
    switch (obj->type) {
      case HWLOC_OBJ_NODE:
      case HWLOC_OBJ_MACHINE:
      case HWLOC_OBJ_SYSTEM:
	if (!obj->memory.page_types) {
	  obj->memory.page_types = malloc(sizeof(*obj->memory.page_types));
	  obj->memory.page_types_len = 1;
	}
	obj->memory.page_types[0].size = lvalue << 10;
	break;
      default:
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring huge_page_size_kB attribute for object type without huge pages\n");
	break;
    }
  }
  else if (!strcmp(name, "huge_page_free")) {
    unsigned long lvalue = strtoul(value, NULL, 10);
    switch (obj->type) {
      case HWLOC_OBJ_NODE:
      case HWLOC_OBJ_MACHINE:
      case HWLOC_OBJ_SYSTEM:
	if (!obj->memory.page_types) {
	  obj->memory.page_types = malloc(sizeof(*obj->memory.page_types));
	  obj->memory.page_types_len = 1;
	}
	obj->memory.page_types[0].count = lvalue;
	break;
      default:
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring huge_page_free attribute for object type without huge pages\n");
	break;
    }
  }
  /*
   * end of deprecated (from 0.9)
   *******************************/



  else if (hwloc__xml_verbose())
    fprintf(stderr, "ignoring unknown object attribute %s\n", name);
}


static int
hwloc__xml_import_info(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_obj_t obj,
		       hwloc__xml_import_state_t state)
{
  char *infoname = NULL;
  char *infovalue = NULL;

  while (1) {
    char *attrname, *attrvalue;
    if (state->next_attr(state, &attrname, &attrvalue) < 0)
      break;
    if (!strcmp(attrname, "name"))
      infoname = attrvalue;
    else if (!strcmp(attrname, "value"))
      infovalue = attrvalue;
    else
      return -1;
  }

  if (infoname)
    /* empty strings are ignored by libxml */
    hwloc_obj_add_info(obj, infoname, infovalue ? infovalue : "");

  return state->close_tag(state);
}

static int
hwloc__xml_import_pagetype(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_obj_t obj,
			   hwloc__xml_import_state_t state)
{
  uint64_t size = 0, count = 0;

  while (1) {
    char *attrname, *attrvalue;
    if (state->next_attr(state, &attrname, &attrvalue) < 0)
      break;
    if (!strcmp(attrname, "size"))
      size = strtoull(attrvalue, NULL, 10);
    else if (!strcmp(attrname, "count"))
      count = strtoull(attrvalue, NULL, 10);
    else
      return -1;
  }

  if (size) {
    int idx = obj->memory.page_types_len;
    obj->memory.page_types = realloc(obj->memory.page_types, (idx+1)*sizeof(*obj->memory.page_types));
    obj->memory.page_types_len = idx+1;
    obj->memory.page_types[idx].size = size;
    obj->memory.page_types[idx].count = count;
  }

  return state->close_tag(state);
}

static int
hwloc__xml_import_distances(struct hwloc_xml_backend_data_s *data,
			    hwloc_obj_t obj,
			    hwloc__xml_import_state_t state)
{
  unsigned long reldepth = 0, nbobjs = 0;
  float latbase = 0;
  char *tag;
  int ret;

  while (1) {
    char *attrname, *attrvalue;
    if (state->next_attr(state, &attrname, &attrvalue) < 0)
      break;
    if (!strcmp(attrname, "nbobjs"))
      nbobjs = strtoul(attrvalue, NULL, 10);
    else if (!strcmp(attrname, "relative_depth"))
      reldepth = strtoul(attrvalue, NULL, 10);
    else if (!strcmp(attrname, "latency_base"))
      latbase = (float) atof(attrvalue);
    else
      return -1;
  }

  if (nbobjs && reldepth && latbase) {
    unsigned i;
    float *matrix, latmax = 0;
    struct hwloc_xml_imported_distances_s *distances;

    distances = malloc(sizeof(*distances));
    distances->root = obj;
    distances->distances.relative_depth = reldepth;
    distances->distances.nbobjs = nbobjs;
    distances->distances.latency = matrix = malloc(nbobjs*nbobjs*sizeof(float));
    distances->distances.latency_base = latbase;

    for(i=0; i<nbobjs*nbobjs; i++) {
      struct hwloc__xml_import_state_s childstate;
      char *attrname, *attrvalue;
      float val;

      ret = state->find_child(state, &childstate, &tag);
      if (ret <= 0 || strcmp(tag, "latency")) {
	/* a latency child is needed */
	free(distances->distances.latency);
	free(distances);
	return -1;
      }

      ret = state->next_attr(&childstate, &attrname, &attrvalue);
      if (ret < 0 || strcmp(attrname, "value")) {
	free(distances->distances.latency);
	free(distances);
	return -1;
      }

      val = (float) atof((char *) attrvalue);
      matrix[i] = val;
      if (val > latmax)
	latmax = val;

      ret = state->close_tag(&childstate);
      if (ret < 0)
	return -1;

      state->close_child(&childstate);
    }

    distances->distances.latency_max = latmax;

    if (data->last_distances)
      data->last_distances->next = distances;
    else
      data->first_distances = distances;
    distances->prev = data->last_distances;
    distances->next = NULL;
  }

  return state->close_tag(state);
}

static int
hwloc__xml_import_userdata(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_obj_t obj,
			   hwloc__xml_import_state_t state)
{
  size_t length = 0;
  int encoded = 0;
  char *name = NULL; /* optional */

  while (1) {
    char *attrname, *attrvalue;
    if (state->next_attr(state, &attrname, &attrvalue) < 0)
      break;
    if (!strcmp(attrname, "length"))
      length = strtoul(attrvalue, NULL, 10);
    else if (!strcmp(attrname, "encoding"))
      encoded = !strcmp(attrvalue, "base64");
    else if (!strcmp(attrname, "name"))
      name = attrvalue;
    else
      return -1;
  }

  if (length && topology->userdata_import_cb) {
    int ret;

    if (encoded) {
      char *encoded_buffer;
      size_t encoded_length = 4*((length+2)/3);
      ret = state->get_content(state, &encoded_buffer, encoded_length);
      if (ret < 0)
        return -1;
      if (ret) {
	char *decoded_buffer = malloc(length+1);
	if (!decoded_buffer)
	  return -1;
	assert(encoded_buffer[encoded_length] == 0);
	ret = hwloc_decode_from_base64(encoded_buffer, decoded_buffer, length+1);
	if (ret != (int) length)
	  return -1;
	topology->userdata_import_cb(topology, obj, name, decoded_buffer, length);
	free(decoded_buffer);
      }
    } else {
      char *buffer;
      ret = state->get_content(state, &buffer, length);
      if (ret < 0)
        return -1;
      topology->userdata_import_cb(topology, obj, name, buffer, length);
    }
    state->close_content(state);
  }
  return state->close_tag(state);
}

static int
hwloc__xml_import_object(hwloc_topology_t topology,
			 struct hwloc_xml_backend_data_s *data,
			 hwloc_obj_t obj,
			 hwloc__xml_import_state_t state)
{
  /* process attributes */
  while (1) {
    char *attrname, *attrvalue;
    if (state->next_attr(state, &attrname, &attrvalue) < 0)
      break;
    if (!strcmp(attrname, "type")) {
      obj->type = hwloc_obj_type_of_string(attrvalue);
      if (obj->type == (hwloc_obj_type_t)-1)
        return -1;
    } else {
      /* type needed first */
      if (obj->type == (hwloc_obj_type_t)-1)
        return -1;
      hwloc__xml_import_object_attr(topology, obj, attrname, attrvalue);
    }
  }

  /* process subnodes */
  while (1) {
    struct hwloc__xml_import_state_s childstate;
    char *tag;
    int ret;

    ret = state->find_child(state, &childstate, &tag);
    if (ret < 0)
      return -1;
    if (!ret)
      break;

    if (!strcmp(tag, "object")) {
      hwloc_obj_t childobj = hwloc_alloc_setup_object(HWLOC_OBJ_TYPE_MAX, -1);
      hwloc_insert_object_by_parent(topology, obj, childobj);
      /* insert_object_by_parent() doesn't merge during insert, so childobj is still valid */
      ret = hwloc__xml_import_object(topology, data, childobj, &childstate);
    } else if (!strcmp(tag, "page_type")) {
      ret = hwloc__xml_import_pagetype(topology, obj, &childstate);
    } else if (!strcmp(tag, "info")) {
      ret = hwloc__xml_import_info(topology, obj, &childstate);
    } else if (!strcmp(tag, "distances")) {
      ret = hwloc__xml_import_distances(data, obj, &childstate);
    } else if (!strcmp(tag, "userdata")) {
      ret = hwloc__xml_import_userdata(topology, obj, &childstate);
    } else
      ret = -1;

    if (ret < 0)
      return ret;

    state->close_child(&childstate);
  }

  return state->close_tag(state);
}

/***********************************
 ********* main XML import *********
 ***********************************/

static int
hwloc_xml__handle_distances(struct hwloc_topology *topology,
			    struct hwloc_xml_backend_data_s *data)
{
  struct hwloc_xml_imported_distances_s *xmldist, *next = data->first_distances;

  if (!next)
    return 0;

  /* connect things now because we need levels to check/build, they'll be reconnected properly later anyway */
  hwloc_connect_children(topology->levels[0][0]);
  if (hwloc_connect_levels(topology) < 0)
    return -1;

  while ((xmldist = next) != NULL) {
    hwloc_obj_t root = xmldist->root;
    unsigned depth = root->depth + xmldist->distances.relative_depth;
    unsigned nbobjs = hwloc_get_nbobjs_inside_cpuset_by_depth(topology, root->cpuset, depth);
    if (nbobjs != xmldist->distances.nbobjs) {
      /* distances invalid, drop */
      if (hwloc__xml_verbose())
	fprintf(stderr, "ignoring invalid distance matrix with %u objs instead of %u\n",
		xmldist->distances.nbobjs, nbobjs);
      free(xmldist->distances.latency);
    } else {
      /* distances valid, add it to the internal OS distances list for grouping */
      unsigned *indexes = malloc(nbobjs * sizeof(unsigned));
      hwloc_obj_t child, *objs = malloc(nbobjs * sizeof(hwloc_obj_t));
      unsigned j;
      for(j=0, child = hwloc_get_next_obj_inside_cpuset_by_depth(topology, root->cpuset, depth, NULL);
	  j<nbobjs;
	  j++, child = hwloc_get_next_obj_inside_cpuset_by_depth(topology, root->cpuset, depth, child)) {
	indexes[j] = child->os_index;
	objs[j] = child;
      }
      for(j=0; j<nbobjs*nbobjs; j++)
	xmldist->distances.latency[j] *= xmldist->distances.latency_base;
      hwloc_distances_set(topology, objs[0]->type, nbobjs, indexes, objs, xmldist->distances.latency, 0 /* XML cannot force */);
    }

    next = xmldist->next;
    free(xmldist);
  }

  return 0;
}

/* this canNOT be the first XML call */
static int
hwloc_look_xml(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  struct hwloc_xml_backend_data_s *data = backend->private_data;
  struct hwloc__xml_import_state_s state, childstate;
  char *tag;
  hwloc_localeswitch_declare;
  int ret;

  assert(!topology->levels[0][0]->cpuset);

  hwloc_localeswitch_init();

  data->first_distances = data->last_distances = NULL;

  ret = data->look_init(data, &state);
  if (ret < 0)
    goto failed;

  /* find root object tag and import it */
  ret = state.find_child(&state, &childstate, &tag);
  if (ret < 0 || !ret || strcmp(tag, "object"))
    goto failed;
  ret = hwloc__xml_import_object(topology, data, topology->levels[0][0], &childstate);
  if (ret < 0)
    goto failed;
  state.close_child(&childstate);

  /* find end of topology tag */
  state.close_tag(&state);

  /* keep the "Backend" information intact */
  /* we could add "BackendSource=XML" to notify that XML was used between the actual backend and here */

  /* if we added some distances, we must check them, and make them groupable */
  if (hwloc_xml__handle_distances(topology, data) < 0)
    goto err;
  data->first_distances = data->last_distances = NULL;
  topology->support.discovery->pu = 1;

  hwloc_localeswitch_fini();
  return 1;

 failed:
  if (data->look_failed)
    data->look_failed(data);
  if (hwloc__xml_verbose())
    fprintf(stderr, "XML component discovery failed.\n");
 err:
  hwloc_localeswitch_fini();
  return -1;
}

/************************************************
 ********* XML export (common routines) *********
 ************************************************/

#define HWLOC_XML_CHAR_VALID(c) (((c) >= 32 && (c) <= 126) || (c) == '\t' || (c) == '\n' || (c) == '\r')

static int
hwloc__xml_export_check_buffer(const char *buf, size_t length)
{
  unsigned i;
  for(i=0; i<length; i++)
    if (!HWLOC_XML_CHAR_VALID(buf[i]))
      return -1;
  return 0;
}

/* strdup and remove ugly chars from random string */
static char*
hwloc__xml_export_safestrdup(const char *old)
{
  char *new = malloc(strlen(old)+1);
  char *dst = new;
  const char *src = old;
  while (*src) {
    if (HWLOC_XML_CHAR_VALID(*src))
      *(dst++) = *src;
    src++;
  }
  *dst = '\0';
  return new;
}

void
hwloc__xml_export_object (hwloc__xml_export_state_t parentstate, hwloc_topology_t topology, hwloc_obj_t obj)
{
  struct hwloc__xml_export_state_s state;
  char *cpuset = NULL;
  char tmp[255];
  unsigned i;

  parentstate->new_child(parentstate, &state, "object");

  state.new_prop(&state, "type", hwloc_obj_type_string(obj->type));
  if (obj->os_level != -1) {
    sprintf(tmp, "%d", obj->os_level);
    state.new_prop(&state, "os_level", tmp);
  }
  if (obj->os_index != (unsigned) -1) {
    sprintf(tmp, "%u", obj->os_index);
    state.new_prop(&state, "os_index", tmp);
  }
  if (obj->cpuset) {
    hwloc_bitmap_asprintf(&cpuset, obj->cpuset);
    state.new_prop(&state, "cpuset", cpuset);
    free(cpuset);
  }
  if (obj->complete_cpuset) {
    hwloc_bitmap_asprintf(&cpuset, obj->complete_cpuset);
    state.new_prop(&state, "complete_cpuset", cpuset);
    free(cpuset);
  }
  if (obj->online_cpuset) {
    hwloc_bitmap_asprintf(&cpuset, obj->online_cpuset);
    state.new_prop(&state, "online_cpuset", cpuset);
    free(cpuset);
  }
  if (obj->allowed_cpuset) {
    hwloc_bitmap_asprintf(&cpuset, obj->allowed_cpuset);
    state.new_prop(&state, "allowed_cpuset", cpuset);
    free(cpuset);
  }
  if (obj->nodeset && !hwloc_bitmap_isfull(obj->nodeset)) {
    hwloc_bitmap_asprintf(&cpuset, obj->nodeset);
    state.new_prop(&state, "nodeset", cpuset);
    free(cpuset);
  }
  if (obj->complete_nodeset && !hwloc_bitmap_isfull(obj->complete_nodeset)) {
    hwloc_bitmap_asprintf(&cpuset, obj->complete_nodeset);
    state.new_prop(&state, "complete_nodeset", cpuset);
    free(cpuset);
  }
  if (obj->allowed_nodeset && !hwloc_bitmap_isfull(obj->allowed_nodeset)) {
    hwloc_bitmap_asprintf(&cpuset, obj->allowed_nodeset);
    state.new_prop(&state, "allowed_nodeset", cpuset);
    free(cpuset);
  }

  if (obj->name) {
    char *name = hwloc__xml_export_safestrdup(obj->name);
    state.new_prop(&state, "name", name);
    free(name);
  }

  switch (obj->type) {
  case HWLOC_OBJ_CACHE:
    sprintf(tmp, "%llu", (unsigned long long) obj->attr->cache.size);
    state.new_prop(&state, "cache_size", tmp);
    sprintf(tmp, "%u", obj->attr->cache.depth);
    state.new_prop(&state, "depth", tmp);
    sprintf(tmp, "%u", (unsigned) obj->attr->cache.linesize);
    state.new_prop(&state, "cache_linesize", tmp);
    sprintf(tmp, "%d", (unsigned) obj->attr->cache.associativity);
    state.new_prop(&state, "cache_associativity", tmp);
    sprintf(tmp, "%d", (unsigned) obj->attr->cache.type);
    state.new_prop(&state, "cache_type", tmp);
    break;
  case HWLOC_OBJ_GROUP:
    sprintf(tmp, "%u", obj->attr->group.depth);
    state.new_prop(&state, "depth", tmp);
    break;
  case HWLOC_OBJ_BRIDGE:
    sprintf(tmp, "%u-%u", obj->attr->bridge.upstream_type, obj->attr->bridge.downstream_type);
    state.new_prop(&state, "bridge_type", tmp);
    sprintf(tmp, "%u", obj->attr->bridge.depth);
    state.new_prop(&state, "depth", tmp);
    if (obj->attr->bridge.downstream_type == HWLOC_OBJ_BRIDGE_PCI) {
      sprintf(tmp, "%04x:[%02x-%02x]",
	      (unsigned) obj->attr->bridge.downstream.pci.domain,
	      (unsigned) obj->attr->bridge.downstream.pci.secondary_bus,
	      (unsigned) obj->attr->bridge.downstream.pci.subordinate_bus);
      state.new_prop(&state, "bridge_pci", tmp);
    }
    if (obj->attr->bridge.upstream_type != HWLOC_OBJ_BRIDGE_PCI)
      break;
    /* fallthrough */
  case HWLOC_OBJ_PCI_DEVICE:
    sprintf(tmp, "%04x:%02x:%02x.%01x",
	    (unsigned) obj->attr->pcidev.domain,
	    (unsigned) obj->attr->pcidev.bus,
	    (unsigned) obj->attr->pcidev.dev,
	    (unsigned) obj->attr->pcidev.func);
    state.new_prop(&state, "pci_busid", tmp);
    sprintf(tmp, "%04x [%04x:%04x] [%04x:%04x] %02x",
	    (unsigned) obj->attr->pcidev.class_id,
	    (unsigned) obj->attr->pcidev.vendor_id, (unsigned) obj->attr->pcidev.device_id,
	    (unsigned) obj->attr->pcidev.subvendor_id, (unsigned) obj->attr->pcidev.subdevice_id,
	    (unsigned) obj->attr->pcidev.revision);
    state.new_prop(&state, "pci_type", tmp);
    sprintf(tmp, "%f", obj->attr->pcidev.linkspeed);
    state.new_prop(&state, "pci_link_speed", tmp);
    break;
  case HWLOC_OBJ_OS_DEVICE:
    sprintf(tmp, "%u", obj->attr->osdev.type);
    state.new_prop(&state, "osdev_type", tmp);
    break;
  default:
    break;
  }

  if (obj->memory.local_memory) {
    sprintf(tmp, "%llu", (unsigned long long) obj->memory.local_memory);
    state.new_prop(&state, "local_memory", tmp);
  }

  for(i=0; i<obj->memory.page_types_len; i++) {
    struct hwloc__xml_export_state_s childstate;
    state.new_child(&state, &childstate, "page_type");
    sprintf(tmp, "%llu", (unsigned long long) obj->memory.page_types[i].size);
    childstate.new_prop(&childstate, "size", tmp);
    sprintf(tmp, "%llu", (unsigned long long) obj->memory.page_types[i].count);
    childstate.new_prop(&childstate, "count", tmp);
    childstate.end_object(&childstate, "page_type");
  }

  for(i=0; i<obj->infos_count; i++) {
    char *name = hwloc__xml_export_safestrdup(obj->infos[i].name);
    char *value = hwloc__xml_export_safestrdup(obj->infos[i].value);
    struct hwloc__xml_export_state_s childstate;
    state.new_child(&state, &childstate, "info");
    childstate.new_prop(&childstate, "name", name);
    childstate.new_prop(&childstate, "value", value);
    childstate.end_object(&childstate, "info");
    free(name);
    free(value);
  }

  for(i=0; i<obj->distances_count; i++) {
    unsigned nbobjs = obj->distances[i]->nbobjs;
    unsigned j;
    struct hwloc__xml_export_state_s childstate;
    state.new_child(&state, &childstate, "distances");
    sprintf(tmp, "%u", nbobjs);
    childstate.new_prop(&childstate, "nbobjs", tmp);
    sprintf(tmp, "%u", obj->distances[i]->relative_depth);
    childstate.new_prop(&childstate, "relative_depth", tmp);
    sprintf(tmp, "%f", obj->distances[i]->latency_base);
    childstate.new_prop(&childstate, "latency_base", tmp);
    for(j=0; j<nbobjs*nbobjs; j++) {
      struct hwloc__xml_export_state_s greatchildstate;
      childstate.new_child(&childstate, &greatchildstate, "latency");
      sprintf(tmp, "%f", obj->distances[i]->latency[j]);
      greatchildstate.new_prop(&greatchildstate, "value", tmp);
      greatchildstate.end_object(&greatchildstate, "latency");
    }
    childstate.end_object(&childstate, "distances");
  }

  if (obj->userdata && topology->userdata_export_cb)
    topology->userdata_export_cb((void*) &state, topology, obj);

  if (obj->arity) {
    unsigned x;
    for (x=0; x<obj->arity; x++)
      hwloc__xml_export_object (&state, topology, obj->children[x]);
  }

  state.end_object(&state, "object");
}

/**********************************
 ********* main XML export ********
 **********************************/

/* this can be the first XML call */
int hwloc_topology_export_xml(hwloc_topology_t topology, const char *filename)
{
  hwloc_localeswitch_declare;
  char *env;
  int force_nolibxml;
  int ret;

  if (!hwloc_libxml_callbacks && !hwloc_nolibxml_callbacks) {
    errno = ENOSYS;
    return -1;
  }

  hwloc_localeswitch_init();

  env = getenv("HWLOC_NO_LIBXML_EXPORT");
  force_nolibxml = (env && atoi(env));
  if (!hwloc_libxml_callbacks || (hwloc_nolibxml_callbacks && force_nolibxml))
    ret = hwloc_nolibxml_callbacks->export_file(topology, filename);
  else
    ret = hwloc_libxml_callbacks->export_file(topology, filename);

  hwloc_localeswitch_fini();
  return ret;
}

/* this can be the first XML call */
int hwloc_topology_export_xmlbuffer(hwloc_topology_t topology, char **xmlbuffer, int *buflen)
{
  hwloc_localeswitch_declare;
  char *env;
  int force_nolibxml;
  int ret;

  if (!hwloc_libxml_callbacks && !hwloc_nolibxml_callbacks) {
    errno = ENOSYS;
    return -1;
  }

  hwloc_localeswitch_init();

  env = getenv("HWLOC_NO_LIBXML_EXPORT");
  force_nolibxml = (env && atoi(env));
  if (!hwloc_libxml_callbacks || (hwloc_nolibxml_callbacks && force_nolibxml))
    ret = hwloc_nolibxml_callbacks->export_buffer(topology, xmlbuffer, buflen);
  else
    ret = hwloc_libxml_callbacks->export_buffer(topology, xmlbuffer, buflen);

  hwloc_localeswitch_fini();
  return ret;
}

void hwloc_free_xmlbuffer(hwloc_topology_t topology __hwloc_attribute_unused, char *xmlbuffer)
{
  char *env;
  int force_nolibxml;

  if (!hwloc_libxml_callbacks && !hwloc_nolibxml_callbacks) {
    errno = ENOSYS;
    return ;
  }

  env = getenv("HWLOC_NO_LIBXML_EXPORT");
  force_nolibxml = (env && atoi(env));
  if (!hwloc_libxml_callbacks || (hwloc_nolibxml_callbacks && force_nolibxml))
    hwloc_nolibxml_callbacks->free_buffer(xmlbuffer);
  else
    hwloc_libxml_callbacks->free_buffer(xmlbuffer);
}

void
hwloc_topology_set_userdata_export_callback(hwloc_topology_t topology,
					    void (*export)(void *reserved, struct hwloc_topology *topology, struct hwloc_obj *obj))
{
  topology->userdata_export_cb = export;
}

static void
hwloc__export_obj_userdata(hwloc__xml_export_state_t parentstate, int encoded,
			   const char *name, size_t length, const void *buffer, size_t encoded_length)
{
  struct hwloc__xml_export_state_s state;
  char tmp[255];
  parentstate->new_child(parentstate, &state, "userdata");
  if (name)
    state.new_prop(&state, "name", name);
  sprintf(tmp, "%lu", (unsigned long) length);
  state.new_prop(&state, "length", tmp);
  if (encoded)
    state.new_prop(&state, "encoding", "base64");
  state.add_content(&state, buffer, encoded ? encoded_length : length);
  state.end_object(&state, "userdata");
}

int
hwloc_export_obj_userdata(void *reserved,
			  struct hwloc_topology *topology __hwloc_attribute_unused, struct hwloc_obj *obj __hwloc_attribute_unused,
			  const char *name, const void *buffer, size_t length)
{
  hwloc__xml_export_state_t state = reserved;

  if ((name && hwloc__xml_export_check_buffer(name, strlen(name)) < 0)
      || hwloc__xml_export_check_buffer(buffer, length) < 0) {
    errno = EINVAL;
    return -1;
  }

  hwloc__export_obj_userdata(state, 0, name, length, buffer, length);
  return 0;
}

int
hwloc_export_obj_userdata_base64(void *reserved,
				 struct hwloc_topology *topology __hwloc_attribute_unused, struct hwloc_obj *obj __hwloc_attribute_unused,
				 const char *name, const void *buffer, size_t length)
{
  hwloc__xml_export_state_t state = reserved;
  size_t encoded_length;
  char *encoded_buffer;
  int ret;

  if (name && hwloc__xml_export_check_buffer(name, strlen(name)) < 0) {
    errno = EINVAL;
    return -1;
  }

  encoded_length = 4*((length+2)/3);
  encoded_buffer = malloc(encoded_length+1);
  if (!encoded_buffer) {
    errno = ENOMEM;
    return -1;
  }

  ret = hwloc_encode_to_base64(buffer, length, encoded_buffer, encoded_length+1);
  assert(ret == (int) encoded_length);

  hwloc__export_obj_userdata(state, 1, name, length, encoded_buffer, encoded_length);

  free(encoded_buffer);
  return 0;
}

void
hwloc_topology_set_userdata_import_callback(hwloc_topology_t topology,
					    void (*import)(struct hwloc_topology *topology, struct hwloc_obj *obj, const char *name, const void *buffer, size_t length))
{
  topology->userdata_import_cb = import;
}

/***************************************
 ************ XML component ************
 ***************************************/

static void
hwloc_xml_backend_disable(struct hwloc_backend *backend)
{
  struct hwloc_xml_backend_data_s *data = backend->private_data;
  data->backend_exit(data);
  free(data);
}

static struct hwloc_backend *
hwloc_xml_component_instantiate(struct hwloc_disc_component *component,
				const void *_data1,
				const void *_data2,
				const void *_data3)
{
  struct hwloc_xml_backend_data_s *data;
  struct hwloc_backend *backend;
  char *env;
  int force_nolibxml;
  const char * xmlpath = (const char *) _data1;
  const char * xmlbuffer = (const char *) _data2;
  int xmlbuflen = (int)(uintptr_t) _data3;
  int err;

  if (!hwloc_libxml_callbacks && !hwloc_nolibxml_callbacks) {
    errno = ENOSYS;
    goto out;
  }

  if (!xmlpath && !xmlbuffer) {
    errno = EINVAL;
    goto out;
  }

  backend = hwloc_backend_alloc(component);
  if (!backend)
    goto out;

  data = malloc(sizeof(*data));
  if (!data) {
    errno = ENOMEM;
    goto out_with_backend;
  }

  backend->private_data = data;
  backend->discover = hwloc_look_xml;
  backend->disable = hwloc_xml_backend_disable;
  backend->is_thissystem = 0;

  env = getenv("HWLOC_NO_LIBXML_IMPORT");
  force_nolibxml = (env && atoi(env));
  if (!hwloc_libxml_callbacks || (hwloc_nolibxml_callbacks && force_nolibxml))
    err = hwloc_nolibxml_callbacks->backend_init(data, xmlpath, xmlbuffer, xmlbuflen);
  else
    err = hwloc_libxml_callbacks->backend_init(data, xmlpath, xmlbuffer, xmlbuflen);
  if (err < 0)
    goto out_with_data;

  return backend;

 out_with_data:
  free(data);
 out_with_backend:
  free(backend);
 out:
  return NULL;
}

static struct hwloc_disc_component hwloc_xml_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  "xml",
  ~0,
  hwloc_xml_component_instantiate,
  30,
  NULL
};

const struct hwloc_component hwloc_xml_component = {
  HWLOC_COMPONENT_ABI,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_xml_disc_component
};
