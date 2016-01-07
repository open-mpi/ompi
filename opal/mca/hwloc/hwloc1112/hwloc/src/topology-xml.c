/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2015 Inria.  All rights reserved.
 * Copyright © 2009-2011 Université Bordeaux
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
    const char *env = getenv("HWLOC_XML_VERBOSE");
    if (env)
      verbose = atoi(env);
    first = 0;
  }
  return verbose;
}

static int
hwloc_nolibxml_import(void)
{
  static int first = 1;
  static int nolibxml = 0;
  if (first) {
    const char *env = getenv("HWLOC_NO_LIBXML_IMPORT");
    if (env)
      nolibxml = atoi(env);
    first = 0;
  }
  return nolibxml;
}

static int
hwloc_nolibxml_export(void)
{
  static int first = 1;
  static int nolibxml = 0;
  if (first) {
    const char *env = getenv("HWLOC_NO_LIBXML_EXPORT");
    if (env)
      nolibxml = atoi(env);
    first = 0;
  }
  return nolibxml;
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
			      const char *name, const char *value,
			      hwloc__xml_import_state_t state)
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
      fprintf(stderr, "%s: ignoring cache_size attribute for non-cache object type\n",
	      state->global->msgprefix);
  }

  else if (!strcmp(name, "cache_linesize")) {
    unsigned long lvalue = strtoul(value, NULL, 10);
    if (obj->type == HWLOC_OBJ_CACHE)
      obj->attr->cache.linesize = lvalue;
    else if (hwloc__xml_verbose())
      fprintf(stderr, "%s: ignoring cache_linesize attribute for non-cache object type\n",
	      state->global->msgprefix);
  }

  else if (!strcmp(name, "cache_associativity")) {
    unsigned long lvalue = strtoul(value, NULL, 10);
    if (obj->type == HWLOC_OBJ_CACHE)
      obj->attr->cache.associativity = lvalue;
    else if (hwloc__xml_verbose())
      fprintf(stderr, "%s: ignoring cache_associativity attribute for non-cache object type\n",
	      state->global->msgprefix);
  }

  else if (!strcmp(name, "cache_type")) {
    unsigned long lvalue = strtoul(value, NULL, 10);
    if (obj->type == HWLOC_OBJ_CACHE) {
      if (lvalue == HWLOC_OBJ_CACHE_UNIFIED
	  || lvalue == HWLOC_OBJ_CACHE_DATA
	  || lvalue == HWLOC_OBJ_CACHE_INSTRUCTION)
	obj->attr->cache.type = (hwloc_obj_cache_type_t) lvalue;
      else
	fprintf(stderr, "%s: ignoring invalid cache_type attribute %ld\n",
		state->global->msgprefix, lvalue);
    } else if (hwloc__xml_verbose())
      fprintf(stderr, "%s: ignoring cache_type attribute for non-cache object type\n",
	      state->global->msgprefix);
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
	  fprintf(stderr, "%s: ignoring depth attribute for object type without depth\n",
		  state->global->msgprefix);
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
	  fprintf(stderr, "%s: ignoring invalid pci_busid format string %s\n",
		  state->global->msgprefix, value);
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
	fprintf(stderr, "%s: ignoring pci_busid attribute for non-PCI object\n",
		state->global->msgprefix);
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
	  fprintf(stderr, "%s: ignoring invalid pci_type format string %s\n",
		  state->global->msgprefix, value);
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
	fprintf(stderr, "%s: ignoring pci_type attribute for non-PCI object\n",
		state->global->msgprefix);
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
	fprintf(stderr, "%s: ignoring pci_link_speed attribute for non-PCI object\n",
		state->global->msgprefix);
      break;
    }
  }

  else if (!strcmp(name, "bridge_type")) {
    switch (obj->type) {
    case HWLOC_OBJ_BRIDGE: {
      unsigned upstream_type, downstream_type;
      if (sscanf(value, "%u-%u", &upstream_type, &downstream_type) != 2) {
	if (hwloc__xml_verbose())
	  fprintf(stderr, "%s: ignoring invalid bridge_type format string %s\n",
		  state->global->msgprefix, value);
      } else {
	obj->attr->bridge.upstream_type = (hwloc_obj_bridge_type_t) upstream_type;
	obj->attr->bridge.downstream_type = (hwloc_obj_bridge_type_t) downstream_type;
      };
      break;
    }
    default:
      if (hwloc__xml_verbose())
	fprintf(stderr, "%s: ignoring bridge_type attribute for non-bridge object\n",
		state->global->msgprefix);
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
	  fprintf(stderr, "%s: ignoring invalid bridge_pci format string %s\n",
		  state->global->msgprefix, value);
      } else {
	obj->attr->bridge.downstream.pci.domain = domain;
	obj->attr->bridge.downstream.pci.secondary_bus = secbus;
	obj->attr->bridge.downstream.pci.subordinate_bus = subbus;
      }
      break;
    }
    default:
      if (hwloc__xml_verbose())
	fprintf(stderr, "%s: ignoring bridge_pci attribute for non-bridge object\n",
		state->global->msgprefix);
      break;
    }
  }

  else if (!strcmp(name, "osdev_type")) {
    switch (obj->type) {
    case HWLOC_OBJ_OS_DEVICE: {
      unsigned osdev_type;
      if (sscanf(value, "%u", &osdev_type) != 1) {
	if (hwloc__xml_verbose())
	  fprintf(stderr, "%s: ignoring invalid osdev_type format string %s\n",
		  state->global->msgprefix, value);
      } else
	obj->attr->osdev.type = (hwloc_obj_osdev_type_t) osdev_type;
      break;
    }
    default:
      if (hwloc__xml_verbose())
	fprintf(stderr, "%s: ignoring osdev_type attribute for non-osdev object\n",
		state->global->msgprefix);
      break;
    }
  }




  /*************************
   * deprecated (from 1.0)
   */
  else if (!strcmp(name, "dmi_board_vendor")) {
    hwloc_obj_add_info(obj, "DMIBoardVendor", value);
  }
  else if (!strcmp(name, "dmi_board_name")) {
    hwloc_obj_add_info(obj, "DMIBoardName", value);
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
      case HWLOC_OBJ_NUMANODE:
      case HWLOC_OBJ_MACHINE:
      case HWLOC_OBJ_SYSTEM:
	obj->memory.local_memory = lvalue << 10;
	break;
      default:
	if (hwloc__xml_verbose())
	  fprintf(stderr, "%s: ignoring memory_kB attribute for object type without memory\n",
		  state->global->msgprefix);
	break;
    }
  }
  else if (!strcmp(name, "huge_page_size_kB")) {
    unsigned long lvalue = strtoul(value, NULL, 10);
    switch (obj->type) {
      case HWLOC_OBJ_NUMANODE:
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
	  fprintf(stderr, "%s: ignoring huge_page_size_kB attribute for object type without huge pages\n",
		  state->global->msgprefix);
	break;
    }
  }
  else if (!strcmp(name, "huge_page_free")) {
    unsigned long lvalue = strtoul(value, NULL, 10);
    switch (obj->type) {
      case HWLOC_OBJ_NUMANODE:
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
	  fprintf(stderr, "%s: ignoring huge_page_free attribute for object type without huge pages\n",
		  state->global->msgprefix);
	break;
    }
  }
  /*
   * end of deprecated (from 0.9)
   *******************************/



  else if (hwloc__xml_verbose())
    fprintf(stderr, "%s: ignoring unknown object attribute %s\n",
	    state->global->msgprefix, name);
}


static int
hwloc__xml_import_info(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_obj_t obj,
		       hwloc__xml_import_state_t state)
{
  char *infoname = NULL;
  char *infovalue = NULL;

  while (1) {
    char *attrname, *attrvalue;
    if (state->global->next_attr(state, &attrname, &attrvalue) < 0)
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

  return state->global->close_tag(state);
}

static int
hwloc__xml_import_pagetype(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_obj_t obj,
			   hwloc__xml_import_state_t state)
{
  uint64_t size = 0, count = 0;

  while (1) {
    char *attrname, *attrvalue;
    if (state->global->next_attr(state, &attrname, &attrvalue) < 0)
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

  return state->global->close_tag(state);
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
    if (state->global->next_attr(state, &attrname, &attrvalue) < 0)
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

      ret = state->global->find_child(state, &childstate, &tag);
      if (ret <= 0 || strcmp(tag, "latency")) {
	/* a latency child is needed */
	free(distances->distances.latency);
	free(distances);
	return -1;
      }

      ret = state->global->next_attr(&childstate, &attrname, &attrvalue);
      if (ret < 0 || strcmp(attrname, "value")) {
	free(distances->distances.latency);
	free(distances);
	return -1;
      }

      val = (float) atof((char *) attrvalue);
      matrix[i] = val;
      if (val > latmax)
	latmax = val;

      ret = state->global->close_tag(&childstate);
      if (ret < 0)
	return -1;

      state->global->close_child(&childstate);
    }

    distances->distances.latency_max = latmax;

    if (data->last_distances)
      data->last_distances->next = distances;
    else
      data->first_distances = distances;
    distances->prev = data->last_distances;
    distances->next = NULL;
  }

  return state->global->close_tag(state);
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
    if (state->global->next_attr(state, &attrname, &attrvalue) < 0)
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
      ret = state->global->get_content(state, &encoded_buffer, encoded_length);
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
      ret = state->global->get_content(state, &buffer, length);
      if (ret < 0)
        return -1;
      topology->userdata_import_cb(topology, obj, name, buffer, length);
    }
    state->global->close_content(state);
  }
  return state->global->close_tag(state);
}

static int
hwloc__xml_import_object(hwloc_topology_t topology,
			 struct hwloc_xml_backend_data_s *data,
			 hwloc_obj_t obj,
			 hwloc__xml_import_state_t state)
{
  hwloc_obj_t parent = obj->parent;

  /* process attributes */
  while (1) {
    char *attrname, *attrvalue;
    if (state->global->next_attr(state, &attrname, &attrvalue) < 0)
      break;
    if (!strcmp(attrname, "type")) {
      if (hwloc_obj_type_sscanf(attrvalue, &obj->type, NULL, NULL, 0) < 0)
	goto error;
    } else {
      /* type needed first */
      if (obj->type == (hwloc_obj_type_t)-1)
	goto error;
      hwloc__xml_import_object_attr(topology, obj, attrname, attrvalue, state);
    }
  }

  if (parent) {
    /* root->parent is NULL, and root is already inserted */

    /* warn if inserting out-of-order */
    if (parent->cpuset) { /* don't compare children if multinode parent */
      hwloc_obj_t *current;
      for (current = &parent->first_child; *current; current = &(*current)->next_sibling) {
	hwloc_bitmap_t curcpuset = (*current)->cpuset;
	if (obj->cpuset && (!curcpuset || hwloc__object_cpusets_compare_first(obj, *current) < 0)) {
	  static int reported = 0;
	  if (!reported && !hwloc_hide_errors()) {
	    char *progname = hwloc_progname(topology);
	    const char *origversion = hwloc_obj_get_info_by_name(topology->levels[0][0], "hwlocVersion");
	    const char *origprogname = hwloc_obj_get_info_by_name(topology->levels[0][0], "ProcessName");
	    char *c1, *cc1, t1[64];
	    char *c2 = NULL, *cc2 = NULL, t2[64];
	    hwloc_bitmap_asprintf(&c1, obj->cpuset);
	    hwloc_bitmap_asprintf(&cc1, obj->complete_cpuset);
	    hwloc_obj_type_snprintf(t1, sizeof(t1), obj, 0);
	    if (curcpuset)
	      hwloc_bitmap_asprintf(&c2, curcpuset);
	    if ((*current)->complete_cpuset)
	      hwloc_bitmap_asprintf(&cc2, (*current)->complete_cpuset);
	    hwloc_obj_type_snprintf(t2, sizeof(t2), *current, 0);
	    fprintf(stderr, "****************************************************************************\n");
	    fprintf(stderr, "* hwloc has encountered an out-of-order XML topology load.\n");
	    fprintf(stderr, "* Object %s cpuset %s complete %s\n",
		    t1, c1, cc1);
	    fprintf(stderr, "* was inserted after object %s with %s and %s.\n",
		    t2, c2 ? c2 : "none", cc2 ? cc2 : "none");
	    fprintf(stderr, "* The error occured in hwloc %s inside process `%s', while\n",
		    HWLOC_VERSION,
		    progname ? progname : "<unknown>");
	    if (origversion || origprogname)
	      fprintf(stderr, "* the input XML was generated by hwloc %s inside process `%s'.\n",
		      origversion ? origversion : "(unknown version)",
		      origprogname ? origprogname : "<unknown>");
	    else
	      fprintf(stderr, "* the input XML was generated by an unspecified ancient hwloc release.\n");
	    fprintf(stderr, "* Please check that your input topology XML file is valid.\n");
	    fprintf(stderr, "****************************************************************************\n");
	    free(c1);
	    free(cc1);
	    if (c2)
	      free(c2);
	    if (cc2)
	      free(cc2);
	    free(progname);
	    reported = 1;
	  }
	}
      }
    }

    hwloc_insert_object_by_parent(topology, obj->parent /* filled by the caller */, obj);
    /* insert_object_by_parent() doesn't merge during insert, so obj is still valid */
  }

  /* process subnodes */
  while (1) {
    struct hwloc__xml_import_state_s childstate;
    char *tag;
    int ret;

    ret = state->global->find_child(state, &childstate, &tag);
    if (ret < 0)
      goto error;
    if (!ret)
      break;

    if (!strcmp(tag, "object")) {
      hwloc_obj_t childobj = hwloc_alloc_setup_object(HWLOC_OBJ_TYPE_MAX, -1);
      childobj->parent = obj; /* store the parent pointer for use in insert() below */
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
      goto error;

    state->global->close_child(&childstate);
  }

  return state->global->close_tag(state);

 error:
  hwloc_free_unlinked_object(obj);
  return -1;
}

static int
hwloc__xml_import_diff_one(hwloc__xml_import_state_t state,
			   hwloc_topology_diff_t *firstdiffp,
			   hwloc_topology_diff_t *lastdiffp)
{
  char *type_s = NULL;
  char *obj_depth_s = NULL;
  char *obj_index_s = NULL;
  char *obj_attr_type_s = NULL;
/* char *obj_attr_index_s = NULL; unused for now */
  char *obj_attr_name_s = NULL;
  char *obj_attr_oldvalue_s = NULL;
  char *obj_attr_newvalue_s = NULL;

  while (1) {
    char *attrname, *attrvalue;
    if (state->global->next_attr(state, &attrname, &attrvalue) < 0)
      break;
    if (!strcmp(attrname, "type"))
      type_s = attrvalue;
    else if (!strcmp(attrname, "obj_depth"))
      obj_depth_s = attrvalue;
    else if (!strcmp(attrname, "obj_index"))
      obj_index_s = attrvalue;
    else if (!strcmp(attrname, "obj_attr_type"))
      obj_attr_type_s = attrvalue;
    else if (!strcmp(attrname, "obj_attr_index"))
      { /* obj_attr_index_s = attrvalue; unused for now */ }
    else if (!strcmp(attrname, "obj_attr_name"))
      obj_attr_name_s = attrvalue;
    else if (!strcmp(attrname, "obj_attr_oldvalue"))
      obj_attr_oldvalue_s = attrvalue;
    else if (!strcmp(attrname, "obj_attr_newvalue"))
      obj_attr_newvalue_s = attrvalue;
    else {
      if (hwloc__xml_verbose())
	fprintf(stderr, "%s: ignoring unknown diff attribute %s\n",
		state->global->msgprefix, attrname);
      return -1;
    }
  }

  if (type_s) {
    switch (atoi(type_s)) {
    default:
      break;
    case HWLOC_TOPOLOGY_DIFF_OBJ_ATTR: {
      /* object attribute diff */
      hwloc_topology_diff_obj_attr_type_t obj_attr_type;
      hwloc_topology_diff_t diff;

      /* obj_attr mandatory generic attributes */
      if (!obj_depth_s || !obj_index_s || !obj_attr_type_s) {
	if (hwloc__xml_verbose())
	  fprintf(stderr, "%s: missing mandatory obj attr generic attributes\n",
		  state->global->msgprefix);
	break;
      }

      /* obj_attr mandatory attributes common to all subtypes */
      if (!obj_attr_oldvalue_s || !obj_attr_newvalue_s) {
	if (hwloc__xml_verbose())
	  fprintf(stderr, "%s: missing mandatory obj attr value attributes\n",
		  state->global->msgprefix);
	break;
      }

      /* mandatory attributes for obj_attr_info subtype */
      obj_attr_type = atoi(obj_attr_type_s);
      if (obj_attr_type == HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_INFO && !obj_attr_name_s) {
	if (hwloc__xml_verbose())
	  fprintf(stderr, "%s: missing mandatory obj attr info name attribute\n",
		  state->global->msgprefix);
	break;
      }

      /* now we know we have everything we need */
      diff = malloc(sizeof(*diff));
      if (!diff)
	return -1;
      diff->obj_attr.type = HWLOC_TOPOLOGY_DIFF_OBJ_ATTR;
      diff->obj_attr.obj_depth = atoi(obj_depth_s);
      diff->obj_attr.obj_index = atoi(obj_index_s);
      memset(&diff->obj_attr.diff, 0, sizeof(diff->obj_attr.diff));
      diff->obj_attr.diff.generic.type = obj_attr_type;

      switch (atoi(obj_attr_type_s)) {
      case HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_SIZE:
	diff->obj_attr.diff.uint64.oldvalue = strtoull(obj_attr_oldvalue_s, NULL, 0);
	diff->obj_attr.diff.uint64.newvalue = strtoull(obj_attr_newvalue_s, NULL, 0);
	break;
      case HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_INFO:
	diff->obj_attr.diff.string.name = strdup(obj_attr_name_s);
	/* fallthrough */
      case HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_NAME:
	diff->obj_attr.diff.string.oldvalue = strdup(obj_attr_oldvalue_s);
	diff->obj_attr.diff.string.newvalue = strdup(obj_attr_newvalue_s);
	break;
      }

      if (*firstdiffp)
	(*lastdiffp)->generic.next = diff;
      else
        *firstdiffp = diff;
      *lastdiffp = diff;
      diff->generic.next = NULL;
    }
    }
  }

  return state->global->close_tag(state);
}

int
hwloc__xml_import_diff(hwloc__xml_import_state_t state,
		       hwloc_topology_diff_t *firstdiffp)
{
  hwloc_topology_diff_t firstdiff = NULL, lastdiff = NULL;
  *firstdiffp = NULL;

  while (1) {
    struct hwloc__xml_import_state_s childstate;
    char *tag;
    int ret;

    ret = state->global->find_child(state, &childstate, &tag);
    if (ret < 0)
      return -1;
    if (!ret)
      break;

    if (!strcmp(tag, "diff")) {
      ret = hwloc__xml_import_diff_one(&childstate, &firstdiff, &lastdiff);
    } else
      ret = -1;

    if (ret < 0)
      return ret;

    state->global->close_child(&childstate);
  }

  *firstdiffp = firstdiff;
  return 0;
}

/***********************************
 ********* main XML import *********
 ***********************************/

static int
hwloc_xml__handle_distances(struct hwloc_topology *topology,
			    struct hwloc_xml_backend_data_s *data,
			    const char *msgprefix)
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
	fprintf(stderr, "%s: ignoring invalid distance matrix with %u objs instead of %u\n",
		msgprefix, xmldist->distances.nbobjs, nbobjs);
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

  state.global = data;

  assert(!topology->levels[0][0]->cpuset);

  hwloc_localeswitch_init();

  data->first_distances = data->last_distances = NULL;

  ret = data->look_init(data, &state);
  if (ret < 0)
    goto failed;

  /* find root object tag and import it */
  ret = state.global->find_child(&state, &childstate, &tag);
  if (ret < 0 || !ret || strcmp(tag, "object"))
    goto failed;
  ret = hwloc__xml_import_object(topology, data, topology->levels[0][0], &childstate);
  if (ret < 0)
    goto failed;
  state.global->close_child(&childstate);

  /* find end of topology tag */
  state.global->close_tag(&state);

  /* keep the "Backend" information intact */
  /* we could add "BackendSource=XML" to notify that XML was used between the actual backend and here */

  /* if we added some distances, we must check them, and make them groupable */
  if (hwloc_xml__handle_distances(topology, data, data->msgprefix) < 0)
    goto err;
  data->first_distances = data->last_distances = NULL;
  topology->support.discovery->pu = 1;

  hwloc_localeswitch_fini();
  return 1;

 failed:
  if (data->look_failed)
    data->look_failed(data);
  if (hwloc__xml_verbose())
    fprintf(stderr, "%s: XML component discovery failed.\n",
	    data->msgprefix);
 err:
  hwloc_localeswitch_fini();
  return -1;
}

/* this can be the first XML call */
int
hwloc_topology_diff_load_xml(hwloc_topology_t topology __hwloc_attribute_unused,
			     const char *xmlpath,
			     hwloc_topology_diff_t *firstdiffp, char **refnamep)
{
  struct hwloc__xml_import_state_s state;
  struct hwloc_xml_backend_data_s fakedata; /* only for storing global info during parsing */
  hwloc_localeswitch_declare;
  const char *basename;
  int force_nolibxml;
  int ret;

  state.global = &fakedata;

  basename = strrchr(xmlpath, '/');
  if (basename)
    basename++;
  else
    basename = xmlpath;
  fakedata.msgprefix = strdup(basename);

  if (!hwloc_libxml_callbacks && !hwloc_nolibxml_callbacks) {
    free(fakedata.msgprefix);
    errno = ENOSYS;
    return -1;
  }

  hwloc_localeswitch_init();

  *firstdiffp = NULL;

  force_nolibxml = hwloc_nolibxml_import();
retry:
  if (!hwloc_libxml_callbacks || (hwloc_nolibxml_callbacks && force_nolibxml))
    ret = hwloc_nolibxml_callbacks->import_diff(&state, xmlpath, NULL, 0, firstdiffp, refnamep);
  else {
    ret = hwloc_libxml_callbacks->import_diff(&state, xmlpath, NULL, 0, firstdiffp, refnamep);
    if (ret < 0 && errno == ENOSYS) {
      hwloc_libxml_callbacks = NULL;
      goto retry;
    }
  }

  hwloc_localeswitch_fini();

  free(fakedata.msgprefix);
  return ret;
}

/* this can be the first XML call */
int
hwloc_topology_diff_load_xmlbuffer(hwloc_topology_t topology __hwloc_attribute_unused,
				   const char *xmlbuffer, int buflen,
				   hwloc_topology_diff_t *firstdiffp, char **refnamep)
{
  struct hwloc__xml_import_state_s state;
  struct hwloc_xml_backend_data_s fakedata; /* only for storing global info during parsing */
  hwloc_localeswitch_declare;
  int force_nolibxml;
  int ret;

  state.global = &fakedata;
  fakedata.msgprefix = strdup("xmldiffbuffer");

  if (!hwloc_libxml_callbacks && !hwloc_nolibxml_callbacks) {
    free(fakedata.msgprefix);
    errno = ENOSYS;
    return -1;
  }

  hwloc_localeswitch_init();

  *firstdiffp = NULL;

  force_nolibxml = hwloc_nolibxml_import();
 retry:
  if (!hwloc_libxml_callbacks || (hwloc_nolibxml_callbacks && force_nolibxml))
    ret = hwloc_nolibxml_callbacks->import_diff(&state, NULL, xmlbuffer, buflen, firstdiffp, refnamep);
  else {
    ret = hwloc_libxml_callbacks->import_diff(&state, NULL, xmlbuffer, buflen, firstdiffp, refnamep);
    if (ret < 0 && errno == ENOSYS) {
      hwloc_libxml_callbacks = NULL;
      goto retry;
    }
  }

  hwloc_localeswitch_fini();

  free(fakedata.msgprefix);
  return ret;
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

void
hwloc__xml_export_diff(hwloc__xml_export_state_t parentstate, hwloc_topology_diff_t diff)
{
  while (diff) {
    struct hwloc__xml_export_state_s state;
    char tmp[255];

    parentstate->new_child(parentstate, &state, "diff");

    sprintf(tmp, "%u", diff->generic.type);
    state.new_prop(&state, "type", tmp);

    switch (diff->generic.type) {
    case HWLOC_TOPOLOGY_DIFF_OBJ_ATTR:
      sprintf(tmp, "%d", diff->obj_attr.obj_depth);
      state.new_prop(&state, "obj_depth", tmp);
      sprintf(tmp, "%u", diff->obj_attr.obj_index);
      state.new_prop(&state, "obj_index", tmp);

      sprintf(tmp, "%u", diff->obj_attr.diff.generic.type);
      state.new_prop(&state, "obj_attr_type", tmp);

      switch (diff->obj_attr.diff.generic.type) {
      case HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_SIZE:
	sprintf(tmp, "%llu", (unsigned long long) diff->obj_attr.diff.uint64.index);
	state.new_prop(&state, "obj_attr_index", tmp);
	sprintf(tmp, "%llu", (unsigned long long) diff->obj_attr.diff.uint64.oldvalue);
	state.new_prop(&state, "obj_attr_oldvalue", tmp);
	sprintf(tmp, "%llu", (unsigned long long) diff->obj_attr.diff.uint64.newvalue);
	state.new_prop(&state, "obj_attr_newvalue", tmp);
	break;
      case HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_NAME:
      case HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_INFO:
	if (diff->obj_attr.diff.string.name)
	  state.new_prop(&state, "obj_attr_name", diff->obj_attr.diff.string.name);
	state.new_prop(&state, "obj_attr_oldvalue", diff->obj_attr.diff.string.oldvalue);
	state.new_prop(&state, "obj_attr_newvalue", diff->obj_attr.diff.string.newvalue);
	break;
      }

      break;
    default:
      assert(0);
    }
    state.end_object(&state, "diff");

    diff = diff->generic.next;
  }
}

/**********************************
 ********* main XML export ********
 **********************************/

/* this can be the first XML call */
int hwloc_topology_export_xml(hwloc_topology_t topology, const char *filename)
{
  hwloc_localeswitch_declare;
  int force_nolibxml;
  int ret;

  if (!hwloc_libxml_callbacks && !hwloc_nolibxml_callbacks) {
    errno = ENOSYS;
    return -1;
  }

  hwloc_localeswitch_init();

  force_nolibxml = hwloc_nolibxml_export();
retry:
  if (!hwloc_libxml_callbacks || (hwloc_nolibxml_callbacks && force_nolibxml))
    ret = hwloc_nolibxml_callbacks->export_file(topology, filename);
  else {
    ret = hwloc_libxml_callbacks->export_file(topology, filename);
    if (ret < 0 && errno == ENOSYS) {
      hwloc_libxml_callbacks = NULL;
      goto retry;
    }
  }

  hwloc_localeswitch_fini();
  return ret;
}

/* this can be the first XML call */
int hwloc_topology_export_xmlbuffer(hwloc_topology_t topology, char **xmlbuffer, int *buflen)
{
  hwloc_localeswitch_declare;
  int force_nolibxml;
  int ret;

  if (!hwloc_libxml_callbacks && !hwloc_nolibxml_callbacks) {
    errno = ENOSYS;
    return -1;
  }

  hwloc_localeswitch_init();

  force_nolibxml = hwloc_nolibxml_export();
retry:
  if (!hwloc_libxml_callbacks || (hwloc_nolibxml_callbacks && force_nolibxml))
    ret = hwloc_nolibxml_callbacks->export_buffer(topology, xmlbuffer, buflen);
  else {
    ret = hwloc_libxml_callbacks->export_buffer(topology, xmlbuffer, buflen);
    if (ret < 0 && errno == ENOSYS) {
      hwloc_libxml_callbacks = NULL;
      goto retry;
    }
  }

  hwloc_localeswitch_fini();
  return ret;
}

/* this can be the first XML call */
int
hwloc_topology_diff_export_xml(hwloc_topology_t topology __hwloc_attribute_unused,
			       hwloc_topology_diff_t diff, const char *refname,
			       const char *filename)
{
  hwloc_localeswitch_declare;
  hwloc_topology_diff_t tmpdiff;
  int force_nolibxml;
  int ret;

  if (!hwloc_libxml_callbacks && !hwloc_nolibxml_callbacks) {
    errno = ENOSYS;
    return -1;
  }

  tmpdiff = diff;
  while (tmpdiff) {
    if (tmpdiff->generic.type == HWLOC_TOPOLOGY_DIFF_TOO_COMPLEX) {
      errno = EINVAL;
      return -1;
    }
    tmpdiff = tmpdiff->generic.next;
  }

  hwloc_localeswitch_init();

  force_nolibxml = hwloc_nolibxml_export();
retry:
  if (!hwloc_libxml_callbacks || (hwloc_nolibxml_callbacks && force_nolibxml))
    ret = hwloc_nolibxml_callbacks->export_diff_file(diff, refname, filename);
  else {
    ret = hwloc_libxml_callbacks->export_diff_file(diff, refname, filename);
    if (ret < 0 && errno == ENOSYS) {
      hwloc_libxml_callbacks = NULL;
      goto retry;
    }
  }

  hwloc_localeswitch_fini();
  return ret;
}

/* this can be the first XML call */
int
hwloc_topology_diff_export_xmlbuffer(hwloc_topology_t topology __hwloc_attribute_unused,
				     hwloc_topology_diff_t diff, const char *refname,
				     char **xmlbuffer, int *buflen)
{
  hwloc_localeswitch_declare;
  hwloc_topology_diff_t tmpdiff;
  int force_nolibxml;
  int ret;

  if (!hwloc_libxml_callbacks && !hwloc_nolibxml_callbacks) {
    errno = ENOSYS;
    return -1;
  }

  tmpdiff = diff;
  while (tmpdiff) {
    if (tmpdiff->generic.type == HWLOC_TOPOLOGY_DIFF_TOO_COMPLEX) {
      errno = EINVAL;
      return -1;
    }
    tmpdiff = tmpdiff->generic.next;
  }

  hwloc_localeswitch_init();

  force_nolibxml = hwloc_nolibxml_export();
retry:
  if (!hwloc_libxml_callbacks || (hwloc_nolibxml_callbacks && force_nolibxml))
    ret = hwloc_nolibxml_callbacks->export_diff_buffer(diff, refname, xmlbuffer, buflen);
  else {
    ret = hwloc_libxml_callbacks->export_diff_buffer(diff, refname, xmlbuffer, buflen);
    if (ret < 0 && errno == ENOSYS) {
      hwloc_libxml_callbacks = NULL;
      goto retry;
    }
  }

  hwloc_localeswitch_fini();
  return ret;
}

void hwloc_free_xmlbuffer(hwloc_topology_t topology __hwloc_attribute_unused, char *xmlbuffer)
{
  int force_nolibxml;

  if (!hwloc_libxml_callbacks && !hwloc_nolibxml_callbacks) {
    errno = ENOSYS;
    return ;
  }

  force_nolibxml = hwloc_nolibxml_export();
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
  int ret __hwloc_attribute_unused;

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
  free(data->msgprefix);
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
  int force_nolibxml;
  const char * xmlpath = (const char *) _data1;
  const char * xmlbuffer = (const char *) _data2;
  int xmlbuflen = (int)(uintptr_t) _data3;
  const char *basename;
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

  if (xmlpath) {
    basename = strrchr(xmlpath, '/');
    if (basename)
      basename++;
    else
      basename = xmlpath;
  } else {
    basename = "xmlbuffer";
  }
  data->msgprefix = strdup(basename);

  force_nolibxml = hwloc_nolibxml_import();
retry:
  if (!hwloc_libxml_callbacks || (hwloc_nolibxml_callbacks && force_nolibxml))
    err = hwloc_nolibxml_callbacks->backend_init(data, xmlpath, xmlbuffer, xmlbuflen);
  else {
    err = hwloc_libxml_callbacks->backend_init(data, xmlpath, xmlbuffer, xmlbuflen);
    if (err < 0 && errno == ENOSYS) {
      hwloc_libxml_callbacks = NULL;
      goto retry;
    }
  }
  if (err < 0)
    goto out_with_data;

  return backend;

 out_with_data:
  free(data->msgprefix);
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
  NULL, NULL,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_xml_disc_component
};
