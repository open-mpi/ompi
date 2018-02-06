/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2018 Inria.  All rights reserved.
 * Copyright © 2009-2013, 2015 Université Bordeaux
 * Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <time.h>

#include "lstopo.h"

#define EPOXY_R_COLOR 0xe7
#define EPOXY_G_COLOR 0xff
#define EPOXY_B_COLOR 0xb5

#define DARK_EPOXY_R_COLOR ((EPOXY_R_COLOR * 100) / 110)
#define DARK_EPOXY_G_COLOR ((EPOXY_G_COLOR * 100) / 110)
#define DARK_EPOXY_B_COLOR ((EPOXY_B_COLOR * 100) / 110)

#define DARKER_EPOXY_R_COLOR ((DARK_EPOXY_R_COLOR * 100) / 110)
#define DARKER_EPOXY_G_COLOR ((DARK_EPOXY_G_COLOR * 100) / 110)
#define DARKER_EPOXY_B_COLOR ((DARK_EPOXY_B_COLOR * 100) / 110)

/* each of these colors must be declared in declare_colors() */
const struct lstopo_color BLACK_COLOR = { 0, 0, 0 };
const struct lstopo_color WHITE_COLOR = { 0xff, 0xff, 0xff };
const struct lstopo_color PACKAGE_COLOR = { DARK_EPOXY_R_COLOR, DARK_EPOXY_G_COLOR, DARK_EPOXY_B_COLOR };
const struct lstopo_color MEMORY_COLOR = { 0xef, 0xdf, 0xde };
const struct lstopo_color MEMORIES_COLOR = { 0xf2, 0xe8, 0xe8}; /* slightly lighter than MEMORY_COLOR */
const struct lstopo_color CORE_COLOR = { 0xbe, 0xbe, 0xbe };
const struct lstopo_color THREAD_COLOR = { 0xff, 0xff, 0xff };
const struct lstopo_color BINDING_COLOR = { 0, 0xff, 0 };
const struct lstopo_color FORBIDDEN_COLOR = { 0xff, 0, 0 };
const struct lstopo_color CACHE_COLOR = { 0xff, 0xff, 0xff };
const struct lstopo_color MACHINE_COLOR = { 0xff, 0xff, 0xff };
const struct lstopo_color GROUP_IN_PACKAGE_COLOR = { EPOXY_R_COLOR, EPOXY_G_COLOR, EPOXY_B_COLOR };
const struct lstopo_color MISC_COLOR = { 0xff, 0xff, 0xff };
const struct lstopo_color PCI_DEVICE_COLOR = { DARKER_EPOXY_R_COLOR, DARKER_EPOXY_G_COLOR, DARKER_EPOXY_B_COLOR };
const struct lstopo_color OS_DEVICE_COLOR = { 0xde, 0xde, 0xde };
const struct lstopo_color BRIDGE_COLOR = { 0xff, 0xff, 0xff };

static unsigned
get_textwidth(void *output,
	      const char *text, unsigned length,
	      unsigned fontsize)
{
  struct lstopo_output *loutput = output;
  if (loutput->methods->textsize) {
    unsigned width;
    loutput->methods->textsize(output, text, length, fontsize, &width);
    return width;
  }
  return (length * fontsize * 3) / 4;
}

/* preferred width/height compromise */
#define RATIO (4.f/3.f)

/* do we prefer ratio1 over ratio2? */
static int prefer_ratio(float ratio1, float ratio2) {
  float _ratio1 = (ratio1) / RATIO;
  float _ratio2 = (ratio2) / RATIO;
  if (_ratio1 < 1)
    _ratio1 = 1/_ratio1;
  if (_ratio2 < 1)
    _ratio2 = 1/_ratio2;
  return _ratio1 < _ratio2;
}

/*
 * foo_draw functions take a OBJ, computes which size it needs, recurse into
 * sublevels with drawing=PREPARE to recursively compute the needed size
 * without actually drawing anything, then draw things about OBJ (chip draw,
 * cache size information etc) at (X,Y), recurse into sublevels again to
 * actually draw things, and return in RETWIDTH and RETHEIGHT the amount of
 * space that the drawing took.
 *
 * For generic detailed comments, see the node_draw function.
 *
 * border is added around the objects
 * separator is added between objects
 */

typedef void (*foo_draw)(struct lstopo_output *loutput, hwloc_obj_t obj, unsigned depth, unsigned x, unsigned y);

static foo_draw get_type_fun(hwloc_obj_type_t type);

/* next child, in all children list, with memory before CPU, ignoring PU if needed.
 * similar to hwloc_get_next_child() but returns memory children first.
 */
#define NEXT_CHILD_INIT_STATE -1
static hwloc_obj_t next_child(struct lstopo_output *loutput, hwloc_obj_t parent, unsigned kind, hwloc_obj_t prev, int *statep)
{
  int state;
  hwloc_obj_t obj;
  if (prev) {
    obj = prev->next_sibling;
    state = *statep;
  } else {
    obj = NULL;
    state = NEXT_CHILD_INIT_STATE;
  }

 again:
  if (!obj && state <= -1 && (kind & LSTOPO_CHILD_KIND_MEMORY)) {
    obj = parent->memory_first_child;
    state = 0;
  }
  if (!obj && state <= 0 && (kind & LSTOPO_CHILD_KIND_NORMAL)) {
    obj = parent->first_child;
    state = 1;
  }
  if (!obj && state <= 1 && (kind & LSTOPO_CHILD_KIND_IO)) {
    obj = parent->io_first_child;
    state = 2;
  }
  if (!obj && state <= 2 && (kind & LSTOPO_CHILD_KIND_MISC)) {
    obj = parent->misc_first_child;
    state = 3;
  }
  if (!obj)
    return NULL;

  if (obj->type == HWLOC_OBJ_PU && loutput->ignore_pus) {
    obj = obj->next_sibling;
    goto again;
  }
  if (obj->type == HWLOC_OBJ_NUMANODE && loutput->ignore_numanodes) {
    obj = obj->next_sibling;
    goto again;
  }
  if (loutput->collapse && obj->type == HWLOC_OBJ_PCI_DEVICE) {
    struct lstopo_obj_userdata *lud = obj->userdata;
    if (lud->pci_collapsed == -1) {
      obj = obj->next_sibling;
      goto again;
    }
  }
  *statep = state;
  return obj;
}

/**************************
 * Placing children
 */

static void
place_children_horiz(struct lstopo_output *loutput, hwloc_obj_t parent,
		     unsigned kind, unsigned border, unsigned separator,
		     unsigned *width, unsigned *height)
{
  unsigned curx = 0;
  unsigned maxh = 0;
  hwloc_obj_t child;
  int ncstate;
  for(child = next_child(loutput, parent, kind, NULL, &ncstate);
      child;
      child = next_child(loutput, parent, kind, child, &ncstate)) {
    struct lstopo_obj_userdata *clud = child->userdata;
    clud->xrel = curx + border;
    clud->yrel = border;
    if (clud->height > maxh)
      maxh = clud->height;
    curx += separator + clud->width;
  }
  *width = curx - separator + 2*border;
  *height = maxh + 2*border;
}

static void
place_children_vert(struct lstopo_output *loutput, hwloc_obj_t parent,
		    unsigned kind, unsigned border, unsigned separator,
		    unsigned *width, unsigned *height)
{
  unsigned cury = 0;
  unsigned maxw = 0;
  hwloc_obj_t child;
  int ncstate;
  for(child = next_child(loutput, parent, kind, NULL, &ncstate);
      child;
      child = next_child(loutput, parent, kind, child, &ncstate)) {
    struct lstopo_obj_userdata *clud = child->userdata;
    clud->xrel = border;
    clud->yrel = cury + border;
    if (clud->width > maxw)
      maxw = clud->width;
    cury += separator + clud->height;
  }
  *width = maxw + 2*border;
  *height = cury - separator + 2*border;
}

static void
place_children_rect(struct lstopo_output *loutput, hwloc_obj_t parent,
		    unsigned kind, unsigned border, unsigned separator,
		    unsigned *width, unsigned *height)
{
  unsigned numsubobjs = 0, obj_totwidth = 0, obj_totheight = 0;
  unsigned area = 0;
  unsigned rows, columns;
  unsigned totwidth, totheight; /* total children array size, without borders */
  unsigned rowwidth; /* current row width */
  unsigned maxheight; /* max height for current row */
  int found, i;
  hwloc_obj_t child = NULL;
  int ncstate;

  /* Total area for subobjects */
  while ((child=next_child(loutput, parent, kind, child, &ncstate)) != NULL) {
    struct lstopo_obj_userdata *clud = child->userdata;
    numsubobjs++;
    obj_totwidth += clud->width + separator;
    obj_totheight += clud->height + separator;
    area += (clud->width + separator) * (clud->height + separator);
  }

  /* Try to find a fitting rectangle */
  found = 0;
  for (rows = (unsigned) (float) floor(sqrt(numsubobjs));
       rows >= (unsigned) (float) ceil(pow(numsubobjs, 0.33)) && rows > 1;
       rows--) {
    columns = numsubobjs / rows;
    if (columns > 1 && columns * rows == numsubobjs) {
      found = 1;
      break;
    }
  }

  if (!found) {
    /* Average object size */
    unsigned obj_avgwidth = obj_totwidth / numsubobjs;
    unsigned obj_avgheight = obj_totheight / numsubobjs;
    /* Ideal total height for spreading that area with RATIO */
    float idealtotheight = (float) sqrt(area/RATIO);
    float under_ratio, over_ratio;
    /* approximation of number of rows */
    rows = (unsigned) (idealtotheight / obj_avgheight);
    columns = rows ? (numsubobjs + rows - 1) / rows : 1;
    /* Ratio obtained by underestimation */
    under_ratio = (float) (columns * obj_avgwidth) / (rows * obj_avgheight);
    /* try to overestimate too */
    rows++;
    columns = (numsubobjs + rows - 1) / rows;
    /* Ratio obtained by overestimation */
    over_ratio = (float) (columns * obj_avgwidth) / (rows * obj_avgheight);
    /* Did we actually preferred underestimation? (good row/column fit or good ratio) */
    if (rows > 1 && prefer_ratio(under_ratio, over_ratio)) {
      rows--;
      columns = (numsubobjs + rows - 1) / rows;
    }
  }

  rowwidth = 0;
  maxheight = 0;
  totwidth = 0;
  totheight = 0;
  for(i = 0, child = next_child(loutput, parent, kind, NULL, &ncstate);
      child;
      i++, child = next_child(loutput, parent, kind, child, &ncstate)) {
    struct lstopo_obj_userdata *clud = child->userdata;
    /* Newline? */
    if (i && i%columns == 0) {
      /* Update total width using new row */
      if (rowwidth > totwidth)
	totwidth = rowwidth;
      rowwidth = 0;
      /* Update total height */
      totheight += maxheight + separator;
      maxheight = 0;
    }
    /* Add new child */
    clud->xrel = rowwidth + border;
    clud->yrel = totheight + border;
    rowwidth += clud->width + separator;
    if (clud->height > maxheight)
      maxheight = clud->height;
  }
  /* Update total width using last row */
  if (rowwidth > totwidth)
    totwidth = rowwidth;
  /* Remove spurious separator on the right */
  totwidth -= separator;
  /* Update total height using last row */
  totheight += maxheight; /* no separator */

  *width = totwidth + 2*border;
  *height = totheight + 2*border;
}

static void
place__children(struct lstopo_output *loutput, hwloc_obj_t parent,
		unsigned kind,
		enum lstopo_orient_e *orientp,
		unsigned border, unsigned separator,
		unsigned *widthp, unsigned *heightp)
{
  if (*orientp == LSTOPO_ORIENT_HORIZ) {
    /* force horizontal */
    place_children_horiz(loutput, parent, kind, border, separator, widthp, heightp);

  } else if (*orientp == LSTOPO_ORIENT_VERT) {
    /* force vertical */
    place_children_vert(loutput, parent, kind, border, separator, widthp, heightp);

  } else {
    /* NONE or forced RECT, do a rectangular placement */
    place_children_rect(loutput, parent, kind, border, separator, widthp, heightp);
  }
}

/* Recurse into children to get their size.
 * Place them.
 * Save their position and the parent total size for later.
 */
static void
place_children(struct lstopo_output *loutput, hwloc_obj_t parent,
	       unsigned xrel, unsigned yrel /* position of children within parent */)
{
  struct lstopo_obj_userdata *plud = parent->userdata;
  enum lstopo_orient_e orient = loutput->force_orient[parent->type];
  unsigned border = loutput->gridsize;
  unsigned separator = loutput->gridsize;
  unsigned totwidth = plud->width, totheight = plud->height;
  unsigned children_width = 0, children_height = 0;
  unsigned above_children_width, above_children_height;
  hwloc_obj_t child;
  int ncstate;
  unsigned i;

  /* defaults */
  plud->children.box = 0;
  plud->above_children.box = 0;

  /* select which children kinds go where */
  if (loutput->plain_children_order)
    plud->children.kinds = LSTOPO_CHILD_KIND_ALL;
  else
    plud->children.kinds = LSTOPO_CHILD_KIND_ALL & ~LSTOPO_CHILD_KIND_MEMORY;
  if (parent->memory_arity && !(plud->children.kinds & LSTOPO_CHILD_KIND_MEMORY))
    plud->above_children.kinds = LSTOPO_CHILD_KIND_MEMORY;
  else
    plud->above_children.kinds = 0;

  /* bridge children always vertical */
  if (parent->type == HWLOC_OBJ_BRIDGE)
    orient = LSTOPO_ORIENT_VERT;

  /* recurse into children to prepare their sizes */
  for(i = 0, child = next_child(loutput, parent, LSTOPO_CHILD_KIND_ALL, NULL, &ncstate);
      child;
      i++, child = next_child(loutput, parent, LSTOPO_CHILD_KIND_ALL, child, &ncstate)) {
    get_type_fun(child->type)(loutput, child, 0, 0, 0);
  }
  if (!i)
    return;


  /* no separator between core or L1 children */
  if (parent->type == HWLOC_OBJ_CORE
      || (hwloc_obj_type_is_cache(parent->type) && parent->attr->cache.depth == 1))
    separator = 0;

  /* place non-memory children */
  if (parent->arity + parent->io_arity + parent->misc_arity)
    place__children(loutput, parent, plud->children.kinds, &orient, 0, separator, &children_width, &children_height);

  /* place memory children */
  if (plud->above_children.kinds) {
    enum lstopo_orient_e morient = LSTOPO_ORIENT_HORIZ;
    unsigned memory_border;

    assert(plud->above_children.kinds == LSTOPO_CHILD_KIND_MEMORY);
    memory_border = parent->memory_arity > 1 ? border : 0;

    place__children(loutput, parent, plud->above_children.kinds, &morient, memory_border, separator, &above_children_width, &above_children_height);

    if (parent->memory_arity > 1) {
      /* if there are multiple memory children, add a box, as large as the parent */
      if (above_children_width < children_width) {
	above_children_width = children_width;
      }
      plud->above_children.boxcolor = MEMORIES_COLOR;
      plud->above_children.box = 1;

    } else {
      /* if there's a single memory child without wide memory box, enlarge that child */
      struct lstopo_obj_userdata *clud = parent->memory_first_child->userdata;
      if (clud->width < children_width) {
	clud->width = children_width;
	above_children_width = children_width;
      }
    }
  }

  /* adjust parent size */
  if (hwloc_obj_type_is_cache(parent->type)) {
    /* cache children are below */
    if (children_width > totwidth)
      totwidth = children_width;
    if (children_height)
      totheight += children_height + border;
    if (plud->above_children.kinds) {
      totheight += above_children_height + separator;
      if (above_children_width > totwidth)
	totwidth = above_children_width;
    }
  } else if (parent->type == HWLOC_OBJ_BRIDGE) {
    /* bridge children are on the right, within any space between bridge and children */
    if (children_width)
      totwidth += children_width;
    if (children_height > totheight)
      totheight = children_height;
  } else {
    /* normal objects have children inside their box, with space around them */
    if (children_width + 2*border > totwidth)
      totwidth = children_width + 2*border;
    if (children_height)
      totheight += children_height + border;
    if (plud->above_children.kinds) {
      totheight += above_children_height + separator;
      if (above_children_width + 2*border > totwidth)
	totwidth = above_children_width + 2*border;
    }
  }

  /* save config for draw_children() later */
  plud->orient = orient;
  plud->width = totwidth;
  plud->height = totheight;
  plud->children.width = children_width;
  plud->children.height = children_height;
  plud->children.xrel = xrel;
  plud->children.yrel = yrel;
  if (plud->above_children.kinds) {
    plud->above_children.width = above_children_width;
    plud->above_children.height = above_children_height;
    plud->above_children.xrel = xrel;
    plud->above_children.yrel = yrel;
    plud->children.yrel += above_children_height + separator;
  }
}

/***********************
 * Drawing children
 */

/* additional gridsize when fontsize > 0 */
#define FONTGRIDSIZE (fontsize ? gridsize : 0)

static void
draw__children(struct lstopo_output *loutput, hwloc_obj_t parent,
	       struct lstopo_children_position *children,
	       unsigned depth,
	       unsigned x, unsigned y)
{
  hwloc_obj_t child;
  int ncstate;

  if (children->box)
    loutput->methods->box(loutput, &children->boxcolor, depth, x, children->width, y, children->height);

  for(child = next_child(loutput, parent, children->kinds, NULL, &ncstate);
      child;
      child = next_child(loutput, parent, children->kinds, child, &ncstate)) {
      struct lstopo_obj_userdata *clud = child->userdata;
      get_type_fun(child->type)(loutput, child, depth-1, x + clud->xrel, y + clud->yrel);
    }
}

static void
draw_children(struct lstopo_output *loutput, hwloc_obj_t parent, unsigned depth,
	      unsigned x, unsigned y)
{
  struct lstopo_obj_userdata *plud = parent->userdata;

  if (plud->children.kinds)
    draw__children(loutput, parent, &plud->children, depth, x + plud->children.xrel, y + plud->children.yrel);

  if (plud->above_children.kinds)
    draw__children(loutput, parent, &plud->above_children, depth, x + plud->above_children.xrel, y + plud->above_children.yrel);
}

/*******
 * Misc
 */

static int
lstopo_obj_snprintf(struct lstopo_output *loutput, char *text, size_t textlen, hwloc_obj_t obj)
{
  int logical = loutput->logical;
  unsigned idx = logical ? obj->logical_index : obj->os_index;
  const char *indexprefix = logical ? " L#" : " P#";
  char typestr[32];
  char indexstr[32]= "";
  char attrstr[256];
  char totmemstr[64] = "";
  int attrlen;

  /* For OSDev, Misc and Group, name replaces type+index+attrs */
  if (obj->name && (obj->type == HWLOC_OBJ_OS_DEVICE || obj->type == HWLOC_OBJ_MISC || obj->type == HWLOC_OBJ_GROUP)) {
    return snprintf(text, textlen, "%s", obj->name);
  }

  /* subtype replaces the basic type name */
  if (obj->subtype) {
    snprintf(typestr, sizeof(typestr), "%s", obj->subtype);
  } else {
    hwloc_obj_type_snprintf(typestr, sizeof(typestr), obj, 0);
  }

  if (loutput->show_indexes[obj->type]
      && idx != (unsigned)-1 && obj->depth != 0
      && obj->type != HWLOC_OBJ_PCI_DEVICE
      && (obj->type != HWLOC_OBJ_BRIDGE || obj->attr->bridge.upstream_type == HWLOC_OBJ_BRIDGE_HOST))
    snprintf(indexstr, sizeof(indexstr), "%s%u", indexprefix, idx);

  if (loutput->show_attrs[obj->type]) {
    attrlen = hwloc_obj_attr_snprintf(attrstr, sizeof(attrstr), obj, " ", 0);
    /* display the root total_memory (cannot be local_memory since root cannot be a NUMA node) */
    if (!obj->parent && obj->total_memory)
      snprintf(totmemstr, sizeof(totmemstr), " (%lu%s total)",
	       (unsigned long) hwloc_memory_size_printf_value(obj->total_memory, 0),
	       hwloc_memory_size_printf_unit(obj->total_memory, 0));
  } else
    attrlen = 0;

  if (attrlen > 0)
    return snprintf(text, textlen, "%s%s (%s)%s", typestr, indexstr, attrstr, totmemstr);
  else
    return snprintf(text, textlen, "%s%s%s", typestr, indexstr, totmemstr);
}

static void
lstopo__prepare_custom_styles(struct lstopo_output *loutput, hwloc_obj_t obj)
{
  struct lstopo_obj_userdata *lud = obj->userdata;
  struct lstopo_style *s = &lud->style;
  hwloc_obj_t child;
  unsigned forcer, forceg, forceb;
  const char *stylestr;

  lud->style_set = 0;

  stylestr = hwloc_obj_get_info_by_name(obj, "lstopoStyle");
  if (stylestr) {
    while (*stylestr != '\0') {
      if (sscanf(stylestr, "%02x%02x%02x", &forcer, &forceg, &forceb) == 3
	  || sscanf(stylestr, "Background=#%02x%02x%02x", &forcer, &forceg, &forceb) == 3) {
	s->bg.r = forcer & 255;
	s->bg.g = forceg & 255;
	s->bg.b = forceb & 255;
	lud->style_set |= LSTOPO_STYLE_BG;
	loutput->methods->declare_color(loutput, &s->bg);
	s->t.r = s->t.g = s->t.b = (s->bg.r + s->bg.g + s->bg.b < 0xff) ? 0xff : 0;
      } else if (sscanf(stylestr, "Text=#%02x%02x%02x", &forcer, &forceg, &forceb) == 3) {
	s->t.r = forcer & 255;
	s->t.g = forceg & 255;
	s->t.b = forceb & 255;
	lud->style_set |= LSTOPO_STYLE_T;
	loutput->methods->declare_color(loutput, &s->t);
      } else if (sscanf(stylestr, "Text2=#%02x%02x%02x", &forcer, &forceg, &forceb) == 3) {
	s->t2.r = forcer & 255;
	s->t2.g = forceg & 255;
	s->t2.b = forceb & 255;
	lud->style_set |= LSTOPO_STYLE_T2;
	loutput->methods->declare_color(loutput, &s->t2);
      }
      stylestr = strchr(stylestr, ';');
      if (!stylestr)
	break;
      stylestr++;
    }
  }

  for_each_child(child, obj)
    lstopo__prepare_custom_styles(loutput, child);
  for_each_memory_child(child, obj)
    lstopo__prepare_custom_styles(loutput, child);
  for_each_io_child(child, obj)
    lstopo__prepare_custom_styles(loutput, child);
  for_each_misc_child(child, obj)
    lstopo__prepare_custom_styles(loutput, child);
}

void
lstopo_prepare_custom_styles(struct lstopo_output *loutput)
{
  lstopo__prepare_custom_styles(loutput, hwloc_get_root_obj(loutput->topology));
}

static void
lstopo_set_object_color(struct lstopo_output *loutput,
			hwloc_obj_t obj,
			struct lstopo_style *s)
{
  struct lstopo_obj_userdata *lud = obj->userdata;

  memset(s, 0, sizeof(*s));

  switch (obj->type) {

  case HWLOC_OBJ_MACHINE:
    s->bg = MACHINE_COLOR;
    break;

  case HWLOC_OBJ_GROUP: {
    hwloc_obj_t parent;
    s->bg = MISC_COLOR;
    parent = obj->parent;
    while (parent) {
      if (parent->type == HWLOC_OBJ_PACKAGE) {
	s->bg = GROUP_IN_PACKAGE_COLOR;
	break;
      }
      parent = parent->parent;
    }
    break;
  }

  case HWLOC_OBJ_MISC:
    s->bg = MISC_COLOR;
    break;

  case HWLOC_OBJ_NUMANODE:
    if (lstopo_numa_forbidden(loutput, obj)) {
      s->bg = FORBIDDEN_COLOR;
    } else if (lstopo_numa_binding(loutput, obj)) {
      s->bg = BINDING_COLOR;
    } else {
      s->bg = MEMORY_COLOR;
    }
    break;

  case HWLOC_OBJ_PACKAGE:
    s->bg = PACKAGE_COLOR;
    break;

  case HWLOC_OBJ_CORE:
    s->bg = CORE_COLOR;
    break;

  case HWLOC_OBJ_L1CACHE:
  case HWLOC_OBJ_L2CACHE:
  case HWLOC_OBJ_L3CACHE:
  case HWLOC_OBJ_L4CACHE:
  case HWLOC_OBJ_L5CACHE:
  case HWLOC_OBJ_L1ICACHE:
  case HWLOC_OBJ_L2ICACHE:
  case HWLOC_OBJ_L3ICACHE:
    s->bg = CACHE_COLOR;
    break;

  case HWLOC_OBJ_PU:
    if (lstopo_pu_forbidden(loutput, obj)) {
      s->bg = FORBIDDEN_COLOR;
    } else if (lstopo_pu_binding(loutput, obj)) {
      s->bg = BINDING_COLOR;
    } else {
      s->bg = THREAD_COLOR;
    }
    break;

  case HWLOC_OBJ_BRIDGE:
    s->bg = BRIDGE_COLOR;
    break;

  case HWLOC_OBJ_PCI_DEVICE:
    s->bg = PCI_DEVICE_COLOR;
    break;

  case HWLOC_OBJ_OS_DEVICE:
    s->bg = OS_DEVICE_COLOR;
    break;

  default:
    assert(0);
  }

  if (lud->style_set & LSTOPO_STYLE_BG)
    memcpy(&s->bg, &lud->style.bg, sizeof(struct lstopo_color));
  if (lud->style_set & LSTOPO_STYLE_T)
    memcpy(&s->t, &lud->style.t, sizeof(struct lstopo_color));
  if (lud->style_set & LSTOPO_STYLE_T2)
    memcpy(&s->t2, &lud->style.t2, sizeof(struct lstopo_color));
}

static void
prepare_more_text(struct lstopo_output *loutput, hwloc_obj_t obj)
{
  struct lstopo_obj_userdata *lud = obj->userdata;
  unsigned i;

  if (!loutput->show_attrs[obj->type])
    return;

  if (HWLOC_OBJ_OS_DEVICE == obj->type) {
    if (HWLOC_OBJ_OSDEV_COPROC == obj->attr->osdev.type && obj->subtype) {
      /* Coprocessor */
      if (!strcmp(obj->subtype, "CUDA")) {
	/* CUDA */
	const char *value, *value2, *value3;
	value = hwloc_obj_get_info_by_name(obj, "CUDAGlobalMemorySize");
	if (value) {
	  unsigned long long mb = strtoull(value, NULL, 10) / 1024;
	  snprintf(lud->text[lud->ntext++], sizeof(lud->text[0]),
		   mb >= 10240 ? "%llu GB" : "%llu MB",
		   mb >= 10240 ? mb/1024 : mb);
	}
	value = hwloc_obj_get_info_by_name(obj, "CUDAL2CacheSize");
	if (value) {
	  unsigned long long kb = strtoull(value, NULL, 10);
	  snprintf(lud->text[lud->ntext++], sizeof(lud->text[0]),
		   kb >= 10240 ? "L2 (%llu MB)" : "L2 (%llu kB)",
		   kb >= 10240 ? kb/1024 : kb);
	}
	value = hwloc_obj_get_info_by_name(obj, "CUDAMultiProcessors");
	value2 = hwloc_obj_get_info_by_name(obj, "CUDACoresPerMP");
	value3 = hwloc_obj_get_info_by_name(obj, "CUDASharedMemorySizePerMP");
	if (value && value2 && value3) {
	  snprintf(lud->text[lud->ntext++], sizeof(lud->text[0]),
		   "%s MP x (%s cores + %s kB)", value, value2, value3);
	}

      } else if (!strcmp(obj->subtype, "MIC")) {
	/* MIC */
	const char *value;
	value = hwloc_obj_get_info_by_name(obj, "MICActiveCores");
	if (value) {
	  snprintf(lud->text[lud->ntext++], sizeof(lud->text[0]),
		   "%s cores", value);
	}
	value = hwloc_obj_get_info_by_name(obj, "MICMemorySize");
	if (value) {
	  unsigned long long mb = strtoull(value, NULL, 10) / 1024;
	  snprintf(lud->text[lud->ntext++], sizeof(lud->text[0]),
		   mb >= 10240 ? "%llu GB" : "%llu MB",
		   mb >= 10240 ? mb/1024 : mb);
	}

      } else if (!strcmp(obj->subtype, "OpenCL")) {
	/* OpenCL */
	const char *value;
	value = hwloc_obj_get_info_by_name(obj, "OpenCLComputeUnits");
	if (value) {
	  unsigned long long cu = strtoull(value, NULL, 10);
	  snprintf(lud->text[lud->ntext++], sizeof(lud->text[0]),
		   "%llu compute units", cu);
	}
	value = hwloc_obj_get_info_by_name(obj, "OpenCLGlobalMemorySize");
	if (value) {
	  unsigned long long mb = strtoull(value, NULL, 10) / 1024;
	  snprintf(lud->text[lud->ntext++], sizeof(lud->text[0]),
		   mb >= 10240 ? "%llu GB" : "%llu MB",
		   mb >= 10240 ? mb/1024 : mb);
	}
      }

    } else if (HWLOC_OBJ_OSDEV_BLOCK == obj->attr->osdev.type) {
      /* Block */
      const char *value;
      value = hwloc_obj_get_info_by_name(obj, "Size");
      if (value) {
	unsigned long long mb = strtoull(value, NULL, 10) / 1024;
	snprintf(lud->text[lud->ntext++], sizeof(lud->text[0]),
		 mb >= 10485760 ? "%llu TB" : mb >= 10240 ? "%llu GB" : "%llu MB",
		 mb >= 10485760 ? mb/1048576 : mb >= 10240 ? mb/1024 : mb);
      }
    }
  }

  for(i=1; i<lud->ntext; i++) {
    unsigned nn = (unsigned)strlen(lud->text[i]);
    unsigned ntextwidth = get_textwidth(loutput, lud->text[i], nn, loutput->fontsize);
    if (ntextwidth > lud->textwidth)
      lud->textwidth = ntextwidth;
  }
}

static void
prepare_text(struct lstopo_output *loutput, hwloc_obj_t obj)
{
  struct lstopo_obj_userdata *lud = obj->userdata;
  unsigned fontsize = loutput->fontsize;
  int n;

  /* sane defaults */
  lud->ntext = 0;
  lud->textwidth = 0;
  lud->textxoffset = 0;

  if (!fontsize)
    return;

  /* main object identifier line */
  if (obj->type == HWLOC_OBJ_PCI_DEVICE && loutput->show_attrs[HWLOC_OBJ_PCI_DEVICE]) {
    /* PCI text collapsing */
    char busid[32];
    char _text[64];
    lstopo_obj_snprintf(loutput, _text, sizeof(_text), obj);
    lstopo_busid_snprintf(busid, sizeof(busid), obj, lud->pci_collapsed, loutput->need_pci_domain);
    if (lud->pci_collapsed > 1) {
      n = snprintf(lud->text[0], sizeof(lud->text[0]), "%d x { %s %s }", lud->pci_collapsed, _text, busid);
    } else {
      n = snprintf(lud->text[0], sizeof(lud->text[0]), "%s %s", _text, busid);
    }
  } else {
    /* normal object text */
    n = lstopo_obj_snprintf(loutput, lud->text[0], sizeof(lud->text[0]), obj);
  }
  lud->textwidth = get_textwidth(loutput, lud->text[0], n, fontsize);
  lud->ntext = 1;

  if (obj->type == HWLOC_OBJ_PU) {
    /* if smaller than other PU, artificially extend/shift it
     * to make PU boxes nicer when vertically stacked.
     */
    if (lud->textwidth < loutput->min_pu_textwidth) {
      lud->textxoffset = (loutput->min_pu_textwidth - lud->textwidth) / 2;
      lud->textwidth = loutput->min_pu_textwidth;
    }
  }

  /* additional text */
  prepare_more_text(loutput, obj);
}

static void
draw_text(struct lstopo_output *loutput, hwloc_obj_t obj, struct lstopo_color *lcolor, unsigned depth, unsigned x, unsigned y)
{
  struct draw_methods *methods = loutput->methods;
  struct lstopo_obj_userdata *lud = obj->userdata;
  unsigned fontsize = loutput->fontsize;
  unsigned gridsize = loutput->gridsize;
  unsigned i;

  if (!fontsize)
    return;

  methods->text(loutput, lcolor, fontsize, depth, x + lud->textxoffset, y, lud->text[0]);
  for(i=1; i<lud->ntext; i++)
    methods->text(loutput, lcolor, fontsize, depth, x, y + i*gridsize + i*fontsize, lud->text[i]);
}

static void
pci_device_draw(struct lstopo_output *loutput, hwloc_obj_t level, unsigned depth, unsigned x, unsigned y)
{
  struct lstopo_obj_userdata *lud = level->userdata;
  unsigned gridsize = loutput->gridsize;
  unsigned fontsize = loutput->fontsize;
  unsigned overlaidoffset = 0;

  if (lud->pci_collapsed > 1) {
    /* additional depths and height for overlaid boxes */
    depth -= 2;
    if (lud->pci_collapsed > 2) {
      overlaidoffset = gridsize;
    } else {
      overlaidoffset = gridsize/2;
    }
  }

  if (loutput->drawing == LSTOPO_DRAWING_PREPARE) {
    /* compute children size and position, our size, and save it */
    prepare_text(loutput, level);
    lud->width = lud->textwidth + gridsize + overlaidoffset + FONTGRIDSIZE;
    lud->height = fontsize + gridsize + overlaidoffset + FONTGRIDSIZE;
    place_children(loutput, level,
		   gridsize, fontsize + gridsize + FONTGRIDSIZE);

  } else { /* LSTOPO_DRAWING_DRAW */
    struct draw_methods *methods = loutput->methods;
    struct lstopo_style style;
    unsigned totwidth, totheight;

    /* restore our size that was computed during prepare */
    totwidth = lud->width;
    totheight = lud->height;

    lstopo_set_object_color(loutput, level, &style);

    if (lud->pci_collapsed > 1) {
      methods->box(loutput, &style.bg, depth+2, x + overlaidoffset, totwidth - overlaidoffset, y + overlaidoffset, totheight - overlaidoffset);
      if (lud->pci_collapsed > 2)
	methods->box(loutput, &style.bg, depth+1, x + overlaidoffset/2, totwidth - overlaidoffset, y + overlaidoffset/2, totheight - overlaidoffset);
      methods->box(loutput, &style.bg, depth, x, totwidth - overlaidoffset, y, totheight - overlaidoffset);
    } else {
      methods->box(loutput, &style.bg, depth, x, totwidth, y, totheight);
    }

    draw_text(loutput, level, &style.t, depth-1, x + gridsize, y + gridsize);

    /* Draw sublevels for real */
    draw_children(loutput, level, depth-1, x, y);
  }
}

/* bridge object height: linkspeed + a small empty box */
#define BRIDGE_HEIGHT (gridsize + fontsize + FONTGRIDSIZE)

static void
bridge_draw(struct lstopo_output *loutput, hwloc_obj_t level, unsigned depth, unsigned x, unsigned y)
{
  struct lstopo_obj_userdata *lud = level->userdata;
  unsigned gridsize = loutput->gridsize;
  unsigned fontsize = loutput->fontsize;
  unsigned speedwidth = fontsize ? fontsize + gridsize : 0;

  if (loutput->drawing == LSTOPO_DRAWING_PREPARE) {
    /* compute children size and position, our size, and save it */
    lud->width = 2*gridsize + gridsize + speedwidth;
    lud->height = gridsize + FONTGRIDSIZE;
    place_children(loutput, level,
		   3*gridsize + speedwidth, 0);

  } else { /* LSTOPO_DRAWING_DRAW */
    struct draw_methods *methods = loutput->methods;
    struct lstopo_style style;

    /* Square and left link */
    lstopo_set_object_color(loutput, level, &style);
    methods->box(loutput, &style.bg, depth, x, gridsize, y + BRIDGE_HEIGHT/2 - gridsize/2, gridsize);
    methods->line(loutput, &BLACK_COLOR, depth, x + gridsize, y + BRIDGE_HEIGHT/2, x + 2*gridsize, y + BRIDGE_HEIGHT/2);

    if (level->io_arity > 0) {
      hwloc_obj_t child = NULL;
      unsigned ymax = -1;
      unsigned ymin = (unsigned) -1;
      int ncstate;
      while ((child=next_child(loutput, level, LSTOPO_CHILD_KIND_ALL, child, &ncstate)) != NULL) {
	struct lstopo_obj_userdata *clud = child->userdata;
	unsigned ymid = y + clud->yrel + BRIDGE_HEIGHT/2;
	/* Line to PCI device */
	methods->line(loutput, &BLACK_COLOR, depth-1, x+2*gridsize, ymid, x+3*gridsize+speedwidth, ymid);
	if (ymin == (unsigned) -1)
	  ymin = ymid;
	ymax = ymid;
	/* Negotiated link speed */
	if (fontsize) {
	  float speed = 0.;
	  if (child->type == HWLOC_OBJ_PCI_DEVICE)
	    speed = child->attr->pcidev.linkspeed;
	  if (child->type == HWLOC_OBJ_BRIDGE && child->attr->bridge.upstream_type == HWLOC_OBJ_BRIDGE_PCI)
	    speed = child->attr->bridge.upstream.pci.linkspeed;
	  if (loutput->show_attrs[HWLOC_OBJ_BRIDGE] && speed != 0.) {
	    char text[4];
	    if (speed >= 10.)
	      snprintf(text, sizeof(text), "%.0f", child->attr->pcidev.linkspeed);
	    else
	      snprintf(text, sizeof(text), "%0.1f", child->attr->pcidev.linkspeed);
	    methods->text(loutput, &style.t2, fontsize, depth-1, x + 3*gridsize, ymid - BRIDGE_HEIGHT/2, text);
	  }
	}
      }
      methods->line(loutput, &BLACK_COLOR, depth-1, x+2*gridsize, ymin, x+2*gridsize, ymax);

      /* Draw sublevels for real */
      draw_children(loutput, level, depth-1, x, y);
    }
  }
}

static void
cache_draw(struct lstopo_output *loutput, hwloc_obj_t level, unsigned depth, unsigned x, unsigned y)
{
  struct lstopo_obj_userdata *lud = level->userdata;
  unsigned gridsize = loutput->gridsize;
  unsigned fontsize = loutput->fontsize;
  unsigned myheight = fontsize + gridsize + FONTGRIDSIZE; /* totheight also contains children outside of this actual cache box */

  if (loutput->drawing == LSTOPO_DRAWING_PREPARE) {
    /* compute children size and position, our size, and save it */
    prepare_text(loutput, level);
    lud->width = lud->textwidth + gridsize + FONTGRIDSIZE;
    lud->height = myheight;
    place_children(loutput, level,
		   0, myheight + gridsize);

  } else { /* LSTOPO_DRAWING_DRAW */
    struct draw_methods *methods = loutput->methods;
    struct lstopo_style style;
    unsigned totwidth;
    unsigned myoff = 0;

    /* restore our size that was computed during prepare */
    totwidth = lud->width;

    if (lud->above_children.kinds) {
      /* display above_children even above the cache itself */
      myoff = lud->above_children.height + gridsize;
      lud->above_children.yrel = 0;
    }

    lstopo_set_object_color(loutput, level, &style);
    methods->box(loutput, &style.bg, depth, x, totwidth, y + myoff, myheight);

    draw_text(loutput, level, &style.t, depth-1, x + gridsize, y + gridsize + myoff);

    /* Draw sublevels for real */
    draw_children(loutput, level, depth-1, x, y);
  }
}

static void
normal_draw(struct lstopo_output *loutput, hwloc_obj_t level, unsigned depth, unsigned x, unsigned y)
{
  struct lstopo_obj_userdata *lud = level->userdata;
  unsigned gridsize = loutput->gridsize;
  unsigned fontsize = loutput->fontsize;

  if (loutput->drawing == LSTOPO_DRAWING_PREPARE) {
    /* compute children size and position, our size, and save it */
    prepare_text(loutput, level);
    lud->width = lud->textwidth + gridsize + FONTGRIDSIZE;
    lud->height = gridsize + (fontsize + FONTGRIDSIZE) * lud->ntext;
    place_children(loutput, level,
		   gridsize, gridsize + (fontsize + FONTGRIDSIZE) * lud->ntext);

  } else { /* LSTOPO_DRAWING_DRAW */
    struct draw_methods *methods = loutput->methods;
    struct lstopo_style style;
    unsigned totwidth, totheight;

    /* restore our size that was computed during prepare */
    totwidth = lud->width;
    totheight = lud->height;

    lstopo_set_object_color(loutput, level, &style);
    methods->box(loutput, &style.bg, depth, x, totwidth, y, totheight);
    draw_text(loutput, level, &style.t, depth-1, x + gridsize, y + gridsize);

    /* Draw sublevels for real */
    draw_children(loutput, level, depth-1, x, y);
  }
}

static void
output_compute_pu_min_textwidth(struct lstopo_output *output)
{
  unsigned fontsize = output->fontsize;
  char text[64];
  int n;
  hwloc_topology_t topology = output->topology;
  hwloc_obj_t lastpu;

  if (!output->methods->textsize) {
    output->min_pu_textwidth = 0;
    return;
  }

  if (output->logical) {
    int depth = hwloc_get_type_depth(topology, HWLOC_OBJ_PU);
    lastpu = hwloc_get_obj_by_depth(topology, depth, hwloc_get_nbobjs_by_depth(topology, depth)-1);
  } else {
    unsigned lastidx = hwloc_bitmap_last(hwloc_topology_get_topology_cpuset(topology));
    lastpu = hwloc_get_pu_obj_by_os_index(topology, lastidx);
  }

  n = lstopo_obj_snprintf(output, text, sizeof(text), lastpu);
  output->min_pu_textwidth = get_textwidth(output, text, n, fontsize);
}

void
output_draw(struct lstopo_output *loutput)
{
  hwloc_topology_t topology = loutput->topology;
  struct draw_methods *methods = loutput->methods;
  int legend = loutput->legend;
  unsigned gridsize = loutput->gridsize;
  unsigned fontsize = loutput->fontsize;
  hwloc_obj_t root = hwloc_get_root_obj(topology);
  struct lstopo_obj_userdata *rlud = root->userdata;
  unsigned depth = 100;
  unsigned totwidth, totheight, offset, i;
  time_t t;
  char text[3][128];
  unsigned ntext = 0;
  char hostname[128] = "";
  const char *forcedhostname = NULL;
  unsigned long hostname_size = sizeof(hostname);
  unsigned maxtextwidth = 0, textwidth;

  if (legend && fontsize) {
    forcedhostname = hwloc_obj_get_info_by_name(hwloc_get_root_obj(topology), "HostName");
    if (!forcedhostname && hwloc_topology_is_thissystem(topology)) {
#if defined(HWLOC_WIN_SYS) && !defined(__CYGWIN__)
      GetComputerName(hostname, &hostname_size);
#else
      gethostname(hostname, hostname_size);
#endif
    }
    if (forcedhostname || *hostname) {
      if (forcedhostname)
	snprintf(text[ntext], sizeof(text[ntext]), "Host: %s", forcedhostname);
      else
	snprintf(text[ntext], sizeof(text[ntext]), "Host: %s", hostname);
      textwidth = get_textwidth(loutput, text[ntext], (unsigned) strlen(text[ntext]), fontsize);
      if (textwidth > maxtextwidth)
	maxtextwidth = textwidth;
      ntext++;
    }

    /* Display whether we're showing physical or logical IDs */
    snprintf(text[ntext], sizeof(text[ntext]), "Indexes: %s", loutput->logical ? "logical" : "physical");
    textwidth = get_textwidth(loutput, text[ntext], (unsigned) strlen(text[ntext]), fontsize);
    if (textwidth > maxtextwidth)
      maxtextwidth = textwidth;
    ntext++;

    /* Display timestamp */
    t = time(NULL);
#ifdef HAVE_STRFTIME
    {
      struct tm *tmp;
      tmp = localtime(&t);
      strftime(text[ntext], sizeof(text[ntext]), "Date: %c", tmp);
    }
#else /* HAVE_STRFTIME */
    {
      char *date;
      int n;
      date = ctime(&t);
      n = strlen(date);
      if (n && date[n-1] == '\n') {
        date[n-1] = 0;
      }
      snprintf(text[ntext], sizeof(text[ntext]), "Date: %s", date);
    }
#endif /* HAVE_STRFTIME */
    textwidth = get_textwidth(loutput, text[ntext], (unsigned) strlen(text[ntext]), fontsize);
    if (textwidth > maxtextwidth)
      maxtextwidth = textwidth;
    ntext++;
  }

  if (loutput->drawing == LSTOPO_DRAWING_PREPARE) {
    /* compute root size, our size, and save it */

    output_compute_pu_min_textwidth(loutput);

    get_type_fun(root->type)(loutput, root, depth, 0, 0);

    /* loutput width is max(root, legend) */
    totwidth = rlud->width;
    if (maxtextwidth + 2*gridsize > totwidth)
      totwidth = maxtextwidth + 2*gridsize;
    loutput->width = totwidth;

    /* loutput height is sum(root, legend) */
    totheight = rlud->height;
    if (legend && fontsize)
      totheight += gridsize + (ntext+loutput->legend_append_nr) * (gridsize+fontsize);
    loutput->height = totheight;

  } else { /* LSTOPO_DRAWING_DRAW */
    /* restore our size that was computed during prepare */
    totwidth = rlud->width;
    totheight = rlud->height;

    /* Draw root for real */
    get_type_fun(root->type)(loutput, root, depth, 0, 0);

    /* Draw legend */
    if (legend && fontsize) {
      offset = rlud->height + gridsize;
      methods->box(loutput, &WHITE_COLOR, depth, 0, loutput->width, totheight, gridsize + (ntext+loutput->legend_append_nr) * (gridsize+fontsize));
      for(i=0; i<ntext; i++, offset += gridsize + fontsize)
	methods->text(loutput, &BLACK_COLOR, fontsize, depth, gridsize, offset, text[i]);
      for(i=0; i<loutput->legend_append_nr; i++, offset += gridsize + fontsize)
	methods->text(loutput, &BLACK_COLOR, fontsize, depth, gridsize, offset, loutput->legend_append[i]);
    }
  }
}

/*
 * given a type, return a pointer FUN to the function that draws it.
 */
static foo_draw
get_type_fun(hwloc_obj_type_t type)
{
  switch (type) {
    case HWLOC_OBJ_MACHINE:
    case HWLOC_OBJ_NUMANODE:
    case HWLOC_OBJ_PACKAGE:
    case HWLOC_OBJ_CORE:
    case HWLOC_OBJ_PU:
    case HWLOC_OBJ_GROUP:
    case HWLOC_OBJ_OS_DEVICE:
    case HWLOC_OBJ_MISC: return normal_draw;
    case HWLOC_OBJ_L1CACHE: return cache_draw;
    case HWLOC_OBJ_L2CACHE: return cache_draw;
    case HWLOC_OBJ_L3CACHE: return cache_draw;
    case HWLOC_OBJ_L4CACHE: return cache_draw;
    case HWLOC_OBJ_L5CACHE: return cache_draw;
    case HWLOC_OBJ_L1ICACHE: return cache_draw;
    case HWLOC_OBJ_L2ICACHE: return cache_draw;
    case HWLOC_OBJ_L3ICACHE: return cache_draw;
    case HWLOC_OBJ_PCI_DEVICE: return pci_device_draw;
    case HWLOC_OBJ_BRIDGE: return bridge_draw;
    default:
    case HWLOC_OBJ_TYPE_MAX: assert(0);
  }
  /* for dumb compilers */
  return normal_draw;
}

void
declare_colors(struct lstopo_output *output)
{
  struct draw_methods *methods = output->methods;
  methods->declare_color(output, &BLACK_COLOR);
  methods->declare_color(output, &WHITE_COLOR);
  methods->declare_color(output, &PACKAGE_COLOR);
  methods->declare_color(output, &MEMORY_COLOR);
  methods->declare_color(output, &MEMORIES_COLOR);
  methods->declare_color(output, &CORE_COLOR);
  methods->declare_color(output, &THREAD_COLOR);
  methods->declare_color(output, &BINDING_COLOR);
  methods->declare_color(output, &FORBIDDEN_COLOR);
  methods->declare_color(output, &CACHE_COLOR);
  methods->declare_color(output, &MACHINE_COLOR);
  methods->declare_color(output, &GROUP_IN_PACKAGE_COLOR);
  methods->declare_color(output, &MISC_COLOR);
  methods->declare_color(output, &PCI_DEVICE_COLOR);
  methods->declare_color(output, &OS_DEVICE_COLOR);
  methods->declare_color(output, &BRIDGE_COLOR);
}
