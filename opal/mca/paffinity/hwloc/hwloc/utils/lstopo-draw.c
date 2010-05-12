/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * Copyright © 2009 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/config.h>
#include <private/private.h>
#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "lstopo.h"

#define EPOXY_R_COLOR 0xe7
#define EPOXY_G_COLOR 0xff
#define EPOXY_B_COLOR 0xb5

#define SOCKET_R_COLOR 0xde
#define SOCKET_G_COLOR 0xde
#define SOCKET_B_COLOR 0xde

#define MEMORY_R_COLOR 0xef
#define MEMORY_G_COLOR 0xdf
#define MEMORY_B_COLOR 0xde

#define CORE_R_COLOR 0xbe
#define CORE_G_COLOR 0xbe
#define CORE_B_COLOR 0xbe

#define THREAD_R_COLOR 0xff
#define THREAD_G_COLOR 0xff
#define THREAD_B_COLOR 0xff

#define RUNNING_R_COLOR 0
#define RUNNING_G_COLOR 0xff
#define RUNNING_B_COLOR 0

#define FORBIDDEN_R_COLOR 0xff
#define FORBIDDEN_G_COLOR 0
#define FORBIDDEN_B_COLOR 0

#define OFFLINE_R_COLOR 0
#define OFFLINE_G_COLOR 0
#define OFFLINE_B_COLOR 0

#define CACHE_R_COLOR 0xff
#define CACHE_G_COLOR 0xff
#define CACHE_B_COLOR 0xff

#define MACHINE_R_COLOR EPOXY_R_COLOR
#define MACHINE_G_COLOR EPOXY_G_COLOR
#define MACHINE_B_COLOR EPOXY_B_COLOR

#define NODE_R_COLOR ((EPOXY_R_COLOR * 100) / 110)
#define NODE_G_COLOR ((EPOXY_G_COLOR * 100) / 110)
#define NODE_B_COLOR ((EPOXY_B_COLOR * 100) / 110)

#define SYSTEM_R_COLOR 0xff
#define SYSTEM_G_COLOR 0xff
#define SYSTEM_B_COLOR 0xff

#define MISC_R_COLOR 0xff
#define MISC_G_COLOR 0xff
#define MISC_B_COLOR 0xff

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

static void* null_start(void *output, int width __hwloc_attribute_unused, int height __hwloc_attribute_unused) { return output; }
static void null_declare_color(void *output __hwloc_attribute_unused, int r __hwloc_attribute_unused, int g __hwloc_attribute_unused, int b __hwloc_attribute_unused) { }
static void null_box(void *output __hwloc_attribute_unused, int r __hwloc_attribute_unused, int g __hwloc_attribute_unused, int b __hwloc_attribute_unused, unsigned depth __hwloc_attribute_unused, unsigned x __hwloc_attribute_unused, unsigned width __hwloc_attribute_unused, unsigned y __hwloc_attribute_unused, unsigned height __hwloc_attribute_unused) { }
static void null_line(void *output __hwloc_attribute_unused, int r __hwloc_attribute_unused, int g __hwloc_attribute_unused, int b __hwloc_attribute_unused, unsigned depth __hwloc_attribute_unused, unsigned x1 __hwloc_attribute_unused, unsigned y1_arg __hwloc_attribute_unused, unsigned x2 __hwloc_attribute_unused, unsigned y2 __hwloc_attribute_unused) { }
static void null_text(void *output __hwloc_attribute_unused, int r __hwloc_attribute_unused, int g __hwloc_attribute_unused, int b __hwloc_attribute_unused, int size __hwloc_attribute_unused, unsigned depth __hwloc_attribute_unused, unsigned x __hwloc_attribute_unused, unsigned y __hwloc_attribute_unused, const char *text __hwloc_attribute_unused) { }

static struct draw_methods null_draw_methods = {
  .start = null_start,
  .declare_color = null_declare_color,
  .box = null_box,
  .line = null_line,
  .text = null_text,
};

/*
 * foo_draw functions take a OBJ, computes which size it needs, recurse into
 * sublevels with null_draw_methods to recursively compute the needed size
 * without actually drawing anything, then draw things about OBJ (chip draw,
 * cache size information etc) at (X,Y), recurse into sublevels again to
 * actually draw things, and return in RETWIDTH and RETHEIGHT the amount of
 * space that the drawing took.
 *
 * For generic detailed comments, see the node_draw function.
 */

typedef void (*foo_draw)(hwloc_topology_t topology, struct draw_methods *methods, int logical, hwloc_obj_t obj, void *output, unsigned depth, unsigned x, unsigned *retwidth, unsigned y, unsigned *retheight);

static foo_draw get_type_fun(hwloc_obj_type_t type);

/*
 * Helper to recurse into sublevels, either horizontally or vertically
 * Updates caller's totwidth/myheight and maxwidth/maxheight
 * Needs textwidth, border, topology, output, depth, x and y
 */

#define RECURSE_BEGIN(obj, border) do { \
  hwloc_obj_t *subobjs = obj->children; \
  unsigned numsubobjs = obj->arity; \
  unsigned width, height; \
  unsigned maxwidth __hwloc_attribute_unused, maxheight __hwloc_attribute_unused; \
  maxwidth = maxheight = 0; \
  totwidth = (border) + mywidth; \
  totheight = (border) + myheight; \
  if (numsubobjs) { \
    unsigned i; \

#define RECURSE_FOR() \
    /* Iterate over subobjects */ \
    for (i = 0; i < numsubobjs; i++) { \

      /* Recursive call */
#define RECURSE_CALL_FUN(methods) \
      get_type_fun(subobjs[i]->type)(topology, methods, logical, subobjs[i], output, depth-1, x + totwidth, &width, y + totheight, &height) \

#define RECURSE_END_HORIZ(separator, border) \
      /* Add the subobject's width and separator */ \
      totwidth += width + (separator); \
      /* Update maximum height */ \
      if (height > maxheight) \
	maxheight = height; \
    } \
    \
    /* Remove spurious separator on the right */ \
    totwidth -= (separator); \
    \
    /* Make sure there is width for the heading text */ \
    /* Add subobjects height */ \
    totheight += maxheight; \
    /* And add border below */ \
    totheight += (border); \
  } \
  if (totwidth < textwidth) \
    totwidth = textwidth; \
  /* Add border on the right */ \
  totwidth += (border); \
  /* Update returned values */ \
  *retwidth = totwidth; \
  *retheight = totheight; \
} while(0)

#define RECURSE_END_VERT(separator, border) \
      /* Add the subobject's height and separator */ \
      totheight += height + (separator); \
      if (width > maxwidth) \
      /* Update maximum width */ \
	maxwidth = width; \
    } \
    /* Remove spurious separator at the bottom */ \
    totheight -= (separator); \
  } \
  \
  /* Make sure there is width for the heading text */ \
  if (maxwidth < textwidth) \
    maxwidth = textwidth; \
  /* Add subobjects width */ \
  totwidth += maxwidth; \
  /* And add border on the right */ \
  totwidth += (border); \
  /* Add border at the bottom */ \
  totheight = totheight + (border); \
  /* Update returned values */ \
  *retwidth = totwidth; \
  *retheight = totheight; \
} while(0)

/* Pack objects horizontally */
#define RECURSE_HORIZ(obj, methods, separator, border) \
  RECURSE_BEGIN(obj, border) \
  RECURSE_FOR() \
    RECURSE_CALL_FUN(methods); \
  RECURSE_END_HORIZ(separator, border)

/* Pack objects vertically */
#define RECURSE_VERT(obj, methods, separator, border) \
  RECURSE_BEGIN(obj, border) \
  RECURSE_FOR() \
    RECURSE_CALL_FUN(methods); \
  RECURSE_END_VERT(separator, border)

#define RECURSE_RECT_BEGIN(obj, methods, separator, border) \
RECURSE_BEGIN(obj, border) \
    unsigned obj_maxwidth = 0, obj_maxheight = 0; \
    unsigned area; \
    float idealtotheight; \
    unsigned rows, columns; \
    float under_ratio, over_ratio; \
    RECURSE_FOR() \
      RECURSE_CALL_FUN(&null_draw_methods); \
      if (height > obj_maxheight) \
        obj_maxheight = height; \
      if (width > obj_maxwidth) \
        obj_maxwidth = width; \
    } \
    /* Total area for subobjects */ \
    area = (obj_maxwidth + (separator)) * (obj_maxheight + (separator)) * numsubobjs; \
    /* Ideal total height for spreading that area with RATIO */ \
    idealtotheight = sqrtf(area/RATIO); \
    /* Underestimated number of rows */ \
    rows = idealtotheight / (obj_maxheight + (separator)); \
    columns = rows ? (numsubobjs + rows - 1) / rows : 1; \
    /* Ratio obtained by underestimation */ \
    under_ratio = (float) (columns * (obj_maxwidth + (separator))) / (rows * (obj_maxheight + (separator))); \
    \
    /* try to overestimate too */ \
    rows++; \
    columns = (numsubobjs + rows - 1) / rows; \
    /* Ratio obtained by overestimation */ \
    over_ratio = (float) (columns * (obj_maxwidth + (separator))) / (rows * (obj_maxheight + (separator))); \
    /* Did we actually preferred underestimation? (good row/column fit or good ratio) */ \
    if (rows > 1 && prefer_ratio(under_ratio, over_ratio)) { \
      rows--; \
      columns = (numsubobjs + rows - 1) / rows; \
    } \
    if (force_horiz) { \
      rows =  1; \
      columns = numsubobjs; \
    } \
    if (force_vert) { \
      columns =  1; \
      rows = numsubobjs; \
    } \
    \
    RECURSE_FOR() \
      /* Newline? */ \
      if (i && i%columns == 0) { \
        totwidth = (border) + mywidth; \
        /* Add the same height to all rows */ \
        totheight += obj_maxheight + (separator); \
      } \

#define RECURSE_RECT_END(obj, methods, separator, border) \
      if (totwidth + width + (separator) > maxwidth) \
        maxwidth = totwidth + width + (separator); \
      /* Add the same width to all columns */ \
      totwidth += obj_maxwidth + (separator); \
    } \
    /* Remove spurious separator on the right */ \
    maxwidth -= (separator); \
    /* Make sure there is width for the heading text */ \
    if (maxwidth < textwidth) \
      maxwidth = textwidth; \
    /* Compute total width */ \
    totwidth = maxwidth + (border); \
    /* Add the last row's height and border at the bottom */ \
    totheight += obj_maxheight + (border); \
    /* Update returned values */ \
    *retwidth = totwidth; \
    *retheight = totheight; \
  } \
} while(0)

/* Pack objects in a grid */
#define RECURSE_RECT(obj, methods, separator, border) do {\
  if (obj->arity && obj->children[0]->type == HWLOC_OBJ_NODE) { \
    /* Nodes shouldn't be put with an arbitrary geometry, as NUMA distances may not be that way */ \
    int pvert = prefer_vert(topology, logical, level, output, depth, x, y, separator); \
    if (pvert) \
      RECURSE_VERT(level, methods, separator, border); \
    else \
      RECURSE_HORIZ(level, methods, separator, border); \
  } else {\
    RECURSE_RECT_BEGIN(obj, methods, separator, border) \
    RECURSE_CALL_FUN(methods); \
    RECURSE_RECT_END(obj, methods, separator, border); \
  } \
} while (0)

/* Dynamic programming */

/* Per-object data: width and height of drawing for this object and sub-objects */
struct dyna_save {
  unsigned width;
  unsigned height;
};

/* Save the computed size */
#define DYNA_SAVE() do { \
  if (!level->userdata) { \
    struct dyna_save *save = malloc(sizeof(*save)); \
    save->width = *retwidth; \
    save->height = *retheight; \
    level->userdata = save; \
  } \
} while (0)

/* Check whether we already computed the size and we are not actually drawing, in that case return it */
#define DYNA_CHECK() do { \
  if (level->userdata && methods == &null_draw_methods) { \
    struct dyna_save *save = level->userdata; \
    *retwidth = save->width; \
    *retheight = save->height; \
    return; \
  } \
} while (0)

static int
prefer_vert(hwloc_topology_t topology, int logical, hwloc_obj_t level, void *output, unsigned depth, unsigned x, unsigned y, unsigned separator)
{
  float horiz_ratio, vert_ratio;
  unsigned textwidth = 0;
  unsigned mywidth = 0, myheight = 0;
  unsigned totwidth, *retwidth = &totwidth, totheight, *retheight = &totheight;
  RECURSE_HORIZ(level, &null_draw_methods, separator, 0);
  horiz_ratio = (float)totwidth / totheight;
  RECURSE_VERT(level, &null_draw_methods, separator, 0);
  vert_ratio = (float)totwidth / totheight;
  return force_vert || (!force_horiz && prefer_ratio(vert_ratio, horiz_ratio));
}

static int
lstopo_obj_snprintf(char *text, size_t textlen, hwloc_obj_t obj, int logical)
{
  unsigned idx = logical ? obj->logical_index : obj->os_index;
  const char *indexprefix = logical ? " #" : " p#";
  char typestr[32];
  char indexstr[32]= "";
  char attrstr[256];
  int attrlen;
  hwloc_obj_type_snprintf(typestr, sizeof(typestr), obj, 0);
  if (idx != (unsigned)-1 && obj->depth != 0)
    snprintf(indexstr, sizeof(indexstr), "%s%u", indexprefix, idx);
  attrlen = hwloc_obj_attr_snprintf(attrstr, sizeof(attrstr), obj, " ", 0);
  if (attrlen > 0)
    return snprintf(text, textlen, "%s%s (%s)", typestr, indexstr, attrstr);
  else
    return snprintf(text, textlen, "%s%s", typestr, indexstr);
}

static void
pu_draw(hwloc_topology_t topology, struct draw_methods *methods, int logical, hwloc_obj_t level, void *output, unsigned depth, unsigned x, unsigned *retwidth, unsigned y, unsigned *retheight)
{
  unsigned myheight = (fontsize ? (fontsize + gridsize) : 0), totheight;
  unsigned textwidth = fontsize ? (6-logical)*fontsize : gridsize;
  unsigned mywidth = 0, totwidth;

  DYNA_CHECK();

  RECURSE_HORIZ(level, &null_draw_methods, 0, gridsize);

  if (hwloc_cpuset_isset(level->online_cpuset, level->os_index))
    if (!hwloc_cpuset_isset(level->allowed_cpuset, level->os_index))
      methods->box(output, FORBIDDEN_R_COLOR, FORBIDDEN_G_COLOR, FORBIDDEN_B_COLOR, depth, x, *retwidth, y, *retheight);
    else {
      hwloc_cpuset_t bind = hwloc_cpuset_alloc();
      if (pid != (hwloc_pid_t) -1 && pid != 0)
        hwloc_get_proc_cpubind(topology, pid, bind, 0);
      else if (pid == 0)
        hwloc_get_cpubind(topology, bind, 0);
      if (bind && hwloc_cpuset_isset(bind, level->os_index))
        methods->box(output, RUNNING_R_COLOR, RUNNING_G_COLOR, RUNNING_B_COLOR, depth, x, *retwidth, y, *retheight);
      else
        methods->box(output, THREAD_R_COLOR, THREAD_G_COLOR, THREAD_B_COLOR, depth, x, *retwidth, y, *retheight);
      hwloc_cpuset_free(bind);
    }
  else
    methods->box(output, OFFLINE_R_COLOR, OFFLINE_G_COLOR, OFFLINE_B_COLOR, depth, x, *retwidth, y, *retheight);

  if (fontsize) {
    char text[64];
    lstopo_obj_snprintf(text, sizeof(text), level, logical);
    if (hwloc_cpuset_isset(level->online_cpuset, level->os_index))
      methods->text(output, 0, 0, 0, fontsize, depth-1, x + gridsize, y + gridsize, text);
    else
      methods->text(output, 0xff, 0xff, 0xff, fontsize, depth-1, x + gridsize, y + gridsize, text);
  }

  RECURSE_HORIZ(level, methods, 0, gridsize);

  DYNA_SAVE();
}

static void
cache_draw(hwloc_topology_t topology, struct draw_methods *methods, int logical, hwloc_obj_t level, void *output, unsigned depth, unsigned x, unsigned *retwidth, unsigned y, unsigned *retheight)
{
  unsigned myheight = gridsize + (fontsize ? (fontsize + gridsize) : 0) + gridsize, totheight;
  unsigned mywidth = 0, totwidth;
  unsigned textwidth = fontsize ? ((logical ? level->logical_index : level->os_index) == (unsigned) -1 ? 7*fontsize : 9*fontsize) : 0;
  /* Do not separate objects when in L1 (SMT) */
  unsigned separator = level->attr->cache.depth > 1 ? gridsize : 0;

  DYNA_CHECK();

  RECURSE_RECT(level, &null_draw_methods, separator, 0);

  methods->box(output, CACHE_R_COLOR, CACHE_G_COLOR, CACHE_B_COLOR, depth, x, totwidth, y, myheight - gridsize);

  if (fontsize) {
    char text[64];

    lstopo_obj_snprintf(text, sizeof(text), level, logical);
    methods->text(output, 0, 0, 0, fontsize, depth-1, x + gridsize, y + gridsize, text);
  }

  RECURSE_RECT(level, methods, separator, 0);

  DYNA_SAVE();
}

static void
core_draw(hwloc_topology_t topology, struct draw_methods *methods, int logical, hwloc_obj_t level, void *output, unsigned depth, unsigned x, unsigned *retwidth, unsigned y, unsigned *retheight)
{
  unsigned myheight = (fontsize ? (fontsize + gridsize) : 0), totheight;
  unsigned mywidth = 0, totwidth;
  unsigned textwidth = 5*fontsize;

  DYNA_CHECK();

  RECURSE_RECT(level, &null_draw_methods, 0, gridsize);

  methods->box(output, CORE_R_COLOR, CORE_G_COLOR, CORE_B_COLOR, depth, x, totwidth, y, totheight);

  if (fontsize) {
    char text[64];
    lstopo_obj_snprintf(text, sizeof(text), level, logical);
    methods->text(output, 0, 0, 0, fontsize, depth-1, x + gridsize, y + gridsize, text);
  }

  RECURSE_RECT(level, methods, 0, gridsize);

  DYNA_SAVE();
}

static void
socket_draw(hwloc_topology_t topology, struct draw_methods *methods, int logical, hwloc_obj_t level, void *output, unsigned depth, unsigned x, unsigned *retwidth, unsigned y, unsigned *retheight)
{
  unsigned myheight = (fontsize ? (fontsize + gridsize) : 0), totheight;
  unsigned mywidth = 0, totwidth;
  unsigned textwidth = 6*fontsize;

  DYNA_CHECK();

  RECURSE_RECT(level, &null_draw_methods, gridsize, gridsize);

  methods->box(output, SOCKET_R_COLOR, SOCKET_G_COLOR, SOCKET_B_COLOR, depth, x, totwidth, y, totheight);

  if (fontsize) {
    char text[64];
    lstopo_obj_snprintf(text, sizeof(text), level, logical);
    methods->text(output, 0, 0, 0, fontsize, depth-1, x + gridsize, y + gridsize, text);
  }

  RECURSE_RECT(level, methods, gridsize, gridsize);

  DYNA_SAVE();
}

static void
node_draw(hwloc_topology_t topology, struct draw_methods *methods, int logical, hwloc_obj_t level, void *output, unsigned depth, unsigned x, unsigned *retwidth, unsigned y, unsigned *retheight)
{
  /* Reserve room for the heading memory box and separator */
  unsigned myheight = (fontsize ? (gridsize + fontsize) : 0) + gridsize + gridsize;
  /* Currently filled height */
  unsigned totheight;
  /* Nothing on the left */
  unsigned mywidth = 0;
  /* Currently filled width */
  unsigned totwidth;
  /* Width of the heading text, thus minimal width */
  unsigned textwidth = 11*fontsize;

  /* Check whether dynamic programming can save us time */
  DYNA_CHECK();

  /* Compute the size needed by sublevels */
  RECURSE_RECT(level, &null_draw_methods, gridsize, gridsize);

  /* Draw the epoxy box */
  methods->box(output, NODE_R_COLOR, NODE_G_COLOR, NODE_B_COLOR, depth, x, totwidth, y, totheight);
  /* Draw the memory box */
  methods->box(output, MEMORY_R_COLOR, MEMORY_G_COLOR, MEMORY_B_COLOR, depth-1, x + gridsize, totwidth - 2 * gridsize, y + gridsize, myheight - gridsize);

  if (fontsize) {
    char text[64];
    /* Output text */
    lstopo_obj_snprintf(text, sizeof(text), level, logical);
    methods->text(output, 0, 0, 0, fontsize, depth-2, x + 2 * gridsize, y + 2 * gridsize, text);
  }

  /* Restart, now really drawing sublevels */
  RECURSE_RECT(level, methods, gridsize, gridsize);

  /* Save result for dynamic programming */
  DYNA_SAVE();
}

static void
machine_draw(hwloc_topology_t topology, struct draw_methods *methods, int logical, hwloc_obj_t level, void *output, unsigned depth, unsigned x, unsigned *retwidth, unsigned y, unsigned *retheight)
{
  unsigned myheight = (fontsize ? (fontsize + gridsize) : 0), totheight;
  unsigned mywidth = 0, totwidth;
  unsigned textwidth = 11*fontsize;

  DYNA_CHECK();

  RECURSE_RECT(level, &null_draw_methods, gridsize, gridsize);

  methods->box(output, MACHINE_R_COLOR, MACHINE_G_COLOR, MACHINE_B_COLOR, depth, x, totwidth, y, totheight);

  if (fontsize) {
    char text[64];
    lstopo_obj_snprintf(text, sizeof(text), level, logical);
    methods->text(output, 0, 0, 0, fontsize, depth-1, x + gridsize, y + gridsize, text);
  }

  RECURSE_RECT(level, methods, gridsize, gridsize);

  DYNA_SAVE();
}

static void
system_draw(hwloc_topology_t topology, struct draw_methods *methods, int logical, hwloc_obj_t level, void *output, unsigned depth, unsigned x, unsigned *retwidth, unsigned y, unsigned *retheight)
{
  unsigned myheight = (fontsize ? (fontsize + gridsize) : 0), totheight;
  unsigned mywidth = 0, totwidth;
  unsigned textwidth = 10*fontsize;
  int vert = prefer_vert(topology, logical, level, output, depth, x, y, gridsize);

  DYNA_CHECK();

  if (level->arity > 1 && level->children[0]->type == HWLOC_OBJ_MACHINE) {
    /* network of machines, either horizontal or vertical */
    if (vert) {
      mywidth += gridsize;
      RECURSE_VERT(level, &null_draw_methods, gridsize, gridsize);
    } else
      RECURSE_HORIZ(level, &null_draw_methods, gridsize, gridsize);
  } else
    RECURSE_RECT(level, &null_draw_methods, gridsize, gridsize);

  methods->box(output, SYSTEM_R_COLOR, SYSTEM_G_COLOR, SYSTEM_B_COLOR, depth, x, totwidth, y, totheight);

  if (fontsize) {
    char text[64];
    lstopo_obj_snprintf(text, sizeof(text), level, logical);
    methods->text(output, 0, 0, 0, fontsize, depth-1, x + gridsize, y + gridsize, text);
  }

  if (level->arity > 1 && level->children[0]->type == HWLOC_OBJ_MACHINE) {
    if (vert) {
      unsigned top = 0, bottom = 0;
      unsigned center;
      RECURSE_BEGIN(level, gridsize)
      RECURSE_FOR()
	RECURSE_CALL_FUN(methods);
        center = y + totheight + height / 2;
	if (!top)
	  top = center;
	bottom = center;
	methods->line(output, 0, 0, 0, depth, x + mywidth, center, x + mywidth + gridsize, center);
      RECURSE_END_VERT(gridsize, gridsize);

      if (level->arity > 1 && level->children[0]->type == HWLOC_OBJ_MACHINE)
	methods->line(output, 0, 0, 0, depth, x + mywidth, top, x + mywidth, bottom);
    } else {
      unsigned left = 0, right = 0;
      unsigned center;
      RECURSE_BEGIN(level, gridsize)
      RECURSE_FOR()
	RECURSE_CALL_FUN(methods);
        center = x + totwidth + width / 2;
	if (!left)
	  left = center;
	right = center;
	methods->line(output, 0, 0, 0, depth, center, y + myheight, center, y + myheight + gridsize);
      RECURSE_END_HORIZ(gridsize, gridsize);

      if (level->arity > 1 && level->children[0]->type == HWLOC_OBJ_MACHINE)
	methods->line(output, 0, 0, 0, depth, left, y + myheight, right, y + myheight);
    }
  } else
    RECURSE_RECT(level, methods, gridsize, gridsize);

  DYNA_SAVE();
}

static void
group_draw(hwloc_topology_t topology, struct draw_methods *methods, int logical, hwloc_obj_t level, void *output, unsigned depth, unsigned x, unsigned *retwidth, unsigned y, unsigned *retheight)
{
  unsigned myheight = (fontsize ? (fontsize + gridsize) : 0), totheight;
  unsigned mywidth = 0, totwidth;
  unsigned textwidth = level->name ? strlen(level->name) * fontsize : 6*fontsize;

  DYNA_CHECK();

  RECURSE_RECT(level, &null_draw_methods, gridsize, gridsize);

  methods->box(output, MISC_R_COLOR, MISC_G_COLOR, MISC_B_COLOR, depth, x, totwidth, y, totheight);

  if (fontsize) {
    if (level->name) {
      methods->text(output, 0, 0, 0, fontsize, depth-1, x + gridsize, y + gridsize, level->name);
    } else {
      char text[64];
      lstopo_obj_snprintf(text, sizeof(text), level, logical);
      methods->text(output, 0, 0, 0, fontsize, depth-1, x + gridsize, y + gridsize, text);
    }
  }

  RECURSE_RECT(level, methods, gridsize, gridsize);

  DYNA_SAVE();
}

static void
misc_draw(hwloc_topology_t topology, struct draw_methods *methods, int logical, hwloc_obj_t level, void *output, unsigned depth, unsigned x, unsigned *retwidth, unsigned y, unsigned *retheight)
{
  unsigned boxheight = gridsize + (fontsize ? (fontsize + gridsize) : 0);
  unsigned myheight = boxheight + (level->arity?gridsize:0), totheight;
  unsigned mywidth = 0, totwidth;
  unsigned textwidth = level->name ? strlen(level->name) * fontsize : 6*fontsize;

  DYNA_CHECK();

  RECURSE_HORIZ(level, &null_draw_methods, gridsize, 0);

  methods->box(output, MISC_R_COLOR, MISC_G_COLOR, MISC_B_COLOR, depth, x, totwidth, y, boxheight);

  if (fontsize) {
    if (level->name) {
      methods->text(output, 0, 0, 0, fontsize, depth-1, x + gridsize, y + gridsize, level->name);
    } else {
      char text[64];
      lstopo_obj_snprintf(text, sizeof(text), level, logical);
      methods->text(output, 0, 0, 0, fontsize, depth-1, x + gridsize, y + gridsize, text);
    }
  }

  RECURSE_HORIZ(level, methods, gridsize, 0);

  DYNA_SAVE();
}

static void
fig(hwloc_topology_t topology, struct draw_methods *methods, int logical, hwloc_obj_t level, void *output, unsigned depth, unsigned x, unsigned y)
{
  unsigned totwidth, totheight;

  system_draw(topology, methods, logical, level, output, depth, x, &totwidth, y, &totheight);
}

/*
 * given a type, return a pointer FUN to the function that draws it.
 */
static foo_draw
get_type_fun(hwloc_obj_type_t type)
{
  switch (type) {
    case HWLOC_OBJ_SYSTEM: return system_draw;
    case HWLOC_OBJ_MACHINE: return machine_draw;
    case HWLOC_OBJ_NODE: return node_draw;
    case HWLOC_OBJ_SOCKET: return socket_draw;
    case HWLOC_OBJ_CACHE: return cache_draw;
    case HWLOC_OBJ_CORE: return core_draw;
    case HWLOC_OBJ_PU: return pu_draw;
    case HWLOC_OBJ_GROUP: return group_draw;
    case HWLOC_OBJ_MISC: return misc_draw;
  }
  return NULL;
}

/*
 * Dummy drawing methods to get the bounding box.
 */

struct coords {
  unsigned x;
  unsigned y;
};

static void
getmax_box(void *output, int r __hwloc_attribute_unused, int g __hwloc_attribute_unused, int b __hwloc_attribute_unused, unsigned depth __hwloc_attribute_unused, unsigned x, unsigned width, unsigned y, unsigned height)
{
  struct coords *coords = output;

  if (x > coords->x)
    coords->x = x;
  if (x + width > coords->x)
    coords->x = x + width;
  if (y > coords->y)
    coords->y = y;
  if (y + height > coords->y)
    coords->y = y + height;
}

static void
getmax_line(void *output, int r __hwloc_attribute_unused, int g __hwloc_attribute_unused, int b __hwloc_attribute_unused, unsigned depth __hwloc_attribute_unused, unsigned x1_arg, unsigned y1_arg, unsigned x2_arg, unsigned y2_arg)
{
  struct coords *coords = output;

  if (x1_arg > coords->x)
    coords->x = x1_arg;
  if (x2_arg > coords->x)
    coords->x = x2_arg;
  if (y1_arg > coords->y)
    coords->y = y1_arg;
  if (y2_arg > coords->y)
    coords->y = y2_arg;
}

static struct draw_methods getmax_draw_methods = {
  .start = null_start,
  .declare_color = null_declare_color,
  .box = getmax_box,
  .line = getmax_line,
  .text = null_text,
};

void *
output_draw_start(struct draw_methods *methods, int logical, hwloc_topology_t topology, void *output)
{
  struct coords coords = { .x = 0, .y = 0};
  fig(topology, &getmax_draw_methods, logical, hwloc_get_root_obj(topology), &coords, 100, 0, 0);
  output = methods->start(output, coords.x, coords.y);
  methods->declare_color(output, 0, 0, 0);
  methods->declare_color(output, NODE_R_COLOR, NODE_G_COLOR, NODE_B_COLOR);
  methods->declare_color(output, SOCKET_R_COLOR, SOCKET_G_COLOR, SOCKET_B_COLOR);
  methods->declare_color(output, MEMORY_R_COLOR, MEMORY_G_COLOR, MEMORY_B_COLOR);
  methods->declare_color(output, CORE_R_COLOR, CORE_G_COLOR, CORE_B_COLOR);
  methods->declare_color(output, THREAD_R_COLOR, THREAD_G_COLOR, THREAD_B_COLOR);
  methods->declare_color(output, RUNNING_R_COLOR, RUNNING_G_COLOR, RUNNING_B_COLOR);
  methods->declare_color(output, FORBIDDEN_R_COLOR, FORBIDDEN_G_COLOR, FORBIDDEN_B_COLOR);
  methods->declare_color(output, OFFLINE_R_COLOR, OFFLINE_G_COLOR, OFFLINE_B_COLOR);
  methods->declare_color(output, CACHE_R_COLOR, CACHE_G_COLOR, CACHE_B_COLOR);
  methods->declare_color(output, MACHINE_R_COLOR, MACHINE_G_COLOR, MACHINE_B_COLOR);
  methods->declare_color(output, SYSTEM_R_COLOR, SYSTEM_G_COLOR, SYSTEM_B_COLOR);
  methods->declare_color(output, MISC_R_COLOR, MISC_G_COLOR, MISC_B_COLOR);
  return output;
}

void
output_draw(struct draw_methods *methods, int logical, hwloc_topology_t topology, void *output)
{
	fig(topology, methods, logical, hwloc_get_root_obj(topology), output, 100, 0, 0);
}
