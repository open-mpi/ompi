/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009-2010, 2012, 2015 Université Bordeaux
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#ifndef UTILS_LSTOPO_H
#define UTILS_LSTOPO_H

#include <private/autogen/config.h>
#include <hwloc.h>
#include <misc.h>

enum lstopo_drawing_e {
  LSTOPO_DRAWING_PREPARE,
  LSTOPO_DRAWING_DRAW
};

enum lstopo_orient_e {
  LSTOPO_ORIENT_NONE = 0,
  LSTOPO_ORIENT_HORIZ,
  LSTOPO_ORIENT_VERT,
  LSTOPO_ORIENT_RECT
};

FILE *open_output(const char *filename, int overwrite) __hwloc_attribute_malloc;

struct draw_methods;

/* if embedded in backend-specific output structure, must be at the beginning */
struct lstopo_output {
  hwloc_topology_t topology;

  /* file config */
  FILE *file;
  int overwrite;

  /* misc config */
  int logical;
  int verbose_mode;
  int ignore_pus;
  int ignore_numanodes;
  int collapse;
  int pid_number;
  hwloc_pid_t pid;
  int need_pci_domain;

  /* export config */
  unsigned long export_synthetic_flags;
  unsigned long export_xml_flags;

  /* legend */
  int legend;
  char ** legend_append;
  unsigned legend_append_nr;

  /* text config */
  int show_distances_only;
  hwloc_obj_type_t show_only;
  int show_cpuset;
  int show_taskset;

  /* draw config */
  unsigned plain_children_order;
  unsigned int gridsize, fontsize;
  enum lstopo_orient_e force_orient[HWLOC_OBJ_TYPE_MAX]; /* orientation of children within an object of the given type */
  void *backend_data;
  struct draw_methods *methods;
  unsigned width, height; /* total output size */
  unsigned min_pu_textwidth;
  int show_indexes[HWLOC_OBJ_TYPE_MAX];
  int show_attrs[HWLOC_OBJ_TYPE_MAX];
  enum lstopo_drawing_e drawing;
};

struct lstopo_color { int r, g, b; };

struct lstopo_style {
  struct lstopo_color
	bg,	/* main box background color */
	t,	/* main text color */
	t2;	/* other text color */
};

#define LSTOPO_CHILD_KIND_NORMAL 0x1
#define LSTOPO_CHILD_KIND_MEMORY 0x2
#define LSTOPO_CHILD_KIND_IO     0x4
#define LSTOPO_CHILD_KIND_MISC   0x8
#define LSTOPO_CHILD_KIND_ALL    0xf

struct lstopo_obj_userdata {
  /* original common userdata (we replace the first one with this extended structure) */
  struct hwloc_utils_userdata common;

  /* PCI collapsing */
  int pci_collapsed; /* 0 if no collapsing, -1 if collapsed with a previous one, >1 if collapsed with several next */

  /* custom style */
  struct lstopo_style style;
#define LSTOPO_STYLE_BG  0x1
#define LSTOPO_STYLE_T   0x2
#define LSTOPO_STYLE_T2  0x4
  unsigned style_set; /* OR'ed LSTOPO_STYLE_* */

  /* object size (including children if they are outside of it, not including borders) */
  unsigned width;
  unsigned height;

  /* a child position is: its parent position + parent->children_*rel + child->*rel */
  /* relative position of first child with respect to top-left corner of this object */
  struct lstopo_children_position {
    unsigned kinds;
    unsigned width;
    unsigned height;
    unsigned xrel;
    unsigned yrel;
    unsigned box;
    struct lstopo_color boxcolor;
  } children;
  /* relative position of first memory child */
  struct lstopo_children_position above_children;

  /* relative position of this object within its parent children zone */
  unsigned xrel;
  unsigned yrel;

  /* children orientation */
  enum lstopo_orient_e orient;

  /* text lines within object */
  char text[4][128]; /* current max number of lines is osdev name + 3 cuda attributes */
  unsigned ntext;
  unsigned textwidth;
  unsigned textxoffset;
};

typedef int output_method (struct lstopo_output *output, const char *filename);
extern output_method output_console, output_synthetic, output_ascii, output_fig, output_png, output_pdf, output_ps, output_svg, output_x11, output_windows, output_xml;

struct draw_methods {
  /* only called when loutput->draw_methods == LSTOPO_DRAWING_DRAW */
  void (*declare_color) (struct lstopo_output *loutput, const struct lstopo_color *lcolor);
  void (*box) (struct lstopo_output *loutput, const struct lstopo_color *lcolor, unsigned depth, unsigned x, unsigned width, unsigned y, unsigned height);
  void (*line) (struct lstopo_output *loutput, const struct lstopo_color *lcolor, unsigned depth, unsigned x1, unsigned y1, unsigned x2, unsigned y2);
  void (*text) (struct lstopo_output *loutput, const struct lstopo_color *lcolor, int size, unsigned depth, unsigned x, unsigned y, const char *text);
  /* may be called when loutput->drawing == LSTOPO_DRAWING_PREPARE */
  void (*textsize) (struct lstopo_output *loutput, const char *text, unsigned textlength, unsigned fontsize, unsigned *width);
};

extern void output_draw(struct lstopo_output *output);

extern void lstopo_prepare_custom_styles(struct lstopo_output *loutput);
extern void declare_colors(struct lstopo_output *output);

int rgb_to_color(int r, int g, int b) __hwloc_attribute_const;
int declare_color(int r, int g, int b);


static __hwloc_inline int lstopo_pu_forbidden(struct lstopo_output *loutput, hwloc_obj_t l)
{
  hwloc_topology_t topology = loutput->topology;
  return !hwloc_bitmap_isset(hwloc_topology_get_allowed_cpuset(topology), l->os_index);
}

static __hwloc_inline int lstopo_pu_binding(struct lstopo_output *loutput, hwloc_obj_t l)
{
  hwloc_topology_t topology = loutput->topology;
  hwloc_bitmap_t bind = hwloc_bitmap_alloc();
  int res;
  if (loutput->pid_number != -1 && loutput->pid_number != 0)
    hwloc_get_proc_cpubind(topology, loutput->pid, bind, 0);
  else if (loutput->pid_number == 0)
    hwloc_get_cpubind(topology, bind, 0);
  res = bind && hwloc_bitmap_isset(bind, l->os_index);
  hwloc_bitmap_free(bind);
  return res;
}

static __hwloc_inline int lstopo_numa_forbidden(struct lstopo_output *loutput, hwloc_obj_t l)
{
  hwloc_topology_t topology = loutput->topology;
  return !hwloc_bitmap_isset(hwloc_topology_get_allowed_nodeset(topology), l->os_index);
}

static __hwloc_inline int lstopo_numa_binding(struct lstopo_output *loutput, hwloc_obj_t l)
{
  hwloc_topology_t topology = loutput->topology;
  hwloc_bitmap_t bind = hwloc_bitmap_alloc();
  hwloc_membind_policy_t policy;
  int res;
  if (loutput->pid_number != -1 && loutput->pid_number != 0)
    hwloc_get_proc_membind(topology, loutput->pid, bind, &policy, HWLOC_MEMBIND_BYNODESET);
  else if (loutput->pid_number == 0)
    hwloc_get_membind(topology, bind, &policy, HWLOC_MEMBIND_BYNODESET);
  res = bind && hwloc_bitmap_isset(bind, l->os_index);
  hwloc_bitmap_free(bind);
  return res;
}

static __hwloc_inline int lstopo_busid_snprintf(char *text, size_t textlen, hwloc_obj_t firstobj, int collapse, unsigned needdomain)
{
  hwloc_obj_t lastobj;
  char domain[6] = "";
  unsigned i;

  assert(collapse >= 0); /* should be called on the first object of a collapsed range */

  if (needdomain)
    snprintf(domain, sizeof(domain), "%04x:", firstobj->attr->pcidev.domain);

  /* single busid */
  if (collapse <= 1) {
      return snprintf(text, textlen, "%s%02x:%02x.%01x",
		      domain,
		      firstobj->attr->pcidev.bus,
		      firstobj->attr->pcidev.dev,
		      firstobj->attr->pcidev.func);
  }

  for(lastobj=firstobj, i=1; i<(unsigned)collapse; i++)
    lastobj = lastobj->next_cousin;

  /* multiple busid functions for same busid device */
  if (firstobj->attr->pcidev.dev == lastobj->attr->pcidev.dev)
    return snprintf(text, textlen, "%s%02x:%02x.%01x-%01x",
		    domain,
		    firstobj->attr->pcidev.bus,
		    firstobj->attr->pcidev.dev,
		    firstobj->attr->pcidev.func,
		    lastobj->attr->pcidev.func);

  /* multiple busid devices */
  return snprintf(text, textlen, "%s%02x:%02x.%01x-%02x.%01x",
		  domain,
		  firstobj->attr->pcidev.bus,
		  firstobj->attr->pcidev.dev,
		  firstobj->attr->pcidev.func,
		  lastobj->attr->pcidev.dev,
		  lastobj->attr->pcidev.func);
}

#endif /* UTILS_LSTOPO_H */
