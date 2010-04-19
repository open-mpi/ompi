/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * Copyright © 2009 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/config.h>
#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#ifdef HAVE_SETLOCALE
#include <locale.h>
#endif /* HAVE_SETLOCALE */

#ifdef HAVE_NL_LANGINFO
#include <langinfo.h>
#endif /* HAVE_NL_LANGINFO */

#ifdef HAVE_PUTWC
#include <wchar.h>
#endif /* HAVE_PUTWC */

#ifdef HWLOC_HAVE_LIBTERMCAP
#include <curses.h>
#include <term.h>
#endif /* HWLOC_HAVE_LIBTERMCAP */

#include "lstopo.h"

#define indent(output, i) \
  fprintf (output, "%*s", (int) i, "");

/*
 * Console fashion text output
 */

static void
output_console_obj (hwloc_obj_t l, FILE *output, int logical, int verbose_mode)
{
  char type[32], attr[256], phys[32] = "";
  unsigned index = logical ? l->logical_index : l->os_index;
  const char *indexprefix = logical ? " #" :  " p#";
  if (show_cpuset < 2) {
    if (l->type == HWLOC_OBJ_MISC && l->name)
      fprintf(output, "%s", l->name);
    else {
      hwloc_obj_type_snprintf (type, sizeof(type), l, verbose_mode-1);
      fprintf(output, "%s", type);
    }
    if (l->depth != 0 && index != (unsigned)-1)
      fprintf(output, "%s%u", indexprefix, index);
    if (logical && l->os_index != (unsigned) -1 &&
	(verbose_mode >= 2 || l->type == HWLOC_OBJ_PU || l->type == HWLOC_OBJ_NODE))
      snprintf(phys, sizeof(phys), "phys=%u", l->os_index);
    hwloc_obj_attr_snprintf (attr, sizeof(attr), l, " ", verbose_mode-1);
    if (*phys || *attr) {
      const char *separator = *phys != '\0' && *attr!= '\0' ? " " : "";
      fprintf(output, " (%s%s%s)",
	      phys, separator, attr);
    }
    if (verbose_mode >= 2 && l->name && l->type != HWLOC_OBJ_MISC)
      fprintf(output, " \"%s\"", l->name);
  }
  if (!l->cpuset)
    return;
  if (show_cpuset == 1)
    fprintf(output, " cpuset=");
  if (show_cpuset) {
    char *cpusetstr;
    hwloc_cpuset_asprintf(&cpusetstr, l->cpuset);
    fprintf(output, "%s", cpusetstr);
    free(cpusetstr);
  }
}

/* Recursively output topology in a console fashion */
static void
output_topology (hwloc_topology_t topology, hwloc_obj_t l, hwloc_obj_t parent, FILE *output, int i, int logical, int verbose_mode)
{
  unsigned x;
  int group_identical = (verbose_mode <= 1) && !show_cpuset;
  if (group_identical
      && parent && parent->arity == 1
      && l->cpuset && parent->cpuset && hwloc_cpuset_isequal(l->cpuset, parent->cpuset)) {
    /* in non-verbose mode, merge objects with their parent is they are exactly identical */
    fprintf(output, " + ");
  } else {
    if (parent)
      fprintf(output, "\n");
    indent (output, 2*i);
    i++;
  }
  output_console_obj(l, output, logical, verbose_mode);
  if (l->arity || (!i && !l->arity))
    {
      for (x=0; x<l->arity; x++)
	output_topology (topology, l->children[x], l, output, i, logical, verbose_mode);
  }
}

/* Recursive so that multiple depth types are properly shown */
static void
output_only (hwloc_topology_t topology, hwloc_obj_t l, FILE *output, int logical, int verbose_mode)
{
  unsigned x;
  if (show_only == l->type) {
    output_console_obj (l, output, logical, verbose_mode);
    fprintf (output, "\n");
  }
  for (x=0; x<l->arity; x++)
    output_only (topology, l->children[x], output, logical, verbose_mode);
}

void output_console(hwloc_topology_t topology, const char *filename, int logical, int verbose_mode)
{
  unsigned topodepth;
  FILE *output;

  if (!filename || !strcmp(filename, "-"))
    output = stdout;
  else {
    output = open_file(filename, "w"); 
    if (!output) {
      fprintf(stderr, "Failed to open %s for writing (%s)\n", filename, strerror(errno));
      return;
    }
  }

  topodepth = hwloc_topology_get_depth(topology);

  /*
   * if verbose_mode == 0, only print the summary.
   * if verbose_mode == 1, only print the topology tree.
   * if verbose_mode > 1, print both.
   */

  if (show_only != (hwloc_obj_type_t)-1) {
    if (verbose_mode > 1)
      fprintf(output, "Only showing %s objects\n", hwloc_obj_type_string(show_only));
    output_only (topology, hwloc_get_root_obj(topology), output, logical, verbose_mode);
  } else if (verbose_mode >= 1) {
    output_topology (topology, hwloc_get_root_obj(topology), NULL, output, 0, logical, verbose_mode);
    fprintf(output, "\n");
  }

  if (verbose_mode > 1 || !verbose_mode) {
    unsigned depth;
    for (depth = 0; depth < topodepth; depth++) {
      hwloc_obj_type_t type = hwloc_get_depth_type (topology, depth);
      unsigned nbobjs = hwloc_get_nbobjs_by_depth (topology, depth);
      indent(output, depth);
      fprintf (output, "depth %u:\t%u %s%s (type #%u)\n",
	       depth, nbobjs, hwloc_obj_type_string (type), nbobjs>1?"s":"", type);
    }
  }

  if (verbose_mode > 1) {
    hwloc_const_cpuset_t complete = hwloc_topology_get_complete_cpuset(topology);
    hwloc_const_cpuset_t topo = hwloc_topology_get_topology_cpuset(topology);
    hwloc_const_cpuset_t online = hwloc_topology_get_online_cpuset(topology);
    hwloc_const_cpuset_t allowed = hwloc_topology_get_allowed_cpuset(topology);

    if (!hwloc_cpuset_isequal(topo, complete)) {
      hwloc_cpuset_t unknown = hwloc_cpuset_alloc();
      char *unknownstr;
      hwloc_cpuset_copy(unknown, complete);
      hwloc_cpuset_andnot(unknown, unknown, topo);
      hwloc_cpuset_asprintf(&unknownstr, unknown);
      fprintf (output, "%d processors not represented in topology: %s\n", hwloc_cpuset_weight(unknown), unknownstr);
      free(unknownstr);
      hwloc_cpuset_free(unknown);
    }
    if (!hwloc_cpuset_isequal(online, complete)) {
      hwloc_cpuset_t offline = hwloc_cpuset_alloc();
      char *offlinestr;
      hwloc_cpuset_copy(offline, complete);
      hwloc_cpuset_andnot(offline, offline, online);
      hwloc_cpuset_asprintf(&offlinestr, offline);
      fprintf (output, "%d processors offline: %s\n", hwloc_cpuset_weight(offline), offlinestr);
      free(offlinestr);
      hwloc_cpuset_free(offline);
    }
    if (!hwloc_cpuset_isequal(allowed, online)) {
      if (!hwloc_cpuset_isincluded(online, allowed)) {
        hwloc_cpuset_t forbidden = hwloc_cpuset_alloc();
        char *forbiddenstr;
        hwloc_cpuset_copy(forbidden, online);
        hwloc_cpuset_andnot(forbidden, forbidden, allowed);
        hwloc_cpuset_asprintf(&forbiddenstr, forbidden);
        fprintf(output, "%d processors online but not allowed: %s\n", hwloc_cpuset_weight(forbidden), forbiddenstr);
        free(forbiddenstr);
        hwloc_cpuset_free(forbidden);
      }
      if (!hwloc_cpuset_isincluded(allowed, online)) {
        hwloc_cpuset_t potential = hwloc_cpuset_alloc();
        char *potentialstr;
        hwloc_cpuset_copy(potential, allowed);
        hwloc_cpuset_andnot(potential, potential, online);
        hwloc_cpuset_asprintf(&potentialstr, potential);
        fprintf(output, "%d processors allowed but not online: %s\n", hwloc_cpuset_weight(potential), potentialstr);
        free(potentialstr);
        hwloc_cpuset_free(potential);
      }
    }
    if (!hwloc_topology_is_thissystem(topology))
      fprintf (output, "Topology not from this system\n");
  }

  fclose(output);
}

/*
 * Pretty text output
 */

/* Uses unicode bars if available */
#ifdef HAVE_PUTWC
typedef wchar_t character;
#define PRIchar "lc"
#define putcharacter(c,f) putwc(c,f)
#else /* HAVE_PUTWC */
typedef unsigned char character;
#define PRIchar "c"
#define putcharacter(c,f) putc(c,f)
#endif /* HAVE_PUTWC */

#ifdef HWLOC_HAVE_LIBTERMCAP
static int myputchar(int c) {
  return putcharacter(c, stdout);
}
#endif /* HWLOC_HAVE_LIBTERMCAP */

/* Off-screen rendering buffer */
struct cell {
  character c;
#ifdef HWLOC_HAVE_LIBTERMCAP
  int fr, fg, fb;
  int br, bg, bb;
#endif /* HWLOC_HAVE_LIBTERMCAP */
};

struct display {
  struct cell **cells;
  int width;
  int height;
  int utf8;
};

/* Allocate the off-screen buffer */
static void *
text_start(void *output __hwloc_attribute_unused, int width, int height)
{
  int j, i;
  struct display *disp = malloc(sizeof(*disp));
  /* terminals usually have narrow characters, so let's make them wider */
  width /= (gridsize/2);
  height /= gridsize;
  disp->cells = malloc(height * sizeof(*disp->cells));
  disp->width = width;
  disp->height = height;
  for (j = 0; j < height; j++) {
    disp->cells[j] = calloc(width, sizeof(**disp->cells));
    for (i = 0; i < width; i++)
      disp->cells[j][i].c = ' ';
  }
#ifdef HAVE_NL_LANGINFO
  disp->utf8 = !strcmp(nl_langinfo(CODESET), "UTF-8");
#endif /* HAVE_NL_LANGINFO */
  return disp;
}

#ifdef HWLOC_HAVE_LIBTERMCAP
/* Standard terminfo strings */
static char *initc = NULL, *initp = NULL;

/* Set text color to bright white or black according to the background */
static int set_textcolor(int rr, int gg, int bb)
{
  if (!initc && !initp && rr + gg + bb < 2) {
    if (enter_bold_mode)
      tputs(enter_bold_mode, 1, myputchar);
    return 7;
  } else {
    if (exit_attribute_mode)
      tputs(exit_attribute_mode, 1, myputchar);
    return 0;
  }
}

static void
set_color(int fr, int fg, int fb, int br, int bg, int bb)
{
  char *toput;
  int color, textcolor;

  if (initc || initp) {
    /* Can set rgb color, easy */
    textcolor = rgb_to_color(fr, fg, fb) + 16;
    color = rgb_to_color(br, bg, bb) + 16;
  } else {
    /* Magic trigger: it seems to separate colors quite well */
    int brr = br >= 0xe0;
    int bgg = bg >= 0xe0;
    int bbb = bb >= 0xe0;

    if (set_a_background)
      /* ANSI colors */
      color = (brr << 0) | (bgg << 1) | (bbb << 2);
    else
      /* Legacy colors */
      color = (brr << 2) | (bgg << 1) | (bbb << 0);
    textcolor = set_textcolor(brr, bgg, bbb);
  }

  /* And now output magic string to TTY */
  if (set_a_foreground) {
    /* foreground */
    if ((toput = tparm(set_a_foreground, textcolor, 0, 0, 0, 0, 0, 0, 0, 0)))
      tputs(toput, 1, myputchar);
    /* background */
    if ((toput = tparm(set_a_background, color, 0, 0, 0, 0, 0, 0, 0, 0)))
      tputs(toput, 1, myputchar);
  } else if (set_foreground) {
    /* foreground */
    if ((toput = tparm(set_foreground, textcolor, 0, 0, 0, 0, 0, 0, 0, 0)))
      tputs(toput, 1, myputchar);
    /* background */
    if ((toput = tparm(set_background, color, 0, 0, 0, 0, 0, 0, 0, 0)))
      tputs(toput, 1, myputchar);
  } else if (set_color_pair) {
    /* pair */
    if ((toput = tparm(set_color_pair, color, 0, 0, 0, 0, 0, 0, 0, 0)))
      tputs(toput, 1, myputchar);
  }
}
#endif /* HWLOC_HAVE_LIBTERMCAP */

/* We we can, allocate rgb colors */
static void
text_declare_color(void *output __hwloc_attribute_unused, int r, int g, int b)
{
#ifdef HWLOC_HAVE_LIBTERMCAP
  int color = declare_color(r, g, b);
  /* Yes, values seem to range from 0 to 1000 inclusive */
  int rr = (r * 1001) / 256;
  int gg = (g * 1001) / 256;
  int bb = (b * 1001) / 256;
  char *toput;

  if (initc) {
    if ((toput = tparm(initc, color + 16, rr, gg, bb, 0, 0, 0, 0, 0)))
      tputs(toput, 1, myputchar);
  } else if (initp) {
    if ((toput = tparm(initp, color + 16, 0, 0, 0, rr, gg, bb, 0, 0)))
      tputs(toput, 1, myputchar);
  }
#endif /* HWLOC_HAVE_LIBTERMCAP */
}

/* output text, erasing any previous content */
static void
put(struct display *disp, int x, int y, character c, int fr, int fg, int fb, int br, int bg, int bb)
{
  if (x >= disp->width || y >= disp->height) {
    /* fprintf(stderr, "%"PRIchar" overflowed to (%d,%d)\n", c, x, y); */
    return;
  }
  disp->cells[y][x].c = c;
#ifdef HWLOC_HAVE_LIBTERMCAP
  if (fr != -1) {
    disp->cells[y][x].fr = fr;
    disp->cells[y][x].fg = fg;
    disp->cells[y][x].fb = fb;
  }
  if (br != -1) {
    disp->cells[y][x].br = br;
    disp->cells[y][x].bg = bg;
    disp->cells[y][x].bb = bb;
  }
#endif /* HWLOC_HAVE_LIBTERMCAP */
}

/* Where bars of a character go to */
enum {
  up = (1<<0),
  down = (1<<1),
  left = (1<<2),
  right = (1<<3),
};

/* Convert a bar character into its directions */
static int
to_directions(struct display *disp, character c)
{
#ifdef HAVE_PUTWC
  if (disp->utf8) {
    switch (c) {
      case L'\x250c': return down|right;
      case L'\x2510': return down|left;
      case L'\x2514': return up|right;
      case L'\x2518': return up|left;
      case L'\x2500': return left|right;
      case L'\x2502': return down|up;
      case L'\x2577': return down;
      case L'\x2575': return up;
      case L'\x2576': return right;
      case L'\x2574': return left;
      case L'\x251c': return down|up|right;
      case L'\x2524': return down|up|left;
      case L'\x252c': return down|left|right;
      case L'\x2534': return up|left|right;
      case L'\x253c': return down|up|left|right;
      default: return 0;
    }
  } else
#endif /* HAVE_PUTWC */
  {
    switch (c) {
      case L'-': return left|right;
      case L'|': return down|up;
      case L'/':
      case L'\\':
      case L'+': return down|up|left|right;
      default: return 0;
    }
  }
}

/* Produce a bar character given the wanted directions */
static character
from_directions(struct display *disp, int direction)
{
#ifdef HAVE_PUTWC
  if (disp->utf8) {
    static const wchar_t chars[] = {
      [down|right]	= L'\x250c',
      [down|left]	= L'\x2510',
      [up|right]	= L'\x2514',
      [up|left]		= L'\x2518',
      [left|right]	= L'\x2500',
      [down|up]		= L'\x2502',
      [down]		= L'\x2577',
      [up]		= L'\x2575',
      [right]		= L'\x2576',
      [left]		= L'\x2574',
      [down|up|right]	= L'\x251c',
      [down|up|left]	= L'\x2524',
      [down|left|right]	= L'\x252c',
      [up|left|right]	= L'\x2534',
      [down|up|left|right]	= L'\x253c',
      [0]		= L' ',
    };
    return chars[direction];
  } else
#endif /* HAVE_PUTWC */
  {
    static const char chars[] = {
      [down|right]	= '/',
      [down|left]	= '\\',
      [up|right]	= '\\',
      [up|left]		= '/',
      [left|right]	= '-',
      [down|up]		= '|',
      [down]		= '|',
      [up]		= '|',
      [right]		= '-',
      [left]		= '-',
      [down|up|right]	= '+',
      [down|up|left]	= '+',
      [down|left|right]	= '+',
      [up|left|right]	= '+',
      [down|up|left|right]	= '+',
      [0]		= ' ',
    };
    return chars[direction];
  }
}

/* output bars, merging with existing bars: `andnot' are removed, `or' are added */
static void
merge(struct display *disp, int x, int y, int or, int andnot, int r, int g, int b)
{
  character current = disp->cells[y][x].c;
  int directions = (to_directions(disp, current) & ~andnot) | or;
  put(disp, x, y, from_directions(disp, directions), -1, -1, -1, r, g, b);
}

/* Now we can implement the standard drawing helpers */
static void
text_box(void *output, int r, int g, int b, unsigned depth __hwloc_attribute_unused, unsigned x1, unsigned width, unsigned y1, unsigned height)
{
  struct display *disp = output;
  unsigned i, j;
  unsigned x2, y2;
  x1 /= (gridsize/2);
  width /= (gridsize/2);
  y1 /= gridsize;
  height /= gridsize;
  x2 = x1 + width - 1;
  y2 = y1 + height - 1;

  /* Corners */
  merge(disp, x1, y1, down|right, 0, r, g, b);
  merge(disp, x2, y1, down|left, 0, r, g, b);
  merge(disp, x1, y2, up|right, 0, r, g, b);
  merge(disp, x2, y2, up|left, 0, r, g, b);

  for (i = 1; i < width - 1; i++) {
    /* upper line */
    merge(disp, x1 + i, y1, left|right, down, r, g, b);
    /* lower line */
    merge(disp, x1 + i, y2, left|right, up, r, g, b);
  }
  for (j = 1; j < height - 1; j++) {
    /* left line */
    merge(disp, x1, y1 + j, up|down, right, r, g, b);
    /* right line */
    merge(disp, x2, y1 + j, up|down, left, r, g, b);
  }
  for (j = y1 + 1; j < y2; j++) {
    for (i = x1 + 1; i < x2; i++) {
      put(disp, i, j, ' ', -1, -1, -1, r, g, b);
    }
  }
}

static void
text_line(void *output, int r __hwloc_attribute_unused, int g __hwloc_attribute_unused, int b __hwloc_attribute_unused, unsigned depth __hwloc_attribute_unused, unsigned x1, unsigned y1, unsigned x2, unsigned y2)
{
  struct display *disp = output;
  unsigned i, j, z;
  x1 /= (gridsize/2);
  y1 /= gridsize;
  x2 /= (gridsize/2);
  y2 /= gridsize;

  /* Canonicalize coordinates */
  if (x1 > x2) {
    z = x1;
    x1 = x2;
    x2 = z;
  }
  if (y1 > y2) {
    z = y1;
    y1 = y2;
    y2 = z;
  }

  /* vertical/horizontal should be enough, but should mix with existing
   * characters for better output ! */

  if (x1 == x2) {
    /* Vertical */
    if (y1 == y2) {
      /* Hu ?! That's a point, let's do nothing... */
    } else {
      /* top */
      merge(disp, x1, y1, down, 0, -1, -1, -1);
      /* bottom */
      merge(disp, x1, y2, up, 0, -1, -1, -1);
    }
    for (j = y1 + 1; j < y2; j++)
      merge(disp, x1, j, down|up, 0, -1, -1, -1);
  } else if (y1 == y2) {
    /* Horizontal */
    /* left */
    merge(disp, x1, y1, right, 0, -1, -1, -1);
    /* right */
    merge(disp, x2, y1, left, 0, -1, -1, -1);
    for (i = x1 + 1; i < x2; i++)
      merge(disp, i, y1, left|right, 0, -1, -1, -1);
  } else {
    /* Unsupported, sorry */
  }
}

static void
text_text(void *output, int r, int g, int b, int size __hwloc_attribute_unused, unsigned depth __hwloc_attribute_unused, unsigned x, unsigned y, const char *text)
{
  struct display *disp = output;
  x /= (gridsize/2);
  y /= gridsize;
  for ( ; *text; text++)
    put(disp, x++, y, *text, r, g, b, -1, -1, -1);
}

static struct draw_methods text_draw_methods = {
  .start = text_start,
  .declare_color = text_declare_color,
  .box = text_box,
  .line = text_line,
  .text = text_text,
};

void output_text(hwloc_topology_t topology, const char *filename, int logical, int verbose_mode __hwloc_attribute_unused)
{
  FILE *output;
  struct display *disp;
  int i, j;
  int lfr, lfg, lfb; /* Last foreground color */
  int lbr, lbg, lbb; /* Last background color */
#ifdef HWLOC_HAVE_LIBTERMCAP
  int term = 0;
#endif

  if (!filename || !strcmp(filename, "-"))
    output = stdout;
  else {
    output = open_file(filename, "w"); 
    if (!output) {
      fprintf(stderr, "Failed to open %s for writing (%s)\n", filename, strerror(errno));
      return;
    }
  }

  /* Try to use utf-8 characters */
#ifdef HAVE_SETLOCALE
  setlocale(LC_ALL, "");
#endif /* HAVE_SETLOCALE */

#ifdef HWLOC_HAVE_LIBTERMCAP
  /* If we are outputing to a tty, use colors */
  if (output == stdout && isatty(STDOUT_FILENO)) {
    term = !setupterm(NULL, STDOUT_FILENO, NULL);

    if (term) {
      /* reset colors */
      if (orig_colors)
        tputs(orig_colors, 1, myputchar);

      /* Get terminfo(5) strings */
      initp = initialize_pair;
      if (max_pairs <= 16 || !initp || !set_color_pair) {
	/* Can't use max_pairs to define our own colors */
	initp = NULL;
	if (max_colors > 16)
	  if (can_change)
            initc = initialize_color;
      }
      if (tgetflag("lhs"))
	/* Sorry, I'm lazy to convert colors and I don't know any terminal
	 * using LHS anyway */
	initc = initp = 0;
    }
  }
#endif /* HWLOC_HAVE_LIBTERMCAP */

  disp = output_draw_start(&text_draw_methods, logical, topology, output);
  output_draw(&text_draw_methods, logical, topology, disp);

  lfr = lfg = lfb = -1;
  lbr = lbg = lbb = -1;
  for (j = 0; j < disp->height; j++) {
    for (i = 0; i < disp->width; i++) {
#ifdef HWLOC_HAVE_LIBTERMCAP
      if (term) {
	/* TTY output, use colors */
	int fr = disp->cells[j][i].fr;
	int fg = disp->cells[j][i].fg;
	int fb = disp->cells[j][i].fb;
	int br = disp->cells[j][i].br;
	int bg = disp->cells[j][i].bg;
	int bb = disp->cells[j][i].bb;

	/* Avoid too much work for the TTY */
	if (fr != lfr || fg != lfg || fb != lfb
	 || br != lbr || bg != lbg || bb != lbb) {
	  set_color(fr, fg, fb, br, bg, bb);
	  lfr = fr;
	  lfg = fg;
	  lfb = fb;
	  lbr = br;
	  lbg = bg;
	  lbb = bb;
	}
      }
#endif /* HWLOC_HAVE_LIBTERMCAP */
      putcharacter(disp->cells[j][i].c, output);
    }
#ifdef HWLOC_HAVE_LIBTERMCAP
    /* Keep the rest of the line as default */
    if (term && orig_pair) {
      lfr = lfg = lfb = -1;
      lbr = lbg = lbb = -1;
      tputs(orig_pair, 1, myputchar);
    }
#endif /* HWLOC_HAVE_LIBTERMCAP */
    putcharacter('\n', output);
  }
}
