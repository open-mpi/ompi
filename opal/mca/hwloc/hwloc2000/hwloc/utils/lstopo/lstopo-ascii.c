/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009-2012 Université Bordeaux
 * Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

/*
 * Pretty text output
 */

#include <private/autogen/config.h>
#include <hwloc.h>

#ifdef HAVE_NL_LANGINFO
#include <langinfo.h>
#endif /* HAVE_NL_LANGINFO */

#ifdef HAVE_PUTWC
#include <wchar.h>
#endif /* HAVE_PUTWC */

#ifdef HWLOC_HAVE_LIBTERMCAP
#ifdef HWLOC_USE_NCURSES
#  include <ncurses.h>
#else
#  include <curses.h>
#endif
#include <term.h>
#endif /* HWLOC_HAVE_LIBTERMCAP */

#include "lstopo.h"

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

struct lstopo_ascii_output {
  struct lstopo_output *loutput;
  struct cell **cells;
  int utf8;
  /* loutput->width and ->height converted to array of chars to simplify internal management below*/
  int width;
  int height;
};

static struct draw_methods ascii_draw_methods;

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
ascii_declare_color(struct lstopo_output *loutput __hwloc_attribute_unused, const struct lstopo_color *lcolor __hwloc_attribute_unused)
{
#ifdef HWLOC_HAVE_LIBTERMCAP
  int r = lcolor->r, g = lcolor->g, b = lcolor->b;
  int color, rr, gg, bb;
  char *toput;
#endif

#ifdef HWLOC_HAVE_LIBTERMCAP
  color = declare_color(r, g, b);
  /* Yes, values seem to range from 0 to 1000 inclusive */
  rr = (r * 1001) / 256;
  gg = (g * 1001) / 256;
  bb = (b * 1001) / 256;

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
put(struct lstopo_ascii_output *disp, int x, int y, character c, int fr __hwloc_attribute_unused, int fg __hwloc_attribute_unused, int fb __hwloc_attribute_unused, int br __hwloc_attribute_unused, int bg __hwloc_attribute_unused, int bb __hwloc_attribute_unused)
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
  right = (1<<3)
};

/* Convert a bar character into its directions */
static int
to_directions(struct lstopo_ascii_output *disp, character c)
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
from_directions(struct lstopo_ascii_output *disp, int direction)
{
#ifdef HAVE_PUTWC
  if (disp->utf8) {
    switch (direction) {
    case down|right:		return L'\x250c';
    case down|left:		return L'\x2510';
    case up|right:		return L'\x2514';
    case up|left:		return L'\x2518';
    case left|right:		return L'\x2500';
    case down|up:		return L'\x2502';
    case down:			return L'\x2577';
    case up:			return L'\x2575';
    case right:			return L'\x2576';
    case left:			return L'\x2574';
    case down|up|right:		return L'\x251c';
    case down|up|left:		return L'\x2524';
    case down|left|right:	return L'\x252c';
    case up|left|right:		return L'\x2534';
    case down|up|left|right:	return L'\x253c';
    default:			return L' ';
    };
  } else
#endif /* HAVE_PUTWC */
  {
    switch (direction) {
    case down|right:		return '/';
    case down|left:		return '\\';
    case up|right:		return '\\';
    case up|left:		return '/';
    case left|right:		return '-';
    case down|up:		return '|';
    case down:			return '|';
    case up:			return '|';
    case right:			return '-';
    case left:			return '-';
    case down|up|right:		return '+';
    case down|up|left:		return '+';
    case down|left|right:	return '+';
    case up|left|right:		return '+';
    case down|up|left|right:	return '+';
    default:			return ' ';
    };
  }
}

/* output bars, merging with existing bars: `andnot' are removed, `or' are added */
static void
merge(struct lstopo_ascii_output *disp, int x, int y, int or, int andnot, int r, int g, int b)
{
  character current;
  int directions;
  if (x >= disp->width || y >= disp->height) {
    /* fprintf(stderr, "|%x &~%x overflowed to (%d,%d)\n", or, andnot, x, y); */
    return;
  }
  current = disp->cells[y][x].c;
  directions = (to_directions(disp, current) & ~andnot) | or;
  put(disp, x, y, from_directions(disp, directions), -1, -1, -1, r, g, b);
}

/* Now we can implement the standard drawing helpers */
static void
ascii_box(struct lstopo_output *loutput, const struct lstopo_color *lcolor, unsigned depth __hwloc_attribute_unused, unsigned x1, unsigned width, unsigned y1, unsigned height)
{
  struct lstopo_ascii_output *disp = loutput->backend_data;
  int r = lcolor->r, g = lcolor->g, b = lcolor->b;
  unsigned gridsize = loutput->gridsize;
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
ascii_line(struct lstopo_output *loutput, const struct lstopo_color *lcolor __hwloc_attribute_unused, unsigned depth __hwloc_attribute_unused, unsigned x1, unsigned y1, unsigned x2, unsigned y2)
{
  struct lstopo_ascii_output *disp = loutput->backend_data;
  unsigned gridsize = loutput->gridsize;
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
ascii_text(struct lstopo_output *loutput, const struct lstopo_color *lcolor, int size __hwloc_attribute_unused, unsigned depth __hwloc_attribute_unused, unsigned x, unsigned y, const char *text)
{
  struct lstopo_ascii_output *disp = loutput->backend_data;
  int r = lcolor->r, g = lcolor->g, b = lcolor->b;
  unsigned gridsize = loutput->gridsize;

  x /= (gridsize/2);
  y /= gridsize;

#if defined(HAVE_PUTWC) && !defined(__MINGW32__) && !defined(_MSC_VER)
  {
    size_t len = strlen(text) + 1;
    wchar_t *wbuf = malloc(len * sizeof(wchar_t)), *wtext;
    swprintf(wbuf, len, L"%s", text);
    for (wtext = wbuf ; *wtext; wtext++)
      put(disp, x++, y, *wtext, r, g, b, -1, -1, -1);
    free(wbuf);
  }
#else
  for ( ; *text; text++)
    put(disp, x++, y, *text, r, g, b, -1, -1, -1);
#endif
}

static void
ascii_textsize(struct lstopo_output *loutput, const char *text __hwloc_attribute_unused, unsigned textlength, unsigned fontsize __hwloc_attribute_unused, unsigned *width)
{
  unsigned gridsize = loutput->gridsize;
  *width = textlength*(gridsize/2);
}

static struct draw_methods ascii_draw_methods = {
  ascii_declare_color,
  ascii_box,
  ascii_line,
  ascii_text,
  ascii_textsize,
};

int
output_ascii(struct lstopo_output *loutput, const char *filename)
{
  FILE *output;
  struct lstopo_ascii_output disp;
  int i, j;
  int lfr, lfg, lfb; /* Last foreground color */
  int lbr, lbg, lbb; /* Last background color */
#ifdef HWLOC_HAVE_LIBTERMCAP
  int term = 0;
  char *tmp;
#endif
  unsigned gridsize = loutput->gridsize;
  int width, height;

  output = open_output(filename, loutput->overwrite);
  if (!output) {
    fprintf(stderr, "Failed to open %s for writing (%s)\n", filename, strerror(errno));
    return -1;
  }

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
      /* Prevent a trivial compiler warning because the param of
         tgetflag is (char*), not (const char*). */
      tmp = strdup("lhs");
      if (tgetflag(tmp)) {
	/* Sorry, I'm lazy to convert colors and I don't know any terminal
	 * using LHS anyway */
	initc = initp = 0;
      }
      free(tmp);
    }
  }
#endif /* HWLOC_HAVE_LIBTERMCAP */

  disp.loutput = loutput;
  loutput->backend_data = &disp;
  loutput->methods = &ascii_draw_methods;

  /* recurse once for preparing sizes and positions */
  loutput->drawing = LSTOPO_DRAWING_PREPARE;
  output_draw(loutput);
  width = disp.width = (loutput->width + 1) / (gridsize/2);
  height = disp.height = (loutput->height + 1) / gridsize;
  loutput->drawing = LSTOPO_DRAWING_DRAW;

  /* terminals usually have narrow characters, so let's make them wider */
  disp.cells = malloc(height * sizeof(*disp.cells));
  for (j = 0; j < height; j++) {
    disp.cells[j] = calloc(width, sizeof(**disp.cells));
    for (i = 0; i < width; i++)
      disp.cells[j][i].c = ' ';
  }
#ifdef HAVE_NL_LANGINFO
  disp.utf8 = !strcmp(nl_langinfo(CODESET), "UTF-8");
#endif /* HAVE_NL_LANGINFO */

  /* ready */
  declare_colors(loutput);
  lstopo_prepare_custom_styles(loutput);

  output_draw(loutput);

  lfr = lfg = lfb = -1;
  lbr = lbg = lbb = -1;
  for (j = 0; j < disp.height; j++) {
    for (i = 0; i < disp.width; i++) {
#ifdef HWLOC_HAVE_LIBTERMCAP
      if (term) {
	/* TTY output, use colors */
	int fr = disp.cells[j][i].fr;
	int fg = disp.cells[j][i].fg;
	int fb = disp.cells[j][i].fb;
	int br = disp.cells[j][i].br;
	int bg = disp.cells[j][i].bg;
	int bb = disp.cells[j][i].bb;

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
      putcharacter(disp.cells[j][i].c, output);
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

  for (j = 0; j < disp.height; j++)
    free(disp.cells[j]);
  free(disp.cells);

  if (output != stdout)
    fclose(output);

  return 0;
}
