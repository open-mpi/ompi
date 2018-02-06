/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009-2010 Université Bordeaux
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "lstopo.h"

/* .fig back-end.  */

#define FIG_FACTOR 20

static int __hwloc_attribute_const
lcolor_to_fig(const struct lstopo_color *lcolor)
{
  int r = lcolor->r, g = lcolor->g, b = lcolor->b;

  if (r == 0xff && g == 0xff && b == 0xff)
    return 7;

  if (!r && !g && !b)
    return 0;

  return 32 + rgb_to_color(r, g, b);
}

static void
fig_declare_color(struct lstopo_output *loutput, const struct lstopo_color *lcolor)
{
  FILE *file = loutput->file;
  int r = lcolor->r, g = lcolor->g, b = lcolor->b;
  int color;

  if (r == 0xff && g == 0xff && b == 0xff)
    return;

  if (!r && !g && !b)
    return;

  color = declare_color(r, g, b);

  fprintf(file, "0 %d #%02x%02x%02x\n", 32 + color, (unsigned) r, (unsigned) g, (unsigned) b);
}

static void
fig_box(struct lstopo_output *loutput, const struct lstopo_color *lcolor, unsigned depth, unsigned x, unsigned width, unsigned y, unsigned height)
{
  FILE *file = loutput->file;

  x *= FIG_FACTOR;
  y *= FIG_FACTOR;
  width *= FIG_FACTOR;
  height *= FIG_FACTOR;
  fprintf(file, "2 2 0 1 0 %d %u -1 20 0.0 0 0 -1 0 0 5\n\t", lcolor_to_fig(lcolor), depth);
  fprintf(file, " %u %u", x, y);
  fprintf(file, " %u %u", x + width, y);
  fprintf(file, " %u %u", x + width, y + height);
  fprintf(file, " %u %u", x, y + height);
  fprintf(file, " %u %u", x, y);
  fprintf(file, "\n");
}

static void
fig_line(struct lstopo_output *loutput, const struct lstopo_color *lcolor, unsigned depth, unsigned x1, unsigned y1, unsigned x2, unsigned y2)
{
  FILE *file = loutput->file;

  x1 *= FIG_FACTOR;
  y1 *= FIG_FACTOR;
  x2 *= FIG_FACTOR;
  y2 *= FIG_FACTOR;
  fprintf(file, "2 1 0 1 0 %d %u -1 -1 0.0 0 0 -1 0 0 2\n\t", lcolor_to_fig(lcolor), depth);
  fprintf(file, " %u %u", x1, y1);
  fprintf(file, " %u %u", x2, y2);
  fprintf(file, "\n");
}

static void
fig_text(struct lstopo_output *loutput, const struct lstopo_color *lcolor, int size, unsigned depth, unsigned x, unsigned y, const char *text)
{
  FILE *file = loutput->file;
  int len = (int)strlen(text);
  int color;

  color = lcolor_to_fig(lcolor);
  x *= FIG_FACTOR;
  y *= FIG_FACTOR;
  size = (size * 16) / 10;
  fprintf(file, "4 0 %d %u -1 0 %d 0.0 4 %d %d %u %u %s\\001\n", color, depth, size, size * 10, len * size * 10, x, y + size * 10, text);
}

static struct draw_methods fig_draw_methods = {
  fig_declare_color,
  fig_box,
  fig_line,
  fig_text,
  NULL, /* textsize not supported, fallback to default estimation in get_textsize() */
};

int
output_fig (struct lstopo_output *loutput, const char *filename)
{
  FILE *output = open_output(filename, loutput->overwrite);
  if (!output) {
    fprintf(stderr, "Failed to open %s for writing (%s)\n", filename, strerror(errno));
    return -1;
  }

  loutput->file = output;
  loutput->methods = &fig_draw_methods;

  /* recurse once for preparing sizes and positions */
  loutput->drawing = LSTOPO_DRAWING_PREPARE;
  output_draw(loutput);
  loutput->drawing = LSTOPO_DRAWING_DRAW;

  fprintf(output, "#FIG 3.2  Produced by hwloc's lstopo\n");
  fprintf(output, "Landscape\n");
  fprintf(output, "Center\n");
  fprintf(output, "Inches\n");
  fprintf(output, "letter\n");
  fprintf(output, "100.00\n");	/* magnification */
  fprintf(output, "Single\n");	/* single page */
  fprintf(output, "-2\n");	/* no transparent color */
  fprintf(output, "1200 2\n");	/* 1200 ppi resolution, upper left origin */

  /* ready */
  declare_colors(loutput);
  lstopo_prepare_custom_styles(loutput);

  output_draw(loutput);

  if (output != stdout)
    fclose(output);

  return 0;
}
