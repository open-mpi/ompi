/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

#include <private/config.h>
#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "lstopo.h"

/* .fig back-end.  */

#define FIG_FACTOR 20

static void *
fig_start(void *output_, int width __hwloc_attribute_unused, int height __hwloc_attribute_unused)
{
  FILE *output = output_;
  fprintf(output, "#FIG 3.2  Produced by hwloc's lstopo\n");
  fprintf(output, "Landscape\n");
  fprintf(output, "Center\n");
  fprintf(output, "Inches\n");
  fprintf(output, "letter\n");
  fprintf(output, "100.00\n");	/* magnification */
  fprintf(output, "Single\n");	/* single page */
  fprintf(output, "-2\n");	/* no transparent color */
  fprintf(output, "1200 2\n");	/* 1200 ppi resolution, upper left origin */
  return output;
}

static int __hwloc_attribute_const
rgb_to_fig(int r, int g, int b)
{
  if (r == 0xff && g == 0xff && b == 0xff)
    return 7;

  if (!r && !g && !b)
    return 0;

  return 32 + rgb_to_color(r, g, b);
}

static void
fig_declare_color(void *output_, int r, int g, int b)
{
  FILE *output = output_;
  int color;

  if (r == 0xff && g == 0xff && b == 0xff)
    return;

  if (!r && !g && !b)
    return;

  color = declare_color(r, g, b);

  fprintf(output, "0 %d #%02x%02x%02x\n", 32 + color, r, g, b);
}

static void
fig_box(void *output_, int r, int g, int b, unsigned depth, unsigned x, unsigned width, unsigned y, unsigned height)
{
  FILE *output = output_;
  x *= FIG_FACTOR;
  y *= FIG_FACTOR;
  width *= FIG_FACTOR;
  height *= FIG_FACTOR;
  fprintf(output, "2 2 0 1 0 %d %u -1 20 0.0 0 0 -1 0 0 5\n\t", rgb_to_fig(r, g, b), depth);
  fprintf(output, " %u %u", x, y);
  fprintf(output, " %u %u", x + width, y);
  fprintf(output, " %u %u", x + width, y + height);
  fprintf(output, " %u %u", x, y + height);
  fprintf(output, " %u %u", x, y);
  fprintf(output, "\n");
}

static void
fig_line(void *output_, int r, int g, int b, unsigned depth, unsigned x1, unsigned y1, unsigned x2, unsigned y2)
{
  FILE *output = output_;
  x1 *= FIG_FACTOR;
  y1 *= FIG_FACTOR;
  x2 *= FIG_FACTOR;
  y2 *= FIG_FACTOR;
  fprintf(output, "2 1 0 1 0 %d %u -1 -1 0.0 0 0 -1 0 0 2\n\t", rgb_to_fig(r, g, b), depth);
  fprintf(output, " %u %u", x1, y1);
  fprintf(output, " %u %u", x2, y2);
  fprintf(output, "\n");
}

static void
fig_text(void *output_, int r, int g, int b, int size, unsigned depth, unsigned x, unsigned y, const char *text)
{
  FILE *output = output_;
  unsigned len = strlen(text);
  int color = rgb_to_fig(r, g, b);
  x *= FIG_FACTOR;
  y *= FIG_FACTOR;
  size = (size * 16) / 10;
  fprintf(output, "4 0 %d %u -1 0 %d 0.0 4 %d %u %u %u %s\\001\n", color, depth, size, size * 10, len * size * 10, x, y + size * 10, text);
}

static struct draw_methods fig_draw_methods = {
  .start = fig_start,
  .declare_color = fig_declare_color,
  .box = fig_box,
  .line = fig_line,
  .text = fig_text,
};

void
output_fig (hwloc_topology_t topology, const char *filename, int logical, int verbose_mode __hwloc_attribute_unused)
{
  FILE *output = open_file(filename, "w");
  if (!output) {
    fprintf(stderr, "Failed to open %s for writing (%s)\n", filename, strerror(errno));
    return;
  }

  output = output_draw_start(&fig_draw_methods, logical, topology, output);
  output_draw(&fig_draw_methods, logical, topology, output);
  fclose(output);
}
