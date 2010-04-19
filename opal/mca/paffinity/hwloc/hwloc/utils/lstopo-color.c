/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

#include <stdlib.h>
#include <stdio.h>

#include "lstopo.h"

static struct color {
  int r, g, b;
} *colors;

static int numcolors;

static int
find_color(int r, int g, int b)
{
  int i;

  for (i = 0; i < numcolors; i++)
    if (colors[i].r == r && colors[i].g == g && colors[i].b == b)
      return i;

  return -1;
}

int
rgb_to_color(int r, int g, int b)
{
  int color = find_color(r, g, b);

  if (color != -1)
    return color;

  fprintf(stderr, "color #%02x%02x%02x not declared\n", r, g, b);
  exit(EXIT_FAILURE);
}

int
declare_color(int r, int g, int b)
{
  int color = find_color(r, g, b);

  if (color != -1)
    return color;

  color = numcolors++;
  colors = realloc(colors, sizeof(*colors) * (numcolors));
  colors[color].r = r;
  colors[color].g = g;
  colors[color].b = b;

  return color;
}
