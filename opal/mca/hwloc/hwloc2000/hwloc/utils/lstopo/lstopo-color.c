/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009 Université Bordeaux
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>

#include <stdlib.h>
#include <stdio.h>

#include "lstopo.h"

static struct lstopo_color *colors;
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

  fprintf(stderr, "color #%02x%02x%02x not declared\n", (unsigned) r, (unsigned) g, (unsigned) b);
  exit(EXIT_FAILURE);
}

int
declare_color(int r, int g, int b)
{
  int color = find_color(r, g, b);
  struct lstopo_color *tmp;

  if (color != -1)
    return color;

  tmp = realloc(colors, sizeof(*colors) * (numcolors+1));

  if (!tmp) {
    /* FIXME: return -1 on error? tell the caller to use the default? */
    fprintf(stderr, "Failed to realloc the colors array\n");
    exit(EXIT_FAILURE);
  }

  colors = tmp;
  color = numcolors++;
  colors[color].r = r;
  colors[color].g = g;
  colors[color].b = b;

  return color;
}
