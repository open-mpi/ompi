/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * Copyright © 2009 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/config.h>

#ifdef HWLOC_HAVE_CAIRO
#include <cairo.h>

#if CAIRO_HAS_PDF_SURFACE
#include <cairo-pdf.h>
#endif /* CAIRO_HAS_PDF_SURFACE */

#if CAIRO_HAS_PS_SURFACE
#include <cairo-ps.h>
#endif /* CAIRO_HAS_PS_SURFACE */

#if CAIRO_HAS_SVG_SURFACE
#include <cairo-svg.h>
#endif /* CAIRO_HAS_SVG_SURFACE */

#ifndef HWLOC_HAVE_X11
/* In case X11 headers aren't availble, forcefully disable Cairo/Xlib.  */
# undef CAIRO_HAS_XLIB_SURFACE
# define CAIRO_HAS_XLIB_SURFACE 0
#endif

#if CAIRO_HAS_XLIB_SURFACE
#include <cairo-xlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>
#endif /* CAIRO_HAS_XLIB_SURFACE */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

#include "lstopo.h"

#if (CAIRO_HAS_XLIB_SURFACE + CAIRO_HAS_PNG_FUNCTIONS + CAIRO_HAS_PDF_SURFACE + CAIRO_HAS_PS_SURFACE + CAIRO_HAS_SVG_SURFACE)
/* Cairo methods */
static void
topo_cairo_box(void *output, int r, int g, int b, unsigned depth __hwloc_attribute_unused, unsigned x, unsigned width, unsigned y, unsigned height)
{
  cairo_t *c = output;
  cairo_rectangle(c, x, y, width, height);
  cairo_set_source_rgb(c, (float)r / 255, (float) g / 255, (float) b / 255);
  cairo_fill(c);

  cairo_rectangle(c, x, y, width, height);
  cairo_set_source_rgb(c, 0, 0, 0);
  cairo_set_line_width(c, 1);
  cairo_stroke(c);
}

static void
topo_cairo_line(void *output, int r, int g, int b, unsigned depth __hwloc_attribute_unused, unsigned x1, unsigned y1, unsigned x2, unsigned y2)
{
  cairo_t *c = output;
  cairo_move_to(c, x1, y1);
  cairo_set_source_rgb(c, (float) r / 255, (float) g / 255, (float) b / 255);
  cairo_set_line_width(c, 1);
  cairo_line_to(c, x2, y2);
  cairo_stroke(c);
}

static void
topo_cairo_text(void *output, int r, int g, int b, int size, unsigned depth __hwloc_attribute_unused, unsigned x, unsigned y, const char *text)
{
  cairo_t *c = output;
  cairo_move_to(c, x, y + size);
  cairo_set_font_size(c, size);
  cairo_set_source_rgb(c, (float)r / 255, (float) g / 255, (float) b / 255);
  cairo_show_text(c, text);
}

#if (CAIRO_HAS_PNG_FUNCTIONS + CAIRO_HAS_PDF_SURFACE + CAIRO_HAS_PS_SURFACE + CAIRO_HAS_SVG_SURFACE)
static cairo_status_t
topo_cairo_write(void *closure, const unsigned char *data, unsigned int length)
{
  if (fwrite(data, length, 1, closure) < 1)
    return CAIRO_STATUS_WRITE_ERROR;
  return CAIRO_STATUS_SUCCESS;
}
#endif /* (CAIRO_HAS_PNG_FUNCTIONS + CAIRO_HAS_PDF_SURFACE + CAIRO_HAS_PS_SURFACE + CAIRO_HAS_SVG_SURFACE) */

static void
topo_cairo_paint(struct draw_methods *methods, int logical, hwloc_topology_t topology, cairo_surface_t *cs)
{
  cairo_t *c;
  c = cairo_create(cs);
  output_draw(methods, logical, topology, c);
  cairo_show_page(c);
  cairo_destroy(c);
}

static void null_declare_color (void *output __hwloc_attribute_unused, int r __hwloc_attribute_unused, int g __hwloc_attribute_unused, int b __hwloc_attribute_unused) {}
#endif /* (CAIRO_HAS_XLIB_SURFACE + CAIRO_HAS_PNG_FUNCTIONS + CAIRO_HAS_PDF_SURFACE + CAIRO_HAS_PS_SURFACE + CAIRO_HAS_SVG_SURFACE) */


#if CAIRO_HAS_XLIB_SURFACE
/* X11 back-end */

struct display {
  Display *dpy;
  cairo_surface_t *cs;
  Window win;
  Cursor hand;
  int screen_width, screen_height;	/** visible part size */
  int width, height;			/** total size */
  int x, y;				/** top left corner of the visible part */
};

static void *
x11_start(void *output __hwloc_attribute_unused, int width, int height)
{
  cairo_surface_t *cs;
  Display *dpy;
  Window root, top, win;
  int scr;
  Screen *screen;
  int screen_width = width, screen_height = height;
  struct display *disp;
  Cursor hand;

  if (!(dpy = XOpenDisplay(NULL))) {
    fprintf(stderr, "couldn't connect to X\n");
    exit(EXIT_FAILURE);
  }
  scr = DefaultScreen(dpy);
  screen = ScreenOfDisplay(dpy, scr);
  if (screen_width >= screen->width)
    screen_width = screen->width;
  if (screen_height >= screen->height)
    screen_height = screen->height;
  root = RootWindow(dpy, scr);
  top = XCreateSimpleWindow(dpy, root, 0, 0, screen_width, screen_height, 0, WhitePixel(dpy, scr), WhitePixel(dpy, scr));
  win = XCreateSimpleWindow(dpy, top, 0, 0, width, height, 0, WhitePixel(dpy, scr), WhitePixel(dpy, scr));
  hand = XCreateFontCursor(dpy, XC_fleur);
  XDefineCursor(dpy, win, hand);

  XSelectInput(dpy, win,
      KeyPressMask |
      ButtonPressMask | ButtonReleaseMask |
      PointerMotionMask |
      ExposureMask);
  XSelectInput(dpy,top, StructureNotifyMask);
  XMapWindow(dpy, win);
  XMapWindow(dpy, top);

  cs = cairo_xlib_surface_create(dpy, win, DefaultVisual(dpy, scr), width, height);

  disp = malloc(sizeof(*disp));
  disp->dpy = dpy;
  disp->cs = cs;
  disp->win = win;
  disp->hand = hand;
  disp->screen_width = screen_width;
  disp->screen_height = screen_height;
  disp->width = width;
  disp->height = height;
  disp->x = 0;
  disp->y = 0;

  return disp;
}

static struct draw_methods x11_draw_methods = {
  .start = x11_start,
  .declare_color = null_declare_color,
  .box = topo_cairo_box,
  .line = topo_cairo_line,
  .text = topo_cairo_text,
};

/** Clip coordinates of the visible part. */
static void
move_x11(struct display *disp)
{
  if (disp->width <= disp->screen_width) {
    disp->x = 0;
  } else {
    if (disp->x < 0)
      disp->x = 0;
    if (disp->x >= disp->width - disp->screen_width)
      disp->x = disp->width - disp->screen_width;
  }

  if (disp->height <= disp->screen_height) {
    disp->y = 0;
  } else {
    if (disp->y < 0)
      disp->y = 0;
    if (disp->y >= disp->height - disp->screen_height)
      disp->y = disp->height - disp->screen_height;
  }
}

void
output_x11(hwloc_topology_t topology, const char *filename __hwloc_attribute_unused, int logical, int verbose_mode __hwloc_attribute_unused)
{
  struct display *disp = output_draw_start(&x11_draw_methods, logical, topology, NULL);
  int finish = 0;
  int state = 0;
  int x = 0, y = 0; /* shut warning down */
  int lastx = disp->x, lasty = disp->y;

  topo_cairo_paint(&x11_draw_methods, logical, topology, disp->cs);

  while (!finish) {
    XEvent e;
    if (!XEventsQueued(disp->dpy, QueuedAfterFlush)) {
      /* No pending event, flush moving windows before waiting for next event */
      if (disp->x != lastx || disp->y != lasty) {
	XMoveWindow(disp->dpy, disp->win, -disp->x, -disp->y);
	lastx = disp->x;
	lasty = disp->y;
      }
    }
    XNextEvent(disp->dpy, &e);
    switch (e.type) {
      case Expose:
	if (e.xexpose.count < 1)
	  topo_cairo_paint(&x11_draw_methods, logical, topology, disp->cs);
	break;
      case MotionNotify:
	if (state) {
	  disp->x -= e.xmotion.x_root - x;
	  disp->y -= e.xmotion.y_root - y;
	  x = e.xmotion.x_root;
	  y = e.xmotion.y_root;
	  move_x11(disp);
	}
	break;
      case ConfigureNotify:
	disp->screen_width = e.xconfigure.width;
	disp->screen_height = e.xconfigure.height;
	move_x11(disp);
	if (disp->x != lastx || disp->y != lasty)
	  XMoveWindow(disp->dpy, disp->win, -disp->x, -disp->y);
	break;
      case ButtonPress:
	  if (e.xbutton.button == Button1) {
	  state = 1;
	  x = e.xbutton.x_root;
	  y = e.xbutton.y_root;
	}
	break;
      case ButtonRelease:
	if (e.xbutton.button == Button1)
	  state = 0;
	break;
      case MappingNotify:
	XRefreshKeyboardMapping(&e.xmapping);
	break;
      case KeyPress: {
	KeySym keysym;
	XLookupString(&e.xkey, NULL, 0, &keysym, NULL);
        switch (keysym) {
          case XK_q:
          case XK_Q:
          case XK_Escape:
            finish = 1;
            break;
          case XK_Left:
            disp->x -= disp->screen_width/10;
            move_x11(disp);
            break;
          case XK_Right:
            disp->x += disp->screen_width/10;
            move_x11(disp);
            break;
          case XK_Up:
            disp->y -= disp->screen_height/10;
            move_x11(disp);
            break;
          case XK_Down:
            disp->y += disp->screen_height/10;
            move_x11(disp);
            break;
          case XK_Page_Up:
            if (e.xkey.state & ControlMask) {
              disp->x -= disp->screen_width;
              move_x11(disp);
            } else {
              disp->y -= disp->screen_height;
              move_x11(disp);
            }
            break;
          case XK_Page_Down:
            if (e.xkey.state & ControlMask) {
              disp->x += disp->screen_width;
              move_x11(disp);
            } else {
              disp->y += disp->screen_height;
              move_x11(disp);
            }
            break;
          case XK_Home:
            disp->x = 0;
            disp->y = 0;
            move_x11(disp);
            break;
          case XK_End:
            disp->x = INT_MAX;
            disp->y = INT_MAX;
            move_x11(disp);
            break;
        }
	break;
      }
    }
  }
  cairo_surface_destroy(disp->cs);
  XFreeCursor(disp->dpy, disp->hand);
  XCloseDisplay(disp->dpy);
  free(disp);
}
#endif /* CAIRO_HAS_XLIB_SURFACE */


#if CAIRO_HAS_PNG_FUNCTIONS
/* PNG back-end */
static void *
png_start(void *output __hwloc_attribute_unused, int width, int height)
{
  return cairo_image_surface_create(CAIRO_FORMAT_ARGB32, width, height);
}

static struct draw_methods png_draw_methods = {
  .start = png_start,
  .declare_color = null_declare_color,
  .box = topo_cairo_box,
  .line = topo_cairo_line,
  .text = topo_cairo_text,
};

void
output_png(hwloc_topology_t topology, const char *filename, int logical, int verbose_mode __hwloc_attribute_unused)
{
  FILE *output = open_file(filename, "w");
  cairo_surface_t *cs;

  if (!output) {
    fprintf(stderr, "Failed to open %s for writing (%s)\n", filename, strerror(errno));
    return;
  }

  cs = output_draw_start(&png_draw_methods, logical, topology, output);

  topo_cairo_paint(&png_draw_methods, logical, topology, cs);
  cairo_surface_write_to_png_stream(cs, topo_cairo_write, output);
  cairo_surface_destroy(cs);
  fclose(output);
}
#endif /* CAIRO_HAS_PNG_FUNCTIONS */


#if CAIRO_HAS_PDF_SURFACE
/* PDF back-end */
static void *
pdf_start(void *output, int width, int height)
{
  return cairo_pdf_surface_create_for_stream(topo_cairo_write, output, width, height);
}

static struct draw_methods pdf_draw_methods = {
  .start = pdf_start,
  .declare_color = null_declare_color,
  .box = topo_cairo_box,
  .line = topo_cairo_line,
  .text = topo_cairo_text,
};

void
output_pdf(hwloc_topology_t topology, const char *filename, int logical, int verbose_mode __hwloc_attribute_unused)
{
  FILE *output = open_file(filename, "w");
  cairo_surface_t *cs;

  if (!output) {
    fprintf(stderr, "Failed to open %s for writing (%s)\n", filename, strerror(errno));
    return;
  }

  cs = output_draw_start(&pdf_draw_methods, logical, topology, output);

  topo_cairo_paint(&pdf_draw_methods, logical, topology, cs);
  cairo_surface_flush(cs);
  cairo_surface_destroy(cs);
  fclose(output);
}
#endif /* CAIRO_HAS_PDF_SURFACE */


#if CAIRO_HAS_PS_SURFACE
/* PS back-end */
static void *
ps_start(void *output, int width, int height)
{
  return cairo_ps_surface_create_for_stream(topo_cairo_write, output, width, height);
}

static struct draw_methods ps_draw_methods = {
  .start = ps_start,
  .declare_color = null_declare_color,
  .box = topo_cairo_box,
  .line = topo_cairo_line,
  .text = topo_cairo_text,
};

void
output_ps(hwloc_topology_t topology, const char *filename, int logical, int verbose_mode __hwloc_attribute_unused)
{
  FILE *output = open_file(filename, "w");
  cairo_surface_t *cs;

  if (!output) {
    fprintf(stderr, "Failed to open %s for writing (%s)\n", filename, strerror(errno));
    return;
  }

  cs = output_draw_start(&ps_draw_methods, logical, topology, output);

  topo_cairo_paint(&ps_draw_methods, logical, topology, cs);
  cairo_surface_flush(cs);
  cairo_surface_destroy(cs);
  fclose(output);
}
#endif /* CAIRO_HAS_PS_SURFACE */


#if CAIRO_HAS_SVG_SURFACE
/* SVG back-end */
static void *
svg_start(void *output, int width, int height)
{
  return cairo_svg_surface_create_for_stream(topo_cairo_write, output, width, height);
}

static struct draw_methods svg_draw_methods = {
  .start = svg_start,
  .declare_color = null_declare_color,
  .box = topo_cairo_box,
  .line = topo_cairo_line,
  .text = topo_cairo_text,
};

void
output_svg(hwloc_topology_t topology, const char *filename, int logical, int verbose_mode __hwloc_attribute_unused)
{
  FILE *output = open_file(filename, "w");
  cairo_surface_t *cs;

  if (!output) {
    fprintf(stderr, "Failed to open %s for writing (%s)\n", filename, strerror(errno));
    return;
  }

  cs = output_draw_start(&svg_draw_methods, logical, topology, output);

  topo_cairo_paint(&svg_draw_methods, logical, topology, cs);
  cairo_surface_flush(cs);
  cairo_surface_destroy(cs);
  fclose(output);
}
#endif /* CAIRO_HAS_SVG_SURFACE */

#endif /* CAIRO */
