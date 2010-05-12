/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include <windows.h>
#include <windowsx.h>

#include "lstopo.h"

/* windows back-end.  */

static struct color {
  int r, g, b;
  HGDIOBJ brush;
} *colors;

static int numcolors;

static HGDIOBJ
rgb_to_brush(int r, int g, int b)
{
  int i;

  for (i = 0; i < numcolors; i++)
    if (colors[i].r == r && colors[i].g == g && colors[i].b == b)
      return colors[i].brush;

  fprintf(stderr, "color #%02x%02x%02x not declared\n", r, g, b);
  exit(EXIT_FAILURE);
}

struct draw_methods windows_draw_methods;

static hwloc_topology_t the_topology;
static int the_logical;
static int state, control;
static int x, y, x_delta, y_delta;
static int finish;
static int the_width, the_height;
static int win_width, win_height;

static LRESULT CALLBACK
WndProc(HWND hwnd, UINT message, WPARAM wparam, LPARAM lparam)
{
  int redraw = 0;
  switch (message) {
    case WM_PAINT: {
      HDC hdc;
      PAINTSTRUCT ps;
      hdc = BeginPaint(hwnd, &ps);
      output_draw(&windows_draw_methods, the_logical, the_topology, &ps);
      EndPaint(hwnd, &ps);
      break;
    }
    case WM_LBUTTONDOWN: 
      state = 1;
      x = GET_X_LPARAM(lparam);
      y = GET_Y_LPARAM(lparam);
      break;
    case WM_LBUTTONUP: 
      state = 0;
      break;
    case WM_MOUSEMOVE:
      if (!(wparam & MK_LBUTTON))
        state = 0;
      if (state) {
        int new_x = GET_X_LPARAM(lparam);
        int new_y = GET_Y_LPARAM(lparam);
        x_delta -= new_x - x;
        y_delta -= new_y - y;
        x = new_x;
        y = new_y;
        redraw = 1;
      }
      break;
    case WM_KEYDOWN:
      switch (wparam) {
      case 'q':
      case 'Q':
      case VK_ESCAPE:
        finish = 1;
        break;
      case VK_LEFT:
        x_delta -= win_width/10;
        redraw = 1;
        break;
      case VK_RIGHT:
        x_delta += win_width/10;
        redraw = 1;
        break;
      case VK_UP:
        y_delta -= win_height/10;
        redraw = 1;
        break;
      case VK_DOWN:
        y_delta += win_height/10;
        redraw = 1;
        break;
      case VK_PRIOR:
        if (control) {
          x_delta -= win_width;
          redraw = 1;
        } else {
          y_delta -= win_height;
          redraw = 1;
        }
        break;
      case VK_NEXT:
        if (control) {
          x_delta += win_width;
          redraw = 1;
        } else {
          y_delta += win_height;
          redraw = 1;
        }
        break;
      case VK_HOME:
        x_delta = 0;
        y_delta = 0;
        redraw = 1;
        break;
      case VK_END:
        x_delta = INT_MAX;
        y_delta = INT_MAX;
        redraw = 1;
        break;
      case VK_CONTROL:
        control = 1;
        break;
      }
      break;
    case WM_KEYUP:
      switch (wparam) {
      case VK_CONTROL:
        control = 0;
        break;
      }
      break;
    case WM_DESTROY:
      PostQuitMessage(0);
      return 0;
    case WM_SIZE:
      win_width = LOWORD(lparam);
      win_height = HIWORD(lparam);
      redraw = 1;
      break;
  }
  if (redraw) {
    if (x_delta > the_width - win_width)
      x_delta = the_width - win_width;
    if (y_delta > the_height - win_height)
      y_delta = the_height - win_height;
    if (x_delta < 0)
      x_delta = 0;
    if (y_delta < 0)
      y_delta = 0;
    RedrawWindow(hwnd, NULL, NULL, RDW_INVALIDATE);
  }
  return DefWindowProc(hwnd, message, wparam, lparam);
}

static void *
windows_start(void *output_ __hwloc_attribute_unused, int width, int height)
{
  WNDCLASS wndclass = {
    .hbrBackground = (HBRUSH) GetStockObject(WHITE_BRUSH),
    .hCursor = LoadCursor(NULL, IDC_SIZEALL),
    .hIcon = LoadIcon(NULL, IDI_APPLICATION),
    .lpfnWndProc = WndProc,
    .lpszClassName = "lstopo",
  };
  HWND toplevel;

  win_width = width + 2*GetSystemMetrics(SM_CXSIZEFRAME);
  win_height = height + 2*GetSystemMetrics(SM_CYSIZEFRAME) + GetSystemMetrics(SM_CYCAPTION);

  if (win_width > GetSystemMetrics(SM_CXFULLSCREEN))
    win_width = GetSystemMetrics(SM_CXFULLSCREEN);

  if (win_height > GetSystemMetrics(SM_CYFULLSCREEN))
    win_height = GetSystemMetrics(SM_CYFULLSCREEN);

  RegisterClass(&wndclass);
  toplevel = CreateWindow("lstopo", "lstopo", WS_OVERLAPPEDWINDOW,
		  CW_USEDEFAULT, CW_USEDEFAULT,
		  win_width, win_height, NULL, NULL, NULL, NULL);

  the_width = width;
  the_height = height;

  ShowWindow(toplevel, SW_SHOWDEFAULT);

  return toplevel;
}

static void
windows_declare_color(void *output_ __hwloc_attribute_unused, int r, int g, int b)
{
  HBRUSH brush;
  COLORREF color = RGB(r, g, b);

  brush = CreateSolidBrush(color);
  if (!brush) {
    fprintf(stderr,"Could not allocate color %02x%02x%02x\n", r, g, b);
    exit(EXIT_FAILURE);
  }

  colors = realloc(colors, sizeof(*colors) * (numcolors + 1));
  colors[numcolors].r = r;
  colors[numcolors].g = g;
  colors[numcolors].b = b;
  colors[numcolors].brush = (HGDIOBJ) brush;
  numcolors++;
}

static void
windows_box(void *output, int r, int g, int b, unsigned depth __hwloc_attribute_unused, unsigned x, unsigned width, unsigned y, unsigned height)
{
  PAINTSTRUCT *ps = output;
  SelectObject(ps->hdc, rgb_to_brush(r, g, b));
  SetBkColor(ps->hdc, RGB(r, g, b));
  Rectangle(ps->hdc, x - x_delta, y - y_delta, x + width - x_delta, y + height - y_delta);
}

static void
windows_line(void *output, int r, int g, int b, unsigned depth __hwloc_attribute_unused, unsigned x1, unsigned y1, unsigned x2, unsigned y2)
{
  PAINTSTRUCT *ps = output;
  SelectObject(ps->hdc, rgb_to_brush(r, g, b));
  MoveToEx(ps->hdc, x1 - x_delta, y1 - y_delta, NULL);
  LineTo(ps->hdc, x2 - x_delta, y2 - y_delta);
}

static void
windows_text(void *output, int r, int g, int b, int size, unsigned depth __hwloc_attribute_unused, unsigned x, unsigned y, const char *text)
{
  PAINTSTRUCT *ps = output;
  HFONT font;
  SetTextColor(ps->hdc, RGB(r, g, b));
  font = CreateFont(size, 0, 0, 0, 0, FALSE, FALSE, FALSE, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH, NULL);
  SelectObject(ps->hdc, (HGDIOBJ) font);
  TextOut(ps->hdc, x - x_delta, y - y_delta, text, strlen(text));
  DeleteObject(font);
}

struct draw_methods windows_draw_methods = {
  .start = windows_start,
  .declare_color = windows_declare_color,
  .box = windows_box,
  .line = windows_line,
  .text = windows_text,
};

void
output_windows (hwloc_topology_t topology, const char *filename __hwloc_attribute_unused, int logical, int verbose_mode __hwloc_attribute_unused)
{
  HWND toplevel;
  the_topology = topology;
  the_logical = logical;
  toplevel = output_draw_start(&windows_draw_methods, logical, topology, NULL);
  UpdateWindow(toplevel);
  MSG msg;
  while (!finish && GetMessage(&msg, NULL, 0, 0)) {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
  }
}
