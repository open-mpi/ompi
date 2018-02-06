/*
 * Copyright © 2013 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_GL_X11_XLIB_H
#define HWLOC_PORT_GL_X11_XLIB_H

typedef struct _XDisplay Display;

Display *XOpenDisplay(const char*);
int XQueryExtension(Display*, const char*, int*, int*, int*);
int XCloseDisplay(Display*);
int ScreenCount(Display*);

#endif /* HWLOC_PORT_GL_X11_XLIB_H */
