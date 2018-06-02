/*
 * Copyright © 2013 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_GL_NVCTRL_NVCTRLLIB_H
#define HWLOC_PORT_GL_NVCTRL_NVCTRLLIB_H

int XNVCTRLIsNvScreen(Display *, int);
int XNVCTRLQueryTargetBinaryData(Display *, int, int, unsigned int, unsigned int, unsigned char **, int *);
int XNVCTRLQueryTargetAttribute(Display *, int, int, unsigned int, unsigned int, int *);
int XNVCTRLQueryTargetStringAttribute(Display *, int, int, unsigned int, unsigned int, char **ptr);

#endif /* HWLOC_PORT_GL_NVCTRL_NVCTRLLIB_H */
