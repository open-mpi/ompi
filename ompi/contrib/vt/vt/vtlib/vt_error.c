/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich GmbH, Federal
 * Republic of Germany
 *
 * See the file COPYRIGHT in the package base directory for details
 **/

#include "vt_error.h"
#include "vt_env.h"
#include "vt_iowrap.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#define VT_MSG_PFIX  "VampirTrace"
#define VT_MSG_SIZE  4096

static int vt_pid = -1;

void vt_error_pid(const int pid)
{
  vt_pid = pid;
  return;
}

static void vt_print_msg(const char* fmt, va_list az)
{
  char buffer[VT_MSG_SIZE];

  if (vt_pid != -1) sprintf(buffer, "[%d]", vt_pid); else buffer[0]=0;
  sprintf(buffer + strlen(buffer), "%s: ", VT_MSG_PFIX);
  vsprintf(buffer + strlen(buffer), fmt, az);
  VT_SUSPEND_IO_TRACING();
  fprintf(stderr, "%s\n", buffer);
  fflush(NULL);
  VT_RESUME_IO_TRACING();
  return;
}

void vt_error_impl(const char* f, int l)
{
  char buffer[VT_MSG_SIZE];
  sprintf(buffer, "%s [%s:%d]", VT_MSG_PFIX, f, l);

  VT_SUSPEND_IO_TRACING();
  perror(buffer);
  fflush(stderr);
  exit(EXIT_FAILURE);
}

void vt_error_msg(const char* fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  vt_print_msg(fmt, ap);
  exit(EXIT_FAILURE);
}

void vt_warning(const char* fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  vt_print_msg(fmt, ap);
  return;
}

void vt_cntl_msg(const char* fmt, ...)
{
  va_list ap;

  if (vt_env_is_verbose())
    {
      va_start(ap, fmt);
      vt_print_msg(fmt, ap);
      return;
    }
}

inline void vt_debug_msg(int level, const char* fmt, ...)
{
  va_list ap;
#if defined(VT_DEBUG) && (VT_DEBUG > 0)
  if( level <= VT_DEBUG ) {
    va_start(ap, fmt);
    vt_print_msg(fmt, ap);
  }
#else
  va_start(ap, fmt); /* only for avoiding a compiler warning */
#endif
}







