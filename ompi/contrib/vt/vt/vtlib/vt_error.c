/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "config.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "vt_error.h"
#include "vt_env.h"
#include "vt_iowrap.h"

#define VT_MSG_PFIX  "VampirTrace"
#define VT_MSG_SIZE  1024

static int vt_pid = -1;

void vt_error_pid(const int pid)
{
  vt_pid = pid;
  return;
}

static void vt_print_msg(const char* fmt, va_list az)
{
  char buffer[VT_MSG_SIZE];

  if (vt_pid != -1) snprintf(buffer, sizeof(buffer)-1,
			     "[%d]", vt_pid); else buffer[0]=0;
  snprintf(buffer + strlen(buffer), sizeof(buffer)-1, "%s: ", VT_MSG_PFIX);
  vsnprintf(buffer + strlen(buffer), sizeof(buffer)-1, fmt, az);
  VT_SUSPEND_IO_TRACING();
  fprintf(stderr, "%s\n", buffer);
  fflush(NULL);
  VT_RESUME_IO_TRACING();
  return;
}

void vt_error_impl(const char* f, int l)
{
  char buffer[VT_MSG_SIZE];
  snprintf(buffer, sizeof(buffer)-1, "%s [%s:%d]", VT_MSG_PFIX, f, l);

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
  va_end(ap);
  exit(EXIT_FAILURE);
}

void vt_warning(const char* fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  vt_print_msg(fmt, ap);
  va_end(ap);
  return;
}

void vt_cntl_msg(const char* fmt, ...)
{
  va_list ap;

  if (vt_env_is_verbose())
    {
      va_start(ap, fmt);
      vt_print_msg(fmt, ap);
      va_end(ap);
      return;
    }
}

void vt_debug_msg(int level, const char* fmt, ...)
{
  va_list ap;

  if( vt_env_debug() >= level ) {
    va_start(ap, fmt);
    vt_print_msg(fmt, ap);
    va_end(ap);
  }
}







