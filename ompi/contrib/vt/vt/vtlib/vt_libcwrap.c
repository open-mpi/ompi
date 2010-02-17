/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2010, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "config.h"

#include "vt_env.h"
#include "vt_error.h"
#include "vt_fork.h"
#include "vt_inttypes.h"
#include "vt_iowrap.h"
#include "vt_libcwrap.h"
#include "vt_memhook.h"
#include "vt_pform.h"
#include "vt_trc.h"

#include <errno.h>
#include <dlfcn.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#if (defined(HAVE_WAIT) && HAVE_WAIT) || (defined(HAVE_WAITPID) && HAVE_WAITPID)
# include <sys/types.h>
# include <sys/wait.h>
# ifndef __WAIT_STATUS_DEFN
#   define WAIT_STATUS_TYPE int*
# else
#   define WAIT_STATUS_TYPE __WAIT_STATUS
# endif /* __WAIT_STATUS_DEFN */
#endif /* HAVE_WAIT || HAVE_WAITPID */

#define execl_FUNCIDX   0
#define execl_FUNCDEF   (int (*)(const char*, const char*, ...))
#define execle_FUNCIDX  1
#define execle_FUNCDEF  (int (*)(const char*, const char*, ...))
#define execlp_FUNCIDX  2
#define execlp_FUNCDEF  (int (*)(const char*, const char*, ...))
#define execv_FUNCIDX   3
#define execv_FUNCDEF   (int (*)(const char*, char* const[]))
#define execve_FUNCIDX  4
#define execve_FUNCDEF  (int (*)(const char*, char* const[], char* const[]))
#define execvp_FUNCIDX  5
#define execvp_FUNCDEF  (int (*)(const char*, char* const[]))
#define fork_FUNCIDX    6
#define fork_FUNCDEF    (pid_t (*)(void))
#define system_FUNCIDX  7
#define system_FUNCDEF  (int (*)(const char*))
#define wait_FUNCIDX    8
#define wait_FUNCDEF    (pid_t (*)(WAIT_STATUS_TYPE status))
#define waitpid_FUNCIDX 9
#define waitpid_FUNCDEF (pid_t (*)(pid_t, int*, int))
#define LIBC_FUNC_NUM  10

#define FUNCIDX(fname) fname ## _FUNCIDX
#define FUNCDEF(fname) fname ## _FUNCDEF

/* macro: defines function */
#define INIT_FUNC(fname)                                             \
  libc_funcs[FUNCIDX(fname)].traceme = 1;                            \
  libc_funcs[FUNCIDX(fname)].rid =                                   \
    vt_def_region(#fname, libc_fid, VT_NO_LNO, VT_NO_LNO, NULL,      \
                  VT_LIBC);  \
  GET_REAL_FUNC(fname);

/* macro: gets (real) function pointer */
#define GET_REAL_FUNC(fname)                                         \
  if( !libc_funcs[FUNCIDX(fname)].fptr.p ) {                         \
    get_libc_handle();                                               \
    (void)dlerror(); /* clear any existing error */                  \
    libc_funcs[FUNCIDX(fname)].fptr.p = dlsym(libc_handle, #fname);  \
    if( !libc_funcs[FUNCIDX(fname)].fptr.p ) {                       \
      printf("VampirTrace: FATAL: dlsym() error for symbol %s: %s\n",\
             #fname, dlerror());                                     \
      exit(EXIT_FAILURE);                                            \
    }                                                                \
  }

/* macro: calls (real) function */
#define CALL_FUNC(fname, fret, fargs)                                \
  GET_REAL_FUNC(fname);                                              \
  fret = (FUNCDEF(fname)(libc_funcs[FUNCIDX(fname)].fptr.f))fargs

/* macro: checks if we shall trace right now 
 * Tracing can be disabled globally via vt_libc_tracing_enabled = 0
 * or for each function separately via libc_funcs[IDX].traceme = 0 */
#define DO_TRACE(fname)                                              \
  (vt_libc_tracing_enabled && libc_funcs[FUNCIDX(fname)].traceme)

struct libc_func {
  uint8_t traceme;
  uint32_t rid;

/* The following is necessary to avoid "warning: ISO C forbids conversion of
 * object pointer to function pointer type". If the function calls break on
 * some platform, the cause would most possibly lie here.
 * Then sizeof(void *) != sizeof(<function pointer>)
 */
  union {
    void *p;
    void (*f)(void);
  } fptr;
};

/* flag for enabling and disabling tracing */
int vt_libc_tracing_enabled = 0;
int vt_libc_tracing_state = 0;

static uint32_t libc_fid = VT_NO_ID;
static struct libc_func libc_funcs[LIBC_FUNC_NUM];
static void *libc_handle = NULL;

static void get_libc_handle(void)
{
  if( !libc_handle ) {
#ifndef DEFAULT_LIBC_PATHNAME
#error VampirTrace is not properly configured, DEFAULT_LIBC_PATHNAME is not set! Please report this incident to vampirsupport@zih.tu-dresden.de
#endif
    const char *libc_pathname = DEFAULT_LIBC_PATHNAME;

    (void)dlerror();
    libc_handle = dlopen( libc_pathname, RTLD_LAZY | RTLD_LOCAL );
    if( !libc_handle ) {
      printf("VampirTrace: FATAL: dlopen(\"%s\") error: %s\n", libc_pathname, dlerror());
      exit(EXIT_FAILURE);
    }
  }
}

void vt_libcwrap_init(void)
{
  /* get file-id for LIBC functions */
  libc_fid = vt_def_scl_file("LIBC");

#if (!defined (VT_MPI) && !defined (VT_MT) && !defined(VT_HYB) && !defined(VT_JAVA))

#if defined(HAVE_EXECL) && HAVE_EXECL
  INIT_FUNC(execl);
#endif /* HAVE_EXECL */

#if defined(HAVE_EXECLE) && HAVE_EXECLE
  INIT_FUNC(execle);
#endif /* HAVE_EXECLE */

#if defined(HAVE_EXECLP) && HAVE_EXECLP
  INIT_FUNC(execlp);
#endif /* HAVE_EXECLP */

#if defined(HAVE_EXECV) && HAVE_EXECV
  INIT_FUNC(execv);
#endif /* HAVE_EXECV */

#if defined(HAVE_EXECVE) && HAVE_EXECVE
  INIT_FUNC(execve);
#endif /* HAVE_EXECVE */

#if defined(HAVE_EXECVP) && HAVE_EXECVP
  INIT_FUNC(execvp);
#endif /* HAVE_EXECVP */

#if defined(HAVE_FORK) && HAVE_FORK
  INIT_FUNC(fork);
#endif /* HAVE_FORK */

#endif /* !VT_MPI && !VT_MT && !VT_HYB && !VT_JAVA */

#if defined(HAVE_SYSTEM) && HAVE_SYSTEM
  INIT_FUNC(system);
#endif /* HAVE_SYSTEM */

#if defined(HAVE_WAIT) && HAVE_WAIT
  INIT_FUNC(wait);
#endif /* HAVE_WAIT */

#if defined(HAVE_WAITPID) && HAVE_WAITPID
  INIT_FUNC(waitpid);
#endif /* HAVE_WAITPID */
}

void vt_libcwrap_finalize(void)
{
  if( libc_handle )
    dlclose( libc_handle );
}


#if (!defined (VT_MPI) && !defined (VT_MT) && !defined(VT_HYB) && !defined(VT_JAVA))

/* -- execl -- */

#if defined(HAVE_EXECL) && HAVE_EXECL
int execl(const char* path, const char* arg, ...)
{
  int rc;
  char* argv[100];
  char* tmp;
  int i;
  va_list ap;

  VT_MEMHOOKS_OFF();

  va_start(ap, arg);

  i = 0;
  argv[i++] = (char*)arg;
  while((tmp = va_arg(ap, char*) ))
    argv[i++] = tmp;
  argv[i] = NULL;

  va_end(ap);

  if ( DO_TRACE(execl) )
  {
    /* mark enter function */
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, libc_funcs[FUNCIDX(execl)].rid);
  }

  /* close VT for current process */
  vt_close();

  /* call (real) function */
  CALL_FUNC(execv, rc, (path, argv));

  vt_warning("execl failed: %s", strerror(errno));
  return rc;
}
#endif /* HAVE_EXECL */

/* -- execle -- */

#if defined(HAVE_EXECLE) && HAVE_EXECLE
int execle(const char* path, const char* arg, ...)
{
  int rc;
  char* argv[100];
  char** envp;
  char* tmp;
  int i;
  va_list ap;

  VT_MEMHOOKS_OFF();

  va_start(ap, arg);

  i = 0;
  argv[i++] = (char*)arg;
  while((tmp = va_arg(ap, char*) ))
    argv[i++] = tmp;
  argv[i] = NULL;
  envp = va_arg(ap, char**);

  va_end(ap);

  if ( DO_TRACE(execle) )
  {
    /* mark enter function */
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, libc_funcs[FUNCIDX(execle)].rid);
  }

  /* close VT for current process */
  vt_close();

  /* call (real) function */
  CALL_FUNC(execve, rc, (path, argv, envp));

  vt_warning("execle failed: %s", strerror(errno));
  return rc;
}
#endif /* HAVE_EXECLE */

/* -- execlp -- */

#if defined(HAVE_EXECLP) && HAVE_EXECLP
int execlp(const char* file, const char* arg, ...)
{
  int rc;
  char* argv[100];
  char* tmp;
  int i;
  va_list ap;

  VT_MEMHOOKS_OFF();

  va_start(ap, arg);

  i = 0;
  argv[i++] = (char*)arg;
  while((tmp = va_arg(ap, char*) ))
    argv[i++] = tmp;
  argv[i] = NULL;

  va_end(ap);

  if ( DO_TRACE(execlp) )
  {
    /* mark enter function */
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, libc_funcs[FUNCIDX(execlp)].rid);
  }

  /* close VT for current process */
  vt_close();

  /* call (real) function */
  CALL_FUNC(execvp, rc, (file, argv));

  vt_warning("execlp failed: %s", strerror(errno));
  return rc;
}
#endif /* HAVE_EXECLP */

/* -- execv -- */

#if defined(HAVE_EXECV) && HAVE_EXECV
int execv(const char* path, char* const argv[])
{
  int rc;

  VT_MEMHOOKS_OFF();

  if ( DO_TRACE(execv) )
  {
    /* mark enter function */
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, libc_funcs[FUNCIDX(execv)].rid);
  }

  /* close VT for current process */
  vt_close();

  /* call (real) function */
  CALL_FUNC(execv, rc, (path, argv));

  vt_warning("execv failed: %s", strerror(errno));
  return rc;
}
#endif /* HAVE_EXECV */

/* -- execve -- */

#if defined(HAVE_EXECVE) && HAVE_EXECVE
int execve(const char* file, char* const argv[], char* const envp[])
{
  int rc;

  VT_MEMHOOKS_OFF();

  if ( DO_TRACE(execve) )
  {
    /* mark enter function */
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, libc_funcs[FUNCIDX(execve)].rid);
  }

  /* close VT for current process */
  vt_close();

  /* call (real) function */
  CALL_FUNC(execve, rc, (file, argv, envp));

  vt_warning("execve failed: %s", strerror(errno));
  return rc;
}
#endif /* HAVE_EXECVE */

/* -- execvp -- */

#if defined(HAVE_EXECVP) && HAVE_EXECVP
int execvp(const char* path, char* const argv[])
{
  int rc;

  VT_MEMHOOKS_OFF();

  if ( DO_TRACE(execvp) )
  {
    /* mark enter function */
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, libc_funcs[FUNCIDX(execvp)].rid);
  }

  /* close VT for current process */
  vt_close();

  /* call (real) function */
  CALL_FUNC(execvp, rc, (path, argv));

  vt_warning("execvp failed: %s", strerror(errno));
  return rc;
}
#endif /* HAVE_EXECVP */

/* -- fork -- */

#if defined(HAVE_FORK) && HAVE_FORK

pid_t fork(void)
{
  pid_t rc;
  uint64_t time;

  VT_MEMHOOKS_OFF();

  if ( DO_TRACE(fork) )
  {
    /* mark enter function */
    time = vt_pform_wtime();
    vt_enter(&time, libc_funcs[FUNCIDX(fork)].rid);
  }

  /* call (real) function */
  CALL_FUNC(fork, rc, ());

  if ( DO_TRACE(fork) )
  {
    /* handle fork, if succeeded */
    if ( rc != -1 )
      vt_fork(rc);

    if ( rc != 0 )
    {
      /* mark leave function */
      time = vt_pform_wtime();
      vt_exit(&time);
    }
  }
    
  VT_MEMHOOKS_ON();

  return rc;
}
#endif /* HAVE_FORK */

#endif /* !VT_MPI && !VT_MT && !VT_HYB && !VT_JAVA */

/* -- system -- */

#if defined(HAVE_SYSTEM) && HAVE_SYSTEM
int system(const char* string)
{
  int rc;
  uint64_t time;

  VT_MEMHOOKS_OFF();

  if ( DO_TRACE(system) )
  {
    /* mark enter function */
    time = vt_pform_wtime();
    vt_enter(&time, libc_funcs[FUNCIDX(system)].rid);
  }

  /* call (real) function */
  CALL_FUNC(system, rc, (string));

  if ( DO_TRACE(system) )
  {
    /* mark leave function */
    time = vt_pform_wtime();
    vt_exit(&time);
  }

  VT_MEMHOOKS_ON();

  return rc;
}
#endif /* HAVE_SYSTEM */

/* -- wait -- */

#if defined(HAVE_WAIT) && HAVE_WAIT
pid_t wait(WAIT_STATUS_TYPE status)
{
  pid_t rc;
  uint64_t time;

  VT_MEMHOOKS_OFF();

  if ( DO_TRACE(wait) )
  {
    /* mark enter function */
    time = vt_pform_wtime();
    vt_enter(&time, libc_funcs[FUNCIDX(wait)].rid);
  }

  /* call (real) function */
  CALL_FUNC(wait, rc, (status));

  if ( DO_TRACE(wait) )
  {
    /* mark leave function */
    time = vt_pform_wtime();
    vt_exit(&time);
  }

  VT_MEMHOOKS_ON();

  return rc;
}
#endif /* HAVE_WAIT */

/* -- waitpid -- */

#if defined(HAVE_WAITPID) && HAVE_WAITPID
pid_t waitpid(pid_t pid, int* status, int options)
{
  pid_t rc;
  uint64_t time;

  VT_MEMHOOKS_OFF();

  if ( DO_TRACE(waitpid) )
  {
    /* mark enter function */
    time = vt_pform_wtime();
    vt_enter(&time, libc_funcs[FUNCIDX(waitpid)].rid);
  }

  /* call (real) function */
  CALL_FUNC(waitpid, rc, (pid, status, options));

  if ( DO_TRACE(waitpid) )
  {
    /* mark leave function */
    time = vt_pform_wtime();
    vt_exit(&time);
  }

  VT_MEMHOOKS_ON();

  return rc;
}
#endif /* HAVE_WAITPID */
