/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "include/constants.h"

#ifdef HAVE_EXECINFO_H
#include <execinfo.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#include "util/stacktrace.h"
#include "mca/base/mca_base_param.h"

#ifndef _NSIG
#define _NSIG 32
#endif

/**
 * This function is being called as a signal-handler in response
 * to a user-specified signal (e.g. SIGFPE or SIGSEGV).
 * For Linux/Glibc, it then uses backtrace and backtrace_symbols
 * to figure the current stack and then prints that out to stdout.
 * Yes, printf and malloc are not signal-safe per se, but should be 
 * on Linux?
 *
 *  @param signo with the signal number raised 
 *  @param info with information regarding the reason/send of the signal
 *  @param p 
 *
 */
#ifndef WIN32
static void ompi_show_stackframe (int signo, siginfo_t * info, void * p)
{   
#ifdef __GLIBC__
    int i;
    int trace_size;
    void * trace[32];
    char ** messages = (char **)NULL;
#endif
    char print_buffer[1024];
    char * tmp = print_buffer;
    int size = sizeof (print_buffer);
    int ret;
    char * str;

    /*
     * Yes, we are doing printf inside a signal-handler.
     * However, backtrace itself calls malloc (which may not be signal-safe,
     * under linux, printf and malloc are)
     *
     * We could use backtrace_symbols_fd and write directly into an
     * filedescriptor, however, without formatting -- also this fd 
     * should be opened in a sensible way...
     */
    memset (print_buffer, 0, sizeof (print_buffer));

    switch (signo)
    {
      case SIGILL:
        switch (info->si_code)
          {
            case ILL_ILLOPC: str = "ILL_ILLOPC"; break;
            case ILL_ILLOPN: str = "ILL_ILLOPN"; break;
            case ILL_ILLADR: str = "ILL_ILLADR"; break;
            case ILL_ILLTRP: str = "ILL_ILLTRP"; break;
            case ILL_PRVOPC: str = "ILL_PRVOPC"; break;
            case ILL_PRVREG: str = "ILL_PRVREG"; break;
            case ILL_COPROC: str = "ILL_COPROC"; break;
            case ILL_BADSTK: str = "ILL_BADSTK"; break;
          }
        break;
      case SIGFPE:
        switch (info->si_code)
          {
            case FPE_INTDIV: str = "FPE_INTDIV"; break;
            case FPE_INTOVF: str = "FPE_INTOVF"; break;
            case FPE_FLTDIV: str = "FPE_FLTDIV"; break;
            case FPE_FLTOVF: str = "FPE_FLTOVF"; break;
            case FPE_FLTUND: str = "FPE_FLTUND"; break;
            case FPE_FLTRES: str = "FPE_FLTRES"; break;
            case FPE_FLTINV: str = "FPE_FLTINV"; break;
            case FPE_FLTSUB: str = "FPE_FLTSUB"; break;
          }
        break;
      case SIGSEGV:
        switch (info->si_code)
          {
            case SEGV_MAPERR: str = "SEGV_MAPERR"; break;
            case SEGV_ACCERR: str = "SEGV_ACCERR"; break;
          }
        break;
      case SIGBUS:
        switch (info->si_code)
          {
            case BUS_ADRALN: str = "BUS_ADRALN"; break;
            case BUS_ADRERR: str = "BUS_ADRERR"; break;
            case BUS_OBJERR: str = "BUS_OBJERR"; break;
          }
        break;
      case SIGTRAP:
        switch (info->si_code)
          {
            case TRAP_BRKPT: str = "TRAP_BRKPT"; break;
            case TRAP_TRACE: str = "TRAP_TRACE"; break;
          }
        break;
      case SIGCHLD:
        switch (info->si_code)
          {
            case CLD_EXITED: str = "CLD_EXITED"; break;
            case CLD_KILLED: str = "CLD_KILLED"; break;
            case CLD_DUMPED: str = "CLD_DUMPED"; break;
            case CLD_TRAPPED: str = "CLD_TRAPPED"; break;
            case CLD_STOPPED: str = "CLD_STOPPED"; break;
            case CLD_CONTINUED: str = "CLD_CONTINUED"; break;
          }
        break;
      case SIGPOLL:
        switch (info->si_code)
          {
             case POLL_IN: str = "POLL_IN"; break;
             case POLL_OUT: str = "POLL_OUT"; break;
             case POLL_MSG: str = "POLL_MSG"; break;
             case POLL_ERR: str = "POLL_ERR"; break;
             case POLL_PRI: str = "POLL_PRI"; break;
             case POLL_HUP: str = "POLL_HUP"; break;
          }
        break;
      default:
        switch (info->si_code)
         {
            case SI_ASYNCNL: str = "SI_ASYNCNL"; break;
            case SI_SIGIO: str = "SI_SIGIO"; break;
            case SI_ASYNCIO: str = "SI_ASYNCIO"; break;
            case SI_MESGQ: str = "SI_MESGQ"; break;
            case SI_TIMER: str = "SI_TIMER"; break;
            case SI_QUEUE: str = "SI_QUEUE"; break;
            case SI_USER: str = "SI_USER"; break;
            case SI_KERNEL: str = "SI_KERNEL"; break;
          }
    }

    ret = snprintf (tmp, size, "Signal:%d info.si_errno:%d(%s) si_code:%d(%s)\n",
	            signo, info->si_errno, strerror (info->si_errno),
	            info->si_code, str);
    size -= ret;
    tmp += ret;

    switch (signo)
    {
    case SIGILL:
    case SIGFPE: 
    case SIGSEGV: 
    case SIGBUS:
    {
      ret = snprintf (tmp, size, "Failing at addr:%p\n",
		      info->si_addr);
      size -= ret;
      tmp += ret;
      break;
    }
    case SIGCHLD: {
      ret = snprintf (tmp, size, "si_pid:%d si_uid:%d si_status:%d si_utime:%d, si_stime:%d\n",
                      info->si_pid, info->si_uid, info->si_status,
                      info->si_utime, info->si_stime);
      size -= ret;
      tmp += ret;
      break;
    }
    case SIGPOLL: {
      ret = snprintf (tmp, size, "si_band:%ld si_fd:%d\n",
                      info->si_band, info->si_fd);
      size -= ret;
      tmp += ret;
      break;
    }
    }
   
    printf ("%s", print_buffer);

#ifdef __GLIBC__
    trace_size = backtrace (trace, 32);
    messages = backtrace_symbols (trace, trace_size);

    for (i = 0; i < trace_size; i++)
      printf ("[%d] func:%s\n", i, messages[i]);
#endif
}

#endif /* WIN32 */


/**
 * Here we register the ompi_show_stackframe function for signals
 * passed to OpenMPI by the mpi_signal-parameter passed to mpirun
 * by the user.
 *
 *  @returnvalue OMPI_SUCCESS
 *  @returnvalue OMPI_ERR_BAD_PARAM if the value in the signal-list
 *    is not a valid signal-number
 *               
 */
int ompi_util_register_stackhandlers (void)
{
#ifndef WIN32
    struct sigaction act;
    char * string_value;
    char * tmp;
    char * next;
    int param;

    param = mca_base_param_find ("mpi", NULL, "signal");
    mca_base_param_lookup_string (param, &string_value);

    memset(&act, 0, sizeof(act));
    act.sa_sigaction = ompi_show_stackframe;
    act.sa_flags = SA_SIGINFO | SA_ONESHOT;

    for (tmp = next = string_value ; 
	 next != NULL && *next != '\0'; 
	 tmp = next + 1)
    {
      int sig;
      int ret;

      sig = strtol (tmp, &next, 10);
      /*
      printf ("ompi_util_register_stackhandlers: sig:%d tmp:%p next:%p "
              "tmp:[%s] next:[%s] _NSIG:%d\n",
	      sig, tmp, next, tmp, next, _NSIG);
      */

      /*
       *  If there is no sensible number in the string, exit.
       *  Similarly for any number which is not in the signal-number range
       */
      if (((0 == sig) && (tmp == next)) || (0 > sig) || (_NSIG <= sig))
	 return OMPI_ERR_BAD_PARAM;

      if ((next == NULL) || ((*next != ',') && (*next != '\0')))
	 return OMPI_ERR_BAD_PARAM;

      ret = sigaction (sig, &act, NULL);
      if (ret != 0)
        return OMPI_ERR_IN_ERRNO;
    }
#endif /* WIN32 */
    return OMPI_SUCCESS;
}

