/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2017-2021 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "prte_config.h"
#include "constants.h"

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#include <errno.h>
#include <sys/types.h>
#ifdef HAVE_SYS_WAIT_H
#    include <sys/wait.h>
#endif
#include <signal.h>
#ifdef HAVE_UTIL_H
#    include <util.h>
#endif
#ifdef HAVE_PTY_H
#    include <pty.h>
#endif
#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#endif
#ifdef HAVE_TERMIOS_H
#    include <termios.h>
#    ifdef HAVE_TERMIO_H
#        include <termio.h>
#    endif
#endif
#ifdef HAVE_LIBUTIL_H
#    include <libutil.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#    include <sys/ioctl.h>
#endif

#include "src/mca/errmgr/errmgr.h"
#include "src/pmix/pmix-internal.h"
#include "src/runtime/prte_globals.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_basename.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_os_dirpath.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_pty.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/iof/base/base.h"
#include "src/mca/iof/base/iof_base_setup.h"
#include "src/mca/iof/iof.h"

int prte_iof_base_setup_prefork(prte_iof_base_io_conf_t *opts)
{
    int ret = -1;

    fflush(stdout);

    /* first check to make sure we can do ptys */
#if PRTE_ENABLE_PTY_SUPPORT
    if (opts->usepty) {
        struct winsize *wp = NULL;
        /**
         * It has been reported that on MAC OS X 10.4 and prior one cannot
         * safely close the writing side of a pty before completly reading
         * all data inside.
         * There seems to be two issues: first all pending data is
         * discarded, and second it randomly generate kernel panics.
         * Apparently this issue was fixed in 10.5 so by now we use the
         * pty exactly as we use the pipes.
         * This comment is here as a reminder.
         */
#ifdef TIOCGWINSZ
        struct winsize ws;
        if (0 == ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws)) {
            wp = &ws;
        }
#endif
        ret = pmix_openpty(&(opts->p_stdout[0]), &(opts->p_stdout[1]), (char *) NULL,
                           (struct termios *) NULL, wp);
    }
#else
    opts->usepty = 0;
#endif

    if (ret < 0) {
        opts->usepty = 0;
        if (pipe(opts->p_stdout) < 0) {
            PMIX_ERROR_LOG(PMIX_ERR_SYS_LIMITS_PIPES);
            return PMIX_ERR_SYS_LIMITS_PIPES;
        }
    }
    if (opts->connect_stdin) {
        if (pipe(opts->p_stdin) < 0) {
            PMIX_ERROR_LOG(PMIX_ERR_SYS_LIMITS_PIPES);
            return PMIX_ERR_SYS_LIMITS_PIPES;
        }
    }
    if (pipe(opts->p_stderr) < 0) {
        PMIX_ERROR_LOG(PMIX_ERR_SYS_LIMITS_PIPES);
        return PMIX_ERR_SYS_LIMITS_PIPES;
    }
    return PRTE_SUCCESS;
}

int prte_iof_base_setup_child(prte_iof_base_io_conf_t *opts,
                              char ***env)
{
    int ret;
    PRTE_HIDE_UNUSED_PARAMS(env);

    if (opts->connect_stdin) {
        close(opts->p_stdin[1]);
    }
    close(opts->p_stdout[0]);
    close(opts->p_stderr[0]);

    if (opts->usepty) {
        /* disable echo */
        struct termios term_attrs;
        if (tcgetattr(opts->p_stdout[1], &term_attrs) < 0) {
            return PMIX_ERR_PIPE_SETUP_FAILURE;
        }
        term_attrs.c_lflag &= ~(ECHO | ECHOE | ECHOK | ECHOCTL | ECHOKE | ECHONL);
        term_attrs.c_iflag &= ~(ICRNL | INLCR | ISTRIP | INPCK | IXON);
        term_attrs.c_oflag &= ~(
#ifdef OCRNL
            /* OS X 10.3 does not have this
               value defined */
            OCRNL |
#endif
            ONLCR);
        if (tcsetattr(opts->p_stdout[1], TCSANOW, &term_attrs) == -1) {
            return PMIX_ERR_PIPE_SETUP_FAILURE;
        }
#ifdef HAVE_FILENO_UNLOCKED
        ret = dup2(opts->p_stdout[1], fileno_unlocked(stdout));
#else
        ret = dup2(opts->p_stdout[1], fileno(stdout));
#endif
        if (ret < 0) {
            return PMIX_ERR_PIPE_SETUP_FAILURE;
        }
        close(opts->p_stdout[1]);
    } else {
#ifdef HAVE_FILENO_UNLOCKED
        if (opts->p_stdout[1] != fileno_unlocked(stdout)) {
            ret = dup2(opts->p_stdout[1], fileno_unlocked(stdout));
#else
        if (opts->p_stdout[1] != fileno(stdout)) {
            ret = dup2(opts->p_stdout[1], fileno(stdout));
#endif
            if (ret < 0) {
                return PMIX_ERR_PIPE_SETUP_FAILURE;
            }
            close(opts->p_stdout[1]);
        }
    }
    if (opts->connect_stdin) {
#ifdef HAVE_FILENO_UNLOCKED
        if (opts->p_stdin[0] != fileno_unlocked(stdin)) {
            ret = dup2(opts->p_stdin[0], fileno_unlocked(stdin));
#else
        if (opts->p_stdin[0] != fileno(stdin)) {
            ret = dup2(opts->p_stdin[0], fileno(stdin));
#endif
            if (ret < 0) {
                return PMIX_ERR_PIPE_SETUP_FAILURE;
            }
            close(opts->p_stdin[0]);
        }
    } else {
        int fd;

        /* connect input to /dev/null */
        fd = open("/dev/null", O_RDONLY, 0);
        if (fd != fileno(stdin)) {
            dup2(fd, fileno(stdin));
        }
        close(fd);
    }

#ifdef HAVE_FILENO_UNLOCKED
    if (opts->p_stderr[1] != fileno_unlocked(stderr)) {
        ret = dup2(opts->p_stderr[1], fileno_unlocked(stderr));
#else
    if (opts->p_stderr[1] != fileno(stderr)) {
        ret = dup2(opts->p_stderr[1], fileno(stderr));
#endif
        if (ret < 0) {
            return PMIX_ERR_PIPE_SETUP_FAILURE;
        }
        close(opts->p_stderr[1]);
    }

    return PRTE_SUCCESS;
}

int prte_iof_base_setup_parent(const pmix_proc_t *name,
                               prte_iof_base_io_conf_t *opts)
{
    int ret;

    /* connect stdin endpoint */
    if (opts->connect_stdin) {
        /* and connect the pty to stdin */
        ret = prte_iof.pull(name, PRTE_IOF_STDIN, opts->p_stdin[1]);
        if (PRTE_SUCCESS != ret) {
            PRTE_ERROR_LOG(ret);
            return ret;
        }
    }

    /* connect read ends to IOF */
    ret = prte_iof.push(name, PRTE_IOF_STDOUT, opts->p_stdout[0]);
    if (PRTE_SUCCESS != ret) {
        PRTE_ERROR_LOG(ret);
        return ret;
    }

    ret = prte_iof.push(name, PRTE_IOF_STDERR, opts->p_stderr[0]);
    if (PRTE_SUCCESS != ret) {
        PRTE_ERROR_LOG(ret);
        return ret;
    }

    return PRTE_SUCCESS;
}
