/* -*- C -*-
 * 
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
 *
 */

#include "ompi_config.h"

#include <errno.h>
#include <unistd.h>

#include "pcm_bproc.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "class/ompi_list.h"


void
mca_pcm_bproc_monitor_cb(pid_t pid, int status, void *data)
{
    printf("pcm: bproc: have callback for pid %d\n", pid);
}


void
mca_pcm_bproc_stdin_handler(int sd, short flags, void *user)
{

}


void
mca_pcm_bproc_stdout_handler(int sd, short flags, void *user)
{
    char buf[1024];
    ssize_t ret;
    size_t len;

    while (1) {
        ret = read(sd, buf, sizeof(buf));
        if (ret < 0 && errno == EAGAIN) break;
        if (ret < 0 && errno == EINTR) continue;
        if (ret < 0) {
            perror("bproc stdout");
            /* BWB - what do I do now? */
            break;
        }
        if (ret == 0) {
            printf("bproc stdout: returned 0\n");
            break;
        }

        len = (size_t) ret;
        while (len > 0) {
            ret = write(1, buf, len);
            if (ret < 0 && errno != EINTR) {
                perror("bproc stdout");
                break;
            }
            len -= (size_t) ret;
        }
    }
}


void
mca_pcm_bproc_stderr_handler(int sd, short flags, void *user)
{
    char buf[1024];
    ssize_t ret;
    size_t len;

    while (1) {
        ret = read(sd, buf, sizeof(buf));
        if (ret < 0 && errno == EAGAIN) break;
        if (ret < 0 && errno == EINTR) continue;
        if (ret < 0) {
            perror("bproc stdout");
            /* BWB - what do I do now? */
            break;
        }
        if (ret == 0) {
            printf("bproc stdout: returned 0\n");
            break;
        }

        len = (size_t) ret;
        while (len > 0) {
            ret = write(2, buf, len);
            if (ret < 0 && errno != EINTR) {
                perror("bproc stdout");
                break;
            }
            len -= (size_t) ret;
        }
    }
}
