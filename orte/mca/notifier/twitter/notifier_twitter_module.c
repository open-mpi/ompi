/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2009 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Because we can. :-)
 */

#include "orte_config.h"

#include <errno.h>
#include <string.h>
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif
#include <stdio.h>
#include <unistd.h>

#include "opal/util/show_help.h"

#include "orte/version.h"
#include "orte/constants.h"
#include "orte/mca/ess/ess.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/notifier/base/base.h"

#include "notifier_twitter.h"


/* Static API's */
static void mylog(int severity, int errcode, const char *msg, ...);
static void myhelplog(int severity, int errcode, const char *filename, 
                      const char *topic, ...);
static void mypeerlog(int severity, int errcode, orte_process_name_t *peer_proc,
                      const char *msg, ...);

/* Module */
orte_notifier_base_module_t orte_notifier_twitter_module = {
    NULL,
    NULL,
    mylog,
    myhelplog,
    mypeerlog
};

static char base64_convert(uint8_t i)
{
    if (i < 26) {
        return 'A' + i;
    } else if (i < 52) {
        return 'a' + i - 26;
    } else if (i < 62) {
        return '0' + i - 52;
    } else if (62 == i) {
        return '+';
    } else {
        return '/';
    }
}

static void base64_encode(const char *input, char *output, int olen)
{
    int ilen, i, j;
    uint8_t c;
    const char pad = '=';

    /* Simplistic loop for base64 encoding */
    memset(output, 0, olen);
    ilen = (int) strlen(input);
    for (j = i = 0; j + 4 < olen && i < ilen; i += 3) {
        /* First output character is always emitted with no
           complications */
        c = input[i] >> 2;
        output[j++] = base64_convert(c);
        
        /* If we do not have another input character, then the next
           output character is just the last 2 bits of the current
           input character, shifted up 4 bits, and then output 2
           ='s */
        if (i + 1 == ilen) {
            c = (input[i] & 3) << 4;
            output[j++] = base64_convert(c);
            output[j++] = pad;
            output[j++] = pad;
        } else {
            c = ((input[i] & 3) << 4) + (input[i + 1] >> 4);
            output[j++] = base64_convert(c);

            /* If we do not have another input character, then the
               next output character is just the last 4 bits of the
               current input character.  Then output a single =. */
            if (i + 2 == ilen) {
                c = (input[i + 1] & 15) << 2;
                output[j++] = base64_convert(c);
                output[j++] = pad;
            } else {
                c = ((input[i + 1] & 15) << 2) + (input[i + 2] >> 6);
                output[j++] = base64_convert(c);
                
                c = (input[i + 2] & 63);
                output[j++] = base64_convert(c);
            }
        }
    }

    if (j >= olen) {
        output[sizeof(output) - 1] = '\0';
    }
}

static char *auth_basic(void)
{
    char *str;
    static char output[256];

    asprintf(&str, "%s:%s", mca_notifier_twitter_component.username,
             mca_notifier_twitter_component.password);
    memset(output, 0, sizeof(output));
    base64_encode(str, output, sizeof(output));
    free(str);

    return output;
}

static char *version_string(void)
{
    int i;
    static char temp[BUFSIZ];
    mca_base_component_t *c = 
        &(mca_notifier_twitter_component.super.base_version);

    memset(temp, 0, sizeof(temp));
    snprintf(temp, BUFSIZ - 1, "%d.%d", 
             c->mca_component_major_version,
             c->mca_component_minor_version);
    i = strlen(temp);

    if (c->mca_component_release_version > 0) {
        snprintf(temp + i, sizeof(temp) - i, ".%d", 
                 c->mca_component_release_version);
        i = strlen(temp);
    }

    if (strlen(ORTE_GREEK_VERSION) > 0) {
        strncat(temp + i, ORTE_GREEK_VERSION, sizeof(temp) - i);
        i = strlen(temp);
    }

    if (ORTE_WANT_SVN && strlen(ORTE_SVN_R) > 0) {
        strncat(temp + i, ORTE_SVN_R, sizeof(temp) - i);
    }

    return temp;
}

static void tweet(char *msg)
{
    int fd, ret, sent, len;
    char *str, buf[80];
    struct sockaddr_in peer;

    if (NULL == msg) {
        return;
    }

    asprintf(&str, "POST /%s HTTP/1.1\nAuthorization: Basic %s\nUser-Agent: Open MPI %s Twitter notifier\nHost: %s\nAccept: */*\nContent-Length: %d\nContent-Type: application/x-www-form-urlencoded\n\nstatus=%s",
             mca_notifier_twitter_component.uri,
             auth_basic(),
             version_string(),
             mca_notifier_twitter_component.server,
             (int) (strlen(msg) + strlen("status=")),
             msg);

    /* Get a socket */
    fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (fd < 0) {
        orte_show_help("help-orte-notifier-twitter.txt", 
                       "tweet failed",
                       true, "socket() system call failed", errno,
                       msg);
        return;
    }

    /* Connect to the server */
    memset(&peer, 0, sizeof(peer));
    peer.sin_family = AF_INET;
    memcpy((char *) &peer.sin_addr, 
           mca_notifier_twitter_component.server_hostent->h_addr,
           sizeof(peer.sin_addr));
    peer.sin_port = htons(mca_notifier_twitter_component.port);
    ret = connect(fd, (const struct sockaddr *) &peer, sizeof(peer));
    if (ret < 0) {
        orte_show_help("help-orte-notifier-twitter.txt", 
                       "tweet failed",
                       true, "connect() system call failed", errno,
                       msg);
        return;
    }

    /* Send HTTP update */
    for (sent = 0, len = strlen(str); sent < len; ) {
        ret = write(fd, str, strlen(str));
        if (ret > 0) {
            str += ret;
            sent += ret;
        } else if (0 == ret) {
            /* ??? */
            break;
        } else {
            /* If we were interrupted by a signal, no problem */
            if (EINTR == errno) {
                continue;
            }
            break;
        }
    }

    /* Shutdown nicely; send a FIN */
    shutdown(fd, SHUT_WR);
    while (1) {
        ret = read(fd, buf, sizeof(buf));
        if (0 == ret) {
            /* Server ACK'ed the FIN */
            break;
        } else if (ret > 0) {
            /* Print what the server sent us */
            buf[ret] = '\0';
        } else {
            /* If we were interrupted by a signal, no problem */
            if (EINTR == errno) {
                continue;
            }
            /* Otherwise, just bail */
            break;
        }
    }
    close(fd);
}

static void mylog(int severity, int errcode, const char *msg, ...)
{
    char *output;
    va_list arglist;

    /* is the severity value above the threshold - I know
     * this seems backward, but lower severity values are
     * considered "more severe"
     */
    if (severity > orte_notifier_threshold_severity) {
        return;
    }

    /* If there was a message, output it */
    va_start(arglist, msg);
    vasprintf(&output, msg, arglist);
    va_end(arglist);

    if (NULL != output) {
        tweet(output);
        free(output);
    }
}

static void myhelplog(int severity, int errcode, const char *filename, 
                      const char *topic, ...)
{
    va_list arglist;
    char *output;
    
    /* is the severity value above the threshold - I know
     * this seems backward, but lower severity values are
     * considered "more severe"
     */
    if (severity > orte_notifier_threshold_severity) {
        return;
    }

    va_start(arglist, topic);
    output = opal_show_help_vstring(filename, topic, false, arglist);
    va_end(arglist);
    
    if (NULL != output) {
        tweet(output);
        free(output);
    }
}

static void mypeerlog(int severity, int errcode, 
                      orte_process_name_t *peer_proc, const char *msg, ...)
{
    va_list arglist;
    char buf[ORTE_NOTIFIER_MAX_BUF + 1];
    char *peer_host = NULL, *peer_name = NULL;
    char *pos = buf;
    char *errstr = (char*)orte_err2str(errcode);
    int len, space = ORTE_NOTIFIER_MAX_BUF;

    /* is the severity value above the threshold - I know
     * this seems backward, but lower severity values are
     * considered "more severe"
     */
    if (severity > orte_notifier_threshold_severity) {
        return;
    }

    if (peer_proc) {
        peer_host = orte_ess.proc_get_hostname(peer_proc);
        peer_name = ORTE_NAME_PRINT(peer_proc);
    }

    len = snprintf(pos, space,
                   "While communicating to proc %s on node %s,"
                   " proc %s on node %s encountered an error ",
                   peer_name ? peer_name : "UNKNOWN",
                   peer_host ? peer_host : "UNKNOWN",
                   ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                   orte_process_info.nodename);
    space -= len;
    pos += len;
    
    if (0 < space) {
        if (errstr) {
            len = snprintf(pos, space, "'%s':", errstr);
        } else {
            len = snprintf(pos, space, "(%d):", errcode);
        }
        space -= len;
        pos += len;
    }

    if (0 < space) {
        va_start(arglist, msg);
        vsnprintf(pos, space, msg, arglist);
        va_end(arglist);
    }

    buf[ORTE_NOTIFIER_MAX_BUF] = '\0';
    tweet(buf);
}
