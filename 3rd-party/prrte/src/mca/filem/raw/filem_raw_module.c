/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved
 * Copyright (c) 2013-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 *
 */

#include "prte_config.h"
#include "constants.h"

#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif /* HAVE_UNISTD_H */
#ifdef HAVE_DIRENT_H
#    include <dirent.h>
#endif /* HAVE_DIRENT_H */
#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#endif

#include "src/class/pmix_list.h"
#include "src/event/event-internal.h"

#include "src/util/pmix_argv.h"
#include "src/util/pmix_basename.h"
#include "src/util/pmix_os_dirpath.h"
#include "src/util/pmix_os_path.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_path.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/grpcomm/base/base.h"
#include "src/rml/rml.h"
#include "src/mca/state/state.h"
#include "src/runtime/prte_globals.h"
#include "src/threads/pmix_threads.h"
#include "src/util/name_fns.h"
#include "src/util/proc_info.h"
#include "src/util/session_dir.h"

#include "src/mca/filem/base/base.h"
#include "src/mca/filem/filem.h"

#include "filem_raw.h"

static int raw_init(void);
static int raw_finalize(void);
static int raw_preposition_files(prte_job_t *jdata, prte_filem_completion_cbfunc_t cbfunc,
                                 void *cbdata);
static int raw_link_local_files(prte_job_t *jdata, prte_app_context_t *app);

prte_filem_base_module_t prte_filem_raw_module = {.filem_init = raw_init,
                                                  .filem_finalize = raw_finalize,
                                                  /* we don't use any of the following */
                                                  .put = prte_filem_base_none_put,
                                                  .put_nb = prte_filem_base_none_put_nb,
                                                  .get = prte_filem_base_none_get,
                                                  .get_nb = prte_filem_base_none_get_nb,
                                                  .rm = prte_filem_base_none_rm,
                                                  .rm_nb = prte_filem_base_none_rm_nb,
                                                  .wait = prte_filem_base_none_wait,
                                                  .wait_all = prte_filem_base_none_wait_all,
                                                  /* now the APIs we *do* use */
                                                  .preposition_files = raw_preposition_files,
                                                  .link_local_files = raw_link_local_files};

static pmix_list_t outbound_files;
static pmix_list_t incoming_files;
static pmix_list_t positioned_files;

static void send_chunk(int fd, short argc, void *cbdata);
static void recv_files(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                       prte_rml_tag_t tag, void *cbdata);
static void recv_ack(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                     prte_rml_tag_t tag, void *cbdata);
static void write_handler(int fd, short event, void *cbdata);

static int raw_init(void)
{
    PMIX_CONSTRUCT(&incoming_files, pmix_list_t);

    /* start a recv to catch any files sent to me */
    PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_FILEM_BASE,
                  PRTE_RML_PERSISTENT, recv_files, NULL);

    /* if I'm the HNP, start a recv to catch acks sent to me */
    if (PRTE_PROC_IS_MASTER) {
        PMIX_CONSTRUCT(&outbound_files, pmix_list_t);
        PMIX_CONSTRUCT(&positioned_files, pmix_list_t);
        PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_FILEM_BASE_RESP,
                      PRTE_RML_PERSISTENT, recv_ack, NULL);
    }

    return PRTE_SUCCESS;
}

static int raw_finalize(void)
{
    pmix_list_item_t *item;

    while (NULL != (item = pmix_list_remove_first(&incoming_files))) {
        PMIX_RELEASE(item);
    }
    PMIX_DESTRUCT(&incoming_files);

    if (PRTE_PROC_IS_MASTER) {
        while (NULL != (item = pmix_list_remove_first(&outbound_files))) {
            PMIX_RELEASE(item);
        }
        PMIX_DESTRUCT(&outbound_files);
        while (NULL != (item = pmix_list_remove_first(&positioned_files))) {
            PMIX_RELEASE(item);
        }
        PMIX_DESTRUCT(&positioned_files);
    }

    return PRTE_SUCCESS;
}

static void xfer_complete(int status, prte_filem_raw_xfer_t *xfer)
{
    prte_filem_raw_outbound_t *outbound = xfer->outbound;

    /* transfer the status, if not success */
    if (PRTE_SUCCESS != status) {
        outbound->status = status;
    }

    /* this transfer is complete - remove it from list */
    pmix_list_remove_item(&outbound->xfers, &xfer->super);
    /* add it to the list of files that have been positioned */
    pmix_list_append(&positioned_files, &xfer->super);

    /* if the list is now empty, then the xfer is complete */
    if (0 == pmix_list_get_size(&outbound->xfers)) {
        /* do the callback */
        if (NULL != outbound->cbfunc) {
            outbound->cbfunc(outbound->status, outbound->cbdata);
        }
        /* release the object */
        pmix_list_remove_item(&outbound_files, &outbound->super);
        PMIX_RELEASE(outbound);
    }
}

static void recv_ack(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                     prte_rml_tag_t tag, void *cbdata)
{
    pmix_list_item_t *item, *itm;
    prte_filem_raw_outbound_t *outbound;
    prte_filem_raw_xfer_t *xfer;
    char *file;
    int st, n, rc;
    PRTE_HIDE_UNUSED_PARAMS(status, tag, cbdata);

    /* unpack the file */
    n = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &file, &n, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }

    /* unpack the status */
    n = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &st, &n, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }

    PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                         "%s filem:raw: recvd ack from %s for file %s status %d",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(sender), file, st));

    /* find the corresponding outbound object */
    for (item = pmix_list_get_first(&outbound_files); item != pmix_list_get_end(&outbound_files);
         item = pmix_list_get_next(item)) {
        outbound = (prte_filem_raw_outbound_t *) item;
        for (itm = pmix_list_get_first(&outbound->xfers);
             itm != pmix_list_get_end(&outbound->xfers); itm = pmix_list_get_next(itm)) {
            xfer = (prte_filem_raw_xfer_t *) itm;
            if (0 == strcmp(file, xfer->file)) {
                /* if the status isn't success, record it */
                if (0 != st) {
                    xfer->status = st;
                }
                /* track number of respondents */
                xfer->nrecvd++;
                /* if all daemons have responded, then this is complete */
                if (xfer->nrecvd == prte_process_info.num_daemons) {
                    PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                                         "%s filem:raw: xfer complete for file %s status %d",
                                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), file, xfer->status));
                    xfer_complete(xfer->status, xfer);
                }
                free(file);
                return;
            }
        }
    }
}

static int raw_preposition_files(prte_job_t *jdata,
                                 prte_filem_completion_cbfunc_t cbfunc,
                                 void *cbdata)
{
    prte_app_context_t *app;
    pmix_list_item_t *item, *itm, *itm2;
    prte_filem_base_file_set_t *fs;
    int fd;
    prte_filem_raw_xfer_t *xfer, *xptr;
    int flags, i, j;
    char **files = NULL;
    prte_filem_raw_outbound_t *outbound, *optr;
    char *cptr, *nxt, *filestring;
    pmix_list_t fsets;
    bool already_sent;

    PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                         "%s filem:raw: preposition files for job %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(jdata->nspace)));

    /* cycle across the app_contexts looking for files or
     * binaries to be prepositioned
     */
    PMIX_CONSTRUCT(&fsets, pmix_list_t);
    for (i = 0; i < jdata->apps->size; i++) {
        if (NULL == (app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        if (prte_get_attribute(&app->attributes, PRTE_APP_PRELOAD_BIN, NULL, PMIX_BOOL)) {
            /* add the executable to our list */
            PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                                 "%s filem:raw: preload executable %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), app->app));
            fs = PMIX_NEW(prte_filem_base_file_set_t);
            fs->local_target = strdup(app->app);
            fs->target_flag = PRTE_FILEM_TYPE_EXE;
            pmix_list_append(&fsets, &fs->super);
            /* if we are preloading the binary, then the app must be in relative
             * syntax or we won't find it - the binary will be positioned in the
             * session dir, so ensure the app is relative to that location
             */
            cptr = pmix_basename(app->app);
            free(app->app);
            pmix_asprintf(&app->app, "./%s", cptr);
            free(app->argv[0]);
            app->argv[0] = strdup(app->app);
            fs->remote_target = strdup(app->app);
            /* ensure the app uses that location as its cwd */
            prte_set_attribute(&app->attributes, PRTE_APP_SSNDIR_CWD,
                               PRTE_ATTR_GLOBAL, NULL, PMIX_BOOL);
        }
        if (prte_get_attribute(&app->attributes, PRTE_APP_PRELOAD_FILES, (void **) &filestring,
                               PMIX_STRING)) {
            files = PMIX_ARGV_SPLIT_COMPAT(filestring, ',');
            free(filestring);
            for (j = 0; NULL != files[j]; j++) {
                fs = PMIX_NEW(prte_filem_base_file_set_t);
                fs->local_target = strdup(files[j]);
                /* check any suffix for file type */
                if (NULL != (cptr = strchr(files[j], '.'))) {
                    if (0 == strncmp(cptr, ".tar", 4)) {
                        PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                                             "%s filem:raw: marking file %s as TAR",
                                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), files[j]));
                        fs->target_flag = PRTE_FILEM_TYPE_TAR;
                    } else if (0 == strncmp(cptr, ".bz", 3)) {
                        PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                                             "%s filem:raw: marking file %s as BZIP",
                                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), files[j]));
                        fs->target_flag = PRTE_FILEM_TYPE_BZIP;
                    } else if (0 == strncmp(cptr, ".gz", 3)) {
                        PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                                             "%s filem:raw: marking file %s as GZIP",
                                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), files[j]));
                        fs->target_flag = PRTE_FILEM_TYPE_GZIP;
                    } else {
                        fs->target_flag = PRTE_FILEM_TYPE_FILE;
                    }
                } else {
                    fs->target_flag = PRTE_FILEM_TYPE_FILE;
                }
                /* if we are flattening directory trees, then the
                 * remote path is just the basename file name
                 */
                if (prte_filem_raw_flatten_trees) {
                    fs->remote_target = pmix_basename(files[j]);
                } else {
                    /* if this was an absolute path, then we need
                     * to convert it to a relative path - we do not
                     * allow positioning of files to absolute locations
                     * due to the potential for unintentional overwriting
                     * of files
                     */
                    if (pmix_path_is_absolute(files[j])) {
                        fs->remote_target = strdup(&files[j][1]);
                    } else {
                        fs->remote_target = strdup(files[j]);
                    }
                }
                pmix_list_append(&fsets, &fs->super);
                /* prep the filename for matching on the remote
                 * end by stripping any leading '.' directories to avoid
                 * stepping above the session dir location - all
                 * files will be relative to that point. Ensure
                 * we *don't* mistakenly strip the dot from a
                 * filename that starts with one
                 */
                cptr = fs->remote_target;
                nxt = cptr;
                nxt++;
                while ('\0' != *cptr) {
                    if ('.' == *cptr) {
                        /* have to check the next character to
                         * see if it's a dotfile or not
                         */
                        if ('.' == *nxt || '/' == *nxt) {
                            cptr = nxt;
                            nxt++;
                        } else {
                            /* if the next character isn't a dot
                             * or a slash, then this is a dot-file
                             * and we need to leave it alone
                             */
                            break;
                        }
                    } else if ('/' == *cptr) {
                        /* move to the next character */
                        cptr = nxt;
                        nxt++;
                    } else {
                        /* the character isn't a dot or a slash,
                         * so this is the beginning of the filename
                         */
                        break;
                    }
                }
                free(files[j]);
                files[j] = strdup(cptr);
            }
            /* replace the app's file list with the revised one so we
             * can find them on the remote end
             */
            filestring = PMIX_ARGV_JOIN_COMPAT(files, ',');
            prte_set_attribute(&app->attributes, PRTE_APP_PRELOAD_FILES, PRTE_ATTR_GLOBAL,
                               filestring, PMIX_STRING);
            /* cleanup for the next app */
            PMIX_ARGV_FREE_COMPAT(files);
            free(filestring);
        }
    }
    if (0 == pmix_list_get_size(&fsets)) {
        /* nothing to preposition */
        PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                             "%s filem:raw: nothing to preposition",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        if (NULL != cbfunc) {
            cbfunc(PRTE_SUCCESS, cbdata);
        }
        PMIX_DESTRUCT(&fsets);
        return PRTE_SUCCESS;
    }

    PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                         "%s filem:raw: found %d files to position",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), (int) pmix_list_get_size(&fsets)));

    /* track the outbound file sets */
    outbound = PMIX_NEW(prte_filem_raw_outbound_t);
    outbound->cbfunc = cbfunc;
    outbound->cbdata = cbdata;
    pmix_list_append(&outbound_files, &outbound->super);

    /* only the HNP should ever call this function - loop thru the
     * fileset and initiate xcast transfer of each file to every
     * daemon
     */
    while (NULL != (item = pmix_list_remove_first(&fsets))) {
        fs = (prte_filem_base_file_set_t *) item;
        PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                             "%s filem:raw: checking prepositioning of file %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), fs->local_target));

        /* have we already sent this file? */
        already_sent = false;
        for (itm = pmix_list_get_first(&positioned_files);
             !already_sent && itm != pmix_list_get_end(&positioned_files);
             itm = pmix_list_get_next(itm)) {
            xptr = (prte_filem_raw_xfer_t *) itm;
            if (0 == strcmp(fs->local_target, xptr->src)) {
                already_sent = true;
            }
        }
        if (already_sent) {
            /* no need to send it again */
            PMIX_OUTPUT_VERBOSE((3, prte_filem_base_framework.framework_output,
                                 "%s filem:raw: file %s is already in position - ignoring",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), fs->local_target));
            PMIX_RELEASE(item);
            continue;
        }
        /* also have to check if this file is already in the process
         * of being transferred, or was included multiple times
         * for transfer
         */
        for (itm = pmix_list_get_first(&outbound_files);
             !already_sent && itm != pmix_list_get_end(&outbound_files);
             itm = pmix_list_get_next(itm)) {
            optr = (prte_filem_raw_outbound_t *) itm;
            for (itm2 = pmix_list_get_first(&optr->xfers); itm2 != pmix_list_get_end(&optr->xfers);
                 itm2 = pmix_list_get_next(itm2)) {
                xptr = (prte_filem_raw_xfer_t *) itm2;
                if (0 == strcmp(fs->local_target, xptr->src)) {
                    already_sent = true;
                }
            }
        }
        if (already_sent) {
            /* no need to send it again */
            PMIX_OUTPUT_VERBOSE((3, prte_filem_base_framework.framework_output,
                                 "%s filem:raw: file %s is already queued for output - ignoring",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), fs->local_target));
            PMIX_RELEASE(item);
            continue;
        }

        /* attempt to open the specified file */
        if (0 > (fd = open(fs->local_target, O_RDONLY))) {
            pmix_output(0, "%s CANNOT ACCESS FILE %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        fs->local_target);
            PMIX_RELEASE(item);
            pmix_list_remove_item(&outbound_files, &outbound->super);
            PMIX_RELEASE(outbound);
            return PRTE_ERROR;
        }
        /* set the flags to non-blocking */
        if ((flags = fcntl(fd, F_GETFL, 0)) < 0) {
            pmix_output(prte_filem_base_framework.framework_output,
                        "[%s:%d]: fcntl(F_GETFL) failed with errno=%d\n", __FILE__, __LINE__,
                        errno);
        } else {
            flags |= O_NONBLOCK;
            if (fcntl(fd, F_SETFL, flags) < 0) {
                pmix_output(prte_filem_base_framework.framework_output,
                            "[%s:%d]: fcntl(F_GETFL) failed with errno=%d\n", __FILE__, __LINE__,
                            errno);
            }
        }
        PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                             "%s filem:raw: setting up to position file %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), fs->local_target));
        xfer = PMIX_NEW(prte_filem_raw_xfer_t);
        /* save the source so we can avoid duplicate transfers */
        xfer->src = strdup(fs->local_target);
        /* strip any leading '.' directories to avoid
         * stepping above the session dir location - all
         * files will be relative to that point. Ensure
         * we *don't* mistakenly strip the dot from a
         * filename that starts with one
         */
        cptr = fs->remote_target;
        nxt = cptr;
        nxt++;
        while ('\0' != *cptr) {
            if ('.' == *cptr) {
                /* have to check the next character to
                 * see if it's a dotfile or not
                 */
                if ('.' == *nxt || '/' == *nxt) {
                    cptr = nxt;
                    nxt++;
                } else {
                    /* if the next character isn't a dot
                     * or a slash, then this is a dot-file
                     * and we need to leave it alone
                     */
                    break;
                }
            } else if ('/' == *cptr) {
                /* move to the next character */
                cptr = nxt;
                nxt++;
            } else {
                /* the character isn't a dot or a slash,
                 * so this is the beginning of the filename
                 */
                break;
            }
        }
        xfer->fd = fd;
        xfer->file = strdup(cptr);
        xfer->type = fs->target_flag;
        xfer->app_idx = fs->app_idx;
        xfer->outbound = outbound;
        pmix_list_append(&outbound->xfers, &xfer->super);
        PRTE_PMIX_THREADSHIFT(xfer, prte_event_base, send_chunk);
        PMIX_RELEASE(item);
    }
    PMIX_DESTRUCT(&fsets);

    /* check to see if anything remains to be sent - if everything
     * is a duplicate, then the list will be empty
     */
    if (0 == pmix_list_get_size(&outbound->xfers)) {
        PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                             "%s filem:raw: all duplicate files - no positioning reqd",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        pmix_list_remove_item(&outbound_files, &outbound->super);
        PMIX_RELEASE(outbound);
        if (NULL != cbfunc) {
            cbfunc(PRTE_SUCCESS, cbdata);
        }
        return PRTE_SUCCESS;
    }

    if (0 < pmix_output_get_verbosity(prte_filem_base_framework.framework_output)) {
        pmix_output(0, "%s Files to be positioned:", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        for (itm2 = pmix_list_get_first(&outbound->xfers);
             itm2 != pmix_list_get_end(&outbound->xfers); itm2 = pmix_list_get_next(itm2)) {
            xptr = (prte_filem_raw_xfer_t *) itm2;
            pmix_output(0, "%s\t%s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), xptr->src);
        }
    }

    return PRTE_SUCCESS;
}

static int create_link(char *my_dir, char *path, char *link_pt)
{
    char *mypath, *fullname, *basedir;
    struct stat buf;
    int rc = PRTE_SUCCESS;

    /* form the full source path name */
    mypath = pmix_os_path(false, my_dir, link_pt, NULL);
    /* form the full target path name */
    fullname = pmix_os_path(false, path, link_pt, NULL);
    /* there may have been multiple files placed under the
     * same directory, so check for existence first
     */
    if (0 != stat(fullname, &buf)) {
        PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                             "%s filem:raw: creating symlink to %s\n\tmypath: %s\n\tlink: %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), link_pt, mypath, fullname));
        /* create any required path to the link location */
        basedir = pmix_dirname(fullname);
        if (PMIX_SUCCESS != (rc = pmix_os_dirpath_create(basedir, S_IRWXU))) {
            PMIX_ERROR_LOG(rc);
            pmix_output(0, "%s Failed to symlink %s to %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        mypath, fullname);
            free(basedir);
            free(mypath);
            free(fullname);
            rc = prte_pmix_convert_status(rc);
            return rc;
        }
        free(basedir);
        /* do the symlink */
        if (0 != symlink(mypath, fullname)) {
            pmix_output(0, "%s Failed to symlink %s to %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        mypath, fullname);
            rc = PRTE_ERROR;
        }
    }
    free(mypath);
    free(fullname);
    return rc;
}

static int raw_link_local_files(prte_job_t *jdata, prte_app_context_t *app)
{
    char *session_dir, *path = NULL;
    prte_proc_t *proc;
    int i, j, rc;
    prte_filem_raw_incoming_t *inbnd;
    pmix_list_item_t *item;
    char **files = NULL, *bname, *filestring;

    /* check my job's session directory for files I have received and
     * symlink them to the proc-level session directory of each
     * local process in the job
     */
    session_dir = jdata->session_dir;
    if (NULL == session_dir) {
        /* we were unable to find any suitable directory */
        rc = PRTE_ERR_BAD_PARAM;
        PRTE_ERROR_LOG(rc);
        return rc;
    }

    /* get the list of files this app wants */
    if (prte_get_attribute(&app->attributes, PRTE_APP_PRELOAD_FILES, (void **) &filestring,
                           PMIX_STRING)) {
        files = PMIX_ARGV_SPLIT_COMPAT(filestring, ',');
        free(filestring);
    }
    if (prte_get_attribute(&app->attributes, PRTE_APP_PRELOAD_BIN, NULL, PMIX_BOOL)) {
        /* add the app itself to the list */
        bname = pmix_basename(app->app);
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&files, bname);
        free(bname);
    }

    /* if there are no files to link, then ignore this */
    if (NULL == files) {
        return PRTE_SUCCESS;
    }

    for (i = 0; i < prte_local_children->size; i++) {
        if (NULL == (proc = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, i))) {
            continue;
        }
        PMIX_OUTPUT_VERBOSE((10, prte_filem_base_framework.framework_output,
                             "%s filem:raw: working symlinks for proc %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&proc->name)));
        if (!PMIX_CHECK_NSPACE(proc->name.nspace, jdata->nspace)) {
            PMIX_OUTPUT_VERBOSE((10, prte_filem_base_framework.framework_output,
                                 "%s filem:raw: proc %s not part of job %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&proc->name),
                                 PRTE_JOBID_PRINT(jdata->nspace)));
            continue;
        }
        if (proc->app_idx != app->idx) {
            PMIX_OUTPUT_VERBOSE((10, prte_filem_base_framework.framework_output,
                                 "%s filem:raw: proc %s not part of app_idx %d",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&proc->name),
                                 (int) app->idx));
            continue;
        }
        /* ignore children we have already handled */
        if (PRTE_FLAG_TEST(proc, PRTE_PROC_FLAG_ALIVE) ||
            (PRTE_PROC_STATE_INIT != proc->state && PRTE_PROC_STATE_RESTART != proc->state)) {
            continue;
        }

        PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                             "%s filem:raw: creating symlinks for %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&proc->name)));

        /* get the session dir name in absolute form */
        pmix_asprintf(&path, "%s/%s", session_dir, PMIX_RANK_PRINT(proc->name.rank));

        /* cycle thru the incoming files */
        for (item = pmix_list_get_first(&incoming_files);
             item != pmix_list_get_end(&incoming_files); item = pmix_list_get_next(item)) {
            inbnd = (prte_filem_raw_incoming_t *) item;
            PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                                 "%s filem:raw: checking file %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), inbnd->file));

            /* is this file for this app_context? */
            for (j = 0; NULL != files[j]; j++) {
                if (0 == strcmp(inbnd->file, files[j])) {
                    /* this must be one of the files we are to link against */
                    if (NULL != inbnd->link_pts) {
                        PMIX_OUTPUT_VERBOSE((10, prte_filem_base_framework.framework_output,
                                             "%s filem:raw: creating links for file %s",
                                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), inbnd->file));
                        /* cycle thru the link points and create symlinks to them */
                        for (j = 0; NULL != inbnd->link_pts[j]; j++) {
                            if (PRTE_SUCCESS
                                != (rc = create_link(session_dir, path, inbnd->link_pts[j]))) {
                                PRTE_ERROR_LOG(rc);
                                free(files);
                                free(path);
                                return rc;
                            }
                        }
                    } else {
                        PMIX_OUTPUT_VERBOSE((10, prte_filem_base_framework.framework_output,
                                             "%s filem:raw: file %s has no link points",
                                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), inbnd->file));
                    }
                    break;
                }
            }
        }
        free(path);
    }
    PMIX_ARGV_FREE_COMPAT(files);
    return PRTE_SUCCESS;
}

static void send_chunk(int xxx, short argc, void *cbdata)
{
    prte_filem_raw_xfer_t *rev = (prte_filem_raw_xfer_t *) cbdata;
    int fd = rev->fd;
    unsigned char data[PRTE_FILEM_RAW_CHUNK_MAX];
    int32_t numbytes;
    int rc;
    pmix_data_buffer_t chunk;
    prte_grpcomm_signature_t *sig;
    PRTE_HIDE_UNUSED_PARAMS(xxx, argc);

    PMIX_ACQUIRE_OBJECT(rev);

    /* read up to the fragment size */
    numbytes = read(fd, data, sizeof(data));

    if (numbytes < 0) {
        /* either we have a connection error or it was a non-blocking read */

        /* non-blocking, retry */
        if (EAGAIN == errno || EINTR == errno) {
            PMIX_POST_OBJECT(rev);
            prte_event_add(&rev->ev, 0);
            return;
        }

        PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                             "%s filem:raw:read error %s(%d) on file %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                             strerror(errno), errno, rev->file));

        /* Un-recoverable error. Allow the code to flow as usual in order to
         * to send the zero bytes message up the stream, and then close the
         * file descriptor and delete the event.
         */
        numbytes = 0;
    }

    /* if job termination has been ordered, just ignore the
     * data and delete the read event
     */
    if (prte_dvm_abort_ordered) {
        PMIX_RELEASE(rev);
        return;
    }

    PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                         "%s filem:raw:read handler sending chunk %d of %d bytes for file %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), rev->nchunk, numbytes, rev->file));

    /* package it for transmission */
    PMIX_DATA_BUFFER_CONSTRUCT(&chunk);
    rc = PMIx_Data_pack(NULL, &chunk, &rev->file, 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        close(fd);
        PMIX_DATA_BUFFER_DESTRUCT(&chunk);
        return;
    }
    rc = PMIx_Data_pack(NULL, &chunk, &rev->nchunk, 1, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        close(fd);
        PMIX_DATA_BUFFER_DESTRUCT(&chunk);
        return;
    }
    rc = PMIx_Data_pack(NULL, &chunk, data, numbytes, PMIX_BYTE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        close(fd);
        PMIX_DATA_BUFFER_DESTRUCT(&chunk);
        return;
    }
    /* if it is the first chunk, then add file type and index of the app */
    if (0 == rev->nchunk) {
        rc = PMIx_Data_pack(NULL, &chunk, &rev->type, 1, PMIX_INT32);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            close(fd);
            PMIX_DATA_BUFFER_DESTRUCT(&chunk);
            return;
        }
    }

    /* goes to all daemons */
    sig = PMIX_NEW(prte_grpcomm_signature_t);
    sig->signature = (pmix_proc_t *) malloc(sizeof(pmix_proc_t));
    sig->sz = 1;
    PMIX_LOAD_PROCID(&sig->signature[0], PRTE_PROC_MY_NAME->nspace, PMIX_RANK_WILDCARD);
    if (PRTE_SUCCESS != (rc = prte_grpcomm.xcast(sig, PRTE_RML_TAG_FILEM_BASE, &chunk))) {
        PRTE_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_DESTRUCT(&chunk);
        close(fd);
        return;
    }
    PMIX_DATA_BUFFER_DESTRUCT(&chunk);
    PMIX_RELEASE(sig);
    rev->nchunk++;

    /* if num_bytes was zero, then we need to terminate the event
     * and close the file descriptor
     */
    if (0 == numbytes) {
        close(fd);
        return;
    } else {
        /* restart the read event */
        rev->pending = true;
        PMIX_POST_OBJECT(rev);
        prte_event_active(&rev->ev, PRTE_EV_WRITE, 1);
    }
}

static void send_complete(char *file, int status)
{
    pmix_data_buffer_t *buf;
    int rc;

    PMIX_DATA_BUFFER_CREATE(buf);
    rc = PMIx_Data_pack(NULL, buf, &file, 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(buf);
        return;
    }
    rc = PMIx_Data_pack(NULL, buf, &status, 1, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(buf);
        return;
    }
    PRTE_RML_SEND(rc, PRTE_PROC_MY_HNP->rank, buf, PRTE_RML_TAG_FILEM_BASE_RESP);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
        PMIX_RELEASE(buf);
    }
}

/* This is a little tricky as the name of the archive doesn't
 * necessarily have anything to do with the paths inside it -
 * so we have to first query the archive to retrieve that info
 */
static int link_archive(prte_filem_raw_incoming_t *inbnd)
{
    FILE *fp;
    char *cmd;
    char path[MAXPATHLEN];

    PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                         "%s filem:raw: identifying links for archive %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), inbnd->fullpath));

    pmix_asprintf(&cmd, "tar tf %s", inbnd->fullpath);
    fp = popen(cmd, "r");
    free(cmd);
    if (NULL == fp) {
        PRTE_ERROR_LOG(PRTE_ERR_FILE_OPEN_FAILURE);
        return PRTE_ERR_FILE_OPEN_FAILURE;
    }
    /* because app_contexts might share part or all of a
     * directory tree, but link to different files, we
     * have to link to each individual file
     */
    while (fgets(path, sizeof(path), fp) != NULL) {
        PMIX_OUTPUT_VERBOSE((10, prte_filem_base_framework.framework_output,
                             "%s filem:raw: path %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), path));
        /* protect against an empty result */
        if (0 == strlen(path)) {
            continue;
        }
        /* trim the trailing cr */
        path[strlen(path) - 1] = '\0';
        /* ignore directories */
        if ('/' == path[strlen(path) - 1]) {
            PMIX_OUTPUT_VERBOSE((10, prte_filem_base_framework.framework_output,
                                 "%s filem:raw: path %s is a directory - ignoring it",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), path));
            continue;
        }
        /* ignore specific useless directory trees */
        if (NULL != strstr(path, ".deps")) {
            PMIX_OUTPUT_VERBOSE((10, prte_filem_base_framework.framework_output,
                                 "%s filem:raw: path %s includes .deps - ignoring it",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), path));
            continue;
        }
        PMIX_OUTPUT_VERBOSE((10, prte_filem_base_framework.framework_output,
                             "%s filem:raw: adding path %s to link points",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), path));
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&inbnd->link_pts, path);
    }
    /* close */
    pclose(fp);
    return PRTE_SUCCESS;
}

static void recv_files(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                       prte_rml_tag_t tag, void *cbdata)
{
    char *file, *session_dir;
    int32_t nchunk, n, nbytes;
    unsigned char data[PRTE_FILEM_RAW_CHUNK_MAX];
    int rc;
    prte_filem_raw_output_t *output;
    prte_filem_raw_incoming_t *ptr, *incoming;
    pmix_list_item_t *item;
    int32_t type;
    char *cptr;
    PRTE_HIDE_UNUSED_PARAMS(status, sender, tag, cbdata);

    /* unpack the data */
    n = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &file, &n, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        send_complete(NULL, rc);
        return;
    }
    n = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &nchunk, &n, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        send_complete(file, rc);
        free(file);
        return;
    }
    /* if the chunk number is < 0, then this is an EOF message */
    if (nchunk < 0) {
        /* just set nbytes to zero so we close the fd */
        nbytes = 0;
    } else {
        nbytes = PRTE_FILEM_RAW_CHUNK_MAX;
        rc = PMIx_Data_unpack(NULL, buffer, data, &nbytes, PMIX_BYTE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            send_complete(file, rc);
            free(file);
            return;
        }
    }
    /* if the chunk is 0, then additional info should be present */
    if (0 == nchunk) {
        n = 1;
        rc = PMIx_Data_unpack(NULL, buffer, &type, &n, PMIX_INT32);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            send_complete(file, rc);
            free(file);
            return;
        }
    }

    PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                         "%s filem:raw: received chunk %d for file %s containing %d bytes",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), nchunk, file, nbytes));

    /* do we already have this file on our list of incoming? */
    incoming = NULL;
    for (item = pmix_list_get_first(&incoming_files); item != pmix_list_get_end(&incoming_files);
         item = pmix_list_get_next(item)) {
        ptr = (prte_filem_raw_incoming_t *) item;
        if (0 == strcmp(file, ptr->file)) {
            incoming = ptr;
            break;
        }
    }
    if (NULL == incoming) {
        /* nope - add it */
        PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                             "%s filem:raw: adding file %s to incoming list",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), file));
        incoming = PMIX_NEW(prte_filem_raw_incoming_t);
        incoming->file = strdup(file);
        incoming->type = type;
        pmix_list_append(&incoming_files, &incoming->super);
    }

    /* if this is the first chunk, we need to open the file descriptor */
    if (0 == nchunk) {
        /* separate out the top-level directory of the target */
        char *tmp;
        tmp = strdup(file);
        if (NULL != (cptr = strchr(tmp, '/'))) {
            *cptr = '\0';
        }
        /* save it */
        incoming->top = strdup(tmp);
        free(tmp);
        /* define the full path to where we will put it */
        session_dir = prte_process_info.top_session_dir;

        incoming->fullpath = pmix_os_path(false, session_dir, file, NULL);

        PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                             "%s filem:raw: opening target file %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), incoming->fullpath));
        /* create the path to the target, if not already existing */
        tmp = pmix_dirname(incoming->fullpath);
        if (PMIX_SUCCESS != (rc = pmix_os_dirpath_create(tmp, S_IRWXU))) {
            PMIX_ERROR_LOG(rc);
            send_complete(file, PRTE_ERR_FILE_WRITE_FAILURE);
            free(file);
            free(tmp);
            PMIX_RELEASE(incoming);
            return;
        }
        /* open the file descriptor for writing */
        if (PRTE_FILEM_TYPE_EXE == type) {
            if (0
                > (incoming->fd = open(incoming->fullpath, O_RDWR | O_CREAT | O_TRUNC, S_IRWXU))) {
                pmix_output(0, "%s CANNOT CREATE FILE %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                            incoming->fullpath);
                send_complete(file, PRTE_ERR_FILE_WRITE_FAILURE);
                free(file);
                free(tmp);
                return;
            }
        } else {
            if (0 > (incoming->fd = open(incoming->fullpath, O_RDWR | O_CREAT | O_TRUNC,
                                         S_IRUSR | S_IWUSR))) {
                pmix_output(0, "%s CANNOT CREATE FILE %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                            incoming->fullpath);
                send_complete(file, PRTE_ERR_FILE_WRITE_FAILURE);
                free(file);
                free(tmp);
                return;
            }
        }
        free(tmp);
        incoming->pending = true;
        PRTE_PMIX_THREADSHIFT(incoming, prte_event_base, write_handler);
    }
    /* create an output object for this data */
    output = PMIX_NEW(prte_filem_raw_output_t);
    if (0 < nbytes) {
        /* don't copy 0 bytes - we just need to pass
         * the zero bytes so the fd can be closed
         * after it writes everything out
         */
        memcpy(output->data, data, nbytes);
    }
    output->numbytes = nbytes;

    /* add this data to the write list for this fd */
    pmix_list_append(&incoming->outputs, &output->super);

    if (!incoming->pending) {
        /* add the event */
        incoming->pending = true;
        prte_event_active(&incoming->ev, PRTE_EV_WRITE, 1);
    }

    /* cleanup */
    free(file);
}

static void write_handler(int fd, short event, void *cbdata)
{
    prte_filem_raw_incoming_t *sink = (prte_filem_raw_incoming_t *) cbdata;
    pmix_list_item_t *item;
    prte_filem_raw_output_t *output;
    int num_written;
    char *dirname, *cmd;
    char homedir[MAXPATHLEN];
    int rc;
    PRTE_HIDE_UNUSED_PARAMS(fd, event);

    PMIX_ACQUIRE_OBJECT(sink);

    PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                         "%s write:handler writing data to %d", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         sink->fd));

    /* note that the event is off */
    sink->pending = false;

    while (NULL != (item = pmix_list_remove_first(&sink->outputs))) {
        output = (prte_filem_raw_output_t *) item;
        if (0 == output->numbytes) {
            /* indicates we are to close this stream */
            PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                                 "%s write:handler zero bytes - reporting complete for file %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), sink->file));
            /* close the file descriptor */
            close(sink->fd);
            sink->fd = -1;
            if (PRTE_FILEM_TYPE_FILE == sink->type || PRTE_FILEM_TYPE_EXE == sink->type) {
                /* just link to the top as this will be the
                 * name we will want in each proc's session dir
                 */
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&sink->link_pts, sink->top);
                send_complete(sink->file, PRTE_SUCCESS);
            } else {
                /* unarchive the file */
                if (PRTE_FILEM_TYPE_TAR == sink->type) {
                    pmix_asprintf(&cmd, "tar xf %s", sink->file);
                } else if (PRTE_FILEM_TYPE_BZIP == sink->type) {
                    pmix_asprintf(&cmd, "tar xjf %s", sink->file);
                } else if (PRTE_FILEM_TYPE_GZIP == sink->type) {
                    pmix_asprintf(&cmd, "tar xzf %s", sink->file);
                } else {
                    PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
                    send_complete(sink->file, PRTE_ERR_FILE_WRITE_FAILURE);
                    return;
                }
                if (NULL == getcwd(homedir, sizeof(homedir))) {
                    PRTE_ERROR_LOG(PRTE_ERROR);
                    send_complete(sink->file, PRTE_ERR_FILE_WRITE_FAILURE);
                    return;
                }
                dirname = pmix_dirname(sink->fullpath);
                if (0 != chdir(dirname)) {
                    PRTE_ERROR_LOG(PRTE_ERROR);
                    send_complete(sink->file, PRTE_ERR_FILE_WRITE_FAILURE);
                    return;
                }
                PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                                     "%s write:handler unarchiving file %s with cmd: %s",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), sink->file, cmd));
                if (0 != system(cmd)) {
                    PRTE_ERROR_LOG(PRTE_ERROR);
                    send_complete(sink->file, PRTE_ERR_FILE_WRITE_FAILURE);
                    return;
                }
                if (0 != chdir(homedir)) {
                    PRTE_ERROR_LOG(PRTE_ERROR);
                    send_complete(sink->file, PRTE_ERR_FILE_WRITE_FAILURE);
                    return;
                }
                free(dirname);
                free(cmd);
                /* setup the link points */
                if (PRTE_SUCCESS != (rc = link_archive(sink))) {
                    PRTE_ERROR_LOG(rc);
                    send_complete(sink->file, PRTE_ERR_FILE_WRITE_FAILURE);
                } else {
                    send_complete(sink->file, PRTE_SUCCESS);
                }
            }
            return;
        }
        num_written = write(sink->fd, output->data, output->numbytes);
        PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                             "%s write:handler wrote %d bytes to file %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), num_written, sink->file));
        if (num_written < 0) {
            if (EAGAIN == errno || EINTR == errno) {
                /* push this item back on the front of the list */
                pmix_list_prepend(&sink->outputs, item);
                /* leave the write event running so it will call us again
                 * when the fd is ready.
                 */
                sink->pending = true;
                PMIX_POST_OBJECT(sink);
                prte_event_add(&sink->ev, 0);
                return;
            }
            /* otherwise, something bad happened so all we can do is abort
             * this attempt
             */
            PMIX_OUTPUT_VERBOSE((1, prte_filem_base_framework.framework_output,
                                 "%s write:handler error on write for file %s: %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), sink->file, strerror(errno)));
            PMIX_RELEASE(output);
            pmix_list_remove_item(&incoming_files, &sink->super);
            send_complete(sink->file, PRTE_ERR_FILE_WRITE_FAILURE);
            PMIX_RELEASE(sink);
            return;
        } else if (num_written < output->numbytes) {
            /* incomplete write - adjust data to avoid duplicate output */
            memmove(output->data, &output->data[num_written], output->numbytes - num_written);
            /* push this item back on the front of the list */
            pmix_list_prepend(&sink->outputs, item);
            /* leave the write event running so it will call us again
             * when the fd is ready
             */
            sink->pending = true;
            PMIX_POST_OBJECT(sink);
            prte_event_active(&sink->ev, PRTE_EV_WRITE, 1);
            return;
        }
        PMIX_RELEASE(output);
    }
}

static void xfer_construct(prte_filem_raw_xfer_t *ptr)
{
    memset(&ptr->ev, 0, sizeof(prte_event_t));
    ptr->fd = -1;
    ptr->outbound = NULL;
    ptr->app_idx = 0;
    ptr->pending = false;
    ptr->src = NULL;
    ptr->file = NULL;
    ptr->nchunk = 0;
    ptr->status = PRTE_SUCCESS;
    ptr->nrecvd = 0;
}
static void xfer_destruct(prte_filem_raw_xfer_t *ptr)
{
    if (ptr->pending) {
        prte_event_del(&ptr->ev);
    }
    if (NULL != ptr->src) {
        free(ptr->src);
    }
    if (NULL != ptr->file) {
        free(ptr->file);
    }
}
PMIX_CLASS_INSTANCE(prte_filem_raw_xfer_t,
                    pmix_list_item_t,
                    xfer_construct, xfer_destruct);

static void out_construct(prte_filem_raw_outbound_t *ptr)
{
    PMIX_CONSTRUCT(&ptr->xfers, pmix_list_t);
    ptr->status = PRTE_SUCCESS;
    ptr->cbfunc = NULL;
    ptr->cbdata = NULL;
}
static void out_destruct(prte_filem_raw_outbound_t *ptr)
{
    PMIX_LIST_DESTRUCT(&ptr->xfers);
}
PMIX_CLASS_INSTANCE(prte_filem_raw_outbound_t,
                    pmix_list_item_t,
                    out_construct, out_destruct);

static void in_construct(prte_filem_raw_incoming_t *ptr)
{
    ptr->app_idx = 0;
    ptr->pending = false;
    ptr->fd = -1;
    ptr->file = NULL;
    ptr->top = NULL;
    ptr->fullpath = NULL;
    ptr->link_pts = NULL;
    PMIX_CONSTRUCT(&ptr->outputs, pmix_list_t);
}
static void in_destruct(prte_filem_raw_incoming_t *ptr)
{
    if (ptr->pending) {
        prte_event_del(&ptr->ev);
    }
    if (0 <= ptr->fd) {
        close(ptr->fd);
    }
    if (NULL != ptr->file) {
        free(ptr->file);
    }
    if (NULL != ptr->top) {
        free(ptr->top);
    }
    if (NULL != ptr->fullpath) {
        free(ptr->fullpath);
    }
    PMIX_ARGV_FREE_COMPAT(ptr->link_pts);
    PMIX_LIST_DESTRUCT(&ptr->outputs);
}
PMIX_CLASS_INSTANCE(prte_filem_raw_incoming_t,
                    pmix_list_item_t,
                    in_construct, in_destruct);

static void output_construct(prte_filem_raw_output_t *ptr)
{
    ptr->numbytes = 0;
}
PMIX_CLASS_INSTANCE(prte_filem_raw_output_t,
                    pmix_list_item_t,
                    output_construct, NULL);
