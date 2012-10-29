/*
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <sys/stat.h>

#include "opal/util/if.h"
#include "opal/util/output.h"
#include "opal/util/uri.h"
#include "opal/dss/dss.h"

#include "orte/util/error_strings.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/show_help.h"
#include "orte/util/nidmap.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/state/state.h"

#include "orte/runtime/orte_quit.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/dfs/dfs.h"
#include "orte/mca/dfs/base/base.h"
#include "dfs_orted.h"

/*
 * Module functions: Global
 */
static int init(void);
static int finalize(void);

static void dfs_open(char *uri,
                     orte_dfs_open_callback_fn_t cbfunc,
                     void *cbdata);
static void dfs_close(int fd,
                      orte_dfs_close_callback_fn_t cbfunc,
                      void *cbdata);
static void dfs_get_file_size(int fd,
                              orte_dfs_size_callback_fn_t cbfunc,
                              void *cbdata);
static void dfs_seek(int fd, long offset, int whence,
                     orte_dfs_seek_callback_fn_t cbfunc,
                     void *cbdata);
static void dfs_read(int fd, uint8_t *buffer,
                     long length,
                     orte_dfs_read_callback_fn_t cbfunc,
                     void *cbdata);
static void dfs_post_file_map(opal_byte_object_t *bo,
                              orte_dfs_post_callback_fn_t cbfunc,
                              void *cbdata);
static void dfs_get_file_map(orte_process_name_t *target,
                             orte_dfs_fm_callback_fn_t cbfunc,
                             void *cbdata);
static void dfs_load_file_maps(orte_jobid_t jobid,
                               opal_byte_object_t *bo,
                               orte_dfs_load_callback_fn_t cbfunc,
                               void *cbdata);
static void dfs_purge_file_maps(orte_jobid_t jobid,
                                orte_dfs_purge_callback_fn_t cbfunc,
                                void *cbdata);
/******************
 * Daemon/HNP module
 ******************/
orte_dfs_base_module_t orte_dfs_orted_module = {
    init,
    finalize,
    dfs_open,
    dfs_close,
    dfs_get_file_size,
    dfs_seek,
    dfs_read,
    dfs_post_file_map,
    dfs_get_file_map,
    dfs_load_file_maps,
    dfs_purge_file_maps
};

static opal_list_t requests, active_files, file_maps;
static int local_fd = 0;
static uint64_t req_id = 0;
static void recv_dfs_cmd(int status, orte_process_name_t* sender,
                         opal_buffer_t* buffer, orte_rml_tag_t tag,
                         void* cbdata);
static void recv_dfs_data(int status, orte_process_name_t* sender,
                          opal_buffer_t* buffer, orte_rml_tag_t tag,
                          void* cbdata);

static int init(void)
{
    int rc;

    OBJ_CONSTRUCT(&requests, opal_list_t);
    OBJ_CONSTRUCT(&active_files, opal_list_t);
    OBJ_CONSTRUCT(&file_maps, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_DFS_CMD,
                                                      ORTE_RML_PERSISTENT,
                                                      recv_dfs_cmd,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_DFS_DATA,
                                                      ORTE_RML_PERSISTENT,
                                                      recv_dfs_data,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

static int finalize(void)
{
    opal_list_item_t *item;

    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DFS_CMD);
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DFS_DATA);
    while (NULL != (item = opal_list_remove_first(&requests))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&requests);
    while (NULL != (item = opal_list_remove_first(&active_files))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&active_files);
    while (NULL != (item = opal_list_remove_first(&file_maps))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&file_maps);

    return ORTE_SUCCESS;
}

static void open_local_file(orte_dfs_request_t *dfs)
{
    char *filename;
    orte_dfs_tracker_t *trk;

    /* extract the filename from the uri */
    if (NULL == (filename = opal_filename_from_uri(dfs->uri, NULL))) {
        /* something wrong - error was reported, so just get out */
        if (NULL != dfs->open_cbfunc) {
            dfs->open_cbfunc(-1, dfs->cbdata);
        }
        OBJ_RELEASE(dfs);
        return;
    }
    opal_output_verbose(1, orte_dfs_base.output,
                        "%s opening local file %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        filename);
    /* attempt to open the file */
    if (0 > (dfs->remote_fd = open(filename, O_RDONLY))) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
        if (NULL != dfs->open_cbfunc) {
            dfs->open_cbfunc(dfs->remote_fd, dfs->cbdata);
        }
        OBJ_RELEASE(dfs);
        return;
    }
    /* otherwise, create a tracker for this file */
    trk = OBJ_NEW(orte_dfs_tracker_t);
    trk->requestor.jobid = ORTE_PROC_MY_NAME->jobid;
    trk->requestor.vpid = ORTE_PROC_MY_NAME->vpid;
    trk->filename = strdup(dfs->uri);
    /* define the local fd */
    trk->local_fd = local_fd++;
    /* record the remote file descriptor */
    trk->remote_fd = dfs->remote_fd;
    /* add it to our list of active files */
    opal_list_append(&active_files, &trk->super);
    /* the file is locally hosted */
    trk->host_daemon.jobid = ORTE_PROC_MY_DAEMON->jobid;
    trk->host_daemon.vpid = ORTE_PROC_MY_DAEMON->vpid;
    opal_output_verbose(1, orte_dfs_base.output,
                        "%s local file %s mapped localfd %d to remotefd %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        filename, trk->local_fd, trk->remote_fd);
    /* let the caller know */
    if (NULL != dfs->open_cbfunc) {
        dfs->open_cbfunc(trk->local_fd, dfs->cbdata);
    }
    /* request will be released by the calling routing */
}

static void process_opens(int fd, short args, void *cbdata)
{
    orte_dfs_request_t *dfs = (orte_dfs_request_t*)cbdata;
    int rc;
    opal_buffer_t *buffer;
    char *scheme, *host, *filename;
    int v;
    orte_node_t *node, *nptr;

    /* get the scheme to determine if we can process locally or not */
    if (NULL == (scheme = opal_uri_get_scheme(dfs->uri))) {
        OBJ_RELEASE(dfs);
        return;
    }

    if (0 == strcmp(scheme, "nfs")) {
        open_local_file(dfs);
        goto complete;
    }

    if (0 != strcmp(scheme, "file")) {
        /* not yet supported */
        orte_show_help("orte_dfs_help.txt", "unsupported-filesystem",
                       true, dfs->uri);
        if (NULL != dfs->open_cbfunc) {
            dfs->open_cbfunc(-1, dfs->cbdata);
        }
        goto complete;
    }

    /* dissect the uri to extract host and filename/path */
    if (NULL == (filename = opal_filename_from_uri(dfs->uri, &host))) {
        goto complete;
    }
    /* if the host is our own, then treat it as a local file */
    if (NULL == host ||
        0 == strcmp(host, orte_process_info.nodename) ||
        0 == strcmp(host, "localhost") ||
        opal_ifislocal(host)) {
        opal_output_verbose(1, orte_dfs_base.output,
                            "%s file %s on local host",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            filename);
        open_local_file(dfs);
        goto complete;
    }

    /* ident the daemon on that host */
    node = NULL;
    for (v=0; v < orte_node_pool->size; v++) {
        if (NULL == (nptr = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, v))) {
            continue;
        }
        if (NULL == nptr->daemon) {
            continue;
        }
        if (0 == strcmp(host, nptr->name)) {
            node = nptr;
            break;
        }
    }
    if (NULL == node) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        goto complete;
    }
    opal_output_verbose(1, orte_dfs_base.output,
                        "%s file %s on host %s daemon %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        filename, host, ORTE_NAME_PRINT(&node->daemon->name));
    /* double-check: if it is our local daemon, then we
     * treat this as local
     */
    if (node->daemon->name.vpid == ORTE_PROC_MY_DAEMON->vpid) {
        opal_output_verbose(1, orte_dfs_base.output,
                            "%s local file %s on same daemon",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            filename);
        open_local_file(dfs);
        goto complete;
    }
    /* add this request to our local list so we can
     * match it with the returned response when it comes
     */
    dfs->id = req_id++;
    opal_list_append(&requests, &dfs->super);

    /* setup a message for the daemon telling
     * them what file we want to access
     */
    buffer = OBJ_NEW(opal_buffer_t);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &dfs->cmd, 1, ORTE_DFS_CMD_T))) {
        ORTE_ERROR_LOG(rc);
        opal_list_remove_item(&requests, &dfs->super);
        goto complete;
    }
    /* pass the request id */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &dfs->id, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        opal_list_remove_item(&requests, &dfs->super);
        goto complete;
    }
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &filename, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        opal_list_remove_item(&requests, &dfs->super);
        goto complete;
    }
    
    opal_output_verbose(1, orte_dfs_base.output,
                        "%s sending open file request to %s file %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&node->daemon->name),
                        filename);
    /* send it */
    if (0 > (rc = orte_rml.send_buffer_nb(&node->daemon->name, buffer,
                                          ORTE_RML_TAG_DFS_CMD, 0,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        opal_list_remove_item(&requests, &dfs->super);
        goto complete;
    }
    /* don't release it */
    return;

 complete:
    OBJ_RELEASE(dfs);
}


/* in order to handle the possible opening/reading of files by
 * multiple threads, we have to ensure that all operations are
 * carried out in events - so the "open" cmd simply posts an
 * event containing the required info, and then returns
 */
static void dfs_open(char *uri,
                     orte_dfs_open_callback_fn_t cbfunc,
                     void *cbdata)
{
    orte_dfs_request_t *dfs;

    opal_output_verbose(1, orte_dfs_base.output,
                        "%s opening file %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), uri);

    /* setup the request */
    dfs = OBJ_NEW(orte_dfs_request_t);
    dfs->cmd = ORTE_DFS_OPEN_CMD;
    dfs->uri = strdup(uri);
    dfs->open_cbfunc = cbfunc;
    dfs->cbdata = cbdata;

    /* post it for processing */
    ORTE_DFS_POST_REQUEST(dfs, process_opens);
}

static void process_close(int fd, short args, void *cbdata)
{
    orte_dfs_request_t *close_dfs = (orte_dfs_request_t*)cbdata;
    orte_dfs_tracker_t *tptr, *trk;
    opal_list_item_t *item;
    opal_buffer_t *buffer;
    int rc;

    opal_output_verbose(1, orte_dfs_base.output,
                        "%s closing fd %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        close_dfs->local_fd);

    /* look in our local records for this fd */
    trk = NULL;
    for (item = opal_list_get_first(&active_files);
         item != opal_list_get_end(&active_files);
         item = opal_list_get_next(item)) {
        tptr = (orte_dfs_tracker_t*)item;
        if (tptr->local_fd == close_dfs->local_fd) {
            trk = tptr;
            break;
        }
    }
    if (NULL == trk) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        if (NULL != close_dfs->close_cbfunc) {
            close_dfs->close_cbfunc(close_dfs->local_fd, close_dfs->cbdata);
        }
        OBJ_RELEASE(close_dfs);
        return;
    }

    /* if the file is local, close it */
    if (trk->host_daemon.vpid == ORTE_PROC_MY_DAEMON->vpid) {
        close(trk->remote_fd);
        goto complete;
    }

    /* setup a message for the daemon telling
     * them what file to close
     */
    buffer = OBJ_NEW(opal_buffer_t);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &close_dfs->cmd, 1, ORTE_DFS_CMD_T))) {
        ORTE_ERROR_LOG(rc);
        goto complete;
    }
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &trk->remote_fd, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        goto complete;
    }
    
    opal_output_verbose(1, orte_dfs_base.output,
                        "%s sending close file request to %s for fd %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&trk->host_daemon),
                        trk->local_fd);
    /* send it */
    if (0 > (rc = orte_rml.send_buffer_nb(&trk->host_daemon, buffer,
                                          ORTE_RML_TAG_DFS_CMD, 0,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        goto complete;
    }

 complete:
    opal_list_remove_item(&active_files, &trk->super);
    OBJ_RELEASE(trk);
    if (NULL != close_dfs->close_cbfunc) {
        close_dfs->close_cbfunc(close_dfs->local_fd, close_dfs->cbdata);
    }
    OBJ_RELEASE(close_dfs);
}

static void dfs_close(int fd,
                      orte_dfs_close_callback_fn_t cbfunc,
                      void *cbdata)
{
    orte_dfs_request_t *dfs;

    opal_output_verbose(1, orte_dfs_base.output,
                        "%s close called on fd %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), fd);

    dfs = OBJ_NEW(orte_dfs_request_t);
    dfs->cmd = ORTE_DFS_CLOSE_CMD;
    dfs->local_fd = fd;
    dfs->close_cbfunc = cbfunc;
    dfs->cbdata = cbdata;

    /* post it for processing */
    ORTE_DFS_POST_REQUEST(dfs, process_close);
}

static void process_sizes(int fd, short args, void *cbdata)
{
    orte_dfs_request_t *size_dfs = (orte_dfs_request_t*)cbdata;
    orte_dfs_tracker_t *tptr, *trk;
    opal_list_item_t *item;
    opal_buffer_t *buffer;
    int rc;
    struct stat buf;

    opal_output_verbose(1, orte_dfs_base.output,
                        "%s processing get_size on fd %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        size_dfs->local_fd);

    /* look in our local records for this fd */
    trk = NULL;
    for (item = opal_list_get_first(&active_files);
         item != opal_list_get_end(&active_files);
         item = opal_list_get_next(item)) {
        tptr = (orte_dfs_tracker_t*)item;
        if (tptr->local_fd == size_dfs->local_fd) {
            trk = tptr;
            break;
        }
    }
    if (NULL == trk) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OBJ_RELEASE(size_dfs);
        return;
    }

    /* if the file is local, execute the seek on it - we
     * stuck the "whence" value in the remote_fd
     */
    if (trk->host_daemon.vpid == ORTE_PROC_MY_DAEMON->vpid) {
        /* stat the file and get its size */
        if (0 > stat(trk->filename, &buf)) {
            /* cannot stat file */
            opal_output_verbose(1, orte_dfs_base.output,
                                "%s could not stat %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                trk->filename);
            if (NULL != size_dfs->size_cbfunc) {
                size_dfs->size_cbfunc(-1, size_dfs->cbdata);
            }
        }
        goto complete;
    }

    /* setup a message for the daemon telling
     * them what file to get the size of
     */
    buffer = OBJ_NEW(opal_buffer_t);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &size_dfs->cmd, 1, ORTE_DFS_CMD_T))) {
        ORTE_ERROR_LOG(rc);
        goto complete;
    }
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &trk->remote_fd, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        goto complete;
    }

    opal_output_verbose(1, orte_dfs_base.output,
                        "%s sending get_size request to %s for fd %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&trk->host_daemon),
                        trk->local_fd);
    /* send it */
    if (0 > (rc = orte_rml.send_buffer_nb(&trk->host_daemon, buffer,
                                          ORTE_RML_TAG_DFS_CMD, 0,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        if (NULL != size_dfs->size_cbfunc) {
            size_dfs->size_cbfunc(-1, size_dfs->cbdata);
        }
        goto complete;
    }

 complete:
    OBJ_RELEASE(size_dfs);
}

static void dfs_get_file_size(int fd,
                              orte_dfs_size_callback_fn_t cbfunc,
                              void *cbdata)
{
    orte_dfs_request_t *dfs;

    opal_output_verbose(1, orte_dfs_base.output,
                        "%s get_size called on fd %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), fd);

    dfs = OBJ_NEW(orte_dfs_request_t);
    dfs->cmd = ORTE_DFS_SIZE_CMD;
    dfs->local_fd = fd;
    dfs->size_cbfunc = cbfunc;
    dfs->cbdata = cbdata;

    /* post it for processing */
    ORTE_DFS_POST_REQUEST(dfs, process_sizes);
}


static void process_seeks(int fd, short args, void *cbdata)
{
    orte_dfs_request_t *seek_dfs = (orte_dfs_request_t*)cbdata;
    orte_dfs_tracker_t *tptr, *trk;
    opal_list_item_t *item;
    opal_buffer_t *buffer;
    int64_t i64;
    int rc;
    struct stat buf;

    opal_output_verbose(1, orte_dfs_base.output,
                        "%s processing seek on fd %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        seek_dfs->local_fd);

    /* look in our local records for this fd */
    trk = NULL;
    for (item = opal_list_get_first(&active_files);
         item != opal_list_get_end(&active_files);
         item = opal_list_get_next(item)) {
        tptr = (orte_dfs_tracker_t*)item;
        if (tptr->local_fd == seek_dfs->local_fd) {
            trk = tptr;
            break;
        }
    }
    if (NULL == trk) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OBJ_RELEASE(seek_dfs);
        return;
    }

    /* if the file is local, execute the seek on it - we
     * stuck the "whence" value in the remote_fd
     */
    if (trk->host_daemon.vpid == ORTE_PROC_MY_DAEMON->vpid) {
        opal_output_verbose(1, orte_dfs_base.output,
                            "%s local seek on fd %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            seek_dfs->local_fd);
        /* stat the file and get its size */
        if (0 > stat(trk->filename, &buf)) {
            /* cannot stat file */
            opal_output_verbose(1, orte_dfs_base.output,
                                "%s could not stat %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                trk->filename);
            if (NULL != seek_dfs->seek_cbfunc) {
                seek_dfs->seek_cbfunc(-1, seek_dfs->cbdata);
            }
        } else if (buf.st_size < seek_dfs->read_length &&
                   SEEK_SET == seek_dfs->remote_fd) {
            /* seek would take us past EOF */
            if (NULL != seek_dfs->seek_cbfunc) {
                seek_dfs->seek_cbfunc(-1, seek_dfs->cbdata);
            }
        } else if (buf.st_size < (off_t)(trk->location + seek_dfs->read_length) &&
                   SEEK_CUR == seek_dfs->remote_fd) {
            /* seek would take us past EOF */
            if (NULL != seek_dfs->seek_cbfunc) {
                seek_dfs->seek_cbfunc(-1, seek_dfs->cbdata);
            }
        } else {
            lseek(trk->remote_fd, seek_dfs->read_length, seek_dfs->remote_fd);
            if (SEEK_SET == seek_dfs->remote_fd) {
                trk->location = seek_dfs->read_length;
            } else {
                trk->location += seek_dfs->read_length;
            }
        }
        goto complete;
    }
    /* add this request to our local list so we can
     * match it with the returned response when it comes
     */
    seek_dfs->id = req_id++;
    opal_list_append(&requests, &seek_dfs->super);

    /* setup a message for the daemon telling
     * them what file to seek
     */
    buffer = OBJ_NEW(opal_buffer_t);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &seek_dfs->cmd, 1, ORTE_DFS_CMD_T))) {
        ORTE_ERROR_LOG(rc);
        goto complete;
    }
    /* pass the request id */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &seek_dfs->id, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        opal_list_remove_item(&requests, &seek_dfs->super);
        goto complete;
    }
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &trk->remote_fd, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        goto complete;
    }
    i64 = (int64_t)seek_dfs->read_length;
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &i64, 1, OPAL_INT64))) {
        ORTE_ERROR_LOG(rc);
        goto complete;
    }
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &seek_dfs->remote_fd, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        goto complete;
    }

    opal_output_verbose(1, orte_dfs_base.output,
                        "%s sending seek file request to %s for fd %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&trk->host_daemon),
                        trk->local_fd);
    /* send it */
    if (0 > (rc = orte_rml.send_buffer_nb(&trk->host_daemon, buffer,
                                          ORTE_RML_TAG_DFS_CMD, 0,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        goto complete;
    }

 complete:
    OBJ_RELEASE(seek_dfs);
}


static void dfs_seek(int fd, long offset, int whence,
                     orte_dfs_seek_callback_fn_t cbfunc,
                     void *cbdata)
{
    orte_dfs_request_t *dfs;

    opal_output_verbose(1, orte_dfs_base.output,
                        "%s seek called on fd %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), fd);

    dfs = OBJ_NEW(orte_dfs_request_t);
    dfs->cmd = ORTE_DFS_SEEK_CMD;
    dfs->local_fd = fd;
    dfs->read_length = offset;
    dfs->remote_fd = whence;
    dfs->seek_cbfunc = cbfunc;
    dfs->cbdata = cbdata;

    /* post it for processing */
    ORTE_DFS_POST_REQUEST(dfs, process_seeks);
}

static void process_reads(int fd, short args, void *cbdata)
{
    orte_dfs_request_t *read_dfs = (orte_dfs_request_t*)cbdata;
    orte_dfs_tracker_t *tptr, *trk;
    long nbytes;
    opal_list_item_t *item;
    opal_buffer_t *buffer;
    int64_t i64;
    int rc;

    /* look in our local records for this fd */
    trk = NULL;
    for (item = opal_list_get_first(&active_files);
         item != opal_list_get_end(&active_files);
         item = opal_list_get_next(item)) {
        tptr = (orte_dfs_tracker_t*)item;
        if (tptr->local_fd == read_dfs->local_fd) {
            trk = tptr;
            break;
        }
    }
    if (NULL == trk) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OBJ_RELEASE(read_dfs);
        return;
    }

    /* if the file is local, read the desired bytes */
    if (trk->host_daemon.vpid == ORTE_PROC_MY_DAEMON->vpid) {
        nbytes = read(trk->remote_fd, read_dfs->read_buffer, read_dfs->read_length);
        if (0 < nbytes) {
            /* update our location */
            trk->location += nbytes;
        }
        /* pass them back to the caller */
        if (NULL != read_dfs->read_cbfunc) {
            read_dfs->read_cbfunc(nbytes, read_dfs->read_buffer, read_dfs->cbdata);
        }
        /* request is complete */
        OBJ_RELEASE(read_dfs);
        return;
    }
    /* add this request to our pending list */
    read_dfs->id = req_id++;
    opal_list_append(&requests, &read_dfs->super);

    /* setup a message for the daemon telling
     * them what file to read
     */
    buffer = OBJ_NEW(opal_buffer_t);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &read_dfs->cmd, 1, ORTE_DFS_CMD_T))) {
        ORTE_ERROR_LOG(rc);
        goto complete;
    }
    /* include the request id */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &read_dfs->id, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        goto complete;
    }
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &trk->remote_fd, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        goto complete;
    }
    i64 = (int64_t)read_dfs->read_length;
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &i64, 1, OPAL_INT64))) {
        ORTE_ERROR_LOG(rc);
        goto complete;
    }
    
    opal_output_verbose(1, orte_dfs_base.output,
                        "%s sending read file request to %s for fd %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&trk->host_daemon),
                        trk->local_fd);
    /* send it */
    if (0 > (rc = orte_rml.send_buffer_nb(&trk->host_daemon, buffer,
                                          ORTE_RML_TAG_DFS_CMD, 0,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
    }
    /* don't release the request */
    return;

 complete:
    /* don't need to hang on to this request */
    opal_list_remove_item(&requests, &read_dfs->super);
    OBJ_RELEASE(read_dfs);
}

static void dfs_read(int fd, uint8_t *buffer,
                     long length,
                     orte_dfs_read_callback_fn_t cbfunc,
                     void *cbdata)
{
    orte_dfs_request_t *dfs;

    dfs = OBJ_NEW(orte_dfs_request_t);
    dfs->cmd = ORTE_DFS_READ_CMD;
    dfs->local_fd = fd;
    dfs->read_buffer = buffer;
    dfs->read_length = length;
    dfs->read_cbfunc = cbfunc;
    dfs->cbdata = cbdata;

    /* post it for processing */
    ORTE_DFS_POST_REQUEST(dfs, process_reads);
}

static void process_posts(int fd, short args, void *cbdata)
{
    orte_dfs_request_t *dfs = (orte_dfs_request_t*)cbdata;
    orte_dfs_jobfm_t *jptr, *jfm;
    orte_dfs_vpidfm_t *vptr, *vfm;
    opal_list_item_t *item;
    int rc;
    opal_buffer_t buf;

    opal_output_verbose(1, orte_dfs_base.output,
                        "%s posting file map for target %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&dfs->target));

    /* lookup the job map */
    jfm = NULL;
    for (item = opal_list_get_first(&file_maps);
         item != opal_list_get_end(&file_maps);
         item = opal_list_get_next(item)) {
        jptr = (orte_dfs_jobfm_t*)item;
        if (jptr->jobid == dfs->target.jobid) {
            jfm = jptr;
            break;
        }
    }
    if (NULL == jfm) {
        /* add it */
        jfm = OBJ_NEW(orte_dfs_jobfm_t);
        jfm->jobid = dfs->target.jobid;
        opal_list_append(&file_maps, &jfm->super);
    }
    /* see if we already have an entry for this source */
    vfm = NULL;
    for (item = opal_list_get_first(&jfm->maps);
         item != opal_list_get_end(&jfm->maps);
         item = opal_list_get_next(item)) {
        vptr = (orte_dfs_vpidfm_t*)item;
        if (vptr->vpid == dfs->target.vpid) {
            vfm = vptr;
            break;
        }
    }
    if (NULL == vfm) {
        /* add it */
        vfm = OBJ_NEW(orte_dfs_vpidfm_t);
        vfm->vpid = dfs->target.vpid;
        opal_list_append(&jfm->maps, &vfm->super);
    }

    /* add this entry to any prior one */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    opal_dss.load(&buf, vfm->fm.bytes, vfm->fm.size);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &dfs->bo, 1, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        goto complete;
    }
    vfm->num_entries++;
    opal_dss.unload(&buf, (void**)&vfm->fm.bytes, &vfm->fm.size);
    opal_output_verbose(1, orte_dfs_base.output,
                        "%s target %s now has %d entries",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&dfs->target),
                        vfm->num_entries);

 complete:
    if (NULL != dfs->post_cbfunc) {
        dfs->post_cbfunc(dfs->cbdata);
    }
    OBJ_RELEASE(dfs);
}

static void dfs_post_file_map(opal_byte_object_t *bo,
                              orte_dfs_post_callback_fn_t cbfunc,
                              void *cbdata)
{
    orte_dfs_request_t *dfs;

    dfs = OBJ_NEW(orte_dfs_request_t);
    dfs->cmd = ORTE_DFS_POST_CMD;
    dfs->target.jobid = ORTE_PROC_MY_NAME->jobid;
    dfs->target.vpid = ORTE_PROC_MY_NAME->vpid;
    dfs->bo = bo;
    dfs->post_cbfunc = cbfunc;
    dfs->cbdata = cbdata;

    /* post it for processing */
    ORTE_DFS_POST_REQUEST(dfs, process_posts);
}

static void get_job_maps(orte_dfs_jobfm_t *jfm,
                         orte_vpid_t vpid,
                         opal_buffer_t *buf)
{
    orte_dfs_vpidfm_t *vfm;
    opal_list_item_t *item;
    opal_byte_object_t *boptr;
    int rc;

    /* if the target vpid is WILDCARD, then process
     * data for all vpids - else, find the one
     */
    for (item = opal_list_get_first(&jfm->maps);
         item != opal_list_get_end(&jfm->maps);
         item = opal_list_get_next(item)) {
        vfm = (orte_dfs_vpidfm_t*)item;
        if (ORTE_VPID_WILDCARD == vpid ||
            vfm->vpid == vpid) {
            /* indicate data from this vpid */
            if (OPAL_SUCCESS != (rc = opal_dss.pack(buf, &vfm->vpid, 1, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                return;
            }
            /* pack the number of entries in its byte object */
            if (OPAL_SUCCESS != (rc = opal_dss.pack(buf, &vfm->num_entries, 1, OPAL_INT32))) {
                ORTE_ERROR_LOG(rc);
                return;
            }
            /* pack the byte object */
            boptr = &vfm->fm;
            if (OPAL_SUCCESS != (rc = opal_dss.pack(buf, &boptr, 1, OPAL_BYTE_OBJECT))) {
                ORTE_ERROR_LOG(rc);
                return;
            }
        }
    }
}

static void process_getfm(int fd, short args, void *cbdata)
{
    orte_dfs_request_t *dfs = (orte_dfs_request_t*)cbdata;
    orte_dfs_jobfm_t *jfm;
    opal_buffer_t buf;
    opal_list_item_t *item;
    opal_byte_object_t bo;

    /* prep the collection bucket */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);

    /* if the target job is WILDCARD, then process
     * data for all jobids - else, find the one
     */
    for (item = opal_list_get_first(&file_maps);
         item != opal_list_get_end(&file_maps);
         item = opal_list_get_next(item)) {
        jfm = (orte_dfs_jobfm_t*)item;
        if (ORTE_JOBID_WILDCARD == dfs->target.jobid ||
            jfm->jobid == dfs->target.jobid) {
            get_job_maps(jfm, dfs->target.vpid, &buf);
        }
    }

    /* unload the result */
    opal_dss.unload(&buf, (void**)&bo.bytes, &bo.size);
    OBJ_DESTRUCT(&buf);

    if (NULL != dfs->fm_cbfunc) {
        dfs->fm_cbfunc(&bo, dfs->cbdata);
    }
    if (NULL != bo.bytes) {
        free(bo.bytes);
    }
    OBJ_RELEASE(dfs);
}

static void dfs_get_file_map(orte_process_name_t *target,
                             orte_dfs_fm_callback_fn_t cbfunc,
                             void *cbdata)
{
    orte_dfs_request_t *dfs;

    opal_output_verbose(1, orte_dfs_base.output,
                        "%s get file map for %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(target));

    dfs = OBJ_NEW(orte_dfs_request_t);
    dfs->cmd = ORTE_DFS_GETFM_CMD;
    dfs->target.jobid = target->jobid;
    dfs->target.vpid = target->vpid;
    dfs->fm_cbfunc = cbfunc;
    dfs->cbdata = cbdata;

    /* post it for processing */
    ORTE_DFS_POST_REQUEST(dfs, process_getfm);
}

static void process_load(int fd, short args, void *cbdata)
{
    orte_dfs_request_t *dfs = (orte_dfs_request_t*)cbdata;
    opal_list_item_t *item;
    orte_dfs_jobfm_t *jfm, *jptr;
    orte_dfs_vpidfm_t *vfm;
    orte_vpid_t vpid;
    int32_t entries;
    opal_byte_object_t *boptr;
    opal_buffer_t buf;
    int cnt;
    int rc;

    /* see if we already have a tracker for this job */
    jfm = NULL;
    for (item = opal_list_get_first(&file_maps);
         item != opal_list_get_end(&file_maps);
         item = opal_list_get_next(item)) {
        jptr = (orte_dfs_jobfm_t*)item;
        if (jptr->jobid == dfs->target.jobid) {
            jfm = jptr;
            break;
        }
    }
    if (NULL != jfm) {
        /* need to purge it first */
        while (NULL != (item = opal_list_remove_first(&jfm->maps))) {
            OBJ_RELEASE(item);
        }
    } else {
        jfm = OBJ_NEW(orte_dfs_jobfm_t);
        jfm->jobid = dfs->target.jobid;
        opal_list_append(&file_maps, &jfm->super);
    }

    /* setup to unpack the byte object */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    opal_dss.load(&buf, dfs->bo->bytes, dfs->bo->size);

    /* unpack the buffer */
    cnt = 1;
    while (OPAL_SUCCESS == opal_dss.unpack(&buf, &vpid, &cnt, ORTE_VPID)) {
        /* unpack the number of entries contained in the byte object */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, &entries, &cnt, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            goto complete;
        }
        /* unpack the byte object */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, &boptr, &cnt, OPAL_BYTE_OBJECT))) {
            ORTE_ERROR_LOG(rc);
            goto complete;
        }
        /* create the entry */
        vfm = OBJ_NEW(orte_dfs_vpidfm_t);
        vfm->vpid = vpid;
        vfm->num_entries = entries;
        vfm->fm.bytes = boptr->bytes;
        boptr->bytes = NULL;
        vfm->fm.size = boptr->size;
        opal_list_append(&jfm->maps, &vfm->super);
        /* cleanup */
        free(boptr);
    }

 complete:
    /* must reload the byte object as it belongs to
     * our original caller - the load function will
     * have cleared it
     */
    opal_dss.unload(&buf, (void**)&dfs->bo->bytes, &dfs->bo->size);
    OBJ_DESTRUCT(&buf);

    if (NULL != dfs->load_cbfunc) {
        dfs->load_cbfunc(dfs->cbdata);
    }
    OBJ_RELEASE(dfs);
}

static void dfs_load_file_maps(orte_jobid_t jobid,
                               opal_byte_object_t *bo,
                               orte_dfs_load_callback_fn_t cbfunc,
                               void *cbdata)
{
    orte_dfs_request_t *dfs;

    opal_output_verbose(1, orte_dfs_base.output,
                        "%s loading file maps for %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_JOBID_PRINT(jobid));

    dfs = OBJ_NEW(orte_dfs_request_t);
    dfs->cmd = ORTE_DFS_LOAD_CMD;
    dfs->target.jobid = jobid;
    dfs->bo = bo;
    dfs->load_cbfunc = cbfunc;
    dfs->cbdata = cbdata;

    /* post it for processing */
    ORTE_DFS_POST_REQUEST(dfs, process_load);
}

static void process_purge(int fd, short args, void *cbdata)
{
    orte_dfs_request_t *dfs = (orte_dfs_request_t*)cbdata;
    opal_list_item_t *item;
    orte_dfs_jobfm_t *jfm, *jptr;

    /* find the job tracker */
    jfm = NULL;
    for (item = opal_list_get_first(&file_maps);
         item != opal_list_get_end(&file_maps);
         item = opal_list_get_next(item)) {
        jptr = (orte_dfs_jobfm_t*)item;
        if (jptr->jobid == dfs->target.jobid) {
            jfm = jptr;
            break;
        }
    }
    if (NULL == jptr) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
    } else {
        /* remove it from the list */
        opal_list_remove_item(&file_maps, &jptr->super);
        /* the destructor will release the list of maps
         * in the jobfm object
         */
        OBJ_RELEASE(jptr);
    }

    if (NULL != dfs->purge_cbfunc) {
        dfs->purge_cbfunc(dfs->cbdata);
    }
    OBJ_RELEASE(dfs);
}

static void dfs_purge_file_maps(orte_jobid_t jobid,
                                orte_dfs_purge_callback_fn_t cbfunc,
                                void *cbdata)
{
    orte_dfs_request_t *dfs;

    opal_output_verbose(1, orte_dfs_base.output,
                        "%s purging file maps for job %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_JOBID_PRINT(jobid));

    dfs = OBJ_NEW(orte_dfs_request_t);
    dfs->cmd = ORTE_DFS_PURGE_CMD;
    dfs->target.jobid = jobid;
    dfs->purge_cbfunc = cbfunc;
    dfs->cbdata = cbdata;

    /* post it for processing */
    ORTE_DFS_POST_REQUEST(dfs, process_purge);
}


/* receives take place in an event, so we are free to process
 * the request list without fear of getting things out-of-order
 */
static void recv_dfs_cmd(int status, orte_process_name_t* sender,
                         opal_buffer_t* buffer, orte_rml_tag_t tag,
                         void* cbdata)
{
    orte_dfs_cmd_t cmd;
    int32_t cnt;
    opal_list_item_t *item;
    int remote_fd, my_fd, rc;
    char *filename;
    orte_dfs_tracker_t *trk;
    int64_t i64, bytes_read;
    uint8_t *read_buf;
    uint64_t rid;
    int whence;
    struct stat buf;
    orte_process_name_t source;
    opal_byte_object_t *bo;
    orte_dfs_request_t *dfs;
    orte_dfs_jobfm_t *jfm;
    opal_buffer_t *answer, bucket;

    /* unpack the command */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &cmd, &cnt, ORTE_DFS_CMD_T))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    opal_output_verbose(1, orte_dfs_base.output,
                        "%s received command %d from %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)cmd,
                        ORTE_NAME_PRINT(sender));

    switch (cmd) {
    case ORTE_DFS_OPEN_CMD:
        /* unpack their request id */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &rid, &cnt, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* unpack the filename */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &filename, &cnt, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* attempt to open the file */
        opal_output_verbose(1, orte_dfs_base.output,
                            "%s opening file %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            filename);
        if (0 > (my_fd = open(filename, O_RDONLY))) {
            ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
            goto answer_open;
        }
        /* create a tracker for this file */
        trk = OBJ_NEW(orte_dfs_tracker_t);
        trk->requestor.jobid = sender->jobid;
        trk->requestor.vpid = sender->vpid;
        trk->host_daemon.jobid = ORTE_PROC_MY_NAME->jobid;
        trk->host_daemon.vpid = ORTE_PROC_MY_NAME->vpid;
        trk->filename = strdup(filename);
        trk->local_fd = my_fd;
        opal_list_append(&active_files, &trk->super);
    answer_open:
        /* construct the return message */
        answer = OBJ_NEW(opal_buffer_t);
        if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &cmd, 1, ORTE_DFS_CMD_T))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &rid, 1, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &my_fd, 1, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* send it */
        if (0 > (rc = orte_rml.send_buffer_nb(sender, answer,
                                              ORTE_RML_TAG_DFS_DATA, 0,
                                              orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(answer);
            return;
        }
        break;

    case ORTE_DFS_CLOSE_CMD:
        /* unpack our fd */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &my_fd, &cnt, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* find the corresponding tracker */
        for (item = opal_list_get_first(&active_files);
             item != opal_list_get_end(&active_files);
             item = opal_list_get_next(item)) {
            trk = (orte_dfs_tracker_t*)item;
            if (my_fd == trk->local_fd) {
                /* remove it */
                opal_list_remove_item(&active_files, item);
                OBJ_RELEASE(item);
                /* close the file */
                close(my_fd);
                break;
            }
        }
        break;

    case ORTE_DFS_SIZE_CMD:
        /* unpack their request id */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &rid, &cnt, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* unpack our fd */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &my_fd, &cnt, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* find the corresponding tracker */
        i64 = -1;
        for (item = opal_list_get_first(&active_files);
             item != opal_list_get_end(&active_files);
             item = opal_list_get_next(item)) {
            trk = (orte_dfs_tracker_t*)item;
            if (my_fd == trk->local_fd) {
                /* stat the file and get its size */
                if (0 > stat(trk->filename, &buf)) {
                    /* cannot stat file */
                    opal_output_verbose(1, orte_dfs_base.output,
                                        "%s could not stat %s",
                                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                        trk->filename);
                } else {
                    i64 = buf.st_size;
                }
                break;
            }
        }
        /* construct the return message */
        answer = OBJ_NEW(opal_buffer_t);
        if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &cmd, 1, ORTE_DFS_CMD_T))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &rid, 1, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &i64, 1, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* send it */
        if (0 > (rc = orte_rml.send_buffer_nb(sender, answer,
                                              ORTE_RML_TAG_DFS_DATA, 0,
                                              orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(answer);
            return;
        }
        break;

    case ORTE_DFS_SEEK_CMD:
        /* unpack their request id */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &rid, &cnt, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* unpack our fd */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &my_fd, &cnt, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* unpack the offset */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &i64, &cnt, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* unpack the whence */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &whence, &cnt, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* set default error */
        bytes_read = -1;
        /* find the corresponding tracker - we do this to ensure
         * that the local fd we were sent is actually open
         */
        for (item = opal_list_get_first(&active_files);
             item != opal_list_get_end(&active_files);
             item = opal_list_get_next(item)) {
            trk = (orte_dfs_tracker_t*)item;
            if (my_fd == trk->local_fd) {
                /* stat the file and get its size */
                if (0 > stat(trk->filename, &buf)) {
                    /* cannot stat file */
                    opal_output_verbose(1, orte_dfs_base.output,
                                        "%s seek could not stat %s",
                                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                        trk->filename);
                } else if (buf.st_size < i64 && SEEK_SET == whence) {
                    /* seek would take us past EOF */
                    opal_output_verbose(1, orte_dfs_base.output,
                                        "%s seek SET past EOF on file %s",
                                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                        trk->filename);
                    bytes_read = -2;
                } else if (buf.st_size < (off_t)(trk->location + i64) &&
                           SEEK_CUR == whence) {
                    /* seek would take us past EOF */
                    opal_output_verbose(1, orte_dfs_base.output,
                                        "%s seek CUR past EOF on file %s",
                                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                        trk->filename);
                    bytes_read = -3;
                } else {
                    lseek(my_fd, i64, whence);
                    if (SEEK_SET == whence) {
                        trk->location = i64;
                    } else {
                        trk->location += i64;
                    }
                    bytes_read = i64;
                }
                break;
            }
        }
        /* construct the return message */
        answer = OBJ_NEW(opal_buffer_t);
        if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &cmd, 1, ORTE_DFS_CMD_T))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &rid, 1, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* return the offset/status */
        if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &bytes_read, 1, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* send it */
        opal_output_verbose(1, orte_dfs_base.output,
                            "%s sending %ld offset back to %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            (long)bytes_read,
                            ORTE_NAME_PRINT(sender));
        if (0 > (rc = orte_rml.send_buffer_nb(sender, answer,
                                              ORTE_RML_TAG_DFS_DATA, 0,
                                              orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(answer);
            return;
        }
        break;

    case ORTE_DFS_READ_CMD:
        /* set default error */
        remote_fd = -1;
        my_fd = -1;
        bytes_read = -1;
        read_buf = NULL;
        /* unpack their request id */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &rid, &cnt, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* unpack our fd */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &my_fd, &cnt, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            goto answer_read;
        }
        /* unpack the number of bytes to read */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &i64, &cnt, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            goto answer_read;
        }
        opal_output_verbose(1, orte_dfs_base.output,
                            "%s reading %ld bytes from local fd %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            (long)i64, my_fd);
        /* find the corresponding tracker - we do this to ensure
         * that the local fd we were sent is actually open
         */
        for (item = opal_list_get_first(&active_files);
             item != opal_list_get_end(&active_files);
             item = opal_list_get_next(item)) {
            trk = (orte_dfs_tracker_t*)item;
            if (my_fd == trk->local_fd) {
                /* do the read */
                opal_output_verbose(1, orte_dfs_base.output,
                                    "%s issuing read",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                read_buf = (uint8_t*)malloc(i64);
                bytes_read = read(my_fd, read_buf, (long)i64);
                if (0 < bytes_read) {
                    /* update our location */
                    trk->location += bytes_read;
                }
                break;
            }
        }
    answer_read:
        /* construct the return message */
        answer = OBJ_NEW(opal_buffer_t);
        if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &cmd, 1, ORTE_DFS_CMD_T))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &rid, 1, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* include the number of bytes read */
        if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &bytes_read, 1, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* include the bytes read */
        if (0 < bytes_read) {
            if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, read_buf, bytes_read, OPAL_UINT8))) {
                ORTE_ERROR_LOG(rc);
                return;
            }
        }
        /* send it */
        opal_output_verbose(1, orte_dfs_base.output,
                            "%s sending %ld bytes back to %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            (long)bytes_read,
                            ORTE_NAME_PRINT(sender));
        if (0 > (rc = orte_rml.send_buffer_nb(sender, answer,
                                              ORTE_RML_TAG_DFS_DATA, 0,
                                              orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(answer);
            return;
        }
        if (NULL != read_buf) {
            free(read_buf);
        }
        break;

    case ORTE_DFS_POST_CMD:
        /* unpack their request id */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &rid, &cnt, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* unpack the name of the source of this data */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &source, &cnt, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* unpack their byte object */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &bo, &cnt, OPAL_BYTE_OBJECT))) {
            ORTE_ERROR_LOG(rc);
            goto answer_read;
        }
        /* add the contents to the storage for this process */
        dfs = OBJ_NEW(orte_dfs_request_t);
        dfs->target.jobid = source.jobid;
        dfs->target.vpid = source.vpid;
        dfs->bo = bo;
        dfs->post_cbfunc = NULL;
        process_posts(0, 0, (void*)dfs);
        /* return an ack */
        answer = OBJ_NEW(opal_buffer_t);
        if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &cmd, 1, ORTE_DFS_CMD_T))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &rid, 1, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        if (0 > (rc = orte_rml.send_buffer_nb(sender, answer,
                                              ORTE_RML_TAG_DFS_DATA, 0,
                                              orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(answer);
        }
        if (NULL != bo->bytes) {
            free(bo->bytes);
        }
        free(bo);
        break;

    case ORTE_DFS_GETFM_CMD:
        /* unpack their request id */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &rid, &cnt, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* unpack the target */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &source, &cnt, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            goto answer_read;
        }
        /* search our data tree for matches, assembling them
         * into a byte object
         */
        /* if the target job is WILDCARD, then process
         * data for all jobids - else, find the one
         */
        OBJ_CONSTRUCT(&bucket, opal_buffer_t);
        for (item = opal_list_get_first(&file_maps);
             item != opal_list_get_end(&file_maps);
             item = opal_list_get_next(item)) {
            jfm = (orte_dfs_jobfm_t*)item;
            if (ORTE_JOBID_WILDCARD == source.jobid ||
                jfm->jobid == source.jobid) {
                get_job_maps(jfm, source.vpid, &bucket);
            }
        }
        /* unload the result */
        bo = (opal_byte_object_t*)malloc(sizeof(opal_byte_object_t));
        opal_dss.unload(&bucket, (void**)&bo->bytes, &bo->size);
        OBJ_DESTRUCT(&bucket);
        opal_output_verbose(1, orte_dfs_base.output,
                            "%s getf-cmd: returning %d bytes to sender %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), bo->size,
                            ORTE_NAME_PRINT(sender));
        /* construct the response */
        answer = OBJ_NEW(opal_buffer_t);
        if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &cmd, 1, ORTE_DFS_CMD_T))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &rid, 1, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &bo, 1, OPAL_BYTE_OBJECT))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        if (NULL != bo->bytes) {
            free(bo->bytes);
        }
        free(bo);
        if (0 > (rc = orte_rml.send_buffer_nb(sender, answer,
                                              ORTE_RML_TAG_DFS_DATA, 0,
                                              orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(answer);
        }
        break;

    default:
        opal_output(0, "ORTED:DFS:RECV_DFS WTF");
        break;
    }
}

static void recv_dfs_data(int status, orte_process_name_t* sender,
                          opal_buffer_t* buffer, orte_rml_tag_t tag,
                          void* cbdata)
{
    orte_dfs_cmd_t cmd;
    int32_t cnt;
    orte_dfs_request_t *dfs, *dptr;
    opal_list_item_t *item;
    int remote_fd, rc;
    int64_t i64;
    uint64_t rid;
    orte_dfs_tracker_t *trk;

    /* unpack the command this message is responding to */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &cmd, &cnt, ORTE_DFS_CMD_T))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    opal_output_verbose(1, orte_dfs_base.output,
                        "%s recvd:data cmd %d from sender %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)cmd,
                        ORTE_NAME_PRINT(sender));

    switch (cmd) {
    case ORTE_DFS_OPEN_CMD:
        /* unpack the request id */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &rid, &cnt, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* unpack the remote fd */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &remote_fd, &cnt, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* search our list of requests to find the matching one */
        dfs = NULL;
        for (item = opal_list_get_first(&requests);
             item != opal_list_get_end(&requests);
             item = opal_list_get_next(item)) {
            dptr = (orte_dfs_request_t*)item;
            if (dptr->id == rid) {
                /* as the request has been fulfilled, remove it */
                opal_list_remove_item(&requests, item);
                dfs = dptr;
                break;
            }
        }
        if (NULL == dfs) {
            opal_output_verbose(1, orte_dfs_base.output,
                                "%s recvd:data open file - no corresponding request found for local fd %d",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), local_fd);
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return;
        }

        /* if the remote_fd < 0, then we had an error, so return
         * the error value to the caller
         */
        if (remote_fd < 0) {
            opal_output_verbose(1, orte_dfs_base.output,
                                "%s recvd:data open file response error file %s [error: %d]",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                dfs->uri, remote_fd);
            if (NULL != dfs->open_cbfunc) {
                dfs->open_cbfunc(remote_fd, dfs->cbdata);
            }
            /* release the request */
            OBJ_RELEASE(dfs);
            return;
        }
        /* otherwise, create a tracker for this file */
        trk = OBJ_NEW(orte_dfs_tracker_t);
        trk->requestor.jobid = ORTE_PROC_MY_NAME->jobid;
        trk->requestor.vpid = ORTE_PROC_MY_NAME->vpid;
        trk->host_daemon.jobid = sender->jobid;
        trk->host_daemon.vpid = sender->vpid;
        trk->filename = strdup(dfs->uri);
        /* define the local fd */
        trk->local_fd = local_fd++;
        /* record the remote file descriptor */
        trk->remote_fd = remote_fd;
        /* add it to our list of active files */
        opal_list_append(&active_files, &trk->super);
        /* return the local_fd to the caller for
         * subsequent operations
         */
        opal_output_verbose(1, orte_dfs_base.output,
                            "%s recvd:data open file completed for file %s [local fd: %d remote fd: %d]",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            dfs->uri, trk->local_fd, remote_fd);
        if (NULL != dfs->open_cbfunc) {
            dfs->open_cbfunc(trk->local_fd, dfs->cbdata);
        }
        /* release the request */
        OBJ_RELEASE(dfs);
        break;

    case ORTE_DFS_SIZE_CMD:
        /* unpack the request id for this request */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &rid, &cnt, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* search our list of requests to find the matching one */
        dfs = NULL;
        for (item = opal_list_get_first(&requests);
             item != opal_list_get_end(&requests);
             item = opal_list_get_next(item)) {
            dptr = (orte_dfs_request_t*)item;
            if (dptr->id == rid) {
                /* request was fulfilled, so remove it */
                opal_list_remove_item(&requests, item);
                dfs = dptr;
                break;
            }
        }
        if (NULL == dfs) {
            opal_output_verbose(1, orte_dfs_base.output,
                                "%s recvd:data size - no corresponding request found for local fd %d",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), local_fd);
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return;
        }
        /* get the size */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &i64, &cnt, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(dfs);
            return;
        }
        /* pass them back to the original caller */
        if (NULL != dfs->read_cbfunc) {
            dfs->size_cbfunc(i64, dfs->cbdata);
        }
        /* release the request */
        OBJ_RELEASE(dfs);
        break;

    case ORTE_DFS_READ_CMD:
        /* unpack the request id for this read */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &rid, &cnt, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* search our list of requests to find the matching one */
        dfs = NULL;
        for (item = opal_list_get_first(&requests);
             item != opal_list_get_end(&requests);
             item = opal_list_get_next(item)) {
            dptr = (orte_dfs_request_t*)item;
            if (dptr->id == rid) {
                /* request was fulfilled, so remove it */
                opal_list_remove_item(&requests, item);
                dfs = dptr;
                break;
            }
        }
        if (NULL == dfs) {
            opal_output_verbose(1, orte_dfs_base.output,
                                "%s recvd:data read - no corresponding request found for local fd %d",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), local_fd);
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return;
        }
        /* get the bytes read */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &i64, &cnt, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(dfs);
            return;
        }
        if (0 < i64) {
            cnt = i64;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, dfs->read_buffer, &cnt, OPAL_UINT8))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(dfs);
                return;
            }
        }
        /* pass them back to the original caller */
        if (NULL != dfs->read_cbfunc) {
            dfs->read_cbfunc(i64, dfs->read_buffer, dfs->cbdata);
        }
        /* release the request */
        OBJ_RELEASE(dfs);
        break;

    default:
        opal_output(0, "ORTED:DFS:RECV:DATA WTF");
        break;
    }
}
