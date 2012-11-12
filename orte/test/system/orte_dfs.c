/* -*- C -*-
 *
 * $HEADER$
 *
 */
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>

#include "opal/util/output.h"
#include "opal/util/uri.h"
#include "opal/mca/event/event.h"

#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/dfs/dfs.h"

static bool active;
static bool read_active;
static int numread = 0;

#define READ_SIZE 500
#define OFFSET_VALUE   313

static void dfs_open_cbfunc(int fd, void *cbdata)
{
    int *remote_fd = (int*)cbdata;

    opal_output(0, "GOT FD %d", fd);
    *remote_fd = fd;
    active = false;

}

static void dfs_close_cbfunc(int fd, void *cbdata)
{
    opal_output(0, "CLOSE CONFIRMED");
    active = false;
}

static void dfs_size_cbfunc(long size, void *cbdata)
{
    opal_output(0, "GOT FILE SIZE %ld", size);
    active = false;

}

static void dfs_seek_cbfunc(long offset, void *cbdata)
{
    int *check = (int*)cbdata;

    opal_output(0, "GOT FILE OFFSET %ld", offset);
    active = false;
    if (NULL != cbdata && offset != *check) {
        exit(1);
    }
}

static void dfs_post_cbfunc(void *cbdata)
{
    opal_buffer_t *bo = (opal_buffer_t*)cbdata;

    opal_output(0, "GOT POST CALLBACK");
    active = false;
    OBJ_RELEASE(bo);
}

static void dfs_getfm_cbfunc(opal_buffer_t *bo, void *cbdata)
{
    opal_buffer_t *bptr = (opal_buffer_t*)cbdata;

    opal_output(0, "GOT GETFM CALLBACK");
    active = false;
    opal_dss.copy_payload(bptr, bo);
}

static void read_cbfunc(long status, uint8_t *buffer, void *cbdata)
{
    int *check = (int*)cbdata;

    if (status < 0) {
        read_active = false;
        active = false;
        return;
    }
    numread += status;

    if (NULL != cbdata && status < *check) {
        read_active = false;
        opal_output(0, "EOF RECEIVED: read total of %d bytes", numread);
        active = false;
        return;
    }
    active = false;
}

int main(int argc, char* argv[])
{
    int rc;
    int fd;
    char *uri, *host, *path;
    uint8_t buffer[READ_SIZE];
    opal_buffer_t *buf, *xfer;
    int i, k, cnt;
    int64_t i64, length, offset, partition;
    int32_t n, nvpids, nentries;
    orte_vpid_t vpid;

    if (0 != (rc = orte_init(&argc, &argv, ORTE_PROC_NON_MPI))) {
        fprintf(stderr, "orte_dfs: couldn't init orte - error code %d\n", rc);
        return rc;
    }
    
    /* if I am part of an initial job, then test my basic
     * API operations
     */
    if (1 == ORTE_LOCAL_JOBID(ORTE_PROC_MY_NAME->jobid)) {
        /* user must provide a file to be read - the contents
         * of the file will be output to stdout
         */
        if (1 == argc) {
            fprintf(stderr, "Usage: orte_dfs <input-file> <host [optional]\n");
            orte_finalize();
            return 1;
        }
        if (3 == argc) {
            host = strdup(argv[2]);
        } else {
            host = NULL;
        }

        if (NULL == (uri = opal_filename_to_uri(argv[1], host))) {
            return 1;
        }

        active = true;
        orte_dfs.open(uri, dfs_open_cbfunc, &fd);
        ORTE_WAIT_FOR_COMPLETION(active);

        if (fd < 0) {
            /* hit an error */
            return 1;
        }

        active = true;
        orte_dfs.get_file_size(fd, dfs_size_cbfunc, NULL);
        ORTE_WAIT_FOR_COMPLETION(active);

        active = true;
        read_active = true;
        rc = 0;
        numread = 0;
        while (read_active) {
            i = READ_SIZE;
            active = true;
            orte_dfs.read(fd, buffer, READ_SIZE, read_cbfunc, &i);
            ORTE_WAIT_FOR_COMPLETION(active);
            rc++;
            if (2 == rc) {
                active = true;
                i = OFFSET_VALUE;
                opal_output(0, "%s execute absolute seek of %d bytes\n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), OFFSET_VALUE);
                orte_dfs.seek(fd, OFFSET_VALUE, SEEK_SET, dfs_seek_cbfunc, &i);
                ORTE_WAIT_FOR_COMPLETION(active);
            }
            if (5 == rc) {
                active = true;
                i = OFFSET_VALUE;
                opal_output(0, "%s execute relative seek of %d bytes\n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), OFFSET_VALUE);
                orte_dfs.seek(fd, OFFSET_VALUE, SEEK_CUR, dfs_seek_cbfunc, &i);
                ORTE_WAIT_FOR_COMPLETION(active);
            }
            active = true;
        }

        active= true;
        orte_dfs.close(fd, dfs_close_cbfunc, NULL);
        ORTE_WAIT_FOR_COMPLETION(active);

        /* construct a file map to pass to our successor */
        for (i=0; i < 10; i++) {
            buf = OBJ_NEW(opal_buffer_t);
            opal_dss.pack(buf, &host, 1, OPAL_STRING);
            opal_dss.pack(buf, &argv[1], 1, OPAL_STRING);
            i64 = 100; /* assign 100 bytes to this partition */
            opal_dss.pack(buf, &i64, 1, OPAL_INT64);
            i64 = i * 100;  /* space things out */
            opal_dss.pack(buf, &i64, 1, OPAL_INT64);
            i64 = i;  /* set the partition */
            opal_dss.pack(buf, &i64, 1, OPAL_INT64);
            active = true;
            orte_dfs.post_file_map(buf, dfs_post_cbfunc, buf);
            ORTE_WAIT_FOR_COMPLETION(active);
        }
    } else {
        opal_output(0, "PROC %s REPORTING IN", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        /* retrieve any file maps from our predecessor */
        active = true;
        buf = OBJ_NEW(opal_buffer_t);
        orte_dfs.get_file_map(ORTE_PROC_MY_NAME, dfs_getfm_cbfunc, buf);
        ORTE_WAIT_FOR_COMPLETION(active);

        opal_output(0, "%s RECVD %d BYTES IN FILE MAPS",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)buf->bytes_used);

        /* retrieve the number of vpids in the map */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &nvpids, &cnt, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return 1;
        }

        opal_output(0, "%s RECVD DATA FROM %d VPIDS",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), nvpids);

        /* find a partition for us */
        for (k=0; k < nvpids; k++) {
            cnt = 1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &vpid, &cnt, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                break;
            }
            opal_output(0, "CHECKING VPID %s", ORTE_VPID_PRINT(vpid));
            cnt = 1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &nentries, &cnt, OPAL_INT32))) {
                ORTE_ERROR_LOG(rc);
                break;
            }
            opal_output(0, "%s RECVD %d ENTRIES IN THIS MAP",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), nentries);
            cnt = 1;
            for (i=0; i < nentries; i++) {
                cnt = 1;
                if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &xfer, &cnt, OPAL_BUFFER))) {
                    ORTE_ERROR_LOG(rc);
                    break;
                }
                cnt = 1;
                if (OPAL_SUCCESS != (rc = opal_dss.unpack(xfer, &host, &cnt, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    break;
                }
                cnt = 1;
                if (OPAL_SUCCESS != (rc = opal_dss.unpack(xfer, &path, &cnt, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    break;
                }
                cnt = 1;
                if (OPAL_SUCCESS != (rc = opal_dss.unpack(xfer, &length, &cnt, OPAL_INT64))) {
                    ORTE_ERROR_LOG(rc);
                    break;
                }
                cnt = 1;
                if (OPAL_SUCCESS != (rc = opal_dss.unpack(xfer, &offset, &cnt, OPAL_INT64))) {
                    ORTE_ERROR_LOG(rc);
                    break;
                }
                cnt = 1;
                if (OPAL_SUCCESS != (rc = opal_dss.unpack(xfer, &partition, &cnt, OPAL_INT64))) {
                    ORTE_ERROR_LOG(rc);
                    break;
                }
                OBJ_RELEASE(xfer);
                opal_output(0, "CHECKING PARTITION %d\n\thost %s\n\tpath %s\n\tlength: %d offset: %d",
                            (int)partition, (NULL == host) ? "NULL" : host, path, (int)length, (int)offset);
                continue;
                /* if this is my partition, use the file data */
                if (partition == (int64_t)ORTE_PROC_MY_NAME->vpid) {
                    /* open the file */
                    if (NULL == (uri = opal_filename_to_uri(path, host))) {
                        return 1;
                    }
                    
                    active = true;
                    orte_dfs.open(uri, dfs_open_cbfunc, &fd);
                    ORTE_WAIT_FOR_COMPLETION(active);
                    
                    if (fd < 0) {
                        /* hit an error */
                        return 1;
                    }
                    /* position it */
                    active = true;
                    orte_dfs.seek(fd, offset, SEEK_SET, dfs_seek_cbfunc, NULL);
                    ORTE_WAIT_FOR_COMPLETION(active);
                    /* read it */
                    active = true;
                    numread = 0;
                    orte_dfs.read(fd, buffer, length, read_cbfunc, NULL);
                    ORTE_WAIT_FOR_COMPLETION(active);
                    
                    opal_output(0, "%s successfully read %d bytes",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), numread);
                    active= true;
                    orte_dfs.close(fd, dfs_close_cbfunc, NULL);
                    ORTE_WAIT_FOR_COMPLETION(active);
                    goto complete;
                }
            }
        }
    }

 complete:
    orte_finalize();
    return 0;
}
