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

#include "orte/mca/dfs/dfs.h"

static bool active;
static bool read_active;
static int numread = 0;

#define READ_SIZE 1000

static void dfs_open_cbfunc(int fd, void *cbdata)
{
    int *remote_fd = (int*)cbdata;

    opal_output(0, "GOT FD %d", fd);
    *remote_fd = fd;
    active = false;

}

static void dfs_size_cbfunc(long size, void *cbdata)
{
    opal_output(0, "GOT FILE SIZE %ld", size);
    active = false;

}

static void read_cbfunc(long status, uint8_t *buffer, void *cbdata)
{
    opal_output(0, "GOT READ STATUS %d", (int)status);
    if (status < 0) {
        read_active = false;
        active = false;
        return;
    }
    numread += status;

    if (status < READ_SIZE) {
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
    char *uri, *host;
    uint8_t buffer[READ_SIZE];

    /* user must provide a file to be read - the contents
     * of the file will be output to stdout
     */
    if (1 == argc) {
        fprintf(stderr, "Usage: orte_dfs <input-file> <host [optional]\n");
        exit(1);
    }

    if (0 != (rc = orte_init(&argc, &argv, ORTE_PROC_NON_MPI))) {
        fprintf(stderr, "orte_db: couldn't init orte - error code %d\n", rc);
        return rc;
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
    while (active) {
        opal_event_loop(orte_event_base, OPAL_EVLOOP_ONCE);
    }

    active = true;
    orte_dfs.get_file_size(fd, dfs_size_cbfunc, NULL);
    while (active) {
        opal_event_loop(orte_event_base, OPAL_EVLOOP_ONCE);
    }

    active = true;
    read_active = true;
    rc = 0;
    numread = 0;
    while (read_active) {
        fprintf(stderr, "reading next %d bytes\n", READ_SIZE);
        orte_dfs.read(fd, buffer, READ_SIZE, read_cbfunc, NULL);
        while (active) {
            opal_event_loop(orte_event_base, OPAL_EVLOOP_ONCE);
        }
        rc++;
        if (2 == rc) {
            orte_dfs.seek(fd, 326, SEEK_SET);
        }
        active = true;
    }

    orte_dfs.close(fd);
    opal_event_loop(orte_event_base, OPAL_EVLOOP_ONCE);

    orte_finalize();
    return 0;
}
