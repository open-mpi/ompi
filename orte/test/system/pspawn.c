/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <spawn.h>

#include "opal/util/argv.h"

int main(int argc, char* argv[])
{
    int rc;
    char **pargv = NULL;
    pid_t pid;
    posix_spawn_file_actions_t factions;
    posix_spawnattr_t attrs;

    rc = posix_spawnattr_init(&attrs);
    if (0 != rc) {
        fprintf(stderr, "ERROR INIT ATTRS: %d\n", errno);
        exit(1);
    }

    rc = posix_spawn_file_actions_init(&factions);
    if (0 != rc) {
        fprintf(stderr, "ERROR INIT FACTIONS: %d\n", errno);
        exit(1);
    }
    posix_spawn_file_actions_addclose(&factions, fileno(stdin));

    opal_argv_append_nosize(&pargv, "hostname");

    rc = posix_spawn(&pid, "/usr/bin/hostname", NULL, NULL, pargv, NULL);
    posix_spawn_file_actions_destroy(&factions);
    posix_spawnattr_destroy(&attrs);

    sleep(1);
    return 0;
}
