/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2009 Sun Microsystems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * Read /proc/<pid>/map and /proc/<pid>/path to find the file mapped
 * into the process over the given address.
 */

#include "opal_config.h"
#include "opal_stdint.h"

#include "opal/constants.h"

#include <procfs.h>
#include <unistd.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

const char *
opal_installdirs_autodetect_path(uintptr_t my_addr)
{
    pid_t my_pid;
    char *map_path;
    prmap_t map;
    int map_fd;
    char *obj_name;
    ssize_t path_size, ls;
    char *path;

    my_pid = getpid();
    asprintf(&map_path, "/proc/%d/map", my_pid);
    if (NULL == map_path) {
        return NULL;
    }
    map_fd = open(map_path, O_RDONLY);
    free(map_path);
    if (map_fd < 0) {
        return NULL;
    }

    for (;;) {
        if (read(map_fd, &map, sizeof(map)) < sizeof(map)) {
            return OPAL_ERR_NOT_FOUND;
        }
        if (map.pr_vaddr <= my_addr && map.pr_vaddr + map.pr_size > my_addr) {
            break;
        }
    }
    close(map_fd);
    asprintf(&obj_name, "/proc/%d/path/%s", my_pid, map.pr_mapname);
    if (NULL == obj_name) {
        return NULL;
    }
    /*
      We don't know how long the path to the load object might be.
      Try allocating a reasonable length.  If the readlink system
      call doesn't use the entire buffer passed to it then we know
      the path is complete.
    */
    for (path_size = 100; ; path_size += 100) {
        path = malloc(path_size);
        if (NULL == path) {
            free(obj_name);
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        ls = readlink(obj_name, path, path_size);
        if (ls < 0) {
            free(obj_name);
            free(path);
            return OPAL_ERR_NOT_FOUND;
        }
        if (ls < path_size) {
            path[ls] = '\0';
            break;
        }
        free(path);
    }

    free(obj_name);

    return path;
}

