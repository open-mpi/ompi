/*
 * Copyright (c) 2014, Cisco Systems, Inc. All rights reserved.
 *
 * LICENSE_BEGIN
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * BSD license below:
 *
 *     Redistribution and use in source and binary forms, with or
 *     without modification, are permitted provided that the following
 *     conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * LICENSE_END
 *
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <dirent.h>
#include <pthread.h>
#include <errno.h>
#include <sys/stat.h>

#include <infiniband/driver.h>

#include "usd.h"
#include "usd_ib_sysfs.h"
#include "usd_util.h"

/*
 * usnic_direct routines that depend on Infiniband /sysfs directory structure
 */

/*
 * Perform one-time initialization
 */
int
usd_ib_get_devlist(
    struct usd_ib_dev **dev_list)
{
    char *class_path = "/sys/class/infiniband_verbs";
    DIR *class_dir;
    struct dirent *dent;
    struct stat sbuf;
    char dev_path[PATH_MAX];
    char ibdev_path[PATH_MAX];
    char ibdev_buf[32];
    struct usd_ib_dev *idp;
    struct usd_ib_dev *last_idp;
    int fd;
    int rc;
    int n;

    /*
     * For now, we are glomming onto Infiniband driver for setup
     */
    class_dir = opendir(class_path);
    if (class_dir == NULL) {
        return -ENODEV;
    }

    /* Check dir entries for USNIC devices */
    last_idp = NULL;
    fd = -1;
    while ((dent = readdir(class_dir)) != NULL) {

        /* skip "." and ".." */
        if (dent->d_name[0] == '.')
            continue;

        /* build path to entry */
        snprintf(dev_path, sizeof(dev_path), "%s/%s", class_path,
                 dent->d_name);

        /* see if it's a dir */
        rc = stat(dev_path, &sbuf);
        if (rc != 0) {
            usd_perror(dev_path);
            rc = -errno;
            goto out;
        }

        /* Must be a directory */
        if (!S_ISDIR(sbuf.st_mode))
            continue;

        /* read the ibdev */
        snprintf(ibdev_path, sizeof(ibdev_path), "%s/ibdev", dev_path);
        fd = open(ibdev_path, O_RDONLY);
        if (fd == -1) {
            usd_perror(ibdev_path);
            rc = -errno;
            goto out;
        }
        n = read(fd, ibdev_buf, sizeof(ibdev_buf));
        if (n == -1) {
            usd_perror("reading ibdev");
            rc = -errno;
            goto out;
        }
        close(fd);
        fd = -1;
        if (n > 0 && ibdev_buf[n - 1] == '\n') {
            ibdev_buf[n - 1] = '\0';       /* newline -> EOF */
        }

        /* If USNIC device, remember this one */
        if (strncmp(ibdev_buf, "usnic", 5) == 0) {
            idp = calloc(sizeof(*idp), 1);
            if (idp == NULL) {
                usd_perror("calloc IB device");
                rc = -errno;
                goto out;
            }
            strncpy(idp->id_name, dent->d_name, sizeof(idp->id_name));
            strncpy(idp->id_usnic_name, ibdev_buf,
                    sizeof(idp->id_usnic_name));
            snprintf(idp->id_dev_path, sizeof(idp->id_dev_path),
                     "/dev/infiniband/%s", idp->id_name);
            snprintf(idp->id_class_path, sizeof(idp->id_class_path),
                     "%s/device/infiniband/%s", dev_path, ibdev_buf);

            if (last_idp == NULL) {
                *dev_list = idp;
            } else {
                last_idp->id_next = idp;
            }
            idp->id_next = NULL;
            last_idp = idp;
        }
    }
    rc = 0;

out:
    /* clean up */
    if (class_dir != NULL) {
        closedir(class_dir);
    }
    if (fd != -1) {
        close(fd);
    }

    return rc;
}

/*
 * Find MAC for a device
 * (we assume port 0)
 */
int
usd_get_mac(
    struct usd_device *dev,
    uint8_t * mac)
{
    char name[128];
    char gid[80];
    char *p;
    uint16_t v;
    struct usd_ib_dev *idp;
    int fd;
    int n;

    idp = dev->ud_ib_dev;
    snprintf(name, sizeof(name), "%s/ports/1/gids/0", idp->id_class_path);

    fd = open(name, O_RDONLY);
    if (fd == -1) {
        usd_perror(name);
        return -errno;
    }

    n = read(fd, gid, sizeof(gid) - 1);
    close(fd);
    if (n < 0) {
        usd_perror("reading GID");
        return -errno;
    }
    gid[n] = '\0';

    p = gid + 20;
    sscanf(p, "%hx", &v);
    *mac++ = (v >> 8) ^ 2;
    *mac++ = v & 0xFF;
    p += 5;
    sscanf(p, "%hx", &v);
    *mac++ = v >> 8;
    p += 5;
    sscanf(p, "%hx", &v);
    *mac++ = v & 0xFF;
    p += 5;
    sscanf(p, "%hx", &v);
    *mac++ = v >> 8;
    *mac++ = v & 0xFF;

    return 0;
}

/*
 * Find interface for a device
 */
int
usd_get_iface(
    struct usd_device *dev)
{
    char name[128];
    struct usd_ib_dev *idp;
    int fd;
    int n;

    idp = dev->ud_ib_dev;
    snprintf(name, sizeof(name), "%s/iface", idp->id_class_path);

    fd = open(name, O_RDONLY);
    if (fd == -1) {
        usd_perror(name);
        dev->ud_attrs.uda_ifname[0] = '\0';
        return -errno;
    }

    n = read(fd, dev->ud_attrs.uda_ifname,
             sizeof(dev->ud_attrs.uda_ifname));
    close(fd);
    if (n < 0) {
        usd_perror("reading iface");
        return -errno;
    }

    /* allow for trailing newline */
    if (dev->ud_attrs.uda_ifname[n - 1] == '\n')
        dev->ud_attrs.uda_ifname[n - 1] = '\0';
    else
        dev->ud_attrs.uda_ifname[n] = '\0';

    return 0;
}

/*
 * Read an integer from a sysfs entry
 */
static int
usd_ib_sysfs_get_int(
    struct usd_device *dev,
    char *entry,
    int *result)
{
    char name[128];
    char buf[32];
    struct usd_ib_dev *idp;
    int fd;
    int n;

    idp = dev->ud_ib_dev;
    snprintf(name, sizeof(name), "%s/%s", idp->id_class_path, entry);

    fd = open(name, O_RDONLY);
    if (fd == -1) {
        usd_perror(name);
        return -errno;
    }

    n = read(fd, buf, sizeof(buf));
    close(fd);
    if (n < 0) {
        fprintf(stderr, "Error %d reading %s\n", errno, entry);
        return -errno;
    }

    *result = atoi(buf);
    return 0;
}

/*
 * Get usNIC configuration
 */
int
usd_get_usnic_config(
    struct usd_device *dev)
{
    int v;
    int ret;

    ret = usd_ib_sysfs_get_int(dev, "max_vf", &v);
    if (ret != 0)
        return ret;
    dev->ud_attrs.uda_num_vf = v;

    ret = usd_ib_sysfs_get_int(dev, "qp_per_vf", &v);
    if (ret != 0)
        return ret;
    dev->ud_attrs.uda_qp_per_vf = v;

    ret = usd_ib_sysfs_get_int(dev, "cq_per_vf", &v);
    if (ret != 0)
        return ret;
    dev->ud_attrs.uda_cq_per_vf = v;

    return ret;
}

/*
 * Find firmware version
 */
int
usd_get_firmware(
    struct usd_device *dev)
{
    char name[128];
    struct usd_ib_dev *idp;
    char *fw;
    int fd;
    int n;

    idp = dev->ud_ib_dev;
    snprintf(name, sizeof(name), "%s/fw_ver", idp->id_class_path);

    fd = open(name, O_RDONLY);
    if (fd == -1) {
        usd_perror(name);
        return -errno;
    }

    fw = &dev->ud_attrs.uda_firmware[0];
    n = read(fd, fw, sizeof(dev->ud_attrs.uda_firmware));
    close(fd);
    if (n < 0) {
        usd_perror("reading fw_ver");
        return -errno;
    }
    /* allow for trailing newline */
    if (fw[n - 1] == '\n')
        fw[n - 1] = '\0';
    else
        fw[n] = '\0';

    return 0;
}
