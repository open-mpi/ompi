/*
 * Copyright (c) 2007-2008 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Bull SAS.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <infiniband/verbs.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <dlfcn.h>

#include "opal/mca/btl/base/base.h"
#include "btl_openib_xrc.h"
#include "btl_openib.h"

#if HAVE_XRC
#define SIZE_OF3(A, B, C) (sizeof(A) + sizeof(B) + sizeof(C))

static void ib_address_constructor(ib_address_t *ib_addr);
static void ib_address_destructor(ib_address_t *ib_addr);

OBJ_CLASS_INSTANCE(ib_address_t,
                   opal_list_item_t,
                   ib_address_constructor,
                   ib_address_destructor);

/* run-time check for which libibverbs XRC API we really have underneath */
bool mca_btl_openib_xrc_check_api()
{
    void *lib = dlopen(NULL, RTLD_NOW); /* current program */
    if (!lib) {
        BTL_ERROR(("XRC error: could not find XRC API version"));
        return false;
    }

#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
    if (NULL != dlsym(lib, "ibv_open_xrcd")) {
        BTL_ERROR(("XRC error: bad XRC API (require XRC from OFED 3.12+)"));
        return false;
    }
#else
    if (NULL != dlsym(lib, "ibv_create_xrc_rcv_qp")) {
        BTL_ERROR(("XRC error: bad XRC API (require XRC from OFED pre 3.12)."));
        return false;
    }
#endif
    return true;
}

/* This func. opens XRC domain */
int mca_btl_openib_open_xrc_domain(struct mca_btl_openib_device_t *device)
{
    int len;
    char *xrc_file_name;
    const char *dev_name;
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
    struct ibv_xrcd_init_attr xrcd_attr;
#endif

    dev_name = ibv_get_device_name(device->ib_dev);
    len = asprintf(&xrc_file_name,
            "%s"OPAL_PATH_SEP"openib_xrc_domain_%s",
            opal_process_info.job_session_dir, dev_name);
    if (0 > len) {
        BTL_ERROR(("Failed to allocate memomry for XRC file name: %s\n",
                   strerror(errno)));
        return OPAL_ERROR;
    }

    device->xrc_fd = open(xrc_file_name, O_CREAT, S_IWUSR|S_IRUSR);
    if (0 > device->xrc_fd) {
        BTL_ERROR(("Failed to open XRC domain file %s, errno says %s\n",
                xrc_file_name,strerror(errno)));
        free(xrc_file_name);
        return OPAL_ERROR;
    }
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
    memset(&xrcd_attr, 0, sizeof xrcd_attr);
    xrcd_attr.comp_mask = IBV_XRCD_INIT_ATTR_FD | IBV_XRCD_INIT_ATTR_OFLAGS;
    xrcd_attr.fd = device->xrc_fd;
    xrcd_attr.oflags = O_CREAT;
    device->xrcd = ibv_open_xrcd(device->ib_dev_context, &xrcd_attr);
    if (NULL == device->xrcd) {
#else
    device->xrc_domain = ibv_open_xrc_domain(device->ib_dev_context, device->xrc_fd, O_CREAT);
    if (NULL == device->xrc_domain) {
#endif
        BTL_ERROR(("Failed to open XRC domain\n"));
        close(device->xrc_fd);
        free(xrc_file_name);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

/* This func. closes XRC domain */
int mca_btl_openib_close_xrc_domain(struct mca_btl_openib_device_t *device)
{
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
    if (NULL == device->xrcd) {
#else
    if (NULL == device->xrc_domain) {
#endif
        /* No XRC domain, just exit */
        return OPAL_SUCCESS;
    }
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
    if (ibv_close_xrcd(device->xrcd)) {
#else
    if (ibv_close_xrc_domain(device->xrc_domain)) {
#endif
        BTL_ERROR(("Failed to close XRC domain, errno %d says %s\n",
                    device->xrc_fd, strerror(errno)));
        return OPAL_ERROR;
    }
    /* do we need to check exit status */
    if (close(device->xrc_fd)) {
        BTL_ERROR(("Failed to close XRC file descriptor, errno %d says %s\n",
                device->xrc_fd, strerror(errno)));
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static void ib_address_constructor(ib_address_t *ib_addr)
{
    ib_addr->key = NULL;
    ib_addr->subnet_id = 0;
    ib_addr->lid = 0;
    ib_addr->status = MCA_BTL_IB_ADDR_CLOSED;
    ib_addr->qp = NULL;
    OBJ_CONSTRUCT(&ib_addr->addr_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&ib_addr->pending_ep, opal_list_t);
}

static void ib_address_destructor(ib_address_t *ib_addr)
{
    if (NULL != ib_addr->key) {
        free(ib_addr->key);
    }
    OBJ_DESTRUCT(&ib_addr->addr_lock);
    OBJ_DESTRUCT(&ib_addr->pending_ep);
}

static int ib_address_init(ib_address_t *ib_addr, uint16_t lid, uint64_t s_id, opal_jobid_t ep_jobid)
{
    ib_addr->key = malloc(SIZE_OF3(s_id, lid, ep_jobid));
    if (NULL == ib_addr->key) {
        BTL_ERROR(("Failed to allocate memory for key\n"));
        return OPAL_ERROR;
    }
    memset(ib_addr->key, 0, SIZE_OF3(s_id, lid, ep_jobid));
    /* creating the key = lid + s_id + ep_jobid */
    memcpy(ib_addr->key, &lid, sizeof(lid));
    memcpy((void*)((char*)ib_addr->key + sizeof(lid)), &s_id, sizeof(s_id));
    memcpy((void*)((char*)ib_addr->key + sizeof(lid) + sizeof(s_id)),
            &ep_jobid, sizeof(ep_jobid));
    /* caching lid and subnet id */
    ib_addr->subnet_id = s_id;
    ib_addr->lid = lid;

    return OPAL_SUCCESS;
}

/* Create new entry in hash table for subnet_id and lid,
 * update the endpoint pointer.
 * Before call to this function you need to protect with
 */
int mca_btl_openib_ib_address_add_new (uint16_t lid, uint64_t s_id,
        opal_jobid_t ep_jobid, mca_btl_openib_endpoint_t *ep)
{
    void *tmp;
    int ret = OPAL_SUCCESS;
    struct ib_address_t *ib_addr = OBJ_NEW(ib_address_t);

    ret = ib_address_init(ib_addr, lid, s_id, ep_jobid);
    if (OPAL_SUCCESS != ret ) {
        BTL_ERROR(("XRC Internal error. Failed to init ib_addr\n"));
        OBJ_DESTRUCT(ib_addr);
        return ret;
    }
    /* is it already in the table ?*/
    OPAL_THREAD_LOCK(&mca_btl_openib_component.ib_lock);
    if (OPAL_SUCCESS != opal_hash_table_get_value_ptr(&mca_btl_openib_component.ib_addr_table,
                ib_addr->key,
                SIZE_OF3(s_id, lid, ep_jobid), &tmp)) {
        /* It is new one, lets put it on the table */
        ret = opal_hash_table_set_value_ptr(&mca_btl_openib_component.ib_addr_table,
                ib_addr->key, SIZE_OF3(s_id, lid, ep_jobid), (void*)ib_addr);
        if (OPAL_SUCCESS != ret) {
            BTL_ERROR(("XRC Internal error."
                        " Failed to add element to mca_btl_openib_component.ib_addr_table\n"));
            OPAL_THREAD_UNLOCK(&mca_btl_openib_component.ib_lock);
            OBJ_DESTRUCT(ib_addr);
            return ret;
        }
        /* update the endpoint with pointer to ib address */
        ep->ib_addr = ib_addr;
    } else {
        /* so we have this one in the table, just add the pointer to the endpoint */
        ep->ib_addr = (ib_address_t *)tmp;
        assert(lid == ep->ib_addr->lid && s_id == ep->ib_addr->subnet_id);
        OBJ_DESTRUCT(ib_addr);
    }
    OPAL_THREAD_UNLOCK(&mca_btl_openib_component.ib_lock);

    return ret;
}
#endif
