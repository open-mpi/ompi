/*
 * Copyright (c) 2007 Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <infiniband/verbs.h> 
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

#include "opal/util/output.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "btl_openib_xrc.h"
#include "btl_openib.h"

#if HAVE_XRC
#define SIZE_OF2(A,B) (sizeof(A) + sizeof(B))

static void ib_address_constructor(ib_address_t *ib_addr);
static void ib_address_destructor(ib_address_t *ib_addr);

OBJ_CLASS_INSTANCE(ib_address_t,
                   opal_list_item_t,
                   ib_address_constructor,
                   ib_address_destructor);

/* This func. opens XRC domain */
int mca_btl_openib_open_xrc_domain(struct mca_btl_openib_hca_t *hca)
{
    int len;
    char *xrc_file_name;
    const char *dev_name;

    dev_name = ibv_get_device_name(hca->ib_dev);
    len = asprintf(&xrc_file_name,
            "%s"OPAL_PATH_SEP"openib_xrc_domain_%s",
            orte_process_info.job_session_dir, dev_name);
    if (0 > len) {
        BTL_ERROR(("Failed to allocate memomry for XRC file name\n",
                strerror(errno)));
        return OMPI_ERROR;
    }
    
    hca->xrc_fd = open(xrc_file_name, O_CREAT, S_IWUSR|S_IRUSR);
    if (0 > hca->xrc_fd) {
        BTL_ERROR(("Failed to open XRC domain file %s, errno says %s\n",
                xrc_file_name,strerror(errno)));
        free(xrc_file_name);
        return OMPI_ERROR;
    }
   
    hca->xrc_domain = ibv_open_xrc_domain(hca->ib_dev_context, hca->xrc_fd, O_CREAT);
    if (NULL == hca->xrc_domain) {
        BTL_ERROR(("Failed to open XRC domain\n"));
        close(hca->xrc_fd);
        free(xrc_file_name);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

/* This func. closes XRC domain */
int mca_btl_openib_close_xrc_domain(struct mca_btl_openib_hca_t *hca)
{
    if (ibv_close_xrc_domain(hca->xrc_domain)) {
        BTL_ERROR(("Failed to close XRC domain, errno says %s\n",
                    hca->xrc_fd, strerror(errno)));
        return OMPI_ERROR;
    }
    /* do we need to check exit status */
    if (close(hca->xrc_fd)) {
        BTL_ERROR(("Failed to close XRC file descriptor %s, errno says %s\n",
                hca->xrc_fd, strerror(errno)));
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
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

static int ib_address_init(ib_address_t *ib_addr, uint64_t s_id, uint16_t lid)
{
    ib_addr->key = malloc(SIZE_OF2(s_id,lid));
    if (NULL == ib_addr->key) {
        BTL_ERROR(("Failed to allocate memory for key\n"));
        return OMPI_ERROR;
    }

    memset(ib_addr->key, 0, SIZE_OF2(s_id,lid));
    /* creating the key */
    memcpy(ib_addr->key, &lid, sizeof(lid));
    memcpy((void*)((char*)ib_addr->key + sizeof(lid)), &s_id, sizeof(s_id));
    /* caching lid and subnet id */
    ib_addr->subnet_id = s_id;
    ib_addr->lid = lid;

    return OMPI_SUCCESS;
}

/* Create new entry in hash table for subnet_id and lid,
 * update the endpoint pointer. 
 * Before call to this function you need to protect with
 */
int mca_btl_openib_ib_address_add_new (uint64_t s_id, uint16_t lid, mca_btl_openib_endpoint_t *ep)
{
    void *tmp;
    int ret = OMPI_SUCCESS;
    struct ib_address_t *ib_addr = OBJ_NEW(ib_address_t);

    ret = ib_address_init(ib_addr, s_id, lid);
    if (OMPI_SUCCESS != ret ) {
        BTL_ERROR(("XRC Internal error. Failed to init ib_addr\n"));
        OBJ_DESTRUCT(ib_addr);
        return ret;
    }
    /* is it already in the table ?*/
    OPAL_THREAD_LOCK(&mca_btl_openib_component.ib_lock);
    if (OPAL_SUCCESS != opal_hash_table_get_value_ptr(&mca_btl_openib_component.ib_addr_table,
                ib_addr->key, 
                SIZE_OF2(s_id,lid), &tmp)) {
        /* It is new one, lets put it on the table */
        ret = opal_hash_table_set_value_ptr(&mca_btl_openib_component.ib_addr_table,
                ib_addr->key, SIZE_OF2(s_id,lid), (void*)ib_addr);
        if (OPAL_SUCCESS != ret) {
            BTL_ERROR(("XRC Internal error."
                        " Failed to add element to mca_btl_openib_component.ib_addr_table\n"));
            OPAL_THREAD_UNLOCK(&mca_btl_openib_component.ib_lock);
            OBJ_DESTRUCT(ib_addr);
            return ret;
        }
        /* opal_list_append(&mca_btl_openib_component.ib_addr_list,(opal_list_item_t*)ib_addr); */
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
