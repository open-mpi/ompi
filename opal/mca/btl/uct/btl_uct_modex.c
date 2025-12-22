/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2018-2024 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019-2025 Google, LLC. All rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "btl_uct_modex.h"
#include "btl_uct_types.h"
#include "btl_uct_device_context.h"
#include "opal/class/opal_list.h"
#include "opal/mca/pmix/pmix-internal.h"

static uint16_t mca_btl_uct_tl_modex_size(mca_btl_uct_tl_t *tl)
{
    uint16_t size = sizeof(mca_btl_uct_tl_modex_t);

    if (tl->uct_iface_attr.cap.flags & UCT_IFACE_FLAG_CONNECT_TO_IFACE) {
        size += (uint16_t)tl->uct_iface_attr.iface_addr_len;
    }

    /* pad out to a multiple of 4 bytes */
    return (3 + size + (uint16_t)tl->uct_iface_attr.device_addr_len) & ~3;
}

static uint16_t mca_btl_uct_md_modex_size(mca_btl_uct_md_t *md)
{
    uint16_t modex_size = sizeof(mca_btl_uct_md_modex_t);

    mca_btl_uct_tl_t *tl;
    OPAL_LIST_FOREACH(tl, &md->tls, mca_btl_uct_tl_t) {
      modex_size += mca_btl_uct_tl_modex_size(tl);
    }

    return modex_size;
}

static uint8_t *mca_btl_uct_tl_modex_pack(mca_btl_uct_module_t *module, mca_btl_uct_tl_t *tl,
                                        uint8_t *modex_data)
{
    mca_btl_uct_device_context_t *dev_context =
        mca_btl_uct_module_get_tl_context_specific(module, tl, /*context_id=*/0);

    mca_btl_uct_tl_modex_t *tl_modex = (mca_btl_uct_tl_modex_t *)modex_data;
    tl_modex->size = mca_btl_uct_tl_modex_size(tl);

    memset(tl_modex->tl_name, 0, sizeof(tl_modex->tl_name));
    strncpy(tl_modex->tl_name, tl->uct_tl_name, sizeof(tl_modex->tl_name));

    uint8_t *tl_modex_data = (uint8_t *) tl_modex->data;

    /* NTH: only the first context is available. i assume the device addresses of the
     * contexts will be the same but they will have different iface addresses. i also
     * am assuming that it doesn't really matter if all remote contexts connect to
     * the same endpoint since we are only doing RDMA. if any of these assumptions are
     * wrong then we can't delay creating the other contexts and must include their
     * information in the modex. */
    if (tl->uct_iface_attr.cap.flags & UCT_IFACE_FLAG_CONNECT_TO_IFACE) {
        uct_iface_get_address(dev_context->uct_iface, (uct_iface_addr_t *) tl_modex_data);
        tl_modex_data += tl->uct_iface_attr.iface_addr_len;
    }

    uct_iface_get_device_address(dev_context->uct_iface, (uct_device_addr_t *) tl_modex_data);
    tl_modex_data += tl->uct_iface_attr.device_addr_len;

    return modex_data + tl_modex->size;
}

static uint8_t *mca_btl_uct_modex_pack(mca_btl_uct_md_t *md, uint8_t *modex_data)
{
    mca_btl_uct_module_t *module = NULL;
    for (int i = 0 ; i < mca_btl_uct_component.module_count ; ++i) {
      if (mca_btl_uct_component.modules[i]->md == md) {
        module = mca_btl_uct_component.modules[i];
        break;
      }
    }

    mca_btl_uct_md_modex_t *md_modex = (mca_btl_uct_md_modex_t *)modex_data;
    modex_data = md_modex->data;

    md_modex->size = mca_btl_uct_md_modex_size(md);
    md_modex->module_index = module ? module->module_index : (uint16_t) -1;

    memset(md_modex->md_name, 0, sizeof(md_modex->md_name));
    strncpy(md_modex->md_name, md->md_name, sizeof(md_modex->md_name));

    mca_btl_uct_tl_t *tl;
    OPAL_LIST_FOREACH(tl, &md->tls, mca_btl_uct_tl_t) {
        modex_data = mca_btl_uct_tl_modex_pack(module, tl, modex_data);
    }
    
    return modex_data;
}

int mca_btl_uct_component_modex_send(void)
{
    size_t modex_size = sizeof(mca_btl_uct_modex_t);
    mca_btl_uct_modex_t *modex;
    uint8_t *modex_data;
    int rc;

    mca_btl_uct_md_t *md;
    OPAL_LIST_FOREACH(md, &mca_btl_uct_component.md_list, mca_btl_uct_md_t) {
      modex_size += mca_btl_uct_md_modex_size(md);
    }

    modex = alloca(modex_size);
    modex_data = modex->data;

    modex->module_count = opal_list_get_size(&mca_btl_uct_component.md_list);
    OPAL_LIST_FOREACH(md, &mca_btl_uct_component.md_list, mca_btl_uct_md_t) {
      modex_data = mca_btl_uct_modex_pack(md, modex_data);
    }

    OPAL_MODEX_SEND(rc, PMIX_GLOBAL, &mca_btl_uct_component.super.btl_version, modex, modex_size);
    return rc;
}

static uint8_t *mca_btl_uct_find_tl_modex(mca_btl_uct_md_modex_t *md_modex, mca_btl_uct_tl_t *tl)
{
    uint8_t *modex_data = md_modex->data;

    for (uint16_t modex_offset = 0 ; modex_offset < md_modex->size ; ){
        mca_btl_uct_tl_modex_t *tl_modex = (mca_btl_uct_tl_modex_t *)(modex_data + modex_offset);

        BTL_VERBOSE(("found modex for tl %s searching for %s", tl_modex->tl_name, tl->uct_tl_name));

        if (0 == strcmp(tl->uct_tl_name, tl_modex->tl_name)) {
            return tl_modex->data;
        }

        BTL_VERBOSE(("no match, continuing"));

        modex_offset += tl_modex->size;
    }

    return NULL;
}

uint8_t *mca_btl_uct_find_modex(mca_btl_uct_modex_t *modex, mca_btl_uct_tl_t *tl, int *remote_module_index) {
    uint8_t *modex_data = modex->data;

    /* look for matching transport in the modex */
    for (int i = 0; i < modex->module_count; ++i) {
        mca_btl_uct_md_modex_t *md_modex = (mca_btl_uct_md_modex_t *)modex_data;

        BTL_VERBOSE(("found modex for md %s (remote module index %hu), searching for %s",
                     md_modex->md_name, md_modex->module_index, tl->uct_md->md_name));

        if (0 != strcmp(tl->uct_md->md_name, md_modex->md_name)) {
            /* modex belongs to a different module, skip it and continue */
            modex_data += md_modex->size;
            continue;
        }

        uint8_t *tl_modex = mca_btl_uct_find_tl_modex(md_modex, tl);
        if (NULL == tl_modex) {
            break;
        }

        if (NULL != remote_module_index) {
            *remote_module_index = md_modex->module_index;
        }

        BTL_VERBOSE(("finished processing modex for %s", tl->uct_md->md_name));

        return tl_modex;
    }

    BTL_ERROR(("could not find modex for %s::%s", tl->uct_md->md_name, tl->uct_tl_name));

    return NULL;
}
