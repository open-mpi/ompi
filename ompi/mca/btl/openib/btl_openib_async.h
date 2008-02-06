/*
 * Copyright (c) 2007 Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * @file
 */

#ifndef MCA_BTL_OPENIB_ASYNC_H
#define MCA_BTL_OPENIB_ASYNC_H

void*      btl_openib_async_thread(void *one_hca);
void       mca_btl_openib_load_apm(struct ibv_qp *qp, struct mca_btl_openib_module_t *btl);
void       mca_btl_openib_load_apm_xrc_rcv(uint32_t qp_num, struct mca_btl_openib_module_t *btl);
#endif
