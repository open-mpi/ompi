/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * In order to add a new QP you need to do next steps:
 *
 *    1) Add new index to enum with list of the all QPs,
 *           MCA_BCOL_IBOFFLOAD_QP_NEW_QP e.g.
 *
 *    2) In the setup_qps_fn array init MCA_BCOL_IBOFFLOAD_QP_NEW_QP
 *        index with your init func for this QP.
 *
 *    3) In the init func you added init the next func pointers:
 *       a) config_qp - in this func you need to fill in ibv_qp_init_attr
 *                      structure will be used for this QP creation.
 *
 *       b) prepost_recv - you have to specify this poiner if you want
 *                         automatically executed preposting to your new QP.
 *
 *       c) alloc_resource - will be called during device activation,
 *                           if you need any device resource (list of frags for example)
 *                           for your new QP here the right place to allocate it.
 *
 *       d) dealloc_resource - if any resource was allocated dynamically
 *                             by alloc_resource func destruct it in this func.
 *
 *       e) get_preposted_recv - the function returns preposted recieve for 'wait task'.
 *
 *       d) If you don't need any of these funcs you have to init appropriate pointer with NULL.
 */

#ifndef MCA_BCOL_IBOFFLOAD_QP_INFO_H
#define MCA_BCOL_IBOFFLOAD_QP_INFO_H

#include "ompi_config.h"

BEGIN_C_DECLS

/* forward declarations */
struct mca_bcol_iboffload_device_t;
struct mca_bcol_iboffload_collreq_t;
struct mca_bcol_iboffload_qp_info_t;
struct mca_bcol_iboffload_endpoint_t;

/* The list of the all required QPs */
enum {
    MCA_BCOL_IBOFFLOAD_QP_BARRIER,
    MCA_BCOL_IBOFFLOAD_QP_REGULAR,
    MCA_BCOL_IBOFFLOAD_QP_SYNC,
    MCA_BCOL_IBOFFLOAD_QP_CREDIT,
    MCA_BCOL_IBOFFLOAD_QP_LARGE_BUFF,
    MCA_BCOL_IBOFFLOAD_QP_LAST
};

typedef enum {
    MCA_BCOL_IBOFFLOAD_PP_QP,
    MCA_BCOL_IBOFFLOAD_SRQ_QP,
    MCA_BCOL_IBOFFLOAD_XRC_QP
} mca_bcol_iboffload_qp_type_t;

struct mca_bcol_iboffload_pp_qp_info_t {
    int32_t rd_win;
    int32_t rd_rsv;
}; typedef struct mca_bcol_iboffload_pp_qp_info_t mca_bcol_iboffload_pp_qp_info_t;

struct mca_bcol_iboffload_srq_qp_info_t {
    int32_t sd_max;
}; typedef struct mca_bcol_iboffload_srq_qp_info_t mca_bcol_iboffload_srq_qp_info_t;

typedef int (*mca_bcol_iboffload_setup_qps_fn_t) (struct mca_bcol_iboffload_qp_info_t*);
typedef int (*mca_bcol_iboffload_prepost_qps_fn_t)
                        (struct mca_bcol_iboffload_endpoint_t *endpoint,
                         int qp_index, int num_to_prepost);

typedef void (*mca_bcol_iboffload_config_qps_fn_t)
                        (int qp_index,
                         struct mca_bcol_iboffload_endpoint_t *ep,
                         ompi_common_ofacm_base_qp_config_t *qp_config);

typedef int (*mca_bcol_iboffload_alloc_qps_resource_fn_t)
                               (int qp_index,
                                struct mca_bcol_iboffload_device_t *device);

typedef int (*mca_bcol_iboffload_dealloc_qps_resource_fn_t)
                               (int qp_index,
                                struct mca_bcol_iboffload_device_t *device);

typedef struct mca_bcol_iboffload_frag_t* (*mca_bcol_iboffload_get_preposted_recv_fn_t)
                         (struct mca_bcol_iboffload_endpoint_t *ep, int qp_index);

struct mca_bcol_iboffload_qp_info_t {
    size_t size;

    int32_t rd_num;
    int32_t rd_low;
    int32_t rd_pp_win; /* prepost window = rd_num - rd_low */
    int qp_index;

    mca_bcol_iboffload_qp_type_t type;

    mca_bcol_iboffload_config_qps_fn_t config_qp;
    mca_bcol_iboffload_prepost_qps_fn_t prepost_recv;

    mca_bcol_iboffload_alloc_qps_resource_fn_t alloc_resource;
    mca_bcol_iboffload_dealloc_qps_resource_fn_t dealloc_resource;

    mca_bcol_iboffload_get_preposted_recv_fn_t get_preposted_recv;

    union {
        mca_bcol_iboffload_pp_qp_info_t pp_qp;
        mca_bcol_iboffload_srq_qp_info_t srq_qp;
    } u;
}; typedef struct mca_bcol_iboffload_qp_info_t mca_bcol_iboffload_qp_info_t;

extern mca_bcol_iboffload_setup_qps_fn_t setup_qps_fn[MCA_BCOL_IBOFFLOAD_QP_LAST];

END_C_DECLS

#endif /* MCA_BCOL_IBOFFLOAD_QP_INFO_H */

