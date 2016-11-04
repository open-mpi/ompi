/*
 * Copyright (c) 2013-2015 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Bull SAS.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef COLL_PORTALS4_REQUEST_H
#define COLL_PORTALS4_REQUEST_H

#include "ompi/request/request.h"
#include "coll_portals4.h"


enum ompi_coll_portals4_bcast_algo_t {
    OMPI_COLL_PORTALS4_BCAST_KARY_TREE_ALGO,
    OMPI_COLL_PORTALS4_BCAST_PIPELINE_ALGO,
};
typedef enum ompi_coll_portals4_bcast_algo_t ompi_coll_portals4_bcast_algo_t;


enum ompi_coll_portals4_request_type_t {
    OMPI_COLL_PORTALS4_TYPE_BARRIER,
    OMPI_COLL_PORTALS4_TYPE_BCAST,
    OMPI_COLL_PORTALS4_TYPE_SCATTER,
    OMPI_COLL_PORTALS4_TYPE_GATHER,
    OMPI_COLL_PORTALS4_TYPE_REDUCE,
    OMPI_COLL_PORTALS4_TYPE_ALLREDUCE,
};
typedef enum ompi_coll_portals4_request_type_t ompi_coll_portals4_request_type_t;


struct ompi_coll_portals4_request_t {
    ompi_request_t super;
    ompi_coll_portals4_request_type_t type;
    bool is_sync;

    ompi_request_t **fallback_request;
    union {
        struct {
            ptl_handle_me_t data_me_h;
            ptl_handle_ct_t rtr_ct_h;
        } barrier;

        struct {
            bool needs_pack;
            bool is_root;
            opal_convertor_t convertor;
            void *tmpbuf;
            size_t tmpsize;

            union {
                ptl_rank_t   child;
                unsigned int child_nb;
            } u;
            ompi_coll_portals4_bcast_algo_t algo;
            int segment_nb;
            ptl_handle_ct_t rtr_ct_h;
            ptl_handle_ct_t trig_ct_h;
            ptl_handle_ct_t ack_ct_h;
        } bcast;

        struct {
            bool is_optim;
            bool use_ack_ct_h;
            unsigned int child_nb;
            void *free_buffer;
            ptl_handle_me_t data_me_h;
            ptl_handle_ct_t trig_ct_h;
            ptl_handle_ct_t ack_ct_h;
        } reduce;

        struct {
            bool is_optim;
            unsigned int child_nb;
            ptl_handle_me_t data_me_h;
            ptl_handle_ct_t trig_ct_h;
            ptl_handle_ct_t ack_ct_h;
        } allreduce;

        struct {
            opal_convertor_t        send_converter;
            opal_convertor_t        recv_converter;
            size_t                  packed_size;
    	    int8_t                  is_sync;
            int8_t                  free_after;
            size_t                  coll_count;
    	    char                   *gather_buf;
    	    uint64_t                gather_bytes;
            ptl_match_bits_t        gather_match_bits;
            ptl_handle_md_t         gather_mdh;
            ptl_size_t              gather_offset;
            ptl_handle_ct_t         gather_cth;
            ptl_handle_md_t         gather_meh;
            ptl_match_bits_t        sync_match_bits;
            ptl_handle_md_t         sync_mdh;
            ptl_handle_ct_t         sync_cth;
            ptl_handle_me_t         sync_meh;
            int                     my_rank;
            int                     root_rank;
            int                     size;
            const void             *sbuf;
            void                   *rbuf;
            const char             *pack_src_buf;
            int                     pack_src_count;
            struct ompi_datatype_t *pack_src_dtype;
            MPI_Aint                pack_src_extent;
            MPI_Aint                pack_src_true_extent;
            MPI_Aint                pack_src_lb;
            MPI_Aint                pack_src_true_lb;
            MPI_Aint                pack_src_offset;
            char                   *unpack_dst_buf;
            int                     unpack_dst_count;
            struct ompi_datatype_t *unpack_dst_dtype;
            MPI_Aint                unpack_dst_extent;
            MPI_Aint                unpack_dst_true_extent;
            MPI_Aint                unpack_dst_lb;
            MPI_Aint                unpack_dst_true_lb;
        } gather;

        struct {
            opal_convertor_t        send_converter;
            opal_convertor_t        recv_converter;
            size_t                  packed_size;
            int8_t                  is_sync;
            int8_t                  free_after;
            size_t                  coll_count;
            char                   *scatter_buf;
            uint64_t                scatter_bytes;
            ptl_match_bits_t        scatter_match_bits;
            ptl_handle_md_t         scatter_mdh;
            ptl_handle_ct_t         scatter_cth;
            ptl_handle_md_t         scatter_meh;
            ptl_match_bits_t        sync_match_bits;
            ptl_handle_md_t         sync_mdh;
            ptl_handle_ct_t         sync_cth;
            ptl_handle_me_t         sync_meh;
            int                     my_rank;
            int                     root_rank;
            int                     size;
            const void             *sbuf;
            void                   *rbuf;
            uint64_t                pack_bytes;
            const char             *pack_src_buf;
            int                     pack_src_count;
            struct ompi_datatype_t *pack_src_dtype;
            MPI_Aint                pack_src_extent;
            MPI_Aint                pack_src_true_extent;
            MPI_Aint                pack_src_lb;
            MPI_Aint                pack_src_true_lb;
            uint64_t                unpack_bytes;
            char                   *unpack_dst_buf;
            int                     unpack_dst_count;
            struct ompi_datatype_t *unpack_dst_dtype;
            MPI_Aint                unpack_dst_extent;
            MPI_Aint                unpack_dst_true_extent;
            MPI_Aint                unpack_dst_lb;
            MPI_Aint                unpack_dst_true_lb;
            MPI_Aint                unpack_dst_offset;
        } scatter;
    } u;
};
typedef struct ompi_coll_portals4_request_t ompi_coll_portals4_request_t;

OBJ_CLASS_DECLARATION(ompi_coll_portals4_request_t);

#define OMPI_COLL_PORTALS4_REQUEST_ALLOC(comm, req)                       \
    do {                                                                  \
        opal_free_list_item_t *item;                                      \
        item = opal_free_list_get(&mca_coll_portals4_component.requests); \
                req = (ompi_coll_portals4_request_t*) item;               \
                OMPI_REQUEST_INIT(&req->super, false);                    \
                req->super.req_mpi_object.comm = comm;                    \
                req->super.req_complete = false;                          \
                req->super.req_state = OMPI_REQUEST_ACTIVE;               \
    } while (0)

#define OMPI_COLL_PORTALS4_REQUEST_RETURN(req)                       \
    do {                                                             \
        OMPI_REQUEST_FINI(&request->super);                          \
        opal_free_list_return(&mca_coll_portals4_component.requests, \
                (opal_free_list_item_t*) req);                       \
    } while (0)


#endif
