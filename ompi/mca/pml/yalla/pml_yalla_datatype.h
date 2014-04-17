/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_YALLA_DATATYPE_H_
#define PML_YALLA_DATATYPE_H_

#include "pml_yalla.h"

struct pml_yalla_convertor {
    ompi_free_list_item_t     super;
    ompi_datatype_t           *datatype;
    opal_convertor_t          convertor;
};

OBJ_CLASS_DECLARATION(mca_pml_yalla_convertor_t);

#define PML_YALLA_INIT_MXM_REQ_DATA(_req_base, _buf, _count, _dtype, _stream_type, ...) \
    { \
        ptrdiff_t size, lb; \
        \
        if (opal_datatype_is_contiguous_memory_layout(&(_dtype)->super, _count)) { \
            ompi_datatype_type_size(_dtype, &size); \
            ompi_datatype_type_lb(_dtype, &lb); \
            (_req_base)->data_type          = MXM_REQ_DATA_BUFFER; \
            (_req_base)->data.buffer.ptr    = _buf + lb; \
            (_req_base)->data.buffer.length = size * (_count); \
        } else { \
            mca_pml_yalla_set_noncontig_data_ ## _stream_type(_req_base, \
                                                              _buf, _count, \
                                                              _dtype, ## __VA_ARGS__); \
        } \
    }

#define PML_YALLA_RESET_PML_REQ_DATA(_pml_req) \
    { \
        if ((_pml_req)->convertor != NULL) { \
            size_t _position = 0; \
            opal_convertor_set_position(&(_pml_req)->convertor->convertor, &_position); \
        } \
    }


static inline void mca_pml_yalla_convertor_free(mca_pml_yalla_convertor_t *convertor)
{
    opal_convertor_cleanup(&convertor->convertor);
    OBJ_RELEASE(convertor->datatype);
    PML_YALLA_FREELIST_RETURN(&ompi_pml_yalla.convs, &convertor->super);
}

void mca_pml_yalla_set_noncontig_data_irecv(mxm_req_base_t *mxm_req, void *buf,
                                            size_t count, ompi_datatype_t *datatype,
                                            mca_pml_yalla_recv_request_t *rreq);

void mca_pml_yalla_set_noncontig_data_recv(mxm_req_base_t *mxm_req, void *buf,
                                           size_t count, ompi_datatype_t *datatype);

void mca_pml_yalla_set_noncontig_data_isend(mxm_req_base_t *mxm_req, void *buf,
                                            size_t count, ompi_datatype_t *datatype,
                                            mca_pml_yalla_send_request_t *sreq);

void mca_pml_yalla_set_noncontig_data_send(mxm_req_base_t *mxm_req, void *buf,
                                           size_t count, ompi_datatype_t *datatype);

void mca_pml_yalla_init_datatype(void);


#endif /* PML_YALLA_DATATYPE_H_ */
