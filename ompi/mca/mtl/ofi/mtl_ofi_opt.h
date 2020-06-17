/*
 * Copyright (c) 2013-2018 Intel, Inc. All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_OFI_OPT_H
#define MTL_OFI_OPT_H

#include "mtl_ofi.h"

BEGIN_C_DECLS

#define CQ_DATA_TYPES 2
#define OMPI_MTL_OFI_SEND_TYPES     [CQ_DATA_TYPES]
#define OMPI_MTL_OFI_ISEND_TYPES    [CQ_DATA_TYPES]
#define OMPI_MTL_OFI_IRECV_TYPES    [CQ_DATA_TYPES]
#define OMPI_MTL_OFI_IPROBE_TYPES   [CQ_DATA_TYPES]
#define OMPI_MTL_OFI_IMPROBE_TYPES  [CQ_DATA_TYPES]

struct ompi_mtl_ofi_symtable {
    int (*ompi_mtl_ofi_send OMPI_MTL_OFI_SEND_TYPES )
            (struct mca_mtl_base_module_t *mtl,
              struct ompi_communicator_t *comm,
              int dest,
              int tag,
              struct opal_convertor_t *convertor,
              mca_pml_base_send_mode_t mode);
    int (*ompi_mtl_ofi_isend OMPI_MTL_OFI_ISEND_TYPES )
            (struct mca_mtl_base_module_t *mtl,
               struct ompi_communicator_t *comm,
               int dest,
               int tag,
               struct opal_convertor_t *convertor,
               mca_pml_base_send_mode_t mode,
               bool blocking,
               mca_mtl_request_t *mtl_request);
    int (*ompi_mtl_ofi_irecv OMPI_MTL_OFI_IRECV_TYPES )
            (struct mca_mtl_base_module_t *mtl,
               struct ompi_communicator_t *comm,
               int src,
               int tag,
               struct opal_convertor_t *convertor,
               mca_mtl_request_t *mtl_request);
    int (*ompi_mtl_ofi_iprobe OMPI_MTL_OFI_IPROBE_TYPES )
            (struct mca_mtl_base_module_t *mtl,
                struct ompi_communicator_t *comm,
                int src,
                int tag,
                int *flag,
                struct ompi_status_public_t *status);
    int (*ompi_mtl_ofi_improbe OMPI_MTL_OFI_IMPROBE_TYPES )
            (struct mca_mtl_base_module_t *mtl,
                 struct ompi_communicator_t *comm,
                 int src,
                 int tag,
                 int *matched,
                 struct ompi_message_t **message,
                 struct ompi_status_public_t *status);
};

/**
 * MTL OFI specialization function symbol table init
 */
void ompi_mtl_ofi_send_symtable_init(struct ompi_mtl_ofi_symtable* sym_table);
void ompi_mtl_ofi_isend_symtable_init(struct ompi_mtl_ofi_symtable* sym_table);
void ompi_mtl_ofi_irecv_symtable_init(struct ompi_mtl_ofi_symtable* sym_table);
void ompi_mtl_ofi_iprobe_symtable_init(struct ompi_mtl_ofi_symtable* sym_table);
void ompi_mtl_ofi_improbe_symtable_init(struct ompi_mtl_ofi_symtable* sym_table);

END_C_DECLS

#endif /* MTL_OFI_OPT_H */