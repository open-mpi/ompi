/*
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef COMM_NETPATTERNS_H
#define COMM_NETPATTERNS_H

#include "ompi_config.h"
#include "orte/include/orte/types.h"
#include "orte/mca/rml/rml_types.h"

BEGIN_C_DECLS

#define MAX_TMP_BUFFER            8192

static void send_completion(int status, struct orte_process_name_t* peer,
                struct iovec* msg, int count, orte_rml_tag_t tag, void* cbdata);

static void recv_completion(int status, struct orte_process_name_t* peer,
                struct iovec* msg, int count, orte_rml_tag_t tag, void* cbdata);

static int op_reduce(int op_type,void *src_dest_buf,void * src_buf, int count,
                int data_type);


END_C_DECLS

#endif /* COMM_NETPATTERNS_H */
