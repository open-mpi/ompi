/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      University of Houston. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_SHAREDFP_addproc_control_H
#define MCA_SHAREDFP_addproc_control_H

#include <stdbool.h>
#include "mpi.h"
#include "sharedfp_addproc.h"

BEGIN_C_DECLS

void nodeDelete(node **front, node **rear);
void nodeInsert(node **front, node **rear, int procNo, long numBytesArrAddr);
int Check_Request_Offset(int tag_received);
int Check_Acknowledgement(int tag_received);
int End_control_shared_request(int tag_received);

END_C_DECLS

#endif /* MCA_SHAREDFP_addproc_control_H */
