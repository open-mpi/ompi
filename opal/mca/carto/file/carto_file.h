/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * The file component uses a cartograpy file to discover the
 * host cartography.
 * 
 * An example cartography file:
 * 
 
# This is a comment
# Node declaration   Node type (Free string)   Node name (Free string)
# (Reserve word)     (slot is a reserve word   (free string)
#                     for CPU slot)
#=======================================================================
  NODE               Memory                    mem0
  NODE               Memory                    mem1
  NODE               Memory                    mem2
  NODE               Memory                    mem3
#
  NODE               slot                      slot0
  NODE               slot                      slot1
  NODE               slot                      slot2
  NODE               slot                      slot3
#
  NODE               Infiniband                mthca0
  NODE               Infiniband                mthca1
#
  NODE               Ethernet                  eth0
  NODE               Ethernet                  eth1
#
#
# Connection decleration  From node   To node:weight   To node:weight   ......   
# (Reserve word)          (declered   (declered        (declered 
#                          above)      above)           above)          
#===============================================================================================
  CONNECTION              mem0        slot0:0
  CONNECTION              mem1        slot1:0
  CONNECTION              mem2        slot2:0
  CONNECTION              mem3        slot3:0
  CONNECTION              slot0       mem0:0           slot1:1           slot2:1 mthca0:1 eth0:1
  CONNECTION              slot1       mem1:0           slot0:1           slot3:1
  CONNECTION              slot2       mem2:0           slot1:1           slot3:1
  CONNECTION              slot3       mem3:0           slot1:1           slot2:1 mthca1:1 eth1:1
#
  CONNECTION              mthca0      slot0:1
  CONNECTION              mthca1      slot3:1
#
  CONNECTION              eth0        slot0:1
  CONNECTION              eth1        slot3:1 
#
# end of carto file.

 * 
 * 
 * 
 * 
 * 
 */

#ifndef MCA_CARTO_FILE_H
#define MCA_CARTO_FILE_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/carto/carto.h"

BEGIN_C_DECLS

extern char *carto_file_path;

/**
 * Globally exported variable
 */
OPAL_DECLSPEC extern const opal_carto_base_component_1_0_0_t
mca_carto_file_component;


/**
 * carto query API function
 */
const opal_carto_base_module_1_0_0_t *
opal_carto_file_component_query(int *query);

END_C_DECLS

#endif /* MCA_CARTO_FILE_EXPORT_H */
