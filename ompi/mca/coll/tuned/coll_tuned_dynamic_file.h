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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_TUNED_DYNAMIC_FILE_H_HAS_BEEN_INCLUDED
#define MCA_COLL_TUNED_DYNAMIC_FILE_H_HAS_BEEN_INCLUDED

#include "ompi_config.h"

/* also need the dynamic rule structures */
#include "coll_tuned_dynamic_rules.h"

/*
 * @file
 *
 * #######################
 * # Dynamic file format #
 * #######################
 * File defined rules precede MCA parameter defined rule.
 * To activate file reader, the MCA parameter use_dynamic_file_rules must
 * be set to true. The path to the dynamic file is given by the MCA
 * parameter dynamic_rules_filename. If there is any issue reading the file,
 * the file is considered as invalid and only MCA parameter defined rules are
 * used. If a potential logical issue is identified in the file, a
 * warning is printed but the file is not considered as invalid.
 *
 *
 * Here is an example of a dynamic rules file:
 * 1 # number of collectives
 * 3 # Collective identifier 1 (defined in ompi/mca/coll/base/coll_base_functions.h)
 * 2   # number of comm sizes
 * 1   # comm size 1
 * 1               # number of message size rules
 * 0 1 0 0         # for message size 0, choose algorithm 1, topo 0, 0 segmentation
 * 8   # comm size 8
 * 4               # number of message size rules
 * 0 1 0 0         # for message size 0, choose algorithm 1, topo 0, 0 segmentation
 * 32768 2 0 0     # for message size 32768, choose algorithm 2, topo 0, 0 segmentation
 * 262144 1 0 0    # for message size 262144, choose algorithm 1, topo 0, 0 segmentation
 * 524288 2 0 0    # for message size 524288, choose algorithm 2, topo 0, 0 segmentation
 *
 * Optionally, specify topological level in the message size rules,
 * which can be singlenode or disjoint.
 * 1 # number of collectives
 * 3 # Collective identifier 1 (defined in ompi/mca/coll/base/coll_base_functions.h)
 * 2   # number of comm sizes
 * 1   # comm size 1
 * 2                          # number of message size rules
 * 0@singlenode 2 0 0         # for message size 0 and single node communication, choose algorithm 2, topo 0, 0 segmentation
 * 0@disjoint 1 0 0           # for message size 0 and disjoint communication, choose algorithm 1, topo 0, 0 segmentation
 * 8   # comm size 8
 * 6                          # number of message size rules
 * 0@singlenode 2 0 0         # for message size 0 and single node communication, choose algorithm 2, topo 0, 0 segmentation
 * 0@disjoint 1 0 0           # for message size 0 and disjoint communication, choose algorithm 1, topo 0, 0 segmentation
 * 32768@singlenode 2 0 0     # for message size 32768 and single node communication, choose algorithm 2, topo 0, 0 segmentation
 * 32768@disjoint 1 0 0       # for message size 32768 and disjoint communication, choose algorithm 1, topo 0, 0 segmentation
 * 262144 1 0 0               # for message size 262144, choose algorithm 1, topo 0, 0 segmentation
 * 524288 2 0 0               # for message size 524288, choose algorithm 2, topo 0, 0 segmentation
 * 
 * 
 * Note that comm size and message size rules define minimal
 * values and each new rule precede every other rules. This property
 * implies that this types of rules must be sorted by increasing value.
 * If they are not, some rules wont be used.
 *
 * The counts define a stack. If the count is set to x, the reader will
 * attempt to read x rules of the corresponding type. If a set of rules
 * has an invalid count, this is an error and it might not be detected by
 * the reader.
 */

BEGIN_C_DECLS

int ompi_coll_tuned_read_rules_config_file (char *fname, ompi_coll_alg_rule_t** rules, int n_collectives);


END_C_DECLS
#endif /* MCA_COLL_TUNED_DYNAMIC_FILE_H_HAS_BEEN_INCLUDED */


