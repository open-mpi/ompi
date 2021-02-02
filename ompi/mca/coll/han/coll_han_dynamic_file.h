/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2020      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Bull S.A.S. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef MCA_COLL_HAN_DYNAMIC_FILE_H
#define MCA_COLL_HAN_DYNAMIC_FILE_H

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/mca/mca.h"
#include "opal/util/output.h"

/*
 * @file
 *
 * ############################
 * # MCA parameters interface #
 * ############################
 * An MCA parameter defined rule allows the user to choose which collective
 * module will be used for a specific collective communication on a specific
 * topological level. The standard name for these parameters is:
 * [collective]_dynamic_[topologic_level]_module
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
 * The file is built recursively.
 * A set of rules of a type is built as follows:
 * Number of rules of the set
 * Rule1
 * Rule2
 * ...
 *
 * A rule of the level i is built as follows (excluding message size rule):
 * Rule property
 * Set of rules of level i+1
 *
 * A message size rule is built as follows:
 * Message_size Component
 *
 * Rule properties are (by increasing level):
 *     - Collective identifier:
 *           Defined in ompi/mca/coll/base/coll_base_functions.h.
 *     - Topologic level:
 *           Defined in coll_han_dynamic.h. It defines the communicator
 *           topology level. This is GLOBAL_COMMUNICATOR for the user
 *           communicator and the corresponding level for sub-communicators
 *           created by han.
 *     - Configuration size:
 *           The configuration size is the number of elements in a topology level. 
 *           For example, if topology levels are intra-node and inter-node, it can
 *           be the number of MPI ranks per node or the number of nodes in the global
 *           communicator. For the GLOBAL_COMMUNICATOR topologic level,
 *           the configuration size is the communicator size.
 *     - Message_size Component:
 *           This is the message size, in bytes, of the message. Component is
 *           the component identifier to use for this collective on this
 *           communicator with this message size. Components identifier are
 *           defined in coll_han_dynamic.h
 *
 * Here is an example of a dynamic rules file:
 * 2 # Collective count
 * 7 # Collective identifier 1 (defined in ompi/mca/coll/base/coll_base_functions.h)
 * 2   # Topologic level count
 * 0   # Topologic level identifier 1
 * 1     # Configuration count
 * 1     # Configuration size 1
 * 2       # Message size rules count
 * 0 3     # Message size 1 and component identifier
 * 128 1   # Message size 2 and component identifier
 * 1   # Topologic level identifier 2
 * 1     # Configuration count
 * 1     # Configuration size 1
 * 1       # Message size rules count
 * 0 1     # Message size 1 and component identifier
 * 3 # Collective identifier 2
 * # Set of topological rules
 *
 * Note that configuration size and message size rules define minimal
 * values and each new rule precede every other rules. This property
 * implies that this types of rules must be sorted by increasing value.
 * If they are not, some rules wont be used.
 *
 * The counts define a stack. If the count is set to x, the reader will
 * attempt to read x rules of the corresponding type. If a set of rules
 * has an invalid count, this is an error and it might not be detected by
 * the reader.
 */

int mca_coll_han_init_dynamic_rules(void);
void mca_coll_han_free_dynamic_rules(void);
void mca_coll_han_dump_dynamic_rules(void);

#endif
