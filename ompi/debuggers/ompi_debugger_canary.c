/*
 * Copyright (c) 2008 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2008 Sun Microystems, Inc.  All rights reserved
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

/*
 * Include all header files for the datatypes that we care about / use
 * in the DLL code 
 */
#include "ompi/class/ompi_free_list.h"
#include "ompi/request/request.h"
#include "ompi/group/group.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/datatype.h"
#include "ompi/mca/pml/base/pml_base_request.h"
#include "ompi/mca/pml/base/pml_base_sendreq.h"
#include "ompi/mca/pml/base/pml_base_recvreq.h"
#include "ompi/mca/topo/topo.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_pointer_array.h"

/*
 * Define ompi_field_offset() to be a debugging macro only -- just
 * instantiate a variable and then use the field member that we're
 * trying to use in the DLL.  If it compiles, good.  If it doesn't,
 * then it means that the DLL no longer matches the main OMPI code
 * base.
 */
#define ompi_field_offset(out_name, qh_type, struct_name, field_name)  \
    { struct_name foo; char *bogus = (char*) &foo.field_name; *bogus = 'a'; }

/*
 * Now include the common dll .c file that will use the above macro.
 */
#include "ompi_common_dll.c"
