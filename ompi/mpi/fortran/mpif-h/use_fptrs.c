#include <mpi.h>
#include <strings.h>
#include <stdbool.h>

#include "ompi/include/ompi/constants.h"
#include "opal/mca/base/mca_base_var.h"

#include "constructed_fptr_definitions.h"

// Most fortran entrypoints mpi_foo() boil down to a C call through
// a function pointer ompi_fptr_MPI_Foo() which is set to either
// &MPI_Foo or &PMPI_Foo.
//
// We decide which setting to use based on an MCA parameter
//   --mca mpi_fortcall PMPI   (the default)
//   --mca mpi_fortcall MPI
// but it's legal for a few functions (MPI_GET_VERSION,
// MPI_GET_LIBRARY_VERSION, MPI_INITIALIZED, MPI_FINALIZED)
// to be called before MPI_Init and thus before the MCA system
// is setup, so in that case we'll just make a best guess as to
// what the function pointers should point to.
//
// That gray area before MPI_Init where we still need some initial value
// is what the STATE_PARTIALLY_INTIALIZED below is for.

#define STATE_NOT_INITIALIZED       0
#define STATE_PARTIALLY_INTIALIZED  1
#define STATE_INTIALIZED            2

static int ompi_fptr_initialization_state = STATE_NOT_INITIALIZED;

void
ompi_fptr_init(int mca_system_is_ready) {
    int use_mpi = 0; // the default is to use PMPI

    if (ompi_fptr_initialization_state == STATE_INTIALIZED) {
        return;
    }
    else if (ompi_fptr_initialization_state == STATE_PARTIALLY_INTIALIZED
        && !mca_system_is_ready)
    {
        return;
    }
    else if (ompi_fptr_initialization_state < STATE_INTIALIZED
        && mca_system_is_ready)
    {
        int var_id;
        const char **value;
        var_id = mca_base_var_find("ompi", "mpi", NULL, "fortcall");
        if (0 <= var_id) {
            int ret = mca_base_var_get_value(var_id, &value, NULL, NULL);
            if (OMPI_SUCCESS == ret &&
                value && *value && 0 == strcasecmp(*value, "MPI"))
            {
                use_mpi = 1;
            }
        }

        ompi_fptr_initialization_state = STATE_INTIALIZED;
    }
    else if (ompi_fptr_initialization_state < STATE_PARTIALLY_INTIALIZED
        && !mca_system_is_ready)
    {
        char *p;
        p = getenv("OMPI_MCA_mpi_fortcall");
        if (p && *p && 0==strcasecmp(p, "MPI")) {
            use_mpi = 1;
        }

        ompi_fptr_initialization_state = STATE_PARTIALLY_INTIALIZED;
    }

    if (use_mpi) {
#include "constructed_fptr_assignments_MPI.h"
    } else {
#include "constructed_fptr_assignments_PMPI.h"
    }
}
