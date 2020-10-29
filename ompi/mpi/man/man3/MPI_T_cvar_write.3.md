# Name
MPI\_T\_cvar\_write - Write the value of a bound control variable

# Syntax
## C Syntax
c
#include <mpi.h>
int MPI_T_cvar_write(MPI_T_cvar_handle handle, const void *buf)


# Input Parameters
`handle` : Handle of the control variable to be written.
`buf` : Initial address of storage location for variable value.

# Description

MPI\_T\_cvar\_write sets the value the control variable identified by
the handle specified in *handle* from the buffer provided in *buf*. The
caller must ensure that the buffer specified in *buf* is large enough to
hold the entire value of the control variable. If the variable has
global scope, any write call must be issued on all connected MPI
processes. For more information see MPI-3 � 14.3.6.


# Errors

MPI\_T\_cvar\_write() will fail if:

`\[MPI\_T\_ERR\_NOT\_INITIALIZED\]` : The MPI Tools interface not initialized

:   The MPI Tools interface not initialized

`\[MPI\_T\_ERR\_INVALID\_HANDLE\]` : The handle is invalid

:   The handle is invalid

`\[MPI\_T\_ERR\_CVAR\_SET\_NOT\_NOW\]` : Variable cannot be set at this moment

:   Variable cannot be set at this moment

`\[MPI\_T\_ERR\_CVAR\_SET\_NEVER\]` : Variable cannot be set until end of execution

:   Variable cannot be set until end of execution


# See Also

MPI_T_cvar_handle_alloc
MPI_T_cvar_get_info
