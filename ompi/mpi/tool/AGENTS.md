# AGENTS.md

Guidance for AI coding agents working with generated MPI_T (MPI Tool) bindings.

## Overview

Many files in this directory (`ompi/mpi/tool/`) are **automatically generated**
from template files by the Python-based binding generator, following the same
patterns as the MPI C bindings.

For detailed information about the code generation system, see:

**[`ompi/mpi/c/AGENTS.md`](../c/AGENTS.md)**

That file provides comprehensive guidance on:
- How template files map to generated files
- The SUFFIXES naming conventions
- The binding generator location (`ompi/mpi/bindings/`)
- Common patterns (especially ABI converter registration)
- Workflow for modifying generated code
- Testing procedures

## MPI_T-Specific Notes

### Files in this directory

This directory contains the MPI Tool Information Interface (MPI_T) functions:
- Variable queries and control
- Performance variable access
- Category enumeration
- **Event management** (event registration, callback handling)

Like the MPI C bindings, files here follow the generation pattern:
- `*_ompi_generated.c` ← Open MPI ABI variant
- `*_abi_generated.c` ← MPI Standard ABI variant

### Event callback registration

The `event_register_callback_*_generated.c` files are particularly important
for ABI converter registration. `MPI_T_event_register_callback` can be called
**before MPI_Init** (MPI_T is init-independent), so it must register ABI
converters to handle pre-init events like `ompi.mpi.errhandler_invoked`.

The Standard ABI variant (`event_register_callback_abi_generated.c`) includes
the same converter registration pattern as the init functions:

```c
#if OMPI_ABI_SRC
    ompi_mpit_callback_abi = OMPI_MPIT_ABI_STANDARD;
    ompi_mpit_register_abi_handle_convert(ompi_mpit_abi_handle_convert_impl);
    ompi_mpit_register_abi_error_convert(ompi_mpit_abi_error_convert_impl);
    ompi_mpit_register_abi_bind_convert(ompi_mpit_abi_bind_convert_impl);
    ompi_mpit_register_abi_thread_level_convert(ompi_mpit_abi_thread_level_convert_impl);
#else
    ompi_mpit_callback_abi = OMPI_MPIT_ABI_OMPI;
#endif
```

This registration is idempotent with the init-time registration, ensuring
converters are available regardless of whether MPI_T or MPI is initialized first.

## See Also

- **[`ompi/mpi/c/AGENTS.md`](../c/AGENTS.md)** - Main guidance (READ THIS FIRST)
- `ompi/runtime/ompi_mpit_events.h` - MPI_T event ABI converter infrastructure
- `ompi/mpi/bindings/` - Binding generator implementation
