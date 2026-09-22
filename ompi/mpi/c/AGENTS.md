# AGENTS.md

Guidance for AI coding agents working with generated MPI C bindings.

## Overview

Many files in this directory (`ompi/mpi/c/`) are **automatically generated**
from template files by the Python-based binding generator. These files
contain a header comment:

```c
/* THIS FILE WAS AUTOMATICALLY GENERATED. DO NOT EDIT BY HAND. */
```

**Never edit generated files directly.** Changes will be lost when the
bindings are regenerated. Always edit the corresponding template file instead.

## Generated Files and Their Templates

Generated files follow naming patterns that map to template files:

### File naming conventions (see Makefile.am SUFFIXES)

The `Makefile.am` in this directory defines suffix rules that show how
template files are transformed into generated code:

- `*_ompi_generated.c` ← generated from `*.c.in` (Open MPI ABI variant)
- `*_abi_generated.c` ← generated from `*.c.in` (MPI Standard ABI variant)

For example:
- `init_ompi_generated.c` and `init_abi_generated.c` are both generated from
  a single template (processed by the binding generator with different ABI settings)

The same pattern applies in `ompi/mpi/tool/` for MPI_T functions.

### The binding generator

The Python infrastructure that processes template files and generates the C
bindings is located in:

```
ompi/mpi/bindings/
```

The generator handles:
- Parsing template files
- Generating both Open MPI ABI and Standard ABI variants
- Applying ABI-specific transformations (handle conversions, error code mappings, etc.)
- Emitting correctly formatted C code with appropriate guards and includes

To regenerate bindings after editing a template, run `make` from the build
directory. The build system will detect template changes and regenerate the
corresponding output files.

## Common Patterns in Templates

### Standard ABI converter registration

When the Standard ABI variant of an initialization function (MPI_Init,
MPI_Init_thread, MPI_Session_init) or MPI_T_event_register_callback is
generated, it must register ABI converters so that MPI_T event payloads
publish Standard-ABI values instead of internal Open MPI values.

The registration pattern appears in templates guarded by `#if OMPI_ABI_SRC`:

```c
#if OMPI_ABI_SRC
    ompi_mpit_callback_abi = OMPI_MPIT_ABI_STANDARD;
    ompi_mpit_register_abi_handle_convert(ompi_mpit_abi_handle_convert_impl);
    ompi_mpit_register_abi_error_convert(ompi_mpit_abi_error_convert_impl);
    ompi_mpit_register_abi_bind_convert(ompi_mpit_abi_bind_convert_impl);
    ompi_mpit_register_abi_thread_level_convert(ompi_mpit_abi_thread_level_convert_impl);
#endif
```

These converters ensure that MPI object handles, error codes, bind values,
and thread support levels in MPI_T event payloads are transformed from
internal representations to Standard ABI representations.

**When adding a new converter type**, you must:

1. Add the registration function declaration to `ompi/runtime/ompi_mpit_events.h`
2. Implement the registration and forwarding functions in
   `ompi/runtime/ompi_mpit_register_events.c`
3. Add the converter implementation to `ompi/mpi/c/mpit_abi_handle_convert.{c,h}`
4. Update ALL template files that perform converter registration to include
   the new registration call (init, init_thread, session_init templates, and
   event_register_callback template)

### MPI_T_init_thread and MPI_Init_thread interaction

**Important behavior**: When `MPI_T_init_thread(level, ...)` is called before
`MPI_Init_thread(level2, ...)`, the effective thread support provided by
`MPI_Init_thread` will be constrained to the level requested in the earlier
`MPI_T_init_thread` call, even if `level2` is higher.

This means:
- If `MPI_T_init_thread(MPI_THREAD_SINGLE, ...)` is called first, a subsequent
  `MPI_Init_thread(MPI_THREAD_MULTIPLE, ...)` will only provide `MPI_THREAD_SINGLE`
- Tests that validate thread level in MPI_T initialization events must request
  the same (or lower) thread level in both calls

Example from `callback_mpit_event_handle_init_thread` test:
```c
/* Use MPI_THREAD_MULTIPLE to match the subsequent MPI_Init_thread request */
MPI_T_init_thread(MPI_THREAD_MULTIPLE, &provided);
/* ... register callback ... */
MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
```

## Workflow for Modifying Generated Code

1. **Identify the template**: Given a generated file like `init_abi_generated.c`,
   find its template by examining the `Makefile.am` SUFFIXES rules and looking
   for a corresponding `.c.in` file or understanding the generator's template
   structure.

2. **Edit the template**: Make changes to the template file (or the generator
   in `ompi/mpi/bindings/` if the change is structural).

3. **Regenerate**: Run `make` from the build directory. The build system will
   regenerate affected files.

4. **Verify**: Check that the generated files contain your changes and that
   `make check` passes.

5. **Commit both**: Commit both the template changes and the regenerated output
   files together, so the repository stays consistent.

## Testing

After modifying templates that affect ABI converter registration:

```sh
make install
export PATH="<prefix>/bin:$PATH"
make check-abi
```

The `check-abi` target runs the MPI Standard ABI conformance tests, including
tests that validate MPI_T event payloads contain correct Standard-ABI values.

## See Also

- `ompi/mpi/README_ABI.md` - MPI-5 Standard ABI support overview
- `ompi/mpi/bindings/` - Binding generator implementation
- `ompi/runtime/ompi_mpit_events.h` - MPI_T event ABI converter infrastructure
- `CLAUDE.md` (repository root) - General guidance for AI coding agents
