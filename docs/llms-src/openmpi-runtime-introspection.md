# Open MPI runtime introspection with `ompi_info`

The rest of this corpus describes the MPI **API** — the signatures,
parameters, and semantics that are fixed for a given Open MPI version. It
deliberately does **not** describe any single *installation*: the exact
version and build configuration, which optional components were compiled in,
or the run-time tunables (MCA parameters) those components expose. All of that
is installation-specific, so it is not snapshotted into this corpus.

Instead, query the installation directly. If you (or the program you are
assisting) can reach an Open MPI installation, the `ompi_info` command is the
authoritative, version-correct source for everything below — far more reliable
than guessing from this corpus, which documents the API, not the local build.
`ompi_info` itself is documented by the `ompi_info(1)` command man page
(indexed in this `llms.txt`).

## Version and build configuration

- `ompi_info --version` prints the Open MPI version, e.g. `Open MPI v6.1.0a1`.
- `ompi_info` with no arguments prints a human-readable summary: the version,
  the `./configure` command line, the compilers used, which language bindings
  (C, `mpif.h`, `use mpi`, `use mpi_f08`) are enabled, and the MCA components
  that were built.
- `ompi_info --parsable` prints the same information as stable, machine-readable
  `key:value` lines. Useful examples:

  ```
  ompi:version:full:6.1.0a1     # the Open MPI version
  mpi-api:version:full:4.1.0    # the MPI standard level this build implements
  ```

## Available components

Open MPI is built from MCA components (plugins) grouped into frameworks (such
as `btl` for byte transport, `coll` for collective algorithms, and `pml` for
point-to-point messaging). Which components are present depends on how the
installation was configured and built, so the only reliable way to list them
is from the installation itself, with:

```
ompi_info --all --parsable
```

Component lines are keyed `mca:<framework>:<component>:version:...`; for
example, an `mca:btl:tcp:version:...` line shows that the TCP BTL component is
present in this build.

## Discovering MCA parameters (run-time tunables)

MCA parameters control almost all run-time behavior — network selection,
collective algorithms, buffer sizes, and much more. The available set again
depends on which components were built, so list them from the installation
rather than expecting them in this corpus:

```
ompi_info --all --parsable
```

`--all` raises the detail to the maximum (every parameter at every "level",
1–9) and `--parsable` emits the machine-readable form. Plain `ompi_info` shows
only the small set of level-1 parameters; `--all` is what makes the listing
comprehensive.

The output is **self-describing**: each parameter emits one line per field,
keyed `mca:<framework>:<component>:param:<name>:<field>:<value>`. For example:

```
mca:btl:tcp:param:btl_tcp_if_include:value:
mca:btl:tcp:param:btl_tcp_if_include:source:default
mca:btl:tcp:param:btl_tcp_if_include:status:read-only
mca:btl:tcp:param:btl_tcp_if_include:level:1
mca:btl:tcp:param:btl_tcp_if_include:help:Comma-delimited list of devices and/or CIDR notation of networks to use for MPI communication ...
mca:btl:tcp:param:btl_tcp_if_include:type:string
mca:btl:tcp:param:btl_tcp_if_include:deprecated:no
```

Read the field names directly from the output rather than relying on a fixed
list here — fields can be added over time. The fields you will typically see
include the current `value`, where it came from (`source`), whether it can be
changed at run time (`status`), the verbosity `level` (1–3 end user, 4–6
application tuner, 7–9 implementer), the `help` text, the value `type`, whether
it is `deprecated`, any `enumerator` lines (the allowed values of an enumerated
parameter), and any `synonym_of` (an older name that still works).

Narrow the query to one component with `--param <framework> <component>`
(e.g. `ompi_info --param btl tcp --level 9`), and add `--internal` to include
parameters intended only for Open MPI developers.

## Setting MCA parameters

Once you know a parameter's name, it can be set several ways. They are listed
here highest precedence first (a command-line setting overrides an environment
variable, which overrides a file):

**1. On the `mpirun` / `mpiexec` command line** with `--mca <name> <value>`
(highest precedence). `mpiexec` is a synonym for `mpirun`.

```sh
# Use only the TCP and shared-memory BTLs for MPI communication:
mpirun --mca btl tcp,self,sm -np 4 ./my_app

# Restrict the TCP BTL to a specific network:
mpiexec --mca btl_tcp_if_include 192.168.1.0/24 -np 4 ./my_app
```

Quote any value that contains spaces or shell metacharacters, e.g.
`--mca <name> "value with spaces"`.

**2. In the environment**, by prefixing the parameter name with `OMPI_MCA_`.
This has the same effect as the command line but applies to every launch in
that environment:

```sh
export OMPI_MCA_btl=tcp,self,sm
export OMPI_MCA_btl_tcp_if_include=192.168.1.0/24
mpirun -np 4 ./my_app
```

**3. In MCA parameter files.** A tuning file passed with `mpirun --tune <file>`,
or the default files `$HOME/.openmpi/mca-params.conf` (per user) and
`$prefix/etc/openmpi-mca-params.conf` (system-wide), with one `name = value`
per line:

```ini
# ~/.openmpi/mca-params.conf
btl = tcp,self,sm
btl_tcp_if_include = 192.168.1.0/24
```

> **Note:** PMIx and PRRTE (Open MPI's run-time dependencies) have their own
> MCA systems. Their parameters use the `--pmixmca` / `--prtemca` command-line
> options and the `PMIX_MCA_` / `PRTE_MCA_` environment-variable prefixes
> (note the single "R" in `PRTE_MCA_`), not `--mca` / `OMPI_MCA_`.

For guidance on *which* parameters to set for a given network, accelerator, or
workload, see the human-facing "Tuning MPI applications" documentation and the
"Modular Component Architecture (MCA)" section of the Open MPI documentation.
