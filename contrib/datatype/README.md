<!--
Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.

$COPYRIGHT$

Additional copyrights may follow

$HEADER$
-->

# Datatype performance tools

This directory contains unsupported tools for building benchmarks, collecting and plotting
cross-implementation results, and tuning architecture-dependent datatype policies. The tools do not
modify Open MPI source files or include datatype construction in timed regions.

## Prerequisites

- Python 3.10 or newer.
- Matplotlib for the two plotting tools. Data collection can run without it by using `--no-plot`.
- An optimized Open MPI build for internal descriptor sweeps and architecture tuning.
- MPI installation prefixes, compiler wrappers, build directories, or existing `to_self`
  executables for cross-implementation comparisons.

Run the examples from the Open MPI source root. The collection tools preserve their effective
configuration and commands, and refuse to overwrite existing result tables unless `--force` is
specified. The builder and plotters replace their explicitly named outputs.

## `build_to_self_release.py`

Build one `to_self.c` source with a selected MPI compiler wrapper. This is useful when the benchmark
source comes from the current checkout but the executable must link against an installed release.
The output is validated with `--help`, and `<output>.build.json` records the complete compiler
command, working directory, standard output, and standard error.

For a public MPI application build, only the source, compiler wrapper, and output are required:

```sh
python3 contrib/datatype/build_to_self_release.py \
  --source ompi/test/datatype/to_self.c \
  --mpicc /opt/openmpi/5.0/bin/mpicc \
  --output /tmp/to-self-openmpi-5
```

When the source needs headers or configured flags from a matching Open MPI checkout, provide both
the source and build roots:

```sh
python3 contrib/datatype/build_to_self_release.py \
  --source ompi/test/datatype/to_self.c \
  --release-source /src/openmpi-5 \
  --release-build /build/openmpi-5 \
  --mpicc /opt/openmpi/5.0/bin/mpicc \
  --output /tmp/to-self-openmpi-5
```

## `run_to_self_pack_comparison.py`

Compare one operation across a named baseline and any number of MPI implementations. Each datatype,
implementation, and repetition runs in a fresh process, with implementation order rotated between
repetitions. The runner retains every raw trial and computes paired median speedups with bootstrap
confidence intervals.

`--base` and each repeatable `--against LABEL PATH` accept an MPI installation prefix, `mpicc`, an
Open MPI build directory, or an existing `to_self` executable. Installation prefixes and compiler
wrappers cause the runner to compile `ompi/test/datatype/to_self.c`; communication operations also use
the launcher installed beside the corresponding compiler wrapper.

The supported operations are `pack`, `unpack`, `isend_recv`, `isend_irecv`, `irecv_send`, and
`irecv_isend`. Pack and unpack run with one process. Communication operations run with two processes
and accept independent `--send=ddt|packed` and `--recv=ddt|packed` endpoints. A `packed` endpoint is a
compact buffer represented by a datatype with the equivalent basic-type signature, so it can match
a derived endpoint legally.

Compare pack performance against current Open MPI, including its hand-written baseline:

```sh
python3 contrib/datatype/run_to_self_pack_comparison.py \
  --base-label current \
  --base /opt/openmpi/current \
  --against MPICH /opt/mpich \
  --against "Open MPI 5.x" /opt/openmpi/5.0 \
  --operation pack \
  --data all \
  --repetitions 5 \
  --output /tmp/to-self-pack
```

Measure a derived send into a compact receive buffer without generating hand-written comparisons:

```sh
python3 contrib/datatype/run_to_self_pack_comparison.py \
  --base-label MPICH \
  --base /opt/mpich \
  --against current /opt/openmpi/current \
  --operation isend_recv \
  --send ddt \
  --recv packed \
  --exclude-hand-made \
  --output /tmp/to-self-ddt-to-packed
```

The result directory contains `run_config.json`, `manifest.txt`, `commands.tsv`, raw process output,
`measurements.tsv`, `trials.tsv`, `summary.tsv`, and `datatype_summary.tsv`. Unless `--no-plot` is
used, the runner also invokes `plot_to_self_pack_comparison.py`.

Every measured run also passes `to_self --validate`, a short single-pass self-check that compares
`MPI_Pack`/`MPI_Unpack` against the datatype's by-hand reference before the timed loops. This guards
against an implementation whose numbers are fast only because they are *wrong* -- for example a mover
whose stride silently drops the inter-block gaps. The per-direction verdict (`PASS`/`FAIL`/`SKIP`) is
recorded in a `valid` column in `measurements.tsv`, summarized per datatype/implementation in
`validation.tsv`, and any `FAIL` is called out loudly on the console; a failing implementation's
timings must not be read as a speedup.

## `plot_to_self_pack_comparison.py`

Regenerate graphs from one `summary.tsv` produced by `run_to_self_pack_comparison.py`. It creates a
combined PNG/PDF and one PNG per datatype under `by_datatype`. Tukey-filtered results are written at
the output root and unfiltered results under `unfiltered`.

```sh
python3 contrib/datatype/plot_to_self_pack_comparison.py \
  --input /tmp/to-self-pack/summary.tsv \
  --output /tmp/to-self-pack/graphs \
  --columns 4 \
  --noise 3
```

Use `--exclude-hand-made` to omit the base-versus-hand-written series while retaining comparisons
against the named MPI implementations.

## `run_to_self_suite_comparison.py`

Run the complete comparison suite by invoking `run_to_self_pack_comparison.py` for five scenarios:

- Single-process `MPI_Pack`.
- Single-process `MPI_Unpack`.
- Two-process derived-to-compact communication.
- Two-process compact-to-derived communication.
- Two-process derived-to-derived communication.

The baseline is commonly MPICH so every Open MPI line has the same reference. The following command
runs all datatypes with 40 trials and five independent repetitions:

```sh
python3 contrib/datatype/run_to_self_suite_comparison.py \
  --base-label MPICH \
  --base /opt/mpich \
  --against current /opt/openmpi/current \
  --against "Open MPI 5.x" /opt/openmpi/5.0 \
  --data all \
  --cycles 100 \
  --trials 40 \
  --warmups 5 \
  --min-work-bytes 1048576 \
  --communication-min-work-bytes 0 \
  --repetitions 5 \
  --output /tmp/to-self-suite
```

Each scenario has its own result subdirectory. The suite writes the five exact runner invocations to
`commands.log`, records the common configuration in `run_config.json`, merges scenario summaries into
`summary.tsv`, and normally invokes `plot_to_self_suite_comparison.py`.

## `plot_to_self_suite_comparison.py`

Generate one five-panel PNG and PDF per datatype from a merged suite summary. Positive percentages
mean the named implementation is faster than the baseline. The shaded band defaults to +/-3%.

```sh
python3 contrib/datatype/plot_to_self_suite_comparison.py \
  --input /tmp/to-self-suite/summary.tsv \
  --output /tmp/to-self-suite/graphs \
  --statistic filtered \
  --noise 3
```

Use `--statistic=unfiltered` to plot all trials instead of the outlier-filtered measurements.

## `run_pack_description_sweep.py`

Drive `ompi/test/datatype/pack_description_sweep` over exact synthetic descriptor shapes. The sweep
controls operation, predefined-element size, DATA count, block length, loop-body item count, loop
iterations, datatype count, and gaps. Unlike `to_self`, this tester can install implementation-
specific optimized descriptions and directly compare equivalent copy and loop geometries.

Build `ompi/test/datatype/pack_description_sweep` in an optimized Open MPI build, then compare equivalent
eight-element signatures represented by different count and block-length combinations:

```sh
python3 contrib/datatype/run_pack_description_sweep.py \
  --tester /build/openmpi/ompi/test/datatype/pack_description_sweep \
  --operation pack \
  --element-size 4 \
  --data-counts 1,2,4,8 \
  --blocklens 1,2,4,8 \
  --block-gap 0 \
  --equivalent-elements 8 \
  --loop-items 1,2,4,8 \
  --total-items 8 \
  --repetitions 5 \
  --output /tmp/pack-description-sweep
```

The output contains `manifest.txt`, `run_config.json`, `commands.tsv`, `program-output.log`, every
measurement in `raw.tsv`, and the reduced comparisons in `summary.tsv`.

## `tune_datatype.py`

Measure architecture-dependent datatype policies through controlled `mpirun` jobs. The tuner uses
two programs from the same configured Open MPI build:

- `to_self` covers the complete public-MPI datatype corpus and compares datatype-consolidation
  thresholds.
- `pack_description_sweep` installs exact internal descriptors and measures loop grouping, copy
  geometry, output fragmentation, and the current type-labeled interpreter against its compact
  reference implementation.

The tuner reports candidate values that must be validated before changing production policy. Build
both test programs in an optimized configuration, then run the default `quick` profile:

```sh
python3 contrib/datatype/tune_datatype.py \
  --build-dir "$PWD/../build/release" \
  --output /tmp/ompi-datatype-tuning
```

The `full` profile adds every block length from 1 through 9, 16, 32, 64, and 128; more count, gap,
loop, and fragment shapes; and is intended for a quiet, dedicated machine:

```sh
python3 contrib/datatype/tune_datatype.py \
  --build-dir "$PWD/../build/release" \
  --profile full \
  --trials 30 \
  --warmups 3 \
  --output /tmp/ompi-datatype-tuning-full
```

Launcher-specific placement can be supplied repeatedly. For example:

```sh
python3 contrib/datatype/tune_datatype.py \
  --build-dir "$PWD/../build/release" \
  --mpirun-arg=--bind-to \
  --mpirun-arg=core \
  --output /tmp/ompi-datatype-tuning-bound
```

Use `--check=corpus`, `--check=loops`, or `--check=movers` for an individual stage. Explicit
`--mpirun`, `--to-self`, and `--description-sweep` paths support build trees with another layout.

### Tuner results

The output directory contains:

- `manifest.json`: host, processor, cache, launcher version, executable paths, and binary hashes.
- `commands.log` and `program-output.log`: exact reproduction commands and unmodified output.
- `corpus_raw.tsv`: all parsed `to_self` pack/unpack measurements.
- `controlled_raw.tsv` and `controlled_summary.tsv`: synthetic measurements and repeated medians.
- `loop_details.tsv` and `mover_details.tsv`: per-case speedups and classifications.
- `recommendations.json` and `report.md`: candidate policy values and compact supporting evidence.

When the measurements are decisive, the report names candidates for the consolidation threshold,
loop-unroll item and DATA-byte limits, pack vectorized block length, and unpack vectorized block-byte
limit. It leaves a candidate unset when the tested alternatives do not beat the configured noise
threshold consistently.

Positive `current_speedup_pct` in `mover_details.tsv` means the current type-labeled interpreter is
faster than the reference interpreter. Values within `--noise-pct` (3% by default) are not used as
evidence for a policy change.

The current/reference comparison changes only the prepared convertor's `fAdvance` function. Buffer
allocation, datatype construction, descriptor installation, and representation construction remain
outside the timed region; normal convertor preparation and cleanup remain inside because public
`MPI_Pack` and `MPI_Unpack` pay those costs.
