#!/usr/bin/env python3
#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

"""Unit tests for the pure helpers of the MPI ABI test runner.

These exercise the self-contained parsing, evaluation, classification,
and selection helpers that gate the whole ABI suite's correctness but do
not require an installed MPI, a launcher, or a compiler.  They run under
`make check` (wired into check-local) so a regression in any of these
functions is caught in CI.  Keep this module Python 3.7 compatible.
"""

import ast
import contextlib
import os
from pathlib import Path
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from _abi_common import (
    _check_counts, _count_by, _env_positive_int, _language_counts)
from _abi_cross import _cross_direction_selection, _cross_direction_summary
from _abi_discovery import _env_bool
from _abi_manifest import (
    _api_family, _detect_optional_features, _feature_requirement,
    _is_collective_op, _rank_requirement)
from _abi_probes import (
    _callback_api_coverage_audit, _eval_integer_expression_node,
    _extract_integer_constant, _normalize_c_signature_text,
    _optional_feature_skip_reason, _parse_c_header_prototypes,
    _parse_header_constant_names, _parse_header_constants)
from _abi_tables import LEGACY_ATTRIBUTE_API_NAMES


@contextlib.contextmanager
def _environ(name, value):
    """Temporarily set (or unset, if value is None) one environment var."""
    sentinel = object()
    previous = os.environ.get(name, sentinel)
    if value is None:
        os.environ.pop(name, None)
    else:
        os.environ[name] = value
    try:
        yield
    finally:
        if previous is sentinel:
            os.environ.pop(name, None)
        else:
            os.environ[name] = previous


@contextlib.contextmanager
def _header_file(text):
    """Write header text to a temporary file and yield its Path."""
    with tempfile.TemporaryDirectory() as tmp:
        path = Path(tmp) / "mpi.h"
        path.write_text(text, encoding="utf-8")
        yield path


class IntegerConstantTests(unittest.TestCase):
    def test_literals_and_expressions(self):
        cases = {
            "0": 0,
            "42": 42,
            "0x2A": 42,
            "0xFFu": 255,
            "1 << 8": 256,
            "(1 << 4)": 16,
            "0xff & 0x0f": 15,
            "1 | 4 | 16": 21,
            "3 + 4 * 2": 11,
            "-1": -1,
            "~0": -1,
            # Cast forms as they appear in the generated ABI header:
            # an outer-parenthesized ((Type) value) handle constant.
            "((int)0x10)": 16,
            "((MPI_Comm) 0x54000000)": 0x54000000,
        }
        for expr, expected in cases.items():
            with self.subTest(expr=expr):
                self.assertEqual(_extract_integer_constant(expr), expected)

    def test_identifier_bearing_expressions_reject(self):
        # Anything that would need a preprocessor model must return None
        # rather than pulling one integer token out of the wrong place.
        for expr in ("MPI_VERSION", "MPI_COMM_WORLD", "SOME_MACRO + 1",
                     "0x1p4", "1 / 0 is not parsed"):
            with self.subTest(expr=expr):
                self.assertIsNone(_extract_integer_constant(expr))

    def test_eval_node_directly(self):
        tree = ast.parse("(2 << 3) | 1", mode="eval")
        self.assertEqual(_eval_integer_expression_node(tree), 17)
        # A bare identifier node evaluates to None (unresolved).
        name_tree = ast.parse("FOO", mode="eval")
        self.assertIsNone(_eval_integer_expression_node(name_tree))


class HeaderConstantParsingTests(unittest.TestCase):
    HEADER = """\
#define MPI_TAG_UB 100
#define MPI_UNDEFINED (-1)
#define MPI_ALIAS MPI_TAG_UB
#define MPI_NOT_NUMERIC some_runtime_symbol
enum {
    MPI_IDENT = 0,
    MPI_CONGRUENT,
    MPI_SIMILAR,
    MPI_UNEQUAL = 5
};
"""

    def test_parse_header_constants_values(self):
        with _header_file(self.HEADER) as path:
            constants, unparsed = _parse_header_constants(path)
        self.assertEqual(constants["MPI_TAG_UB"], 100)
        self.assertEqual(constants["MPI_UNDEFINED"], -1)
        # Alias resolves to its target's value.
        self.assertEqual(constants["MPI_ALIAS"], 100)
        # Implicit enum continuation.
        self.assertEqual(constants["MPI_IDENT"], 0)
        self.assertEqual(constants["MPI_CONGRUENT"], 1)
        self.assertEqual(constants["MPI_SIMILAR"], 2)
        self.assertEqual(constants["MPI_UNEQUAL"], 5)
        # A non-numeric, non-alias #define is tracked as unparsed.
        self.assertIn("MPI_NOT_NUMERIC", unparsed)

    def test_parse_header_constant_names(self):
        with _header_file(self.HEADER) as path:
            names = _parse_header_constant_names(path)
        for expected in ("MPI_TAG_UB", "MPI_UNDEFINED", "MPI_ALIAS",
                         "MPI_IDENT", "MPI_CONGRUENT", "MPI_SIMILAR",
                         "MPI_UNEQUAL"):
            self.assertIn(expected, names)

    def test_constant_name_cache_is_isolated(self):
        # The memoized parser must not leak a mutable set between callers.
        with _header_file(self.HEADER) as path:
            first = _parse_header_constant_names(path)
            first.add("BOGUS")
            second = _parse_header_constant_names(path)
        self.assertNotIn("BOGUS", second)


class PrototypeParsingTests(unittest.TestCase):
    HEADER = """\
typedef int MPI_Fint;
int MPI_Send(const void *buf, int count);
OMPI_DECLSPEC int MPI_Recv(void *buf, int count);
extern int PMPI_Send(const void *buf, int count);
int MPI_Init(int *argc, char ***argv);
"""

    def test_prototypes_extracted(self):
        with _header_file(self.HEADER) as path:
            prototypes = _parse_c_header_prototypes(path)
        self.assertEqual(
            set(prototypes),
            {"MPI_Send", "MPI_Recv", "PMPI_Send", "MPI_Init"})
        self.assertEqual(prototypes["MPI_Send"]["return_type"], "int")
        # The typedef line must not be picked up as a prototype.
        self.assertNotIn("MPI_Fint", prototypes)

    def test_prototype_cache_is_isolated(self):
        # The memoized parser must not let a caller poison the cache by
        # mutating the returned map or its per-name entries.
        with _header_file(self.HEADER) as path:
            first = _parse_c_header_prototypes(path)
            first["MPI_Send"]["return_type"] = "MUTATED"
            first["BOGUS"] = {}
            second = _parse_c_header_prototypes(path)
        self.assertEqual(second["MPI_Send"]["return_type"], "int")
        self.assertNotIn("BOGUS", second)


class SignatureNormalizationTests(unittest.TestCase):
    def test_whitespace_collapse(self):
        self.assertEqual(
            _normalize_c_signature_text("int   MPI_Foo(void)"),
            "int MPI_Foo(void)")

    def test_argv_and_pointer_spacing(self):
        normalized = _normalize_c_signature_text(
            "int MPI_Init(int *argc, char argv[])")
        self.assertIn("char* argv[]", normalized)
        self.assertIn("int* argc", normalized)

    def test_idempotent(self):
        text = "int  MPI_Send ( const void * buf , int count ) ;"
        once = _normalize_c_signature_text(text)
        twice = _normalize_c_signature_text(once)
        self.assertEqual(once, twice)


class ApiFamilyTests(unittest.TestCase):
    def test_collective_variants(self):
        for name in ("MPI_Allreduce", "MPI_Iallreduce", "MPI_Ibarrier",
                     "MPI_Iscatterv", "MPI_Scatter", "MPI_Scatterv",
                     "MPI_Alltoallw", "MPI_Barrier_init"):
            with self.subTest(name=name):
                self.assertEqual(_api_family(name), "collective")
                self.assertEqual(
                    _rank_requirement(_api_family(name)), 2)

    def test_neighbor_is_not_collective(self):
        # Neighborhood collectives keep their own topology family.
        self.assertEqual(_api_family("MPI_Neighbor_allgather"), "neighbor")
        self.assertFalse(_is_collective_op("neighbor"))

    def test_other_families(self):
        self.assertEqual(_api_family("MPI_Comm_rank"), "comm")
        self.assertEqual(_api_family("MPI_Send"), "point_to_point")
        self.assertEqual(_api_family("MPI_Win_create"), "win")


class FeatureRequirementTests(unittest.TestCase):
    def test_canonical_feature_keys(self):
        # Keys must match _detect_optional_features()/skip-reason map.
        self.assertEqual(_feature_requirement("mpi_t"), "mpit_events")
        self.assertEqual(_feature_requirement("file"), "mpi_io")
        self.assertEqual(_feature_requirement("rma"), "rma")
        self.assertEqual(_feature_requirement("win"), "rma")
        # Sessions are always built, so the family is not gated.
        self.assertIsNone(_feature_requirement("session"))
        self.assertIsNone(_feature_requirement("comm"))

    def test_feature_keys_are_recognized(self):
        # Guard against drift between the keys _feature_requirement()
        # produces and the keys the detector/skip-reason maps recognize:
        # a literal-value test alone would not have caught the "mpi_t"
        # no-op gate that round 2 fixed.
        valid = set(_detect_optional_features(Path(".")))
        for family in ("mpi_t", "file", "rma", "win", "session", "comm"):
            key = _feature_requirement(family)
            if key is None:
                continue
            self.assertIn(key, valid)
            self.assertNotEqual(
                _optional_feature_skip_reason(key),
                "optional_feature_disabled")


class ResultAggregationTests(unittest.TestCase):
    def test_count_by_missing_key_bucket(self):
        entries = [{"result": "PASS"}, {"result": "PASS"},
                   {"result": "FAIL"}, {}]
        counts = _count_by(entries, "result")
        self.assertEqual(counts["PASS"], 2)
        self.assertEqual(counts["FAIL"], 1)
        # An entry missing the key tallies under the None bucket.
        self.assertEqual(counts[None], 1)

    def test_check_counts(self):
        checks = [{"result": "PASS"}, {"result": "SKIP"}, {"result": "PASS"}]
        self.assertEqual(_check_counts(checks), {"PASS": 2, "SKIP": 1})

    def test_language_counts(self):
        entries = [
            {"languages": {"c": True, "mpif.h": False,
                           "use mpi": True, "use mpi_f08": False}},
            {"languages": {"c": True, "mpif.h": True,
                           "use mpi": False, "use mpi_f08": False}},
        ]
        counts = _language_counts(entries)
        self.assertEqual(counts["c"], 2)
        self.assertEqual(counts["mpif.h"], 1)
        self.assertEqual(counts["use mpi"], 1)
        self.assertEqual(counts["use mpi_f08"], 0)


class CallbackCoverageAuditTests(unittest.TestCase):
    def _legacy_name(self):
        return sorted(LEGACY_ATTRIBUTE_API_NAMES)[0]

    def test_uncovered_legacy_name_counted_once(self):
        # A single authority records an implemented/declared/uncovered/
        # untolerated legacy attribute API exactly once.
        legacy = self._legacy_name()
        manifest = {"apis": [
            {"name": legacy, "classification": "implemented",
             "languages": {"c": True}, "callback": True},
        ]}
        header = "int {0}(void);\n".format(legacy)
        with _header_file(header) as path:
            result = _callback_api_coverage_audit(
                manifest, path, [], tolerated_legacy_names=set())
        counts = result["details"]["missing_by_package_counts"]
        self.assertEqual(
            counts.get("chunk10a_legacy_attribute_callbacks"), 1)

    def test_covered_legacy_name_not_flagged_missing(self):
        # A legacy name a callback probe covers must not be flagged as
        # missing (the double-count fix's second failure mode).
        legacy = self._legacy_name()
        manifest = {"apis": [
            {"name": legacy, "classification": "implemented",
             "languages": {"c": True}, "callback": True},
        ]}
        header = "int {0}(void);\n".format(legacy)
        cases = [{"name": "probe", "api_names": [legacy]}]
        with _header_file(header) as path:
            result = _callback_api_coverage_audit(
                manifest, path, cases, tolerated_legacy_names=set())
        counts = result["details"]["missing_by_package_counts"]
        self.assertNotIn("chunk10a_legacy_attribute_callbacks", counts)
        self.assertEqual(result["details"]["covered"], 1)


class EnvKnobTests(unittest.TestCase):
    def test_env_positive_int(self):
        with _environ("OMPI_ABI_TEST_TIMEOUT", None):
            self.assertEqual(_env_positive_int("OMPI_ABI_TEST_TIMEOUT", 30), 30)
        for raw, expected in (("45", 45), ("30s", 30), ("", 30),
                              ("-5", 30), ("0", 30), ("  12 ", 12)):
            with self.subTest(raw=raw):
                with _environ("OMPI_ABI_TEST_TIMEOUT", raw):
                    self.assertEqual(
                        _env_positive_int("OMPI_ABI_TEST_TIMEOUT", 30),
                        expected)

    def test_env_bool_tristate(self):
        with _environ("OMPI_ABI_TEST_KNOB", None):
            self.assertIsNone(_env_bool("OMPI_ABI_TEST_KNOB"))
        for raw in ("1", "true", "YES", "On"):
            with self.subTest(raw=raw):
                with _environ("OMPI_ABI_TEST_KNOB", raw):
                    self.assertTrue(_env_bool("OMPI_ABI_TEST_KNOB"))
        for raw in ("0", "false", "no", "OFF", ""):
            with self.subTest(raw=raw):
                with _environ("OMPI_ABI_TEST_KNOB", raw):
                    self.assertFalse(_env_bool("OMPI_ABI_TEST_KNOB"))

    def test_env_bool_rejects_typo(self):
        # A typo must be a loud error, not a silent False that disables
        # ABI testing.
        with _environ("OMPI_ABI_TEST_KNOB", "ture"):
            with self.assertRaises(RuntimeError):
                _env_bool("OMPI_ABI_TEST_KNOB")


class CrossDirectionSelectionTests(unittest.TestCase):
    def test_default_both(self):
        result = _cross_direction_selection("both")
        self.assertEqual(
            result["directions"],
            ["mpich_compile_open_mpi_run", "open_mpi_compile_mpich_run"])
        self.assertFalse(result["error"])

    def test_aliases(self):
        self.assertEqual(
            _cross_direction_selection("mpich-to-ompi")["directions"],
            ["mpich_compile_open_mpi_run"])
        self.assertEqual(
            _cross_direction_selection("ompi-to-mpich")["directions"],
            ["open_mpi_compile_mpich_run"])

    def test_typo_is_error(self):
        result = _cross_direction_selection("bogus")
        self.assertTrue(result["error"])
        self.assertEqual(result["invalid_values"], ["bogus"])
        self.assertEqual(result["directions"], [])

    def test_empty_is_error(self):
        result = _cross_direction_selection("")
        self.assertTrue(result["error"])
        self.assertIn("<empty>", result["invalid_values"])

    def test_partial_valid_and_dedup(self):
        result = _cross_direction_selection("mpich-to-ompi,typo")
        self.assertEqual(result["directions"], ["mpich_compile_open_mpi_run"])
        self.assertEqual(result["invalid_values"], ["typo"])
        deduped = _cross_direction_selection("both,mpich-to-ompi")
        self.assertEqual(len(deduped["directions"]), 2)


class CrossDirectionSummaryTests(unittest.TestCase):
    def _tools(self):
        return {
            "open_mpi": {"mpicc_abi": "/omp/mpicc_abi",
                         "mpirun": "/omp/mpirun"},
            "mpich": {"mpicc": "/mpich/mpicc", "mpirun": "/mpich/mpirun"},
            "cross": {
                "runtime_loader": "LD_LIBRARY_PATH",
                "directions": ["mpich_compile_open_mpi_run",
                               "open_mpi_compile_mpich_run"],
                "open_mpi_paths": {"library": "/omp/lib",
                                   "launcher_args": None},
                "mpich_paths": {"library": "/mpich/lib",
                                "launcher_args": "-x"},
            },
        }

    def test_roles_by_direction(self):
        summary = _cross_direction_summary(self._tools())

        forward = summary["mpich_compile_open_mpi_run"]
        self.assertEqual(forward["compile_implementation"], "mpich")
        self.assertEqual(forward["compile_wrapper"], "/mpich/mpicc")
        self.assertEqual(forward["run_implementation"], "open_mpi")
        self.assertEqual(forward["launcher"], "/omp/mpirun")
        self.assertEqual(forward["runtime_library_path"], "/omp/lib")
        self.assertIsNone(forward["launcher_args"])

        reverse = summary["open_mpi_compile_mpich_run"]
        self.assertEqual(reverse["compile_implementation"], "open_mpi")
        self.assertEqual(reverse["compile_wrapper"], "/omp/mpicc_abi")
        self.assertEqual(reverse["run_implementation"], "mpich")
        self.assertEqual(reverse["launcher"], "/mpich/mpirun")
        self.assertEqual(reverse["runtime_library_path"], "/mpich/lib")
        self.assertEqual(reverse["launcher_args"], "-x")


if __name__ == "__main__":
    unittest.main()
