.. _release-notes-mpi-5.0-analysis-label:

MPI-5.0 conformance analysis
============================


This is a comprehensive analysis of Open MPI's conformance to the
`MPI-5.0 specification <https://www.mpi-forum.org/docs/mpi-5.0/mpi50-report.pdf>`_,
covering both **symbol coverage** (does Open MPI provide every required C and
``mpi_f08`` procedure and constant?) and **behavioral conformance** (does Open
MPI actually *do* what MPI-5.0 requires, across its build-time-configurable code
paths?).

.. admonition:: tl;dr
   :class: tip

   * Open MPI main implements the large majority of MPI-5.0, but **self-reports as MPI 4.1** (``MPI_VERSION``/``MPI_SUBVERSION`` and ``MPI_Get_version`` return 4/1; VERSION:23-24), so it does not yet advertise MPI-5.0 conformance.

   * **C API surface:** 537 of 567 MPI-5.0 procedures are declared in the normal ``mpi.h``. The 28 absent are exactly the MPI ABI surface, in-flight via PR #13280; ``MPI_F_sync_reg``/``MPI_Sizeof`` are Fortran-only.

   * **mpi_f08 gap:** the six ``MPI_Status_{get,set}_{source,tag,error}`` procedures have C bindings but no ``mpi_f08`` binding.

   * **Application Binary Interface (Chapter 20):** entirely absent on main; in-flight via PR #13280.

   * **Behavioral requirements:** 399 reconciled across all chapters - 327 supported (83.6% of applicable), 12 conditional, 23 partial, 4 unsupported, 25 missing-but-in-flight, 8 N/A.

   * **Regression flagged:** ``MPI_Testall`` with a real ``statuses[]`` array returns ``MPI_ERR_IN_STATUS`` even on full success - an inverted condition introduced 2025-10-15 by commit 38a7fbb837 (req_test.c:252). ``MPI_Waitall`` is unaffected.

How this report was produced
============================


This report is a **reconciliation of three independent conformance analyses** of
the same Open MPI source tree. Each of the three (referred to here as **b1**,
**b2**, and **b3**; git branches ``pr/main/mpi-conformance-analysis-1`` / ``-2`` /
``-3``) was an independent, AI-driven effort - each itself a *workload of multiple
agents* dividing the MPI-5.0 specification (chapter by chapter, plus symbol-surface
passes) - run from the same starting prompt against the same baseline commit
``71d6dcf7f4``. The three produced overlapping but differently-organized results,
using different status vocabularies and different requirement counts (b1 catalogued
372 behaviors, b2 257, b3 619 findings).

A fourth pass (this document) reconciled them:

#. **Normalize.** The three reviewers' different status vocabularies were mapped onto
   one common taxonomy (the legend below) so that "agreement" is well-defined.
#. **Align and de-duplicate.** Each reviewer's findings were matched - symbols by name,
   behaviors by topic per chapter - and the union was de-duplicated into one set of
   distinct requirements (a single gap that three chapters each mention is counted
   once, not three times).
#. **Adjudicate.** Where **all three agreed**, the result was accepted. Every
   **conflict** (reviewers disagreed) and every **unique** finding (raised by only
   one reviewer) was re-investigated here directly against the Open MPI source and
   the MPI-5.0 specification, and the correct result determined - sometimes
   overturning a reviewer (e.g. the ``MPI_Testall`` regression below was caught by
   only one reviewer, then verified in source and git history here).
#. **Recompute, don't inherit.** Symbol presence was recomputed directly from
   ``ompi/include/mpi.h.in`` and the Fortran sources against the machine-readable
   MPI-5.0 API list (567 procedures), dissolving the reviewers' differing
   denominators; and the headline percentages were recomputed from the reconciled
   rows rather than copied from any single reviewer's totals.

Every finding below carries all three reviewers' verdicts (``b1`` / ``b2`` / ``b3``,
or *silent* where a reviewer did not address it) and an **agreement** label, so the
provenance of each result is visible. The same data is available in machine-readable
form in :download:`mpi-5.0-conformance.json <mpi-5.0-conformance.json>`.

Status and agreement legends
============================


.. list-table::
   :header-rows: 1
   :widths: 22 78

   * - Status
     - Meaning
   * - SUPPORTED
     - Available in the default build/runtime (a non-default --disable-* flag may remove it; see gate).
   * - CONDITIONAL
     - Available only via a non-default optional dependency (optional library, explicitly-enabled configure flag, or optional compiler feature); see gate.
   * - PARTIAL
     - Present but incomplete or not fully standard-conformant.
   * - UNSUPPORTED
     - Required behavior genuinely absent, with no known in-flight work.
   * - MISSING_INFLIGHT
     - Absent on main but in-flight via a known PR/branch (ABI PR #13280; OMPIO MPI_Info branch, issue #13367).
   * - NA
     - Not applicable (advice-to-implementors text, or an explicitly optional feature).

.. list-table::
   :header-rows: 1
   :widths: 22 78

   * - Agreement
     - Meaning
   * - unanimous
     - All three reviewers that addressed the item agree (after vocabulary normalization).
   * - conflict
     - Reviewers disagreed; resolved here by direct source investigation.
   * - unique
     - Only one reviewer raised it; validated/​corrected here.
   * - partial-overlap
     - Two reviewers overlapped; the third was silent or scoped it elsewhere.

Symbol coverage
===============


**C procedures.** 537 of 567 MPI-5.0 procedures are declared in the normal ``mpi.h``
(99.6% excluding the in-flight ABI surface). The 28 absent procedures are *exactly*
the MPI ABI surface (6 ``MPI_Abi_*`` + 22 ``*_toint``/``*_fromint`` handle
converters), in-flight via PR #13280. ``MPI_F_sync_reg`` and ``MPI_Sizeof`` are
Fortran-only and correctly absent from C.

**mpi_f08 procedures.** One genuine gap: the six
``MPI_Status_{get,set}_{source,tag,error}`` procedures have C bindings but **no**
``mpi_f08`` binding (verified; reviewers b2 and b3 concur). The six ``MPI_Abi_*``
f08 bindings are part of the in-flight ABI work.

**Constants.** All MPI-5.0 datatype, op, error-class, comparison, and info-related
named constants are present. Only ``MPI_T_ERR_NOT_ACCESSIBLE`` and
``MPI_T_ERR_NOT_SUPPORTED`` are genuinely absent (verified; all three reviewers
concur). ``MPI_ABI_VERSION``/``MPI_ABI_SUBVERSION``/``MPI_ERR_ABI`` are in-flight
(PR #13280). Extended-precision Fortran KIND constants (``MPI_REAL16``,
``MPI_COMPLEX32``, ``MPI_INTEGER16``, ...) are present only when the Fortran
compiler supports those kinds (conditional).

Behavioral conformance
======================


Across all chapters, **399** distinct MPI-5.0 requirements/behaviors were
reconciled:

.. list-table::
   :header-rows: 1
   :widths: 34 12 54

   * - Status
     - Count
     -
   * - Supported (default build/runtime)
     - 327
     -
   * - Conditional (optional dependency)
     - 12
     -
   * - Partial (present but incomplete)
     - 23
     -
   * - Unsupported (genuine gap)
     - 4
     -
   * - Missing - in-flight (ABI / OMPIO info)
     - 25
     -
   * - N/A (advice / optional)
     - 8
     -

That is **327 of 391 applicable requirements (83.6%) fully supported** in the default
build. Setting aside the 25 requirements blocked on in-flight work (almost all the
ABI), **339 of the 366 currently-in-scope requirements (92.6%) are supported or
conditionally supported**; the remaining 27 are partial (23) or genuinely
unsupported (4).

Reviewer agreement across all findings: 177 unanimous, 77 conflicts resolved here by
direct source investigation, 95 unique findings validated, 50 partial-overlap.

Notable findings
================


**Regression - ``MPI_Testall`` returns a spurious error.** When ``MPI_Testall`` is
called with a real ``statuses[]`` array (not ``MPI_STATUSES_IGNORE``) and all
requests have completed successfully, it returns ``MPI_ERR_IN_STATUS`` - firing the
error handler - instead of ``MPI_SUCCESS``. The success/failure condition was
inverted at ``ompi/request/req_test.c:252`` (``MPI_SUCCESS ==`` should be ``!=``) by
commit ``38a7fbb837`` ("Fix for issue #13432", 2025-10-15). The same commit also
touched ``req_wait.c``, but ``MPI_Waitall`` (``req_wait.c:373``) and ``MPI_Testsome``
(``req_test.c:390``) keep the correct ``!=`` condition and are unaffected; only
``MPI_Testall`` is broken. It is doubly wrong - a spurious error on success and a
masked error on genuine failure. This is a correctness regression on ``main``,
filed as `open-mpi/ompi issue #13967
<https://github.com/open-mpi/ompi/issues/13967>`_; see Chapter 3.

**The tree advertises MPI 4.1.** ``MPI_VERSION``/``MPI_SUBVERSION`` and
``MPI_Get_version`` return 4/1 (``VERSION:23-24``), so despite implementing the
large majority of MPI-5.0, Open MPI ``main`` does not advertise MPI-5.0 conformance;
see Chapters 1 and 9.

**Genuine gaps (Unsupported).**

* ``mpi_f08`` bindings for ``MPI_Status_{get,set}_{source,tag,error}`` (Chapters 13, 19).
* MPI I/O rejects the required ``internal`` data representation, and
  ``MPI_Register_datarep`` (user-defined representations) is a non-functional stub
  (Chapter 14).
* ``MPI_T_ERR_NOT_ACCESSIBLE`` and ``MPI_T_ERR_NOT_SUPPORTED`` are undefined; the
  MPI_T **events** interface (new in MPI-4.0) is present as symbols but inert - no
  event sources are ever registered (Chapter 15).

**Other partial behaviors** include big-count truncation in ``MPI_Pack_c`` /
``MPI_Unpack_c`` / ``MPI_Pack_size_c`` / ``MPI_Get_count_c`` (Chapters 3, 5),
``MPI_ERRORS_ABORT`` crashing instead of aborting when set on a *session* (Chapter
9), ``MPI_Get_hw_resource_info`` returning an empty info (Chapter 9), and
``MPI_Win_get_info`` not synthesizing default hint values (Chapter 12).

Per-chapter summary
===================


.. list-table::
   :header-rows: 1
   :widths: 4 34 9 9 9 9 9 9

   * - #
     - Chapter
     - Sup
     - Cond
     - Part
     - Unsup
     - InFlt
     - N/A
   * - 1
     - :ref:`Introduction to MPI <release-notes-mpi-5.0-ch01>`
     - 4
     - 0
     - 1
     - 0
     - 1
     - 0
   * - 2
     - :ref:`MPI Terms and Conventions <release-notes-mpi-5.0-ch02>`
     - 19
     - 2
     - 0
     - 0
     - 0
     - 1
   * - 3
     - :ref:`Point-to-Point Communication <release-notes-mpi-5.0-ch03>`
     - 25
     - 1
     - 2
     - 0
     - 0
     - 0
   * - 4
     - :ref:`Partitioned Point-to-Point Communication <release-notes-mpi-5.0-ch04>`
     - 9
     - 0
     - 1
     - 0
     - 0
     - 2
   * - 5
     - :ref:`Datatypes <release-notes-mpi-5.0-ch05>`
     - 18
     - 2
     - 2
     - 0
     - 0
     - 0
   * - 6
     - :ref:`Collective Communication <release-notes-mpi-5.0-ch06>`
     - 17
     - 2
     - 2
     - 0
     - 0
     - 0
   * - 7
     - :ref:`Groups, Contexts, Communicators, and Caching <release-notes-mpi-5.0-ch07>`
     - 35
     - 0
     - 1
     - 0
     - 0
     - 0
   * - 8
     - :ref:`Virtual Topologies for MPI Processes <release-notes-mpi-5.0-ch08>`
     - 13
     - 0
     - 0
     - 0
     - 0
     - 0
   * - 9
     - :ref:`MPI Environmental Management <release-notes-mpi-5.0-ch09>`
     - 25
     - 0
     - 3
     - 0
     - 2
     - 0
   * - 10
     - :ref:`The Info Object <release-notes-mpi-5.0-ch10>`
     - 23
     - 0
     - 0
     - 0
     - 0
     - 1
   * - 11
     - :ref:`Process Initialization, Creation, and Management <release-notes-mpi-5.0-ch11>`
     - 28
     - 0
     - 1
     - 0
     - 2
     - 2
   * - 12
     - :ref:`One-Sided Communications (RMA) <release-notes-mpi-5.0-ch12>`
     - 29
     - 3
     - 1
     - 0
     - 0
     - 0
   * - 13
     - :ref:`External Interfaces <release-notes-mpi-5.0-ch13>`
     - 15
     - 0
     - 0
     - 0
     - 0
     - 0
   * - 14
     - :ref:`I/O <release-notes-mpi-5.0-ch14>`
     - 18
     - 0
     - 2
     - 2
     - 1
     - 0
   * - 15
     - :ref:`Tool Support <release-notes-mpi-5.0-ch15>`
     - 11
     - 0
     - 7
     - 1
     - 0
     - 1
   * - 16
     - :ref:`Deprecated Interfaces <release-notes-mpi-5.0-ch16>`
     - 11
     - 0
     - 0
     - 0
     - 0
     - 0
   * - 17
     - :ref:`Removed Interfaces <release-notes-mpi-5.0-ch17>`
     - 8
     - 0
     - 0
     - 0
     - 0
     - 0
   * - 18
     - :ref:`Semantic Changes and Warnings <release-notes-mpi-5.0-ch18>`
     - 2
     - 1
     - 0
     - 0
     - 0
     - 1
   * - 19
     - :ref:`Language Bindings <release-notes-mpi-5.0-ch19>`
     - 17
     - 1
     - 0
     - 1
     - 1
     - 0
   * - 20
     - :ref:`Application Binary Interface (ABI) <release-notes-mpi-5.0-ch20>`
     - 0
     - 0
     - 0
     - 0
     - 18
     - 0

Per-chapter details
===================


.. toctree::
   :maxdepth: 1

   mpi-5.0/01-introduction
   mpi-5.0/02-terms-conventions
   mpi-5.0/03-point-to-point
   mpi-5.0/04-partitioned
   mpi-5.0/05-datatypes
   mpi-5.0/06-collective
   mpi-5.0/07-groups-comm-caching
   mpi-5.0/08-topologies
   mpi-5.0/09-environmental
   mpi-5.0/10-info-object
   mpi-5.0/11-process-mgmt
   mpi-5.0/12-one-sided
   mpi-5.0/13-external-interfaces
   mpi-5.0/14-io
   mpi-5.0/15-tool-support
   mpi-5.0/16-deprecated
   mpi-5.0/17-removed
   mpi-5.0/18-semantic-changes
   mpi-5.0/19-language-bindings
   mpi-5.0/20-abi
