#!/usr/bin/env python3
#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

"""Unit tests for the MPI binding generator.

The generator under ompi/mpi/bindings/ produces both the Open MPI
bindings and the MPI Forum (standard) ABI bindings.  A defect in it can
silently emit malformed bindings that are only caught at compile time --
or, for a naming/casing mistake, not until run time.

The functions exercised here are pure and deterministic: parameter
parsing, name mangling, prototype classification, and replacement
generation.  The rendering tests additionally drive real .c.in templates
through both the 'ompi' and 'standard' code paths, which is the part most
likely to regress when the ABI type hierarchy is extended.

No MPI installation, launcher, or compiler is required, so this runs
under `make check` (wired into check-local) and therefore in CI.

Keep this module Python 3.7 compatible.
"""

import io
import os
from pathlib import Path
import re
import sys
import unittest

# The generator is not an installed package; add its directory to the path
# so that "from ompi_bindings import ..." resolves.  Everything is located
# relative to this file so that VPATH builds work.
_TEST_DIR = Path(os.path.dirname(os.path.abspath(__file__)))
# ompi/test/bindings-generator -> top of the source tree is three levels up.
_TOP_SRCDIR = _TEST_DIR.parent.parent.parent
_BINDINGS_DIR = _TOP_SRCDIR / 'ompi' / 'mpi' / 'bindings'
_C_TEMPLATE_DIR = _TOP_SRCDIR / 'ompi' / 'mpi' / 'c'
_MPI_H_IN = _TOP_SRCDIR / 'ompi' / 'include' / 'mpi.h.in'

sys.path.insert(0, str(_BINDINGS_DIR))

# c_header is the *other* generator: it emits the MPI Forum ABI mpi.h from
# pympistandard plus the ABI JSON, rather than from the .c.in templates.  It
# has a main() guard, so importing it here runs no work and needs neither
# pympistandard nor any command-line arguments.
import c_header                                         # noqa: E402
from ompi_bindings import c, consts, util               # noqa: E402
from ompi_bindings.c_type import Type                   # noqa: E402
from ompi_bindings.parser import Parameter, Prototype, ReturnType, SourceTemplate  # noqa: E402


def make_params(*texts):
    """Build a list of Parameters from 'TYPE name[:count[:outcount]]' text."""
    return [Parameter(text, type_constructor=Type.construct) for text in texts]


def make_prototype(name, *param_texts):
    """Build a Prototype with the given name and parameters."""
    return Prototype(name,
                     ReturnType('ERROR_CLASS', type_constructor=Type.construct),
                     make_params(*param_texts))


def render(template_name, abi_type):
    """Render a real .c.in template through the generator; return the text.

    Note that this drives c.ompi_abi() / c.standard_abi() directly, which is
    the code under test.  The '/* THIS FILE IS GENERATED ... */' banner is
    emitted by c.generate_source() around them, so it is not in this output.
    """
    template = SourceTemplate.load(str(_C_TEMPLATE_DIR / template_name),
                                   type_constructor=Type.construct)
    base_name = util.mpi_fn_name_from_base_fn_name(template.prototype.name)
    buf = io.StringIO()
    out = util.OutputFile(buf)
    if abi_type == 'ompi':
        c.ompi_abi(base_name, template, out)
    else:
        c.standard_abi(base_name, template, out)
    return buf.getvalue()


class TestParameterParsing(unittest.TestCase):
    """parser.Parameter understands the three 'name[:count[:outcount]]' forms."""

    def test_no_count(self):
        param, = make_params('COMM comm')
        self.assertEqual(param.type_name, 'COMM')
        self.assertEqual(param.name, 'comm')
        self.assertIsNone(param.count_param)
        self.assertIsNone(param.outcount_param)

    def test_count_only_defaults_outcount_to_count(self):
        # A single ':' means the output count is the same as the input
        # count.  This is deliberate, not an oversight: the ABI converter
        # emits "for (int i = 0; i < <outcount>; ++i)" when copying an
        # array back out, and for a parameter such as MPI_Waitsome's
        # "REQUEST_INOUT requests:incount" that bound is simply incount.
        # Do not "fix" this to None -- doing so emits a loop over NULL.
        param, = make_params('REQUEST_INOUT requests:incount')
        self.assertEqual(param.name, 'requests')
        self.assertEqual(param.count_param, 'incount')
        self.assertEqual(param.outcount_param, 'incount')

    def test_separate_outcount(self):
        # Two ':' gives a distinct output count.  Note that the outcount is
        # emitted verbatim into the generated loop bound, so the leading '*'
        # of MPI_Waitsome's "statuses:incount:*outcount" must be preserved:
        # the parameter is an int*, and the bound is the dereferenced value.
        param, = make_params('STATUS_OUT statuses:incount:*outcount')
        self.assertEqual(param.name, 'statuses')
        self.assertEqual(param.count_param, 'incount')
        self.assertEqual(param.outcount_param, '*outcount')

    def test_matches_the_real_waitsome_template(self):
        # Guard the parse of an actual in-tree prototype that uses all three
        # forms at once, so this test fails if waitsome.c.in and the parser
        # ever drift apart.
        template = SourceTemplate.load(str(_C_TEMPLATE_DIR / 'waitsome.c.in'),
                                       type_constructor=Type.construct)
        by_name = {p.name: p for p in template.prototype.params}
        self.assertIsNone(by_name['incount'].count_param)
        self.assertEqual(by_name['requests'].count_param, 'incount')
        self.assertEqual(by_name['requests'].outcount_param, 'incount')
        self.assertEqual(by_name['statuses'].count_param, 'incount')
        self.assertEqual(by_name['statuses'].outcount_param, '*outcount')

    def test_construct_forwards_counts_to_the_type(self):
        param, = make_params('STATUS_OUT statuses:incount:*outcount')
        constructed = param.construct(abi_type='standard')
        self.assertEqual(constructed.name, 'statuses')
        self.assertEqual(constructed.count_param, 'incount')
        self.assertEqual(constructed.outcount_param, '*outcount')


class TestNameMangling(unittest.TestCase):
    """The ABI name-mangling helpers.  Casing/suffix defects land here."""

    def test_abi_internal_name(self):
        self.assertEqual(util.abi_internal_name('MPI_MAX_INFO_KEY'),
                         'MPI_MAX_INFO_KEY_ABI_INTERNAL')

    def test_abi_tmp_name(self):
        self.assertEqual(util.abi_tmp_name('comm'), 'comm_tmp')

    def test_mpi_fn_name_from_base_fn_name(self):
        self.assertEqual(util.mpi_fn_name_from_base_fn_name('send'), 'MPI_Send')

    def test_mpit_fn_name_from_base_fn_name(self):
        self.assertEqual(util.mpit_fn_name_from_base_fn_name('category_get_info'),
                         'MPI_T_category_get_info')

    def test_ext_api_func_name_bigcount_suffix(self):
        self.assertEqual(util.ext_api_func_name('send'), 'MPI_Send')
        self.assertEqual(util.ext_api_func_name('send', bigcount=True), 'MPI_Send_c')

    def test_ext_api_func_name_profile_prefix(self):
        self.assertEqual(util.ext_api_func_name_profile('send'), 'PMPI_Send')
        self.assertEqual(util.ext_api_func_name_profile('send', bigcount=True),
                         'PMPI_Send_c')


class TestPrototypeClassification(unittest.TestCase):
    """The predicates that decide which variants get generated."""

    def test_has_bigcount(self):
        self.assertTrue(util.prototype_has_bigcount(
            make_prototype('send', 'BUFFER_CONST buf', 'COUNT count', 'COMM comm')))

    def test_has_no_bigcount(self):
        self.assertFalse(util.prototype_has_bigcount(
            make_prototype('comm_rank', 'COMM comm', 'INT_OUT rank')))

    def test_has_buffers_is_a_fortran_side_predicate(self):
        # prototype_has_buffers() is only consumed by fortran.py, and its
        # BUFFER_TYPE_NAMES list is the *Fortran* template vocabulary
        # (BUFFER / BUFFER_ASYNC / BUFFER_OUT / BUFFER_ASYNC_OUT).  The C
        # templates spell the send buffer BUFFER_CONST, which is deliberately
        # absent from that list: it is not a Fortran type name.  Do not "fix"
        # this by adding BUFFER_CONST -- that would change which Fortran
        # bindings get the TS/async treatment.
        self.assertTrue(util.prototype_has_buffers(
            make_prototype('send', 'BUFFER buf', 'COUNT count')))
        self.assertTrue(util.prototype_has_buffers(
            make_prototype('isend', 'BUFFER_ASYNC buf', 'COUNT count')))
        self.assertFalse(util.prototype_has_buffers(
            make_prototype('comm_rank', 'COMM comm', 'INT_OUT rank')))

    def test_needs_callback_wrappers(self):
        self.assertTrue(util.prototype_needs_callback_wrappers(
            make_prototype('comm_create_keyval',
                           'COMM_COPY_ATTR_FUNCTION comm_copy_attr_fn',
                           'COMM_DELETE_ATTR_FUNCTION comm_delete_attr_fn',
                           'ATTR_KEY_OUT comm_keyval')))

    def test_does_not_need_callback_wrappers(self):
        self.assertFalse(util.prototype_needs_callback_wrappers(
            make_prototype('send', 'BUFFER_CONST buf', 'COUNT count', 'COMM comm')))


class TestGenerateReplacements(unittest.TestCase):
    """c.generate_replacements() maps the MPI_MAX_* constants."""

    def test_unmangled_is_identity(self):
        replacements = c.generate_replacements(mangle_names=False)
        self.assertEqual(sorted(replacements), sorted(consts.MAX_STRING_LEN_CONSTANTS))
        for key, value in replacements.items():
            self.assertEqual(key, value)

    def test_mangled_appends_abi_internal(self):
        replacements = c.generate_replacements(mangle_names=True)
        self.assertEqual(sorted(replacements), sorted(consts.MAX_STRING_LEN_CONSTANTS))
        for key, value in replacements.items():
            self.assertEqual(value, '{}_ABI_INTERNAL'.format(key))

    def test_covers_the_constants_the_templates_actually_use(self):
        # The body substitution is '@KEY@' -> value, so a constant that a
        # template references but that is missing from MAX_STRING_LEN_CONSTANTS
        # would be emitted verbatim into the standard ABI source and fail to
        # compile (or, worse, resolve to the wrong value).
        self.assertIn('MPI_MAX_ERROR_STRING', consts.MAX_STRING_LEN_CONSTANTS)
        self.assertIn('MPI_MAX_OBJECT_NAME', consts.MAX_STRING_LEN_CONSTANTS)


class TestTypeRegistry(unittest.TestCase):
    """The Type.add_type() registry backing both ABIs."""

    def test_both_registries_are_populated(self):
        self.assertTrue(Type.PARAMS_OMPI_ABI)
        self.assertTrue(Type.PARAMS_STANDARD_ABI)

    def test_construct_dispatches_per_abi(self):
        ompi_comm = Type.construct('ompi', 'COMM', name='comm')
        standard_comm = Type.construct('standard', 'COMM', name='comm')
        self.assertEqual(ompi_comm.name, 'comm')
        self.assertEqual(standard_comm.name, 'comm')
        # The standard ABI has to convert the handle, so it must use a
        # distinct class from the OMPI one.
        self.assertIsNot(type(ompi_comm), type(standard_comm))

    def test_construct_rejects_an_unknown_abi(self):
        with self.assertRaises(RuntimeError):
            Type.construct('nonesuch', 'COMM', name='comm')

    def test_standard_abi_types_use_a_tmp_argument(self):
        # StandardABIType passes a converted temporary to the back end rather
        # than the user's handle; if this regressed, the generated code would
        # hand an ABI handle straight to the OMPI internals.
        standard_comm = Type.construct('standard', 'COMM', name='comm')
        self.assertEqual(standard_comm.tmpname, 'comm_tmp')
        self.assertEqual(standard_comm.argument, 'comm_tmp')

    def test_every_standard_type_is_also_an_ompi_type(self):
        # A type registered only for the standard ABI could never be used by a
        # template, since every template is generated for both ABIs.
        missing = set(Type.PARAMS_STANDARD_ABI) - set(Type.PARAMS_OMPI_ABI)
        self.assertEqual(missing, set())


class TestTemplateRendering(unittest.TestCase):
    """Real templates render through both ABI paths without raising."""

    # Chosen to cover the interesting generator paths: a plain
    # buffer/count function (bigcount variants), one with user callbacks
    # (callback wrapper emission), and one with a separate outcount (the
    # array copy-back loop).
    TEMPLATES = {
        'send.c.in': 'MPI_Send',
        'comm_create_keyval.c.in': 'MPI_Comm_create_keyval',
        'waitsome.c.in': 'MPI_Waitsome',
    }

    def test_renders_for_both_abis(self):
        for template_name, public_name in sorted(self.TEMPLATES.items()):
            for abi_type in ('ompi', 'standard'):
                with self.subTest(template=template_name, abi=abi_type):
                    text = render(template_name, abi_type)
                    self.assertTrue(text.strip())
                    # Both ABIs must define the public entry point...
                    self.assertIn(public_name, text)
                    # ...and the profiling alias must be set up for it.
                    self.assertIn('P' + public_name, text)

    def test_ompi_render_emits_the_public_symbol(self):
        text = render('send.c.in', 'ompi')
        self.assertIn('MPI_Send', text)

    def test_bigcount_variant_is_emitted_only_when_applicable(self):
        # send() has a COUNT, so it gets the _c variant; comm_create_keyval()
        # does not.
        self.assertIn('MPI_Send_c', render('send.c.in', 'ompi'))
        self.assertNotIn('MPI_Comm_create_keyval_c',
                         render('comm_create_keyval.c.in', 'ompi'))

    def test_standard_render_uses_the_internal_back_end_name(self):
        # The standard ABI entry point must call the internal ompi_abi_*
        # shim, never the public MPI_* symbol of the upper layer.
        text = render('send.c.in', 'standard')
        self.assertIn('ompi_abi_send', text)

    def test_standard_render_emits_callback_wrappers(self):
        # comm_create_keyval takes user callbacks, which the standard ABI must
        # wrap so that the user's function sees ABI handles.
        text = render('comm_create_keyval.c.in', 'standard')
        self.assertIn('MPI_Comm_create_keyval', text)
        self.assertIn('_ABI_INTERNAL', text)

    def test_standard_render_uses_the_outcount_as_the_copy_back_bound(self):
        # MPI_Waitsome writes only *outcount entries, so the ABI status
        # copy-back loop must be bounded by *outcount, not by incount.
        text = render('waitsome.c.in', 'standard')
        self.assertIn('*outcount', text)


class TestSharedDomainKnowledge(unittest.TestCase):
    """The two ABI generators must agree, and must agree with mpi.h.

    The binding generator (bindings.py / ompi_bindings) is driven by the
    .c.in templates; the ABI header generator (c_header.py) is driven by
    pympistandard plus the ABI JSON.  Anything they both depend on lives in
    ompi_bindings/consts.py so that it cannot be defined twice and drift.
    These tests fail if someone reintroduces a second copy.
    """

    def test_the_two_generators_share_one_abi_suffix(self):
        self.assertEqual(c_header.ABI_INTERNAL, consts.ABI_INTERNAL_SUFFIX)
        # ...and util.abi_internal_name() must apply that same suffix, since
        # the emitted bindings have to match the emitted header exactly.
        self.assertEqual(util.abi_internal_name('MPI_Comm'),
                         'MPI_Comm' + consts.ABI_INTERNAL_SUFFIX)

    def test_the_two_generators_share_one_deprecated_list(self):
        self.assertIs(c_header.DEPRECATED_FUNCTIONS, consts.DEPRECATED_FUNCTIONS)

    def test_the_abi_header_mangles_every_shared_handle_type(self):
        # c_header.INTERNAL_DATATYPES legitimately carries extra entries of its
        # own (MPI_Status, the callback typedefs, the MPI_T types), but it must
        # cover every opaque handle the binding generator knows about, or the
        # header and the bindings would disagree about which types are mangled.
        missing = set(consts.C_HANDLES) - set(c_header.INTERNAL_DATATYPES)
        self.assertEqual(missing, set())


class TestDeprecatedFunctionsMatchMpiH(unittest.TestCase):
    """consts.DEPRECATED_FUNCTIONS must not drift away from mpi.h.in.

    The list says which symbols the MPI Forum has deprecated or removed, and
    is what prunes them out of the generated Forum ABI header.  Open MPI
    separately records the same fact in ompi/include/mpi.h.in, by marking the
    functions __mpi_interface_deprecated__ / __mpi_interface_removed__.  If
    those two ever disagree, the ABI header is wrong.
    """

    # These entries are constants, not functions, so they cannot carry a
    # deprecation *attribute* in mpi.h.in -- but they must still exist there.
    NON_FUNCTION_ENTRIES = frozenset([
        'MPI_LB',
        'MPI_UB',
        'MPI_COMBINER_HVECTOR_INTEGER',
        'MPI_COMBINER_HINDEXED_INTEGER',
        'MPI_COMBINER_STRUCT_INTEGER',
    ])

    @classmethod
    def setUpClass(cls):
        with open(str(_MPI_H_IN)) as fp:
            cls.mpi_h = fp.read()

    def test_every_entry_exists_in_mpi_h(self):
        # A name that Open MPI does not actually provide has no business being
        # in a list whose job is to *remove* things from the ABI header.
        for name in consts.DEPRECATED_FUNCTIONS:
            with self.subTest(name=name):
                self.assertRegex(self.mpi_h, r'\b%s\b' % re.escape(name))

    def test_every_deprecated_function_is_marked_in_mpi_h(self):
        for name in consts.DEPRECATED_FUNCTIONS:
            if name in self.NON_FUNCTION_ENTRIES:
                continue
            with self.subTest(name=name):
                declarations = re.findall(r'\b%s\b[^;]{0,600}?;' % re.escape(name),
                                          self.mpi_h, re.S)
                self.assertTrue(
                    any('__mpi_interface_deprecated__' in decl
                        or '__mpi_interface_removed__' in decl
                        for decl in declarations),
                    '%s is pruned from the MPI Forum ABI header as deprecated, '
                    'but mpi.h.in does not mark it deprecated or removed' % name)

    def test_the_non_function_entries_really_are_not_functions(self):
        # Guard the exemption list above: if one of these ever becomes a
        # function declaration, it should be checked like the rest.
        for name in self.NON_FUNCTION_ENTRIES:
            with self.subTest(name=name):
                self.assertNotRegex(self.mpi_h,
                                    r'OMPI_DECLSPEC\s+\w+\s+%s\s*\(' % re.escape(name))


if __name__ == '__main__':
    unittest.main()
