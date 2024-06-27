# Copyright (c) 2024      Triad National Security, LLC. All rights
#                         reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
"""Fortran types and corresponding template code.

All types used in the Fortran bindings are defined here as classes that derive
from the FortranType base class. These are used for generating both Fortran and
supporting C code for the mpi_f08 bindings.
"""
from abc import ABC, abstractmethod
from ompi_bindings import compiler, consts, util


class FortranType(ABC):

    def __init__(self, name, fn_name, bigcount=False, ts=False, **kwargs):
        self.name = name
        self.fn_name = fn_name
        # Generate the bigcount interface version?
        self.bigcount = bigcount
        # Generate with support for TS 29113?
        self.ts = ts
        # List of dependent type/parameters, such as for counts
        self.dep_params = None
        self.used_counters = 0

    TYPES = {}

    @classmethod
    def add(cls, type_name):
        """Decorator for adding types."""
        def wrapper(class_):
            cls.TYPES[type_name] = class_
            return class_
        return wrapper

    @classmethod
    def get(cls, type_name):
        return cls.TYPES[type_name]

    @staticmethod
    def validate_dep_param_keys(param_name, keys):
        """Validate the keys that are allowed to be used for a dependent param."""
        # No dependent parameters allowed by default
        if keys:
            raise util.BindingError(
                f'Invalid keys found for parameter "{param_name}": {list(keys)}'
            )

    @property
    def fn_api_name(self):
        """Return the MPI API name to be used in error messages, etc.."""
        return util.ext_api_func_name(self.fn_name, bigcount=self.bigcount).upper()

    @property
    def tmp_name(self):
        """Return a temporary name for use in C."""
        return f'c_{self.name}'

    @property
    def tmp_name2(self):
        """Return a secondary temporary name for use in C."""
        return f'c_{self.name}2'

    def tmp_counter(self):
        """Get a temporary counter variable to be used in a loop."""
        name = f'{self.name}_i_{self.used_counters}'
        self.used_counters += 1
        return name

    def interface_predeclare(self):
        """Return predeclaration code, if required for the interface."""
        return ''

    @abstractmethod
    def declare(self):
        """Return a declaration for the type."""

    def declare_tmp(self):
        """Declare temporaries on in the subroutine."""
        return ''

    def declare_cbinding_fortran(self):
        """Return the C binding declaration as seen from Fortran."""
        return self.declare()

    def argument(self):
        """Return the value to pass as an argument."""
        return self.name

    def use(self):
        """Return list of (module, name) for a Fortran use-statement."""
        return []

    def post(self):
        """Return post-processing code to be run after the call."""
        return ''

    @abstractmethod
    def c_parameter(self):
        """Return the parameter expression to be used in the C function."""

    def c_declare_tmp(self):
        """Code to declare temporary variables for conversions, etc.."""
        return ''

    def c_shortcut_condition(self):
        """Shortcut conditional code.

        If the conditional evaluates to true in C, then code defined in
        c_shortcut_code() for all other parameters will be run and the
        underlying C function will not be called.
        """
        return None

    def c_shortcut_code(self):
        """Shortcut code to run if a parameter defines a shortcut condition."""
        return ''

    def c_prepare(self):
        """Code to be called before being passed to underlying C function."""
        return ''

    def c_argument(self):
        """Return the value to pass as an argument in the C code."""
        return self.name

    def c_post(self):
        """Code to be run after a call to the underlying C function."""
        return ''

#
# Definitions of generic types in Fortran and how these can be converted
# to and from C.
#

@FortranType.add('BUFFER')
class BufferType(FortranType):
    @staticmethod
    def validate_dep_param_keys(param_name, keys):
        util.validate_allowed_keys(keys, ['count', 'type', 'comm'], 'BUFFER', param_name)

    def interface_predeclare(self):
        return f'!OMPI_F08_IGNORE_TKR_PREDECL {self.name}'

    def declare(self):
        return f'OMPI_F08_IGNORE_TKR_TYPE, INTENT(IN) :: {self.name}'

    @property
    def tmp_datatype1(self):
        return self.dep_params['type'].tmp_name

    @property
    def tmp_datatype2(self):
        return self.dep_params['type'].tmp_name2

    @property
    def tmp_count(self):
        return self.dep_params['count'].tmp_name

    @property
    def tmp_comm(self):
        return self.dep_params['comm'].tmp_name

    def c_parameter(self):
        if self.ts:
            return f'CFI_cdesc_t *{self.name}'
        return f'char *{self.name}'

    def c_declare_tmp(self):
        if self.ts:
            type_name = self.dep_params['type'].name
            count_name = self.dep_params['count'].name
            if self.bigcount:
                count_init = f'MPI_Count {self.tmp_count} = *{count_name};'
            else:
                count_init = f'int {self.tmp_count} = OMPI_FINT_2_INT(*{count_name});'
            return util.prepare_text(f"""
            void *{self.tmp_name} = {self.name}->base_addr;
            {count_init}
            MPI_Datatype {self.tmp_datatype1} = PMPI_Type_f2c(*{type_name}), {self.tmp_datatype2};
            """)
        return ''

    def c_prepare(self):
        if self.ts:
            type_name = self.dep_params['type'].name
            return util.prepare_text(f"""
            OMPI_CFI_2_C({self.name}, {self.tmp_count}, {self.tmp_datatype1}, {self.tmp_datatype2}, {consts.C_ERROR_TMP_NAME});
            if (MPI_SUCCESS != {consts.C_ERROR_TMP_NAME}) {{
                *{consts.C_ERROR_NAME} = OMPI_INT_2_FINT({consts.C_ERROR_TMP_NAME});
                OMPI_ERRHANDLER_INVOKE({self.tmp_comm}, {consts.C_ERROR_TMP_NAME}, "{self.fn_api_name}");
                return;
            }}""")
        return ''

    def c_argument(self):
        if self.ts:
            return f'OMPI_F2C_BOTTOM({self.tmp_name})'
        return f'OMPI_F2C_BOTTOM({self.name})'

    def c_post(self):
        if self.ts:
            return util.prepare_text(f"""
            if ({self.tmp_datatype2} != {self.tmp_datatype1}) {{
                ompi_datatype_destroy(&{self.tmp_datatype2});
            }}""")
        return ''


@FortranType.add('BUFFER_ASYNC')
class BufferAsyncType(BufferType):
    def declare(self):
        return f'OMPI_F08_IGNORE_TKR_TYPE, INTENT(IN) OMPI_ASYNCHRONOUS :: {self.name}'


@FortranType.add('BUFFER_OUT')
class BufferOutType(BufferType):
    def declare(self):
        return f'OMPI_F08_IGNORE_TKR_TYPE :: {self.name}'


@FortranType.add('BUFFER_ASYNC_OUT')
class BufferAsyncOutType(BufferType):
    def declare(self):
        return f'OMPI_F08_IGNORE_TKR_TYPE OMPI_ASYNCHRONOUS :: {self.name}'


@FortranType.add('VBUFFER')
class VBufferType(FortranType):
    """Variable buffer type, as used by MPI_*v() functions."""
    @staticmethod
    def validate_dep_param_keys(param_name, keys):
        util.validate_allowed_keys(keys, ['counts', 'displs', 'type', 'comm'], 'VBUFFER', param_name)

    def declare(self):
        return f'OMPI_F08_IGNORE_TKR_TYPE, INTENT(IN) :: {self.name}'

    def c_parameter(self):
        if self.ts:
            return f'CFI_cdesc_t *{self.name}'
        return f'char *{self.name}'

    def c_declare_tmp(self):
        if self.ts:
            datatype = self.dep_params['type']
            # NOTE: Using tmp_name2 here for the datatype, since the datatype
            #       class assumes it will be used when TS support is enabled
            return util.prepare_text(f"""
            MPI_Datatype {datatype.tmp_name2} = NULL;
            char *{self.tmp_name} = {self.name}->base_addr;""")
        return ''

    def c_prepare(self):
        if self.ts:
            comm = self.dep_params['comm']
            datatype = self.dep_params['type'].name
            tmp_datatype = self.dep_params['type'].tmp_name2
            counts = self.dep_params['counts'].name
            displs = self.dep_params['displs'].name
            array_setup = ''
            if not self.bigcount:
                array_setup = f"""
                OMPI_ARRAY_FINT_2_INT({counts}, {comm.size});
                OMPI_ARRAY_FINT_2_INT({displs}, {comm.size});"""
            return util.prepare_text(f"""
            if (OMPI_COMM_IS_INTER({comm.tmp_name}) || !OMPI_IS_FORTRAN_IN_PLACE({self.tmp_name})) {{
                {tmp_datatype} = PMPI_Type_f2c(*{datatype});
                OMPI_CFI_CHECK_CONTIGUOUS({self.name}, {consts.C_ERROR_TMP_NAME});
                if (MPI_SUCCESS != {consts.C_ERROR_TMP_NAME}) {{
                    *{consts.C_ERROR_NAME} = OMPI_INT_2_FINT({consts.C_ERROR_TMP_NAME});
                    OMPI_ERRHANDLER_INVOKE({comm.tmp_name}, {consts.C_ERROR_TMP_NAME}, "{self.fn_name}");
                    return;
                }}
                {array_setup}
            }} else {{
                {self.tmp_name} = MPI_IN_PLACE;
            }}
            """)
        return ''

    def c_post(self):
        if self.ts and not self.bigcount:
            counts = self.dep_params['counts'].name
            displs = self.dep_params['displs'].name
            return util.prepare_text(f"""
            OMPI_ARRAY_FINT_2_INT_CLEANUP({counts});
            OMPI_ARRAY_FINT_2_INT_CLEANUP({displs});""")
        return ''

    def c_argument(self):
        name = self.tmp_name if self.ts else self.name
        return f'OMPI_F2C_BOTTOM(OMPI_F2C_IN_PLACE({name}))'


@FortranType.add('VBUFFER_OUT')
class VBufferType(FortranType):
    """Variable buffer receive type, as used by MPI_*v() functions."""
    @staticmethod
    def validate_dep_param_keys(param_name, keys):
        util.validate_allowed_keys(keys, ['type', 'comm'], 'VBUFFER_OUT', param_name)

    def declare(self):
        return f'OMPI_F08_IGNORE_TKR_TYPE :: {self.name}'

    def c_parameter(self):
        if self.ts:
            return f'CFI_cdesc_t *{self.name}'
        return f'char *{self.name}'

    def c_declare_tmp(self):
        if self.ts:
            datatype = self.dep_params['type']
            return util.prepare_text(f"""
            MPI_Datatype {datatype.tmp_name2} = PMPI_Type_f2c(*{datatype.name});
            char *{self.tmp_name} = {self.name}->base_addr;
            """)
        return ''

    def c_prepare(self):
        if self.ts:
            comm = self.dep_params['comm']
            return util.prepare_text(f"""
            OMPI_CFI_CHECK_CONTIGUOUS({self.name}, {consts.C_ERROR_TMP_NAME});
            if (MPI_SUCCESS != {consts.C_ERROR_TMP_NAME}) {{
                *{consts.C_ERROR_NAME} = OMPI_INT_2_FINT({consts.C_ERROR_TMP_NAME});
                OMPI_ERRHANDLER_INVOKE({comm.tmp_name}, {consts.C_ERROR_TMP_NAME}, "{self.fn_name}");
            }}
            """)
        return ''

    def c_argument(self):
        name = self.tmp_name if self.ts else self.name
        return f'OMPI_F2C_BOTTOM({name})'


class WBufferBase(FortranType):
    def c_prepare(self):
        if self.ts:
            c_ierr = consts.C_ERROR_TMP_NAME
            return util.prepare_text(f"""
            OMPI_CFI_CHECK_CONTIGUOUS({self.name}, {c_ierr});
            if (MPI_SUCCESS != {c_ierr}) {{
                *{consts.C_ERROR_NAME} = OMPI_INT_2_FINT({c_ierr});
                OMPI_ERRHANDLER_INVOKE({self.dep_params['comm'].tmp_name}, {c_ierr}, "{self.fn_name}")
                return;
            }}""")
        return ''


@FortranType.add('WBUFFER')
class WBufferType(WBufferBase):
    """Variable buffer send type, used with MPI_*w() functions."""
    @staticmethod
    def validate_dep_param_keys(param_name, keys):
        util.validate_allowed_keys(keys, ['counts', 'displs', 'types', 'comm'], 'WBUFFER',
                                   param_name)

    def declare(self):
        return f'OMPI_F08_IGNORE_TKR_TYPE, INTENT(IN) :: {self.name}'

    def c_parameter(self):
        if self.ts:
            return f'CFI_cdesc_t *{self.name}'
        else:
            return f'char *{self.name}'

    def c_declare_tmp(self):
        lines = [f'MPI_Datatype *{self.dep_params["types"].tmp_name} = NULL;']
        if self.ts:
            lines.append(f'char *{self.tmp_name} = {self.name}->base_addr;')
        return '\n'.join(lines)

    def c_prepare(self):
        first_part = super().c_prepare()
        datatypes = self.dep_params['types']
        comm = self.dep_params['comm']
        counts = self.dep_params['counts']
        displs = self.dep_params['displs']
        name = self.tmp_name if self.ts else self.name
        array_setup = ''
        if not self.bigcount:
            array_setup = """
            OMPI_ARRAY_FINT_2_INT({counts.name}, {comm.size});
            OMPI_ARRAY_FINT_2_INT({displs.name}, {comm.size});
            """
        second_part = util.prepare_text(f"""
        if (!OMPI_IS_FORTRAN_IN_PLACE({name})) {{
            {array_setup}
            {datatypes.tmp_name} = malloc({comm.size} * sizeof(MPI_Datatype));
            for (int i = 0; i < size; ++i) {{
                {datatypes.tmp_name}[i] = PMPI_Type_f2c({datatypes.name}[i]);
            }}
        }}""")
        return '\n'.join([first_part, second_part])

    def c_argument(self):
        name = self.tmp_name if self.ts else self.name
        return f'OMPI_F2C_BOTTOM(OMPI_F2C_IN_PLACE({name}))'

    def c_post(self):
        datatypes = self.dep_params['types']
        counts = self.dep_params['counts']
        displs = self.dep_params['displs']
        name = self.tmp_name if self.ts else self.name
        array_cleanup = ''
        if not self.bigcount:
            array_cleanup = """
            OMPI_ARRAY_FINT_2_INT_CLEANUP({counts.name});
            OMPI_ARRAY_FINT_2_INT_CLEANUP({displs.name});
            """
        return util.prepare_text(f"""
        if (!OMPI_IS_FORTRAN_IN_PLACE({name})) {{
            free({datatypes.tmp_name});
            {array_cleanup}
        }}""")


@FortranType.add('WBUFFER_OUT')
class WBufferType(WBufferBase):
    """Variable buffer receive type, used with MPI_*w() functions."""
    @staticmethod
    def validate_dep_param_keys(param_name, keys):
        util.validate_allowed_keys(keys, ['comm', 'counts', 'displs', 'types'], 'WBUFFER_OUT', param_name)

    def declare(self):
        return f'OMPI_F08_IGNORE_TKR_TYPE :: {self.name}'

    def c_parameter(self):
        if self.ts:
            return f'CFI_cdesc_t *{self.name}'
        return f'char *{self.name}'

    def c_declare_tmp(self):
        lines = [f'MPI_Datatype *{self.dep_params["types"].tmp_name} = NULL;']
        if self.ts:
            lines.append(f'char *{self.tmp_name} = {self.name}->base_addr;')
        return '\n'.join(lines)

    def c_prepare(self):
        first_part = super().c_prepare()
        comm = self.dep_params['comm']
        datatype = self.dep_params['types']
        counts = self.dep_params['counts']
        displs = self.dep_params['displs']
        second_part = util.prepare_text(f"""
        {datatype.tmp_name} = malloc({comm.size} * sizeof(MPI_Datatype));
        for (int i = 0; i < {comm.size}; ++i) {{
            {datatype.tmp_name}[i] = PMPI_Type_f2c({datatype.name}[i]);
        }}""")
        third_part = ''
        if not self.bigcount:
            third_part = util.prepare_text(f"""
            OMPI_ARRAY_FINT_2_INT({counts.name}, {comm.size});
            OMPI_ARRAY_FINT_2_INT({displs.name}, {comm.size});""")
        return '\n'.join([first_part, second_part, third_part])

    def c_argument(self):
        name = self.tmp_name if self.ts else self.name
        return f'OMPI_F2C_BOTTOM({name})'

    def c_post(self):
        datatypes = self.dep_params['types']
        counts = self.dep_params['counts']
        displs = self.dep_params['displs']
        array_cleanup = ''
        if not self.bigcount:
            util.prepare_text(f"""
            OMPI_ARRAY_FINT_2_INT_CLEANUP({counts.name});
            OMPI_ARRAY_FINT_2_INT_CLEANUP({displs.name});""")
        return util.prepare_text(f"""
        free({datatypes.tmp_name});
        {array_cleanup}""")


@FortranType.add('COUNT')
class CountType(FortranType):
    def declare(self):
        if self.bigcount:
            return f'INTEGER(KIND=MPI_COUNT_KIND), INTENT(IN) :: {self.name}'
        else:
            return f'INTEGER, INTENT(IN) :: {self.name}'

    def use(self):
        return [('mpi_f08_types', 'MPI_COUNT_KIND')]

    def c_parameter(self):
        type_ = 'MPI_Count' if self.bigcount else 'MPI_Fint'
        return f'{type_} *{self.name}'

    def c_argument(self):
        arg = self.tmp_name if self.ts else f'*{self.name}'
        return arg if self.bigcount else f'OMPI_FINT_2_INT({arg})'


@FortranType.add('DATATYPE')
class DatatypeType(FortranType):
    def declare(self):
        return f'TYPE(MPI_Datatype), INTENT(IN) :: {self.name}'

    def declare_cbinding_fortran(self):
        return f'INTEGER, INTENT(IN) :: {self.name}'

    def argument(self):
        return f'{self.name}%MPI_VAL'

    def use(self):
        return [('mpi_f08_types', 'MPI_Datatype')]

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'

    def c_prepare(self):
        if self.ts:
            # Preparation code is done by the BUFFER type
            return ''
        return f'MPI_Datatype {self.tmp_name} = PMPI_Type_f2c(*{self.name});'

    def c_argument(self):
        if self.ts:
            return self.tmp_name2
        return self.tmp_name


@FortranType.add('DATATYPE_ARRAY')
class DatatypeArrayType(FortranType):
    def declare(self):
        return f'TYPE(MPI_Datatype), INTENT(IN) :: {self.name}(*)'

    def use(self):
        return [('mpi_f08_types', 'MPI_Datatype')]

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'

    # Initialization is taken care of by the WBUFFER type

    def c_argument(self):
        return self.tmp_name


@FortranType.add('INT')
class IntType(FortranType):
    def declare(self):
        return f'INTEGER, INTENT(IN) :: {self.name}'

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'

    def c_argument(self):
        return f'OMPI_FINT_2_INT(*{self.name})'


@FortranType.add('RANK')
class RankType(IntType):
    pass


@FortranType.add('TAG')
class TagType(IntType):
    pass


@FortranType.add('INDEX_OUT')
class IndexOutType(IntType):
    def declare(self):
        return f'INTEGER, INTENT(OUT) :: {self.name}'

    def c_declare_tmp(self):
        return f'int {self.tmp_name};'

    def c_shortcut_code(self):
        return f'*{self.name} = OMPI_INT_2_FINT(MPI_UNDEFINED);'

    def c_argument(self):
        return f'&{self.tmp_name}'

    def c_post(self):
        return util.prepare_text(f"""
        if (MPI_SUCCESS == {consts.C_ERROR_TMP_NAME} && MPI_UNDEFINED != {self.tmp_name}) {{
            {self.tmp_name} += 1;
            *{self.name} = OMPI_INT_2_FINT({self.tmp_name});
        }}""")


@FortranType.add('LOGICAL_OUT')
class LogicalOutType(IntType):
    """Logical type.

    NOTE: Since the logical type causes difficulties when passed to C code,
    this code uses a temporary integer in Fortran to pass to the C code. On
    completion the logical type is set based on C's true/false rules.
    """

    def declare(self):
        return f'LOGICAL, INTENT(OUT) :: {self.name}'

    def declare_tmp(self):
        return f'INTEGER :: {self.tmp_name} = 0'

    def declare_cbinding_fortran(self):
        return f'INTEGER, INTENT(OUT) :: {self.name}'

    def argument(self):
        return self.tmp_name

    def post(self):
        return f'{self.name} = {self.tmp_name} /= 0'

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'

    def c_declare_tmp(self):
        return f'int {self.tmp_name};'

    def c_shortcut_code(self):
        return f'*{self.name} = OMPI_INT_2_FINT(1);'

    def c_argument(self):
        return f'&{self.tmp_name}'

    def c_post(self):
        return f'*{self.name} = OMPI_INT_2_FINT({self.tmp_name});'


# List of functions that need a communicator 'size' temporary
FUNCTIONS_NEEDING_COMM_SIZE_TEMP = [
    'alltoallv',
    'alltoallw',
]


@FortranType.add('COMM')
class CommType(FortranType):
    def declare(self):
        return f'TYPE(MPI_Comm), INTENT(IN) :: {self.name}'

    def declare_cbinding_fortran(self):
        return f'INTEGER, INTENT(IN) :: {self.name}'

    def argument(self):
        return f'{self.name}%MPI_VAL'

    def use(self):
        return [('mpi_f08_types', 'MPI_Comm')]

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'

    def c_declare_tmp(self):
        code = [f'MPI_Comm {self.tmp_name} = PMPI_Comm_f2c(*{self.name});']
        if self.fn_name in FUNCTIONS_NEEDING_COMM_SIZE_TEMP:
            code.append(f'int {self.size} = OMPI_COMM_IS_INTER({self.tmp_name}) ? '
                        f'ompi_comm_remote_size({self.tmp_name}) : ompi_comm_size({self.tmp_name});')
        return '\n'.join(code)

    def c_argument(self):
        return self.tmp_name

    @property
    def size(self):
        """Size property accessible by all dependent types."""
        return 'size'


@FortranType.add('STATUS')
class StatusType(FortranType):
    def declare(self):
        return f'TYPE(MPI_Status), INTENT(OUT) :: {self.name}'

    def use(self):
        return [('mpi_f08_types', 'MPI_Status')]

    def c_parameter(self):
        # TODO: Is this correct? (I've listed it as TYPE(MPI_Status) in the binding)
        return f'MPI_Fint *{self.name}'

    def c_shortcut_code(self):
        return f'PMPI_Status_c2f(&ompi_status_empty, {self.name});'

    def c_declare_tmp(self):
        return util.prepare_text(f"""
        OMPI_FORTRAN_STATUS_DECLARATION({self.tmp_name}, {self.tmp_name2});
        OMPI_FORTRAN_STATUS_SET_POINTER({self.tmp_name}, {self.tmp_name2}, {self.name});
        """)

    def c_argument(self):
        return self.tmp_name

    def c_post(self):
        return f'OMPI_FORTRAN_STATUS_RETURN({self.tmp_name}, {self.tmp_name2}, {self.name}, {consts.C_ERROR_TMP_NAME});'


@FortranType.add('SHORTCUT_COUNT')
class ShortcutCountType(FortranType):
    """Shortcut count type.

    This type is an integer that, when 0, can be used to shortcut a call to the
    underyling C binding. Other types may implement a `c_shortcut` method that
    will return code to execute upon a shortcut operation.

    The shortcut conditional is placed right after c temporary declarations but
    before the c prepare code.
    """

    def declare(self):
        return f'INTEGER, INTENT(IN) :: {self.name}'

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'

    def c_shortcut_condition(self):
        return f'OPAL_UNLIKELY(0 == OMPI_FINT_2_INT(*{self.name}))'

    def c_argument(self):
        return f'OMPI_FINT_2_INT(*{self.name})'


@FortranType.add('REQUEST')
class RequestType(FortranType):
    def declare(self):
        return f'TYPE(MPI_Request), INTENT(OUT) :: {self.name}'

    def declare_cbinding_fortran(self):
        return f'INTEGER, INTENT(OUT) :: {self.name}'

    def argument(self):
        return f'{self.name}%MPI_VAL'

    def use(self):
        return [('mpi_f08_types', 'MPI_Request')]

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'

    def c_declare_tmp(self):
        return f'MPI_Request {self.tmp_name};'

    def c_argument(self):
        return f'&{self.tmp_name}'

    def c_post(self):
        return util.prepare_text(f"""
        if (MPI_SUCCESS == {consts.C_ERROR_TMP_NAME}) {{
            *{self.name} = PMPI_Request_c2f({self.tmp_name});
        }}""")


def allocate_array(name, malloc_expr, fn_api_name):
    """Generate code for allocating an array and checking the result."""
    return util.prepare_text(f"""
    {name} = malloc({malloc_expr});
    if (NULL == {name}) {{
        {consts.C_ERROR_TMP_NAME} = OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_NO_MEM, "{fn_api_name}");
        *{consts.C_ERROR_NAME} = OMPI_INT_2_FINT({consts.C_ERROR_TMP_NAME});
        return;
    }}""")


@FortranType.add('REQUEST_ARRAY')
class RequestArrayType(FortranType):
    @staticmethod
    def validate_dep_param_keys(param_name, keys):
        util.validate_allowed_keys(keys, ['count'], 'REQUEST_ARRAY', param_name)

    def declare(self):
        return f'TYPE(MPI_Request), INTENT(INOUT) :: {self.name}({self.dep_params["count"].name})'

    def declare_cbinding_fortran(self):
        return f'INTEGER, INTENT(INOUT) :: {self.name}({self.dep_params["count"].name})'

    def argument(self):
        return f'{self.name}(:)%MPI_VAL'

    def use(self):
        return [('mpi_f08_types', 'MPI_Request')]

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'

    def c_declare_tmp(self):
        return f'MPI_Request *{self.tmp_name};'

    def c_prepare(self):
        tmp_name = self.tmp_name
        code = [allocate_array(tmp_name,
                              f'{self.dep_params["count"].c_argument()} * sizeof(MPI_Request)',
                              self.fn_api_name)]
        i = self.tmp_counter()
        code.append(util.prepare_text(f"""
        for (int {i} = 0; {i} < {self.dep_params["count"].c_argument()}; ++{i}) {{
            {tmp_name}[{i}] = PMPI_Request_f2c({self.name}[{i}]);
        }}"""))
        return '\n'.join(code)

    def c_argument(self):
        return self.tmp_name

    def c_post(self):
        i = self.tmp_counter()
        return util.prepare_text(f"""
        if (MPI_SUCCESS == {consts.C_ERROR_TMP_NAME}) {{
            for (int {i} = 0; {i} < {self.dep_params["count"].c_argument()}; ++{i}) {{
                {self.name}[{i}] = {self.tmp_name}[{i}]->req_f_to_c_index;
            }}
        }}
        free({self.tmp_name});""")


@FortranType.add('STATUS_ARRAY')
class StatusArrayType(FortranType):
    @staticmethod
    def validate_dep_param_keys(param_name, keys):
        util.validate_allowed_keys(keys, ['count'], 'STATUS_ARRAY', param_name)

    def declare(self):
        return f'TYPE(MPI_Status), INTENT(OUT) :: {self.name}(*)'

    def use(self):
        return [('mpi_f08_types', 'MPI_Status')]

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'

    def c_declare_tmp(self):
        return f'MPI_Status *{self.tmp_name};'

    def c_prepare(self):
        return allocate_array(self.tmp_name,
                              f'{self.dep_params["count"].c_argument()} * sizeof(MPI_Status)',
                              self.fn_api_name)

    def c_argument(self):
        return self.tmp_name

    def c_post(self):
        i = self.tmp_counter()
        return util.prepare_text(f"""
        if (MPI_SUCCESS == {consts.C_ERROR_TMP_NAME}) {{
            for (int {i} = 0; {i} < {self.dep_params["count"].c_argument()}; ++{i}) {{
                if (!OMPI_IS_FORTRAN_STATUSES_IGNORE({self.name}) &&
                    !OMPI_IS_FORTRAN_STATUS_IGNORE(&{self.name}[{i}])) {{
                    PMPI_Status_c2f(&{self.tmp_name}[{i}], &{self.name}[{i} * (sizeof(MPI_Status) / sizeof(int))]);
                }}
            }}
        }}
        free({self.tmp_name});""")


@FortranType.add('INT_ARRAY')
class IntArray(FortranType):
    """Integer array as used for MPI_*v() variable length functions."""

    @staticmethod
    def validate_dep_param_keys(param_name, keys):
        util.validate_allowed_keys(keys, ['comm'], 'INT_ARRAY', param_name)

    def declare(self):
        return f'INTEGER, INTENT(IN) :: {self.name}(*)'

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'

    def c_declare_tmp(self):
        if not self.bigcount:
            return f'OMPI_ARRAY_NAME_DECL({self.name});'
        return ''

    # NOTE: Most of this code is initialized and freed in the W/VBUFFER code

    def c_argument(self):
        if not self.bigcount:
            return f'OMPI_ARRAY_NAME_CONVERT({self.name})'
        return self.name


@FortranType.add('COUNT_ARRAY')
class CountArray(IntArray):
    """Array of MPI_Count or int."""

    def declare(self):
        kind = '(KIND=MPI_COUNT_KIND)' if self.bigcount else ''
        return f'INTEGER{kind}, INTENT(IN) :: {self.name}(*)'

    def use(self):
        if self.bigcount:
            return [('mpi_f08_types', 'MPI_COUNT_KIND')]
        return []

    def c_parameter(self):
        count_type = 'MPI_Count' if self.bigcount else 'MPI_Fint'
        return f'{count_type} *{self.name}'


@FortranType.add('DISPL_ARRAY')
class DisplArray(IntArray):
    """Array of MPI_Aint or int."""

    def declare(self):
        kind = '(KIND=MPI_ADDRESS_KIND)' if self.bigcount else ''
        return f'INTEGER{kind}, INTENT(IN) :: {self.name}(*)'

    def use(self):
        if self.bigcount:
            return [('mpi_f08_types', 'MPI_ADDRESS_KIND')]
        return []

    def c_parameter(self):
        count_type = 'MPI_Aint' if self.bigcount else 'MPI_Fint'
        return f'{count_type} *{self.name}'
