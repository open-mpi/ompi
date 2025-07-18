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
from ompi_bindings import consts, util


class FortranType(ABC):

    def __init__(self, name, fn_name, bigcount=False, count_param=None, **kwargs):
        self.name = name
        self.fn_name = fn_name
        # Generate the bigcount interface version?
        self.bigcount = bigcount
        self.count_param = count_param
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

    @classmethod
    def construct(cls, type_name, **kwargs):
        type_ = cls.TYPES[type_name]
        return type_(**kwargs)

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

#
# Definitions of generic types in Fortran and how these can be converted
# to and from C.
#

@FortranType.add('BUFFER')
class BufferType(FortranType):
    def interface_predeclare(self):
        return f'!OMPI_F08_IGNORE_TKR_PREDECL {self.name}'

    def declare(self):
        return f'OMPI_F08_IGNORE_TKR_TYPE, INTENT(IN) :: {self.name}'

    def c_parameter(self):
        # See fortran/use-mpi-f08/base/ts.h; OMPI_CFI_BUFFER is expanded based
        # on whether or not the compiler supports TS 29113.
        return f'OMPI_CFI_BUFFER *{self.name}'


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
    def declare(self):
        return f'OMPI_F08_IGNORE_TKR_TYPE, INTENT(IN) :: {self.name}'

    def c_parameter(self):
        return f'OMPI_CFI_BUFFER *{self.name}'


@FortranType.add('VBUFFER_OUT')
class VBufferType(FortranType):
    """Variable buffer receive type, as used by MPI_*v() functions."""
    def declare(self):
        return f'OMPI_F08_IGNORE_TKR_TYPE :: {self.name}'

    def c_parameter(self):
        return f'OMPI_CFI_BUFFER *{self.name}'


@FortranType.add('WBUFFER')
class WBufferType(FortranType):
    """Variable buffer send type, used with MPI_*w() functions."""
    def declare(self):
        return f'OMPI_F08_IGNORE_TKR_TYPE, INTENT(IN) :: {self.name}'

    def c_parameter(self):
        return f'OMPI_CFI_BUFFER *{self.name}'


@FortranType.add('WBUFFER_OUT')
class WBufferType(FortranType):
    """Variable buffer receive type, used with MPI_*w() functions."""
    def declare(self):
        return f'OMPI_F08_IGNORE_TKR_TYPE :: {self.name}'

    def c_parameter(self):
        return f'OMPI_CFI_BUFFER *{self.name}'


@FortranType.add('C_PTR_OUT')
class CptrType(FortranType):
    def declare(self):
        return f'TYPE(C_PTR), INTENT(OUT) :: {self.name}'

    def use(self):
        return [('ISO_C_BINDING', 'C_PTR')]

    def c_parameter(self):
        return f'char *{self.name}'

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

@FortranType.add('COUNT_INOUT')
class CountTypeInOut(FortranType):
    """COUNT type with INOUT INTENT"""
    def declare(self):
        if self.bigcount:
            return f'INTEGER(KIND=MPI_COUNT_KIND), INTENT(INOUT) :: {self.name}'
        else:
            return f'INTEGER, INTENT(INOUT) :: {self.name}'

    def use(self):
        return [('mpi_f08_types', 'MPI_COUNT_KIND')]

    def c_parameter(self):
        type_ = 'MPI_Count' if self.bigcount else 'MPI_Fint'
        return f'{type_} *{self.name}'

@FortranType.add('COUNT_OUT')
class CountTypeInOut(FortranType):
    """COUNT type with OUT INTENT"""
    def declare(self):
        if self.bigcount:
            return f'INTEGER(KIND=MPI_COUNT_KIND), INTENT(OUT) :: {self.name}'
        else:
            return f'INTEGER, INTENT(OUT) :: {self.name}'

    def use(self):
        return [('mpi_f08_types', 'MPI_COUNT_KIND')]

    def c_parameter(self):
        type_ = 'MPI_Count' if self.bigcount else 'MPI_Fint'
        return f'{type_} *{self.name}'


@FortranType.add('PARTITIONED_COUNT')
class PartitionedCountType(FortranType):
    def declare(self):
            return f'INTEGER(KIND=MPI_COUNT_KIND), INTENT(IN) :: {self.name}'

    def use(self):
        return [('mpi_f08_types', 'MPI_COUNT_KIND')]

    def c_parameter(self):
        return f'MPI_Count *{self.name}'


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

@FortranType.add('DATATYPE_OUT')
class DatatypeTypeOut(FortranType):
    def declare(self):
        return f'TYPE(MPI_Datatype), INTENT(OUT) :: {self.name}'

    def declare_cbinding_fortran(self):
        return f'INTEGER, INTENT(OUT) :: {self.name}'

    def argument(self):
        return f'{self.name}%MPI_VAL'

    def use(self):
        return [('mpi_f08_types', 'MPI_Datatype')]

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'



@FortranType.add('DATATYPE_ARRAY')
class DatatypeArrayType(FortranType):
    def declare(self):
        return f'TYPE(MPI_Datatype), INTENT(IN) :: {self.name}(*)'

    def use(self):
        return [('mpi_f08_types', 'MPI_Datatype')]

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'


@FortranType.add('INT')
class IntType(FortranType):
    def declare(self):
        return f'INTEGER, INTENT(IN) :: {self.name}'

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'


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


@FortranType.add('STATUS')
class StatusType(FortranType):
    def declare(self):
        return f'TYPE(MPI_Status) :: {self.name}'

    def use(self):
        return [('mpi_f08_types', 'MPI_Status')]

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'


@FortranType.add('STATUS_OUT')
class StatusOutType(FortranType):
    def declare(self):
        return f'TYPE(MPI_Status), INTENT(OUT) :: {self.name}'

    def use(self):
        return [('mpi_f08_types', 'MPI_Status')]

    def c_parameter(self):
        # TODO: Is this correct? (I've listed it as TYPE(MPI_Status) in the binding)
        return f'MPI_Fint *{self.name}'


@FortranType.add('REQUEST_OUT')
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


@FortranType.add('REQUEST_ARRAY')
class RequestArrayType(FortranType):
    def declare(self):
        return f'TYPE(MPI_Request), INTENT(INOUT) :: {self.name}({self.count_param})'

    def declare_cbinding_fortran(self):
        return f'INTEGER, INTENT(INOUT) :: {self.name}({self.count_param})'

    def argument(self):
        return f'{self.name}(:)%MPI_VAL'

    def use(self):
        return [('mpi_f08_types', 'MPI_Request')]

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'


@FortranType.add('STATUS_ARRAY')
class StatusArrayType(FortranType):
    def declare(self):
        return f'TYPE(MPI_Status), INTENT(OUT) :: {self.name}(*)'

    def use(self):
        return [('mpi_f08_types', 'MPI_Status')]

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'


@FortranType.add('INT_ARRAY')
class IntArray(FortranType):
    """Integer array as used for MPI_*v() variable length functions."""

    def declare(self):
        return f'INTEGER, INTENT(IN) :: {self.name}(*)'

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'


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

@FortranType.add('AINT_COUNT_ARRAY')
class CountArray(IntArray):
    """Array of MPI_Count or int."""

    def declare(self):
        kind = '(KIND=MPI_COUNT_KIND)' if self.bigcount else '(KIND=MPI_ADDRESS_KIND)'
        return f'INTEGER{kind}, INTENT(IN) :: {self.name}(*)'

    def use(self):
        if self.bigcount:
            return [('mpi_f08_types', 'MPI_COUNT_KIND')]
        else:
            return [('mpi_f08_types', 'MPI_ADDRESS_KIND')]

    def c_parameter(self):
        count_type = 'MPI_Count' if self.bigcount else 'MPI_Aint'
        return f'{count_type} *{self.name}'



@FortranType.add('AINT')
class Aint(FortranType):
    """MPI_Aint type."""

    def declare(self):
        return f'INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: {self.name}'

    def use(self):
        return [('mpi_f08_types', 'MPI_ADDRESS_KIND')]

    def c_parameter(self):
        return f'MPI_Aint *{self.name}'


@FortranType.add('AINT_OUT')
class AintOut(FortranType):
    """MPI_Aint out type."""

    def declare(self):
        return f'INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: {self.name}'

    def use(self):
        return [('mpi_f08_types', 'MPI_ADDRESS_KIND')]

    def c_parameter(self):
        return f'MPI_Aint *{self.name}'


@FortranType.add('AINT_COUNT')
class AintCountTypeIn(FortranType):
    """AINT/COUNT type with ININTENT"""
    def declare(self):
        if self.bigcount:
            return f'INTEGER(KIND=MPI_COUNT_KIND), INTENT(IN) :: {self.name}'
        else:
            return f'INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: {self.name}'

    def use(self):
        if self.bigcount:
            return [('mpi_f08_types', 'MPI_COUNT_KIND')]
        else:
            return [('mpi_f08_types', 'MPI_ADDRESS_KIND')]

    def c_parameter(self):
        type_ = 'MPI_Count' if self.bigcount else 'MPI_Aint'
        return f'{type_} *{self.name}'


@FortranType.add('AINT_COUNT_INOUT')
class AintCountTypeInOut(FortranType):
    """AINT/COUNT type with INOUT INTENT"""
    def declare(self):
        if self.bigcount:
            return f'INTEGER(KIND=MPI_COUNT_KIND), INTENT(INOUT) :: {self.name}'
        else:
            return f'INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(INOUT) :: {self.name}'

    def use(self):
        if self.bigcount:
            return [('mpi_f08_types', 'MPI_COUNT_KIND')]
        else:
            return [('mpi_f08_types', 'MPI_ADDRESS_KIND')]

    def c_parameter(self):
        type_ = 'MPI_Count' if self.bigcount else 'MPI_Aint'
        return f'{type_} *{self.name}'


@FortranType.add('AINT_COUNT_OUT')
class AintCountTypeOut(FortranType):
    """AINT/COUNT type with OUT INTENT"""
    def declare(self):
        if self.bigcount:
            return f'INTEGER(KIND=MPI_COUNT_KIND), INTENT(OUT) :: {self.name}'
        else:
            return f'INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: {self.name}'

    def use(self):
        if self.bigcount:
            return [('mpi_f08_types', 'MPI_COUNT_KIND')]
        else:
            return [('mpi_f08_types', 'MPI_ADDRESS_KIND')]

    def c_parameter(self):
        type_ = 'MPI_Count' if self.bigcount else 'MPI_Aint'
        return f'{type_} *{self.name}'


@FortranType.add('AINT_ARRAY')
class AintArrayType(FortranType):
    """Array of MPI_Aint."""

    def declare(self):
        # TODO: Should there be a separate ASYNC version here, when the OMPI_ASYNCHRONOUS attr is required?
        return f'INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) OMPI_ASYNCHRONOUS :: {self.name}(*)'

    def use(self):
        return [('mpi_f08_types', 'MPI_ADDRESS_KIND')]

    def c_parameter(self):
        return f'MPI_Aint *{self.name}'


@FortranType.add('DISP')
class Disp(FortranType):
    """Displacecment type."""

    def declare(self):
        kind = '(KIND=MPI_ADDRESS_KIND)' if self.bigcount else ''
        return f'INTEGER{kind}, INTENT(IN) :: {self.name}'

    def use(self):
        if self.bigcount:
            return [('mpi_f08_types', 'MPI_ADDRESS_KIND')]
        return []

    def c_parameter(self):
        count_type = 'MPI_Aint' if self.bigcount else 'MPI_Fint'
        return f'{count_type} *{self.name}'

@FortranType.add('DISP_OUT')
class DispOut(FortranType):
    """Displacecment out type."""

    def declare(self):
        kind = '(KIND=MPI_ADDRESS_KIND)' if self.bigcount else ''
        return f'INTEGER{kind}, INTENT(OUT) :: {self.name}'

    def use(self):
        if self.bigcount:
            return [('mpi_f08_types', 'MPI_ADDRESS_KIND')]
        return []

    def c_parameter(self):
        count_type = 'MPI_Aint' if self.bigcount else 'MPI_Fint'
        return f'{count_type} *{self.name}'


@FortranType.add('DISP_ARRAY')
class DispArray(IntArray):
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


@FortranType.add('OP')
class Op(FortranType):
    """MPI_Op type."""

    def declare(self):
        return f'TYPE(MPI_Op), INTENT(IN) :: {self.name}'

    def use(self):
        return [('mpi_f08_types', 'MPI_Op')]

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'


@FortranType.add('WIN')
class Win(FortranType):
    """MPI_Win type."""

    def declare(self):
        return f'TYPE(MPI_Win), INTENT(IN) :: {self.name}'

    def use(self):
        return [('mpi_f08_types', 'MPI_Win')]

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'

@FortranType.add('WIN_OUT')
class WinOut(FortranType):
    """MPI_Win out type."""

    def declare(self):
        return f'TYPE(MPI_Win), INTENT(OUT) :: {self.name}'

    def declare_cbinding_fortran(self):
        return f'INTEGER, INTENT(OUT) :: {self.name}'

    def argument(self):
        return f'{self.name}%MPI_VAL'

    def use(self):
        return [('mpi_f08_types', 'MPI_Win')]

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'


@FortranType.add('FILE')
class File(FortranType):
    """MPI_File type."""

    def declare(self):
        return f'TYPE(MPI_File), INTENT(IN) :: {self.name}'

    def use(self):
        return [('mpi_f08_types', 'MPI_File')]

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'

@FortranType.add('INFO')
class Info(FortranType):
    """MPI_Info type."""

    def declare(self):
        return f'TYPE(MPI_Info), INTENT(IN) :: {self.name}'

    def use(self):
        return [('mpi_f08_types', 'MPI_Info')]

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'

@FortranType.add('OFFSET')
class Offset(FortranType):
    """MPI_Offset type."""

    def declare(self):
        return f'INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: {self.name}'

    def use(self):
        return [('mpi_f08_types', 'MPI_OFFSET_KIND')]

    def c_parameter(self):
        return f'MPI_Offset *{self.name}'


@FortranType.add('CHAR_ARRAY')
class CharArray(FortranType):
    """Fortran CHAR type."""

    def declare(self):
        return f'CHARACTER(LEN=*), INTENT(IN) :: {self.name}'

    def use(self):
        return [('iso_c_binding', 'c_char')]

    def declare_cbinding_fortran(self):
        return f'CHARACTER(KIND=C_CHAR), INTENT(IN) :: {self.name}(*)'

    def c_parameter(self):
        return f'char *{self.name}'


@FortranType.add('MESSAGE_INOUT')
class MessageInOut(FortranType):
    """MPI_Message INOUT type."""

    def declare(self):
        return f'TYPE(MPI_Message), INTENT(INOUT) :: {self.name}'

    def use(self):
        return [('mpi_f08_types', 'MPI_Message')]

    def c_parameter(self):
        return f'MPI_Fint *{self.name}'
