# Copyright (c) 2024      Triad National Security, LLC. All rights
#                         reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
"""C type definitions."""
from abc import ABC, abstractmethod
from ompi_bindings.consts import ConvertFuncs, ConvertOMPIToStandard, IGNORED_STATUS_HANDLES
from ompi_bindings import util 

class Type(ABC):
    """Type representation."""

    PARAMS_OMPI_ABI = {}

    PARAMS_STANDARD_ABI = {}

    def __init__(self, type_name, name=None,
                 mangle_name=lambda name: abi_internal_name(name),
                 count_param=None, **kwargs):
        self.type = type_name
        self.name = name
        self.count_param = count_param
        self.mangle_name = util.abi_internal_name

    @staticmethod
    def construct(abi_type, type_name, **kwargs):
        """Construct the parameter for the given ABI and type."""
        if abi_type == 'ompi':
            return Type.PARAMS_OMPI_ABI[type_name](type_name, **kwargs)
        elif abi_type == 'standard':
#           print("Checkint oug type " + str(type_name))
            return Type.PARAMS_STANDARD_ABI[type_name](type_name, **kwargs)
        else:
            raise RuntimeError(f'invalid ABI type {abi_type}')

    @staticmethod
    def add_type(type_name, abi_type=('ompi', 'standard')):
        """Add a new class corresponding to a type."""
        def wrapper(class_):
            if 'ompi' in abi_type:
                Type.PARAMS_OMPI_ABI[type_name] = class_
            if 'standard' in abi_type:
#               print("Adding type " + str(type_name) + " to PARAMS_STANDARD_ABI")
                Type.PARAMS_STANDARD_ABI[type_name] = class_
            return class_
        return wrapper

    @property
    def is_count(self):
        """Return True if this parameter is a count (requiring bigcount API)."""
        return False

    @property
    def init_code(self):
        """Return the initialization code needed for an ABI wrapper."""
        return []

    @property
    def final_code(self):
        """Return the finalization code needed for an ABI wrapper."""
        return []

    def return_code(self, name):
        """Process a value and then build up a return statement."""
        return [f'return {name};']

    @property
    def argument(self):
        """Return the argument text required for passing an argument to a function."""
        return self.name

    @abstractmethod
    def type_text(self, enable_count=False):
        """Return the source text corresponding to a type definition."""

    def tmp_type_text(self, enable_count=False):
        """Return source text corresponding to a temporary type definition before conversion."""
        return self.type_text(enable_count=enable_count)

    def parameter(self, enable_count=False, **kwargs):
        return f'{self.type_text(enable_count=enable_count)} {self.name}'


@Type.add_type('ERROR_CLASS')
class TypeErrorClass(Type):

    def type_text(self, enable_count=False):
        return 'int'

    def return_code(self, name):
        return [f'return {ConvertFuncs.ERROR_CLASS}({name});']


@Type.add_type('BUFFER')
class TypeBuffer(Type):

    def type_text(self, enable_count=False):
        return 'const void *'


@Type.add_type('BUFFER_OUT')
class TypeBufferOut(Type):

    def type_text(self, enable_count=False):
        return f'void *'


@Type.add_type('COUNT')
class TypeCount(Type):

    @property
    def is_count(self):
        return True

    def type_text(self, enable_count=False):
        return 'MPI_Count' if enable_count else 'int'


@Type.add_type('COUNT_ARRAY')
class TypeCountArray(Type):
    """Array of counts (either int or MPI_Count)."""

    @property
    def is_count(self):
        return True

    def type_text(self, enable_count=False):
        return 'MPI_Count *' if enable_count else 'int *'

    def parameter(self, enable_count=False, **kwargs):
        count_type = 'MPI_Count' if enable_count else 'int'
        return f'const {count_type} {self.name}[]'

@Type.add_type('AINT_COUNT_ARRAY')
class TypeAintCountArray(Type):
    """Array of counts (either MPI_Aint or MPI_Count)."""

    @property
    def is_count(self):
        return True

    def type_text(self, enable_count=False):
        return 'MPI_Count *' if enable_count else 'MPI_Aint *'

    def parameter(self, enable_count=False, **kwargs):
        count_type = 'MPI_Count' if enable_count else 'MPI_Aint'
        return f'const {count_type} {self.name}[]'

@Type.add_type('ELEMENT_COUNT')
class ElementCountType(Type):
    """Special count type for MPI_Get_element_x"""

    def type_text(self, enable_count=False):
        return 'MPI_Count *'


@Type.add_type('PARTITIONED_COUNT')
class TypePartitionedCount(Type):
    """Count type for partitioned communication functions."""

    def type_text(self, enable_count=False):
        return 'MPI_Count'


@Type.add_type('DISP')
class TypeDisp(Type):

    @property
    def is_count(self):
        return True

    def type_text(self, enable_count=False):
        return 'MPI_Aint' if enable_count else 'int'


@Type.add_type('DISP_ARRAY')
class TypeDispArray(Type):

    @property
    def is_count(self):
        return True

    def type_text(self, enable_count=False):
        return 'MPI_Aint *' if enable_count else 'int *'

    def parameter(self, enable_count=False, **kwargs):
        count_type = 'MPI_Aint' if enable_count else 'int'
        return f'const {count_type} {self.name}[]'


@Type.add_type('INT')
class TypeInt(Type):

    def type_text(self, enable_count=False):
        return 'int'


@Type.add_type('AINT')
class TypeAint(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Aint'


@Type.add_type('AINT_OUT')
class TypeAintOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Aint *'


@Type.add_type('AINT_ARRAY')
class TypeAintArray(Type):

    def type_text(self, enable_count=False):
        return 'const MPI_Aint *'

    def parameter(self, enable_count=False, **kwargs):
        return f'const MPI_Aint {self.name}[]'


@Type.add_type('INT_OUT')
class TypeIntOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

    def parameter(self, enable_count=False, **kwargs):
        if self.count_param is None:
            return f'int *{self.name}'
        else:
            return f'int {self.name}[]'


@Type.add_type('COUNT_OUT')
class TypeCountOut(Type):

    @property
    def is_count(self):
        return True

    def type_text(self, enable_count=False):
        return 'MPI_Count *' if enable_count else 'int *'


@Type.add_type('AINT_COUNT')
class TypeAintCountOut(Type):

    @property
    def is_count(self):
        return True

    def type_text(self, enable_count=False):
        return 'MPI_Count' if enable_count else 'MPI_Aint'


@Type.add_type('AINT_COUNT_OUT')
class TypeAintCountOut(Type):

    @property
    def is_count(self):
        return True

    def type_text(self, enable_count=False):
        return 'MPI_Count *' if enable_count else 'MPI_Aint *'


@Type.add_type('INT_ARRAY')
class TypeIntArray(Type):

    def type_text(self, enable_count=False):
        return 'const int *'

    def parameter(self, enable_count=False, **kwargs):
        return f'const int {self.name}[]'

@Type.add_type('INT_AINT_OUT')
class TypeIntAintOut(Type):

    @property
    def is_count(self):
        return True

    def type_text(self, enable_count=False):
        return 'MPI_Aint *' if enable_count else 'int *'

@Type.add_type('RANGE_ARRAY')
class TypeRangeArray(Type):

    def type_text(self, enable_count=False):
        return 'int *'

    def parameter(self, enable_count=False, **kwargs):
        return f'int {self.name}[][3]'


@Type.add_type('OFFSET')
class TypeOffset(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Offset'


@Type.add_type('OFFSET_OUT')
class TypeOffsetOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Offset *'


@Type.add_type('DOUBLE')
class TypeDouble(Type):

    def type_text(self, enable_count=False):
        return 'double'


@Type.add_type('ARGV')
class TypeArgv(Type):

    def type_text(self, enable_count=False):
        return 'char ***'


@Type.add_type('STRING_ARRAY')
class TypeStringArray(Type):

    def type_text(self, enable_count=False):
        return 'char **'

    def parameter(self, enable_count=False, **kwargs):
        return f'char *{self.name}[]'


@Type.add_type('DATATYPE', abi_type=['ompi'])
class TypeDatatype(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Datatype'

@Type.add_type('DATATYPE_OUT', abi_type=['ompi'])
class TypeDatatypeOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Datatype *'


@Type.add_type('DATATYPE_ARRAY', abi_type=['ompi'])
class TypeDatatypeArray(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Datatype'

    def parameter(self, enable_count=False, **kwargs):
        return f'const {self.type_text(enable_count=enable_count)} {self.name}[]'


class StandardABIType(Type):

    @property
    def tmpname(self):
        return f'{self.name}_tmp'

    @property
    def argument(self):
        return self.tmpname


@Type.add_type('DATATYPE', abi_type=['standard'])
class TypeDatatypeStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Datatype {self.tmpname} = {ConvertFuncs.DATATYPE}({self.name});']

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Datatype'

    def return_code(self, name):
        return [f'return {ConvertOMPIToStandard.DATATYPE}({name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Datatype')


@Type.add_type('DATATYPE_OUT', abi_type=['standard'])
class TypeDatatypeOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'*{self.name} = {ConvertOMPIToStandard.DATATYPE}((MPI_Datatype) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Datatype')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Datatype *) {self.name}'

#
# TODO THIS IS NOT COMPLETE
#
@Type.add_type('DATATYPE_ARRAY', abi_type=['standard'])
class TypeDatatypeArrayStandard(StandardABIType):

    @property
    def init_code(self):
        if self.count_param is None:
            code = [f'MPI_Comm comm_{self.tmpname} = {ConvertFuncs.COMM}(comm);']
            code.append(f'int size_{self.tmpname} = OMPI_COMM_IS_INTER(comm_{self.tmpname})?ompi_comm_remote_size(comm_{self.tmpname}):ompi_comm_size(comm_{self.tmpname});')
        else:
            code = [f'int size_{self.tmpname} = {self.count_param};']
        code.append(f'MPI_Datatype *{self.tmpname} = (MPI_Datatype *)malloc(sizeof(MPI_Datatype) * size_{self.tmpname});')
        code.append(f'for(int i=0;i<size_{self.tmpname};i++){{')
        code.append(f'{self.tmpname}[i] = {ConvertFuncs.DATATYPE}({self.name}[i]);')
        code.append(f'}}')
        return code

    @property
    def final_code(self):
        code = [f'free({self.tmpname});']
        return code

    @property
    def tmpname(self):
        return f'{self.name}_tmp'

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Datatype')

    def parameter(self, enable_count=False, **kwargs):
        return f'const {self.type_text(enable_count=enable_count)} {self.name}[]'

    @property
    def argument(self):
        return f'(MPI_Datatype *) {self.tmpname}'

@Type.add_type('OP', abi_type=['ompi'])
class TypeDatatype(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Op'


@Type.add_type('OP', abi_type=['standard'])
class TypeDatatype(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Op {self.tmpname} = {ConvertFuncs.OP}({self.name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Op')

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Op'

    def return_code(self, name):
        return [f'return {ConvertOMPIToStandard.OP}({name});']

@Type.add_type('OP_OUT', abi_type=['ompi'])
class TypeOpOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Op *'


@Type.add_type('OP_OUT', abi_type=['standard'])
class TypeOpOutStandard(Type):

    @property
    def final_code(self):
        return [f'*{self.name} = {ConvertOMPIToStandard.OP}((MPI_Op) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Op')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Op *) {self.name}'


@Type.add_type('RANK')
class TypeRank(Type):

    def type_text(self, enable_count=False):
        return 'int'


@Type.add_type('TAG')
class TypeRank(Type):

    def type_text(self, enable_count=False):
        return 'int'


@Type.add_type('COMM', abi_type=['ompi'])
class TypeCommunicator(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Comm'


@Type.add_type('COMM', abi_type=['standard'])
class TypeCommunicatorStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Comm {self.tmpname} = {ConvertFuncs.COMM}({self.name});']

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Comm'

    def return_code(self, name):
        return [f'return {ConvertOMPIToStandard.COMM}({name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Comm')


@Type.add_type('COMM_OUT', abi_type=['ompi'])
class TypeCommunicator(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Comm *'


@Type.add_type('COMM_OUT', abi_type=['standard'])
class TypeCommunicator(StandardABIType):

    @property
    def final_code(self):
        return [f'*{self.name} = {ConvertOMPIToStandard.COMM}((MPI_Comm) *{self.name});']
 
    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Comm')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Comm *) {self.name}'


@Type.add_type('WIN', abi_type=['ompi'])
class TypeWindow(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Win'


@Type.add_type('WIN', abi_type=['standard'])
class TypeWindowStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Win {self.tmpname} = {ConvertFuncs.WIN}({self.name});']

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Win'

    def return_code(self, name):
        return [f'return {ConvertOMPIToStandard.WIN}({name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Win')

@Type.add_type('WIN_OUT', abi_type=['ompi'])
class TypeWindowOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Win *'


@Type.add_type('WIN_OUT', abi_type=['standard'])
class TypeWindowOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'*{self.name} = {ConvertOMPIToStandard.WIN}((MPI_Win) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Win')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Win *) {self.name}'


@Type.add_type('REQUEST', abi_type=['ompi'])
class TypeRequest(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Request'


@Type.add_type('REQUEST', abi_type=['standard'])
class TypeRequestStandard(StandardABIType):

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Request')

    @property
    def argument(self):
        return f'(MPI_Request) {self.name}'

@Type.add_type('REQUEST_CONST', abi_type=['ompi'])
class TypeConstRequest(TypeRequest):

    def type_text(self, enable_count=False):
        return f'const MPI_Request *'

    def parameter(self, enable_count=False, **kwargs):
        if self.count_param is None:
            return f'const MPI_Request {self.name}'
        else:
            return f'const MPI_Request {self.name}[]'

#
# TODO ABI NEEDS WORK
#
@Type.add_type('REQUEST_CONST', abi_type=['standard'])
class TypeConstRequestStandard(TypeRequestStandard):

    def type_text(self, enable_count=False):
        name = self.mangle_name('MPI_Request')
        return f'const {name}'

    @property
    def argument(self):
        return f'(MPI_Request) {self.name}'

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Request'

    def return_code(self, name):
        return [f'return {ConvertOMPIToStandard.REQUEST}({name});']
        
@Type.add_type('REQUEST_INOUT', abi_type=['ompi'])
class TypeRequestInOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Request *'

@Type.add_type('REQUEST_INOUT', abi_type=['standard'])
class TypeRequestInOutStandard(StandardABIType):

    @property
    def final_code(self):
        if self.count_param is None:
            return [f'*{self.name} = {ConvertOMPIToStandard.REQUEST}((MPI_Request) *{self.name});']
        else:
            return [
                'for (int i = 0; i < %s; ++i) {' % (self.count_param,),
                f'{self.name}[i] = {ConvertOMPIToStandard.REQUEST}((MPI_Request) {self.name}[i]);',
                '}',
            ]

    @property
    def argument(self):
        return f'(MPI_Request *) {self.name}'

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Request')
        return f'{type_name} *'

    def parameter(self, enable_count=False, **kwargs):
        type_name = self.mangle_name('MPI_Request')
        if self.count_param is None:
            return f'{type_name} *{self.name}'
        else:
            return f'{type_name} {self.name}[]'

@Type.add_type('STATUS', abi_type=['ompi'])
class TypeStatus(Type):

    def type_text(self, enable_count=False):
        return 'const MPI_Status *'

@Type.add_type('STATUS', abi_type=['standard'])
class TypeStatusStandard(StandardABIType):

    @property
    def init_code(self):
        # TODO: Need to ensure this is the correct conversion function for MPI_Status
        mangle_type = self.mangle_name('MPI_Status')
        code = [f'MPI_Status {self.tmpname};'];
        code.append(f'{ConvertFuncs.STATUS}(&{self.tmpname}, ({mangle_type} *){self.name});')
        return code

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Status')
#       print("type_name for STATUS is " + str(type_name))
        return f'const {type_name} *'

    @property
    def argument(self):
        return f'&{self.tmpname}'


@Type.add_type('STATUS_OUT', abi_type=['ompi'])
class TypeStatusOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Status *'

    def parameter(self, enable_count=False, **kwargs):
        if self.count_param is None:
            return f'MPI_Status *{self.name}'
        else:
            return f'MPI_Status {self.name}[]'


@Type.add_type('STATUS_OUT', abi_type=['standard'])
class TypeStausOutStandard(StandardABIType):

    def if_should_set_status(self):
        """Generate the condition to check if the status(es) should be set."""
        condition = ' && '.join(f'{self.mangle_name(const)} != {self.name}'
                                for const in IGNORED_STATUS_HANDLES)
        return 'if (%s) {' % (condition,)

    @property
    def status_argument(self):
        return f'{self.name}_arg'

    @property
    def init_code(self):
        code = [f'MPI_Status *{self.status_argument} = NULL;']
        if self.count_param is None:
            code.append(f'MPI_Status {self.tmpname};')
        else:
            code.append(f'MPI_Status *{self.tmpname} = NULL;')
        code.append(self.if_should_set_status())
        if self.count_param is not None:
            code.append(f'{self.tmpname} = malloc({self.count_param} * sizeof(MPI_Status));')
            code.append(f'{self.status_argument} = {self.tmpname};')
        else:
            code.append(f'{self.status_argument} = &{self.tmpname};')
        code.append('} else {')
        if self.count_param is not None:
            code.append(f'{self.status_argument} = MPI_STATUSES_IGNORE;')
        else:
            code.append(f'{self.status_argument} = MPI_STATUS_IGNORE;')
        code.append('}')
        return code

    @property
    def final_code(self):
        code = [self.if_should_set_status()]
        if self.count_param is None:
            code.append(f'{ConvertOMPIToStandard.STATUS}({self.name}, &{self.tmpname});')
        else:
            code.extend([
                'for (int i = 0; i < %s; ++i) {' % (self.count_param,),
                f'{ConvertOMPIToStandard.STATUS}(&{self.name}[i], &{self.tmpname}[i]);',
                '}',
                f'free({self.tmpname});',
            ])
        code.append('}')
        return code

    @property
    def argument(self):
        return self.status_argument

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Status')
        return f'{type_name} *'

    def parameter(self, enable_count=False, **kwargs):
        type_name = self.mangle_name('MPI_Status')
        if self.count_param is None:
            return f'{type_name} *{self.name}'
        else:
            return f'{type_name} {self.name}[]'

@Type.add_type('STATUS_INOUT', abi_type=['ompi'])
class TypeStatusInOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Status *'

    def parameter(self, enable_count=False, **kwargs):
        if self.count_param is None:
            return f'MPI_Status *{self.name}'
        else:
            return f'MPI_Status {self.name}[]'

#
# so far there are no vectors of statuses for inout in the the standard
#
@Type.add_type('STATUS_INOUT', abi_type=['standard'])
class TypeStausInOutStandard(StandardABIType):

    def if_should_set_status(self):
        """Generate the condition to check if the status(es) should be set."""
        condition = ' && '.join(f'{self.mangle_name(const)} != {self.name}'
                                for const in IGNORED_STATUS_HANDLES)
        return 'if (%s) {' % (condition,)

    @property
    def status_argument(self):
        return f'{self.name}_arg'

    @property
    def init_code(self):
        mangle_type = self.mangle_name('MPI_Status')
        code = [f'MPI_Status *{self.status_argument} = NULL;']
        code.append(f'MPI_Status {self.tmpname};')
        code.append(f'{ConvertFuncs.STATUS}(&{self.tmpname}, ({mangle_type} *){self.name});')
        code.append(self.if_should_set_status())
        code.append(f'{self.status_argument} = &{self.tmpname};')
        code.append('} else {')
        code.append(f'{self.status_argument} = MPI_STATUS_IGNORE;')
        code.append('}')
        return code

    @property
    def final_code(self):
        code = [self.if_should_set_status()]
        code.append(f'{ConvertOMPIToStandard.STATUS}({self.name}, &{self.tmpname});')
        code.append('}')
        return code

    @property
    def argument(self):
        return self.status_argument

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Status')
        return f'{type_name} *'

    def parameter(self, enable_count=False, **kwargs):
        type_name = self.mangle_name('MPI_Status')
        return f'{type_name} *{self.name}'


@Type.add_type('F08_STATUS')
class TypeF08Status(Type):

    def type_text(self, enable_count=False):
        return 'const MPI_F08_status *'


@Type.add_type('F08_STATUS_OUT')
class TypeF08StatusOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_F08_status *'


# TODO: For now this just assumes that MPI_Fint doesn't need any conversions
@Type.add_type('FINT')
class TypeFint(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Fint'


@Type.add_type('FINT_CONST')
class TypeFintRef(Type):

    def type_text(self, enable_count=False):
        return 'const MPI_Fint *'


@Type.add_type('FINT_OUT')
class TypeFintOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Fint *'


@Type.add_type('STRING')
class TypeString(Type):

    def type_text(self, enable_count=False):
        return 'const char *'


@Type.add_type('STRING_OUT')
class TypeStringOut(Type):

    def type_text(self, enable_count=False):
        return 'char *'


@Type.add_type('INFO', abi_type=['ompi'])
class TypeInfo(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Info'


@Type.add_type('INFO', abi_type=['standard'])
class TypeInfoStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Info {self.tmpname} = {ConvertFuncs.INFO}({self.name});']

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Info'
        
    def return_code(self, name):
        return [f'return {ConvertOMPIToStandard.INFO}({name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Info')


@Type.add_type('INFO_OUT', abi_type=['ompi'])
class TypeInfoOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Info *'


@Type.add_type('INFO_OUT', abi_type=['standard'])
class TypeInfoOutStandard(Type):

    @property
    def argument(self):
        return f'(MPI_Info *) {self.name}'

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Info')
        return f'{type_name} *'


@Type.add_type('INFO_ARRAY', abi_type=['ompi'])
class TypeInfoArray(Type):

    def type_text(self, enable_count=False):
        return 'const MPI_Info *'

    def parameter(self, enable_count=False, **kwargs):
        return f'const MPI_Info {self.name}[]'


@Type.add_type('INFO_ARRAY', abi_type=['standard'])
class TypeInfoArray(StandardABIType):

#
# TODO may need a better way to generalize for case of non-explicit count_param
#
    @property
    def init_code(self):
        code = [f'int size_{self.tmpname} = {self.count_param};']
        code.append(f'MPI_Info *{self.tmpname} = (MPI_Info *)malloc(sizeof(MPI_Info) * size_{self.tmpname});')
        code.append(f'for(int i=0;i<size_{self.tmpname};i++){{')
        code.append(f'{self.tmpname}[i] = {ConvertFuncs.INFO}({self.name}[i]);')
        code.append(f'}}')
        return code

    @property
    def final_code(self):
        code = [f'free({self.tmpname});']
        return code

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Info')
        return f'const {type_name} *'

    def parameter(self, enable_count=False, **kwargs):
        type_name = self.mangle_name('MPI_Info')
        return f'const {type_name} {self.name}[]'


@Type.add_type('FILE', abi_type=['ompi'])
class TypeFile(Type):

    def type_text(self, enable_count=False):
        return 'MPI_File'


@Type.add_type('FILE', abi_type=['standard'])
class TypeFileStandard(StandardABIType):

#   @property
#   def argument(self):
#       return f'(MPI_File) {self.name}'

    @property
    def init_code(self):
        return [f'MPI_File {self.tmpname} = {ConvertFuncs.FILE}({self.name});']

    def tmp_type_text(self, enable_count=False):
        return 'MPI_File'

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_File')

    def return_code(self, name):
        return [f'return {ConvertOMPIToStandard.FILE}({name});']

@Type.add_type('FILE_OUT', abi_type=['ompi'])
class TypeFileOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_File *'


@Type.add_type('FILE_OUT', abi_type=['standard'])
class TypeFileOutStandard(StandardABIType):

    @property
    def argument(self):
        return f'(MPI_File *) {self.name}'

    @property
    def final_code(self):
        return [f'*{self.name} = {ConvertOMPIToStandard.FILE}((MPI_File) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_File')
        return f'{type_name} *'

@Type.add_type('FILE_INOUT', abi_type=['ompi'])
class TypeFileInOut(TypeFileOut):

    def type_text(self, enable_count=False):
        return 'MPI_File *'

@Type.add_type('FILE_INOUT', abi_type=['standard'])
class TypeFileInOutStandard(TypeFileOutStandard):

    @property
    def init_code(self):
        return [f'MPI_File {self.tmpname} = {ConvertFuncs.FILE}(*{self.name});']

    @property
    def argument(self):
        return f'&{self.tmpname}'

@Type.add_type('MESSAGE', abi_type=['ompi'])
class TypeMessage(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Message'


@Type.add_type('MESSAGE', abi_type=['standard'])
class TypeMessageStandard(StandardABIType):

    @property
    def argument(self):
        return f'(MPI_Message) {self.name}'

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Message')

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Message'

    def return_code(self, name):
        return [f'return {ConvertOMPIToStandard.MESSAGE}({name});']
        
@Type.add_type('MESSAGE_OUT', abi_type=['ompi'])
class TypeMessageOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Message *'


@Type.add_type('MESSAGE_OUT', abi_type=['standard'])
class TypeMessageOutStandard(Type):

    @property
    def argument(self):
        return f'(MPI_Message *) {self.name}'

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Message')
        return f'{type_name} *'


@Type.add_type('COMM_ERRHANDLER_FUNCTION', abi_type=['ompi'])
class TypeCommErrhandlerFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Comm_errhandler_function *'


@Type.add_type('COMM_ERRHANDLER_FUNCTION', abi_type=['standard'])
class TypeCommErrhandlerFunctionStandard(Type):
    # TODO: This may require a special function to wrap the calllback
    # pass

    def type_text(self, enable_count=False):
        return 'MPI_Comm_errhandler_function *'


@Type.add_type('FILE_ERRHANDLER_FUNCTION', abi_type=['ompi'])
class TypeFileErrhandlerFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_File_errhandler_function *'


@Type.add_type('FILE_ERRHANDLER_FUNCTION', abi_type=['standard'])
class TypeFileErrhandlerFunction(Type):
    # TODO: This may require a special function to wrap the callback
    # pass

    def type_text(self, enable_count=False):
        return 'MPI_File_errhandler_function *'


@Type.add_type('COPY_FUNCTION', abi_type=['ompi'])
class TypeCopyFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Copy_function *'


@Type.add_type('COPY_FUNCTION', abi_type=['standard'])
class TypeCopyFunctionStandard(Type):
    # TODO: This may require a special function to wrap the callback
#    pass

    def type_text(self, enable_count=False):
        return 'MPI_Copy_function *'

@Type.add_type('DELETE_FUNCTION', abi_type=['ompi'])
class TypeDeleteFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Delete_function *'


@Type.add_type('DELETE_FUNCTION', abi_type=['standard'])
class TypeDeleteFunctionStandard(Type):
    # TODO: This may require a special function to wrap the callback
#    pass
    def type_text(self, enable_count=False):
        return 'MPI_Delete_function *'


@Type.add_type('USER_FUNCTION', abi_type=['ompi'])
class TypeUserFunction(Type):

    @property
    def is_count(self):
        return True

    def type_text(self, enable_count=False):
        return 'MPI_User_function_c *' if enable_count else 'MPI_User_function *'


@Type.add_type('USER_FUNCTION', abi_type=['standard'])
class TypeUserFunctionStandard(Type):
    # TODO: This may require a special function to wrap the callback
#    pass

    def type_text(self, enable_count=False):
        return 'MPI_User_function_c *' if enable_count else 'MPI_User_function *'

@Type.add_type('COMM_COPY_ATTR_FUNCTION', abi_type=['ompi'])
class TypeCommCopyAttrFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Comm_copy_attr_function *'


@Type.add_type('COMM_COPY_ATTR_FUNCTION', abi_type=['standard'])
class TypeCommCopyAttrFunctionStandard(Type):
    # TODO: This may require a special function to wrap the callback
#    pass

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Comm_copy_attr_function')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Comm_copy_attr_function *) {self.name}'

#   @property
#   def init_code(self):
#       code = []
#       code = ['ompi_abi_wrapper_helper_t *helper = NULL;']
#       code.append('helper = ( ompi_abi_wrapper_helper_t *)malloc(sizeof(ompi_abi_wrapper_helper_t));')
#       code.append('if (NULL == helper)  return MPI_ERR_NO_MEM;')
#       code.append('helper->user_extra_state = extra_state;')
#       code.append('helper->user_copy_fn = comm_copy_attr_fn;')
#       code.append('helper->user_delete_fn = comm_delete_attr_fn;')
#       return code

@Type.add_type('COMM_DELETE_ATTR_FUNCTION', abi_type=['ompi'])
class TypeCommDeleteAttrFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Comm_delete_attr_function *'


@Type.add_type('COMM_DELETE_ATTR_FUNCTION', abi_type=['standard'])
class TypeCommDeleteAttrFunctionStandard(Type):
    # TODO: This may require a special function to wrap the callback
#    pass

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Comm_delete_attr_function')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Comm_delete_attr_function *) {self.name}'

@Type.add_type('GREQUEST_QUERY_FUNCTION', abi_type=['ompi'])
class TypeGrequestQueryFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Grequest_query_function *'


@Type.add_type('GREQUEST_QUERY_FUNCTION', abi_type=['standard'])
class TypeGrequestQueryFunctionStandard(Type):
    # TODO: This may require a special function to wrap the callback
#    pass

    def type_text(self, enable_count=False):
        return 'MPI_Grequest_query_function *'

@Type.add_type('GREQUEST_FREE_FUNCTION', abi_type=['ompi'])
class TypeGrequestFreeFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Grequest_free_function *'


@Type.add_type('GREQUEST_FREE_FUNCTION', abi_type=['standard'])
class TypeGrequestFreeFunctionStandard(Type):
    # TODO: This may require a special function to wrap the callback
#    pass

    def type_text(self, enable_count=False):
        return 'MPI_Grequest_free_function *'

@Type.add_type('GREQUEST_CANCEL_FUNCTION', abi_type=['ompi'])
class TypeGrequestCancelFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Grequest_cancel_function *'


@Type.add_type('GREQUEST_CANCEL_FUNCTION', abi_type=['standard'])
class TypeGrequestCancelFunctionStandard(Type):
    # TODO: This may require a special function to wrap the callback
#    pass

    def type_text(self, enable_count=False):
        return 'MPI_Grequest_cancel_function *'

@Type.add_type('DATAREP_CONVERSION_FUNCTION', abi_type=['ompi'])
class TypeDatarepConversionFunction(Type):

    @property
    def is_count(self):
        return True

    def type_text(self, enable_count=False):
        return 'MPI_Datarep_conversion_function_c *' if enable_count else 'MPI_Datarep_conversion_function *'

@Type.add_type('DATAREP_CONVERSION_FUNCTION', abi_type=['standard'])
class TypeDatarepConversionFunctionStandard(Type):
    # TODO: This may require a special function to wrap the callback
#    pass

    @property
    def is_count(self):
        return True

    def type_text(self, enable_count=False):
        return 'MPI_Datarep_conversion_function_c *' if enable_count else 'MPI_Datarep_conversion_function *'

@Type.add_type('DATAREP_EXTENT_FUNCTION', abi_type=['ompi'])
class TypeDatarepExtentFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Datarep_extent_function *'


@Type.add_type('DATAREP_EXTENT_FUNCTION', abi_type=['standard'])
class TypeDatarepExtentFunctionStandard(Type):
    # TODO: This may require a special function to wrap the callback
#     pass

    def type_text(self, enable_count=False):
        return 'MPI_Datarep_extent_function *'

@Type.add_type('SESSION_ERRHANDLER_FUNCTION', abi_type=['ompi'])
class TypeSessionErrhandlerFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Session_errhandler_function *'


@Type.add_type('SESSION_ERRHANDLER_FUNCTION', abi_type=['standard'])
class TypeSessionErrhandlerFunctionStandard(Type):
    # TODO: This may require a special function to wrap the callback
#    pass

    def type_text(self, enable_count=False):
        return 'MPI_Session_errhandler_function *'

@Type.add_type('TYPE_COPY_ATTR_FUNCTION', abi_type=['ompi'])
class TypeTypeCopyAttrFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Type_copy_attr_function *'


@Type.add_type('TYPE_COPY_ATTR_FUNCTION', abi_type=['standard'])
class TypeTypeCopyAttrFunctionStandard(Type):
    # TODO: This may require a special function to wrap the callback
#    pass

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Type_copy_attr_function')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Type_copy_attr_function *) {self.name}'

@Type.add_type('TYPE_DELETE_ATTR_FUNCTION', abi_type=['ompi'])
class TypeTypeDeleteAttrFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Type_delete_attr_function *'


@Type.add_type('TYPE_DELETE_ATTR_FUNCTION', abi_type=['standard'])
class TypeTypeDeleteAttrFunctionStandard(Type):
    # TODO: This may require a special function to wrap the callback
#    pass

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Type_delete_attr_function')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Type_delete_attr_function *) {self.name}'

@Type.add_type('WIN_ERRHANDLER_FUNCTION', abi_type=['ompi'])

class TypeWinErrhandlerFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Win_errhandler_function *'


@Type.add_type('WIN_ERRHANDLER_FUNCTION', abi_type=['standard'])
class TypeWinErrhandlerFunctionStandard(Type):
    # TODO: This may require a special function to wrap the callback
#    pass

    def type_text(self, enable_count=False):
        return 'MPI_Win_errhandler_function *'

@Type.add_type('WIN_COPY_ATTR_FUNCTION', abi_type=['ompi'])
class TypeWinCopyAttrFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Win_copy_attr_function *'


@Type.add_type('WIN_COPY_ATTR_FUNCTION', abi_type=['standard'])
class TypeWinCopyAttrFunctionStandard(Type):
    # TODO: This may require a special function to wrap the callback
#    pass

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Win_copy_attr_function')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Win_copy_attr_function *) {self.name}'

@Type.add_type('WIN_DELETE_ATTR_FUNCTION', abi_type=['ompi'])
class TypeWinDeleteAttrFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Win_delete_attr_function *'


@Type.add_type('WIN_DELETE_ATTR_FUNCTION', abi_type=['standard'])
class TypeWinDeleteAttrFunctionStandard(Type):
    # TODO: This may require a special function to wrap the callback
#    pass

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Win_delete_attr_function')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Win_delete_attr_function *) {self.name}'

@Type.add_type('ERRHANDLER', abi_type=['ompi'])
class TypeErrhandler(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Errhandler'


@Type.add_type('ERRHANDLER', abi_type=['standard'])
class TypeErrhandlerStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Errhandler {self.tmpname} = {ConvertFuncs.ERRHANDLER}({self.name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Errhandler')

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Errhandler'
        
    def return_code(self, name):
        return [f'return {ConvertOMPIToStandard.ERRHANDLER}({name});']

@Type.add_type('ERRHANDLER_OUT', abi_type=['ompi'])
class TypeErrhandlerOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Errhandler *'


@Type.add_type('ERRHANDLER_OUT', abi_type=['standard'])
class TypeErrhandlerOutStandard(Type):

    @property
    def argument(self):
        return f'(MPI_Errhandler *) {self.name}'

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Errhandler')
        return f'{type_name} *'


@Type.add_type('GROUP', abi_type=['ompi'])
class TypeGroup(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Group'


@Type.add_type('GROUP', abi_type=['standard'])
class TypeGroupStandard(StandardABIType):

#   @property
#   def argument(self):
#       return f'(MPI_Group) {self.name}'

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Group')

    @property
    def init_code(self):
        return [f'MPI_Group {self.tmpname} = {ConvertFuncs.GROUP}({self.name});']
        
    def tmp_type_text(self, enable_count=False):
        return 'MPI_Group'

    def return_code(self, name):
        return [f'return {ConvertOMPIToStandard.GROUP}({name});']


@Type.add_type('GROUP_OUT', abi_type=['ompi'])
class TypeGroupOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Group *'


@Type.add_type('GROUP_OUT', abi_type=['standard'])
class TypeGroupOutStandard(Type):

    @property
    def final_code(self):
        return [f'*{self.name} = {ConvertOMPIToStandard.GROUP}((MPI_Group) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Group')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Group *) {self.name}'


@Type.add_type('SESSION_INOUT', abi_type=['ompi'])
class TypeSessionOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Session *'

@Type.add_type('SESSION_OUT', abi_type=['ompi'])
class TypeSessionOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Session *'


@Type.add_type('SESSION_INOUT', abi_type=['standard'])
class TypeSessionInOutStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Session {self.tmpname} = {ConvertFuncs.SESSION}(*{self.name});']

    @property
    def final_code(self):
        return [f'*{self.name} = {ConvertOMPIToStandard.SESSION}({self.tmpname});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Session')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'&{self.tmpname}'


@Type.add_type('SESSION_OUT', abi_type=['standard'])
class TypeSessionOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'*{self.name} = {ConvertOMPIToStandard.SESSION}((MPI_Session) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Session')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Session *) {self.name}'


@Type.add_type('SESSION', abi_type=['ompi'])
class TypeSession(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Session'


@Type.add_type('SESSION', abi_type=['standard'])
class TypeSessionStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Session {self.tmpname} = {ConvertFuncs.SESSION}({self.name});']

#   @property
#   def argument(self):
#       return f'(MPI_Session) {self.name}'

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Session')

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Session'

    def return_code(self, name):
        return [f'return {ConvertOMPIToStandard.SESSION}({name});']


