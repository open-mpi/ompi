from abc import ABC, abstractmethod
from ompi_bindings.consts import ConvertFuncs


class Type(ABC):
    """Type representation."""

    PARAMS_OMPI_ABI = {}

    PARAMS_STANDARD_ABI = {}

    def __init__(self, type_, name=None,
                 mangle_name=lambda name: abi_internal_name(name),
                 count_param=None, **kwargs):
        self.type = type_
        self.name = name
        self.count_param = count_param
        self.mangle_name = mangle_name

    @staticmethod
    def construct(abi_type, type_, **kwargs):
        """Construct the parameter for the given ABI and type."""
        if abi_type == 'ompi':
            return Type.PARAMS_OMPI_ABI[type_](type_, **kwargs)
        elif abi_type == 'standard':
            return Type.PARAMS_STANDARD_ABI[type_](type_, **kwargs)
        else:
            raise RuntimeError(f'invalid ABI type {abi_type}')

    @staticmethod
    def add_type(type_name, abi_type=('ompi', 'standard')):
        """Add a new class corresponding to a type."""
        def wrapper(class_):
            if 'ompi' in abi_type:
                Type.PARAMS_OMPI_ABI[type_name] = class_
            if 'standard' in abi_type:
                Type.PARAMS_STANDARD_ABI[type_name] = class_
            # Parameter.TYPES[type_] = class_
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
class TypeCount(Type):
    """Array of counts (either int or MPI_Count)."""

    @property
    def is_count(self):
        return True

    def type_text(self, enable_count=False):
        return 'MPI_Count *' if enable_count else 'int *'

    def parameter(self, enable_count=False):
        count_type = 'MPI_Count' if enable_count else 'int'
        return f'const {count_type} {self.name}[]'


@Type.add_type('DISPL_ARRAY')
class TypeCount(Type):

    @property
    def is_count(self):
        return True

    def type_text(self, enable_count=False):
        return 'MPI_Aint *' if enable_count else 'int *'

    def parameter(self, enable_count=False):
        count_type = 'MPI_Aint' if enable_count else 'int'
        return f'const {count_type} {self.name}[]'


@Type.add_type('INT')
class TypeBufferOut(Type):

    def type_text(self, enable_count=False):
        return 'int'


@Type.add_type('AINT')
class TypeBufferOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Aint'


@Type.add_type('INT_OUT')
class TypeBufferOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

    def parameter(self, enable_count=False, **kwargs):
        if self.count_param is None:
            return f'int *{self.name}'
        else:
            return f'int {self.name}[]'


@Type.add_type('DOUBLE')
class TypeDouble(Type):

    def type_text(self, enable_count=False):
        return 'double'


@Type.add_type('ARGV')
class TypeArgv(Type):

    def type_text(self, enable_count=False):
        return 'char ***'


@Type.add_type('DATATYPE', abi_type=['ompi'])
class TypeDatatype(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Datatype'


@Type.add_type('DATATYPE_ARRAY', abi_type=['ompi'])
class TypeDatatypeArray(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Datatype'

    def parameter(self, enable_count=False):
        return f'const {self.type_text(enable_count=enable_count)} {self.name}[]'


class StandardABIType(Type):

    @property
    def tmpname(self):
        return f'{self.name}_tmp'

    @property
    def argument(self):
        return self.tmpname


@Type.add_type('DATATYPE', abi_type=['standard'])
class TypeDatatype(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Datatype {self.tmpname} = {ConvertFuncs.DATATYPE}({self.name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Datatype')


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
class TypeCommunicator(Type):

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

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Win')


@Type.add_type('REQUEST', abi_type=['ompi'])
class TypeRequest(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Request'


@Type.add_type('REQUEST', abi_type=['standard'])
class TypeRequestStandard(Type):

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Request')

    @property
    def argument(self):
        return f'(MPI_Request) {self.name}'


@Type.add_type('REQUEST_INOUT', abi_type=['ompi'])
class TypeRequestInOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Request *'


@Type.add_type('REQUEST_INOUT', abi_type=['standard'])
class TypeRequestInOutStandard(Type):

    @property
    def final_code(self):
        if self.count_param is None:
            return [f'{ConvertFuncs.REQUEST}({self.name});']
        else:
            return [
                'for (int i = 0; i < %s; ++i) {' % (self.count_param,),
                f'{ConvertFuncs.REQUEST}(&{self.name}[i]);',
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
            code.append(f'{ConvertFuncs.STATUS}({self.name}, &{self.tmpname});')
        else:
            code.extend([
                'for (int i = 0; i < %s; ++i) {' % (self.count_param,),
                f'{ConvertFuncs.STATUS}(&{self.name}[i], &{self.tmpname}[i]);',
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


# For now this just assumes that MPI_Fint doesn't need any conversions
@Type.add_type('FINT')
class TypeFint(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Fint'


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

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Info')


@Type.add_type('FILE_OUT', abi_type=['ompi'])
class TypeFileOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_File *'


@Type.add_type('FILE_OUT', abi_type=['standard'])
class TypeFileOutStandard(Type):

    @property
    def argument(self):
        return f'(MPI_File *) {self.name}'

    @property
    def final_code(self):
        return [f'{ConvertFuncs.FILE}({self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_File')
        return f'{type_name} *'
