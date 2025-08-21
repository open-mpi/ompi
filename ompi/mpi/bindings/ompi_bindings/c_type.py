# Copyright (c) 2024-2025 Triad National Security, LLC. All rights
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
                 mangle_name=lambda name: util.abi_internal_name(name),
                 count_param=None, outcount_param=None, **kwargs):
        self.type = type_name
        self.name = name
        self.count_param = count_param
        self.outcount_param = outcount_param
        self.mangle_name = mangle_name

    @staticmethod
    def construct(abi_type, type_name, **kwargs):
        """Construct the parameter for the given ABI and type."""
        if abi_type == 'ompi':
            return Type.PARAMS_OMPI_ABI[type_name](type_name, **kwargs)
        elif abi_type == 'standard':
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
        """Peturn the text to be used for this parameter  in the prototype declaration."""
        return f'{self.type_text(enable_count=enable_count)} {self.name}'

    @property
    def callback_wrapper_code(self):
        """Return True if this parameter has callback wrapper code to generate."""
        return False

    @property
    def need_async_cleanup(self):
        """Return True if this parameter generates async memory cleanup code."""
        return False

class StandardABIType(Type):

    @property
    def tmpname(self):
        return util.abi_tmp_name(self.name)

    @property
    def argument(self):
        return self.tmpname

    @staticmethod
    def async_callback_index(self):
        return "idx"

@Type.add_type('ERROR_CLASS', abi_type=['ompi'])
class TypeErrorClass(Type):

    def type_text(self, enable_count=False):
        return 'int'

    def return_code(self, name):
        return [f'return {name};']

@Type.add_type('ERROR_CLASS', abi_type=['standard'])
class TypeErrorClassStandard(StandardABIType):

    def type_text(self, enable_count=False):
        return 'int'

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.ERROR_CLASS}({self.name});']

    def return_code(self, name):
        return [f'return {ConvertOMPIToStandard.ERROR_CLASS}({name});']


@Type.add_type('ERROR_CLASS_OUT', abi_type=['ompi'])
class TypeErrorClassOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('ERROR_CLASS_OUT', abi_type=['standard'])
class TypeErrorClassOutStandard(StandardABIType):

    def type_text(self, enable_count=False):
        return 'int *'

    @property
    def final_code(self): 
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.ERROR_CLASS}(*{self.name});']

    @property
    def argument(self):
        return f'{self.name}'

#
# types below seem duplicative of ERROR_CLASS but
# are provided for clarity in the template files
# to distinguish between classes and codes which can
# have different values (in theory) if they are not
# predeinfed by MPI
#
@Type.add_type('ERROR_CODE', abi_type=['ompi'])
class TypeErrorCode(Type):

    def type_text(self, enable_count=False):
        return 'int'

    def return_code(self, name):
        return [f'return {name};']

@Type.add_type('ERROR_CODE', abi_type=['standard'])
class TypeErrorCodeStandard(StandardABIType):

    def type_text(self, enable_count=False):
        return 'int'

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.ERROR_CLASS}({self.name});']

    def return_code(self, name):
        return [f'return {ConvertOMPIToStandard.ERROR_CLASS}({name});']


@Type.add_type('ERROR_CODE_OUT', abi_type=['ompi'])
class TypeErrorCodeOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('ERROR_CODE_OUT', abi_type=['standard'])
class TypeErrorCodeOutStandard(StandardABIType):

    def type_text(self, enable_count=False):
        return 'int *'

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.ERROR_CLASS}(*{self.name});']

    @property
    def argument(self):
        return f'{self.name}'

@Type.add_type('BUFFER', abi_type=['ompi'])
class TypeBuffer(Type):

    def type_text(self, enable_count=False):
        return 'void *'

@Type.add_type('BUFFER_CONST', abi_type=['ompi'])
class TypeBufferConst(Type):

    def type_text(self, enable_count=False):
        return 'const void *'


@Type.add_type('BUFFER_OUT')
class TypeBufferOut(Type):

    def type_text(self, enable_count=False):
        return f'void *'


@Type.add_type('BUFFER_ADDR_OUT', abi_type=['ompi'])
class TypeBufferAddrOut(Type):

    def type_text(self, enable_count=False):
        return f'void *'


@Type.add_type('BUFFER_ADDR_OUT', abi_type=['standard'])
class TypeBufferAddrOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *(void **){self.name} = {ConvertOMPIToStandard.BUFFER}(*(void **){self.name});']

    def type_text(self, enable_count=False):
        return f'void *'

    @property
    def argument(self):
        return f'{self.name}'


@Type.add_type('BUFFER', abi_type=['standard'])
class TypeBufferStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'void *{self.tmpname} = (int *){ConvertFuncs.BUFFER}((void *){self.name});']

    def type_text(self, enable_count=False):
        return 'void *'

@Type.add_type('BUFFER_CONST', abi_type=['standard'])
class TypeBufferConstStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'void *{self.tmpname} = (int *){ConvertFuncs.BUFFER}((void *){self.name});']

    def type_text(self, enable_count=False):
        return 'const void *'

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

@Type.add_type('COUNT_ARRAY_OUT')
class TypeCountArrayOut(TypeCountArray):
    """Array of counts out (either int or MPI_Count)."""

    def parameter(self, enable_count=False, **kwargs):
        count_type = 'MPI_Count' if enable_count else 'int'
        return f'{count_type} {self.name}[]'

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

@Type.add_type('AINT_COUNT_ARRAY_OUT')
class TypeAintCountArrayOut(TypeAintCountArray):
    """Array of counts (either MPI_Aint or MPI_Count)."""

    def parameter(self, enable_count=False, **kwargs):
        count_type = 'MPI_Count' if enable_count else 'MPI_Aint'
        return f'{count_type} {self.name}[]'

@Type.add_type('ELEMENT_COUNT')
class TypeElementCount(Type):
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

@Type.add_type('AINT_ARRAY_OUT')
class TypeAintArrayOut(TypeAintArray):

    def parameter(self, enable_count=False, **kwargs):
        return f'MPI_Aint {self.name}[]'

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

@Type.add_type('INT_ARRAY_OUT')
class TypeIntArrayOut(TypeIntArray):

    def type_text(self, enable_count=False):
        return 'int *'

    def parameter(self, enable_count=False, **kwargs):
        return f'int {self.name}[]'

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

@Type.add_type('DATATYPE_INOUT', abi_type=['ompi'])
class TypeDatatypeOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Datatype *'

@Type.add_type('DATATYPE_ARRAY', abi_type=['ompi'])
class TypeDatatypeArray(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Datatype'

    def parameter(self, enable_count=False, **kwargs):
        return f'const {self.type_text(enable_count=enable_count)} {self.name}[]'

@Type.add_type('NEIGHBOR_DATATYPE_ARRAY', abi_type=['ompi'])
class TypeNeighborDatatypeArray(TypeDatatypeArray):
    pass

@Type.add_type('NEIGHBOR_DATATYPE_ARRAY_ASYNC', abi_type=['ompi'])
class TypeNeighborDatatypeArrayAsync(TypeDatatypeArray):
    pass

@Type.add_type('DATATYPE_ARRAY_OUT', abi_type=['ompi'])
class TypeDatatypeArrayOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Datatype'

    def parameter(self, enable_count=False, **kwargs):
        return f'{self.type_text(enable_count=enable_count)} {self.name}[]'

@Type.add_type('DATATYPE_ARRAY_ASYNC', abi_type=['ompi'])
class TypeDatatypeArray(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Datatype'

    def parameter(self, enable_count=False, **kwargs):
        return f'const {self.type_text(enable_count=enable_count)} {self.name}[]'

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
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.DATATYPE}((MPI_Datatype) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Datatype')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Datatype *) {self.name}'

@Type.add_type('DATATYPE_INOUT', abi_type=['standard'])
class TypeDatatypeInoutStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Datatype {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.DATATYPE}(*{self.name}) : MPI_DATATYPE_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.DATATYPE}((MPI_Datatype) {self.tmpname});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Datatype')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Datatype *) (NULL != {self.name} ? &{self.tmpname} : NULL)'

@Type.add_type('DATATYPE_ARRAY', abi_type=['standard'])
class TypeDatatypeArrayStandard(StandardABIType):

    @property
    def init_code(self):
        if self.count_param is None:
            code = [f'MPI_Comm comm_{self.tmpname} = {ConvertFuncs.COMM}(comm);']
            code.append(f'int size_{self.tmpname} = OMPI_COMM_IS_INTER(comm_{self.tmpname})?ompi_comm_remote_size(comm_{self.tmpname}):ompi_comm_size(comm_{self.tmpname});')
        else:
            code = [f'int size_{self.tmpname} = {self.count_param};']
        code.append(f'MPI_Datatype *{self.tmpname} = NULL;')
        code.append('if('+f'{self.name}' + '!= NULL)' + '{')
        code.append(f'{self.tmpname} = (MPI_Datatype *)ompi_abi_malloc(size_{self.tmpname}, sizeof(MPI_Datatype));')
        code.append(f'if(NULL != {self.tmpname})' + '{')
        code.append(f'for(int i=0;i<size_{self.tmpname};i++)' + '{')
        code.append(f'{self.tmpname}[i] = {ConvertFuncs.DATATYPE}({self.name}[i]);')
        code.append('}')
        code.append('}')
        code.append('}')
        return code

    @property
    def final_code(self):
        code = [f'if({self.tmpname} != NULL){{']
        code.append(f'free({self.tmpname});')
        code.append('}')
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

@Type.add_type('DATATYPE_ARRAY_ASYNC', abi_type=['standard'])
class TypeDatatypeArrayAsyncStandard(TypeDatatypeArrayStandard):

    @property
    def need_async_cleanup(self):
        return True

    @property
    def final_code(self):
        request_tmp_name = util.abi_tmp_name('request')
        code = []
        code.append(f'if((MPI_SUCCESS == ret_value) && (MPI_REQUEST_NULL != {request_tmp_name}) && (!REQUEST_COMPLETE({request_tmp_name})))' + '{')
        code.append(f'ompi_coll_base_nbc_request_t* nb_request = (ompi_coll_base_nbc_request_t*){request_tmp_name};')
        code.append('assert(nb_request->data.release_arrays[idx] == NULL);')
        code.append(f'if (NULL != {self.tmpname}) nb_request->data.release_arrays[idx++] = (void *){self.tmpname};')
        code.append('nb_request->data.release_arrays[idx] = NULL;')
        code.append('} else {')
        code.append(f'if (NULL != {self.tmpname}) free({self.tmpname});')
        code.append('}')
        return code

@Type.add_type('DATATYPE_ARRAY_OUT', abi_type=['standard'])
class TypeDatatypeArrayOutStandard(StandardABIType):

    @property
    def init_code(self):
        code = [f'int size_{self.tmpname} = {self.count_param};']
        code.append(f'MPI_Datatype *{self.tmpname} = (MPI_Datatype *)ompi_abi_malloc({self.count_param},sizeof(MPI_Datatype));')
        return code

    @property
    def final_code(self):
        code = [f'if(NULL != {self.tmpname})' + '{'] 
        code.append(f'for(int i=0;i<size_{self.tmpname};i++)' + '{')
        code.append(f'{self.name}[i] = {ConvertOMPIToStandard.DATATYPE}({self.tmpname}[i]);')
        code.append('}')
        code.append(f'free({self.tmpname});')
        code.append('}')
        return code

    @property
    def tmpname(self):
        return f'{self.name}_tmp'

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Datatype')

    def parameter(self, enable_count=False, **kwargs):
        return f'{self.type_text(enable_count=enable_count)} {self.name}[]'

    @property
    def argument(self):
        return f'(MPI_Datatype *) {self.tmpname}'

@Type.add_type('NEIGHBOR_DATATYPE_ARRAY', abi_type=['standard'])
class TypeNeighborDatatypeArrayStandard(TypeDatatypeArrayStandard):

    @property
    def init_code(self):
        code = [f'MPI_Comm comm_{self.tmpname} = {ConvertFuncs.COMM}(comm);']
        code.append(f'int indegree_{self.tmpname}, outdegree_{self.tmpname}, size_{self.tmpname};')
        code.append(f'mca_topo_base_neighbor_count(comm_{self.tmpname}, &indegree_{self.tmpname}, &outdegree_{self.tmpname});')
        code.append(f'MPI_Datatype *{self.tmpname} = NULL;')
        if self.name == "sendtypes":
            code.append(f'size_{self.tmpname} = outdegree_{self.tmpname};')
        if self.name == "recvtypes":
            code.append(f'size_{self.tmpname} = indegree_{self.tmpname};')
        code.append('if('+f'{self.name}' + '!= NULL)' + '{')
        code.append(f'{self.tmpname} = (MPI_Datatype *)ompi_abi_malloc(size_{self.tmpname}, sizeof(MPI_Datatype));')
        code.append('if('+f'{self.tmpname}' + '!= NULL)' + '{')
        code.append(f'for(int i=0;i<size_{self.tmpname};i++){{')
        code.append(f'{self.tmpname}[i] = {ConvertFuncs.DATATYPE}({self.name}[i]);')
        code.append('}')
        code.append('}')
        code.append('}')
        return code

@Type.add_type('NEIGHBOR_DATATYPE_ARRAY_ASYNC', abi_type=['standard'])
class TypeNeighborDatatypeArrayAsyncStandard(TypeNeighborDatatypeArrayStandard):

    @property
    def need_async_cleanup(self):
        return True

    @property
    def final_code(self):
       request_tmp_name = util.abi_tmp_name('request')
       code = []
       code.append(f'if((MPI_SUCCESS == ret_value) && (MPI_REQUEST_NULL != {request_tmp_name}) && (!REQUEST_COMPLETE({request_tmp_name})))' + '{')
       code.append(f'ompi_coll_base_nbc_request_t* nb_request = (ompi_coll_base_nbc_request_t*){request_tmp_name};')
       code.append('assert(nb_request->data.release_arrays[idx] == NULL);')
       code.append(f'nb_request->data.release_arrays[idx++] = (void *){self.tmpname};')
       code.append('nb_request->data.release_arrays[idx] = NULL;')
       code.append('} else {')
       code.append(f'if (NULL != {self.tmpname}) free({self.tmpname});')
       code.append('}')
       return code

@Type.add_type('OP', abi_type=['ompi'])
class TypeOp(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Op'


@Type.add_type('OP', abi_type=['standard'])
class TypeOpStandard(StandardABIType):

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
class TypeOpOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.OP}((MPI_Op) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Op')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Op *) {self.name}'

@Type.add_type('OP_INOUT', abi_type=['ompi'])
class TypeOpInOut(TypeOpOut):
    pass

@Type.add_type('OP_INOUT', abi_type=['standard'])
class TypeOpInOutStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Op {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.OP}(*{self.name}) : MPI_OP_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.OP}((MPI_Op) {self.tmpname});']
        
    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Op')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Op *) (NULL != {self.name} ? &{self.tmpname} : NULL)'

@Type.add_type('TAG', abi_type=['ompi'])
class TypeTag(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('TAG', abi_type=['standard'])
class TypeTagStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.TAG}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('TAG_OUT', abi_type=['ompi'])
class TypeTagOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('TAG_OUT', abi_type=['standard'])
class TypeTagOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.TAG}(*{self.name});']

    def type_text(self, enable_count=False):
        return f'int *'

    @property
    def argument(self):
        return f'(int *) {self.name}'

@Type.add_type('ROOT', abi_type=['ompi'])
class TypeRoot(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('ROOT', abi_type=['standard'])
class TypeRootStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.ROOT}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('SOURCE', abi_type=['ompi'])
class TypeSource(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('SOURCE', abi_type=['standard'])
class TypeSourceStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.SOURCE}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('SOURCE_ARRAY', abi_type=['ompi'])
class TypeSourceArray(Type):

    def type_text(self, enable_count=False):
        return 'const int*'

    def parameter(self, enable_count=False, **kwargs):
        return f'const int {self.name}[]'

@Type.add_type('SOURCE_ARRAY', abi_type=['standard'])
class TypeSourceArrayStandard(StandardABIType):

    @property
    def init_code(self):
        code = [(f'int *{self.tmpname} = NULL;')]
        code.append('if('+f'{self.name}' + '!= NULL)' + '{')
        code.append(f'{self.tmpname} = (int *)ompi_abi_malloc({self.count_param}, sizeof(int));')
        code.append(f'if (NULL != {self.tmpname})' + '{')
        code.append(f'for(int i=0;i<{self.count_param};i++){{')
        code.append(f'{self.tmpname}[i] = {ConvertFuncs.SOURCE}({self.name}[i]);')
        code.append('}')
        code.append('}')
        code.append('}')
        return code

    @property
    def final_code(self):
        code = [f'if({self.tmpname} != NULL){{']
        code.append(f'free({self.tmpname});')
        code.append('}')
        return code

    def type_text(self, enable_count=False):
        return 'int *'

    def parameter(self, enable_count=False, **kwargs):
        return f'const int {self.name}[]'

@Type.add_type('SOURCE_OUT', abi_type=['ompi'])
class TypeSourceOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('SOURCE_OUT', abi_type=['standard'])
class TypeSourceOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.SOURCE}(*{self.name});']
 
    def type_text(self, enable_count=False):
        return f'int *'

    @property
    def argument(self):
        return f'(int *) {self.name}'

@Type.add_type('SOURCE_ARRAY_OUT', abi_type=['ompi'])
class TypeSourceArrayOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

    def parameter(self, enable_count=False, **kwargs):
        return f'int {self.name}[]'

@Type.add_type('SOURCE_ARRAY_OUT', abi_type=['standard'])
class TypeSourceArrayOutStandard(StandardABIType):
        
    @property
    def init_code(self):
        code = [f'int *{self.tmpname} = (int*)ompi_abi_malloc({self.count_param}, sizeof(int));']
        return code
    
    @property
    def final_code(self):
        code = [f'if (NULL != {self.name}){{']
        code.append(f'if (NULL != {self.tmpname}){{')
        code.append(f'for(int i=0;i<{self.count_param};i++){{')
        code.append(f'{self.name}[i] = {ConvertOMPIToStandard.SOURCE}({self.tmpname}[i]);')
        code.append('}')
        code.append('}')
        code.append('}')
        code.append(f'if (NULL != {self.tmpname}) free({self.tmpname});')
        return code
        
    def type_text(self, enable_count=False):
        return 'int *'

    def parameter(self, enable_count=False, **kwargs):
        return f'int {self.name}[]'

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
class TypeCommunicatorOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Comm *'


@Type.add_type('COMM_OUT', abi_type=['standard'])
class TypeCommunicatorOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.COMM}((MPI_Comm) *{self.name});']
 
    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Comm')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Comm *) {self.name}'


@Type.add_type('COMM_INOUT', abi_type=['ompi'])
class TypeCommInOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Comm *'


@Type.add_type('COMM_INOUT', abi_type=['standard'])
class TypeCommInOutStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Comm {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.COMM}(*{self.name}) : MPI_COMM_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.COMM}({self.tmpname});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Comm')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Comm *) (NULL != {self.name} ? &{self.tmpname} : NULL)'


@Type.add_type('WIN', abi_type=['ompi'])
class TypeWin(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Win'


@Type.add_type('WIN', abi_type=['standard'])
class TypeWinStandard(StandardABIType):

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
class TypeWinOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Win *'


@Type.add_type('WIN_OUT', abi_type=['standard'])
class TypeWinOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.WIN}((MPI_Win) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Win')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Win *) {self.name}'

@Type.add_type('WIN_INOUT', abi_type=['ompi'])
class TypeWinInOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Win *'


@Type.add_type('WIN_INOUT', abi_type=['standard'])
class TypeWinInOutStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Win {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.WIN}(*{self.name}) : MPI_WIN_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.WIN}({self.tmpname});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Win')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Win *) (NULL != {self.name} ? &{self.tmpname} : NULL)'

@Type.add_type('REQUEST', abi_type=['ompi'])
class TypeRequest(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Request'


@Type.add_type('REQUEST', abi_type=['standard'])
class TypeRequestStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Request {self.tmpname} = {ConvertFuncs.REQUEST}({self.name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Request')

    @property
    def argument(self):
        return f'(MPI_Request) {self.tmpname}'

@Type.add_type('REQUEST_CONST', abi_type=['ompi'])
class TypeConstRequest(TypeRequest):

    def type_text(self, enable_count=False):
        return f'const MPI_Request *'

    def parameter(self, enable_count=False, **kwargs):
        if self.count_param is None:
            return f'const MPI_Request {self.name}'
        else:
            return f'const MPI_Request {self.name}[]'

@Type.add_type('REQUEST_CONST', abi_type=['standard'])
class TypeConstRequestStandard(TypeRequestStandard):

    @property
    def init_code(self):
        if self.count_param is None:
            code = [f'MPI_Request {self.tmpname} = {ConvertFuncs.REQUEST}({self.name});']
        else:
            code = [f'int size_{self.tmpname} = {self.count_param};']
        code.append(f'MPI_Request *{self.tmpname} = (MPI_Request *)ompi_abi_malloc(size_{self.tmpname}, sizeof(MPI_Request));')
        code.append(f'if(NULL !={self.tmpname})' + '{')
        code.append(f'for(int i=0;i<size_{self.tmpname};i++){{')
        code.append(f'{self.tmpname}[i] = {ConvertFuncs.REQUEST}({self.name}[i]);')
        code.append('}')
        code.append('}')
        return code

    @property
    def final_code(self):
        if self.count_param is not None:
            code = [f'if(NULL != {self.tmpname}) free({self.tmpname});']
        return code

    def type_text(self, enable_count=False):
        name = self.mangle_name('MPI_Request')
        return f'const {name} *'

    @property
    def argument(self):
        return f'(MPI_Request *) {self.tmpname}'

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Request'

    def return_code(self, name):
        return [f'return {ConvertOMPIToStandard.REQUEST}({name});']

    def parameter(self, enable_count=False, **kwargs):
        type_name = self.mangle_name('MPI_Request')
        if self.count_param is None:
            return f'const {type_name} *{self.name}'
        else:
            return f'const {type_name} {self.name}[]'

        
@Type.add_type('REQUEST_INOUT', abi_type=['ompi'])
class TypeRequestInOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Request *'

@Type.add_type('REQUEST_INOUT', abi_type=['standard'])
class TypeRequestInOutStandard(StandardABIType):

    @property
    def init_code(self):
        if self.count_param is None:
            code = [f'MPI_Request {self.tmpname} = {ConvertFuncs.REQUEST}(*{self.name});']
        else:
            code = [f'int size_{self.tmpname} = {self.count_param};']
            code.append(f'MPI_Request *{self.tmpname} = (MPI_Request *)ompi_abi_malloc(size_{self.tmpname}, sizeof(MPI_Request));')
            code.append(f'if (NULL != {self.tmpname})' + '{')
            code.append(f'for(int i=0;i<size_{self.tmpname};i++){{')
            code.append(f'{self.tmpname}[i] = {ConvertFuncs.REQUEST}({self.name}[i]);')
            code.append('}')
            code.append('}')
        return code

    @property
    def final_code(self):
        if self.count_param is None:
            code = [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.REQUEST}({self.tmpname});']
        else:
            code = [f'if (NULL != {self.name})' + '{']
            code.append(f'if (NULL != {self.tmpname})' + '{')
            code.append('for (int i = 0; i < %s; ++i) {' % (self.count_param,))
            code.append(f'{self.name}[i] = {ConvertOMPIToStandard.REQUEST}({self.tmpname}[i]);')
            code.append('}')
            code.append('}')
            code.append('}')
            code.append(f'if (NULL != {self.tmpname}) free({self.tmpname});')
        return code

    @property
    def argument(self):
        if self.count_param is None:
            code = f'(MPI_Request *) &{self.tmpname}'
        else:
            code = f'(MPI_Request *){self.tmpname}'
        return code

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
            code.append(f'MPI_Status {self.tmpname} = ' + '{0};')
        else:
            code.append(f'MPI_Status *{self.tmpname} = NULL;')
        code.append(self.if_should_set_status())
        if self.count_param is not None:
            code.append(f'{self.tmpname} = (MPI_Status *)ompi_abi_malloc({self.count_param}, sizeof(MPI_Status));')
            code.extend([
                'for (int i = 0; i < %s; ++i) {' % (self.count_param,),
                f'{ConvertFuncs.STATUS}(&{self.tmpname}[i], &{self.name}[i]);',
                '}',
            ])
            code.append(f'{self.status_argument} = {self.tmpname};')
        else:
            code.append(f'{ConvertFuncs.STATUS}(&{self.tmpname}, {self.name});')
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
                'for (int i = 0; i < %s; ++i) {' % (self.outcount_param,),
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
        code.append(self.if_should_set_status())
        code.append(f'{ConvertFuncs.STATUS}(&{self.tmpname}, ({mangle_type} *){self.name});')
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


@Type.add_type('FINT')
class TypeFint(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Fint'


@Type.add_type('FINT_CONST')
class TypeFintConst(Type):

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

@Type.add_type('INFO_INOUT', abi_type=['ompi'])
class TypeInfoInOut(TypeInfoOut):
    pass

@Type.add_type('INFO_INOUT', abi_type=['standard'])
class TypeInfoInOutStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Info {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.INFO}(*{self.name}) : MPI_INFO_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.INFO}((MPI_Info) {self.tmpname});']

    @property
    def argument(self):
        return f'(MPI_Info *) (NULL != {self.name} ? &{self.tmpname} : NULL)'

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Info')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Info *) (NULL != {self.name} ? &{self.tmpname} : NULL)'

        
@Type.add_type('INFO_ARRAY', abi_type=['ompi'])
class TypeInfoArray(Type):

    def type_text(self, enable_count=False):
        return 'const MPI_Info *'

    def parameter(self, enable_count=False, **kwargs):
        return f'const MPI_Info {self.name}[]'


@Type.add_type('INFO_ARRAY', abi_type=['standard'])
class TypeInfoArrayStandard(StandardABIType):

#
# TODO may need a better way to generalize for case of non-explicit count_param
#
    @property
    def init_code(self):
        code = [f'int size_{self.tmpname} = {self.count_param};']
        code.append(f'MPI_Info *{self.tmpname} = (MPI_Info *)ompi_abi_malloc(size_{self.tmpname}, sizeof(MPI_Info));')
        code.append(f'if ((NULL != {self.tmpname}) && (NULL != {self.name})){{')
        code.append(f'for(int i=0;i<size_{self.tmpname};i++){{')
        code.append(f'{self.tmpname}[i] = {ConvertFuncs.INFO}({self.name}[i]);')
        code.append('}')
        code.append('}')
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
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.FILE}((MPI_File) *{self.name});']

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
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.FILE}({self.tmpname});']

    @property
    def argument(self):
        return f'(MPI_File *) (NULL != {self.name} ? &{self.tmpname} : NULL)'

@Type.add_type('MESSAGE', abi_type=['ompi'])
class TypeMessage(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Message'


@Type.add_type('MESSAGE', abi_type=['standard'])
class TypeMessageStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Comm {self.tmpname} = {ConvertFuncs.MESSAGE}({self.name});']

#   @property
#   def argument(self):
#       return f'(MPI_Message) {self.name}'

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Message')

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Message'

#   def return_code(self, name):
#       return [f'return {ConvertOMPIToStandard.MESSAGE}({name});']
        
@Type.add_type('MESSAGE_OUT', abi_type=['ompi'])
class TypeMessageOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Message *'


@Type.add_type('MESSAGE_OUT', abi_type=['standard'])
class TypeMessageOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.MESSAGE}((MPI_Message) *{self.name});']

    @property
    def argument(self):
        return f'(MPI_Message *) {self.name}'

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Message')
        return f'{type_name} *'

@Type.add_type('MESSAGE_INOUT', abi_type=['ompi'])
class TypeMessageInOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Message *'


@Type.add_type('MESSAGE_INOUT', abi_type=['standard'])
class TypeMessageInOutStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Message {self.tmpname} = {ConvertFuncs.MESSAGE}(*{self.name});']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.MESSAGE}({self.tmpname});']

    @property
    def argument(self):
        return f'(MPI_Message *) (NULL != {self.name} ? &{self.tmpname} : NULL)'

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Message')
        return f'{type_name} *'


@Type.add_type('TS_LEVEL', abi_type=['ompi'])
class TypeTSLevel(Type):

    def type_text(self, enable_count=False):
        return 'int'


@Type.add_type('TS_LEVEL', abi_type=['standard'])
class TypeTSLevelStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.TS_LEVEL}({self.name});']

    def tmp_type_text(self, enable_count=False):
        return 'int'

    def return_code(self, name):
        return [f'return {ConvertOMPIToStandard.TS_LEVEL}({name});']

    def type_text(self, enable_count=False):
        return 'int'


@Type.add_type('TS_LEVEL_OUT', abi_type=['ompi'])
class TypeTSLevelOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'


@Type.add_type('TS_LEVEL_OUT', abi_type=['standard'])
class TypeTSLevelOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.TS_LEVEL}((int) *{self.name});']

    def type_text(self, enable_count=False):
        return f'int *'

    @property
    def argument(self):
        return f'{self.name}'

@Type.add_type('COMM_ERRHANDLER_FUNCTION', abi_type=['ompi'])
class TypeCommErrhandlerFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Comm_errhandler_function *'


@Type.add_type('COMM_ERRHANDLER_FUNCTION', abi_type=['standard'])
class TypeCommErrhandlerFunctionStandard(StandardABIType):

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Comm_errhandler_function')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Comm_errhandler_function *) {self.name}'

@Type.add_type('FILE_ERRHANDLER_FUNCTION', abi_type=['ompi'])
class TypeFileErrhandlerFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_File_errhandler_function *'


@Type.add_type('FILE_ERRHANDLER_FUNCTION', abi_type=['standard'])
class TypeFileErrhandlerFunctionStandard(StandardABIType):

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_File_errhandler_function')
        return f'{type_name} *'
    
    @property
    def argument(self):
        return f'(MPI_File_errhandler_function *) {self.name}'

@Type.add_type('COPY_FUNCTION', abi_type=['ompi'])
class TypeCopyFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Copy_function *'


@Type.add_type('COPY_FUNCTION', abi_type=['standard'])
class TypeCopyFunctionStandard(StandardABIType):

    def type_text(self, enable_count=False):
        return 'MPI_Copy_function *'

@Type.add_type('DELETE_FUNCTION', abi_type=['ompi'])
class TypeDeleteFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Delete_function *'


@Type.add_type('DELETE_FUNCTION', abi_type=['standard'])
class TypeDeleteFunctionStandard(StandardABIType):

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

    def type_text(self, enable_count=False):
        return 'MPI_User_function_c *' if enable_count else 'MPI_User_function *'

@Type.add_type('COMM_COPY_ATTR_FUNCTION', abi_type=['ompi'])
class TypeCommCopyAttrFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Comm_copy_attr_function *'


@Type.add_type('COMM_COPY_ATTR_FUNCTION', abi_type=['standard'])
class TypeCommCopyAttrFunctionStandard(StandardABIType):

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Comm_copy_attr_function')
        return f'{type_name} *'

#    @property
#    def argument(self):
#        return f'(MPI_Comm_copy_attr_function *) {self.name}'

    @property
    def init_code(self):
        code = []
        code = [f'MPI_Comm_copy_attr_function *{self.tmpname} = {ConvertFuncs.COMM_COPY_ATTR_FUNCTION}({self.name});']
        code.append('ompi_abi_wrapper_helper_t *helper = NULL;')
        code.append('MPI_Comm_copy_attr_function_ABI_INTERNAL *copy_fn;')
        code.append('helper = ( ompi_abi_wrapper_helper_t *)ompi_abi_malloc(1, sizeof(ompi_abi_wrapper_helper_t));')
        code.append('if (NULL == helper)  return MPI_ERR_NO_MEM;')
        code.append(f'if ({self.name} == MPI_COMM_NULL_COPY_FN_ABI_INTERNAL)'  + '{')
        code.append('copy_fn = ABI_C_MPI_COMM_NULL_COPY_FN;')
        code.append('} else if (' + f'{self.name}' + ' == MPI_COMM_DUP_FN_ABI_INTERNAL) {')
        code.append('copy_fn = ABI_C_MPI_COMM_DUP_FN;')
        code.append('} else {')
        code.append(f'copy_fn = {self.name};')
        code.append('}')
        code.append('helper->user_copy_fn = copy_fn;')
        code.append('helper->user_extra_state = extra_state;')
        code.append('extra_state = helper;')
        return code

    # TODO: This should be generalized to be reused with type and win
    @property
    def callback_wrapper_code(self):
        code = []
        code = ['typedef struct {']
        code.append('    MPI_Comm_copy_attr_function_ABI_INTERNAL *user_copy_fn;')
        code.append('    MPI_Comm_delete_attr_function_ABI_INTERNAL *user_delete_fn;')
        code.append('    void *user_extra_state;')
        code.append('} ompi_abi_wrapper_helper_t;')
        code.append('static int ompi_abi_copy_attr_fn(MPI_Comm oldcomm, int comm_keyval, void *extra_state, void *attribute_val_in, void *attribute_val_out, int *flag)')
        code.append('{')
        code.append('    ompi_abi_wrapper_helper_t *helper = (ompi_abi_wrapper_helper_t *)extra_state;')
        code.append('    MPI_Comm_ABI_INTERNAL comm_tmp = ompi_convert_comm_ompi_to_standard(oldcomm);')
        code.append('    int comm_keyval_tmp = ompi_convert_attr_key_ompi_to_standard(comm_keyval);')
        code.append('    return helper->user_copy_fn((MPI_Comm_ABI_INTERNAL)comm_tmp, comm_keyval_tmp, helper->user_extra_state, attribute_val_in, attribute_val_out, flag);')
        code.append('}')
        code.append('static int ompi_abi_delete_attr_fn(MPI_Comm oldcomm, int comm_keyval, void *attribute_val, void *extra_state)')
        code.append('{')
        code.append('    ompi_abi_wrapper_helper_t *helper = (ompi_abi_wrapper_helper_t *)extra_state;')
        code.append('    MPI_Comm_ABI_INTERNAL comm_tmp = ompi_convert_comm_ompi_to_standard(oldcomm);')
        code.append('    int comm_keyval_tmp = ompi_convert_attr_key_ompi_to_standard(comm_keyval);')
        code.append('    return helper->user_delete_fn((MPI_Comm_ABI_INTERNAL)comm_tmp, comm_keyval_tmp, attribute_val, helper->user_extra_state);')
        code.append('}')
        return code

@Type.add_type('COMM_DELETE_ATTR_FUNCTION', abi_type=['ompi'])
class TypeCommDeleteAttrFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Comm_delete_attr_function *'


@Type.add_type('COMM_DELETE_ATTR_FUNCTION', abi_type=['standard'])
class TypeCommDeleteAttrFunctionStandard(StandardABIType):

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Comm_delete_attr_function')
        return f'{type_name} *'

#
# note the code generated here relies on that generated for
# COMM_COPY_ATTR_FUNCTION above
#
    @property
    def init_code(self):
        code = []
        code = [f'MPI_Comm_delete_attr_function *{self.tmpname} = {ConvertFuncs.COMM_DELETE_ATTR_FUNCTION}({self.name});']
        code.append('MPI_Comm_delete_attr_function_ABI_INTERNAL *delete_fn;')
        code.append(f'if ({self.name} == MPI_COMM_NULL_DELETE_FN_ABI_INTERNAL)'  + '{')
        code.append('delete_fn = ABI_C_MPI_COMM_NULL_DELETE_FN;')
        code.append('} else {')
        code.append(f'delete_fn = {self.name};')
        code.append('}')
        code.append('helper->user_delete_fn = delete_fn;')
        return code

@Type.add_type('GREQUEST_QUERY_FUNCTION', abi_type=['ompi'])
class TypeGrequestQueryFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Grequest_query_function *'


@Type.add_type('GREQUEST_QUERY_FUNCTION', abi_type=['standard'])
class TypeGrequestQueryFunctionStandard(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Grequest_query_function *'

@Type.add_type('GREQUEST_FREE_FUNCTION', abi_type=['ompi'])
class TypeGrequestFreeFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Grequest_free_function *'


@Type.add_type('GREQUEST_FREE_FUNCTION', abi_type=['standard'])
class TypeGrequestFreeFunctionStandard(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Grequest_free_function *'

@Type.add_type('GREQUEST_CANCEL_FUNCTION', abi_type=['ompi'])
class TypeGrequestCancelFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Grequest_cancel_function *'


@Type.add_type('GREQUEST_CANCEL_FUNCTION', abi_type=['standard'])
class TypeGrequestCancelFunctionStandard(Type):

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

    def type_text(self, enable_count=False):
        return 'MPI_Datarep_extent_function *'

@Type.add_type('SESSION_ERRHANDLER_FUNCTION', abi_type=['ompi'])
class TypeSessionErrhandlerFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Session_errhandler_function *'


@Type.add_type('SESSION_ERRHANDLER_FUNCTION', abi_type=['standard'])
class TypeSessionErrhandlerFunctionStandard(StandardABIType):

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Session_errhandler_function')
        return f'{type_name} *'
    
    @property
    def argument(self):
        return f'(MPI_Session_errhandler_function *) {self.name}'

@Type.add_type('TYPE_COPY_ATTR_FUNCTION', abi_type=['ompi'])
class TypeTypeCopyAttrFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Type_copy_attr_function *'

@Type.add_type('TYPE_COPY_ATTR_FUNCTION', abi_type=['standard'])
class TypeTypeCopyAttrFunctionStandard(StandardABIType):

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Type_copy_attr_function')
        return f'{type_name} *'

    @property
    def init_code(self):
        code = []
        code = [f'MPI_Type_copy_attr_function *{self.tmpname} = {ConvertFuncs.TYPE_COPY_ATTR_FUNCTION}({self.name});']
        code.append('ompi_abi_wrapper_helper_t *helper = NULL;')
        code.append('MPI_Type_copy_attr_function_ABI_INTERNAL *copy_fn;')
        code.append('helper = ( ompi_abi_wrapper_helper_t *)ompi_abi_malloc(1, sizeof(ompi_abi_wrapper_helper_t));')
        code.append('if (NULL == helper)  return MPI_ERR_NO_MEM;')
        code.append(f'if ({self.name} == MPI_TYPE_NULL_COPY_FN_ABI_INTERNAL)'  + '{')
        code.append('copy_fn = ABI_C_MPI_TYPE_NULL_COPY_FN;')
        code.append('} else if (' + f'{self.name}' + ' == MPI_TYPE_DUP_FN_ABI_INTERNAL) {')
        code.append('copy_fn = ABI_C_MPI_TYPE_DUP_FN;')
        code.append('} else {')
        code.append(f'copy_fn = {self.name};')
        code.append('}')
        code.append('helper->user_copy_fn = copy_fn;')
        code.append('helper->user_extra_state = extra_state;')
        code.append('extra_state = helper;')
        return code

    # TODO: This should be generalized to be reused with type and win
    @property
    def callback_wrapper_code(self):
        code = []
        code = ['typedef struct {']
        code.append('    MPI_Type_copy_attr_function_ABI_INTERNAL *user_copy_fn;')
        code.append('    MPI_Type_delete_attr_function_ABI_INTERNAL *user_delete_fn;')
        code.append('    void *user_extra_state;')
        code.append('} ompi_abi_wrapper_helper_t;')
        code.append('static int ompi_abi_copy_attr_fn(MPI_Datatype oldtype, int type_keyval, void *extra_state, void *attribute_val_in, void *attribute_val_out, int *flag)')
        code.append('{')
        code.append('    ompi_abi_wrapper_helper_t *helper = (ompi_abi_wrapper_helper_t *)extra_state;')
        code.append('    MPI_Datatype_ABI_INTERNAL type_tmp = ompi_convert_datatype_ompi_to_standard(oldtype);')
        code.append('    int type_keyval_tmp = ompi_convert_attr_key_ompi_to_standard(type_keyval);')
        code.append('    return helper->user_copy_fn((MPI_Datatype_ABI_INTERNAL)type_tmp, type_keyval_tmp, helper->user_extra_state, attribute_val_in, attribute_val_out, flag);')
        code.append('}')
        code.append('static int ompi_abi_delete_attr_fn(MPI_Datatype oldtype, int type_keyval, void *attribute_val, void *extra_state)')
        code.append('{')
        code.append('    ompi_abi_wrapper_helper_t *helper = (ompi_abi_wrapper_helper_t *)extra_state;')
        code.append('    MPI_Datatype_ABI_INTERNAL type_tmp = ompi_convert_datatype_ompi_to_standard(oldtype);')
        code.append('    int type_keyval_tmp = ompi_convert_attr_key_ompi_to_standard(type_keyval);')
        code.append('    return helper->user_delete_fn((MPI_Datatype_ABI_INTERNAL)type_tmp, type_keyval_tmp, attribute_val, helper->user_extra_state);')
        code.append('}')
        return code

@Type.add_type('TYPE_DELETE_ATTR_FUNCTION', abi_type=['ompi'])
class TypeTypeDeleteAttrFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Type_delete_attr_function *'


@Type.add_type('TYPE_DELETE_ATTR_FUNCTION', abi_type=['standard'])
class TypeTypeDeleteAttrFunctionStandard(StandardABIType):

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Type_delete_attr_function')
        return f'{type_name} *'
#
# note the code generated here relies on that generated for
# TYPE_COPY_ATTR_FUNCTION above
#
    @property
    def init_code(self):
        code = []
        code = [f'MPI_Type_delete_attr_function *{self.tmpname} = {ConvertFuncs.TYPE_DELETE_ATTR_FUNCTION}({self.name});']
        code.append('MPI_Type_delete_attr_function_ABI_INTERNAL *delete_fn;')
        code.append(f'if ({self.name} == MPI_TYPE_NULL_DELETE_FN_ABI_INTERNAL)'  + '{')
        code.append('delete_fn = ABI_C_MPI_TYPE_NULL_DELETE_FN;')
        code.append('} else {')
        code.append(f'delete_fn = {self.name};')
        code.append('}')
        code.append('helper->user_delete_fn = delete_fn;')
        return code

@Type.add_type('WIN_ERRHANDLER_FUNCTION', abi_type=['ompi'])
class TypeWinErrhandlerFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Win_errhandler_function *'


@Type.add_type('WIN_ERRHANDLER_FUNCTION', abi_type=['standard'])
class TypeWinErrhandlerFunctionStandard(StandardABIType):
                
    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Win_errhandler_function')
        return f'{type_name} *'
    
    @property
    def argument(self):
        return f'(MPI_Win_errhandler_function *) {self.name}'

@Type.add_type('WIN_COPY_ATTR_FUNCTION', abi_type=['ompi'])
class TypeWinCopyAttrFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Win_copy_attr_function *'


@Type.add_type('WIN_COPY_ATTR_FUNCTION', abi_type=['standard'])
class TypeWinCopyAttrFunctionStandard(StandardABIType):

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Win_copy_attr_function')
        return f'{type_name} *'

    @property
    def init_code(self):
        code = []
        code = [f'MPI_Win_copy_attr_function *{self.tmpname} = {ConvertFuncs.WIN_COPY_ATTR_FUNCTION}({self.name});']
        code.append('ompi_abi_wrapper_helper_t *helper = NULL;')
        code.append('MPI_Win_copy_attr_function_ABI_INTERNAL *copy_fn;')
        code.append('helper = ( ompi_abi_wrapper_helper_t *)ompi_abi_malloc(1,sizeof(ompi_abi_wrapper_helper_t));')
        code.append('if (NULL == helper)  return MPI_ERR_NO_MEM;')
        code.append(f'if ({self.name} == MPI_WIN_NULL_COPY_FN_ABI_INTERNAL)'  + '{')
        code.append('copy_fn = ABI_C_MPI_WIN_NULL_COPY_FN;')
        code.append('} else if (' + f'{self.name}' + ' == MPI_WIN_DUP_FN_ABI_INTERNAL) {')
        code.append('copy_fn = ABI_C_MPI_WIN_DUP_FN;')
        code.append('} else {')
        code.append(f'copy_fn = {self.name};')
        code.append('}')
        code.append('helper->user_copy_fn = copy_fn;')
        code.append('helper->user_extra_state = extra_state;')
        code.append('extra_state = helper;')
        return code

    @property
    def callback_wrapper_code(self):
        code = []
        code = ['typedef struct {']
        code.append('    MPI_Win_copy_attr_function_ABI_INTERNAL *user_copy_fn;')
        code.append('    MPI_Win_delete_attr_function_ABI_INTERNAL *user_delete_fn;')
        code.append('    void *user_extra_state;')
        code.append('} ompi_abi_wrapper_helper_t;')
        code.append('static int ompi_abi_copy_attr_fn(MPI_Win oldwin, int win_keyval, void *extra_state, void *attribute_val_in, void *attribute_val_out, int *flag)')
        code.append('{')
        code.append('    ompi_abi_wrapper_helper_t *helper = (ompi_abi_wrapper_helper_t *)extra_state;')
        code.append('    MPI_Win_ABI_INTERNAL win_tmp = ompi_convert_win_ompi_to_standard(oldwin);')
        code.append('    int win_keyval_tmp = ompi_convert_attr_key_ompi_to_standard(win_keyval);')
        code.append('    return helper->user_copy_fn((MPI_Win_ABI_INTERNAL)win_tmp, win_keyval_tmp, helper->user_extra_state, attribute_val_in, attribute_val_out, flag);')
        code.append('}')
        code.append('static int ompi_abi_delete_attr_fn(MPI_Win oldwin, int win_keyval, void *attribute_val, void *extra_state)')
        code.append('{')
        code.append('    ompi_abi_wrapper_helper_t *helper = (ompi_abi_wrapper_helper_t *)extra_state;')
        code.append('    MPI_Win_ABI_INTERNAL win_tmp = ompi_convert_win_ompi_to_standard(oldwin);')
        code.append('    int win_keyval_tmp = ompi_convert_attr_key_ompi_to_standard(win_keyval);')
        code.append('    return helper->user_delete_fn((MPI_Win_ABI_INTERNAL)win_tmp, win_keyval_tmp, attribute_val, helper->user_extra_state);')
        code.append('    free(helper);')
        code.append('}')
        return code


@Type.add_type('WIN_DELETE_ATTR_FUNCTION', abi_type=['ompi'])
class TypeWinDeleteAttrFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Win_delete_attr_function *'


@Type.add_type('WIN_DELETE_ATTR_FUNCTION', abi_type=['standard'])
class TypeWinDeleteAttrFunctionStandard(StandardABIType):

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Win_delete_attr_function')
        return f'{type_name} *'
#
# note the code generated here relies on that generated for
# WIN_COPY_ATTR_FUNCTION above
#
    @property
    def init_code(self):
        code = [f'MPI_Win_delete_attr_function *{self.tmpname} = {ConvertFuncs.WIN_DELETE_ATTR_FUNCTION}({self.name});']
        code.append('MPI_Win_delete_attr_function_ABI_INTERNAL *delete_fn;')
        code.append(f'if ({self.name} == MPI_WIN_NULL_DELETE_FN_ABI_INTERNAL)'  + '{')
        code.append('delete_fn = ABI_C_MPI_WIN_NULL_DELETE_FN;')
        code.append('} else {')
        code.append(f'delete_fn = {self.name};')
        code.append('}')
        code.append('helper->user_delete_fn = delete_fn;')
        return code

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
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.ERRHANDLER}((MPI_Errhandler) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Errhandler')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Errhandler *) {self.name}'

@Type.add_type('ERRHANDLER_INOUT', abi_type=['ompi'])
class TypeErrhandlerInOut(TypeErrhandlerOut):
    pass

@Type.add_type('ERRHANDLER_INOUT', abi_type=['standard'])
class TypeErrhandlerInOutStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Errhandler {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.ERRHANDLER}(*{self.name}) : MPI_ERRHANDLER_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.ERRHANDLER}((MPI_Errhandler) {self.tmpname});']
        
    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Errhandler')
        return f'{type_name} *'
    
    @property
    def argument(self):
        return f'(MPI_Errhandler *) (NULL != {self.name} ? &{self.tmpname} : NULL)'

@Type.add_type('GROUP', abi_type=['ompi'])
class TypeGroup(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Group'


@Type.add_type('GROUP', abi_type=['standard'])
class TypeGroupStandard(StandardABIType):

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
class TypeGroupOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.GROUP}((MPI_Group) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Group')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Group *) {self.name}'


@Type.add_type('GROUP_INOUT', abi_type=['ompi'])
class TypeGroupInOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Group *'


@Type.add_type('GROUP_INOUT', abi_type=['standard'])
class TypeGroupInOutStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Group {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.GROUP}(*{self.name}) : MPI_GROUP_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.GROUP}({self.tmpname});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Group')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Group *) (NULL != {self.name} ? &{self.tmpname} : NULL)'

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
        return [f'MPI_Session {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.SESSION}(*{self.name}) : MPI_SESSION_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.SESSION}({self.tmpname});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Session')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Session *) (NULL != {self.name} ? &{self.tmpname} : NULL)'


@Type.add_type('SESSION_OUT', abi_type=['standard'])
class TypeSessionOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.SESSION}((MPI_Session) *{self.name});']

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

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Session')

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Session'

    def return_code(self, name):
        return [f'return {ConvertOMPIToStandard.SESSION}({name});']


@Type.add_type('T_ENUM', abi_type=['ompi'])
class TypeTEnum(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_enum'

@Type.add_type('T_ENUM', abi_type=['standard'])
class TypeTEnumStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_T_enum {self.tmpname} = {ConvertFuncs.T_ENUM}({self.name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_T_enum')

    def tmp_type_text(self, enable_count=False):
        return 'MPI_T_enum'

@Type.add_type('T_ENUM_OUT', abi_type=['ompi'])
class TypeTEnumOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_enum *'

@Type.add_type('T_ENUM_OUT', abi_type=['standard'])
class TypeTEnumOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.T_ENUM}((MPI_T_enum) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_T_enum')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_T_enum *) {self.name}'

@Type.add_type('CVAR_HANDLE', abi_type=['ompi'])
class TypeCvarHandle(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_cvar_handle'

@Type.add_type('CVAR_HANDLE', abi_type=['standard'])
class TypeCvarHandleStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_T_cvar_handle {self.tmpname} = {ConvertFuncs.CVAR_HANDLE}({self.name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_T_cvar_handle')

    def tmp_type_text(self, enable_count=False):
        return 'MPI_T_cvar_handle'

@Type.add_type('CVAR_HANDLE_OUT', abi_type=['ompi'])
class TypeCvarHandleOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_cvar_handle *'

@Type.add_type('CVAR_HANDLE_OUT', abi_type=['standard'])
class TypeCvarHandleOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.CVAR_HANDLE}((MPI_T_cvar_handle) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_T_cvar_handle')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_T_cvar_handle *) {self.name}'

@Type.add_type('CVAR_HANDLE_INOUT', abi_type=['ompi'])
class TypeCvarHandleInOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_cvar_handle *'

@Type.add_type('CVAR_HANDLE_INOUT', abi_type=['standard'])
class TypeCvarHandleInOutStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_T_cvar_handle {self.tmpname} = {ConvertFuncs.CVAR_HANDLE}(*{self.name});']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.CVAR_HANDLE}((MPI_T_cvar_handle) {self.tmpname});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_T_cvar_handle')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_T_cvar_handle *) &{self.tmpname}'

#
# this type is not actually used
#
@Type.add_type('BIND', abi_type=['ompi'])
class TypeBind(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('BIND_OUT', abi_type=['ompi'])
class TypeBindOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('BIND_OUT', abi_type=['standard'])
class TypeBindOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.T_BIND}(*{self.name});']

    def type_text(self, enable_count=False):
        return 'int *'

    @property
    def argument(self):
        return self.name

@Type.add_type('EVENT_REGISTRATION', abi_type=['ompi'])
class TypeEventRegistration(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_event_registration'

@Type.add_type('EVENT_REGISTRATION', abi_type=['standard'])
class TypeEventRegistrationStandard(StandardABIType):

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_T_event_registration')

    @property
    def argument(self):
        return f'(MPI_T_event_registration){self.name}'

@Type.add_type('EVENT_REGISTRATION_OUT', abi_type=['ompi'])
class TypeEventRegistrationOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_event_registration *'

@Type.add_type('EVENT_REGISTRATION_OUT', abi_type=['standard'])
class TypeEventRegistrationOutStandard(StandardABIType):

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_T_event_registration')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_T_event_registration *){self.name}'

@Type.add_type('PVAR_HANDLE', abi_type=['ompi'])
class TypePvarHandle(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_pvar_handle'

@Type.add_type('PVAR_HANDLE', abi_type=['standard'])
class TypePvarHandleStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_T_pvar_handle {self.tmpname} = {ConvertFuncs.PVAR_HANDLE}({self.name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_T_pvar_handle')

    def tmp_type_text(self, enable_count=False):
        return 'MPI_T_pvar_handle'

@Type.add_type('PVAR_HANDLE_OUT', abi_type=['ompi'])
class TypePvarHandleOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_pvar_handle *'

@Type.add_type('PVAR_HANDLE_OUT', abi_type=['standard'])
class TypePvarHandleOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.PVAR_HANDLE}((MPI_T_pvar_handle) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_T_pvar_handle')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_T_pvar_handle *) {self.name}'

@Type.add_type('PVAR_HANDLE_INOUT', abi_type=['ompi'])
class TypePvarHandleInout(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_pvar_handle *'

@Type.add_type('PVAR_HANDLE_INOUT', abi_type=['standard'])
class TypePvarHandleInoutStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_T_pvar_handle {self.tmpname} = {ConvertFuncs.PVAR_HANDLE}(*{self.name});']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.PVAR_HANDLE}((MPI_T_pvar_handle){self.tmpname});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_T_pvar_handle')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_T_pvar_handle *)&{self.tmpname}'

@Type.add_type('PVAR_SESSION', abi_type=['ompi'])
class TypePvarSession(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_pvar_session'

@Type.add_type('PVAR_SESSION', abi_type=['standard'])
class TypePvarSessionStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_T_pvar_session {self.tmpname} = {ConvertFuncs.PVAR_SESSION}({self.name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_T_pvar_session')

    def tmp_type_text(self, enable_count=False):
        return 'MPI_T_pvar_session'


@Type.add_type('PVAR_SESSION_OUT', abi_type=['ompi'])
class TypePvarSessionOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_pvar_session *'

@Type.add_type('PVAR_SESSION_OUT', abi_type=['standard'])
class TypePvarSessionOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.PVAR_SESSION}((MPI_T_pvar_session)*{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_T_pvar_session')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_T_pvar_session *){self.name}'

@Type.add_type('PVAR_SESSION_INOUT', abi_type=['ompi'])
class TypePvarSessionInOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_pvar_session *'


@Type.add_type('PVAR_SESSION_INOUT', abi_type=['standard'])
class TypePvarSessionInOutStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_T_pvar_session {self.tmpname} = {ConvertFuncs.PVAR_SESSION}(*{self.name});']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.PVAR_SESSION}((MPI_T_pvar_session){self.tmpname});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_T_pvar_session')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_T_pvar_session *)&{self.tmpname}'

@Type.add_type('T_VERBOSITY', abi_type=['ompi'])
class TypeTVerbosity(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('T_VERBOSITY', abi_type=['standard'])
class TypeTVerbosityStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.T_VERBOSITY}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

    @property
    def argument(self):
        return self.name

@Type.add_type('T_VERBOSITY_OUT', abi_type=['ompi'])
class TypeTVerbosityOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('T_VERBOSITY_OUT', abi_type=['standard'])
class TypeTVerbosityOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.T_VERBOSITY}(*{self.name});']

    def type_text(self, enable_count=False):
        return 'int *'

    @property
    def argument(self):
        return self.name

@Type.add_type('PVAR_CLASS', abi_type=['ompi'])
class TypePvarClass(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('PVAR_CLASS', abi_type=['standard'])
class TypePvarClassStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.PVAR_CLASS}({self.name});']
        
    def type_text(self, enable_count=False):
        return 'int'

    @property
    def argument(self):
        return self.name

@Type.add_type('PVAR_CLASS_OUT', abi_type=['ompi'])
class TypePvarClassOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('PVAR_CLASS_OUT', abi_type=['standard'])
class TypePvarClassOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.PVAR_CLASS}(*{self.name});']

    def type_text(self, enable_count=False):
        return f'int *'

    @property
    def argument(self):
        return self.name

@Type.add_type('CB_SAFETY', abi_type=['ompi'])
class TypeCbSafety(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_cb_safety'

@Type.add_type('CB_SAFETY', abi_type=['standard'])
class TypeCbSafetyStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_T_cb_safety {self.tmpname} = {ConvertFuncs.T_CB_SAFETY}({self.name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_T_cb_safety')

    def tmp_type_text(self, enable_count=False):
        return 'MPI_T_cb_safety'

@Type.add_type('SOURCE_ORDER', abi_type=['ompi'])
class TypeSourceOrder(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_source_order'

@Type.add_type('SOURCE_ORDER', abi_type=['standard'])
class TypeSourceOrderStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_T_source_order {self.tmpname} = {ConvertFuncs.T_SOURCE_ORDER}({self.name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_T_source_order')

    @property
    def argument(self):
        return self.name

@Type.add_type('SOURCE_ORDER_OUT', abi_type=['ompi'])
class TypeSourceOrderOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_source_order *'

@Type.add_type('SOURCE_ORDER_OUT', abi_type=['standard'])
class TypeSourceOrderOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.T_SOURCE_ORDER}((MPI_T_source_order) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_T_source_order')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_T_source_order *){self.name}'

@Type.add_type('EVENT_FREE_CB_FUNCTION', abi_type=['ompi'])
class TypeEventFreeCBFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_event_free_cb_function'

@Type.add_type('EVENT_FREE_CB_FUNCTION', abi_type=['standard'])
class TypeEventFreeCBFunctionStandard(StandardABIType):

    def type_text(self, enable_count=False):
        return 'MPI_T_event_free_cb_function'

    @property
    def argument(self):
        return self.name

@Type.add_type('EVENT_DROPPED_CB_FUNCTION', abi_type=['ompi'])
class TypeEventDroppedCBFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_event_dropped_cb_function'

@Type.add_type('EVENT_DROPPED_CB_FUNCTION', abi_type=['standard'])
class TypeEventDroppedCBFunctionStandard(StandardABIType):

    def type_text(self, enable_count=False):
        return 'MPI_T_event_dropped_cb_function'

    @property
    def argument(self):
        return self.name

@Type.add_type('EVENT_CB_FUNCTION', abi_type=['ompi'])
class TypeEventCBFunctionStandard(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_event_cb_function'

@Type.add_type('EVENT_CB_FUNCTION', abi_type=['standard'])
class TypeEventCBFunctionStandard(StandardABIType):

    def type_text(self, enable_count=False):
        return 'MPI_T_event_cb_function'

    @property
    def argument(self):
        return self.name

@Type.add_type('VOID')
class TypeVoid(Type):

    def type_text(self, enable_count=False):
        return 'void *'

@Type.add_type('VOID_CONST')
class TypeVoidConst(Type):

    def type_text(self, enable_count=False):
        return 'const void *'

@Type.add_type('ATTR_KEY', abi_type=['ompi'])
class TypeAttrKey(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('ATTR_KEY', abi_type=['standard'])
class TyperAttrKeyStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.ATTR_KEY}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('ATTR_KEY_OUT', abi_type=['ompi'])
class TypeAttrKeyOut(Type):
    
    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('ATTR_KEY_OUT', abi_type=['standard'])
class TypeAttrKeyOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.ATTR_KEY}(*{self.name});']

    def type_text(self, enable_count=False):
        return f'int *'

    @property
    def argument(self):
        return self.name

@Type.add_type('ATTR_KEY_INOUT', abi_type=['ompi'])
class TypeAttrKeyInOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('ATTR_KEY_INOUT', abi_type=['standard'])
class TypeAttrKeyInOutStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.ATTR_KEY}(*{self.name});']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.ATTR_KEY}({self.tmpname});']

    def type_text(self, enable_count=False):
        return f'int *'

    @property
    def argument(self):
        return f'&{self.tmpname}'

@Type.add_type('SPLIT_TYPE', abi_type=['ompi'])
class TypeSplitType(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('SPLIT_TYPE', abi_type=['standard'])
class TyperSplitTypeStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.SPLIT_TYPE}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('SUBARRAY_ORDER', abi_type=['ompi'])
class TypeSubarrayOrder(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('SUBARRAY_ORDER', abi_type=['standard'])
class TypeSubArrayOrderStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.SUBARRAY_ORDER}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('WEIGHTS', abi_type=['ompi'])
class TypeWeightType(Type):

    def type_text(self, enable_count=False):
        return 'const int *'
    
    def parameter(self, enable_count=False, **kwargs):
        return f'const int {self.name}[]'

#
# TODO this can be made better if we could handle "const int" 
# better as arg to the converter code.
#
@Type.add_type('WEIGHTS', abi_type=['standard'])
class TypeWeightStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'int *{self.tmpname} = (int *){ConvertFuncs.WEIGHTS}((int *){self.name});']

    def type_text(self, enable_count=False):
        return 'const int *'

    def parameter(self, enable_count=False, **kwargs):
        return f'const int * {self.name}'

    @property
    def argument(self):
        return f'(int *){self.tmpname}'

@Type.add_type('COMM_CMP_OUT', abi_type=['ompi'])
class TypeCommCmpOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('COMM_CMP_OUT', abi_type=['standard'])
class TypeCommCmpOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.COMM_CMP}(*{self.name});']

    def type_text(self, enable_count=False):
        return 'int *'

    @property
    def argument(self):
        return f'{self.name}'

@Type.add_type('EVENT_INSTANCE', abi_type=['ompi'])
class TypeEventInstance(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_event_instance'


@Type.add_type('EVENT_INSTANCE', abi_type=['standard'])
class TypeEventInstanceStandard(StandardABIType):

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_T_event_instance')

    @property
    def argument(self):
        return f'(MPI_T_event_instance){self.name}'

@Type.add_type('DISTRIB_ARRAY', abi_type=['ompi'])
class TypeDistributionArray(Type):

    def type_text(self, enable_count=False):
        return 'const int *'

    def parameter(self, enable_count=False, **kwargs):
        return f'const int {self.name}[]'

@Type.add_type('DISTRIB_ARRAY', abi_type=['standard'])
class TypeDistributionArrayStandard(StandardABIType):

    @property
    def init_code(self):
        code = [f'int size_{self.tmpname} = {self.count_param};']
        code.append(f'int *{self.tmpname} = NULL;')
        code.append('if('+f'{self.name}' + '!= NULL)' + '{')
        code.append(f'{self.tmpname} = (int *)ompi_abi_malloc(size_{self.tmpname}, sizeof(int));')
        code.append(f'if (NULL != {self.tmpname}){{')
        code.append(f'for(int i=0;i<size_{self.tmpname};i++){{')
        code.append(f'{self.tmpname}[i] = {ConvertFuncs.SUBARRAY_DISTRIB_TYPES}({self.name}[i]);')
        code.append('}')
        code.append('}')
        code.append('}')
        return code

    @property
    def final_code(self):
        code = [f'if({self.tmpname} != NULL){{']
        code.append(f'free({self.tmpname});')
        code.append('}')
        return code

    def type_text(self, enable_count=False):
        return 'const int *'

    def parameter(self, enable_count=False, **kwargs):
        return f'const int {self.name}[]'

@Type.add_type('DARGS_ARRAY', abi_type=['ompi'])
class TypeDargsArray(Type):

    def type_text(self, enable_count=False):
        return 'const int *'

    def parameter(self, enable_count=False, **kwargs):
        return f'const int {self.name}[]'

@Type.add_type('DARGS_ARRAY', abi_type=['standard'])
class TypeDargsArrayStandard(StandardABIType):

    @property
    def init_code(self):
        code = [f'int size_{self.tmpname} = {self.count_param};']
        code.append(f'int *{self.tmpname} = NULL;')
        code.append('if('+f'{self.name}' + '!= NULL)' + '{')
        code.append(f'{self.tmpname} = (int *)ompi_abi_malloc(size_{self.tmpname}, sizeof(int));')
        code.append(f'if (NULL != {self.tmpname}){{')
        code.append(f'for(int i=0;i<size_{self.tmpname};i++){{')
        code.append(f'{self.tmpname}[i] = {ConvertFuncs.SUBARRAY_DARGS_TYPES}({self.name}[i]);')
        code.append('}')
        code.append('}')
        code.append('}')
        return code

  
    @property
    def final_code(self):
        code = [f'if({self.tmpname} != NULL){{']
        code.append(f'free({self.tmpname});')
        code.append('}')
        return code

    def type_text(self, enable_count=False):
        return 'const int *'

    def parameter(self, enable_count=False, **kwargs):
        return f'const int {self.name}[]'

@Type.add_type('MODE_BITS', abi_type=['ompi'])
class TypeModeBits(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('MODE_BITS_OUT', abi_type=['ompi'])
class TypeModeBitsOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('MODE_BITS', abi_type=['standard'])
class TypeModeBitsStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.MODE_BITS}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('MODE_BITS_OUT', abi_type=['standard'])
class TypeModeBitsOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.MODE_BITS}(*{self.name});']

    def type_text(self, enable_count=False):
        return 'int *'

    @property
    def argument(self):
        return f'{self.name}'

@Type.add_type('RMA_MODE_BITS', abi_type=['ompi'])
class TypeRmaModeBits(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('RMA_MODE_BITS_OUT', abi_type=['ompi'])
class TypeRmaModeBitsOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('RMA_MODE_BITS', abi_type=['standard'])
class TypeRmaModeBitsStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.RMA_MODE_BITS}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('RMA_MODE_BITS_OUT', abi_type=['standard'])
class TypeRmaModeBitsOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.RMA_MODE_BITS}(*{self.name});']

    def type_text(self, enable_count=False):
        return 'int *'

    @property
    def argument(self):
        return f'{self.name}'

@Type.add_type('WHENCE', abi_type=['ompi'])
class TypeWhence(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('WHENCE', abi_type=['standard'])
class TypeWhenceStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.WHENCE}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('COMBINER_OUT', abi_type=['ompi'])
class TypeCombinerOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('COMBINER_OUT', abi_type=['standard'])
class TypeCombinerOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.COMBINER}(*{self.name});']

    def type_text(self, enable_count=False):
        return 'int *'

    @property
    def argument(self):
        return f'{self.name}'

@Type.add_type('WIN_LOCK', abi_type=['ompi'])
class TypeWinLock(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('WIN_LOCK', abi_type=['standard'])
class TypeWinLockStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.WIN_LOCK}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('TOPO_OUT', abi_type=['ompi'])
class TopoOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('TOPO_OUT', abi_type=['standard'])
class TopoOutStandard(StandardABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToStandard.TOPO}(*{self.name});']

    def type_text(self, enable_count=False):
        return 'int *'

    @property
    def argument(self):
        return f'{self.name}'

@Type.add_type('TYPECLASS', abi_type=['ompi'])
class TypeClass(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('TYPECLASS', abi_type=['standard'])
class TypeClassStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.TYPECLASS}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'



