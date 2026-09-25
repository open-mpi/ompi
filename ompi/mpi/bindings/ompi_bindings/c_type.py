# Copyright (c) 2024-2026 Triad National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
"""C type definitions."""
from abc import ABC, abstractmethod
from ompi_bindings.consts import ConvertFuncs, ConvertOMPIToForum, IGNORED_STATUS_HANDLES
from ompi_bindings import util 

class Type(ABC):
    """Type representation."""

    PARAMS_OMPI_ABI = {}

    PARAMS_FORUM_ABI = {}

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
        elif abi_type == 'forum':
            return Type.PARAMS_FORUM_ABI[type_name](type_name, **kwargs)
        else:
            raise RuntimeError(f'invalid ABI type {abi_type}')

    @staticmethod
    def add_type(type_name, abi_type=('ompi', 'forum')):
        """Add a new class corresponding to a type."""
        def wrapper(class_):
            if 'ompi' in abi_type:
                Type.PARAMS_OMPI_ABI[type_name] = class_
            if 'forum' in abi_type:
#               print("Adding type " + str(type_name) + " to PARAMS_FORUM_ABI")
                Type.PARAMS_FORUM_ABI[type_name] = class_
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

class ForumABIType(Type):

    @property
    def tmpname(self):
        return util.abi_tmp_name(self.name)

    @property
    def argument(self):
        return self.tmpname

@Type.add_type('ERROR_CLASS', abi_type=['ompi'])
class TypeErrorClass(Type):

    def type_text(self, enable_count=False):
        return 'int'

    def return_code(self, name):
        return [f'return {name};']

@Type.add_type('ERROR_CLASS', abi_type=['forum'])
class TypeErrorClassForum(ForumABIType):

    def type_text(self, enable_count=False):
        return 'int'

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.ERROR_CLASS}({self.name});']

    def return_code(self, name):
        return [f'return {ConvertOMPIToForum.ERROR_CLASS}({name});']


@Type.add_type('ERROR_CLASS_OUT', abi_type=['ompi'])
class TypeErrorClassOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('ERROR_CLASS_OUT', abi_type=['forum'])
class TypeErrorClassOutForum(ForumABIType):

    def type_text(self, enable_count=False):
        return 'int *'

    @property
    def final_code(self): 
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.ERROR_CLASS}(*{self.name});']

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

@Type.add_type('ERROR_CODE', abi_type=['forum'])
class TypeErrorCodeForum(ForumABIType):

    def type_text(self, enable_count=False):
        return 'int'

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.ERROR_CLASS}({self.name});']

    def return_code(self, name):
        return [f'return {ConvertOMPIToForum.ERROR_CLASS}({name});']


@Type.add_type('ERROR_CODE_OUT', abi_type=['ompi'])
class TypeErrorCodeOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('ERROR_CODE_OUT', abi_type=['forum'])
class TypeErrorCodeOutForum(ForumABIType):

    def type_text(self, enable_count=False):
        return 'int *'

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.ERROR_CLASS}(*{self.name});']

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


@Type.add_type('BUFFER_ADDR_OUT', abi_type=['forum'])
class TypeBufferAddrOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *(void **){self.name} = {ConvertOMPIToForum.BUFFER}(*(void **){self.name});']

    def type_text(self, enable_count=False):
        return f'void *'

    @property
    def argument(self):
        return f'{self.name}'


@Type.add_type('BUFFER', abi_type=['forum'])
class TypeBufferForum(ForumABIType):

    @property
    def init_code(self):
        return [f'void *{self.tmpname} = (int *){ConvertFuncs.BUFFER}((void *){self.name});']

    def type_text(self, enable_count=False):
        return 'void *'

@Type.add_type('BUFFER_CONST', abi_type=['forum'])
class TypeBufferConstForum(ForumABIType):

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

@Type.add_type('ERRCODE_ARRAY_OUT', abi_type=['ompi'])
class TypeErrcodeArrayOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

    def parameter(self, enable_count=False, **kwargs):
        return f'int {self.name}[]'

@Type.add_type('ERRCODE_ARRAY_OUT', abi_type=['forum'])
class TypeErrcodeArrayOutForum(ForumABIType):
    """Array of MPI error codes returned by the spawn calls.

    The intern layer fills the array with OMPI-internal error codes,
    whose numeric values differ from the MPI Forum ABI error classes,
    so each entry must be converted.  The element count comes from the
    annotation: "name:maxprocs" sizes the array from a scalar count
    parameter, while the three-part "name:count:array_of_maxprocs"
    form (comm_spawn_multiple) sums the entries of the named count
    array.  MPI_ERRCODES_IGNORE is ((int *) 0) in both the OMPI and
    the MPI Forum ABI, so a NULL check covers the sentinel.
    """

    @property
    def init_code(self):
        code = [f'MPI_Count size_{self.tmpname} = 0;']
        if self.outcount_param is None or self.outcount_param == self.count_param:
            code.append(f'size_{self.tmpname} = {self.count_param};')
        else:
            code.append(f'for (int i_{self.tmpname} = 0; i_{self.tmpname} < {self.count_param}; ++i_{self.tmpname})' + ' {')
            code.append(f'size_{self.tmpname} += {self.outcount_param}[i_{self.tmpname}];')
            code.append('}')
        code.append(f'int *{self.tmpname} = NULL;')
        code.append(f'if (NULL != {self.name})' + ' {')
        code.append(f'{self.tmpname} = (int *)ompi_abi_malloc(size_{self.tmpname}, sizeof(int));')
        code.append(f'if (NULL == {self.tmpname}) return ompi_convert_intern_error_abi_error(MPI_ERR_NO_MEM);')
        code.append('}')
        return code

    @property
    def final_code(self):
        code = [f'if (NULL != {self.tmpname})' + ' {']
        code.append(f'for (MPI_Count i = 0; i < size_{self.tmpname}; ++i)' + ' {')
        code.append(f'{self.name}[i] = {ConvertOMPIToForum.ERROR_CLASS}({self.tmpname}[i]);')
        code.append('}')
        code.append(f'free({self.tmpname});')
        code.append('}')
        return code

    def type_text(self, enable_count=False):
        return 'int *'

    def parameter(self, enable_count=False, **kwargs):
        return f'int {self.name}[]'

    @property
    def argument(self):
        return f'(NULL != {self.tmpname}) ? {self.tmpname} : MPI_ERRCODES_IGNORE'

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
class TypeDatatypeInOut(Type):

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
class TypeDatatypeArrayAsync(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Datatype'

    def parameter(self, enable_count=False, **kwargs):
        return f'const {self.type_text(enable_count=enable_count)} {self.name}[]'

@Type.add_type('DATATYPE', abi_type=['forum'])
class TypeDatatypeForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_Datatype {self.tmpname} = {ConvertFuncs.DATATYPE}({self.name});']

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Datatype'

    def return_code(self, name):
        return [f'return {ConvertOMPIToForum.DATATYPE}({name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Datatype')


@Type.add_type('DATATYPE_OUT', abi_type=['forum'])
class TypeDatatypeOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.DATATYPE}((MPI_Datatype) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Datatype')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Datatype *) {self.name}'

@Type.add_type('DATATYPE_INOUT', abi_type=['forum'])
class TypeDatatypeInoutForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_Datatype {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.DATATYPE}(*{self.name}) : MPI_DATATYPE_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.DATATYPE}((MPI_Datatype) {self.tmpname});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Datatype')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Datatype *) (NULL != {self.name} ? &{self.tmpname} : NULL)'

@Type.add_type('DATATYPE_ARRAY', abi_type=['forum'])
class TypeDatatypeArrayForum(ForumABIType):

    @property
    def init_code(self):
        # MPI_Count (not int) size: in the bigcount (_c) variants the
        # count parameter is MPI_Count and an int temporary would
        # silently truncate values above INT_MAX.
        if self.count_param is None:
            code = [f'MPI_Comm comm_{self.tmpname} = {ConvertFuncs.COMM}(comm);']
            code.append(f'MPI_Count size_{self.tmpname} = OMPI_COMM_IS_INTER(comm_{self.tmpname})?ompi_comm_remote_size(comm_{self.tmpname}):ompi_comm_size(comm_{self.tmpname});')
        else:
            code = [f'MPI_Count size_{self.tmpname} = {self.count_param};']
        code.append(f'MPI_Datatype *{self.tmpname} = NULL;')
        code.append('if('+f'{self.name}' + '!= NULL)' + '{')
        code.append(f'{self.tmpname} = (MPI_Datatype *)ompi_abi_malloc(size_{self.tmpname}, sizeof(MPI_Datatype));')
        code.append(f'if (NULL == {self.tmpname}) return ompi_convert_intern_error_abi_error(MPI_ERR_NO_MEM);')
        code.append(f'for(MPI_Count i=0;i<size_{self.tmpname};i++)' + '{')
        code.append(f'{self.tmpname}[i] = {ConvertFuncs.DATATYPE}({self.name}[i]);')
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

@Type.add_type('DATATYPE_ARRAY_ASYNC', abi_type=['forum'])
class TypeDatatypeArrayAsyncForum(TypeDatatypeArrayForum):

    @property
    def need_async_cleanup(self):
        return True

    @property
    def final_code(self):
        request_tmp_name = util.abi_tmp_name('request')
        code = []
        code.append(f'if((MPI_SUCCESS == ret_value) && (MPI_REQUEST_NULL != {request_tmp_name}) && (!REQUEST_COMPLETE({request_tmp_name})))' + '{')
        code.append(f'if (NULL != {self.tmpname})' + '{')
        code.append(f'ompi_coll_base_append_array_to_release({request_tmp_name}, (void *){self.tmpname});')
        code.append(f'ompi_coll_base_add_release_arrays_cb({request_tmp_name});')
        code.append('}')
        code.append('} else {')
        code.append(f'if (NULL != {self.tmpname}) free({self.tmpname});')
        code.append('}')
        return code

@Type.add_type('DATATYPE_ARRAY_OUT', abi_type=['forum'])
class TypeDatatypeArrayOutForum(ForumABIType):

    @property
    def init_code(self):
        code = [f'MPI_Count size_{self.tmpname} = {self.count_param};']
        code.append(f'MPI_Datatype *{self.tmpname} = (MPI_Datatype *)ompi_abi_malloc(size_{self.tmpname},sizeof(MPI_Datatype));')
        code.append(f'if (NULL == {self.tmpname}) return ompi_convert_intern_error_abi_error(MPI_ERR_NO_MEM);')
        return code

    @property
    def final_code(self):
        code = [f'for(MPI_Count i=0;i<size_{self.tmpname};i++)' + '{']
        code.append(f'{self.name}[i] = {ConvertOMPIToForum.DATATYPE}({self.tmpname}[i]);')
        code.append('}')
        code.append(f'free({self.tmpname});')
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

@Type.add_type('T_EVENT_DATATYPE_ARRAY_OUT', abi_type=['ompi'])
class TypeTEventDatatypeArrayOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Datatype *'

@Type.add_type('T_EVENT_DATATYPE_ARRAY_OUT', abi_type=['forum'])
class TypeTEventDatatypeArrayOutForum(ForumABIType):
    """Datatype array filled by MPI_T_event_get_info (MPI Forum ABI).

    The caller passes the array capacity in *count_param on input; the
    intern layer fills at most that many entries and then overwrites
    *count_param with the total element count.  Save the capacity
    before the call and convert, in place, every entry that was
    actually written.
    """

    @property
    def init_code(self):
        return [f'int capacity_{self.name} = (NULL != {self.count_param} && NULL != {self.name}) ? *{self.count_param} : 0;']

    @property
    def final_code(self):
        code = [f'if (NULL != {self.name} && NULL != {self.count_param})' + ' {']
        code.append(f'    int n_{self.name} = (*{self.count_param} < capacity_{self.name}) ? *{self.count_param} : capacity_{self.name};')
        code.append(f'    for (int i = 0; i < n_{self.name}; ++i)' + ' {')
        code.append(f'        {self.name}[i] = {ConvertOMPIToForum.DATATYPE}((MPI_Datatype) {self.name}[i]);')
        code.append('    }')
        code.append('}')
        return code

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Datatype')

    def parameter(self, enable_count=False, **kwargs):
        return f'{self.type_text(enable_count=enable_count)} {self.name}[]'

    @property
    def argument(self):
        return f'(MPI_Datatype *) {self.name}'

@Type.add_type('NEIGHBOR_DATATYPE_ARRAY', abi_type=['forum'])
class TypeNeighborDatatypeArrayForum(TypeDatatypeArrayForum):

    @property
    def init_code(self):
        if self.name not in ("sendtypes", "recvtypes"):
            # An unknown parameter name would otherwise leave the size
            # temporary unassigned in the generated C: fail generation
            # instead of silently emitting broken code.
            raise util.BindingError(
                f'NEIGHBOR_DATATYPE_ARRAY expects a parameter named '
                f'"sendtypes" or "recvtypes", got "{self.name}"')
        code = [f'MPI_Comm comm_{self.tmpname} = {ConvertFuncs.COMM}(comm);']
        code.append(f'int indegree_{self.tmpname} = 0, outdegree_{self.tmpname} = 0, size_{self.tmpname} = 0;')
        # Only query the neighbor counts when the communicator actually
        # has a topology: this prologue runs before the intern
        # binding's MPI_PARAM_CHECK, and parameter checking must not
        # crash on a topology-less communicator.  With size 0 nothing
        # is converted, NULL is passed through, and the intern binding
        # raises MPI_ERR_TOPOLOGY through the error handler as usual.
        code.append(f'if (MPI_COMM_NULL != comm_{self.tmpname} && OMPI_COMM_IS_TOPO(comm_{self.tmpname}))' + ' {')
        code.append(f'mca_topo_base_neighbor_count(comm_{self.tmpname}, &indegree_{self.tmpname}, &outdegree_{self.tmpname});')
        code.append('}')
        code.append(f'MPI_Datatype *{self.tmpname} = NULL;')
        if self.name == "sendtypes":
            code.append(f'size_{self.tmpname} = outdegree_{self.tmpname};')
        if self.name == "recvtypes":
            code.append(f'size_{self.tmpname} = indegree_{self.tmpname};')
        code.append('if('+f'{self.name}' + '!= NULL && size_' + f'{self.tmpname}' + ' > 0)' + '{')
        code.append(f'{self.tmpname} = (MPI_Datatype *)ompi_abi_malloc(size_{self.tmpname}, sizeof(MPI_Datatype));')
        code.append(f'if (NULL == {self.tmpname}) return ompi_convert_intern_error_abi_error(MPI_ERR_NO_MEM);')
        code.append(f'for(int i=0;i<size_{self.tmpname};i++){{')
        code.append(f'{self.tmpname}[i] = {ConvertFuncs.DATATYPE}({self.name}[i]);')
        code.append('}')
        code.append('}')
        return code

@Type.add_type('NEIGHBOR_DATATYPE_ARRAY_ASYNC', abi_type=['forum'])
class TypeNeighborDatatypeArrayAsyncForum(TypeNeighborDatatypeArrayForum):

    @property
    def need_async_cleanup(self):
        return True

    @property
    def final_code(self):
        request_tmp_name = util.abi_tmp_name('request')
        code = []
        code.append(f'if((MPI_SUCCESS == ret_value) && (MPI_REQUEST_NULL != {request_tmp_name}) && (!REQUEST_COMPLETE({request_tmp_name})))' + '{')
        code.append(f'if (NULL != {self.tmpname})' + '{')
        code.append(f'ompi_coll_base_append_array_to_release({request_tmp_name}, (void *){self.tmpname});')
        code.append(f'ompi_coll_base_add_release_arrays_cb({request_tmp_name});')
        code.append('}')
        code.append('} else {')
        code.append(f'if (NULL != {self.tmpname}) free({self.tmpname});')
        code.append('}')
        return code

@Type.add_type('OP', abi_type=['ompi'])
class TypeOp(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Op'


@Type.add_type('OP', abi_type=['forum'])
class TypeOpForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_Op {self.tmpname} = {ConvertFuncs.OP}({self.name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Op')

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Op'

    def return_code(self, name):
        return [f'return {ConvertOMPIToForum.OP}({name});']

@Type.add_type('OP_OUT', abi_type=['ompi'])
class TypeOpOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Op *'

@Type.add_type('OP_OUT', abi_type=['forum'])
class TypeOpOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.OP}((MPI_Op) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Op')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Op *) {self.name}'

@Type.add_type('OP_INOUT', abi_type=['ompi'])
class TypeOpInOut(TypeOpOut):
    pass

@Type.add_type('OP_INOUT', abi_type=['forum'])
class TypeOpInOutForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_Op {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.OP}(*{self.name}) : MPI_OP_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.OP}((MPI_Op) {self.tmpname});']
        
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

@Type.add_type('TAG', abi_type=['forum'])
class TypeTagForum(ForumABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.TAG}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('TAG_OUT', abi_type=['ompi'])
class TypeTagOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('TAG_OUT', abi_type=['forum'])
class TypeTagOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.TAG}(*{self.name});']

    def type_text(self, enable_count=False):
        return f'int *'

    @property
    def argument(self):
        return f'(int *) {self.name}'

@Type.add_type('ROOT', abi_type=['ompi'])
class TypeRoot(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('ROOT', abi_type=['forum'])
class TypeRootForum(ForumABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.ROOT}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('SOURCE', abi_type=['ompi'])
class TypeSource(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('SOURCE', abi_type=['forum'])
class TypeSourceForum(ForumABIType):

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

@Type.add_type('SOURCE_ARRAY', abi_type=['forum'])
class TypeSourceArrayForum(ForumABIType):

    @property
    def init_code(self):
        code = [(f'int *{self.tmpname} = NULL;')]
        code.append('if('+f'{self.name}' + '!= NULL)' + '{')
        code.append(f'{self.tmpname} = (int *)ompi_abi_malloc({self.count_param}, sizeof(int));')
        code.append(f'if (NULL == {self.tmpname}) return ompi_convert_intern_error_abi_error(MPI_ERR_NO_MEM);')
        code.append(f'for(int i=0;i<{self.count_param};i++){{')
        code.append(f'{self.tmpname}[i] = {ConvertFuncs.SOURCE}({self.name}[i]);')
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

@Type.add_type('SOURCE_OUT', abi_type=['forum'])
class TypeSourceOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.SOURCE}(*{self.name});']
 
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

@Type.add_type('SOURCE_ARRAY_OUT', abi_type=['forum'])
class TypeSourceArrayOutForum(ForumABIType):
        
    @property
    def init_code(self):
        code = [f'int *{self.tmpname} = (int*)ompi_abi_malloc({self.count_param}, sizeof(int));']
        code.append(f'if (NULL == {self.tmpname}) return ompi_convert_intern_error_abi_error(MPI_ERR_NO_MEM);')
        return code

    @property
    def final_code(self):
        code = [f'if (NULL != {self.name}){{']
        code.append(f'for(int i=0;i<{self.count_param};i++){{')
        code.append(f'{self.name}[i] = {ConvertOMPIToForum.SOURCE}({self.tmpname}[i]);')
        code.append('}')
        code.append('}')
        code.append(f'free({self.tmpname});')
        return code
        
    def type_text(self, enable_count=False):
        return 'int *'

    def parameter(self, enable_count=False, **kwargs):
        return f'int {self.name}[]'

@Type.add_type('COMM', abi_type=['ompi'])
class TypeCommunicator(Type):
 
     def type_text(self, enable_count=False):
         return 'MPI_Comm'

@Type.add_type('COMM', abi_type=['forum'])
class TypeCommunicatorForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_Comm {self.tmpname} = {ConvertFuncs.COMM}({self.name});']

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Comm'

    def return_code(self, name):
        return [f'return {ConvertOMPIToForum.COMM}({name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Comm')


@Type.add_type('COMM_OUT', abi_type=['ompi'])
class TypeCommunicatorOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Comm *'


@Type.add_type('COMM_OUT', abi_type=['forum'])
class TypeCommunicatorOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.COMM}((MPI_Comm) *{self.name});']
 
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


@Type.add_type('COMM_INOUT', abi_type=['forum'])
class TypeCommInOutForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_Comm {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.COMM}(*{self.name}) : MPI_COMM_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.COMM}({self.tmpname});']

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


@Type.add_type('WIN', abi_type=['forum'])
class TypeWinForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_Win {self.tmpname} = {ConvertFuncs.WIN}({self.name});']

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Win'

    def return_code(self, name):
        return [f'return {ConvertOMPIToForum.WIN}({name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Win')

@Type.add_type('WIN_OUT', abi_type=['ompi'])
class TypeWinOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Win *'


@Type.add_type('WIN_OUT', abi_type=['forum'])
class TypeWinOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.WIN}((MPI_Win) *{self.name});']

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


@Type.add_type('WIN_INOUT', abi_type=['forum'])
class TypeWinInOutForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_Win {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.WIN}(*{self.name}) : MPI_WIN_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.WIN}({self.tmpname});']

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


@Type.add_type('REQUEST', abi_type=['forum'])
class TypeRequestForum(ForumABIType):

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

@Type.add_type('REQUEST_CONST', abi_type=['forum'])
class TypeConstRequestForum(TypeRequestForum):

    @property
    def init_code(self):
        if self.count_param is None:
            code = [f'MPI_Request {self.tmpname} = {ConvertFuncs.REQUEST}(*{self.name});']
        else:
            code = [f'MPI_Count size_{self.tmpname} = {self.count_param};']
            code.append(f'MPI_Request *{self.tmpname} = (MPI_Request *)ompi_abi_malloc(size_{self.tmpname}, sizeof(MPI_Request));')
            code.append(f'if (NULL == {self.tmpname}) return ompi_convert_intern_error_abi_error(MPI_ERR_NO_MEM);')
            code.append(f'for(MPI_Count i=0;i<size_{self.tmpname};i++){{')
            code.append(f'{self.tmpname}[i] = {ConvertFuncs.REQUEST}({self.name}[i]);')
            code.append('}')
        return code

    @property
    def final_code(self):
        if self.count_param is None:
            code = []
        else:
            code = [f'if(NULL != {self.tmpname}) free({self.tmpname});']
        return code

    def type_text(self, enable_count=False):
        name = self.mangle_name('MPI_Request')
        return f'const {name} *'

    @property
    def argument(self):
        if self.count_param is None:
            return f'{self.tmpname}'
        else:
            return f'(MPI_Request *) {self.tmpname}'

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Request'

    def return_code(self, name):
        return [f'return {ConvertOMPIToForum.REQUEST}({name});']

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

@Type.add_type('REQUEST_INOUT', abi_type=['forum'])
class TypeRequestInOutForum(ForumABIType):

    @property
    def init_code(self):
        if self.count_param is None:
            code = [f'MPI_Request {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.REQUEST}(*{self.name}) : MPI_REQUEST_NULL;']
        else:
            code = [f'MPI_Count size_{self.tmpname} = {self.count_param};']
            code.append(f'MPI_Request *{self.tmpname} = (MPI_Request *)ompi_abi_malloc(size_{self.tmpname}, sizeof(MPI_Request));')
            code.append(f'if (NULL == {self.tmpname}) return ompi_convert_intern_error_abi_error(MPI_ERR_NO_MEM);')
            code.append(f'for(MPI_Count i=0;i<size_{self.tmpname};i++){{')
            code.append(f'{self.tmpname}[i] = {ConvertFuncs.REQUEST}({self.name}[i]);')
            code.append('}')
        return code

    @property
    def final_code(self):
        if self.count_param is None:
            code = [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.REQUEST}({self.tmpname});']
        else:
            code = [f'if (NULL != {self.name})' + '{']
            code.append(f'if (NULL != {self.tmpname})' + '{')
            code.append('for (int i = 0; i < %s; ++i) {' % (self.count_param,))
            code.append(f'{self.name}[i] = {ConvertOMPIToForum.REQUEST}({self.tmpname}[i]);')
            code.append('}')
            code.append('}')
            code.append('}')
            code.append(f'if (NULL != {self.tmpname}) free({self.tmpname});')
        return code

    @property
    def argument(self):
        if self.count_param is None:
            code = f'(MPI_Request *) (NULL != {self.name} ? &{self.tmpname} : NULL)'
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

@Type.add_type('STATUS', abi_type=['forum'])
class TypeStatusForum(ForumABIType):

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
        return f'(NULL != {self.name} ? &{self.tmpname} : NULL)'


@Type.add_type('STATUS_OUT', abi_type=['ompi'])
class TypeStatusOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Status *'

    def parameter(self, enable_count=False, **kwargs):
        if self.count_param is None:
            return f'MPI_Status *{self.name}'
        else:
            return f'MPI_Status {self.name}[]'


@Type.add_type('STATUS_OUT', abi_type=['forum'])
class TypeStatusOutForum(ForumABIType):

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
            code.append(f'if (NULL == {self.tmpname}) return ompi_convert_intern_error_abi_error(MPI_ERR_NO_MEM);')
            code.extend([
                'for (MPI_Count i = 0; i < %s; ++i) {' % (self.count_param,),
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
            code.append(f'{ConvertOMPIToForum.STATUS}({self.name}, &{self.tmpname});')
        else:
            code.append(f'if (NULL != {self.tmpname}) ' + '{')
            code.extend([
                'for (int i = 0; i < %s; ++i) {' % (self.outcount_param,),
                f'{ConvertOMPIToForum.STATUS}(&{self.name}[i], &{self.tmpname}[i]);',
                '}',
            ])
            code.append('}')
            code.append(f'free({self.tmpname});')
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
@Type.add_type('STATUS_INOUT', abi_type=['forum'])
class TypeStatusInOutForum(ForumABIType):

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
        code.append(f'{ConvertOMPIToForum.STATUS}({self.name}, &{self.tmpname});')
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


@Type.add_type('INFO', abi_type=['forum'])
class TypeInfoForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_Info {self.tmpname} = {ConvertFuncs.INFO}({self.name});']

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Info'
        
    def return_code(self, name):
        return [f'return {ConvertOMPIToForum.INFO}({name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Info')


@Type.add_type('INFO_OUT', abi_type=['ompi'])
class TypeInfoOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Info *'

@Type.add_type('INFO_OUT', abi_type=['forum'])
class TypeInfoOutForum(ForumABIType):

    @property
    def argument(self):
        return f'(MPI_Info *) {self.name}'

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Info')
        return f'{type_name} *'

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.INFO}((MPI_Info) *{self.name});']

@Type.add_type('INFO_INOUT', abi_type=['ompi'])
class TypeInfoInOut(TypeInfoOut):
    pass

@Type.add_type('INFO_INOUT', abi_type=['forum'])
class TypeInfoInOutForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_Info {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.INFO}(*{self.name}) : MPI_INFO_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.INFO}((MPI_Info) {self.tmpname});']

    @property
    def argument(self):
        return f'(MPI_Info *) (NULL != {self.name} ? &{self.tmpname} : NULL)'

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Info')
        return f'{type_name} *'
        
@Type.add_type('INFO_ARRAY', abi_type=['ompi'])
class TypeInfoArray(Type):

    def type_text(self, enable_count=False):
        return 'const MPI_Info *'

    def parameter(self, enable_count=False, **kwargs):
        return f'const MPI_Info {self.name}[]'


@Type.add_type('INFO_ARRAY', abi_type=['forum'])
class TypeInfoArrayForum(ForumABIType):

#
# TODO may need a better way to generalize for case of non-explicit count_param
#
    @property
    def init_code(self):
        # Mirror the DATATYPE_ARRAY pattern: leave the temporary NULL
        # when the input array is NULL (previously a NULL input was
        # passed to the intern layer as an array of uninitialized
        # handles), and fail cleanly when the allocation fails.
        code = [f'MPI_Count size_{self.tmpname} = {self.count_param};']
        code.append(f'MPI_Info *{self.tmpname} = NULL;')
        code.append('if('+f'{self.name}' + '!= NULL)' + '{')
        code.append(f'{self.tmpname} = (MPI_Info *)ompi_abi_malloc(size_{self.tmpname}, sizeof(MPI_Info));')
        code.append(f'if (NULL == {self.tmpname}) return ompi_convert_intern_error_abi_error(MPI_ERR_NO_MEM);')
        code.append(f'for(MPI_Count i=0;i<size_{self.tmpname};i++){{')
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


@Type.add_type('FILE', abi_type=['forum'])
class TypeFileForum(ForumABIType):

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
        return [f'return {ConvertOMPIToForum.FILE}({name});']

@Type.add_type('FILE_OUT', abi_type=['ompi'])
class TypeFileOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_File *'


@Type.add_type('FILE_OUT', abi_type=['forum'])
class TypeFileOutForum(ForumABIType):

    @property
    def argument(self):
        return f'(MPI_File *) {self.name}'

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.FILE}((MPI_File) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_File')
        return f'{type_name} *'

@Type.add_type('FILE_INOUT', abi_type=['ompi'])
class TypeFileInOut(TypeFileOut):

    def type_text(self, enable_count=False):
        return 'MPI_File *'

@Type.add_type('FILE_INOUT', abi_type=['forum'])
class TypeFileInOutForum(TypeFileOutForum):

    @property
    def init_code(self):
        return [f'MPI_File {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.FILE}(*{self.name}) : MPI_FILE_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.FILE}({self.tmpname});']

    @property
    def argument(self):
        return f'(MPI_File *) (NULL != {self.name} ? &{self.tmpname} : NULL)'

@Type.add_type('MESSAGE', abi_type=['ompi'])
class TypeMessage(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Message'


@Type.add_type('MESSAGE', abi_type=['forum'])
class TypeMessageForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_Message {self.tmpname} = {ConvertFuncs.MESSAGE}({self.name});']

#   @property
#   def argument(self):
#       return f'(MPI_Message) {self.name}'

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Message')

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Message'

#   def return_code(self, name):
#       return [f'return {ConvertOMPIToForum.MESSAGE}({name});']
        
@Type.add_type('MESSAGE_OUT', abi_type=['ompi'])
class TypeMessageOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Message *'


@Type.add_type('MESSAGE_OUT', abi_type=['forum'])
class TypeMessageOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.MESSAGE}((MPI_Message) *{self.name});']

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


@Type.add_type('MESSAGE_INOUT', abi_type=['forum'])
class TypeMessageInOutForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_Message {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.MESSAGE}(*{self.name}) : MPI_MESSAGE_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.MESSAGE}({self.tmpname});']

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


@Type.add_type('TS_LEVEL', abi_type=['forum'])
class TypeTSLevelForum(ForumABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.TS_LEVEL}({self.name});']

    def tmp_type_text(self, enable_count=False):
        return 'int'

    def return_code(self, name):
        return [f'return {ConvertOMPIToForum.TS_LEVEL}({name});']

    def type_text(self, enable_count=False):
        return 'int'


@Type.add_type('TS_LEVEL_OUT', abi_type=['ompi'])
class TypeTSLevelOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'


@Type.add_type('TS_LEVEL_OUT', abi_type=['forum'])
class TypeTSLevelOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.TS_LEVEL}((int) *{self.name});']

    def type_text(self, enable_count=False):
        return f'int *'

    @property
    def argument(self):
        return f'{self.name}'

@Type.add_type('COMM_ERRHANDLER_FUNCTION', abi_type=['ompi'])
class TypeCommErrhandlerFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Comm_errhandler_function *'


@Type.add_type('COMM_ERRHANDLER_FUNCTION', abi_type=['forum'])
class TypeCommErrhandlerFunctionForum(ForumABIType):

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


@Type.add_type('FILE_ERRHANDLER_FUNCTION', abi_type=['forum'])
class TypeFileErrhandlerFunctionForum(ForumABIType):

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


@Type.add_type('COPY_FUNCTION', abi_type=['forum'])
class TypeCopyFunctionForum(ForumABIType):

    def type_text(self, enable_count=False):
        return 'MPI_Copy_function *'

@Type.add_type('DELETE_FUNCTION', abi_type=['ompi'])
class TypeDeleteFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Delete_function *'


@Type.add_type('DELETE_FUNCTION', abi_type=['forum'])
class TypeDeleteFunctionForum(ForumABIType):

    def type_text(self, enable_count=False):
        return 'MPI_Delete_function *'


@Type.add_type('USER_FUNCTION', abi_type=['ompi'])
class TypeUserFunction(Type):

    @property
    def is_count(self):
        return True

    def type_text(self, enable_count=False):
        return 'MPI_User_function_c *' if enable_count else 'MPI_User_function *'


@Type.add_type('USER_FUNCTION', abi_type=['forum'])
class TypeUserFunctionForum(Type):

    def type_text(self, enable_count=False):
        return 'MPI_User_function_c *' if enable_count else 'MPI_User_function *'

@Type.add_type('COMM_COPY_ATTR_FUNCTION', abi_type=['ompi'])
class TypeCommCopyAttrFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Comm_copy_attr_function *'


@Type.add_type('COMM_COPY_ATTR_FUNCTION', abi_type=['forum'])
class TypeCommCopyAttrFunctionForum(ForumABIType):

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
        # The helper is handed to ompi_attr_create_keyval() as its
        # bindings_extra_state argument, so the attribute engine owns
        # it from the call onward: the keyval destructor free()s it
        # when the keyval is destroyed.  Do NOT free it here on
        # failure -- the engine's hash-insertion failure path destroys
        # the keyval (and the helper) itself; only the never-created
        # OOM corners leak it, matching the engine's semantics for the
        # Fortran bindings.
        code.append('helper = ( ompi_abi_wrapper_helper_t *)ompi_abi_malloc(1, sizeof(ompi_abi_wrapper_helper_t));')
        code.append('if (NULL == helper) return ompi_convert_intern_error_abi_error(MPI_ERR_NO_MEM);')
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
        code.append('    MPI_Comm_ABI_INTERNAL comm_tmp = ompi_convert_comm_ompi_to_forum(oldcomm);')
        code.append('    int comm_keyval_tmp = ompi_convert_attr_key_ompi_to_forum(comm_keyval);')
        code.append('    return helper->user_copy_fn((MPI_Comm_ABI_INTERNAL)comm_tmp, comm_keyval_tmp, helper->user_extra_state, attribute_val_in, attribute_val_out, flag);')
        code.append('}')
        code.append('static int ompi_abi_delete_attr_fn(MPI_Comm oldcomm, int comm_keyval, void *attribute_val, void *extra_state)')
        code.append('{')
        code.append('    ompi_abi_wrapper_helper_t *helper = (ompi_abi_wrapper_helper_t *)extra_state;')
        code.append('    MPI_Comm_ABI_INTERNAL comm_tmp = ompi_convert_comm_ompi_to_forum(oldcomm);')
        code.append('    int comm_keyval_tmp = ompi_convert_attr_key_ompi_to_forum(comm_keyval);')
        code.append('    return helper->user_delete_fn((MPI_Comm_ABI_INTERNAL)comm_tmp, comm_keyval_tmp, attribute_val, helper->user_extra_state);')
        code.append('}')
        return code

@Type.add_type('COMM_DELETE_ATTR_FUNCTION', abi_type=['ompi'])
class TypeCommDeleteAttrFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Comm_delete_attr_function *'


@Type.add_type('COMM_DELETE_ATTR_FUNCTION', abi_type=['forum'])
class TypeCommDeleteAttrFunctionForum(ForumABIType):

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


@Type.add_type('GREQUEST_QUERY_FUNCTION', abi_type=['forum'])
class TypeGrequestQueryFunctionForum(Type):
    """Generalized request query callback (MPI Forum ABI).

    The intern layer invokes the query callback with an intern-layout
    MPI_Status pointer, but an ABI application's callback reads and
    writes the ABI status layout, so the callback must be wrapped.  The
    free and cancel callbacks take only void*/int arguments, but they
    share extra_state with the query callback, so all three are routed
    through a heap-allocated helper.  The helper is freed in the free
    wrapper, which the intern layer invokes exactly once when the
    generalized request is freed.
    """

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Grequest_query_function')
        return f'{type_name} *'

    @property
    def argument(self):
        return 'ompi_abi_grequest_query_fn'

    @property
    def init_code(self):
        return [
            'ompi_abi_grequest_helper_t *helper;',
            'helper = (ompi_abi_grequest_helper_t *) ompi_abi_malloc(1, sizeof(ompi_abi_grequest_helper_t));',
            'if (NULL == helper) return ompi_convert_intern_error_abi_error(MPI_ERR_NO_MEM);',
            f'helper->user_query_fn = {self.name};',
            'helper->user_extra_state = extra_state;',
            'extra_state = helper;',
        ]

    @property
    def final_code(self):
        # On failure no generalized request was created, so the free
        # callback will never run; release the helper here.
        return ['if (MPI_SUCCESS != ret_value) { free(helper); }']

    @property
    def callback_wrapper_code(self):
        code = ['typedef struct {']
        code.append('    MPI_Grequest_query_function_ABI_INTERNAL *user_query_fn;')
        code.append('    MPI_Grequest_free_function_ABI_INTERNAL *user_free_fn;')
        code.append('    MPI_Grequest_cancel_function_ABI_INTERNAL *user_cancel_fn;')
        code.append('    void *user_extra_state;')
        code.append('} ompi_abi_grequest_helper_t;')
        code.append('static int ompi_abi_grequest_query_fn(void *extra_state, MPI_Status *status)')
        code.append('{')
        code.append('    ompi_abi_grequest_helper_t *helper = (ompi_abi_grequest_helper_t *) extra_state;')
        code.append('    MPI_Status_ABI_INTERNAL status_tmp;')
        code.append('    int rc;')
        code.append('    ompi_convert_intern_status_abi_status(&status_tmp, status);')
        code.append('    rc = helper->user_query_fn(helper->user_extra_state, &status_tmp);')
        code.append('    ompi_convert_abi_status_intern_status(status, &status_tmp);')
        code.append('    return ompi_convert_abi_error_intern_error(rc);')
        code.append('}')
        code.append('static int ompi_abi_grequest_free_fn(void *extra_state)')
        code.append('{')
        code.append('    ompi_abi_grequest_helper_t *helper = (ompi_abi_grequest_helper_t *) extra_state;')
        code.append('    int rc = helper->user_free_fn(helper->user_extra_state);')
        code.append('    free(helper);')
        code.append('    return ompi_convert_abi_error_intern_error(rc);')
        code.append('}')
        code.append('static int ompi_abi_grequest_cancel_fn(void *extra_state, int complete)')
        code.append('{')
        code.append('    ompi_abi_grequest_helper_t *helper = (ompi_abi_grequest_helper_t *) extra_state;')
        code.append('    return ompi_convert_abi_error_intern_error(helper->user_cancel_fn(helper->user_extra_state, complete));')
        code.append('}')
        return code

@Type.add_type('GREQUEST_FREE_FUNCTION', abi_type=['ompi'])
class TypeGrequestFreeFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Grequest_free_function *'


@Type.add_type('GREQUEST_FREE_FUNCTION', abi_type=['forum'])
class TypeGrequestFreeFunctionForum(Type):

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Grequest_free_function')
        return f'{type_name} *'

    @property
    def argument(self):
        return 'ompi_abi_grequest_free_fn'

    @property
    def init_code(self):
        # The helper is allocated by the GREQUEST_QUERY_FUNCTION param,
        # which precedes this one in the prototype.
        return [f'helper->user_free_fn = {self.name};']

@Type.add_type('GREQUEST_CANCEL_FUNCTION', abi_type=['ompi'])
class TypeGrequestCancelFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Grequest_cancel_function *'


@Type.add_type('GREQUEST_CANCEL_FUNCTION', abi_type=['forum'])
class TypeGrequestCancelFunctionForum(Type):

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Grequest_cancel_function')
        return f'{type_name} *'

    @property
    def argument(self):
        return 'ompi_abi_grequest_cancel_fn'

    @property
    def init_code(self):
        # The helper is allocated by the GREQUEST_QUERY_FUNCTION param,
        # which precedes this one in the prototype.
        return [f'helper->user_cancel_fn = {self.name};']

@Type.add_type('DATAREP_CONVERSION_FUNCTION', abi_type=['ompi'])
class TypeDatarepConversionFunction(Type):

    @property
    def is_count(self):
        return True

    def type_text(self, enable_count=False):
        return 'MPI_Datarep_conversion_function_c *' if enable_count else 'MPI_Datarep_conversion_function *'

@Type.add_type('DATAREP_CONVERSION_FUNCTION', abi_type=['forum'])
class TypeDatarepConversionFunctionForum(Type):
    """Datarep conversion callback (MPI Forum ABI).

    Passed through unwrapped: every io component's register_datarep
    entry point rejects user-defined data representations (e.g.,
    ompio's returns OMPI_ERROR unconditionally), so these callbacks
    can never be invoked.  If an io component ever implements
    register_datarep, this callback (and DATAREP_EXTENT_FUNCTION) must
    be wrapped like GREQUEST_QUERY_FUNCTION above, converting the
    MPI_Datatype argument and the returned error code.
    """

    @property
    def is_count(self):
        return True

    def type_text(self, enable_count=False):
        return 'MPI_Datarep_conversion_function_c *' if enable_count else 'MPI_Datarep_conversion_function *'

@Type.add_type('DATAREP_EXTENT_FUNCTION', abi_type=['ompi'])
class TypeDatarepExtentFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Datarep_extent_function *'


@Type.add_type('DATAREP_EXTENT_FUNCTION', abi_type=['forum'])
class TypeDatarepExtentFunctionForum(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Datarep_extent_function *'

@Type.add_type('SESSION_ERRHANDLER_FUNCTION', abi_type=['ompi'])
class TypeSessionErrhandlerFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Session_errhandler_function *'


@Type.add_type('SESSION_ERRHANDLER_FUNCTION', abi_type=['forum'])
class TypeSessionErrhandlerFunctionForum(ForumABIType):

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

@Type.add_type('TYPE_COPY_ATTR_FUNCTION', abi_type=['forum'])
class TypeTypeCopyAttrFunctionForum(ForumABIType):

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Type_copy_attr_function')
        return f'{type_name} *'

    @property
    def init_code(self):
        code = []
        code = [f'MPI_Type_copy_attr_function *{self.tmpname} = {ConvertFuncs.TYPE_COPY_ATTR_FUNCTION}({self.name});']
        code.append('ompi_abi_wrapper_helper_t *helper = NULL;')
        code.append('MPI_Type_copy_attr_function_ABI_INTERNAL *copy_fn;')
        # The helper is handed to ompi_attr_create_keyval() as its
        # bindings_extra_state argument, so the attribute engine owns
        # it from the call onward: the keyval destructor free()s it
        # when the keyval is destroyed.  Do NOT free it here on
        # failure -- the engine's hash-insertion failure path destroys
        # the keyval (and the helper) itself; only the never-created
        # OOM corners leak it, matching the engine's semantics for the
        # Fortran bindings.
        code.append('helper = ( ompi_abi_wrapper_helper_t *)ompi_abi_malloc(1, sizeof(ompi_abi_wrapper_helper_t));')
        code.append('if (NULL == helper) return ompi_convert_intern_error_abi_error(MPI_ERR_NO_MEM);')
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
        code.append('    MPI_Datatype_ABI_INTERNAL type_tmp = ompi_convert_datatype_ompi_to_forum(oldtype);')
        code.append('    int type_keyval_tmp = ompi_convert_attr_key_ompi_to_forum(type_keyval);')
        code.append('    return helper->user_copy_fn((MPI_Datatype_ABI_INTERNAL)type_tmp, type_keyval_tmp, helper->user_extra_state, attribute_val_in, attribute_val_out, flag);')
        code.append('}')
        code.append('static int ompi_abi_delete_attr_fn(MPI_Datatype oldtype, int type_keyval, void *attribute_val, void *extra_state)')
        code.append('{')
        code.append('    ompi_abi_wrapper_helper_t *helper = (ompi_abi_wrapper_helper_t *)extra_state;')
        code.append('    MPI_Datatype_ABI_INTERNAL type_tmp = ompi_convert_datatype_ompi_to_forum(oldtype);')
        code.append('    int type_keyval_tmp = ompi_convert_attr_key_ompi_to_forum(type_keyval);')
        code.append('    return helper->user_delete_fn((MPI_Datatype_ABI_INTERNAL)type_tmp, type_keyval_tmp, attribute_val, helper->user_extra_state);')
        code.append('}')
        return code

@Type.add_type('TYPE_DELETE_ATTR_FUNCTION', abi_type=['ompi'])
class TypeTypeDeleteAttrFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Type_delete_attr_function *'


@Type.add_type('TYPE_DELETE_ATTR_FUNCTION', abi_type=['forum'])
class TypeTypeDeleteAttrFunctionForum(ForumABIType):

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


@Type.add_type('WIN_ERRHANDLER_FUNCTION', abi_type=['forum'])
class TypeWinErrhandlerFunctionForum(ForumABIType):
                
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


@Type.add_type('WIN_COPY_ATTR_FUNCTION', abi_type=['forum'])
class TypeWinCopyAttrFunctionForum(ForumABIType):

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Win_copy_attr_function')
        return f'{type_name} *'

    @property
    def init_code(self):
        code = []
        code = [f'MPI_Win_copy_attr_function *{self.tmpname} = {ConvertFuncs.WIN_COPY_ATTR_FUNCTION}({self.name});']
        code.append('ompi_abi_wrapper_helper_t *helper = NULL;')
        code.append('MPI_Win_copy_attr_function_ABI_INTERNAL *copy_fn;')
        # The helper is handed to ompi_attr_create_keyval() as its
        # bindings_extra_state argument, so the attribute engine owns
        # it from the call onward: the keyval destructor free()s it
        # when the keyval is destroyed.  Do NOT free it here on
        # failure -- the engine's hash-insertion failure path destroys
        # the keyval (and the helper) itself; only the never-created
        # OOM corners leak it, matching the engine's semantics for the
        # Fortran bindings.
        code.append('helper = ( ompi_abi_wrapper_helper_t *)ompi_abi_malloc(1,sizeof(ompi_abi_wrapper_helper_t));')
        code.append('if (NULL == helper) return ompi_convert_intern_error_abi_error(MPI_ERR_NO_MEM);')
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
        code.append('    MPI_Win_ABI_INTERNAL win_tmp = ompi_convert_win_ompi_to_forum(oldwin);')
        code.append('    int win_keyval_tmp = ompi_convert_attr_key_ompi_to_forum(win_keyval);')
        code.append('    return helper->user_copy_fn((MPI_Win_ABI_INTERNAL)win_tmp, win_keyval_tmp, helper->user_extra_state, attribute_val_in, attribute_val_out, flag);')
        code.append('}')
        code.append('static int ompi_abi_delete_attr_fn(MPI_Win oldwin, int win_keyval, void *attribute_val, void *extra_state)')
        code.append('{')
        code.append('    ompi_abi_wrapper_helper_t *helper = (ompi_abi_wrapper_helper_t *)extra_state;')
        code.append('    MPI_Win_ABI_INTERNAL win_tmp = ompi_convert_win_ompi_to_forum(oldwin);')
        code.append('    int win_keyval_tmp = ompi_convert_attr_key_ompi_to_forum(win_keyval);')
        code.append('    return helper->user_delete_fn((MPI_Win_ABI_INTERNAL)win_tmp, win_keyval_tmp, attribute_val, helper->user_extra_state);')
        code.append('}')
        return code


@Type.add_type('WIN_DELETE_ATTR_FUNCTION', abi_type=['ompi'])
class TypeWinDeleteAttrFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Win_delete_attr_function *'


@Type.add_type('WIN_DELETE_ATTR_FUNCTION', abi_type=['forum'])
class TypeWinDeleteAttrFunctionForum(ForumABIType):

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


@Type.add_type('ERRHANDLER', abi_type=['forum'])
class TypeErrhandlerForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_Errhandler {self.tmpname} = {ConvertFuncs.ERRHANDLER}({self.name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Errhandler')

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Errhandler'
        
    def return_code(self, name):
        return [f'return {ConvertOMPIToForum.ERRHANDLER}({name});']

@Type.add_type('ERRHANDLER_OUT', abi_type=['ompi'])
class TypeErrhandlerOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Errhandler *'


@Type.add_type('ERRHANDLER_OUT', abi_type=['forum'])
class TypeErrhandlerOutForum(Type):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.ERRHANDLER}((MPI_Errhandler) *{self.name});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Errhandler')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Errhandler *) {self.name}'

@Type.add_type('ERRHANDLER_INOUT', abi_type=['ompi'])
class TypeErrhandlerInOut(TypeErrhandlerOut):
    pass

@Type.add_type('ERRHANDLER_INOUT', abi_type=['forum'])
class TypeErrhandlerInOutForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_Errhandler {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.ERRHANDLER}(*{self.name}) : MPI_ERRHANDLER_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.ERRHANDLER}((MPI_Errhandler) {self.tmpname});']
        
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


@Type.add_type('GROUP', abi_type=['forum'])
class TypeGroupForum(ForumABIType):

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Group')

    @property
    def init_code(self):
        return [f'MPI_Group {self.tmpname} = {ConvertFuncs.GROUP}({self.name});']
        
    def tmp_type_text(self, enable_count=False):
        return 'MPI_Group'

    def return_code(self, name):
        return [f'return {ConvertOMPIToForum.GROUP}({name});']


@Type.add_type('GROUP_OUT', abi_type=['ompi'])
class TypeGroupOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Group *'


@Type.add_type('GROUP_OUT', abi_type=['forum'])
class TypeGroupOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.GROUP}((MPI_Group) *{self.name});']

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


@Type.add_type('GROUP_INOUT', abi_type=['forum'])
class TypeGroupInOutForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_Group {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.GROUP}(*{self.name}) : MPI_GROUP_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.GROUP}({self.tmpname});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Group')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Group *) (NULL != {self.name} ? &{self.tmpname} : NULL)'

@Type.add_type('SESSION_INOUT', abi_type=['ompi'])
class TypeSessionInOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Session *'

@Type.add_type('SESSION_OUT', abi_type=['ompi'])
class TypeSessionOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_Session *'


@Type.add_type('SESSION_INOUT', abi_type=['forum'])
class TypeSessionInOutForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_Session {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.SESSION}(*{self.name}) : MPI_SESSION_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.SESSION}({self.tmpname});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_Session')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Session *) (NULL != {self.name} ? &{self.tmpname} : NULL)'


@Type.add_type('SESSION_OUT', abi_type=['forum'])
class TypeSessionOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.SESSION}((MPI_Session) *{self.name});']

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


@Type.add_type('SESSION', abi_type=['forum'])
class TypeSessionForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_Session {self.tmpname} = {ConvertFuncs.SESSION}({self.name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_Session')

    def tmp_type_text(self, enable_count=False):
        return 'MPI_Session'

    def return_code(self, name):
        return [f'return {ConvertOMPIToForum.SESSION}({name});']


@Type.add_type('T_ENUM', abi_type=['ompi'])
class TypeTEnum(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_enum'

@Type.add_type('T_ENUM', abi_type=['forum'])
class TypeTEnumForum(ForumABIType):

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

@Type.add_type('T_ENUM_OUT', abi_type=['forum'])
class TypeTEnumOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.T_ENUM}((MPI_T_enum) *{self.name});']

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

@Type.add_type('CVAR_HANDLE', abi_type=['forum'])
class TypeCvarHandleForum(ForumABIType):

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

@Type.add_type('CVAR_HANDLE_OUT', abi_type=['forum'])
class TypeCvarHandleOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.CVAR_HANDLE}((MPI_T_cvar_handle) *{self.name});']

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

@Type.add_type('CVAR_HANDLE_INOUT', abi_type=['forum'])
class TypeCvarHandleInOutForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_T_cvar_handle {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.CVAR_HANDLE}(*{self.name}) : MPI_T_CVAR_HANDLE_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.CVAR_HANDLE}((MPI_T_cvar_handle) {self.tmpname});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_T_cvar_handle')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_T_cvar_handle *) (NULL != {self.name} ? &{self.tmpname} : NULL)'

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

@Type.add_type('BIND_OUT', abi_type=['forum'])
class TypeBindOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.T_BIND}(*{self.name});']

    def type_text(self, enable_count=False):
        return 'int *'

    @property
    def argument(self):
        return self.name

@Type.add_type('EVENT_REGISTRATION', abi_type=['ompi'])
class TypeEventRegistration(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_event_registration'

@Type.add_type('EVENT_REGISTRATION', abi_type=['forum'])
class TypeEventRegistrationForum(ForumABIType):
    """MPI_T event registration handle (MPI Forum ABI).

    Unlike the other MPI_T handle types (CVAR_HANDLE, PVAR_HANDLE,
    PVAR_SESSION, T_ENUM), event registration and event instance
    handles have no predefined constants in the ABI -- their
    converters would map nothing -- so a plain cast is correct in
    both directions and EVENT_REGISTRATION_OUT needs no
    back-conversion either.
    """

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_T_event_registration')

    @property
    def argument(self):
        return f'(MPI_T_event_registration){self.name}'

@Type.add_type('EVENT_REGISTRATION_OUT', abi_type=['ompi'])
class TypeEventRegistrationOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_event_registration *'

@Type.add_type('EVENT_REGISTRATION_OUT', abi_type=['forum'])
class TypeEventRegistrationOutForum(ForumABIType):

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

@Type.add_type('PVAR_HANDLE', abi_type=['forum'])
class TypePvarHandleForum(ForumABIType):

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

@Type.add_type('PVAR_HANDLE_OUT', abi_type=['forum'])
class TypePvarHandleOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.PVAR_HANDLE}((MPI_T_pvar_handle) *{self.name});']

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

@Type.add_type('PVAR_HANDLE_INOUT', abi_type=['forum'])
class TypePvarHandleInoutForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_T_pvar_handle {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.PVAR_HANDLE}(*{self.name}) : MPI_T_PVAR_HANDLE_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.PVAR_HANDLE}((MPI_T_pvar_handle){self.tmpname});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_T_pvar_handle')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_T_pvar_handle *) (NULL != {self.name} ? &{self.tmpname} : NULL)'

@Type.add_type('PVAR_SESSION', abi_type=['ompi'])
class TypePvarSession(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_pvar_session'

@Type.add_type('PVAR_SESSION', abi_type=['forum'])
class TypePvarSessionForum(ForumABIType):

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

@Type.add_type('PVAR_SESSION_OUT', abi_type=['forum'])
class TypePvarSessionOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.PVAR_SESSION}((MPI_T_pvar_session)*{self.name});']

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


@Type.add_type('PVAR_SESSION_INOUT', abi_type=['forum'])
class TypePvarSessionInOutForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_T_pvar_session {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.PVAR_SESSION}(*{self.name}) : MPI_T_PVAR_SESSION_NULL;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.PVAR_SESSION}((MPI_T_pvar_session){self.tmpname});']

    def type_text(self, enable_count=False):
        type_name = self.mangle_name('MPI_T_pvar_session')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_T_pvar_session *) (NULL != {self.name} ? &{self.tmpname} : NULL)'

@Type.add_type('T_VERBOSITY', abi_type=['ompi'])
class TypeTVerbosity(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('T_VERBOSITY', abi_type=['forum'])
class TypeTVerbosityForum(ForumABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.T_VERBOSITY}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

    @property
    def argument(self):
        return self.tmpname

@Type.add_type('T_VERBOSITY_OUT', abi_type=['ompi'])
class TypeTVerbosityOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('T_VERBOSITY_OUT', abi_type=['forum'])
class TypeTVerbosityOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.T_VERBOSITY}(*{self.name});']

    def type_text(self, enable_count=False):
        return 'int *'

    @property
    def argument(self):
        return self.name

@Type.add_type('T_SCOPE_OUT', abi_type=['ompi'])
class TypeTScopeOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('T_SCOPE_OUT', abi_type=['forum'])
class TypeTScopeOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.T_SCOPE}(*{self.name});']

    def type_text(self, enable_count=False):
        return 'int *'

    @property
    def argument(self):
        return self.name

@Type.add_type('PVAR_CLASS', abi_type=['ompi'])
class TypePvarClass(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('PVAR_CLASS', abi_type=['forum'])
class TypePvarClassForum(ForumABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.PVAR_CLASS}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

    @property
    def argument(self):
        return self.tmpname

@Type.add_type('PVAR_CLASS_OUT', abi_type=['ompi'])
class TypePvarClassOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('PVAR_CLASS_OUT', abi_type=['forum'])
class TypePvarClassOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.PVAR_CLASS}(*{self.name});']

    def type_text(self, enable_count=False):
        return f'int *'

    @property
    def argument(self):
        return self.name

@Type.add_type('CB_SAFETY', abi_type=['ompi'])
class TypeCbSafety(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_cb_safety'

@Type.add_type('CB_SAFETY', abi_type=['forum'])
class TypeCbSafetyForum(ForumABIType):

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

@Type.add_type('SOURCE_ORDER', abi_type=['forum'])
class TypeSourceOrderForum(ForumABIType):

    @property
    def init_code(self):
        return [f'MPI_T_source_order {self.tmpname} = {ConvertFuncs.T_SOURCE_ORDER}({self.name});']

    def type_text(self, enable_count=False):
        return self.mangle_name('MPI_T_source_order')

    @property
    def argument(self):
        return self.tmpname

@Type.add_type('SOURCE_ORDER_OUT', abi_type=['ompi'])
class TypeSourceOrderOut(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_source_order *'

@Type.add_type('SOURCE_ORDER_OUT', abi_type=['forum'])
class TypeSourceOrderOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.T_SOURCE_ORDER}((MPI_T_source_order) *{self.name});']

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

@Type.add_type('EVENT_FREE_CB_FUNCTION', abi_type=['forum'])
class TypeEventFreeCBFunctionForum(ForumABIType):

    def type_text(self, enable_count=False):
        return 'MPI_T_event_free_cb_function'

    @property
    def argument(self):
        return self.name

@Type.add_type('EVENT_DROPPED_CB_FUNCTION', abi_type=['ompi'])
class TypeEventDroppedCBFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_event_dropped_cb_function'

@Type.add_type('EVENT_DROPPED_CB_FUNCTION', abi_type=['forum'])
class TypeEventDroppedCBFunctionForum(ForumABIType):

    def type_text(self, enable_count=False):
        return 'MPI_T_event_dropped_cb_function'

    @property
    def argument(self):
        return self.name

@Type.add_type('EVENT_CB_FUNCTION', abi_type=['ompi'])
class TypeEventCBFunction(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_event_cb_function'

@Type.add_type('EVENT_CB_FUNCTION', abi_type=['forum'])
class TypeEventCBFunctionForum(ForumABIType):

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

@Type.add_type('ATTR_KEY', abi_type=['forum'])
class TypeAttrKeyForum(ForumABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.ATTR_KEY}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('ATTR_KEY_OUT', abi_type=['ompi'])
class TypeAttrKeyOut(Type):
    
    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('ATTR_KEY_OUT', abi_type=['forum'])
class TypeAttrKeyOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.ATTR_KEY}(*{self.name});']

    def type_text(self, enable_count=False):
        return f'int *'

    @property
    def argument(self):
        return self.name

@Type.add_type('ATTR_KEY_INOUT', abi_type=['ompi'])
class TypeAttrKeyInOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('ATTR_KEY_INOUT', abi_type=['forum'])
class TypeAttrKeyInOutForum(ForumABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = (NULL != {self.name}) ? {ConvertFuncs.ATTR_KEY}(*{self.name}) : MPI_KEYVAL_INVALID;']

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.ATTR_KEY}({self.tmpname});']

    def type_text(self, enable_count=False):
        return f'int *'

    @property
    def argument(self):
        return f'(NULL != {self.name} ? &{self.tmpname} : NULL)'

@Type.add_type('SPLIT_TYPE', abi_type=['ompi'])
class TypeSplitType(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('SPLIT_TYPE', abi_type=['forum'])
class TypeSplitTypeForum(ForumABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.SPLIT_TYPE}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('SUBARRAY_ORDER', abi_type=['ompi'])
class TypeSubarrayOrder(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('SUBARRAY_ORDER', abi_type=['forum'])
class TypeSubArrayOrderForum(ForumABIType):

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
@Type.add_type('WEIGHTS', abi_type=['forum'])
class TypeWeightForum(ForumABIType):

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

@Type.add_type('COMM_CMP_OUT', abi_type=['forum'])
class TypeCommCmpOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.COMM_CMP}(*{self.name});']

    def type_text(self, enable_count=False):
        return 'int *'

    @property
    def argument(self):
        return f'{self.name}'

@Type.add_type('EVENT_INSTANCE', abi_type=['ompi'])
class TypeEventInstance(Type):

    def type_text(self, enable_count=False):
        return 'MPI_T_event_instance'


@Type.add_type('EVENT_INSTANCE', abi_type=['forum'])
class TypeEventInstanceForum(ForumABIType):

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

@Type.add_type('DISTRIB_ARRAY', abi_type=['forum'])
class TypeDistributionArrayForum(ForumABIType):

    @property
    def init_code(self):
        code = [f'int size_{self.tmpname} = {self.count_param};']
        code.append(f'int *{self.tmpname} = NULL;')
        code.append('if('+f'{self.name}' + '!= NULL)' + '{')
        code.append(f'{self.tmpname} = (int *)ompi_abi_malloc(size_{self.tmpname}, sizeof(int));')
        code.append(f'if (NULL == {self.tmpname}) return ompi_convert_intern_error_abi_error(MPI_ERR_NO_MEM);')
        code.append(f'for(int i=0;i<size_{self.tmpname};i++){{')
        code.append(f'{self.tmpname}[i] = {ConvertFuncs.SUBARRAY_DISTRIB_TYPES}({self.name}[i]);')
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

@Type.add_type('DARGS_ARRAY', abi_type=['forum'])
class TypeDargsArrayForum(ForumABIType):

    @property
    def init_code(self):
        code = [f'int size_{self.tmpname} = {self.count_param};']
        code.append(f'int *{self.tmpname} = NULL;')
        code.append('if('+f'{self.name}' + '!= NULL)' + '{')
        code.append(f'{self.tmpname} = (int *)ompi_abi_malloc(size_{self.tmpname}, sizeof(int));')
        code.append(f'if (NULL == {self.tmpname}) return ompi_convert_intern_error_abi_error(MPI_ERR_NO_MEM);')
        code.append(f'for(int i=0;i<size_{self.tmpname};i++){{')
        code.append(f'{self.tmpname}[i] = {ConvertFuncs.SUBARRAY_DARGS_TYPES}({self.name}[i]);')
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

@Type.add_type('MODE_BITS', abi_type=['forum'])
class TypeModeBitsForum(ForumABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.MODE_BITS}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('MODE_BITS_OUT', abi_type=['forum'])
class TypeModeBitsOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.MODE_BITS}(*{self.name});']

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

@Type.add_type('RMA_MODE_BITS', abi_type=['forum'])
class TypeRmaModeBitsForum(ForumABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.RMA_MODE_BITS}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('RMA_MODE_BITS_OUT', abi_type=['forum'])
class TypeRmaModeBitsOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.RMA_MODE_BITS}(*{self.name});']

    def type_text(self, enable_count=False):
        return 'int *'

    @property
    def argument(self):
        return f'{self.name}'

@Type.add_type('WHENCE', abi_type=['ompi'])
class TypeWhence(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('WHENCE', abi_type=['forum'])
class TypeWhenceForum(ForumABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.WHENCE}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('COMBINER_OUT', abi_type=['ompi'])
class TypeCombinerOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('COMBINER_OUT', abi_type=['forum'])
class TypeCombinerOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.COMBINER}(*{self.name});']

    def type_text(self, enable_count=False):
        return 'int *'

    @property
    def argument(self):
        return f'{self.name}'

@Type.add_type('WIN_LOCK', abi_type=['ompi'])
class TypeWinLock(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('WIN_LOCK', abi_type=['forum'])
class TypeWinLockForum(ForumABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.WIN_LOCK}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('TOPO_OUT', abi_type=['ompi'])
class TopoOut(Type):

    def type_text(self, enable_count=False):
        return 'int *'

@Type.add_type('TOPO_OUT', abi_type=['forum'])
class TopoOutForum(ForumABIType):

    @property
    def final_code(self):
        return [f'if (NULL != {self.name}) *{self.name} = {ConvertOMPIToForum.TOPO}(*{self.name});']

    def type_text(self, enable_count=False):
        return 'int *'

    @property
    def argument(self):
        return f'{self.name}'

@Type.add_type('TYPECLASS', abi_type=['ompi'])
class TypeClass(Type):

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('TYPECLASS', abi_type=['forum'])
class TypeClassForum(ForumABIType):

    @property
    def init_code(self):
        return [f'int {self.tmpname} = {ConvertFuncs.TYPECLASS}({self.name});']

    def type_text(self, enable_count=False):
        return 'int'

@Type.add_type('OBJ_HANDLE', abi_type=['ompi'])
class TypeObjHandle(Type):

    def type_text(self, enable_count=False):
        return 'void *'

@Type.add_type('OBJ_HANDLE', abi_type=['forum'])
class TypeObjHandleForum(ForumABIType):
    """The obj_handle parameter of the MPI_T handle-allocation routines.

    Per MPI-5.0 (e.g. p.751), obj_handle is the ADDRESS of a local
    variable that stores the bound MPI object's handle -- it is a
    pointer to a handle, not a handle value itself.  A NULL obj_handle
    means "no object" and must be passed through unconverted (the
    standard requires it be ignored for an MPI_T_BIND_NO_OBJECT
    variable/event).

    Under the MPI Forum ABI, the *pointed-to* handle is the ABI-encoded
    value that ConvertFuncs.OBJ_HANDLE
    (ompi_convert_abi_obj_handle_intern_obj_handle) knows how to
    translate to an internal pointer.  So: dereference
    obj_handle once to reach that ABI value, convert only the pointed-to
    VALUE, and store the resulting internal pointer in a local
    temporary.  The internal (ompi ABI) entry point expects the same
    "address of a variable holding the handle" contract (see
    ompit_obj_invalid(), mca_base_event_handle_alloc(),
    mca_base_pvar_handle_alloc(), all of which perform exactly one
    dereference), so the argument passed down is the address of that
    temporary -- not the temporary's value.
    """

    @property
    def init_code(self):
        return [
            f'void *{self.tmpname} = NULL;',
            f'if (NULL != {self.name}) {{',
            f'{self.tmpname} = {ConvertFuncs.OBJ_HANDLE}(*(void **) {self.name});',
            '}',
        ]

    @property
    def argument(self):
        return f'(void *) (NULL != {self.name} ? &{self.tmpname} : NULL)'

    def type_text(self, enable_count=False):
        return 'void *'

