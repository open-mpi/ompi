#file: pmix.pxi
#
# Copyright (c) 2022      Nanook Consulting. All rights reserved
#

from libc.string cimport memset, strncpy, strcpy, strlen, strdup
from libc.stdlib cimport malloc, realloc, free
from libc.string cimport memcpy
from cpython.mem cimport PyMem_Malloc, PyMem_Realloc, PyMem_Free
from cpython.pycapsule cimport PyCapsule_New, PyCapsule_GetPointer

# pull in all the constant definitions - we
# store them in a separate file for neatness
include "pmix_constants.pxi"

# provide a lock class for catching information
# returned in callback functions
class myLock(threading.Event):
    def __init__(self):
        threading.Event.__init__(self)
        self.event = threading.Event()
        self.status = PMIX_ERR_NOT_SUPPORTED
        self.sz = 0
        self.info = []

    def set(self, status):
        self.status = status
        self.event.set()

    def clear(self):
        self.event.clear()

    def wait(self):
        self.event.wait()

    def get_status(self):
        return self.status

    def cache_data(self, data, sz):
        self.data = array.array('B', data[0])
        # need to copy the data bytes as the
        # PMIx server will free it upon return
        n = 1
        while n < sz:
            self.data.append(data[n])
            n += 1
        self.sz = sz

    def fetch_data(self):
        return (self.data, self.sz)

    def cache_info(self, info:list):
        # need to copy the info array as the
        # PMIx server will free it upon execing
        # the callback function
        self.info = []
        for x in info:
            self.info.append(x)

    def fetch_info(self, info:list):
        for x in self.info:
            info.append(x)

ctypedef struct pmix_pyshift_t:
    char *op
    pmix_byte_object_t payload
    size_t idx
    pmix_modex_cbfunc_t modex
    pmix_status_t status
    pmix_byte_object_t bo
    pmix_byte_object_t *cred
    pmix_iof_channel_t channel
    pmix_nspace_t nspace
    pmix_proc_t source
    pmix_proc_t *proc
    pmix_pdata_t *pdata
    pmix_info_t *results
    size_t nresults
    pmix_info_t *info
    const char *data
    size_t ndata
    pmix_op_cbfunc_t op_cbfunc
    pmix_iof_cbfunc_t iof
    pmix_info_cbfunc_t query
    pmix_spawn_cbfunc_t spawn
    pmix_lookup_cbfunc_t lookup
    pmix_release_cbfunc_t release_fn
    pmix_event_notification_cbfunc_fn_t event_handler
    pmix_tool_connection_cbfunc_t toolconnected
    pmix_credential_cbfunc_t getcredential
    pmix_validation_cbfunc_t validationcredential
    pmix_info_cbfunc_t allocate
    pmix_info_cbfunc_t sessioncontrol
    void *notification_cbdata
    void *cbdata

cdef void iofhdlr_cache(capsule, ret):
    cdef pmix_pyshift_t *shifter
    cdef pmix_byte_object_t *bo
    shifter = <pmix_pyshift_t*>PyCapsule_GetPointer(capsule, "iofhdlr_cache")
    if NULL == shifter[0].payload.bytes:
        bo = NULL
    else:
        bo = &shifter[0].payload
    pyiofhandler(shifter[0].idx, shifter[0].channel, &shifter[0].source,
                 bo, shifter[0].info, shifter[0].ndata)
    if 0 < shifter[0].ndata:
        pmix_free_info(shifter[0].info, shifter[0].ndata)
    free(shifter[0].payload.bytes)
    return

cdef void event_cache_cb(capsule, ret):
    cdef pmix_pyshift_t *shifter
    shifter = <pmix_pyshift_t*>PyCapsule_GetPointer(capsule, "event_handler")
    pyeventhandler(shifter[0].idx, shifter[0].status, &shifter[0].source,
                   shifter[0].info, shifter[0].ndata,
                   shifter[0].results, shifter[0].nresults,
                   shifter[0].event_handler, shifter[0].notification_cbdata)

cdef void pmix_convert_locality(pmix_locality_t loc, pyloc:list):
    if PMIX_LOCALITY_NONLOCAL & loc:
        pyloc.append(PMIX_LOCALITY_NONLOCAL)
    if PMIX_LOCALITY_SHARE_HWTHREAD & loc:
        pyloc.append(PMIX_LOCALITY_SHARE_HWTHREAD)
    if PMIX_LOCALITY_SHARE_CORE & loc:
        pyloc.append(PMIX_LOCALITY_SHARE_CORE)
    if PMIX_LOCALITY_SHARE_L1CACHE & loc:
        pyloc.append(PMIX_LOCALITY_SHARE_L1CACHE)
    if PMIX_LOCALITY_SHARE_L2CACHE & loc:
        pyloc.append(PMIX_LOCALITY_SHARE_L2CACHE)
    if PMIX_LOCALITY_SHARE_L3CACHE & loc:
        pyloc.append(PMIX_LOCALITY_SHARE_L3CACHE)
    if PMIX_LOCALITY_SHARE_PACKAGE & loc:
        pyloc.append(PMIX_LOCALITY_SHARE_PACKAGE)
    if PMIX_LOCALITY_SHARE_NUMA & loc:
        pyloc.append(PMIX_LOCALITY_SHARE_NUMA)
    if PMIX_LOCALITY_SHARE_NODE & loc:
        pyloc.append(PMIX_LOCALITY_SHARE_NODE)
    if 0 == pyloc.len():
        pyloc.append(PMIX_LOCALITY_NONLOCAL)
    return

cdef void pmix_unload_argv(char **keys, argv:list):
    n = 0
    while NULL != keys[n]:
        mykey = keys[n].decode('ascii')
        argv.append(mykey)
        n += 1

cdef int pmix_load_argv(char **keys, argv:list):
    n = 0
    for a in argv:
        pya = a
        if isinstance(a, str):
            pya = a.encode('ascii')
        keys[n] = strdup(pya)
        n += 1
    keys[n] = NULL
    return PMIX_SUCCESS

cdef int pmix_load_darray(pmix_data_array_t *array, mytype, mylist:list):
    cdef pmix_info_t *infoptr;
    mysize = len(mylist)
    if PMIX_INFO == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(pmix_info_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        infoptr = <pmix_info_t*>array[0].array
        rc = pmix_load_info(infoptr, mylist)
    elif PMIX_BOOL == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(int*))
        n = 0
        if not array[0].array:
            return PMIX_ERR_NOMEM
        boolptr = <int*>array[0].array
        for item in mylist:
            int_bool = pmix_bool_convert(item)
            if int_bool != 0 and int_bool != 1:
                return PMIX_ERR_BAD_PARAM
            boolptr[n] = int_bool
            n += 1
    elif PMIX_BYTE == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(uint8_t*))
        n = 0
        # byte val is uint8 type
        if not array[0].array:
            return PMIX_ERR_NOMEM
        bptr = <uint8_t*> array[0].array
        for item in mylist:
            if not isinstance(item, pmix_int_types):
                print("uint8 value declared but non-integer provided")
                return PMIX_ERR_TYPE_MISMATCH
            if item > 255:
                print("uint8 value is out of bounds")
                return PMIX_ERR_BAD_PARAM
            bptr[n] = int(item)
            n += 1
    elif PMIX_STRING == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(char*))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        strptr = <char**> array[0].array
        for item in mylist:
            if isinstance(item, str):
                pykey = item.encode('ascii')
            else:
                pykey = item
            try:
                strptr[n] = strdup(pykey)
            except:
                print("String value declared but non-string provided")
                return PMIX_ERR_TYPE_MISMATCH
            n += 1
    elif PMIX_SIZE == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(size_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        sptr = <size_t*> array[0].array
        for item in mylist:
            if not isinstance(item, pmix_int_types):
                print("size_t value declared but non-integer provided")
                return PMIX_ERR_TYPE_MISMATCH
            sptr[n] = int(item)
            n += 1
    elif PMIX_PID == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(pid_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        pidptr = <pid_t*> array[0].array
        for item in mylist:
            if not isinstance(item, pmix_int_types):
                print("pid_t value declared but non-integer provided")
                return PMIX_ERR_TYPE_MISMATCH
            pidptr[n] = int(item)
            n += 1
    elif PMIX_INT == mytype or PMIX_UINT == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(int))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        iptr = <int*> array[0].array
        for item in mylist:
            if not isinstance(item, pmix_int_types):
                print("int value declared but non-integer provided")
                return PMIX_ERR_TYPE_MISMATCH
            iptr[n] = int(item)
            n += 1
    elif PMIX_INT8 == mytype or PMIX_UINT8 == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(int8_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        i8ptr = <int8_t*> array[0].array
        for item in mylist:
            if not isinstance(item, pmix_int_types):
                print("8-bit int value declared but non-integer provided")
                return PMIX_ERR_TYPE_MISMATCH
            i8ptr[n] = int(item)
            n += 1
    elif PMIX_INT16 == mytype or PMIX_UINT16 == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(int16_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        i16ptr = <int16_t*> array[0].array
        for item in mylist:
            if not isinstance(item, pmix_int_types):
                print("16-bit int value declared but non-integer provided")
                return PMIX_ERR_TYPE_MISMATCH
            i16ptr[n] = int(item)
            n += 1
    elif PMIX_INT32 == mytype or PMIX_UINT32 == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(int32_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        i32ptr = <int32_t*> array[0].array
        for item in mylist:
            if not isinstance(item, pmix_int_types):
                print("32-bit int value declared but non-integer provided")
                return PMIX_ERR_TYPE_MISMATCH
            i32ptr[n] = int(item)
            n += 1
    elif PMIX_INT64 == mytype or PMIX_UINT64 == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(int64_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        i64ptr = <int64_t*> array[0].array
        for item in mylist:
            if not isinstance(item, pmix_int_types):
                print("64-bit int value declared but non-integer provided")
                return PMIX_ERR_TYPE_MISMATCH
            i64ptr[n] = int(item)
            n += 1
    elif PMIX_FLOAT == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(float))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        fptr = <float*> array[0].array
        for item in mylist:
            fptr[n] = float(item)
            n += 1
    elif PMIX_DOUBLE == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(double))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        dptr = <double*> array[0].array
        for item in mylist:
            dptr[n] = float(item)
            n += 1
            n += 1
    elif PMIX_TIMEVAL == mytype:
        # TODO: Not clear that "timeval" has the same size as
        # "struct timeval"
        array[0].array = PyMem_Malloc(mysize * sizeof(timeval))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        tvptr = <timeval*> array[0].array
        for item in mylist:
            tvptr[n].tv_sec  = item['sec']
            tvptr[n].tv_usec = item['usec']
            n += 1
    elif PMIX_TIME == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(time_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        tmptr = <time_t*> array[0].array
        for item in mylist:
            tmptr[n] = item
            n += 1
    elif PMIX_STATUS == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(int))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        stptr = <int*> array[0].array
        for item in mylist:
            stptr[n] = item
            n += 1
    elif PMIX_PROC_RANK == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(pmix_rank_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        rkptr = <pmix_rank_t*> array[0].array
        for item in mylist:
            rkptr[n] = item
            n += 1
    elif PMIX_PROC == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(pmix_proc_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        prcptr = <pmix_proc_t*> array[0].array
        for item in mylist:
            pmix_copy_nspace(prcptr[n].nspace, item['nspace'])
            prcptr[n].rank = item['rank']
            n += 1
    elif PMIX_BYTE_OBJECT == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(pmix_byte_object_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        boptr = <pmix_byte_object_t*>array[0].array
        for item in mylist:
            boptr[n].size = item['size']
            boptr[n].bytes = <char*> PyMem_Malloc(boptr[n].size)
            if not boptr[n].bytes:
                return PMIX_ERR_NOMEM
            pyarr = bytes(item['bytes'])
            pyptr = <const char*> pyarr
            memcpy(boptr[n].bytes, pyptr, boptr[n].size)
            n += 1
    elif PMIX_PERSISTENCE == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(pmix_persistence_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        perptr = <pmix_persistence_t*> array[0].array
        for item in mylist:
            perptr[n] = item
            n += 1
    elif PMIX_SCOPE == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(pmix_scope_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        scptr = <pmix_scope_t*> array[0].array
        for item in mylist:
            scptr[n] = item
            n += 1
    elif PMIX_RANGE == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(pmix_data_range_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        rgptr = <pmix_data_range_t*> array[0].array
        for item in mylist:
            rgptr[n] = item
            n += 1
    elif PMIX_PROC_STATE == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(pmix_proc_state_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        psptr = <pmix_proc_state_t*> array[0].array
        for item in mylist:
            psptr[n] = item
            n += 1
    elif PMIX_PROC_INFO == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(pmix_proc_info_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        piptr = <pmix_proc_info_t*> array[0].array
        for item in mylist:
            pmix_copy_nspace(piptr[n].proc.nspace, item['proc']['nspace'])
            piptr[n].proc.rank = item['proc']['rank']
            hostname = item['hostname']
            if isinstance(hostname, str):
                pyhostname = hostname.encode('ascii')
            else:
                pyhostname = hostname
            pyhostnameptr = <const char *>(pyhostname)
            piptr[n].hostname = strdup(pyhostnameptr)
            executable = item['executable']
            if isinstance(executable, str):
                pyexec = executable.encode('ascii')
            else:
                pyexec = executable
            pyexecptr = <const char *>(pyexec)
            piptr[n].executable_name = strdup(pyexecptr)
            piptr[n].pid = item['pid']
            piptr[n].exit_code = item['exitcode']
            piptr[n].state = item['state']
            n += 1
    elif PMIX_DATA_ARRAY == mytype:
        array[0].array = <pmix_data_array_t*> PyMem_Malloc(sizeof(pmix_data_array_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        daptr = <pmix_data_array_t*>array[0].array
        n = 0
        for item in mylist:
            daptr[n].type = item['val_type']
            daptr[n].size = len(item['value'])
            daptr[n].array = <pmix_data_array_t*> PyMem_Malloc(sizeof(pmix_data_array_t))
            if not daptr[n].array:
                return PMIX_ERR_NOMEM
            mydaptr = <pmix_data_array_t*>daptr[n].array
            try:
                return pmix_load_darray(mydaptr, daptr[n].type, item['value'])
            except:
                return PMIX_ERR_NOT_SUPPORTED
    elif PMIX_ALLOC_DIRECTIVE == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(pmix_alloc_directive_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        aldptr = <pmix_alloc_directive_t*> array[0].array
        for item in mylist:
            aldptr[n] = item
            n += 1
    elif PMIX_ENVAR == mytype:
        array[0].array = PyMem_Malloc(mysize * sizeof(pmix_envar_t))
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        envptr = <pmix_envar_t*> array[0].array
        for item in mylist:
            enval = item['envar']
            if isinstance(enval, str):
                pyns = enval.encode('ascii')
            else:
                pyns = enval
            pynsptr = <const char *>(pyns)
            envptr[n].envar = strdup(pynsptr)
            if isinstance(enval, str):
                pyns = enval.encode('ascii')
            else:
                pyns = enval
            pynsptr = <const char *>(pyns)
            envptr[n].value = strdup(pynsptr)
            pyseparator = <char>ord(item['separator'])
            envptr[n].separator = pyseparator
            n += 1
    else:
        print("UNRECOGNIZED DATA TYPE IN ARRAY")
        return PMIX_ERR_NOT_SUPPORTED
    return PMIX_SUCCESS

cdef dict pmix_unload_darray(pmix_data_array_t *array):
    cdef pmix_info_t *infoptr;
    if PMIX_INFO == array.type:
        ilist = []
        n = 0
        infoptr = <pmix_info_t*>array[0].array
        rc = pmix_unload_info(infoptr, array.size, ilist)
        darray = {'type':array.type, 'array':ilist}
        pmix_free_info(infoptr, array.size)
        return darray
    elif PMIX_BOOL == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        boolptr = <int*>array[0].array
        list = []
        n = 0
        while n < array.size:
            list.append(boolptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_BYTE == array.type:
        # byte val is uint8 type
        if not array[0].array:
            return PMIX_ERR_NOMEM
        bptr = <uint8_t*> array[0].array
        list = []
        n = 0
        while n < array.size:
            list.append(bptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_STRING == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        strptr = <char**> array[0].array
        strlist = []
        while n < array.size:
            pyb = strptr[n]
            pystr = pyb.decode("ascii")
            strlist.append(pystr)
            free(strptr[n])
            n += 1
        darray = {'type':array.type, 'array':strlist}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_SIZE == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        sptr = <size_t*> array[0].array
        list = []
        while n < array.size:
            list.append(sptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_PID == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        pidptr = <pid_t*> array[0].array
        list = []
        while n < array.size:
            list.append(pidptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_INT == array.type or PMIX_UINT == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        iptr = <int*> array[0].array
        list = []
        while n < array.size:
            list.append(iptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_INT8 == array.type or PMIX_UINT8 == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        i8ptr = <int8_t*> array[0].array
        list = []
        while n < array.size:
            list.append(i8ptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_INT16 == array.type or PMIX_UINT16 == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        i16ptr = <int16_t*> array[0].array
        list = []
        while n < array.size:
            list.append(i16ptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_INT32 == array.type or PMIX_UINT32 == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        i32ptr = <int32_t*> array[0].array
        list = []
        while n < array.size:
            list.append(i32ptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_INT64 == array.type or PMIX_UINT64 == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        i64ptr = <int64_t*> array[0].array
        list = []
        while n < array.size:
            list.append(i64ptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_FLOAT == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        fptr = <float*> array[0].array
        list = []
        while n < array.size:
            list.append(fptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_DOUBLE == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        dptr = <double*> array[0].array
        list = []
        while n < array.size:
            list.append(dptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_TIMEVAL == array.type:
        # TODO: Not clear that "timeval" has the same size as
        # "struct timeval"
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        tvptr = <timeval*> array[0].array
        list = []
        while n < array.size:
            d = {'sec': tvptr[n].tv_sec, 'usec': tvptr[n].tv_usec}
            list.append(d)
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_TIME == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        tmptr = <time_t*> array[0].array
        list = []
        while n < array.size:
            list.append(tmptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_STATUS == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        stptr = <int*> array[0].array
        list = []
        while n < array.size:
            list.append(stptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_PROC_RANK == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        rkptr = <pmix_rank_t*> array[0].array
        list = []
        while n < array.size:
            list.append(rkptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_PROC == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        prcptr = <pmix_proc_t*> array[0].array
        list = []
        while n < array.size:
            d = {'nspace': prcptr[n].nspace, 'rank': prcptr[n].rank}
            list.append(d)
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_BYTE_OBJECT == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        boptr = <pmix_byte_object_t*>array[0].array
        list = []
        while n < array.size:
            if not boptr[n].bytes:
                return PMIX_ERR_NOMEM
            d = {'bytes': boptr[n].bytes, 'size': boptr[n].size}
            list.append(d)
            PyMem_Free(boptr[n].bytes)
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_PERSISTENCE == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        perptr = <pmix_persistence_t*> array[0].array
        list = []
        while n < array.size:
            list.append(perptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_SCOPE == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        scptr = <pmix_scope_t*> array[0].array
        list = []
        while n < array.size:
            list.append(scptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_RANGE == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        rgptr = <pmix_data_range_t*> array[0].array
        list = []
        while n < array.size:
            list.append(rgptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_PROC_STATE == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        psptr = <pmix_proc_state_t*> array[0].array
        list = []
        while n < array.size:
            list.append(psptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_PROC_INFO == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        piptr = <pmix_proc_info_t*> array[0].array
        list = []
        while n < array.size:
            d = {'proc': {'nspace':piptr[n].proc.nspace,
            'rank':piptr[n].proc.rank}, 'hostname':piptr[n].hostname,
            'executable':piptr[n].executable_name, 'pid':piptr[n].pid,
            'exitcode':piptr[n].exit_code, 'state':piptr[n].state}
            list.append(d)
            free(piptr[n].hostname)
            free(piptr[n].executable_name)
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_DATA_ARRAY == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        daptr = <pmix_data_array_t*>array[0].array
        n = 0
        while n < array.size:
            if not daptr[n].array:
                return PMIX_ERR_NOMEM
            mydaptr = <pmix_data_array_t*>daptr[n].array
            try:
                rc = pmix_unload_darray(mydaptr)
                if rc != PMIX_SUCCESS:
                    return rc
            except:
                return PMIX_ERR_NOT_SUPPORTED
            n += 1
    elif PMIX_ALLOC_DIRECTIVE == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        aldptr = <pmix_alloc_directive_t*> array[0].array
        list = []
        while n < array.size:
            list.append(aldptr[n])
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    elif PMIX_ENVAR == array.type:
        if not array[0].array:
            return PMIX_ERR_NOMEM
        n = 0
        envptr = <pmix_envar_t*> array[0].array
        list = []
        while n < array.size:
            pyenv = envptr[n].envar
            pyval = envptr[n].value
            pysep = envptr[n].separator
            d = {'envar':pyenv, 'value':pyval, 'separator':pysep}
            list.append(d)
            free(envptr[n].value)
            free(envptr[n].envar)
            n += 1
        darray = {'type':array.type, 'array':list}
        PyMem_Free(array[0].array)
        return darray
    else:
        print("UNRECOGNIZED DATA TYPE IN ARRAY")
        return PMIX_ERR_NOT_SUPPORTED
    return PMIX_SUCCESS

# provide conversion programs that translate incoming
# PMIx structures into Python dictionaries, and incoming
# arrays into Python lists of objects

def pmix_bool_convert(f):
    int_bool = PMIX_ERR_BAD_PARAM
    if isinstance(f, str):
        if f.startswith('t') or f.startswith('T'):
            int_bool = 1
        elif f.startswith('f') or f.startswith('F'):
            int_bool = 0
        else:
            print("Incorrect boolean value provided")
            int_bool = PMIX_ERR_BAD_PARAM
    elif isinstance(f, (int, float)):
        int_bool = f
    else:
        print("Unrecognized boolean value type provided")

    return int_bool

pmix_int_types = (int, long)

# provide a safe way to copy a Python nspace into
# the pmix_nspace_t structure that guarantees the
# array is NULL-terminated
cdef void pmix_copy_nspace(pmix_nspace_t nspace, ns):
    nslen = len(ns)
    if PMIX_MAX_NSLEN < nslen:
        nslen = PMIX_MAX_NSLEN
    if isinstance(ns, str):
        pyns = ns.encode('ascii')
    else:
        pyns = ns
    pynsptr = <const char *>(pyns)
    memset(nspace, 0, PMIX_MAX_NSLEN+1)
    memcpy(nspace, pynsptr, nslen)

# provide a safe way to copy a Python key into
# the pmix_key_t structure that guarantees the
# array is NULL-terminated
cdef void pmix_copy_key(pmix_key_t key, ky):
    klen = len(ky)
    if PMIX_MAX_KEYLEN < klen:
        klen = PMIX_MAX_KEYLEN
    if isinstance(ky, str):
        pykey = ky.encode('ascii')
    else:
        pykey = ky
    pykeyptr = <const char *>(pykey)
    memset(key, 0, PMIX_MAX_KEYLEN+1)
    if 'b' == ky[0]:
        memcpy(key, &pykeyptr[2], klen-3)
    else:
        memcpy(key, pykeyptr, klen)

# loads a python pmix regex into a python bytearray
cdef bytearray pmix_convert_regex(char *regex):
    if "pmix" == regex[:4].decode("ascii"):
        # remove null characters
        if b'\x00' in regex:
            regex.replace(b'\x00', '')
        ba = bytearray(regex)
    elif "blob" == regex[:4].decode("ascii"):
        sz_str    = len(regex)
        sz_prefix = 5
        # extract length of bytearray
        regex.split(b'\x00')
        len_bytearray = regex[1]
        length = len(len_bytearray) + sz_prefix + sz_str
        ba = bytearray(length)
        pyregex = <bytes> regex[:length]
        index = 0
        while index < length:
            ba[index] = pyregex[index]
            index += 1
    else:
        # last case with no ':' in string
        ba = bytearray(regex)
    return ba

# provide a function for transferring a Python 'value'
# object (a dict with value and val_type as keys)
# to a pmix_value_t
cdef int pmix_load_value(pmix_value_t *value, val:dict):
    if not isinstance(val['val_type'], pmix_int_types):
        return PMIX_ERR_BAD_PARAM
    value[0].type = val['val_type']
    if val['val_type'] == PMIX_BOOL:
        int_bool = pmix_bool_convert(val['value'])
        if int_bool != 0 and int_bool != 1:
            return PMIX_ERR_BAD_PARAM
        value[0].data.flag = int_bool
    elif val['val_type'] == PMIX_BYTE:
        # byte val is uint8 type
        if not isinstance(val['value'], pmix_int_types):
            print("uint8 value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value'] > 255:
            print("uint8 value is out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.byte = val['value']
    elif val['val_type'] == PMIX_STRING:
        if isinstance(val['value'], str):
            pykey = val['value'].encode('ascii')
        else:
            pykey = val['value']
        try:
            value[0].data.string = strdup(pykey)
        except:
            print("String value declared but non-string provided")
            return PMIX_ERR_TYPE_MISMATCH
    elif val['val_type'] == PMIX_SIZE:
        if not isinstance(val['value'], pmix_int_types):
            print("size_t value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        value[0].data.size = val['value']
    elif val['val_type'] == PMIX_PID:
        if not isinstance(val['value'], pmix_int_types):
            print("pid value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value'] < 0:
            print("pid value is negative")
            return PMIX_ERR_BAD_PARAM
        value[0].data.pid = val['value']
    elif val['val_type'] == PMIX_INT:
        if not isinstance(val['value'], pmix_int_types):
            print("integer value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        value[0].data.integer = val['value']
    elif val['val_type'] == PMIX_INT8:
        if not isinstance(val['value'], pmix_int_types):
            print("int8 value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value'] > 127 or val['value'] < -128:
            print("int8 value is out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.int8 = val['value']
    elif val['val_type'] == PMIX_INT16:
        if not isinstance(val['value'], pmix_int_types):
            print("int16 value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value'] > 32767 or val['value'] < -32768:
            print("int16 value is out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.int16 = val['value']
    elif val['val_type'] == PMIX_INT32:
        if not isinstance(val['value'], pmix_int_types):
            print("int32 value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value'] > 2147483647 or val['value'] < -2147483648:
            print("int32 value is out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.int32 = val['value']
    elif val['val_type'] == PMIX_INT64:
        if not isinstance(val['value'], pmix_int_types):
            print("int64 value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value'] > (2147483647*2147483647) or val['value'] < -(2147483648*2147483648):
            print("int64 value is out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.int64 = val['value']
    elif val['val_type'] == PMIX_UINT:
        if not isinstance(val['value'], pmix_int_types):
            print("integer value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value'] < 0:
            print("uint value out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.uint = val['value']
    elif val['val_type'] == PMIX_UINT8:
        if not isinstance(val['value'], pmix_int_types):
            print("uint8 value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value'] > 255 or val['value'] < 0:
            print("uint8 value is out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.uint8 = val['value']
    elif val['val_type'] == PMIX_UINT16:
        if not isinstance(val['value'], pmix_int_types):
            print("uint16 value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value'] > 65536 or val['value'] < 0:
            print("uint16 value is out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.uint16 = val['value']
    elif val['val_type'] == PMIX_UINT32:
        if not isinstance(val['value'], pmix_int_types):
            print("uint32 value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value'] > (65536*65536) or val['value'] < 0:
            print("uint32 value is out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.uint32 = val['value']
    elif val['val_type'] == PMIX_UINT64:
        if not isinstance(val['value'], pmix_int_types):
            print("int64 value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value'] > (2147483648*2147483648):
            print("uint64 value is out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.uint64 = val['value']
    elif val['val_type'] == PMIX_FLOAT:
        float_val = float(val['value'])
        if not isinstance(float_val, float):
            return PMIX_ERR_TYPE_MISMATCH
        value[0].data.fval = float_val
    elif val['val_type'] == PMIX_DOUBLE:
        double_val = float(val['value'])
        if not isinstance(double_val, float):
            return PMIX_ERR_TYPE_MISMATCH
        value[0].data.dval = double_val
    # TODO: need a way to verify usable timevals passed in?
    elif val['val_type'] == PMIX_TIMEVAL:
        value[0].data.tv.tv_sec  = val['value']['sec']
        value[0].data.tv.tv_usec = val['value']['usec']
    elif val['val_type'] == PMIX_TIME:
        value[0].data.time = val['val_type']
    elif val['val_type'] == PMIX_STATUS:
        if not isinstance(val['value'], int):
            return PMIX_ERR_TYPE_MISMATCH
        value[0].data.status = val['value']
    elif val['val_type'] == PMIX_PROC_RANK:
        if not isinstance(val['value'], int):
            return PMIX_ERR_TYPE_MISMATCH
        if val['value'] > (65536*65536) or val['value'] < 0:
            print("uint32 value is out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.rank = val['value']
    elif val['val_type'] == PMIX_PROC:
        value[0].data.proc = <pmix_proc_t*> PyMem_Malloc(sizeof(pmix_proc_t))
        if not value[0].data.proc:
            return PMIX_ERR_NOMEM
        # TODO: check nspace val is a char here
        pmix_copy_nspace(value[0].data.proc[0].nspace, val['value']['nspace'])
        # pmix_rank_t is defined as uint32
        if not isinstance(val['value']['rank'], pmix_int_types):
            print("uint32 value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value']['rank'] > (65536*65536):
            print("uint32 value is out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.proc[0].rank = val['value']['rank']
    # TODO: pmix byte object conversion isn't working
    elif val['val_type'] == PMIX_BYTE_OBJECT:
        value[0].data.bo.size = val['value']['size']
        value[0].data.bo.bytes = <char*> PyMem_Malloc(value[0].data.bo.size)
        if not value[0].data.bo.bytes:
            return PMIX_ERR_NOMEM
        pyptr = <char*>val['value']['bytes']
        memcpy(value[0].data.bo.bytes, pyptr, value[0].data.bo.size)
    elif val['val_type'] == PMIX_PERSISTENCE:
        # pmix_persistence_t is defined as uint8
        if not isinstance(val['value'], pmix_int_types):
            print("uint8 value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value'] > 255 or val['value'] < 0:
            print("uint8 value is out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.persist = val['value']
    elif val['val_type'] == PMIX_SCOPE:
        # pmix_scope_t is defined as uint8
        if not isinstance(val['value'], pmix_int_types):
            print("uint8 value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value'] > 255:
            print("uint8 value is out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.scope = val['value']
    elif val['val_type'] == PMIX_RANGE:
        # pmix_data_range_t is defined as uint8
        if not isinstance(val['value'], pmix_int_types):
            print("uint8 value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value'] > 255:
            print("uint8 value is out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.range = val['value']
    elif val['val_type'] == PMIX_PROC_STATE:
        # pmix_proc_state_t is defined as uint8
        if not isinstance(val['value'], pmix_int_types):
            print("uint8 value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value'] > 255:
            print("uint8 value is out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.state = val['value']
    elif val['val_type'] == PMIX_PROC_INFO:
        value[0].data.pinfo = <pmix_proc_info_t*> PyMem_Malloc(sizeof(pmix_proc_info_t))
        if not value[0].data.pinfo:
            return PMIX_ERR_NOMEM
        # TODO: verify nspace is copied correctly
        pmix_copy_nspace(value[0].data.pinfo[0].proc.nspace, val['value']['proc']['nspace'])
        if not isinstance(val['value']['proc']['rank'], pmix_int_types):
            print("uint32 value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value']['proc']['rank'] > (65536*65536):
            print("uint32 value is out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.pinfo[0].proc.rank = val['value']['proc']['rank']
        hostname = val['value']['hostname']
        if isinstance(hostname, str):
            pyhostname = hostname.encode('ascii')
        else:
            pyhostname = hostname
        pyhostnameptr = <const char *>(pyhostname)
        value[0].data.pinfo[0].hostname = strdup(pyhostnameptr)
        executable = val['value']['executable']
        if isinstance(executable, str):
            pyexec = executable.encode('ascii')
        else:
            pyexec = executable
        pyexecptr = <const char *>(pyexec)
        value[0].data.pinfo[0].executable_name = strdup(pyexecptr)
        # TODO: check this is a pid type
        value[0].data.pinfo[0].pid = val['value']['pid']
        if not isinstance(val['value']['exitcode'], int):
            print("value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        value[0].data.pinfo[0].exit_code = val['value']['exitcode']
        if not isinstance(val['value']['state'], pmix_int_types):
            print("uint8 value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value']['state'] > 255:
            print("uint8 value is out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.pinfo[0].state = val['value']['state']
    elif val['val_type'] == PMIX_DATA_ARRAY:
        value[0].data.darray = <pmix_data_array_t*> PyMem_Malloc(sizeof(pmix_data_array_t))
        if not value[0].data.darray:
            return PMIX_ERR_NOMEM
        value[0].data.darray[0].type = val['value']['type']
        value[0].data.darray[0].size = len(val['value']['array'])
        try:
            # assume pmix_load_darray does own type checks
            # it should return with an error code inside that
            # function if there is one
            pmix_load_darray(value[0].data.darray, value[0].data.darray[0].type, val['value']['array'])
        except:
            return PMIX_ERR_NOT_SUPPORTED
    elif val['val_type'] == PMIX_ALLOC_DIRECTIVE:
        if not isinstance(val['value'], pmix_int_types):
            print("allocdirective value declared but non-integer provided")
            return PMIX_ERR_TYPE_MISMATCH
        if val['value'] > 255:
            print("allocdirective value is out of bounds")
            return PMIX_ERR_BAD_PARAM
        value[0].data.adir = val['value']
    elif val['val_type'] == PMIX_ENVAR:
        enval = val['value']['envar']
        if isinstance(enval, str):
            pyns = enval.encode('ascii')
        else:
            pyns = enval
        pynsptr = <const char *>(pyns)
        value[0].data.envar.envar = strdup(pynsptr)
        enval = val['value']['value']
        if isinstance(enval, str):
            pyns = enval.encode('ascii')
        else:
            pyns = enval
        pynsptr = <const char *>(pyns)
        value[0].data.envar.value = strdup(pynsptr)
        # TODO: way/function to verify char type
        enval = val['value']['separator']
        if isinstance(enval, pmix_int_types):
            value[0].data.envar.separator = enval
        else:
            value[0].data.envar.separator = ord(enval)
    elif val['val_type'] == PMIX_REGEX:
        regex = val['value']
        ba = pmix_convert_regex(regex)
        if not isinstance(ba, bytearray):
            return PMIX_ERR_TYPE_MISMATCH
        value[0].data.bo.size  = len(val['value'])
        value[0].data.bo.bytes = <char*> PyMem_Malloc(value[0].data.bo.size)
        if not value[0].data.bo.bytes:
            return PMIX_ERR_NOMEM
        pyptr = <char*>ba
        memcpy(value[0].data.bo.bytes, pyptr, value[0].data.bo.size)
    else:
        print("UNRECOGNIZED VALUE TYPE")
        return PMIX_ERR_NOT_SUPPORTED
    return PMIX_SUCCESS

cdef dict pmix_unload_value(const pmix_value_t *value):
    if PMIX_BOOL == value[0].type:
        if value[0].data.flag:
            return {'value':True, 'val_type':PMIX_BOOL}
        else:
            return {'value':False, 'val_type':PMIX_BOOL}
    elif PMIX_BYTE == value[0].type:
        return {'value':value[0].data.byte, 'val_type':PMIX_BYTE}
    elif PMIX_STRING == value[0].type:
        pyb = value[0].data.string
        try:
            pystr = pyb.decode("ascii")
        except:
            pystr = pyb
        return {'value':pystr, 'val_type':PMIX_STRING}
    elif PMIX_SIZE == value[0].type:
        return {'value':value[0].data.size, 'val_type':PMIX_SIZE}
    elif PMIX_PID == value[0].type:
        return {'value':value[0].data.pid, 'val_type':PMIX_PID}
    elif PMIX_INT == value[0].type:
        return {'value':value[0].data.integer, 'val_type':PMIX_INT}
    elif PMIX_INT8 == value[0].type:
        return {'value':value[0].data.int8, 'val_type':PMIX_INT8}
    elif PMIX_INT16 == value[0].type:
        return {'value':value[0].data.int16, 'val_type':PMIX_INT16}
    elif PMIX_INT32 == value[0].type:
        return {'value':value[0].data.int32, 'val_type':PMIX_INT32}
    elif PMIX_INT64 == value[0].type:
        return {'value':value[0].data.int64, 'val_type':PMIX_INT64}
    elif PMIX_UINT == value[0].type:
        return {'value':value[0].data.uint, 'val_type':PMIX_UINT}
    elif PMIX_UINT8 == value[0].type:
        return {'value':value[0].data.uint8, 'val_type':PMIX_UINT8}
    elif PMIX_UINT16 == value[0].type:
        return {'value':value[0].data.uint16, 'val_type':PMIX_UINT16}
    elif PMIX_UINT32 == value[0].type:
        return {'value':value[0].data.uint32, 'val_type':PMIX_UINT32}
    elif PMIX_UINT64 == value[0].type:
        return {'value':value[0].data.uint64, 'val_type':PMIX_UINT64}
    elif PMIX_FLOAT == value[0].type:
        return {'value':value[0].data.fval, 'val_type':PMIX_FLOAT}
    elif PMIX_DOUBLE == value[0].type:
        return {'value':value[0].data.dval, 'val_type':PMIX_DOUBLE}
    elif PMIX_TIMEVAL == value[0].type:
        return {'value':{'sec':value[0].data.tv.tv_sec, 'usec':value[0].data.tv.tv_usec},
        'val_type':PMIX_TIMEVAL}
    elif PMIX_TIME == value[0].type:
        return {'value':value[0].data.time, 'val_type':PMIX_TIME}
    elif PMIX_STATUS == value[0].type:
        return {'value':value[0].data.status, 'val_type':PMIX_STATUS}
    elif PMIX_PROC_RANK == value[0].type:
        return {'value':value[0].data.rank, 'val_type':PMIX_PROC_RANK}
    elif PMIX_PROC == value[0].type:
        pyns = (<bytes>value[0].data.proc[0].nspace).decode('UTF-8')
        return {'value':{'nspace':pyns, 'rank':value[0].data.proc[0].rank}, 'val_type':PMIX_PROC}
    elif PMIX_BYTE_OBJECT == value[0].type:
        mybytes = <bytes>value[0].data.bo.bytes[:value[0].data.bo.size]
        return {'value':{'bytes':mybytes, 'size':value[0].data.bo.size}, 'val_type':PMIX_BYTE_OBJECT}
    elif PMIX_PERSISTENCE == value[0].type:
        return {'value':value[0].data.persist, 'val_type':PMIX_PERSISTENCE}
    elif PMIX_SCOPE == value[0].type:
        return {'value':value[0].data.scope, 'val_type':PMIX_SCOPE}
    elif PMIX_RANGE == value[0].type:
        return {'value':value[0].data.range, 'val_type':PMIX_RANGE}
    elif PMIX_PROC_STATE == value[0].type:
        return {'value':value[0].data.state, 'val_type':PMIX_PROC_STATE}
    elif PMIX_PROC_INFO == value[0].type:
        pins = (<bytes>value[0].data.pinfo[0].proc.nspace).decode('UTF-8')
        pirk = value[0].data.pinfo[0].proc.rank
        pihost = (<bytes>value[0].data.pinfo[0].hostname).decode('UTF-8')
        pexec = (<bytes>value[0].data.pinfo[0].executable_name).decode('UTF-8')
        pipid = value[0].data.pinfo[0].pid
        piex = value[0].data.pinfo[0].exit_code
        pist = value[0].data.pinfo[0].state
        pians = {'proc': {'nspace':pins, 'rank':pirk}, 'hostname': pihost, 'executable': pexec, 'pid': pipid, 'exitcode': piex, 'state': pist}
        return {'value':pians, 'val_type':PMIX_PROC_INFO}
    elif PMIX_DATA_ARRAY == value[0].type:
        try:
            # assume pmix_unload_darray does own type checks
            # it should return with an error code inside that
            # function if there is one
            darray = pmix_unload_darray(value[0].data.darray)
            return {'value':darray, 'val_type':PMIX_DATA_ARRAY}
        except:
            return PMIX_ERR_NOT_SUPPORTED
    elif PMIX_ALLOC_DIRECTIVE == value[0].type:
        return {'value':value[0].data.adir, 'val_type':PMIX_ALLOC_DIRECTIVE}
    elif PMIX_ENVAR == value[0].type:
        pyenv = (<bytes>value[0].data.envar.envar).decode('UTF-8')
        pyval = (<bytes>value[0].data.envar.value).decode('UTF-8')
        pysep = value[0].data.envar.separator
        pyenvans = {'envar': pyenv, 'value': pyval, 'separator': pysep}
        return {'value':pyenvans, 'val_type':PMIX_ENVAR}
    elif PMIX_REGEX == value[0].type:
        return {'value': value[0].data.bo.bytes, 'val_type': PMIX_REGEX}
    else:
        print("Unload_value: provided type is unknown", value[0].type)
        return {'value': None, 'val_type': PMIX_UNDEF}

cdef void pmix_destruct_value(pmix_value_t *value):
    if value[0].type == PMIX_STRING:
        free(value[0].data.string);

cdef void pmix_free_value(self, pmix_value_t *value):
    pmix_destruct_value(value);
    PyMem_Free(value)

# Convert a dictionary of key-value pairs into an
# array of pmix_info_t structs
#
# @array [INPUT]
#        - malloc'd array of pmix_info_t structs
#
# @dicts [INPUT]
#          - a list of dictionaries, where each
#            dictionary has a key, value, and val_type
#            defined as such:
#            [{key:y, value:val, val_type:ty}, … ]
#
cdef int pmix_load_info(pmix_info_t *array, dicts:list):
    n = 0
    for d in dicts:
        pykey = str(d['key'])
        pmix_copy_key(array[n].key, pykey)
        try:
            array[n].flags = d['flags']
        except:
            pass
        val = {'value':d['value'], 'val_type':d['val_type']}
        rc = pmix_load_value(&array[n].value, val)
        if PMIX_SUCCESS != rc:
            return rc
        n += 1
    return PMIX_SUCCESS

# Allocate memory and load pmix info structs
#
# @array [INPUT]
#          - array of pmix_info_t structs
#
# @ninfo [INPUT]
#          - length of the list of dictionaries
#
# @dicts [INPUT]
#          - a list of dictionaries, where each
#            dictionary has a key, value, and val_type
#            defined as such:
#            [{key:y, value:val, val_type:ty}, … ]
#
cdef int pmix_alloc_info(pmix_info_t **info_ptr, size_t *ninfo, dicts:list):
    # Convert any provided dictionary to an array of pmix_info_t
    if dicts is not None:
        ninfo[0] = len(dicts)
        if 0 < ninfo[0]:
            info_ptr[0] = <pmix_info_t*>malloc(ninfo[0] * sizeof(pmix_info_t))
            if not info_ptr[0]:
                return PMIX_ERR_NOMEM
            rc = pmix_load_info(info_ptr[0], dicts)
            if PMIX_SUCCESS != rc:
                pmix_free_info(info_ptr[0], ninfo[0])
                return rc
        else:
            info_ptr[0] = NULL
            ninfo[0] = 0
    else:
        info_ptr[0] = NULL
        ninfo[0] = 0
    return PMIX_SUCCESS

cdef int pmix_unload_info(const pmix_info_t *info, size_t ninfo, ilist:list):
    cdef char* kystr
    cdef size_t n
    n = 0
    while n < ninfo:
        kystr = strdup(info[n].key)
        # pmix_unload_value returns a python dict of val, val_type
        val = pmix_unload_value(&info[n].value)
        if val['val_type'] == PMIX_UNDEF:
            return PMIX_ERR_NOT_SUPPORTED
        d     = {}
        pykey = str(kystr.decode("ascii"))
        free(kystr)
        d['key']      = pykey
        # TODO: don't know how to decide if flags was defined or not??
        d['flags']    = info[n].flags
        d['value']    = val['value']
        d['val_type'] = val['val_type']
        ilist.append(d)
        n += 1
    return PMIX_SUCCESS

cdef void pmix_destruct_info(pmix_info_t *info):
    pmix_destruct_value(&info[0].value)

# Free a malloc'd array of pmix_info_t structures
#
# @array [INPUT]
#        - array of pmix_info_t to be free'd
#
# @sz [INPUT]
#     - number of elements in array
cdef void pmix_free_info(pmix_info_t *array, size_t sz):
    n = 0
    while n < sz:
        pmix_destruct_info(&array[n])
        n += 1
    free(array)

# Convert a dictionary of key-value pairs into an
# array of pmix_pdata_t structs
#
# @array [INPUT]
#        - malloc'd array of pmix_pdata_t structs
#
# @pdata [INPUT]
#          - a list of dictionaries, where each
#            dictionary has a key, value, val_type,
#            and proc keys
# @proc [INPUT]
#          - a dictionary with nspace, rank as keys
cdef int pmix_load_pdata(proc:dict, pmix_pdata_t *array, data:list):
    n = 0
    for d in data:
        pykey = str(d['key'])
        pmix_copy_key(array[n].key, pykey)
        val = {'value':d['value'], 'val_type':d['val_type']}
        rc = pmix_load_value(&array[n].value, val)
        array[n].proc.rank = proc['rank']
        pmix_copy_nspace(array[n].proc.nspace, proc['nspace'])
        if PMIX_SUCCESS != rc:
            return rc
        n += 1
    return PMIX_SUCCESS

cdef int pmix_unload_pdata(const pmix_pdata_t *pdata, size_t npdata, ilist:list):
    cdef char* kystr
    cdef size_t n = 0
    while n < npdata:
        val = pmix_unload_value(&pdata[n].value)
        if val['val_type'] == PMIX_UNDEF:
            return PMIX_ERR_NOT_SUPPORTED
        d     = {}
        kystr = strdup(pdata[n].key)
        pykey = kystr.decode("ascii")
        free(kystr)
        d['key']      = pykey
        kystr = strdup(pdata[n].proc.nspace)
        myns = kystr.decode('ascii')
        free(kystr)
        proc = {'nspace':myns, 'rank': pdata[n].proc.rank}
        d['proc']     = proc
        d['value']    = val['value']
        d['val_type'] = val['val_type']
        ilist.append(d)
        n += 1
    return PMIX_SUCCESS

cdef void pmix_destruct_pdata(pmix_pdata_t *pdata):
    pmix_destruct_value(&pdata[0].value)

# Free a malloc'd array of pmix_pdata_t structures
#
# @array [INPUT]
#        - array of pmix_pdata_t to be free'd
#
# @sz [INPUT]
#     - number of elements in array
cdef void pmix_free_pdata(pmix_pdata_t *array, size_t sz):
    n = 0
    while n < sz:
        pmix_destruct_pdata(&array[n])
        n += 1
    PyMem_Free(array)

cdef int pmix_unload_queries(const pmix_query_t *queries, size_t nqueries, ilist:list):
    cdef char* kystr
    cdef size_t n = 0
    keylist = []
    qualist = []
    query = {}
    while n < nqueries:
        rc = pmix_unload_argv(queries[n].keys, keylist)
        pmix_unload_info(queries[n].qualifiers, queries[n].nqual, qualist)
        query['keys']       = keylist
        query['qualifiers'] = qualist
        ilist.append(query)
        n += 1
    return PMIX_SUCCESS


# Free a malloc'd array of pmix_query_t structs to free
#
# @array [INPUT]
#        - pmix_query_t queries to be free'd
#
# @sz [INPUT]
#     - number of elements in array
cdef void pmix_free_queries(pmix_query_t *queries, size_t sz):
    n = 0
    while n < sz:
        if queries[n].keys != NULL:
            j = 0
            while NULL != queries[n].keys[j]:
                PyMem_Free(queries[n].keys[j])
                j += 1
            PyMem_Free(queries[n].keys)
        if queries[n].qualifiers != NULL:
            pmix_free_info(queries[n].qualifiers, queries[n].nqual)
        n += 1
    if queries != NULL:
        PyMem_Free(queries)

# Convert a list of (nspace, rank) tuples into an
# array of pmix_proc_t structs
#
# @proc [INPUT]
#       - malloc'd array of pmix_proc_t structs
#
# @peers [INPUT]
#       - list of (nspace,rank) tuples
#
cdef int pmix_load_procs(pmix_proc_t *proc, peers:list):
    n = 0
    for p in peers:
        pmix_copy_nspace(proc[n].nspace, p['nspace'])
        proc[n].rank = p['rank']
        n += 1
    return PMIX_SUCCESS

cdef int pmix_unload_procs(const pmix_proc_t *procs, size_t nprocs, peers:list):
    cdef char* kystr
    n = 0
    while n < nprocs:
        kystr = strdup(procs[n].nspace)
        myns = kystr.decode('ascii')
        free(kystr)
        peers.append({'nspace':myns, 'rank':procs[n].rank})
        n += 1
    return PMIX_SUCCESS

# Free a malloc'd array of pmix_proc_t structures
#
# @array [INPUT]
#        - array of pmix_proc_t to be free'd
#
# @sz [INPUT]
#     - number of elements in array
#
cdef void pmix_free_procs(pmix_proc_t *array, size_t sz):
    PyMem_Free(array)

cdef void pmix_unload_bytes(char *data, size_t ndata, blist:list):
    cdef size_t n = 0
    while n < ndata:
        blist.append(data[n])
        n += 1

cdef void pmix_free_apps(pmix_app_t *array, size_t sz):
    n = 0
    while n < sz:
        free(array[n].cmd)
        # need to free the argv and env arrays
        free(array[n].cwd)
        if 0 < array[n].ninfo:
            pmix_free_info(array[n].info, array[n].ninfo)
        n += 1

cdef void pmix_unload_apps(const pmix_app_t *apps, size_t napps, pyapps:list):
    cdef size_t n = 0
    while n < napps:
        myapp = {}
        myapp['cmd'] = str(apps[n].cmd)
        myargv = []
        if NULL != apps[n].argv:
            pmix_unload_argv(apps[n].argv, myargv)
            myapp['argv'] = myargv
        myenv = []
        if NULL != apps[n].env:
            pmix_unload_argv(apps[n].env, myenv)
            myapp['env'] = myenv
        myapp['maxprocs'] = apps[n].maxprocs
        keyvals = {}
        if NULL != apps[n].info:
            pmix_unload_info(apps[n].info, apps[n].ninfo, keyvals)
            myapp['info'] = keyvals
        pyapps.append(myapp)
        n += 1

cdef int pmix_load_apps(pmix_app_t *apps, pyapps:list):
    cdef size_t m
    cdef size_t n
    cdef char** argv
    n = 0
    for p in pyapps:
        pycmd = str(p['cmd']).encode('ascii')
        try:
            apps[n].cmd = strdup(pycmd)
        except:
            return PMIX_ERR_TYPE_MISMATCH

        try:
            if p['argv'] is not None:
                m = len(p['argv']) + 1
            else:
                m = 2
            argv = <char**> malloc(m * sizeof(char*))
            if not argv:
                return PMIX_ERR_NOMEM
            memset(argv, 0, m)
            if p['argv'] is not None:
                pmix_load_argv(argv, p['argv'])
            else:
                pmix_load_argv(argv, [p['cmd']])
        except:
            argv = <char**> malloc(2 * sizeof(char*))
            memset(argv, 0, 2)
            rc = pmix_load_argv(argv, [p['cmd']])
            if PMIX_SUCCESS != rc:
                free(argv)
                return rc
        apps[n].argv = argv
        apps[n].env = NULL
        try:
            if p['env'] is not None:
                m = len(p['env']) + 1
                env = <char**> malloc(m * sizeof(char*))
                if not env:
                    return PMIX_ERR_NOMEM
                memset(env, 0, m)
                pmix_load_argv(env, p['env'])
                apps[n].env = env
        except:
            pass

        try:
            pycwd = str(p['cwd']).encode('ascii')
            apps[n].cwd = strdup(pycwd)
        except:
            pycwd = os.getcwd()
            pycwd = pycwd.encode('ascii')
            apps[n].cwd = strdup(pycwd)

        apps[n].info = NULL
        apps[n].ninfo = 0
        try:
            if p['info'] is not None:
                apps[n].ninfo = len(p['info'])
                apps[n].info =  <pmix_info_t*> malloc(apps[n].ninfo * sizeof(pmix_info_t))
                if not apps[n].info:
                    return PMIX_ERR_NOMEM
                rc = pmix_load_info(apps[n].info, p['info'])
                if PMIX_SUCCESS != rc:
                    return rc
        except:
            pass

        apps[n].maxprocs = 1
        try:
            apps[n].maxprocs = p['maxprocs']
        except:
            pass

        n += 1
    return PMIX_SUCCESS
