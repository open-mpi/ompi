# test cython code
#file: cython_test_functions.pyx

# TODO: add lots more types to load/test

pyinfo = [{'key': PMIX_EVENT_HDLR_NAME, 'value': 'SIMPCLIENT-MODEL',
           'val_type': PMIX_STRING}]
darray = {'type':PMIX_INFO, 'array':[{'key':PMIX_ALLOC_FABRIC_ID, 'value':'SIMPSCHED.net', 'val_type':PMIX_STRING},
                                     {'key':PMIX_ALLOC_FABRIC_SEC_KEY, 'value':'T', 'val_type':PMIX_BOOL},
                                     {'key':PMIX_SETUP_APP_ENVARS, 'value':'T', 'val_type':PMIX_BOOL}]
         }
# test putting PMIX_DATA_ARRAY into a pmix info data array
darray2 = {'type':PMIX_INFO, 'array':[{'key':PMIX_ALLOC_FABRIC_ID, 'value':'SIMPSCHED.net', 'val_type':PMIX_STRING},
                                     {'key':PMIX_ALLOC_FABRIC_SEC_KEY, 'value':'T', 'val_type':PMIX_BOOL},
                                     {'key':'foo', 'value':darray, 'val_type':PMIX_DATA_ARRAY}]
         }
str_darray = {'type': PMIX_STRING, 'array':['abc', 'def', 'efg']}
size_darray = {'type': PMIX_SIZE, 'array':[45, 46, 47]}
pid_darray = {'type': PMIX_SIZE, 'array':[3, 4, 5]}
timeval_darray = {'type': PMIX_TIMEVAL, 'array':[{'sec':2, 'usec':3}, {'sec':1, 'usec':2}]}
bool_darray = {'type': PMIX_BOOL, 'array':['False', 'True']}
byte_darray = {'type': PMIX_BYTE, 'array':[1, 1]}
proc_darray = {'type': PMIX_PROC, 'array':[{'nspace':'testnspace', 'rank':0}, {'nspace':'newnspace', 'rank':1}]}
bo_darray = {'type': PMIX_BYTE_OBJECT, 'array':[{'bytes':bytearray(1), 'size':1}, {'bytes':bytearray(1), 'size':1}]}
proc_info_darray = {'type': PMIX_PROC_INFO, 'array':[{'proc': {'nspace': 'fakenspace', 'rank': 0}, 'hostname': 'myhostname', 'executable': 'testexec', 'pid': 2, 'exitcode': 0, 'state': 0}]}
env_darray = {'type': PMIX_ENVAR, 'array': [{'envar': 'TEST_ENVAR', 'value': 'TEST_VAL', 'separator': ':'}]}
pyregex = 'pmix[test[3:0-2]]'.encode('ascii')
values = [{'value': 'True', 'val_type': PMIX_BOOL},
          {'value': 1, 'val_type': PMIX_BYTE},
          {'value': "foo", 'val_type': PMIX_STRING},
          {'value': 45, 'val_type': PMIX_SIZE},
          {'value': 3, 'val_type': PMIX_PID},
          {'value': 11, 'val_type': PMIX_INT},
          {'value': 2, 'val_type': PMIX_INT8},
          {'value': 127, 'val_type': PMIX_INT16},
          {'value': 100, 'val_type': PMIX_INT32},
          {'value': 500, 'val_type': PMIX_INT64},
          {'value': 250, 'val_type': PMIX_UINT},
          {'value': 201, 'val_type': PMIX_UINT8},
          {'value': 700, 'val_type': PMIX_UINT16},
          {'value': 301, 'val_type': PMIX_UINT32},
          {'value': 301, 'val_type': PMIX_UINT64},
          {'value': 301.1, 'val_type': PMIX_FLOAT},
          {'value': 201.2, 'val_type': PMIX_DOUBLE},
          {'value': {'sec': 2, 'usec': 3}, 'val_type': PMIX_TIMEVAL},
          {'value': 100, 'val_type': PMIX_TIME},
          {'value': 34, 'val_type': PMIX_STATUS},
          {'value': 15, 'val_type': PMIX_PROC_RANK},
          {'value': {'nspace': 'testnspace', 'rank': 0}, 'val_type': PMIX_PROC},
          {'value': {'bytes': bytearray(1), 'size': 1}, 'val_type': PMIX_BYTE_OBJECT},
          {'value': 18, 'val_type': PMIX_PERSISTENCE},
          {'value': 1, 'val_type': PMIX_SCOPE},
          {'value': 5, 'val_type': PMIX_RANGE},
          {'value': 45, 'val_type': PMIX_PROC_STATE},
          {'value': {'proc': {'nspace': 'fakenspace', 'rank': 0}, 'hostname': 'myhostname', 'executable': 'testexec', 'pid': 2, 'exitcode': 0, 'state': 0}, 'val_type': PMIX_PROC_INFO},
          {'value': darray, 'val_type': PMIX_DATA_ARRAY},
          {'value': darray2, 'val_type': PMIX_DATA_ARRAY},
          {'value': str_darray, 'val_type': PMIX_DATA_ARRAY},
          {'value': size_darray, 'val_type': PMIX_DATA_ARRAY},
          {'value': pid_darray, 'val_type': PMIX_DATA_ARRAY},
          {'value': 19, 'val_type': PMIX_ALLOC_DIRECTIVE},
          {'value': {'envar': 'TEST_ENVAR', 'value': 'TEST_VAL', 'separator':
                     ':'}, 'val_type': PMIX_ENVAR},
          {'value': pyregex, 'val_type': PMIX_REGEX},
          {'value': timeval_darray, 'val_type': PMIX_DATA_ARRAY},
          {'value': bool_darray, 'val_type': PMIX_DATA_ARRAY},
          {'value': byte_darray, 'val_type': PMIX_DATA_ARRAY},
          {'value': proc_darray, 'val_type': PMIX_DATA_ARRAY},
          {'value': bo_darray, 'val_type': PMIX_DATA_ARRAY},
          {'value': proc_info_darray, 'val_type': PMIX_DATA_ARRAY},
          {'value': env_darray, 'val_type': PMIX_DATA_ARRAY}
        ]

def error_string(pystat:int):
    cdef char *string
    string = <char*>PMIx_Error_string(pystat)
    pystr = string
    val = pystr.decode('ascii')
    return val

def test_load_value(unload:bool):
    global values
    cdef pmix_value_t value
    for val in values:
        rc = pmix_load_value(&value, val)
        if rc == PMIX_SUCCESS:
            print("LOAD SUCCESS -- VALUE DICT: \t" + str(val) + "\n")
        if unload and rc == PMIX_SUCCESS:
            pydict = pmix_unload_value(&value)
            print("UNLOAD SUCCESS -- VALUE CONVERTED: \t" + str(pydict) + "\n")

def test_alloc_info():
    global pyinfo, values
    cdef pmix_info_t *info
    cdef pmix_info_t **info_ptr
    cdef size_t ninfo

    # call pmix_alloc_info
    info_ptr = &info
    rc = pmix_alloc_info(info_ptr, &ninfo, pyinfo)
    if rc == PMIX_SUCCESS:
        print("SUCCESSFULLY LOADED:\n")
        for i in pyinfo:
            print("\t" + str(i['key']) + ", " + str(i['value']) + ", " +
                  str(PMIx_Data_type_string(i['val_type'])) + "\n")
