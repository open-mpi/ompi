#!/usr/bin/env python3

import time, sys
from pmix import *

test_count = 0
test_fails = 0
test_complete = 0

def test_put(client, scope, ky, val):
    print("PUT")
    global test_count, test_fails
    test_count = test_count + 1
    rc = client.put(PMIX_GLOBAL, "mykey", {'value':1, 'val_type':PMIX_INT32})
    if rc != 0 and rc != -47:
        test_fails = test_fails + 1
        print("PUT TEST FAILED: ", client.error_string(rc))

def test_commit(client):
    print("COMMIT")
    global test_count, test_fails
    test_count = test_count + 1
    rc = client.commit()
    if rc != 0 and rc != -47:
        test_fails = test_fails + 1
        print("COMMIT TEST FAILED: ", client.error_string(rc))

def test_fence(client, procs, info):
    print("FENCE")
    global test_count, test_fails
    test_count = test_count + 1
    rc = client.fence(procs, info)
    # don't count 'not supported' as a test fail
    if rc != 0 and rc != -47:
        test_fails = test_fails + 1
        print("FENCE TEST FAILED: ", client.error_string(rc))
    if rc == -47:
        print("FENCE call " + client.error_string(rc))

def test_get(client, proc, ky, dicts):
    print("GET")
    global test_count, test_fails
    test_count = test_count + 1
    rc, get_val = client.get(proc, ky, dicts)
    if rc != 0 and rc != -47:
        test_fails = test_fails + 1
        print("GET TEST FAILED: ", client.error_string(rc))
    print("GET_VAL RETURNED: ", get_val)

def test_publish(client, dicts):
    print("PUBLISH")
    global test_count, test_fails
    test_count = test_count + 1
    rc = client.publish(dicts)
    if rc != 0 and rc != -47:
        test_fails = test_fails + 1
        print("PUBLISH TEST FAILED: ", client.error_string(rc))

def test_lookup(client, data, dicts):
    print("LOOKUP")
    global test_count, test_fails
    test_count = test_count + 1
    rc, pdata = client.lookup(data, dicts)
    if rc != 0 and rc != -47:
        test_fails = test_fails + 1
        print("LOOKUP TEST FAILED: ", client.error_string(rc))
    print("PDATA RETURNED: ", pdata)

def test_unpublish(client, pykeys, dicts):
    print("UNPUBLISH")
    global test_count, test_fails
    test_count = test_count + 1
    rc = client.unpublish(pykeys, dicts)
    if rc != 0 and rc != -47:
        test_fails = test_fails + 1
        print("UNPUBLISH TEST FAILED: ", client.error_string(rc))

def test_query(client, pyqueries):
    print("QUERY")
    global test_count, test_fails
    test_count = test_count + 1
    rc, pyresults = client.query(pyqueries)
    if rc != 0 and rc != -47:
        test_fails = test_fails + 1
        print("QUERY TEST FAILED: ", client.error_string(rc))
    print("QUERY INFO RETURNED: ", pyresults)

def test_pyhandler(evid:int, st:int, pysource:dict, pyinfo:list, pyresults:list):
    global test_complete
    print("PYHANDLER", file=sys.stderr)
    status = PMIX_EVENT_ACTION_COMPLETE
    results = [{'key':'eventkey', 'value':'testevent', 'val_type':PMIX_STRING}]
    print("PYHANDLER RETURNED: ", pyinfo)
    test_complete = 1
    return status, results

def test_register_event_handler(client, pycodes, info, test_pyhandler):
    print("REGISTER EVENT HANDLER", file=sys.stderr)
    global test_count, test_fails
    test_count = test_count + 1
    rc, hdlr = client.register_event_handler(pycodes, info, test_pyhandler)
    if rc != 0 and rc != -47:
        test_fails = test_fails + 1
        print("REGISTER EVENT HANDLER TEST FAILED: ", client.error_string(rc))

def main():
    global test_complete
    foo = PMIxClient()
    print("Testing PMIx ", foo.get_version())
    info = [{'key':PMIX_PROGRAMMING_MODEL, 'flags': 0, 'value':'TEST', 'val_type':PMIX_STRING},
            {'key':PMIX_MODEL_LIBRARY_NAME, 'flags': 0, 'value':'PMIX', 'val_type':PMIX_STRING}]
    my_result,myname = foo.init(info)
    print("Init result ", my_result, file=sys.stderr)
    if 0 != my_result:
        print("FAILED TO INIT")
        exit(1)

    # put value into keystore
    test_put(foo, PMIX_GLOBAL, "mykey", {'value':1, 'val_type':PMIX_INT32})

    # commit it
    test_commit(foo)

    # execute fence
    procs = []
    info = []
    test_fence(foo, procs, info)

    info = []
    test_get(foo, {'nspace':"testnspace", 'rank': 0}, "mykey", info)

    # test a fence that should return not_supported because
    # we pass a required attribute that doesn't exist
    procs = []
    info = [{'key': 'ARBITRARY', 'flags': PMIX_INFO_REQD, 'value':10, 'val_type':PMIX_INT}]
    rc = test_fence(foo, procs, info)

    info = [{'key': 'ARBITRARY', 'flags': None, 'value':10, 'val_type':PMIX_INT}]
    test_publish(foo, info)

    test_fence(foo, procs, info)

    pdata_key = [{'key':'ARBITRARY'}]
    test_lookup(foo, pdata_key, None)

    pykeys = ['ARBITRARY']
    info = []
    test_unpublish(foo, pykeys, info)

    test_fence(foo, procs, info)

    pyq = [{'keys':[PMIX_QUERY_PSET_NAMES], 'qualifiers':[]}]
    test_query(foo, pyq)

    pycodes = []
    code = PMIX_MODEL_DECLARED
    pycodes.append(code)
    info = [{'key': PMIX_EVENT_HDLR_NAME, 'value': 'SIMPCLIENT-MODEL', 'val_type': PMIX_STRING}]
    test_register_event_handler(foo, pycodes, info, test_pyhandler)

    loopcount = 0
    while test_complete != 1 and loopcount < 3:
        time.sleep(1)
        loopcount = loopcount + 1

    if test_complete == 0:
        print("MODEL EVENT FAILED TO ARRIVE", file=sys.stderr)

    # finalize
    info = []
    foo.finalize(info)
    print("Client finalize complete")

    # count client wrapper tests that returned with no error
    total_passed = test_count - test_fails
    print(str(total_passed) + " / " + str(test_count) + " TESTS PASSED")
if __name__ == '__main__':
    main()
