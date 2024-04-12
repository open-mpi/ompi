#!/usr/bin/env python3

from pmix import *
import time
import threading

termEvent = threading.Event()

def default_evhandler(evhdlr:int, status:int,
                      source:dict, info:list, results:list):
    print("DEFAULT HANDLER")
    return PMIX_EVENT_ACTION_COMPLETE,None

def model_evhandler(evhdlr:int, status:int,
                    source:dict, info:list, results:list):
    global termEvent

    print("MODEL HANDLER")
    termEvent.set()
    return PMIX_EVENT_ACTION_COMPLETE,None

def main():
    global termEvent
    
    foo = PMIxClient()
    print("Testing PMIx ", foo.get_version())
    info = [{'key':PMIX_PROGRAMMING_MODEL, 'value':'TEST', 'val_type':PMIX_STRING},
            {'key':PMIX_MODEL_LIBRARY_NAME, 'value':'PMIX', 'val_type':PMIX_STRING}]
    my_result = foo.init(info)
    print("Init result ", my_result)
    if 0 != my_result[0]:
        print("FAILED TO INIT")
        exit(1)
    # register an event handler
    rc,myevhndlr = foo.register_event_handler(None, None, default_evhandler)
    print("REGISTER DEFAULT", foo.error_string(rc))
    # register the model handler
    rc,mymodelhndlr = foo.register_event_handler([PMIX_MODEL_DECLARED], None, model_evhandler)
    print("REGISTER MODEL", foo.error_string(rc))

    # try putting something
    print("PUT")
    rc = foo.put(PMIX_GLOBAL, "mykey", {'value':1, 'val_type':PMIX_INT32})
    print("Put result ",foo.error_string(rc));
    # commit it
    print("COMMIT")
    rc = foo.commit()
    print ("Commit result ", foo.error_string(rc))
    # execute fence
    print("FENCE")
    procs = []
    info = []
    rc = foo.fence(procs, info)
    print("Fence result ", foo.error_string(rc))
    print("GET")
    info = []
    rc, get_val = foo.get({'nspace':"testnspace", 'rank': 0}, "mykey", info)
    print("Get result: ", foo.error_string(rc))
    print("Get value returned: ", get_val)
    # test a fence that should return not_supported because
    # we pass a required attribute that doesn't exist
    procs = []
    info = [{'key': 'ARBITRARY', 'flags': PMIX_INFO_REQD, 'value':10, 'val_type':PMIX_INT}]
    rc = foo.fence(procs, info)
    print("Fence should be not supported:", foo.error_string(rc))
    # wait for model event
    while not termEvent.is_set():
        time.sleep(0.001)
    # finalize
    info = []
    foo.finalize(info)
    print("Client finalize complete")
if __name__ == '__main__':
    main()
