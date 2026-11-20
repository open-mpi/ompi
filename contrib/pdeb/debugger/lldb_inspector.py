#
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
"""Open MPI state inspectors for the pdeb lldb back end.

The gdb back end implements btlsm_dump, req_dump and pml_dump as a gdb-python
prelude.  This module is the lldb equivalent, and deliberately produces the
same output so a dump can be read the same way whichever debugger pdeb picked.

lldb is imported by the debugger itself; it is not importable standalone.
"""

import shlex

import lldb

SM_FIFO_FREE = -2
FLAG_SINGLE_COPY = 1
FLAG_COMPLETE = 2
FLAG_SETUP_FBOX = 4

REQUEST_COMPLETED = 1
REQ_STATE = {0: 'INVALID', 1: 'INACTIVE', 2: 'ACTIVE', 3: 'CANCELLED'}

# mca_pml_base_request_type_t (pml_base_request_dbg.h), used by OB1.
MCA_PML_REQUEST = {0: 'NULL', 1: 'SEND', 2: 'RECV', 3: 'IPROBE', 4: 'PROBE',
                   5: 'IMPROBE', 6: 'MPROBE'}

# mca_pml_cm_request_type_t (pml_cm_request.h) and the concrete CM request
# struct for each value: (type name, is_heavy, is_send).
CM_REQ_TYPE = {0: 'SEND_HEAVY', 1: 'SEND_THIN', 2: 'RECV_HEAVY',
               3: 'RECV_THIN', 4: 'NULL'}
CM_SUBTYPE = {0: ('mca_pml_cm_hvy_send_request_t', True, True),
              1: ('mca_pml_cm_thin_send_request_t', False, True),
              2: ('mca_pml_cm_hvy_recv_request_t', True, False),
              3: ('mca_pml_cm_thin_recv_request_t', False, False)}

SEPARATOR = '============================================================'

# Types whose absence has already been reported, so a dump that touches the
# same missing type repeatedly complains once rather than on every line.
_reported_missing = set()


class Out(object):
    """Collect dump text for one command invocation.

    lldb prints whatever a command leaves in its SBCommandReturnObject, so
    accumulate there instead of using print(): output then stays attached to
    the command, which matters because pdeb reads the per-PID lldb log.
    """

    def __init__(self, result):
        self._result = result

    def __call__(self, fmt='', *args):
        self._result.AppendMessage(fmt % args if args else fmt)


def _type_byte_size(sbtype):
    try:
        return sbtype.GetByteSize()
    except Exception:
        return 0


def _find_type(target, name):
    """Look up a type by name, or None when the target has no debug info.

    A build without -g leaves lldb with symbol tables only: globals come back
    typed as void * and no struct layout is known.  Returning None lets each
    caller explain that rather than emit a confusing member-access error.
    """
    sbtype = target.FindFirstType(name)
    if sbtype.IsValid() and 0 != _type_byte_size(sbtype):
        return sbtype
    # Some types are only present in one module's debug info.
    for module in target.module_iter():
        sbtype = module.FindFirstType(name)
        if sbtype.IsValid() and 0 != _type_byte_size(sbtype):
            return sbtype
    return None


def _missing_type(out, name, what):
    if name not in _reported_missing:
        _reported_missing.add(name)
        out('  <no debug information for %s; %s needs a build with -g '
            '(or a .dSYM alongside the library)>', name, what)


def _symbol_address(target, name):
    contexts = target.FindSymbols(name)
    for i in range(contexts.GetSize()):
        symbol = contexts.GetContextAtIndex(i).GetSymbol()
        if symbol.IsValid():
            address = symbol.GetStartAddress()
            if address.IsValid():
                return address
    return None


def _global(target, name, typename, pointer=False):
    """Fetch a global variable as a typed value.

    FindFirstGlobalVariable() alone is not enough: without debug info for the
    defining compilation unit lldb still finds the symbol but types it void *,
    and every member access then fails.  Fall back to pairing the symbol's
    address with an explicitly looked-up type.  Pass pointer=True when the
    variable is itself a pointer to typename, since the fallback has to build
    the right type rather than read the pointee in place.
    """
    value = target.FindFirstGlobalVariable(name)
    if value.IsValid():
        value_type = value.GetType()
        type_name = value_type.GetName() or ''
        if value_type.IsValid() and not type_name.startswith('void'):
            return value

    sbtype = _find_type(target, typename)
    if sbtype is None:
        return None
    if pointer:
        sbtype = sbtype.GetPointerType()
    address = _symbol_address(target, name)
    if address is None:
        return None
    value = target.CreateValueFromAddress(name, address, sbtype)
    return value if value.IsValid() else None


def _member(value, path):
    """Follow a dotted member path, dereferencing pointers as needed."""
    current = value
    for part in path.split('.'):
        if current is None or not current.IsValid():
            return None
        child = current.GetChildMemberWithName(part)
        if not child.IsValid() and current.GetType().IsPointerType():
            child = current.Dereference().GetChildMemberWithName(part)
        current = child
    return current if (current is not None and current.IsValid()) else None


def _int(value, default=None):
    if value is None or not value.IsValid():
        return default
    result = value.GetValueAsSigned()
    if value.GetError().Fail():
        return default
    return result


def _uint(value, default=None):
    if value is None or not value.IsValid():
        return default
    result = value.GetValueAsUnsigned()
    if value.GetError().Fail():
        return default
    return result


def _field_str(value, path):
    """Render a member for display, mirroring the gdb back end's <path?>."""
    member = _member(value, path)
    if member is None:
        return '<%s?>' % path
    text = member.GetValue()
    if text is None:
        text = member.GetSummary()
    if text is None:
        return '<%s?>' % path
    return text


def _enum_str(value, path):
    """Render an enum member, preferring the enumerator name."""
    member = _member(value, path)
    if member is None:
        return '<%s?>' % path
    return member.GetValue() or str(_int(member, '?'))


def _list_length(value):
    length = _int(_member(value, 'opal_list_length'))
    return -1 if length is None else length


def _array_at(target, pointer, index, element_type):
    """Index a pointer as an array.

    SBValue indexing of a plain pointer is unreliable, so do the arithmetic
    and build the element from its address.
    """
    base = _uint(pointer)
    size = _type_byte_size(element_type)
    if base is None or 0 == size:
        return None
    address = lldb.SBAddress(base + index * size, target)
    value = target.CreateValueFromAddress('[%d]' % index, address,
                                          element_type)
    return value if value.IsValid() else None


def _ep_array(target, component):
    """Return the endpoint array and the name of the member it came from.

    endpoints stays NULL until every local peer's segment is mapped; the
    setup paths use endpoints_storage until then.  A job hung in wire-up is
    precisely when that matters, so fall back rather than report nothing.
    """
    endpoints = _member(component, 'endpoints')
    if endpoints is not None and 0 == (_uint(endpoints) or 0):
        storage = _member(component, 'endpoints_storage')
        if storage is not None:
            # Older targets have a single array and no storage member.
            return storage, 'endpoints_storage'
    return endpoints, 'endpoints'


def _offset_bits(target):
    """Width of the offset field inside an sm fifo_value_t."""
    sbtype = _find_type(target, 'fifo_value_t')
    if sbtype is not None and 8 == _type_byte_size(sbtype):
        return 32, 0xffffffff
    if sbtype is None:
        return 32, 0xffffffff
    return 24, 0x00ffffff


def _flag_str(flags):
    parts = []
    if flags & FLAG_SINGLE_COPY:
        parts.append('SINGLE_COPY')
    if flags & FLAG_COMPLETE:
        parts.append('COMPLETE')
    if flags & FLAG_SETUP_FBOX:
        parts.append('SETUP_FBOX')
    if 0 == flags:
        parts.append('INLINE/0')
    return '|'.join(parts) + ' (0x%x)' % flags


def _find_waitall_frame(process):
    """Locate an ompi_request_default_wait_all frame on any thread.

    The selected thread is checked first; the rest are searched afterwards so
    a hybrid OpenMP+MPI process is still covered.
    """
    selected = process.GetSelectedThread()
    threads = [selected] if selected.IsValid() else []
    for thread in process:
        if not selected.IsValid() or thread.GetThreadID() != \
                selected.GetThreadID():
            threads.append(thread)
    for thread in threads:
        for frame in thread:
            if 'ompi_request_default_wait_all' in (frame.GetFunctionName()
                                                   or ''):
                return frame
    return None


def _requests_from_args(target, out, label, args):
    """Evaluate explicit <requests_ptr> <count> arguments."""
    request_type = _find_type(target, 'ompi_request_t')
    if request_type is None:
        _missing_type(out, 'ompi_request_t', label)
        return None, None
    frame = target.GetProcess().GetSelectedThread().GetSelectedFrame()
    pointer = frame.EvaluateExpression(args[0])
    if not pointer.IsValid() or pointer.GetError().Fail():
        out('%s: cannot evaluate %s', label, args[0])
        return None, None
    count = frame.EvaluateExpression(args[1])
    if not count.IsValid() or count.GetError().Fail():
        out('%s: cannot evaluate %s', label, args[1])
        return None, None
    return pointer, _int(count, 0)


def _requests_from_waitall(target, out, label):
    """Read requests/count out of the active wait_all frame."""
    frame = _find_waitall_frame(target.GetProcess())
    if frame is None:
        out('%s: no ompi_request_default_wait_all frame; '
            'pass: %s <requests_ptr> <count>', label, label)
        return None, None
    requests = frame.FindVariable('requests')
    count = frame.FindVariable('count')
    if not requests.IsValid() or not count.IsValid():
        out('%s: wait_all frame carries no requests/count locals; '
            'an optimized build may need: %s <requests_ptr> <count>',
            label, label)
        return None, None
    return requests, _int(count, 0)


def _resolve_requests(target, out, label, args):
    if 2 == len(args):
        return _requests_from_args(target, out, label, args)
    if args:
        out('%s accepts no arguments or exactly: <requests_ptr> <count>',
            label)
        return None, None
    return _requests_from_waitall(target, out, label)


def _request_at(target, requests, index):
    """Return the index'th ompi_request_t * from a request array."""
    request_type = _find_type(target, 'ompi_request_t')
    if request_type is None:
        return None
    return _array_at(target, requests, index, request_type.GetPointerType())


#
# btlsm_dump
#

def _btlsm_fifo(target, out, component):
    fifo = _member(component, 'my_fifo')
    if fifo is None:
        out('  <FIFO unavailable>')
        return
    head = _int(_member(fifo, 'fifo_head'))
    tail = _int(_member(fifo, 'fifo_tail'))
    if head is None or tail is None:
        out('  <FIFO unavailable>')
        return
    out('my_fifo: head=%d tail=%d fbox_available=%s',
        head, tail, _field_str(fifo, 'fbox_available'))

    out('--- incoming FIFO contents (what THIS rank has not yet consumed) ---')
    if SM_FIFO_FREE == head:
        out('  <empty>')
        return

    header_type = _find_type(target, 'mca_btl_sm_hdr_t')
    if header_type is None:
        _missing_type(out, 'mca_btl_sm_hdr_t', 'btlsm_dump')
        return
    endpoints, _ = _ep_array(target, component)
    offset_bits, offset_mask = _offset_bits(target)
    count = 0
    current = head
    while SM_FIFO_FREE != current and count < 200:
        owner = (current >> offset_bits) & 0xffff
        endpoint = _array_at(target, endpoints, owner,
                             endpoints.GetType().GetPointeeType()) \
            if endpoints is not None else None
        segment_base = _uint(_member(endpoint, 'segment_base')) \
            if endpoint is not None else None
        if segment_base is None:
            out('  [%3d] <decode error: endpoint %d segment_base unavailable>',
                count, owner)
            break
        address = lldb.SBAddress((current & offset_mask) + segment_base,
                                 target)
        header = target.CreateValueFromAddress('hdr', address, header_type)
        flags = _int(_member(header, 'flags'))
        next_offset = _int(_member(header, 'next'))
        if flags is None or next_offset is None:
            out('  [%3d] <decode error: header unreadable>', count)
            break
        out('  [%3d] off=0x%x owner_rank=%d flags=%s tag=%s len=%s next=%d',
            count, current & offset_mask, owner, _flag_str(flags),
            _field_str(header, 'tag'), _field_str(header, 'len'), next_offset)
        current = next_offset
        count += 1
    out('  total queued (capped 200): %d', count)


def _btlsm_endpoints(target, out, component, npeers):
    if npeers is None:
        out('--- endpoints: (opal_process_info unavailable; skipping) ---')
        return
    endpoints, source = _ep_array(target, component)
    out('--- endpoints (pending sends / peer fifo head-tail) [from %s] ---',
        source)
    if endpoints is None:
        out('  <endpoints unavailable>')
        return
    element_type = endpoints.GetType().GetPointeeType()
    for i in range(npeers):
        endpoint = _array_at(target, endpoints, i, element_type)
        if endpoint is None:
            out('  ep[%2d] <error: unreadable>', i)
            continue
        peer = _int(_member(endpoint, 'peer_smp_rank'))
        if peer is None:
            out('  ep[%2d] <error: peer_smp_rank unavailable>', i)
            continue
        pending = _list_length(_member(endpoint, 'pending_frags'))
        fifo = _member(endpoint, 'fifo')
        head = tail = 0
        if fifo is not None and 0 != (_uint(fifo) or 0):
            head = _int(_member(fifo, 'fifo_head'), 0)
            tail = _int(_member(fifo, 'fifo_tail'), 0)
        fbox = 'fbox' if 0 != (_uint(_member(endpoint, 'fbox_out.buffer'))
                               or 0) else '----'
        out('  ep[%2d] peer=%2d pending_frags=%d peer_fifo(head=%d tail=%d) %s',
            i, peer, pending, head, tail, fbox)


def btlsm_dump(debugger, command, exe_ctx, result, internal_dict):
    """Dump the sm BTL's FIFOs, fragment free lists and endpoints."""
    target = debugger.GetSelectedTarget()
    out = Out(result)
    component = _global(target, 'mca_btl_sm_component',
                        'mca_btl_sm_component_t')
    if component is None:
        out('btlsm_dump: sm BTL state unavailable; is the sm BTL in use, and '
            'was this built with -g?')
        return

    out(SEPARATOR)
    npeers = None
    info = _global(target, 'opal_process_info', 'opal_process_info_t')
    if info is None:
        out('(opal_process_info unavailable)')
    else:
        out('local_rank=%s node_rank=%s num_local_peers=%s',
            _field_str(info, 'my_local_rank'),
            _field_str(info, 'my_node_rank'),
            _field_str(info, 'num_local_peers'))
        peers = _int(_member(info, 'num_local_peers'))
        npeers = None if peers is None else peers + 1

    _btlsm_fifo(target, out, component)

    out('--- fragment free lists (allocated) ---')
    for name in ('sm_frags_user', 'sm_frags_eager', 'sm_frags_max_send'):
        allocated = _int(_member(component, '%s.fl_num_allocated' % name))
        if allocated is None:
            out('  %-18s <unavailable>', name)
        else:
            out('  %-18s fl_num_allocated=%d', name, allocated)

    _btlsm_endpoints(target, out, component, npeers)

    out('--- pending_endpoints list length = %d ---',
        _list_length(_member(component, 'pending_endpoints')))
    out(SEPARATOR)


#
# req_dump
#

def req_dump(debugger, command, exe_ctx, result, internal_dict):
    """Report every non-completed request in a wait_all array."""
    target = debugger.GetSelectedTarget()
    out = Out(result)
    try:
        args = shlex.split(command)
    except ValueError as error:
        out('req_dump: cannot parse arguments: %s', error)
        return

    requests, count = _resolve_requests(target, out, 'req_dump', args)
    if requests is None or count is None:
        return

    out(SEPARATOR)
    out('req_dump: count=%d requests=0x%x', count, _uint(requests, 0))
    incomplete = 0
    for i in range(count):
        request = _request_at(target, requests, i)
        if request is None:
            out('  [%3d] <error: unreadable>', i)
            continue
        if 0 == (_uint(request) or 0):
            out('  [%3d] <NULL request>', i)
            incomplete += 1
            continue
        complete = _uint(_member(request, 'req_complete'))
        state = _int(_member(request, 'req_state'))
        if complete is None or state is None:
            out('  [%3d] <error: request fields unreadable>', i)
            continue
        if REQUEST_COMPLETED == complete:
            continue
        incomplete += 1
        if 0 == complete:
            complete_str = 'PENDING'
        else:
            complete_str = 'SYNC@0x%x' % complete
        out('  [%3d] type=%s state=%s(%d) complete=%s '
            'status{SOURCE=%s TAG=%s err=%s _ucount=%s _cancelled=%s}',
            i, _enum_str(request, 'req_type'), REQ_STATE.get(state, '?'),
            state, complete_str,
            _field_str(request, 'req_status.MPI_SOURCE'),
            _field_str(request, 'req_status.MPI_TAG'),
            _field_str(request, 'req_status.MPI_ERROR'),
            _field_str(request, 'req_status._ucount'),
            _field_str(request, 'req_status._cancelled'))
    out('--- incomplete/non-completed requests: %d / %d ---', incomplete,
        count)
    out(SEPARATOR)


#
# pml_dump
#

def _active_pml(target):
    """Name of the selected PML component ('ob1', 'cm', ...), or None."""
    component = _global(target, 'mca_pml_base_selected_component',
                        'mca_pml_base_component_t')
    name = _member(component, 'pmlm_version.mca_component_name') \
        if component is not None else None
    if name is None:
        return None
    text = name.GetSummary() or name.GetValue()
    if not text:
        return None
    return text.strip().strip('"').split('\\0')[0] or None


def _pml_dump_ob1_globals(target, out):
    ob1 = _global(target, 'mca_pml_ob1', 'mca_pml_ob1_t')
    if ob1 is None:
        _missing_type(out, 'mca_pml_ob1_t', 'pml_dump')
        return
    for name in ('send_pending', 'recv_pending', 'rdma_pending',
                 'pckt_pending', 'non_existing_communicator_pending'):
        length = _member(ob1, '%s.opal_list_length' % name)
        if length is not None:
            out('  mca_pml_ob1.%-34s size=%s', name, length.GetValue())
    out('  mca_pml_ob1.enabled = %s', _field_str(ob1, 'enabled'))


def _pml_dump_cm_globals(target, out):
    # CM delegates matching and progress to the MTL, so there are no
    # PML-level pending lists; report the active MTL instead.
    mtl = _global(target, 'ompi_mtl', 'mca_mtl_base_module_t', pointer=True)
    if mtl is None:
        _missing_type(out, 'mca_mtl_base_module_t', 'pml_dump')
        return
    out('  ompi_mtl = 0x%x', _uint(mtl, 0))
    component = _global(target, 'ompi_mtl_base_selected_component',
                        'mca_mtl_base_component_t', pointer=True)
    name = _member(component, 'mtl_version.mca_component_name') \
        if component is not None else None
    if name is not None:
        out('  active MTL = %s', name.GetSummary() or name.GetValue())
    out('  ompi_mtl.mtl_flags = %s', _field_str(mtl, 'mtl_flags'))


def _dump_ob1_send(target, out, index, request):
    send_type = _find_type(target, 'mca_pml_ob1_send_request_t')
    if send_type is None:
        _missing_type(out, 'mca_pml_ob1_send_request_t', 'pml_dump')
        return
    send = request.Cast(send_type.GetPointerType())
    out('  --- request[%d] @0x%x (OB1 send) ---', index, _uint(request, 0))
    out('      req_pending        = %s', _enum_str(send, 'req_pending'))
    out('      req_state          = %s', _field_str(send, 'req_state'))
    out('      req_lock           = %s', _field_str(send, 'req_lock'))
    out('      req_pipeline_depth = %s',
        _field_str(send, 'req_pipeline_depth'))
    out('      req_bytes_delivered= %s',
        _field_str(send, 'req_bytes_delivered'))
    out('      req_rdma_cnt       = %s', _field_str(send, 'req_rdma_cnt'))
    out('      bytes_packed       = %s',
        _field_str(send, 'req_send.req_bytes_packed'))
    out('      send_mode          = %s',
        _enum_str(send, 'req_send.req_send_mode'))
    out('      peer/tag/seq       = %s / %s / %s',
        _field_str(send, 'req_send.req_base.req_peer'),
        _field_str(send, 'req_send.req_base.req_tag'),
        _field_str(send, 'req_send.req_base.req_sequence'))
    out('      pml_complete       = %s',
        _field_str(send, 'req_send.req_base.req_pml_complete'))


def _dump_ob1_recv(target, out, index, request):
    recv_type = _find_type(target, 'mca_pml_ob1_recv_request_t')
    if recv_type is None:
        _missing_type(out, 'mca_pml_ob1_recv_request_t', 'pml_dump')
        return
    recv = request.Cast(recv_type.GetPointerType())
    out('  --- request[%d] @0x%x (OB1 recv) ---', index, _uint(request, 0))
    out('      req_pending        = %s', _enum_str(recv, 'req_pending'))
    out('      req_match_received = %s',
        _field_str(recv, 'req_match_received'))
    out('      req_lock           = %s', _field_str(recv, 'req_lock'))
    out('      req_pipeline_depth = %s',
        _field_str(recv, 'req_pipeline_depth'))
    out('      req_bytes_received = %s',
        _field_str(recv, 'req_bytes_received'))
    out('      req_bytes_expected = %s',
        _field_str(recv, 'req_bytes_expected'))
    out('      req_rdma_cnt       = %s', _field_str(recv, 'req_rdma_cnt'))
    out('      peer/tag/seq       = %s / %s / %s',
        _field_str(recv, 'req_recv.req_base.req_peer'),
        _field_str(recv, 'req_recv.req_base.req_tag'),
        _field_str(recv, 'req_recv.req_base.req_sequence'))
    out('      pml_complete       = %s',
        _field_str(recv, 'req_recv.req_base.req_pml_complete'))


def _dump_ob1_request(target, out, index, request):
    base_type = _find_type(target, 'mca_pml_base_request_t')
    if base_type is None:
        _missing_type(out, 'mca_pml_base_request_t', 'pml_dump')
        return
    base = request.Cast(base_type.GetPointerType())
    base_kind = _int(_member(base, 'req_type'))
    if base_kind is None:
        out('  [%d] <cannot read OB1 base request>', index)
    elif 1 == base_kind:
        _dump_ob1_send(target, out, index, request)
    elif 2 == base_kind:
        _dump_ob1_recv(target, out, index, request)
    else:
        out('  --- request[%d] @0x%x is OB1 %s (no detailed decoder) ---',
            index, _uint(request, 0),
            MCA_PML_REQUEST.get(base_kind, '?(%d)' % base_kind))


def _dump_cm_request(target, out, index, request):
    cm_type = _find_type(target, 'mca_pml_cm_request_t')
    if cm_type is None:
        _missing_type(out, 'mca_pml_cm_request_t', 'pml_dump')
        return
    cm = request.Cast(cm_type.GetPointerType())
    pml_type = _int(_member(cm, 'req_pml_type'), -1)
    out('  --- request[%d] @0x%x (CM %s) ---', index, _uint(request, 0),
        CM_REQ_TYPE.get(pml_type, '?(%d)' % pml_type))
    out('      req_pml_complete   = %s', _field_str(cm, 'req_pml_complete'))
    out('      req_free_called    = %s', _field_str(cm, 'req_free_called'))
    out('      req_comm           = %s', _field_str(cm, 'req_comm'))
    out('      req_datatype       = %s', _field_str(cm, 'req_datatype'))
    out('      req_staged         = %s', _field_str(cm, 'req_staged'))
    out('      status             = {SOURCE=%s TAG=%s err=%s _ucount=%s '
        '_cancelled=%s}',
        _field_str(cm, 'req_ompi.req_status.MPI_SOURCE'),
        _field_str(cm, 'req_ompi.req_status.MPI_TAG'),
        _field_str(cm, 'req_ompi.req_status.MPI_ERROR'),
        _field_str(cm, 'req_ompi.req_status._ucount'),
        _field_str(cm, 'req_ompi.req_status._cancelled'))

    info = CM_SUBTYPE.get(pml_type)
    if info is None:
        return
    type_name, heavy, is_send = info
    concrete_type = _find_type(target, type_name)
    if concrete_type is None:
        _missing_type(out, type_name, 'pml_dump')
        return
    concrete = request.Cast(concrete_type.GetPointerType())
    if is_send:
        out('      send_mode          = %s',
            _enum_str(concrete, 'req_send.req_send_mode'))
    if heavy:
        # Heavy requests keep an explicit copy of the envelope and buffer.
        out('      peer/tag/count     = %s / %s / %s',
            _field_str(concrete, 'req_peer'),
            _field_str(concrete, 'req_tag'),
            _field_str(concrete, 'req_count'))
        out('      req_blocking       = %s',
            _field_str(concrete, 'req_blocking'))
    # The MTL request is the last member; its completion_callback symbol
    # identifies the CM/MTL path.  MTL-private state is appended beyond this
    # base and is not portably decodable here.
    out('      req_mtl.ompi_req   = %s',
        _field_str(concrete, 'req_mtl.ompi_req'))
    out('      req_mtl.completion = %s',
        _field_str(concrete, 'req_mtl.completion_callback'))


def pml_dump(debugger, command, exe_ctx, result, internal_dict):
    """Dump PML globals and decode the first few incomplete PML requests."""
    target = debugger.GetSelectedTarget()
    out = Out(result)
    try:
        args = shlex.split(command)
    except ValueError as error:
        out('pml_dump: cannot parse arguments: %s', error)
        return

    out(SEPARATOR)
    pml = _active_pml(target)
    out('  active PML = %s', pml if pml else '<unknown>')
    if 'ob1' == pml:
        _pml_dump_ob1_globals(target, out)
    elif 'cm' == pml:
        _pml_dump_cm_globals(target, out)

    requests, count = _resolve_requests(target, out, 'pml_dump', args)
    if requests is None or count is None:
        out(SEPARATOR)
        return

    if 2 == len(args):
        indices = list(range(count))
    else:
        # Without explicit arguments, decode only the first few requests that
        # are still outstanding; a full wait array can be very large.
        indices = []
        for i in range(count):
            # One unreadable entry must not cost us the readable ones: a hung
            # rank is exactly where the array may be partly garbage.
            request = _request_at(target, requests, i)
            pointer = None if request is None else _uint(request)
            if pointer is None:
                out('  --- request[%d] unreadable ---', i)
            elif 0 != pointer:
                complete = _uint(_member(request, 'req_complete'))
                if complete is None:
                    out('  --- request[%d] unreadable ---', i)
                elif REQUEST_COMPLETED != complete:
                    indices.append(i)
            if 4 <= len(indices):
                break

    for i in indices:
        request = _request_at(target, requests, i)
        if request is None:
            out('  [%d] <error: unreadable>', i)
            continue
        if 0 == (_uint(request) or 0):
            out('  --- request[%d] <NULL> ---', i)
            continue
        # Only PML requests carry PML send/recv state; a mixed wait array can
        # also hold collective, IO or generalized requests.
        request_kind = _enum_str(request, 'req_type')
        if 'OMPI_REQUEST_PML' not in request_kind:
            out('  --- request[%d] @0x%x is %s; not a PML request ---',
                i, _uint(request, 0), request_kind)
            continue
        if 'ob1' == pml:
            _dump_ob1_request(target, out, i, request)
        elif 'cm' == pml:
            _dump_cm_request(target, out, i, request)
        else:
            out('  --- request[%d] @0x%x: no request decoder for PML %r ---',
                i, _uint(request, 0), pml)
    out(SEPARATOR)


def __lldb_init_module(debugger, internal_dict):
    module = __name__
    for name in ('btlsm_dump', 'req_dump', 'pml_dump'):
        debugger.HandleCommand(
            'command script add -o -f %s.%s ompi_%s' % (module, name, name))
