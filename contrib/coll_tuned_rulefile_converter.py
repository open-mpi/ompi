#!/usr/bin/env python3

# Copyright (c) 2024-2025 Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow

import re
import json
from collections import OrderedDict

coll_dict = {
    'allgather'             : 0,
    'allgatherv'            : 1,
    'allreduce'             : 2,
    'alltoall'              : 3,
    'alltoallv'             : 4,
    'alltoallw'             : 5,
    'barrier'               : 6,
    'bcast'                 : 7,
    'exscan'                : 8,
    'gather'                : 9,
    'gatherv'               : 10,
    'reduce'                : 11,
    'reducescatter'         : 12,
    'reducescatterblock'    : 13,
    'scan'                  : 14,
    'scatter'               : 15,
    'scatterv'              : 16,
    'neighbor_allgather'    : 17,
    'neighbor_allgatherv'   : 18,
    'neighbor_alltoall'     : 19,
    'neighbor_alltoallv'    : 20,
    'neighbor_alltoallw'    : 21 }
coll_dict_rev = { v:k for k,v in coll_dict.items() }

han_component_dict = {
    "self"                  : 0,
    "basic"                 : 1,
    "libnbc"                : 2,
    "tuned"                 : 3,
    "sm"                    : 4,
    "adapt"                 : 5,
    "han"                   : 6,
}

han_topo_level_dict = {
    'intra_node'            : 0,
    'inter_node'            : 1,
    'global_communicator'   : 2,
}


def strip_comments(line):
    return re.sub(r"#.*","",line).strip()

class GenericOpenMPIRuleReader():
    def __init__(self, fp, fname_for_prints=""):
        self.fp = fp
        # The 1-indexed line number which corresponds to the next byte of fp read.
        self.jline = 1
        self.line_start = 0
    def get_next_line(self):
        while True:
            self.line_start = self.fp.tell()
            line = self.fp.readline()
            if not line: return None
            self.jline += 1
            if strip_comments(line):
                return line

    def isnext_digit(self):
        # ompi_coll_base_file_peek_next_char_isdigit
        tell = self.fp.tell()
        while True:
            next = self.fp.read(1)
            if next in ' \t':
                tell += 1
                continue
            self.fp.seek(tell)
            return next in '0123456789'

    def get_next(self):
        # (ompi_coll_base_file_getnext_long)
        while True:
            line = self.get_next_line()
            if not line: return None
            UNK = -1
            jnum_start = UNK
            jnum_end = UNK
            for jc in range(len(line)):
                if line[jc] in "#":
                    break
                if line[jc] in '0123456789':
                    if jnum_start == UNK:
                        jnum_start = jc
                    jnum_end = jc
                else:
                    if jnum_end != UNK:
                        break
            if jnum_end != UNK:
                self.fp.seek(self.line_start+jnum_end+1)
                # decrement the line number, the next read will continue on this line.
                self.jline -= 1
                return int(line[jnum_start:jnum_end+1])

    def read_header(self):
        line = self.get_next_line()
        match = re.match("rule-file-version-([0-9])", line)
        if match:
            return int(match.group(1))
        else:
            self.jline -= 1
            self.fp.seek(self.line_start)
            return 1

class TunedRuleReader(GenericOpenMPIRuleReader):
    def load_rulefile(self):
        json_root = OrderedDict()
        file_ver = self.read_header()
        json_root['rule_file_version'] = 3
        json_root['module'] = 'tuned'
        json_root['collectives'] = OrderedDict()

        ncollectives = self.get_next()
        for jcol in range(ncollectives):
            coll_id = self.get_next()
            coll_name = coll_dict_rev[coll_id]
            comm_rules = []
            ncomm_sizes = self.get_next()
            for jcomm_size in range(ncomm_sizes):
                comm_size = self.get_next()
                nmsg_sizes = self.get_next()
                comm_rule = OrderedDict()
                comm_rule['comm_size_min'] = 0
                if jcomm_size+1 < ncomm_sizes:
                    comm_rule['comm_size_max'] = max(comm_size-1, 0)
                if jcomm_size > 0:
                    comm_rule['comm_size_min'] = comm_rules[jcomm_size-1]['comm_size_max'] + 1
                msg_rules = []
                for jmsg in range(nmsg_sizes):
                    msg_size = self.get_next()
                    result_alg = self.get_next()
                    result_topo_faninout = self.get_next()
                    result_segsize = self.get_next()
                    rule = OrderedDict()
                    rule['msg_size_min'] = msg_size
                    if jmsg < nmsg_sizes - 1:
                        rule['msg_size_max'] = 'Inf'
                    if jmsg > 0:
                        msg_rules[jmsg-1]['msg_size_max'] = msg_size - 1
                    rule['alg'] = result_alg
                    if result_topo_faninout != 0:
                        rule['faninout'] = result_topo_faninout
                    if result_segsize != 0:
                        rule['segsize'] = result_segsize
                    result_maxreq = 0
                    if file_ver > 1 and self.isnext_digit():
                        result_maxreq = self.get_next()
                    if result_maxreq != 0:
                        rule['reqs'] = result_maxreq
                    msg_rules.append(rule)
                comm_rule['rules'] = msg_rules
                comm_rules.append(comm_rule)
            json_root['collectives'][coll_name] = comm_rules
        return json_root

class TunedRuleWriter():
    def __init__(self):
        pass
    def to_file(json_rules):
        for coll in coll_dict.keys():
            if coll in json_rules['collectives']:
                pass

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--input","-i", type=argparse.FileType('r'), required=True)
    # parser.add_argument("--output","-o",type=argparse.FileType('w'), required=True)

    args = parser.parse_args()
    reader = TunedRuleReader(args.input)
    print(json.dumps(reader.load_rulefile(), indent=4))
