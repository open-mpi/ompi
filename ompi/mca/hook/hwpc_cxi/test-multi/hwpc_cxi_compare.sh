compare_counter_logs() {
    local baseline_log="$1"
    local current_log="$2"
    local output_log="$3"
    local tolerance_pct="$4"
    local pe0_gate="${5:-0}"
    local require_data="${6:-0}"
    local ignore_hostname="${7:-0}"
    local base_has_pe0=0 curr_has_pe0=0
    if [[ "${pe0_gate}" == "1" ]]; then
        grep -q '^PE 0:' "${baseline_log}" 2>/dev/null && base_has_pe0=1
        grep -q '^PE 0:' "${current_log}" 2>/dev/null && curr_has_pe0=1
    fi

    awk -v tolerance_pct="${tolerance_pct}" \
        -v base_has_pe0="${base_has_pe0}" \
        -v curr_has_pe0="${curr_has_pe0}" \
        -v require_data="${require_data}" -v ignore_hostname="${ignore_hostname}" '
        function is_num(value) {
            return value ~ /^[-+]?[0-9]+([.][0-9]+)?([eE][-+]?[0-9]+)?$/
        }
        function abs(value) {
            return value < 0 ? -value : value
        }
        function parse_counter_line() {
            if ($0 == "CXI_COUNTER_DATA hostname device counter_name delta rate") {
                return 0
            }
            if ($1 == "CXI_COUNTER_DATA") {
                if (NF != 6 || $3 !~ /^[0-9]+$/ || !is_num($5) || !is_num($6)) {
                    print "ERROR malformed detail row: " $0
                    err++
                    return 0
                }
                parsed_key = "detail" SUBSEP (ignore_hostname ? "" : $2) SUBSEP $3 SUBSEP $4
                parsed_label = $2 "/" $3 "/" $4
                parsed_start = 5
                parsed_count = 2
                return 1
            }
            if (!require_data && NF >= 8 && $1 != "Counter" && is_num($2) && is_num($3)) {
                parsed_key = "summary" SUBSEP $1
                parsed_label = $1
                parsed_start = 2
                parsed_count = NF - 1
                return 1
            }
            return 0
        }
        BEGIN {
            err = 0
            warn = 0
            n_base = 0
            n_curr = 0
            base_active = 0
            curr_active = 0
        }
        FILENAME == ARGV[1] {
            if (tolower($0) ~ /error/) {
                print "ERROR baseline line: " $0
                err++
                next
            }
            if (base_has_pe0 && $0 ~ /^PE 0:/) {
                base_active = 1
            }
            if (base_has_pe0 && !base_active) {
                next
            }
            if (!parse_counter_line()) {
                next
            }
            name = parsed_key
            if (!(name in seen_base)) {
                seen_base[name] = 1
                base_names[++n_base] = name
                base_label[name] = parsed_label
            }
            base_count[name] = parsed_count
            for (i = 0; i < parsed_count; ++i) {
                base_val[name, i + 1] = $(parsed_start + i)
            }
            next
        }
        {
            if (tolower($0) ~ /error/) {
                print "ERROR current line: " $0
                err++
                next
            }
            if (curr_has_pe0 && $0 ~ /^PE 0:/) {
                curr_active = 1
            }
            if (curr_has_pe0 && !curr_active) {
                next
            }
            if (!parse_counter_line()) {
                next
            }
            name = parsed_key
            if (!(name in seen_curr)) {
                seen_curr[name] = 1
                curr_names[++n_curr] = name
                curr_label[name] = parsed_label
            }
            curr_count[name] = parsed_count
            for (i = 0; i < parsed_count; ++i) {
                curr_val[name, i + 1] = $(parsed_start + i)
            }
        }
        END {
            print "Counter comparison (tolerance=" tolerance_pct "%)"
            if (require_data && (0 == n_base || 0 == n_curr)) {
                print "ERROR detail comparison requires data rows in both files"
                err++
            }
            for (idx = 1; idx <= n_base; ++idx) {
                name = base_names[idx]
                label = base_label[name]
                if (!(name in seen_curr)) {
                    print "ERROR missing counter in current log: " label
                    err++
                    continue
                }
                if (base_count[name] != curr_count[name]) {
                    print "ERROR counter column mismatch for " label
                    err++
                    continue
                }
                for (i = 1; i <= base_count[name]; ++i) {
                    b = base_val[name, i]
                    c = curr_val[name, i]
                    if (!is_num(b) || !is_num(c)) {
                        print "ERROR non-numeric field for counter " label " column " i
                        err++
                        continue
                    }
                    if ((b + 0) == 0) {
                        pct = ((c + 0) == 0) ? 0 : 100
                    } else {
                        pct = (abs((c + 0) - (b + 0)) / abs(b + 0)) * 100
                    }
                    if (pct > tolerance_pct) {
                        warn++
                        printf("WARN %s col%d baseline=%s current=%s delta=%.2f%%\n", label, i, b, c, pct)
                    }
                }
            }
            for (idx = 1; idx <= n_curr; ++idx) {
                name = curr_names[idx]
                if (!(name in seen_base)) {
                    print "ERROR new counter in current log: " curr_label[name]
                    err++
                }
            }
            print "Summary: warnings=" warn ", errors=" err
            exit (err > 0)
        }
    ' "${baseline_log}" "${current_log}" > "${output_log}"
}