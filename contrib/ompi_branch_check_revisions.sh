#!/bin/sh

#
# Some initialization
#
WANT_HTML=1
BRANCH_VERSION="v1.5"
TRAC_URL="https://svn.open-mpi.org/trac/ompi"
TRUNK_URL="https://svn.open-mpi.org/svn/ompi/trunk/"
RELATIVE_URL="https://svn.open-mpi.org/svn/ompi/branches/$BRANCH_VERSION"
# XXX might be really tmp by spec $$
TMP_REVS="$TMPDIR/ompi_revs.txt"
REVS_NOTES="ompi_branch_check_revisions-$BRANCH_VERSION.txt"
MERGE_LOG="$TMPDIR/ompi_merge_log.txt"
MERGE_FILE="$TMPDIR/ompi_merge.txt"

MSG_WIDTH=90
PARSE_REVS=0


function usage()
{
    echo "Outputs a HTML or a comma-separated file, listing outstanding r-numbers."
    echo "May use an input file per branch, describing notes such as pending CMRs,"
    echo "dependencies or reasons, why a revision should not be moved."
    echo ""
    echo "Usage: $0 [-h] [-b Branch] [-u URL] [-o Output Type]" 
    echo ""
    echo "-h   This help"
    echo "-b   Branch version, which is appended to default URL [default:v1.5]"
    echo "-u   URL to compare to [default: https://svn..../ompi/branches/BRANCH_VERSION]"
    echo "-o   Output Type, possible: txt, html [default:html]"
    echo "-w   Width of commit msg. that will be cut at [default:90]"
    echo "-m   Merge file [default: extract from svn log of BRANCH_VERSION]"
    echo "-r   Revision file with notes [default:svn update of `basename $0 .sh`-BRANCH_VERSION.txt]"
    echo "-p   Parse revision file notes (see notes below) [default:off]"
    echo ""
    echo "When parsing the revision file notes (see option -r), the following marks will be done"
    echo "  MAILED           Author has been made aware of filing a CMR"
    echo "  CMR:BRANCH[:NUM] There exists a CMR (with link to TRACS)"
    echo "  MERGED           The Revision will not show up in the output"
    echo "The markings are in increasing order, i.e. if MERGED is part of notes the line is not shown."
    exit 1
}

function check_svn_url()
{
    URL=$1

    svn list $URL > /dev/null 2>&1
    if test $? != 0 ; then
        echo "ERROR: The provided URL:$URL is not correct or cannot svn list"
        exit 1
    fi
}


#
# Check the options
#
while getopts "b:o:u:w:m:r:ph" opt; do
    case $opt in
        b)
            BRANCH_VERSION=$OPTARG
            RELATIVE_URL="https://svn.open-mpi.org/svn/ompi/branches/$BRANCH_VERSION"
            ;;
        o)
            if test "$OPTARG" = "html" ; then
                WANT_HTML=1
            elif test "$OPTARG" = "txt" ; then
                WANT_HTML=0
            else
                usage
            fi
            ;;
        u)
            RELATIVE_URL=$OPTARG
            ;;
        w)
            MSG_WIDTH=$OPTARG
            ;;
        m)
            MERGE_FILE=$OPTARG
            ;;
        r)
            REVS_NOTES=$OPTARG
            ;;
        p)
            PARSE_REVS=1
            ;;
        h)
            usage
            ;;
        :)
            echo "Option -$OPTARG requires an argument."
            usage
            ;;
        \?)
            echo "Invalid option: -$OPTARG"
            usage
            ;;
    esac
done

# Check the URLs
check_svn_url $TRUNK_URL
check_svn_url $RELATIVE_URL


svn mergeinfo --show-revs eligible $TRUNK_URL $RELATIVE_URL > $TMP_REVS
if test \! -f $TMP_REVS ; then
    echo "ERROR: TMP_REVS:$TMP_REVS file is not available"
    exit 1
fi

svn update $TRUNK_URL/contrib/$REVS_NOTES > /dev/null 2>&1

# If there is no notes file (nothing checked out, nothing local) reset NOTES
if test \! -f $REVS_NOTES ; then
    unset REVS_NOTES
    NOTES=""
fi

# The ALREADY merged (but by means of "svn mergeinfo" not recognised)
# revisions can be "savely" parsed:
if test $PARSE_REVS = 1 ; then
    svn log --incremental --stop-on-copy $RELATIVE_URL > $MERGE_LOG
    grep '^From r' $MERGE_LOG | sed -e 's/From \(r[0-9]*\):/\1/' > $MERGE_FILE
fi

DATE=`date +%Y.%m.%d`
if test "x$WANT_HTML" = "x1" ; then
    echo "<HTML><HEAD><TITLE>Revisions eligible to merge into $BRANCH_VERSION as of $DATE</TITLE></HEAD>"
    echo "<TABLE BORDER=1 BGCOLOR=#FFFFFF><TR BGCOLOR=#CCCCCC><TH>Revision</TH><TH>Author</TH><TH>Commit Msg.</TH><TH>Notes</TH></TR>"
else
    echo "# Revisions eligible to merge from trunk into $BRANCH_VERSION as of $DATE"
    echo "# Revision, Author, Commit Msg, Notes"
fi

OLD_IFS=$IFS
IFS='^'
LINE_ODD=0;
while read REV ; do
    LOG=`svn log -r$REV $TRUNK_URL`
    REV_NUMBER=${REV##*r}
    AUTHOR=`echo $LOG | head -n2 | tail -n1 | cut -f2 -d'|'`
    MSG=`echo $LOG | head -n4 | tail -n1 | cut -c-$MSG_WIDTH`
    if test "x$REVS_NOTES" != "x" ; then
        NOTES=`grep "^$REV" $REVS_NOTES | cut -f2 -d','`
    fi

    unset BGCOLOR
    if test $PARSE_REVS = 1 ; then
        # Mark MAILED as Yellow
        echo $NOTES | grep -iq "MAILED" && BGCOLOR=#FFFF00
        # Mark FIX as Red
        echo $NOTES | grep -iq "FIX" && BGCOLOR=#FF0000
        # Mark CMR as green -- but only if for this branch
        echo $NOTES | grep -iq "CMR:$BRANCH_VERSION" && BGCOLOR=#00FF00 && NOTES=`echo $NOTES | sed -e "s%#\([0-9]*\)%\<A HREF=\"$TRAC_URL/ticket/\1\"\>\0\<\/A\>%"`

        # Skip MERGED (investigate why svn merge / svn mergeinfo does not work)
        echo $NOTES | grep -iq "MERGED" && continue

        # At last, if we found the revision in the MERGE_FILE, skip as well
        grep -q $REV $MERGE_FILE && continue
    fi

    if test "x$WANT_HTML" = "x1" ; then
        # Only overwrite the color if it has not yet been set
        if test "x$BGCOLOR" = "x" ; then 
            if test $LINE_ODD = 1 ; then
                BGCOLOR=#EEEEEEE
            else
                BGCOLOR=#FFFFFFF
            fi
        fi
        echo "<TR BGCOLOR="$BGCOLOR"><TD><A HREF=\""$TRAC_URL/changeset/$REV_NUMBER"\">$REV</A></TD><TD>$AUTHOR</TD><TD>$MSG</TD><TD>$NOTES</TD></TR>"
    else
        echo "$REV, $AUTHOR, \"$MSG\", $NOTES"
    fi

    if test $LINE_ODD = 0 ; then
        LINE_ODD=1
    else
        LINE_ODD=0
    fi

done < $TMP_REVS
IFS=$OLD_IFS

if test "x$WANT_HTML" = "x1" ; then
    echo "</TABLE></HTML>"
else
    echo "#"
fi

# rm $TMP_REVS $MERGE_LOG $MERGE_FILE

