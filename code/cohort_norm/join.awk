# join.awk -- map batched Ab3P output back to PMIDs.
#
# Ab3P echoes each input line, then prints any pairs it found indented
# beneath it. Counting echoed lines therefore gives the index of the current
# input line, which indexes the manifest to recover the PMID.

BEGIN {
    FS = "|"

    while ((getline p < manifest) > 0)
        pmid[++np] = p

    if (np == 0) {
        print "join.awk: manifest is empty or unreadable: " manifest > "/dev/stderr"
        exit 1
    }

    i = 0
}

# Ab3P result line:
# two leading spaces, followed by short form, long form, and precision.
/^  / {
    sf = $1
    gsub(/^[ \t]+|[ \t]+$/, "", sf)

    lf = $2
    gsub(/^[ \t]+|[ \t]+$/, "", lf)

    if ($3 + 0 > minprec && i >= 1 && i <= np)
        print pmid[i] "\t" sf "\t" lf "\t" ($3 + 0)

    next
}

# Anything else is an echoed input line.
{
    i++
}

END {
    if (i != np)
        printf("join.awk: saw %d input lines but manifest has %d; " \
               "PMID mapping may be wrong\n", i, np) > "/dev/stderr"
}
