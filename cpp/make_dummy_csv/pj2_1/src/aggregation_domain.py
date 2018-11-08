#!/usr/bin/env python3

import csv
from operator import itemgetter

if __name__ == "__main__":
    import sys, os

    logfilepath = sys.argv[1]

    sm = dict()

    with open(logfilepath, "r") as f:
        reader = csv.reader(f)

        for row in reader:
            dom = row[1].split("@")[1]

            c = sm.get(dom, 0)
            sm[dom] = c + 1



    lst = [ (k, v) for (k, v) in sm.items() ]
    lst.sort( key=itemgetter(1), reverse=True )


    with open("summary.csv", "w") as f:
        writer = csv.writer(f)
        cnt = 0

        for elm in lst:
            writer.writerow(elm)
            cnt += 1
            if 100 <= cnt: break

