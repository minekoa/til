#!/usr/bin/env python3

import csv
from operator import itemgetter

if __name__ == "__main__":
    import sys, os

    file1 = sys.argv[1]
    file2 = sys.argv[2]

    f2map = dict()

    with open(file2, "r") as f2:
        seek_offset = 0
        for line in f2.readlines():
            row = line.split(",")
            if len(row) < 5:
                continue

            loginid = row[4]
            f2map[ loginid ] = seek_offset
            seek_offset += len(line)

    print ("start!")

    with open("out.txt", "w") as wf:

        with open(file1,"r") as f1:
            with open(file2,"r") as f2:
                reader1 = csv.reader(f1)

                for row1 in reader1:
                    loginid = row1[4]

                    if loginid in f2map:
                        wf.write( ",".join(row1) + "\n")
                        f2.seek(f2map[loginid],0)
                        s = f2.readline()
                        wf.write(s)
                        wf.write("\n")
#                        print("!", end="")
#
#                    else:
#                        print(".", end="")
