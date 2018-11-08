#!/bin/bash

/usr/bin/time -f "%M(KB) e=%E s=%S u=%U" ./aggregation_domain.py ../../pj1/log.csv 
