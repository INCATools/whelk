#!/bin/bash

sbt "project coreJVM" clean stage "project coreNative" clean nativeLink
gunzip -k uberon-go-cl-ro.ofn.gz

# JVM version uses about 6 GB memory
# reasoning step completes in ~120 seconds on 2020 MacBook Pro
# queries step completes in ~130 seconds
export JAVA_OPTS=-Xmx16G
./modules/core/.jvm/target/universal/stage/bin/whelk uberon-go-cl-ro.ofn

# scala-native
# With immix GC, memory grows quickly and then crashes shortly after reaching ~32 GB
# Segmentation fault: 11
# Same result with commix GC
# With boehm GC, memory grows slowly to around 10 GB and completes reasoning step in ~600 seconds
## completes queries step in ~880 seconds
####
# UPDATE - using SCALANATIVE_MAX_SIZE=30G, it can run to completion.
# Both immix and commix are much faster than boehm, but slower and require a lot more memory than the JVM
# commix is faster than immix but maxes out all 8 CPUs (JVM uses <2).
./modules/core/.native/target/scala-2.13/whelk-out uberon-go-cl-ro.ofn
