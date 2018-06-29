# TripleR

A simple R6 class for running paralel R jobs by making use of Docker containers.

Two types of paralel execution of R analytical procedures:
1. Run multiple jobs, each with a slight variation
2. Split a large dataset into n segments and run same job on each segment

This requires a pre-existing docker image containing R-base and necessary packages to run the R script.
Building docker images is not covered here. For testing can use ```kurakuradave/rarazon:0.01```

## How to use

Checkout examples in example.R
