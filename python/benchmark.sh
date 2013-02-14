#! /bin/sh

export PYTHONPATH=.:tests
python test/benchmark.py $*
