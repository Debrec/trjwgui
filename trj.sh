#!/bin/sh

PROGRAM=trj

SRCPATH=f90trj

rm $SRCPATH/*.o
rm $SRCPATH/*.mod

make -C f90trj

mv $SRCPATH/$PROGRAM .
