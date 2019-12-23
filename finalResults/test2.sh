#!/bin/sh
find . -type d -links 2 -execdir ../ some arguments here \;


