#!/bin/bash

stack build
stack exec blog rebuild
if [ $? -eq 0 ] && [ -z $1 ]; then
  stack exec blog watch
fi
