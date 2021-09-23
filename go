#!/bin/bash

stack build
stack exec blog rebuild
if [ $? -eq 0 ]; then
  stack exec blog watch
fi
