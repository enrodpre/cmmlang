#!/usr/bin/env zsh

example=${1-current.cmm}

echo "set args $example" >gdb/file.gdb

CmmLang $example

./res
result=$?

if [[ $result -eq 255 ]]; then
  echo -e "\033[0;32mOK\033[0m"
else
  echo -e "\033[0;31mNOT OK -> $result\033[0m"
  cat res.asm
  exit 1
fi
