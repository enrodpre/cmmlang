#!/usr/bin/env zsh

function print_error() {
  echo -e "\033[0;31m$1 -> $2\033[0m"
}

function print_ok() {
  echo -e "\033[0;32m$1\033[0m"
}

example=${1-current.cmm}

echo "set args $example" >gdb/file.gdb

CmmLang $example

result=$?

if [ "$result" = 1 ]; then
  if [ $result = 5 ]; then
    print_error "COMPILATION ERROR" $result
  elif [ $result = 6 ]; then
    print_error "ASSEMBLING ERROR" $result
    cat res.asm
  fi
  exit $result
fi

./res
result=$?

if [ $result = 255 ]; then
  print_ok OK
else
  print_error "NOT OK" $result
  cat res.asm
  exit 1
fi
