#!/usr/bin/env zsh

function setcurrfile() {
  echo "set args $1" >.gdb/file.gdb
}

SUCCESS_CODE=255
FILENAME="res"
if [[ "$1" == "test" ]]; then
  shift
  if [[ "$1" == "e2e" ]]; then
    shift
    env -C build/test pytest e2e.py
  elif [[ -n "$1" ]]; then
    setcurrfile build/test/unit_"$1"
    ./build/test/unit_$1
  else
    ctest --output-on-failure --debug -VV --test-dir build/test
  fi
else
  echo '---- COMPILING'

  [ -L CmmLang ] || ln -s ./build/CmmLang CmmLang

  # Clear old files
  [ -f "$FILENAME".preprocessed ] && rip "$FILENAME".preprocessed
  [ -f "$FILENAME".asm ] && rip "$FILENAME".asm
  [ -f "$FILENAME" ] && rip "$FILENAME"

  setcurrfile "$1"

  ./CmmLang "$@"

  result=$?
  if [[ $result = 139 ]]; then
    echo -e "\033[1;31mCORE DUMPED\033[0m"
  elif [[ $result = 0 ]]; then
    echo '---- EXECUTING'
    ./"$FILENAME"
    code=$?
    echo "Exit status -> " $code
    if [[ $code = $SUCCESS_CODE ]]; then
      echo -e "\033[1;32mOK\033[0m"
    else
      echo -e "\033[1;31mERROR\033[0m"
    fi
  fi
fi
