#!/usr/bin/env zsh

example=${1-current.cmm}

CmmLang $example

./res
result=$?

if [[ $result -eq 255 ]]; then
  echo -e "\033[0;32mOK\033[0m"
else
  echo -e "\033[0;31mNOT OK\033[0m"
fi
