#!/bin/bash

# La idea es corregir cosas, pero no puedo hacer que ella te ame

if [[ $# -ne 1 ]]; then
  echo "Usage: `basename $0` nameOfFile > outputFile" 1>&2
  exit 1
fi

objection=false

detectVowel(){
  case $chara in
    "a")
      echo -n "á"
      ;;

    "e")
      echo -n "é"
      ;;

    "i")
      echo -n "í"
      ;;

    "ı")
      echo -n "í"
      ;;

    "o")
      echo -n "ó"
      ;;

    "u")
      if [[ $anteriorxd == "´" ]]; then
        echo -n "ú"
      elif [[ $anteriorxd == "¨" ]]; then
        echo -n "ü"
      fi
      ;;

    "n")
      echo -n "ñ"
      ;;
  esac
}

while IFS="" read -rN1 chara; do
  if [[ "$objection" = true ]]; then
    detectVowel
    objection=false
  elif [[ ($chara == "´") || ($chara == "˜") || ($chara == "¨")]]; then
    objection=true
    anteriorxd=$chara
  else
    echo -en "$chara"
  fi
done < $1

echo 
