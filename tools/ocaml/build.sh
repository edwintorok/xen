#!/bin/sh
set -eu
find_command()
{
  CMD=$(command -v "${1}.opt" "${1}" | head -n1)
  if [ -z "${CMD}" ]; then
    echo "${1} not found" >&2
    exit 1
  fi
  echo "${CMD}"
}

OCAMLDEP=$(find_command ocamldep)
OCAMLOPT=$(find_command ocamlopt)
OCAMLC=$(find_command ocamlc)
OCAMLMKLIB=$(find_command ocamlmklib)

FLAGS=
OCAMLOPT_FLAGS=
LIBS=

build_sources()
{
  DIR="${1}"
  echo "Building ${DIR}" >&2
  "${OCAMLC}" ${FLAGS} ${2} -c $("${OCAMLDEP}" -sort ${DIR}/*.mli)

  DEPS=$("${OCAMLDEP}" -sort ${DIR}/*.ml)
  "${OCAMLOPT}" ${FLAGS} ${2} -c ${DEPS}
  #"${OCAMLOPT}" ${FLAGS} ${2} -c ${DEPS}
  echo "${DEPS}" | sed -e 's/\.ml/\.cmx/g'
}

build_library()
{
  DIR="libs/${1}"
  FLAGSOLD="${FLAGS}"
  FLAGS="-I ${DIR} ${FLAGS}"
  if [ -z "${3}" ]; then
    DEPS=$(build_sources "${DIR}" "")
  else
    DEPS=$(build_sources "${DIR}" "-for-pack ${3}")
    "${OCAMLOPT}" -pack -o "${DIR}/${2}.cmx" ${DEPS}
    DEPS="${DIR}/${2}.cmx"
  fi
  OCAMLOPT_FLAGS="${OCAMLOPT_FLAGS} ${2}.cmxa"
  LIBS="${LIBS} -I ${DIR} ${2}.cmxa"

  DOSTUBS=
  cd ${DIR}
  for CFILE in *_stubs.c; do
    "${OCAMLC}" -ccopt -I../../../include -ccopt -I../mmap "${CFILE}"
    DOSTUBS=1
  done
  cd ../../
  if [ -n "${DOSTUBS}" ]; then
    # build %.cmxa
    "${OCAMLMKLIB}" -v ${4} -custom -o "${DIR}/${2}" ${DEPS}

    # build lib%.a
    "${OCAMLMKLIB}" -v ${4} -custom -o "${DIR}/${2}" ${DIR}/*_stubs.o
  else
    "${OCAMLOPT}" ${4} -o "libs/${1}/${2}.cmxa" ${DEPS}
  fi
}

build_executable()
{
  FLAGS="-I ${1} -open Xc ${FLAGS}"
  DEPS=$(build_sources "${1}" "")
  cd ${1}
  ${OCAMLC} *_stubs.c
  cd ..
  set -x

  "${OCAMLOPT}" -verbose  -o "${1}/${1}" ${2} ${LIBS} ${1}/*_stubs.o ${DEPS}
}

build_library "mmap" "xenmmap" "" ""
build_library "xb" "xenbus" "Xenbus" ""

(cd libs/xc && perl -w xenctrl_abi_check.h xenctrl_stubs.c xenctrl.ml >xenctrl_abi_check.h)

build_library "xc" "xc" "Xc" "-L$(realpath ../libs/call) -lxencall -L$(realpath ../libs/guest) -lxenguest -L$(realpath ../libs/evtchn) -lxenevtchn -L$(realpath ../libs/devicemodel/) -lxendevicemodel -L$(realpath ../libs/gnttab) -lxengnttab -L$(realpath ../libs/foreignmemory/) -lxenforeignmemory $(realpath ../libs/ctrl)/libxenctrl.so"
build_library "eventchn" "xeneventchn" "" "-L$(realpath ../libs/evtchn) -lxenevtchn -L$(realpath ../libs/toollog) -lxentoollog -L$(realpath ../libs/toolcore) -lxentoolcore"

build_executable xenstored "-cclib -Wl,-rpath-link=$(realpath ../libs/call) -cclib -Wl,-rpath-link=$(realpath ../libs/guest) -cclib -Wl,-rpath-link=$(realpath ../libs/evtchn) -cclib -Wl,-rpath-link=$(realpath ../libs/devicemodel/) -cclib -Wl,-rpath-link=$(realpath ../libs/gnttab) -cclib -Wl,-rpath-link=$(realpath ../libs/foreignmemory/) -cclib -Wl,-rpath-link=$(realpath ../libs/ctrl) -I +unix unix.cmxa"
