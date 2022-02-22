#!/bin/bash
set -eu
LAST="${*: -1}"
LASTMODULE="$(basename -s .mli "${LAST}")"
# ocamldep cannot sort these properly because it doesn't see
# the references to other modules inside the GOSPEL annotations
(
(
echo model/stdlib.mli model/string.mli model/buffer.mli model/unix.mli
ocamldep -sort "$@"
)| xargs -n1 echo | while read -r FILENAME; do
    if [ "${FILENAME}" = "${LAST}" ] ; then
        cat "${FILENAME}"
    else
        MODULE="$(basename -s .mli "${FILENAME}")"
 #       echo "# ${LINENO} \"$0\"" # TODO: +1
        echo "module ${MODULE^} : sig"

  #      echo "# 1 \"${FILENAME}\""
        cat "${FILENAME}"

   #     echo "# ${LINENO} \"$0\""
        echo "end"
    fi
done) > "${LASTMODULE}_test.mli"
# the extra model signatures should only be visible to ortac
ortac --frontend=default "${LASTMODULE}_test.mli" >"${LASTMODULE}_rtac.ml"
cp "${LASTMODULE}_test.mli" /tmp
test -s "${LASTMODULE}_rtac.ml"
cp "${LASTMODULE}.mli" "${LASTMODULE}_test.mli"
