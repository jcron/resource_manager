#!/bin/bash

. version.properties

if [ -z "$1" ]	
then 
		BUILD=0;
else
		BUILD=$1
fi

[ -d "rel/resource_manager_node_${PREVIOUS_VERSION}" ] || PREVIOUS_VERSION="None"
[ -z "${PREVIOUS_VERSION}" ] && PREVIOUS_VERSION="None"

export RELTOOL_CONFIG=rel/reltool.config
export VERSION_NSH=installer/Version.nsh
sed --in-place "s/\(rel\s*,\s*\"vht_node_ptk5\"\s*,\s*\)\"[^\"]*\"/\1\"${CURRENT_VERSION}\"/" "${RELTOOL_CONFIG}"

echo "# Generated at $(date)" > "${VERSION_NSH}"
echo '!'"define VERSION ${CURRENT_VERSION}" >> "${VERSION_NSH}"
echo '!'"define BUILD ${BUILD}" >> "${VERSION_NSH}"
echo '!'"define PREVIOUS_VERSION ${PREVIOUS_VERSION}" >> "${VERSION_NSH}"
