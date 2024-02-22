# !/bin/bash

set -e

INCLUDE=${PROTO_HOME:-/usr/local/include}

mkdir -p google/protobuf/compiler
cp -r $INCLUDE/google/protobuf/ google
files="$(find google/protobuf -type f -printf "%p ")"
protoc --plugin="protoc-gen-elm=${PWD}/index.js" --elm_out=src --elm_opt=json=all $files

# we cannot distinquish between 0 and the default (0) - upstream bug or proto2 limitation?
sed -i "s/ oneofIndex = 0/ oneofIndex = -1/" src/Proto/Google/Protobuf/Internals_.elm
