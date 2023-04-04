# !/bin/bash

set -e

INCLUDE=${PROTO_HOME:-/usr/local/include}

mkdir -p google/protobuf/compiler
cat $INCLUDE/google/protobuf/descriptor.proto > google/protobuf/descriptor.proto
cat $INCLUDE/google/protobuf/compiler/plugin.proto > google/protobuf/compiler/plugin.proto
protoc --plugin="protoc-gen-elm=${PWD}/index.js" --elm_out=src google/protobuf/compiler/plugin.proto google/protobuf/descriptor.proto


# we cannot distinquish between 0 and the default (0) - upstream bug or proto2 limitation?
sed -i "s/ oneofIndex = 0/ oneofIndex = -1/" src/Proto/Google/Protobuf/Internals_.elm
