# !/bin/bash
OLDPATH=$PATH
PATH="${PATH}:${PWD}"
protoc --plugin="protoc-gen-elm=index.js" --elm_out=tests tests/singleEnum.proto
# protoc --plugin="protoc-gen-elm=index.js" --elm_out=tests tests/test.proto
PATH=$OLDPATH