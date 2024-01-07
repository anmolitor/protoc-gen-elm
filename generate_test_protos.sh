# !/bin/bash

set -e

files=$(find tests/proto -type f -printf "%p ")

rm -rf generated
mkdir generated
protoc --proto_path=tests/proto --elm_out=generated --elm_opt=json=all --plugin="protoc-gen-elm=index.js" $files
