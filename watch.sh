#!/bin/sh

echo "Watching files for changes"

while true
do
{ git ls-files; git ls-files . --exclude-standard --others; } | entr -c -d ./build.sh
done


