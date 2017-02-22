#!/bin/bash

client_id="YISUqq3DQ-5emQ"
_redirect_uri="https://simplynaoh.github.io/saveddit/"
redirect_uri=$(echo $_redirect_uri | sed -e 's/\//\\\//g')

tmpMain="/tmp/tmpMain.elm"

sed src/Main.elm -e "s/client_id=ThisIsAMagicString/client_id=$client_id/g" \
-e "s/redirect_uri=ThisIsAMagicString/redirect_uri=$redirect_uri/g" > $tmpMain

elm make --output=main.js $tmpMain

rm $tmpMain
