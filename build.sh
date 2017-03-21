#!/bin/bash

#client_id="YISUqq3DQ-5emQ"
#_redirect_uri="https://simplynaoh.github.io/saveddit/"
client_id="IHqVmcphnk2YGw"
_redirect_uri="http://127.0.0.1:8000/"
redirect_uri=$(echo $_redirect_uri | sed -e 's/\//\\\//g')

tmp="/tmp/tmpRequests.elm"

mv src/RedditAPI/Requests.elm $tmp

sed $tmp -e "s/client_id=ThisIsAMagicString/client_id=$client_id/g" \
-e "s/redirect_uri=ThisIsAMagicString/redirect_uri=$redirect_uri/g" > ./src/RedditAPI/Requests.elm

elm make --output=main.js ./src/Main.elm

mv $tmp ./src/RedditAPI/Requests.elm
