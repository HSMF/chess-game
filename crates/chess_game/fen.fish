#!/usr/local/bin/fish


if test (count $argv) -eq 1
    set fen $argv[1]
else
    read fen
end

set -l url https://lichess1.org/export/fen.gif"?"fen=(echo "$fen" | jq -sRr '@uri') 
curl --silent $url | icat
