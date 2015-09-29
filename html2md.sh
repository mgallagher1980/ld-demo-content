#/bin/bash

htmlF="$1"
mdF="${htmlF/html/md}"
echo "$mdF"

curl -L "https://api.cloudconvert.com/convert" \
     -F file=@"$htmlF" \
     -F "apikey=dT1bmP-qVQD4u5BzB4tIa2x0VnQc5MwX9M0jkd2dFOu6ytq97xOaIRK2CaZTLKGUp9VtpjIo1VcYqgsZGsTYVg" \
     -F "input=upload" \
     -F "download=inline" \
     -F "inputformat=html" \
     -F "outputformat=md" \
     -F "converteroptions[output_markdown_syntax]=pandoc" \
      > "$mdF"
