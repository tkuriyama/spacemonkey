
stack install
spacemonkey-make >&2

echo "\nMoving Elm files...\n" >&2
mv -v *.elm ~/Dropbox/CS/Repos/spacemonkey/client/src/ >&2

# echo "\nMaking Elm files...\n"
# cd ~/Dropbox/CS/Repos/spacemonkey/client
# elm make src/Main.elm --optimize --output=elm.js
# uglifyjs elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=elm.min.js
