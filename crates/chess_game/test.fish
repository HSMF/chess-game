#!/usr/local/bin/fish

set -gx RUSTFLAGS "-C instrument-coverage=all"
cargo test
grcov . -s . --binary-path ../../target/debug/ -t lcov,markdown --branch --ignore-not-existing -o ../../target/debug/ --keep-only 'src/**/*'
genhtml -q -o ../../target/debug/coverage/ --show-details --highlight --ignore-errors source --legend ../../target/debug/lcov

if contains -- --open $argv
    open ../../target/debug/coverage/index.html
end

command rm -f default_*.profraw

rg '\s' ../../target/debug/markdown.md -r '$1' | uhead -n-1 | utail -n+2 | xsv select -d '|' 2,3 | xsv table | bat -P --file-name "coverage"
