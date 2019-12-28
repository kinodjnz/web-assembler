#!/bin/sh
cargo web deploy --release
cp -f target/deploy/* docs/
git add docs
git commit -m 'deploy'
