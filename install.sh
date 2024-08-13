#!/bin/bash
cd truncated
for i in *.mid; do
    cp -v "$i" ~/src/daggerfall-unity/Assets/Resources/Songs/"$i.bytes"
done
