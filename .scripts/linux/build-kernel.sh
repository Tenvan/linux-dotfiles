#!/usr/bin/env bash
. ./.configrc

popd kernel/ || exit

make -j14
make modules -j12
make bzImage

popd || exit
