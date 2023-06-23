#!/usr/bin/env sh

set -eux

export PATH=/home/user/.local/bin:$PATH

BINDIR=$PWD/binaries
STACK_ROOT_DIR=$PWD/.alpine-stack
STACK_WORK_DIR=.alpine-stack-work
STACK_ARGS="--stack-root $STACK_ROOT_DIR --work-dir $STACK_WORK_DIR --system-ghc --no-install-ghc --allow-different-user --local-bin-path $BINDIR --stack-yaml stack-alpine.yaml"


sudo apk add tar git gcc

sudo find -type d -print0 | xargs -0 -I"{}" sudo chmod a+rwx "{}"
sudo chmod -R a+r .
mkdir -p $STACK_ROOT_DIR $STACK_WORK_DIR $BINDIR

# show dry run
stack $STACK_ARGS update
stack $STACK_ARGS build --copy-bins --flag attomail:static --dry-run
# build
stack $STACK_ARGS build --copy-bins --flag attomail:static

# output package-name-and-ver for later stages
stack $STACK_ARGS ls dependencies | grep '^attomail\>' | sed 's/ \+/-/g' > PACKAGE_PLUS_VERSION.txt

set +eux
