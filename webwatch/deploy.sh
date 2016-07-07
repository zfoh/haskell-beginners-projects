#!/bin/bash
set -o nounset -o errexit -o pipefail

KEYPAIR="$1"
HOST="$2"

function copy() {
    rsync -ave "ssh -i $KEYPAIR" "$1" ec2-user@"$HOST": --progress
}

function cmd() {
    ssh -tt -i "$KEYPAIR" ec2-user@"$HOST" "$1"
}

copy "$(which webwatch)"
copy "webwatch.conf"
copy "webwatch.service"
cmd "sudo cp webwatch.service /etc/systemd/system"
cmd "sudo systemctl enable webwatch"
