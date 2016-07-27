#!/bin/sh
usage() {
    cat <<HELP
NAME:
   $0 -- {one sentence description}

SYNOPSIS:
  $0 [-h|--help]
  $0 [--verbose]

DESCRIPTION:
   {description here}

  -h  --help      Print this help.
      --verbose   Enables verbose mode.

EXAMPLE:
  {examples if any}

HELP
}

main() {
    SCRIPT_DIR="$(cd $(dirname "$0"); pwd)"
    export PONYO_ROOT=${HOME}/SML/ponyo
    export PATH=${PATH}:${PONYO_ROOT}/bin

    COMMAND=build
    for ARG; do
        case "$ARG" in
            --help) usage; exit 0;;
            --verbose) set -x;;
            run) COMMAND=run;;
            --) break;;
            -*)
                OPTIND=1
                while getopts h OPT "$ARG"; do
                    case "$OPT" in
                        h) usage; exit 0;;
                    esac
                done
                ;;
        esac
    done

    $COMMAND
}

build() {
    (
        cd "${SCRIPT_DIR}"
        ponyo make main.sml -o main
    )
}

run() {
    (
        cd "${SCRIPT_DIR}"
        ./main
    )

}

main "$@"

