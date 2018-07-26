#!/usr/bin/env bash
curl \
    --user "${CIRCLECI_TOKEN}": \
    --request POST \
    --form revision=ee976107e343459fa1b366ffdd9df2294e19aa5a \
    --form config=@"$1" \
    --form notify=false \
    https://circleci.com/api/v1.1/project/github/grisp/grisp/tree/master
