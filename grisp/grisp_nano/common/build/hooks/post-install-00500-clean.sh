#!/bin/sh
set -e -u -x

# Move inner release to top level
export LIB="${OTP_ROOT}/lib"
mv "${LIB}" "${LIB}.old"
mv "${LIB}.old/erlang/"* "${OTP_ROOT}/"
rm -rf "${LIB}.old"

# Remove beam wrapper and strip debug symbols
export BEAM="${OTP_ROOT}/erts-${ERTS_VERSION}/bin/beam"
rm -f "${BEAM}.bin"
arm-rtems6-objcopy -O binary "${BEAM}.smp" "${BEAM}.bin"
