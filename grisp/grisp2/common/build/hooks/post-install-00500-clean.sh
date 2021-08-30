#!/bin/sh
set -e -u -x

# Move inner release to top level
export LIB="${OTP_ROOT}/lib"
mv "${LIB}" "${LIB}.old"
mv "${LIB}.old/erlang/"* "${OTP_ROOT}/"
rm -rf "${LIB}.old"

# Remove beam wrapper and strip debug symbols
export BEAM="${OTP_ROOT}/erts-${ERTS_VERSION}/bin/beam"
rm -f "${BEAM}"
arm-rtems5-objcopy -O binary "${BEAM}.smp" "${BEAM}"
rm -f "${BEAM}.smp"

# Remove unused files
rm "${OTP_ROOT}/bin/epmd"

# Create barebox Linux image
rm -rf "${BEAM}.gz"
gzip -k -9 "${BEAM}"
mkimage -A arm -O linux -T kernel -a 0x80200000 -e 0x80200000 -n RTEMS -d "${BEAM}.gz" "${BEAM}.zImage"

# Remove unused artifacts
rm -rf "${BEAM}" "${BEAM}.gz"

# Copy device tree to bin folder (next to image)
cp "${GRISP_TC_ROOT}/fdt/b-dtb/imx6ull-grisp2.dtb" "${OTP_ROOT}/erts-${ERTS_VERSION}/bin/"
