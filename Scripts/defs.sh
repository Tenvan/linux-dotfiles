echo "run: defs.sh"

#####################
# init distro check #
#####################
export LINUX_VERSION_NAME=$(lsb_release -si)
export MAKEFLAGS="-j$(nproc)"

export IS_MANJARO=false
export IS_ARCO=false

if [[ "$LINUX_VERSION_NAME" == *"Manjaro"* ]]; then
    export IS_MANJARO=true
fi

if [[ "$LINUX_VERSION_NAME" == *"ArcoLinux"* ]]; then
    export IS_ARCO=true
fi

echo Manjaro..: $IS_MANJARO
echo ArcoLinux: $IS_ARCO
