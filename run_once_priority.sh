#!/usr/bin/env bash

# Run the script with the highest priority
install_on_fedora() {
    echo "Installing Ansible on Fedora..."
    sudo dnf install -y ansible
    if [ $? -ne 0 ]; then
        echo "Failed to install Ansible on Fedora"
        exit 1
    fi
}

install_on_ubuntu() {
    echo "Installing Ansible on Ubuntu..."
    sudo apt-get update
    sudo apt-get install -y ansible
    if [ $? -ne 0 ]; then
        echo "Failed to install Ansible on Ubuntu"
        exit 1
    fi
}

install_on_arch() {
    echo "Installing Ansible on Arch-based distro..."
    sudo pacman -Syu --noconfirm ansible
    sudo add-apt-repository universe
    if [ $? -ne 0 ]; then
        echo "Failed to install Ansible on Arch-based distro"
        exit 1
    fi
}

install_on_mac() {
    echo "Installing Ansible on macOS..."
    brew install ansible
    if [ $? -ne 0 ]; then
        echo "Failed to install Ansible on macOS"
        exit 1
    fi
}

OS="$(uname -s)"
case "${OS}" in
    Linux*)
        if [ -f /etc/os-release ] && grep -q "ID=arch" /etc/os-release; then
            install_on_arch
        elif [ -f /etc/fedora-release ]; then
            install_on_fedora
        elif [ -f /etc/lsb-release ]; then
            install_on_ubuntu
        else
            echo "Unsupported Linux distribution"
            exit 1
        fi
        ;;
    Darwin*)
        install_on_mac
        ;;
    *)
        echo "Unsupported operating system: ${OS}"
        exit 1
        ;;
esac

ansible-playbook ~/.bootstrap/setup.yml --ask-become-pass

echo "Ansible installation complete."

# Run the script with the second highest priority

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

source $HOME/.cargo/env
source ~/.profile

# Install cargo binstall
curl -L --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/cargo-bins/cargo-binstall/main/install-from-binstall-release.sh | bash

# Install lsd
cargo binstall lsd

