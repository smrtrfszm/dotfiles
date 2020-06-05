#!/bin/bash

# Some checks

# Check if run as root
if [ "$EUID" -ne 0 ]; then
	echo "Please run as root!"
	exit
fi

# Check if supported distribution
if [ -f /etc/os-release ]; then
	. /etc/os-release
	OS=$NAME
fi

if [ $OS != "Ubuntu" ]; then
	echo "Not supported distribution!"
	exit
fi



## Add PPAs

# i3-gaps PPA
add-apt-repository -y ppa:kgilmer/speed-ricer

# OpenRazer PPA
add-apt-repository ppa:openrazer/stable

echo 'PPAs added'

# Update apt database
apt update
echo 'Apt datebase updated'



## Install packages

# Install utilities
apt install -y man
echo 'Installed: man'
apt install -y wget
echo 'Installed: wget'
apt install -y curl
echo 'Installed: curl'
apt install -y ffmpeg
echo 'Installed: ffmpeg'

# Install i3-gaps (require ppa)
apt install -y i3-gaps
echo 'Installed: i3-gaps'

# Install OpenRazer drivers
apt install -y openrazer-meta
echo 'Installed: OpenRazer drivers'

# Install neovim
apt install -y neovim
echo 'Installed: neovim'

# Install exa
wget -q -O exa https://github.com/ogham/exa/releases/download/v0.9.0/exa-linux-x86_64-0.9.0.zip
unzip ./exa.zip
mv ./exa-linux-x86_64 /usr/local/bin/exa
rm -f exa.zip
# Exa man page
wget -q -O /usr/share/man/man1/exa.1.gz https://raw.githubusercontent.com/ogham/exa/master/contrib/man/exa.1
echo 'Installed: exa'

# Install qutebrowser
apt install -y qutebrowser
echo 'Installed: qutebrowser'

# Install alacritty
apt install -y alacritty
echo 'Installed: alacritty'

# Install fish shell
apt install -y fish
echo 'Installed: fish shell'
