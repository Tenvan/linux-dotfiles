# Linux DotFiles ZSH

- Konfiguration auf RedHat Fedora und Archlinux basierende Distros
- Schwerpunkt liegt auf der Fedora und ZSH-Shell
- Unterstützte Desktops:
  - KDE/Plasma
  - AwesomeWM

Alle Benötigten Paket werden mit mehreren Script - basierend auf Ansible - installiert.

## Installation

Repository klonen und in den Home-Ordner verschieben:

```bash
shopt -s dotglob
eport repo=linux-dotfiles
git clone https://github.com/Tenvan/$repo.git
rsync -vrlptgo --include ".*" $repo/* ~/
rm -fr $repo/
```

Die benötigten Pakete werden mit folgenden Scripten installiert:
```bash
~/.script/install_system
~/.script/install_workstation
~/.script/install_theme
~/.script/install_user
```

Optional:

```bash
~/.script/install_nvidia
~/.script/install_workstation_awesome
```
