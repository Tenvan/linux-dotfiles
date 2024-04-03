# Linux DotFiles ZSH

- Konfiguration auf RedHat Fedora und Archlinux basierende Distros
- Schwerpunkt liegt auf der Fedora und ZSH-Shell
- Unterstützte Desktops:
  - KDE/Plasma
  - AwesomeWM

Alle Benötigten Paket werden mit mehreren Script - basierend auf Ansible - installiert.

## Vorbereitungen
### Debian

In Debian den Main-User sudo Rechte geben
```bash
su -c "export $USERNAME=$USER; bash"
echo "$USERNAME ALL=(ALL) NOPASSWD: ALL"  >> /etc/sudoers.d/mainuser
```


## Installation

Repository klonen und in den Home-Ordner verschieben:

```bash
shopt -s dotglob
export repo=linux-dotfiles
git clone https://github.com/Tenvan/$repo.git
rsync -vrlptgo --include ".*" $repo/* ~/
rm -fr $repo/
```

Die benötigten Pakete werden mit folgenden Scripten installiert:
```bash
~/.scripts/install_ansible
~/.scripts/install_system
~/.scripts/install_workstation
~/.scripts/install_theme
~/.scripts/install_user
```

Optional:

```bash
~/.scripts/install_nvidia
ansible-playbook ~/.ansible/plasma.yml
ansible-playbook ~/.ansible/jetbrains.yml
```
