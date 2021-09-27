# Awesome Desktop

- Komplett konfigurierter Awesome-Desktop.
- Support für Dual-Monitor

Als Basis dient eine Manajaro Full Cinnamon Installation, oder eine vergleichbare.

## Installation

### Repository klonen

```bash
git clone https://github.com/Tenvan/desktop-dotfiles.git
```

und in das Homeverzeichnis verschieben:
```bash
shopt -s dotglob
rsync -vrlptgo --include ".*" desktop-dotfiles/* ~/
rm -fr desktop-dotfiles/
```

### Vorbereitung der weiteren Installation
Die Scripte ausführbar machen, falls noch nicht geschehen:

```bash
chmod +x .bin/*
chmod +x .scripts/*
```
### Installation Softwarepaket mit folgenden Scripten:

```bash
install_init.zsh
install_base.zsh
install_finish.zsh
```

## Manuelle Konfiguration

### Monitor Definition erstellen
Mit 'arandr' die Monitoreinstellung vornehmen und in der Datei '~/.screenlayout/screenlayout.sh' speichern.

Das Install-Script nochmal laufen lassen, anschließend neu booten.

### Environment konfigurieren

LightDm lädt die User-Konfigurationsdateien in folgender Reihenfolge:

    - .profile [.profile-custom]
    - .xprofile [.xprofile-custom]
    - .XResources
    - .Xkbmap
    - .Xmodmap
    - .xsession [.Xresources-custom, screenlayout, autostart-global, .xsession-custom]
