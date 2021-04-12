# Awesome Desktop

- Komplett konfigurierter Awesome-Desktop.
- Support für Dual-Monitor

Als Basis dient eine Manajaro Full Cinnamon Installation, oder eine vergleichbare.

## Installation

Repository klonen

```bash
git clone https://github.com/Tenvan/desktop-dotfiles.git
```

und in das Homeverzeichnis verschieben:
```bash
shopt -s dotglob
rsync -vrlptgo --include ".*" desktop-dotfiles/* ~/
rm -fr desktop-dotfiles/
```

Fehlende Pakete werden mit folgendem Batch nachinstalliert:

```bash
zsh ~/.scipts/install_init.zsh
zsh ~/.scipts/install_base.zsh
zsh ~/.scipts/install_apps.zsh
zsh ~/.scipts/install_rust_apps.zsh
zsh ~/.scipts/install_finish.zsh
```

## Manuelle Konfiguration

### Monitor Definition erstellen
Mit 'arandr' die Monitoreinstellung vornehmen und in der Datei '~/.screenlayout/screenlayout.sh' speichern.

Das Install-Script nochmal laufen lassen, anschließend neu booten.
