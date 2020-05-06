# i3-desktop

- Komplette Konfiguration eines I3-Desktops.
- Fest ausgelegt auf Dual-Monitor
- Umschaltung zwischen Light/Dark Theme durch Git-Branch

Als Basis dient eine Full-I3 Installation durch den Manjaro "Architekt"-Installater, oder eine vergleichbare.

## Installation

Fehlende Pakete werden mit folgendem Batch nachinstalliert:

```
sh ~/i3install.sh
```

## Manuelle Konfiguration

Einzige z.Zt. nötige manuelle Konfiguration ist die Anschlussdefinitiob der beiden Monitore.

Hierfür muss im Homeverzeichnis eine Datei '.Xresources.monitor' angelegt werden, die folgenden Aufbau hat:

```
*monitor1: DP2
*monitor2: DP3
```

Ermittelt werden können diese durch:

```
xrandr | grep connected -w 
```

Rebooten bzw- neu anmdelden nicht vergessen !
