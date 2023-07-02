# Awesome Desktop

- Komplett konfigurierter Awesome-Desktop.
- Support für Dual-Monitor

Als Basis dient eine eine beliebige ArchLinux/Manjaro Installation.
Alle benötigten Packete werden mit Scripts nach installiert.

## Vorbereitungen

Für die Installation und Betrieb ist es Notwendig, dass der Hauptuser *root* bzw. *sudo* Rechte hat.
Am besten erreicht man dies, in dem er den *sudoers' zugeordnet wird. Hierfür gibt es je nach Distro unterschiedliche Gruppen.

### Archlinux, Rehat

Gruppe: wheel

```bash
su -c '/usr/sbin/usermod -aG wheel $USER; echo $USER ist jetzt sudoer'
reboot
```

### Dedian

Gruppe: sudo

```bash
su -c '/usr/sbin/usermod -aG sudo $USER; echo $USER ist jetzt sudoer'
reboot
```

## Installation

### Repository klonen

Git und rsync müssen evtl. vorher noch manuell installiert werden, je na Distro und Installation.

```bash
git clone https://github.com/Tenvan/desktop-dotfiles.git
```

und in das Homeverzeichnis verschieben:

```bash
shopt -s dotglob
rsync -vrlptgo --include ".*" desktop-dotfiles/* ~/
rm -fr desktop-dotfiles/
```

Anschliessen noch in der Shell neu anmelden oder die Terminalanwendung neu starten.

### Installation Softwarepaket

Die Installation erfolgt mithilfe folgender Scripte in genau dieser Reihenfolge.
Dabei sind alle ausser das erste Script optional.

#### Basis System

Installierung dess Basissystems, das auch als Server-Only Installation verwendet werden kann.

```bash
./.scripts/install_system
```

#### Desktop System

Installierung des Desktop-Parts (AWESOMEWM basierend).

```bash
./.scripts/install_workstation
```

#### Benutzer Einstellungen

Konfiguriert das System neu anhand der vorgenommen Konfiguration (Umgebungsvariablen).
Dieser Schritt kann bzw. muss jedesmal ausgeführt werden, wenn an der Themekonfiguation etwas geändert wurde.

```bash
./.scripts/install_user
```

#### NVIDIA Treiber Instllation

Die propiritären NVIDIA Treiber können mit folgendem Script installiert werden.
Dies ist aber nur für bestimmte Linux-Distro erforderlich, die keine eigene Treiberveraltung haben (z.B. Fedora).

```bash
./.scripts/install_nvidia
```

## Manuelle Konfiguration

### Standard Shell auf ZSH setzen

Mit folgender Anweisung wird die Standard-Shell des angemeldeten Users auf ZSH geändert.

```bash
chsh -s $(command -v zsh)
```

### Monitor Definition erstellen

Mit 'arandr' die Monitoreinstellung vornehmen und in der Datei '~/.screenlayout/screenlayout-system.sh' speichern.

Das Install-Script nochmal laufen lassen, anschließend neu booten.

### Environment konfigurieren

LightDm lädt die User-Konfigurationsdateien in folgender Reihenfolge:

- .profile
- .xprofile
- .XResources
- .Xkbmap
- .Xmodmap
- .xsession

## Verschiedenes

### notify-send Hints

| Name             | Value Type | Description                                                                                                                                                                                                                                                                                                                             |
|------------------|------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| "urgency"        | byte       | The urgency level.                                                                                                                                                                                                                                                                                                                      |
| "urgency"        | byte       | The urgency level.                                                                                                                                                                                                                                                                                                                      |
| "category"       | string     | The type of notification this is.                                                                                                                                                                                                                                                                                                       |
| "desktop-entry"  | string     | This specifies the name of the desktop filename representing the calling program. This should be the same as the prefix used for the application's .desktop file. An example would be "rhythmbox" from "rhythmbox.desktop". This can be used by the daemon to retrieve the correct icon for the application, for logging purposes, etc. |
| "image_data"     | (iiibiiay) | This is a raw data image format which describes the width, height, rowstride, has alpha, bits per sample, channels and image data respectively. We use this value if the icon field is left blank.                                                                                                                                      |
| "sound-file"     | string     | The path to a sound file to play  when the notification pops up.                                                                                                                                                                                                                                                                        |
| "suppress-sound" | boolean    | Causes the server to suppress playing any sounds, if it has that ability. This is usually set when the client itself is going to play its own sound.                                                                                                                                                                                    |
| "x"              | int        | Specifies the X location on the screen that the notification should point to. The "y" hint must also be specified.                                                                                                                                                                                                                      |
| "y"              | int        | Specifies the Y location on the screen that the notification should point to. The "x" hint must also be specified.                                                                                                                                                                                                                      |

### notify-send Kategorien

| Type                   | Description                                                                                                                                                   |
|------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------|
| "device"               | A generic device-related notification that  doesn't fit into any other category.                                                                              |
| "device.added"         | A device, such as a USB device, was added to the system.                                                                                                      |
| "device.error"         | A device had some kind of error.                                                                                                                              |
| "device.removed"       | A device, such as a USB device, was removed  from the system.                                                                                                 |
| "email"                | A generic e-mail-related notification that  doesn't fit into any other category.                                                                              |
| "email.arrived"        | A new e-mail notification.                                                                                                                                    |
| "email.bounced"        | A notification stating that an e-mail has  bounced.                                                                                                           |
| "im"                   | A generic instant message-related  notification that doesn't fit into any other  category.                                                                    |
| "im.error"             | An instant message error notification.                                                                                                                        |
| "im.received"          | A received instant message notification.                                                                                                                      |
| "network"              | A generic network notification that doesn't  fit into any other category.                                                                                     |
| "network.connected"    | A network connection notification, such as  successful sign-on to a network service. This  should not be confused with device.added for  new network devices. |
| "network.disconnected" | A network disconnected notification. This should not be confused with device.removed  for disconnected network devices.                                       |
| "network.error"        | A network-related or connection-related  error.                                                                                                               |
| "presence"             | A generic presence change notification that doesn't fit into any other category, such as going away or idle.                                                  |
| "presence.offline"     | An offline presence change notification.                                                                                                                      |
| "presence.online"      | An online presence change notification.                                                                                                                       |
| "transfer"             | A generic file transfer or download notification that doesn't fit into any other category.                                                                    |
| "transfer.complete"    | A file transfer or download complete  notification.                                                                                                           |
| "transfer.error"       | A file transfer or download error.                                                                                                                            |
