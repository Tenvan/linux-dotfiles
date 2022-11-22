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

### Installation Softwarepaket mit folgenden Scripten

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
