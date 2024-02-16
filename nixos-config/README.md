# My NixOS config
La verdad no tengo ni idea de lo que estoy haciendo pero lo ando disfrutanto :thumbsUp:

## IMPORTANTE
+ En la carpeta "system" debe haber un archivo "hardware-configuration.nix", pero no lo he subido ya que este archivo es específico de cada ordenador.
Recomiendo simplemente copiar el archivo /etc/nixos/hardware-configuration.nix.

+ Es necesario tener cuidado con los nombres usados en el archivo flake.nix. Dentro de "nixosConfigurations", se debe usar el **hostname** del equipo, no sirve cualquier nombre. Lo mismo sucede para el nombre de dentro de "homeConfigurations", siendo en este caso necesario usar el **username** del equipo.

## Flakes
La configuración hace uso de Flakes, los cuales gestionan tanto la configuración del sistema como home-manager

## Home-manager
También hago uso de home-manager pero a un nivel muy simple, solo lo uso para generar aliases para bash y zsh. La idea será que gestione neovim también pero querría no usarlo mucho ya que no aporta nada realmente importante.
