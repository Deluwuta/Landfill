# Literally a file to manage the Openbox config
Openbox's config files are huge, fuck XML. This file will try to explain them a bit, spliting them by XML top node and like that being able to manage the file with more ease.

- [Openbox official docs](https://openbox.org/help/COnfiguration)

## rc.xml
Main file. In here we define themes, keybindings, mouse actions and more (my god). It has the following top nodes:
- Resistance: basically controls the overlapping
- Focus
- Placement
- **Theme**
- Desktops
- Resize
- Margins
- **Keyboard**
- **Mouse**
- Menu
- Applications

What '-'

## menu.xml
There is a separate file (300+ lines) just to manage the right click menu. Yes.

## autostart
The only file that I understand completely XD

## environment
To set up environment variables like language or something. Interesting.

## THA BAR
This is not a file per se but I will try to use Tint2 as the main bar. That's all
