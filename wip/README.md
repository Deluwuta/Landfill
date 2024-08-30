# Jijijija

Awesome rewrite to remember how this config works. I'll also use this as an excuse to clean up my old (v5) config

## Dependencies

This is subject to change because of the activity involved at this point (26/08/2024)
+ **ACPI** for battery widget
+ **nm-applet**
+ **brightnessctl** for brightness control
+ **amixer** for audio control
+ **xwallpaper**
+ **redshift**

## TODO
### Must be done
- [x] Battery widget (wanna do something cute, but the objective is functional minimalism)
- [x] Internet widget (same idea as with the battery)
- [-] Brightness and volume popups on change
- [ ] Clean readable code (long term objective)

### QoL
- [ ] Pretty tags
- [ ] Good floating capabilities
- [ ] Screen locker
- [ ] Useful notifications

### Fun
- [ ] Dashboard
- [ ] Notification center
- [ ] Theme switcher
- [ ] Custom logout popup

## Special thanks

+ [Amitabha37377 dots](https://github.com/Amitabha37377/Awful-DOTS?tab=readme-ov-file). I'm going to ~~steal~~ get inspiration from their configs.
+ Bling (Modules)
+ Streetturtle (Awesome widgets)

## Roadmap

### 26/08/2024

Creation of this file and the config as a whole. Right now is pretty functional, but lacks some features like battery or internet connection state on wibar (those are a must for QoL).

It is using the default awesome config with modified keybinding from my old configs and the theme config as been built from the ground up again, not finished yet.

### 27/08/2024

Today I focused on the wibar and the folder structure. I've modularized the wibar code and changed almost all of it. I also added two custom widgets, one of an icon that could be used to show a dashboard and the battery widget. I'd like to modify this one but it works right now.

The next day I would like to focus on the notifications styling or maybe I'll continue modularising the wibar code. I also have to make a proper theme file and use it as a base for the rest of the configs.

### 30/08/2024

nm-applet is used for internet handling. A volume osd popup has been added with a color.lua as a template. The binding for the volume is also a template just for testing. The next day I'll do the osd for the brightness, fix the bindings, add the icons (maybe) and maybe keep working on the wibar / modularising the code.
