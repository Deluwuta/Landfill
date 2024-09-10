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
- [x] Brightness and volume popups on change
- [ ] Clean readable code (long term objective)

### QoL
- [x] Pretty tags
- [ ] Good floating capabilities
- [ ] Screen locker
- [x] Useful notifications

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

### 02/09/2024

Remade the folder structure (last time with some luck). I have moved to its own file the error_handling code and the layouts, but the later is not used in rc.lua because I don't know if I'll keep the file. I also improved the volume osd widget, being updated only on command. Still has some delay, but it works.

The widgets/temporal folder is used to hold the templates or files that I'm using to test stuff, just code to be copied '-'.

Finally I cloned the bling repo because I like some things, may or may not use it but there it is (it may not be included in the commit)

The next thing I wanna do is keep moving code out of the main file, focusing on keybinding and signals. I also have to make the osd for brightness and keyboard brightness.

### 04/09/2024

I've made the global and client binding files where all the key and mouse bindings are compiled. I don't want to **over**modularize the configs, so I don't differentiate between key (key) and mouse (button) bindings. I think is simplier this way.

I also changed a bit of the code of the volume slider to update the number when using the slider to change the volume (I hope it works, not tested). I also added an appropiate icon :D.

Next day, both brightness and keyboard brightness osd. Luckily it will not only be straightforward, but I also will be able to generalise the osd code to just call a function that does everything.

When the osd's are completed, I guess I'll focus on the taskbars or maybe I'll tweak the wibar again, mainly the tags.

I believe I completed the brightness osd. It is SO messy that I'll try cleaning the code before making the last osd. But yeah, that's it for today.

### 06/09/2024

Osd template file done. The idea was to just call the functions inside of the global_binds file, but its better to have their own files and I just took out the general code. Anyway, I have to revisit the osd_helper file and see if it all makes sense or if I can merge both files or something.

### 08/09/2024

I think I can say the taglist is done (88888). Next is the notifications, I want to move their code to its own file (for theme and shape) and I'll start with the notification center I guess, but yeah this is almost done. I also created the biscuit theme file and I'll try to keep it consistent if I make (steal) more themes.

### 10/09/2024

Today I can say this is in an "stable" phase, where everything important should work and is, in general, usable. I have to fix some keybindings and make the kbdBrightness switch script, but right know I have everything I wanted and I think is pretty.

Next would be better titlebars, control center and some things like using Bling modules and maybe keep tinkering with the wibar.
