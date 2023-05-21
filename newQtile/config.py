#
# ░▀▄░▄▀░█▀▄▀█░▄▀▀▄░█▀▀▄░█▀▀▄░█▀▄░░░█▀▀▄░█░▒█░▀█▀░░░█▀▀▄░█▀▀▄░█▀▄
# ░░▒█░░░█░▀░█░█░░█░█░▒█░█▄▄█░█░█░░░█▀▀▄░█░▒█░░█░░░░█▀▀▄░█▄▄█░█░█
# ░▄▀▒▀▄░▀░░▒▀░░▀▀░░▀░░▀░▀░░▀░▀▀░░░░▀▀▀▀░░▀▀▀░░▀░░░░▀▀▀▀░▀░░▀░▀▀░
#
# Ye
#

import os, re, socket, subprocess
from libqtile import qtile, bar, layout, widget, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy

mod = "mod4"
alt = "mod1"
terminal = "alacritty"
rofi = "rofi -modi combi -show combi -display-combi 'Rufos ~>>' -combi-modi run,drun"

keys = [
    # Quit and restart
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    
    # Killin
    Key([mod, "shift"], "c", lazy.window.kill(), desc="Kill focused window"),
    
    # Common stuff
    Key([mod], "Return", lazy.spawn(terminal)),
    Key([mod], "d", lazy.spawn(rofi)),
    Key([mod], "b", lazy.spawn("brave")),
    Key([mod], "f", lazy.spawn("nautilus")),
    Key([], "Print", lazy.spawn("flameshot gui")),
    Key([mod, "shift"], "e", lazy.spawn("emacsclient -c -a 'emacs'")),

    Key([], 'XF86AudioRaiseVolume', lazy.spawn("/home/delta/.config/dunst/notif_scripts/volume_notif.sh up")),
    Key([], 'XF86AudioLowerVolume', lazy.spawn("/home/delta/.config/dunst/notif_scripts/volume_notif.sh down")),

    # CHANGE KEYBOARD LAYOUT AUUUUUUUUUU
    Key([mod], "space", lazy.spawn("/home/delta/.config/scripts/keyboardChanger.sh")),
    
    # Switch between windows
    Key([mod], "h", lazy.layout.left(),   desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(),  desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(),   desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(),     desc="Move focus up"),
    Key([mod], "Tab", lazy.layout.next(), desc="Move window focus to other window"),
    
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod, "control"], "m", lazy.window.toggle_fullscreen()),
    # Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    
    # Toggle between different layouts as defined below
    Key([mod, "control"], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
]

## Workspaces
def init_group_names():
    return [ ("1", {'layout': 'monadtall'}),
             ("2", {'layout': 'monadtall'}),
             ("3", {'layout': 'monadtall'}),
             ("4", {'layout': 'monadtall'}),
             ("5", {'layout': 'monadtall'}),
             ("6", {'layout': 'monadtall'}),
             ("7", {'layout': 'monadtall'}) ]

def init_groups():
    return [Group(name, **kwargs) for name, kwargs in group_names]

if __name__ in ["config", "__main__"]:
    group_names = init_group_names()
    groups = init_groups()

for i, (name, kwargs) in enumerate(group_names, 1):
    keys.append(Key([mod], str(i), lazy.group[name].toscreen()))    # Switch to another group
    keys.append(Key([mod, "shift"], str(i), lazy.window.togroup(name))) # Send current window to another group


## Layouts
def default_layout_theme():
    return {"margin"       : 6,
            "border_width" : 2,
            "single_border_width" : 0,
            "single_margin" : 0,
            "border_focus" : "#8BDCFF",
            "border_normal": "#292d3e"}

layout_theme = default_layout_theme()

layouts = [
    layout.MonadTall(
        align = 0, # Master left side -> 0; Master right size -> 1
        **layout_theme,
        ),
    layout.MonadWide(
        **layout_theme,
        ),
    layout.Max(),

]

## Widgets config
widget_defaults = dict(
    font="Fantasque Sans Mono",
    fontsize=16,
    padding=3,
    background="#1e1e2e",
)
extension_defaults = widget_defaults.copy()

## Shitass bar
screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    highlight_color="#2b2b2f",
                    this_current_screen_border="#cba6f7",

                    borderwidth=2,
                    highlight_method = "line",
                    ),
                widget.CurrentLayout(),
                widget.Prompt(),
                widget.WindowName(),
                # NB Systray is incompatible with Wayland, consider using StatusNotifier instead
                # widget.StatusNotifier(),
                # widget.QuickExit(),
                #widget.Battery(
                #    battery = 0,

                #    update_interval = 120,
                #    ),
                widget.Clock(format="%A %d/%m ~ %I:%M %p"),
                widget.Systray(),
            ],
            24,
            # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
            # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = False
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
        Match(wm_class="discord"),
        Match(wm_class="spotify"),
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser('~')
    subprocess.call([home + '/.config/qtile/autostart.sh'])

# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "Cutie"
