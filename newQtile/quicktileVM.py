import os, re, socket, subprocess
from libqtile import bar, layout, qtile, widget, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.config import EzKey as Key
from libqtile.lazy import lazy

mod = "mod4"
alt = "mod1"
terminal = "alacritty" 
launcher = "dmenu_run"

keys = [
    # Quit and Restart
    Key("M-C-q", lazy.shutdown()),
    Key("M-C-r", lazy.reload_config()),

    # Killin
    Key("M-S-c", lazy.window.kill()),

    Key("M-<Return>", lazy.spawn(terminal)),
    Key("M-p", lazy.spawn(launcher)),
    Key("M-f", lazy.spawn("pcmanfm")),

    # Switch between windows
    Key("M-h", lazy.layout.left() , desc="Move focus to left"),
    Key("M-l", lazy.layout.right(), desc="Move focus to right"),
    Key("M-j", lazy.layout.down() , desc="Move focus down"),
    Key("M-k", lazy.layout.up()   , desc="Move focus up"),
    Key("M-<Space>", lazy.layout.next() , desc="Move window focus to other window"),

    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key("M-S-h", lazy.layout.shuffle_left() , desc="Move window to the left"),
    Key("M-S-l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key("M-S-j", lazy.layout.shuffle_down() , desc="Move window down"),
    Key("M-S-k", lazy.layout.shuffle_up()   , desc="Move window up"),

    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key("M-C-h", lazy.layout.grow_left() , desc="Grow window to the left"),
    Key("M-C-l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key("M-C-j", lazy.layout.grow_down() , desc="Grow window down"),
    Key("M-C-k", lazy.layout.grow_up()   , desc="Grow window up"),
    Key("M-n"  , lazy.layout.normalize() , desc="Reset all window sizes"),

    # Toggle between different layouts as defined below
    Key("M-C-<Tab>", lazy.next_layout(), desc="Toggle between layouts"),
    Key("M-m"    , lazy.window.toggle_fullscreen()),
    Key("M-t"    , lazy.window.toggle_floating(), desc="Toggle floating on the focused window"),
]

# Add key bindings to switch VTs in Wayland.
# We can't check qtile.core.name in default config as it is loaded before qtile is started
# We therefore defer the check until the key binding is run by using .when(func=...)
for vt in range(1, 8):
    keys.append(
        Key(
            f"C-A-{vt}",
            # ["control", "mod1"], f"f{vt}",
            lazy.core.change_vt(vt).when(func=lambda: qtile.core.name == "wayland"),
            desc=f"Switch to VT{vt}",
        )
    )

# groups = [Group(i) for i in "123456789"]
groups = []
group_names = ["1", "2", "3", "4", "5", "6", "7",]
group_labels = [".", "-", "+", "|", "+", "-", ".", ]

group_layouts = ["monadtall", "monadtall", "monadtall", "monadtall", "monadtall", "monadtall", "monadtall", ]

for i in range(len(group_names)):
    groups.append(
        Group(
            name=group_names[i],
            layout=group_layouts[i].lower(),
            label=group_labels[i],
        )
    )

for i in groups:
    keys.extend(
        [
            # mod + group number = switch to group
            Key(
                f"M-{i.name}",
                # [mod],
                # i.name,
                lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name),
            ),
            # mod + shift + group number = move focused window to group
            Key(
                f"M-S-{i.name}",
                # [mod, "shift"],
                # i.name,
                lazy.window.togroup(i.name, switch_group=False),
                desc="Switch to & move focused window to group {}".format(i.name),
            ),
            # mod + shift + group number = switch to & move focused window to group
            Key(
                f"M-C-{i.name}",
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(i.name),
            ),
        ]
    )

default_layout_theme = {
    "margin" : 6,
    "border_width" : 3,
    # "single_border_width" : 0,
    # "single_margin" : 0,
    "border_focus" : "#8BDCFF",
    "border_normal" : "#292d3e"
}

layouts = [
    layout.MonadTall(
        align = 0,
        **default_layout_theme
    ),
    layout.MonadWide(**default_layout_theme),
    layout.Max(),
]

widget_defaults = dict(
    font = "hack",
    fontsize = 14,
    padding = 3,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.Sep(linewidth = 0),
                widget.GroupBox(
                    font = "hack bold",
                    fontsize = 18,
                    rounded = False,
                    borderwidth = 2,
                    highlight_method='line',
                    highlight_color = "#313244",

                    active = "#bac2de",
                    inactive = "#585b70",

                    this_current_screen_border = "#89dceb",
                    this_screen_border = "#f9e2af",
                ),
                widget.Sep(linewidth = 1, padding = 6),
                widget.CurrentLayoutIcon(
                    scale = 0.9,
                    padding = 3,
                ),
                widget.CurrentLayout(),
                widget.Sep(linewidth = 1, padding = 6),
                widget.WindowName(),
                widget.Sep(linewidth = 1, padding = 6),

                # NB Systray is incompatible with Wayland, consider using StatusNotifier instead
                # widget.StatusNotifier(),

                widget.CheckUpdates(
                    custom_command = "zypper --no-refresh lu --best-effort | grep -c 'v |'",
                    distro = "OpenSUSE",
                    no_update_string = "-1",
                    fmt = "SUS: {}",
                    update_intertal = 300,
                ),
                widget.Sep(linewidth = 1, padding = 6),
                widget.Clock(format="%A %d/%m ~ %I:%M %p"),
                widget.Sep(linewidth = 1, padding = 6),
                widget.Systray(),
                widget.Sep(linewidth = 0),
            ],
            24,
            background = "#1e1e2e",
            foreground = "#cdd6f4",
            # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
            # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
        ),
        # You can uncomment this variable if you see that on X11 floating resize/moving is laggy
        # By default we handle these events delayed to already improve performance, however your system might still be struggling
        # This variable is set to None (no cap) by default, but you can set it to 60 to indicate that you limit it to 60 events per second
        # x11_drag_polling_rate = 60,
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
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
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

# xcursor theme (string or None) and size (integer) for Wayland backend
wl_xcursor_theme = None
wl_xcursor_size = 24

@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser('~')
    subprocess.call([home + '/.config/qtile/autostart.sh'])
 
# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
