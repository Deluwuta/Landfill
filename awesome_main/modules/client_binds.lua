local awful = require("awful")

local vars = require("utils.user_variables")

-- client.connect_signal("request::default_keybindings", function ()
-- Both awful.something where inside of here in the default config. I don't know why, but everything seems to work rn
-- end)

local mod = vars.mod

awful.keyboard.append_client_keybindings({
    awful.key({
        modifiers = { mod, "Shift" },
        key = "c",
        description = "KILL",
        group = "client",
        on_press = function (c)
            c:kill()
        end
    }),

    awful.key({
        modifiers = { mod, "Control" },
        key = "space",
        description = "toggle floating",
        group = "client",
        on_press = awful.client.floating.toggle
    }),

    awful.key({
        modifiers = { mod, "Control" },
        key = "Return",
        description = "move focused client to master",
        group = "client",
        on_press = function (c)
            c:swap(awful.client.getmaster())
        end
    }),

    -- Idk what it does
    awful.key({
        modifiers = { mod },
        key = "o",
        description = "move to screen",
        group = "client",
        on_press = function (c)
            c:move_to_screen()
        end
    }),

    awful.key({
        modifiers = { mod, "Control" },
        key = "t",
        description = "toggle keep on top",
        group = "client",
        on_press = function (c)
            c.ontop = not c.ontop
        end
    }),

    -- Client gaming. mod+Control+n to unminimize
    awful.key({
        modifiers = { mod },
        key = "n",
        description = "minimize client",
        group = "client",
        on_press = function (c)
            c.minimized = true
        end
    }),

    -- There are *many* ways of making a window fullscreen

    -- This way the window take the whole screen but the bar
    awful.key({
        modifiers = { mod },
        key = "m",
        description = "(un)maximize",
        group = "client",
        on_press = function (c)
            c.maximized = not c.maximized
            c:raise()
        end
    }),

    -- May have uses in some layouts
    awful.key({
        modifiers = { mod, "Shift" },
        key = "m",
        description = "(un)maximize horizontally",
        group = "client",
        on_press = function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end
    }),

    -- This way the window takes the whole screen
    awful.key({
        modifiers = { mod, "Control" },
        key = "m",
        description = "toggle fullscreen",
        group = "client",
        on_press = function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end
    }),
})

-- Mouse bindings
awful.mouse.append_client_mousebindings({
    awful.button({
        modifiers = { },
        button = 1,
        on_press = function (c)
            c:activate {
                context = "mouse_click"
            }
        end
    }),

    awful.button({
        modifiers = { mod },
        button = 1,
        on_press = function (c)
            c:activate {
                context = "mouse_click",
                action = "mouse_move",
            }
        end
    }),

    awful.button({
        modifiers = { mod },
        button = 2,
        on_press = function (c)
            c:kill()
        end
    }),

    awful.button({
        modifiers = { mod },
        button = 3,
        on_press = function (c)
            c:activate {
                context = "mouse_click",
                action = "mouse_resize",
            }
        end
    }),
})
