local awful = require("awful")
local gears = require("gears")
local hotkeys_popup = require("awful.hotkeys_popup")

local HOME = os.getenv("HOME")

local vars = require("utils.user_variables")

-- Volume change
local _a = require("widgets.volume_osd")
local volume_osd, update_volume, volume_timer = table.unpack(_a)

local _b = require("widgets.brightness_osd")
local brightness_osd, update_bright, brightness_timer = table.unpack(_b)

local mod = vars.mod
local alt = vars.alt

-- Helpers
local function swap_client(t1, t2)
    if not t1 or not t2 then return end

    local clients1 = t1:clients()
    local clients2 = t2:clients()

    for _, c in ipairs(clients1) do
        c:move_to_tag(t2)
    end

    for _, c in ipairs(clients2) do
        c:move_to_tag(t1)
    end
end


awful.keyboard.append_global_keybindings({
    -- Quit and restarting
    awful.key({ mod, "Control" }, "q", awesome.quit,    { description = "quit awesome", group = "awesome" }),
    awful.key({ mod, "Control" }, "r", awesome.restart, { description = "reload awesome", group = "awesome" }),

    -- Common apps
    awful.key({ mod }, "Return"    , function() awful.spawn(vars.terminal)     end, { description = "launch terminal", group = "apps" }),
    awful.key({ mod }, "p"         , function() awful.spawn(vars.launcher)     end, { description = "app launcher", group = "launcher" }),
    awful.key({ mod }, "b"         , function() awful.spawn(vars.browser)      end, { description = "internet browser", group = "apps" }),
    awful.key({ mod }, "f"         , function() awful.spawn(vars.file_manager) end, { description = "file manager", group = "apps" }),
    awful.key({ mod, "Shift" }, "s", function() awful.spawn("flameshot gui")   end, { description = "screenshoter", group = "apps" }),

    -- Multimedia
    awful.key({
        modifiers = { },
        key = "XF86AudioRaiseVolume",
        description = "Raise volume",
        group = "multimedia",
        on_press = function ()
            awful.spawn("amixer set Master 2%+ unmute")
            update_volume()
            volume_timer:again()
            volume_osd.visible = true
        end,
    }),

    awful.key({
        modifiers = { },
        key = "XF86AudioLowerVolume",
        description = "Lower volume",
        group = "multimedia",
        on_press = function ()
            awful.spawn("amixer set Master 2%- unmute")
            update_volume()
            volume_timer:again()
            volume_osd.visible = true
        end,
    }),

    awful.key({
        modifiers = { alt },
        key = "m",
        description = "Raise volume",
        group = "multimedia",
        on_press = function ()
            awful.spawn("amixer set Master 2%+ unmute")
            update_volume()
            volume_timer:again()
            volume_osd.visible = true
        end,
    }),

    awful.key({
        modifiers = { alt },
        key = "n",
        description = "Raise volume",
        group = "multimedia",
        on_press = function ()
            awful.spawn("amixer set Master 2%- unmute")
            update_volume()
            volume_timer:again()
            volume_osd.visible = true
        end,
    }),

    awful.key({
        modifiers = { },
        key = "XF86MonBrightnessUp",
        description = "Raise brightness",
        group = "multimedia",
        on_press = function ()
            awful.spawn("brightnessctl s 5+")
            update_bright()
            brightness_timer:again()
            brightness_osd.visible = true
        end,
    }),

    awful.key({
        modifiers = { },
        key = "XF86MonBrightnessDown",
        description = "Lower brightness",
        group = "multimedia",
        on_press = function ()
            awful.spawn("brightnessctl s 5-")
            update_bright()
            brightness_timer:again()
            brightness_osd.visible = true
        end,
    }),

    awful.key({
        modifiers = { },
        key = "XF86KbdBrightnessUp",
        description = "Raise keyboard brightness",
        group = "multimedia",
        on_pres = function ()
            awful.spawn(HOME .. "./config/scripts/kbdbacklight.sh up")
        end,
    }),

    awful.key({
        modifiers = { },
        key = "XF86KbdBrightnessDown",
        description = "Lower keyboard brightness",
        group = "multimedia",
        on_pres = function ()
            awful.spawn(HOME .. "./config/scripts/kbdbacklight.sh down")
        end,
    }),

    awful.key({
        modifiers = { mod },
        key = "space",
        description = "Change keyboard layout",
        group = "utilities",
        on_pres = function ()
            awful.spawn(HOME .. "./config/scripts/keyboardChanger.sh")
        end,
    }),


    -- Tags related keybindings
    awful.key({ mod }, "Left", awful.tag.viewprev),
    awful.key({ mod }, "Right", awful.tag.viewnext),
    awful.key({ mod }, "Escape", awful.tag.history.restore),

    -- Focus related keybindings
    awful.key({ mod }, "k", function() awful.client.focus.byidx(1) end),
    awful.key({ mod }, "j", function() awful.client.focus.byidx(-1) end),

    awful.key({
        modifiers = { alt },
        key = "Tab",
        description = "Rotate backwards between clients",
        group = "client",
        on_press = function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
    }),

    -- Restore minimized clients (mod+n to minimize)
    awful.key({
        modifiers = { mod, "Control" },
        key = "n",
        description = "restore minimized",
        group = "client",
        on_press = function ()
            local c = awful.client.restore()
            if c then
                c:activate {
                    raise = true,
                    context = "key.unminimize",
                }
            end
        end
    }),

    -- Multimonitor bindings (?)
    awful.key({ mod, "Control" }, "k", function() awful.screen.focus_relative(1) end),
    awful.key({ mod, "Control" }, "j", function() awful.screen.focus_relative(-1) end),

    -- Layout related bindings
    awful.key({ mod, "Shift" }, "k", function() awful.client.swap.byidx(1) end),
    awful.key({ mod, "Shift" }, "j", function() awful.client.swap.byidx(-1) end),

    awful.key({ mod }, "u", awful.client.urgent.jumpto),

    awful.key({ mod }, "l", function() awful.tag.incmwfact(0.05) end),
    awful.key({ mod }, "h", function() awful.tag.incmwfact(-0.05) end),

    -- Layout switching
    awful.key({ mod }, "Tab", function() awful.layout.inc(1) end),
    awful.key({ mod, "Shift" }, "Tab", function() awful.layout.inc(-1) end),

    -- Awesome specific utilities
    awful.key({
        modifiers = { mod, "Control" },
        key = "s",
        description = "show help",
        group = "awesome",
        on_press = hotkeys_popup.show_help,
    }),

    awful.key({
        modifiers = { mod },
        key = "s",
        description = "swap clients between screens",
        group = "awesome",
        on_press = function ()
            local screen1 = screen[1]
            local screen2 = screen[2]

            if not (screen1 and screen2) then return end

            local t1 = screen1.selected_tag
            local t2 = screen2.selected_tag

            if t1 and t2 then
                swap_client(t1, t2)
            end
        end
    })

    -- awful.key({
    --     modifiers = { mod },
    --     key = "w",
    --     description = "show main menu",
    --     group = "awesome",
    --     on_press = function()
    --         mymainmenu:show()
    --     end
    -- }),

    -- awful.key({
    --     modifiers = { mod },
    --     key = "x",
    --     description = "lua execute prompt",
    --     group = "awesome",
    --     on_press = function ()
    --         awful.prompt.run {
    --             prompt = "Run Lua code: ",
    --             textbox = awful.screen.focused().mypromptbox.widget,
    --             exe_callback = awful.util.eval,
    --             history_path = awful.util.get_cache_dir() .. "/history_eval",
    --         }
    --     end,
    -- })
})

-- Tag related bindings. Modifier(s)+number
-- awful.keyboard.append_global_keybindings({
--     awful.key {
--         modifiers = { mod },
--         keygroup = "numrow",
--         description = "only view tag",
--         group = "tag",
--         on_press = function (index)
--             local screen = awful.screen.focused()
--             local tag = screen.tags[index]
--             if tag then
--                 tag:view_only()
--             end
--         end,
--     },
--
--     awful.key {
--         modifiers = { mod, "Control" },
--         keygroup = "numrow",
--         description = "toggle tag",
--         group = "tag",
--         on_press = function (index)
--             local screen = awful.screen.focused()
--             local tag = screen.tags[index]
--             if tag then
--                 awful.tag.viewtoggle(tag)
--             end
--         end,
--     },
--
--     awful.key {
--         modifiers = { mod, "Shift" },
--         keygroup = "numrow",
--         description = "move focused client to tag",
--         group = "tag",
--         on_press = function (index)
--             if client.focus then
--                 local tag = client.focus.screen.tags[index]
--                 if tag then
--                     client.focus:move_to_tag(tag)
--                 end
--             end
--         end,
--     },
--
--     awful.key {
--         modifiers = { mod, "Control", "Shift" },
--         keygroup = "numrow",
--         description = "toggle focused client on tag",
--         group = "tag",
--         on_press = function (index)
--             if client.focus then
--                 local tag = client.focus.screen.tags[index]
--                 if tag then
--                     client.focus:toggle_tag(tag)
--                 end
--             end
--         end,
--     },
--
--     -- This is cool. I don't have a numpad
--     awful.key {
--         modifiers   = { mod },
--         keygroup    = "numpad",
--         description = "select layout directly",
--         group       = "layout",
--         on_press    = function (index)
--             local t = awful.screen.focused().selected_tag
--             if t then
--                 t.layout = t.layouts[index] or t.layout
--             end
--         end,
--     }
-- })

awful.mouse.append_global_mousebindings({
    -- Right click menu
    -- awful.button({
    --     modifiers = { },
    --     button = 3,
    --     on_press = function ()
    --         mymainmenu:toggle()
    --     end
    -- }),

    -- Change workspaces with background scroll
    -- awful.button({
    --     modifiers = { },
    --     button = 4,
    --     on_press = awful.tag.viewprev
    -- }),
    --
    -- awful.button({
    --     modifiers = { },
    --     button = 5,
    --     on_press = awful.tag.viewnext
    -- })
})
