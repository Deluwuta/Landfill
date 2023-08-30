local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")

local volume_control = require("ui.wibar.scripts.volume-control")
local kernel_func = require("ui.wibar.scripts.kernel")
local pacman_widget = require("ui.wibar.scripts.pacman-widget.pacman")

-- Widgets

-- Separator (idk bro)
local separator = wibox.widget {
    {
        id = "separator",
        text = " | ",
        widget = wibox.widget.textbox,
    },
    bg = beautiful.wibar_bg,
    fg = beautiful.color9,
    widget = wibox.container.background,
}

local espacer = wibox.widget {
    {
        id = "espaces",
        text = " ",
        widget = wibox.widget.textbox,
    },
    bg = beautiful.transparent,
    widget = wibox.container.background,
}

-- Keyboard map indicator and switcher
local mykeyboardlayout = awful.widget.keyboardlayout()

local battery = wibox.widget({
    {
        {
            id = "bat_templ",
            text = "  92%",
            font = beautiful.font_name .. "bold 10",
            widget = wibox.widget.textbox,
        },
        fg = beautiful.color10,
        widget = wibox.container.background,
    },
    bottom = 2,
    color = beautiful.color10,
    widget = wibox.container.margin,
})

-- Kernel
local kernel = wibox.widget({
    {
        {
            id = "kernel_role",
            text = " " .. kernel_func(),
            font = beautiful.font_name .. "bold 10",
            widget = wibox.widget.textbox,
        },
        fg = beautiful.color5,
        widget = wibox.container.background,
    },
    bottom = 2,
    color = beautiful.color5,
    widget = wibox.container.margin,
})

-- Internet watcher
-- Volume
local volumecfg = volume_control({})

-- Textclock
local mytextclock = wibox.widget({
    {
        {
            id = "clock_role",
            format = "<b>" .. " %a, %d/%m ~ %H:%M" .. "</b>",
            refresh = 30,
            widget = wibox.widget.textclock(),
        },
        fg = beautiful.color12,
        widget = wibox.container.background,
    },
    bottom = 2,
    color = beautiful.color4,
    widget = wibox.container.margin,
})

-- Image 
local image_widget = wibox.widget {
    {
        {
            image = "/home/delta/Pictures/nixos-icon.svg",
            resize = true,
            widget = wibox.widget.imagebox,
        },
        bottom = 2,
        color = beautiful.extra_blue2,
        widget = wibox.container.margin,
    },
    bg = beautiful.wibar_bg,
    shape = gears.shape.octogon,
    widget = wibox.container.background,
}

image_widget:connect_signal("mouse::enter", function (c) c:set_bg(beautiful.extra_blue2) end)
image_widget:connect_signal("mouse::leave", function (c) c:set_bg(beautiful.wibar_bg) end)
image_widget:connect_signal("button::press", function (c) c:set_bg(beautiful.color14 .. "77") end)
image_widget:connect_signal("button::release", function (c, _, _, button)
    c:set_bg(beautiful.extra_blue2)
    if button == 1 then
        awful.spawn("st")
    end
end)

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
    awful.button({ }, 1, function(t) t:view_only() end),
    awful.button({ mod }, 1, function(t)
        if client.focus then
            client.focus:move_to_tag(t)
        end
    end),
    awful.button({ }, 3, awful.tag.viewtoggle),
    awful.button({ mod }, 3, function(t)
                              if client.focus then
                                  client.focus:toggle_tag(t)
                              end
                          end),
    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

-- Function to update tags
local update_tags = function (self, c3)
    local tagicon = self:get_children_by_id('icon_role')[1]
    if c3.selected then
        tagicon.text = c3.name
        self.bg = beautiful.taglist_bg_focus
        self.fg = beautiful.taglist_fg_focus
    elseif #c3:clients() == 0 then
        tagicon.text = c3.name
        self.fg = beautiful.white
    else
        tagicon.text = c3.name
        self.fg = beautiful.taglist_fg_occupied
    end
end


local window_counter = function (c3)
    return #c3:clients()
end

local window_count = {
    {
        widget = wibox.widget.textbox
    },
    widget = wibox.container.background,
    create_callback = function (c3)
        window_counter(c3)
    end,
    update_callback = function (c3)
        window_counter(c3)
    end,
}

-- Taglist widget_template definitions. Just to cleanup a bit
local taglist_template = {
    {
        {
            id = 'icon_role',
            font = beautiful.font_name .. "Bold 12",
            widget = wibox.widget.textbox
        },
        id = 'margin_role',
        margins = 2,
        -- color = beautiful.color11,
        widget = wibox.container.margin
    },
    id = 'background_role',
    widget = wibox.container.background,
    create_callback = function (self, c3, index, objects)
        update_tags(self, c3)
    end,
    update_callback = function(self, c3, index, objects)
        update_tags(self, c3)
    end
}

awful.screen.connect_for_each_screen(function(s)
    -- Each screen has its own tag table.
    -- awful.tag({ "\u{e24f}" }, s, awful.layout.layouts[1])
    awful.tag({"1", "2", "3", "4", "5", "6", "7"}, s, awful.layout.layouts[1])

    -- Custom tags config
    -- local tag1 = awful.tag.add("8", {
    --     layout = awful.layout.suit.floating,
    --     selected = false
    -- })

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
            awful.button({ }, 1, function () awful.layout.inc( 1) end),
            awful.button({ }, 3, function () awful.layout.inc(-1) end),
            awful.button({ }, 4, function () awful.layout.inc( 1) end),
            awful.button({ }, 5, function () awful.layout.inc(-1) end)
        )
    )

    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        style = {
            font = beautiful.font_name .. "Bold 11",
            -- shape = gears.shape.circle,
            spacing = 4,
        },
        widget_template = taglist_template,
        layout = wibox.layout.fixed.horizontal,
        buttons = taglist_buttons
    }

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.focused,
        style = {
            tasklist_disable_icon = true,
        },
    }

    -- Create the wibox
    s.mywibox = awful.wibar({
        screen = s,
        position = "top",
        height = 24
    })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            espacer,
            image_widget,
            separator,
            s.mytaglist,
            separator,
            s.mylayoutbox,
            window_count,
            separator,
            s.mytasklist,
        },
        { -- Middle widgets
            layout = wibox.layout.fixed.horizontal,
        },
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            volumecfg.widget,
            -- mykeyboardlayout,
            pacman_widget(),
            espacer,
            kernel,
            espacer,
            battery,
            espacer,
            mytextclock,
            separator,
            wibox.widget.systray(),
            -- espacer,
        },
    }
end)
