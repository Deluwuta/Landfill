local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")

local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

-- local widgets = require("ui.wibar.widgets.control_center")

-- Widgets
local separator = wibox.widget {
  {
    id = "separator",
    text = " | ",
    widget = wibox.widget.textbox,
  },
  bg = beautiful.wibar_bg,
  fg = beautiful.color10,
  widget = wibox.container.background,
}

local espacer = wibox.widget {
  {
    id = "espacer",
    text = " ",
    widget = wibox.widget.textbox,
  },
  bg = beautiful.transparent,
  widget = wibox.container.background,
}

-- Keyboard map indicator and switcher
local mykeyboardlayout = awful.widget.keyboardlayout()

-- Create a textclock widget
local clock = wibox.widget({
  {
    {
      id = "clock",
      format = "<b>" .. "%a, %d/%m ~ %H:%M" .. "</b>",
      refresh = 5,
      widget = wibox.widget.textclock(),
    },
    bg = beautiful.wibar_bg,
    fg = beautiful.color12,
    widget = wibox.container.background,
  },
  bottom = 2,
  color = beautiful.color4,
  widget = wibox.container.margin,
})

screen.connect_signal("request::desktop_decoration", function(s)
    -- Each screen has its own tag table.
    awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()

    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox {
        screen  = s,
        buttons = {
            awful.button({ }, 1, function () awful.layout.inc( 1) end),
            awful.button({ }, 3, function () awful.layout.inc(-1) end),
            awful.button({ }, 4, function () awful.layout.inc(-1) end),
            awful.button({ }, 5, function () awful.layout.inc( 1) end),
        }
    }


    s.win_title = awful.widget.tasklist {
      screen = s,
      filter = awful.widget.tasklist.filter.focused,
      style = { tasklist_disable_icon = true, },
    }

    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        style = {
          font = beautiful.font_name .. "Medium 11",
          shape = gears.shape.circle,
          -- spacing = 2,
        },
        buttons = {
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
        }
    }

    -- Create a tasklist widget
    --s.mytasklist = awful.widget.tasklist {
    --    screen  = s,
    --    filter  = awful.widget.tasklist.filter.currenttags,
    --    buttons = {
    --        awful.button({ }, 1, function (c)
    --            c:activate { context = "tasklist", action = "toggle_minimization" }
    --        end),
    --        awful.button({ }, 3, function() awful.menu.client_list { theme = { width = 250 } } end),
    --        awful.button({ }, 4, function() awful.client.focus.byidx(-1) end),
    --        awful.button({ }, 5, function() awful.client.focus.byidx( 1) end),
    --    }
    --}

    -- Create the wibox
    s.mywibox = awful.wibar {
        position = "top",
        screen = s,
        height = 24,
        margins = { top = 4, bottom = 0, left = 3, right = 3 },
        shape = gears.shape.rounded_rect,
        widget   = {
            layout = wibox.layout.align.horizontal,
            { -- Left widgets
                layout = wibox.layout.fixed.horizontal,
                espacer,
                s.mytaglist,
                separator,
                s.mylayoutbox,
                separator,
                s.win_title,
            },
            { -- Middle widgets
                layout = wibox.layout.fixed.horizontal,
            },
            { -- Right widgets
                layout = wibox.layout.fixed.horizontal,
                mykeyboardlayout,
                separator,
                clock,
                separator,
                wibox.widget.systray(),
                espacer,
            },
        }
    }
end)
