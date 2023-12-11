local awful = require("awful")

local function obtener_tipo_conexion()
    local cmd_wireless = "iw dev | awk '$1==\"Interface\" {print $2}'"
    local cmd_cable = "ip link | awk '/state UP/ {print $2}'"

    local wireless = awful.spawn.with_shell(cmd_wireless)
    local wired = awful.spawn.with_shell(cmd_cable)

    return (wireless and "Wi-Fi") or (wired and "Ethernet") or "Disconected"
end

local netwidget = awful.widget.watch(
    function (widget)
        widget:set_text("Estado: " .. obtener_tipo_conexion())
    end,
    1
)

return netwidget
