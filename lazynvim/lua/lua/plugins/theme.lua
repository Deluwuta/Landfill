local function theme_creator(name, colorscheme, background, dependencies)
    return {
        name,
        lazy = false,
        priority = 999,
        config = function()
            vim.cmd("set background="..background)
            vim.cmd("colorscheme "..colorscheme)
        end,
        dependencies = dependencies,
    }
end

local theme = {
    nightfox = theme_creator(
        "EdenEast/nightfox.nvim",
        "nightfox",
        "dark"
    ),

    zenbones = theme_creator(
        "mcchrish/zenbones.nvim",
        "zenbones",
        "dark",
        { "rktjmp/lush.nvim" }
    ),

    melange = theme_creator(
        "savq/melange-nvim",
        "melange",
        "dark"
    ),
}

return theme.melange
