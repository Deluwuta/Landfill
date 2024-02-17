local lualine = {
    "nvim-lualine/lualine.nvim",
    lazy = false,
    config = function ()
        require("lualine").setup({
            options = { icons_enabled = true },
            sections = {
                lualine_a = {
                    {
                        'filename',
                        path = 1,
                    }
                }
            }
        })
    end,
}

return lualine
