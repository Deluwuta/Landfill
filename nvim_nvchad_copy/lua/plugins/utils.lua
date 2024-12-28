local keymap = vim.keymap

local nvchad_ui = {
    -- Plenary for dependencies
    "nvim-lua/plenary.nvim",

    -- Base46 for theming
    {
        "nvchad/base46",
        build = function()
            require("base46").load_all_highlights()
        end,
    },

    -- NvChad UI for statusline
    {
        "nvchad/ui",
        lazy = false,
        config = function()
            -- Load the UI components you need
            require("nvchad") -- This initializes the UI
        end,
    },

    "nvzone/volt",
    "nvzone/menu",
    { "nvzone/minty", cmd = { "Heufy", "Shades" } },
}

local nvim_comment = {
    "terrortylor/nvim-comment",
    lazy = false,
    config = function()
        require("nvim_comment").setup({
            comment_emtpy = false,
            comment_empty_trim_whitespaces = false,
        })
    end,
    keys = {
        keymap.set({"n", "v"}, "<leader>l", ":CommentToggle<CR>")
    },
}

local nvim_highlight_colors = {
    "brenoprata10/nvim-highlight-colors",
    lazy = false,
    config = function ()
        require("nvim-highlight-colors").setup({
            render = "background", -- background | foreground | first_column
            enable_named_colors = true,
            enable_tailwind = false,
        })
    end,
}

local vim_illuminate = {
    "RRethy/vim-illuminate",
    lazy = false,
    config = function()
        require("illuminate").configure({})
    end,
}

local tmux_navigator = {
    "christoomey/vim-tmux-navigator",
    lazy = false,
}

local indent_blankline = {
    "lukas-reineke/indent-blankline.nvim",
    event = "User FilePost",
    -- lazy = false,
    opts = {
      indent = { char = "│" },
      scope = { enabled = false },
    },
    config = function (_, opts)
        dofile(vim.g.base46_cache .. "blankline")

        local hooks = require "ibl.hooks"
        hooks.register(hooks.type.WHITESPACE, hooks.builtin.hide_first_space_indent_level)
        require("ibl").setup(opts)

        dofile(vim.g.base46_cache .. "blankline")
    end
}

local nvim_web_icons = {
    "nvim-tree/nvim-web-devicons",
    opts = function ()
        dofile(vim.g.base46_cache .. "devicons")
        require("nvim-web-devicons").setup({})
    end
}

return {
    nvchad_ui,
    nvim_comment,
    -- nvim_web_icons,
    -- nvim_highlight_colors,
    tmux_navigator,
    indent_blankline,
}
