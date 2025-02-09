return {
    {
        "stevearc/conform.nvim",
        -- event = 'BufWritePre', -- uncomment for format on save
        opts = require "configs.conform",
    },

    -- These are some examples, uncomment them if you want to see them work!
    {
        "neovim/nvim-lspconfig",
        config = function()
            require "configs.lspconfig"
        end,
    },

    -- Copilot
    {
        "github/copilot.vim",
        cmd = "Copilot",
        config = function ()
            require("copilot").setup({})
        end,
    },

    -- Telescope
    {
        "nvim-telescope/telescope.nvim",
        depedencies = { "nvim-treesitter/nvim-treesitter" },
        cmd = "Telescope",
        opts = function()
            require("telescope").setup(
                require("custom.telescope")
            )
            require("telescope").load_extension("file_browser")
        end,
    },
    {
        "nvim-telescope/telescope-file-browser.nvim",
        dependencies = {
            "nvim-telescope/telescope.nvim",
            "nvim-lua/plenary.nvim",
        },
    },

    -- Utils
    {
        "christoomey/vim-tmux-navigator",
        lazy = false,
    },
}
