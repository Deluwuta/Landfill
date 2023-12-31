-- Automatic Packer Install kek
local ensure_packer = function()
    local fn = vim.fn
    local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
    if fn.empty(fn.glob(install_path)) > 0 then
        fn.system({ 'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path })
        vim.cmd [[packadd packer.nvim]]
        return true
    end
    return false
end

local packer_bootstrap = ensure_packer()

-- Reloads Neovim after whenever you save plugins.lua
vim.cmd([[
    augroup packer_user_config
      autocmd!
     autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup END
]])

return require("packer").startup(function(use)
    -- Plugins here
    -- Essential stuff
    use ("wbthomason/packer.nvim")
    use ("lewis6991/impatient.nvim") -- To speed up the loading of Lua modules

    -- COLORING
        use ({
            "nvim-treesitter/nvim-treesitter",
        })

    -- COMPLETIONS
        use ({
            "hrsh7th/nvim-cmp",
            "hrsh7th/cmp-nvim-lsp",
            "hrsh7th/cmp-path",
            "L3MON4D3/LuaSnip",

            -- Vscode in vim
            "saadparwaiz1/cmp_luasnip",
            "rafamadriz/friendly-snippets",
        })

    -- LSP
        use ({
            "williamboman/mason.nvim",
            "williamboman/mason-lspconfig.nvim",
            "neovim/nvim-lspconfig",
        })

    -- NAVIGATION
        use ({
            "theprimeagen/harpoon",
            "christoomey/vim-tmux-navigator",
        })

        -- Telescope
        use ({
            "nvim-lua/plenary.nvim",
            "nvim-telescope/telescope.nvim",
            -- tag = '0.1.1',
            require = { { "nvim-lua/plenary.nvim" } },
        })
        use ("nvim-telescope/telescope-file-browser.nvim")

    -- THEMES
        use ({
            --"navarasu/onedark.nvim",
            --"rebelot/kanagawa.nvim", as = "kanagawa",
            "Shatur/neovim-ayu", as = "ayu",
        })

    -- TOOLS
        use ({
            "terrortylor/nvim-comment",
            "windwp/nvim-autopairs",
            -- "akinsho/toggleterm.nvim",
        })

    -- UI
        -- Status line
        use ({
            "nvim-lualine/lualine.nvim",
        })
        use ({
            "glepnir/galaxyline.nvim",
            brach = "main",
        })
        -- use ({
        --     "nvimdev/whiskyline.nvim",
        --     event = "BufEnter",
        -- })

        -- Hex coloring
        use ({
            "norcalli/nvim-colorizer.lua",
            "brenoprata10/nvim-highlight-colors",
        })
    -- OTHER

    -- Automatically set up the configuration after cloning packer.nvim
    -- This SHOULD be at the end after all plugins
    if packer_bootstrap then
        require("packer").sync()
    end
end)
