-- If packer.installed == false -> Install packer :D
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

return require('packer').startup(function(use)
    -- Essential
    use 'wbthomason/packer.nvim'
    use "lewis6991/impatient.nvim" -- Speed up loading Lua modules
    use 'theprimeagen/harpoon'

    -- Color stuff
    use {
        "norcalli/nvim-colorizer.lua",
        "brenoprata10/nvim-highlight-colors",
        "nvim-treesitter/nvim-treesitter",
    }

    -- Completions
    use {
        "hrsh7th/nvim-cmp",
        "hrsh7th/cmp-nvim-lsp",
        "hrsh7th/cmp-path",
        "L3MON4D3/LuaSnip",

        -- For vscode like snippets
        "saadparwaiz1/cmp_luasnip",
        "rafamadriz/friendly-snippets",
    }

    -- Notifications
    use {
        --    "rcarriga/nvim-notify",
        --    "folke/noice.nvim",
        "MunifTanjim/nui.nvim",
    }

    -- Mason and LSP servers
    use {
        "williamboman/mason.nvim",
        "williamboman/mason-lspconfig.nvim",
        "neovim/nvim-lspconfig",
    }

    -- Telescope shit
    use {
        'nvim-telescope/telescope.nvim',
        tag = '0.1.1',
        requires = { { 'nvim-lua/plenary.nvim' } }
    }
    use 'nvim-telescope/telescope-file-browser.nvim'

    -- Themes
    use { "catppuccin/nvim", as = "catppuccin" }
    use { "folke/tokyonight.nvim", as = "tokyonight" }
    use { "Everblush/nvim", as = "everblush" }
    use({
        "rose-pine/neovim",
        as = "rose-pine",
        config = function()
            vim.cmd('colorscheme rose-pine')
        end
    })

    -- Tools
    use {
        "terrortylor/nvim-comment",
        "windwp/nvim-autopairs",
        --   "akinsho/toggleterm.nvim",
    }

    -- Ui / Visuals
    use {
        "nvim-tree/nvim-web-devicons",
        "nvim-lualine/lualine.nvim",
        "j-hui/fidget.nvim",
        -- "lukas-reineke/indent-blankline.nvim",
    }

    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all plugins
    if packer_bootstrap then
        require('packer').sync()
    end
end)
