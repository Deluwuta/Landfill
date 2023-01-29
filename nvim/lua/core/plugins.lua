-- If packer.installed == false -> Install packer :D
local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

return require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'
  use {'catppuccin/nvim', as = 'catppuccin'}
  -- use 'nvim-tree/nvim-tree.lua'
  use 'nvim-neo-tree/neo-tree.nvim'
  use 'nvim-tree/nvim-web-devicons'
  -- use 'norcalli/nvim-colorizer.lua'
  use 'brenoprata10/nvim-highlight-colors'
  use 'nvim-lualine/lualine.nvim'
  use 'nvim-treesitter/nvim-treesitter'
  use 'nvim-telescope/telescope-file-browser.nvim'
  use "j-hui/fidget.nvim"
  use 'terrortylor/nvim-comment'
  use "lukas-reineke/indent-blankline.nvim"
  use 'windwp/nvim-autopairs'
  use "akinsho/bufferline.nvim"
  use "akinsho/toggleterm.nvim"
  -- use "X3eRo0/dired.nvim"

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
    requires = { {'nvim-lua/plenary.nvim'} }
  }

  -- Completion
  use {
    "hrsh7th/nvim-cmp",
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-path",
    "L3MON4D3/LuaSnip",
    -- For vscode like snippets
    "saadparwaiz1/cmp_luasnip",
    "rafamadriz/friendly-snippets",
  }

  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if packer_bootstrap then
    require('packer').sync()
  end
end)
