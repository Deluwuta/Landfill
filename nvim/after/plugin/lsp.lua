local lsp = require('lsp-zero').preset({})

require("luasnip.loaders.from_vscode").lazy_load()

lsp.ensure_installed({
    'bashls',
    'clangd',
	'rust_analyzer',
})

-- Fix Undefined global 'vim'
--lsp.configure('lua-language-server', {
--    settings = {
--        Lua = {
--            diagnostics = {
--                globals = { 'vim' }
--            }
--        }
--    }
--})

local cmp = require('cmp')
local cmp_select = {behavior = cmp.SelectBehavior.Select}

cmp.setup({
    mapping = cmp.mapping.preset.insert({
        ["<C-Space>"] = cmp.mapping.complete(),
        ["<Tab>"] = cmp.mapping.select_next_item(),
        ["<S-Tab>"] = cmp.mapping.select_prev_item(),
        ["<CR>"] = cmp.mapping.confirm({ select = true }),
    }),

    snippet = {
        expand = function (args)
            require("luasnip").lsp_expand(args.body)
        end,
    },

    sources = cmp.config.sources({
        { name = "nvim_lsp" },
        { name = "luasnip" },
    }, {
        { name = "buffer" },
        { name = "path" },
    })
})

lsp.set_preferences({
    suggest_lsp_servers = false,
    sign_icons = {
        error = 'E',
        warn = 'W',
        hint = 'H',
        info = 'I'
    }
})

lsp.on_attach(function(client, bufnr)
  lsp.default_keymaps({buffer = bufnr})
end)

-- (Optional) Configure lua language server for neovim
require('lspconfig').lua_ls.setup(lsp.nvim_lua_ls())

lsp.setup()

vim.diagnostic.config({
    virtual_text = true
})
