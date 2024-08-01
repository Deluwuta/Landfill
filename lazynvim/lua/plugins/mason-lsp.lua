local servers = {
    "bashls",        -- bash
    "clangd",        -- C, C--
    -- "hls",        -- haskell
    "html",          -- html
    "lua_ls",        -- lua
    "pyright",       -- pyton
    "rust_analyzer", -- rust
    "tsserver",      -- JS / TS
    -- "texlab",     -- latex
}

local mason_lsp_config = function ()
    require("mason-lspconfig").setup({
        ensure_installed = servers,
        automatic_installation = true,
    })
end

-- Even in lazy.nvim, I litterally copy-pasted this, idk
local lspconfig_config = function ()
    local lspconfig = require("lspconfig")

    local lsp_formatting = function(bufnr)
        local deny_formatting = {
            "astro",
            "clangd",
            "gopls",
            -- "html",
            "rust_analyzer",
            "lua_ls",
            "tsserver"
        }

        vim.lsp.buf.format {
            filter = function(client)
                for _, value in pairs(deny_formatting) do
                    if client.name == value then
                        return false
                    end
                end
                return true
            end,
            bufnr = bufnr,
        }
    end

    local opts = {
        on_attach = function(client, bufnr)
            vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
            local opts = { buffer = bufnr }

            vim.keymap.set("n", "<Leader>p", vim.lsp.buf.hover, opts)
            vim.keymap.set("n", "<Leader>i", vim.lsp.buf.definition, opts)
            vim.keymap.set("n", "<Leader>r", vim.lsp.buf.rename, opts)

            if client.supports_method "textDocument/formatting" then
                local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

                vim.api.nvim_clear_autocmds { group = augroup, buffer = bufnr }
                vim.api.nvim_create_autocmd("BufWritePre", {
                    group = augroup,
                    buffer = bufnr,
                    callback = function()
                        lsp_formatting(bufnr)
                    end,
                })
            end
        end,
        capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities()),
    }

    for _, server in pairs(servers) do
        if server == "lua_ls" then
            opts.settings = { Lua = { diagnostics = { globals = { "vim" } } } }
        end
        lspconfig[server].setup(opts)
    end
end

local mason = {
    "williamboman/mason.nvim",
    lazy = false,
    config = function ()
        require("mason").setup({})
    end
}

local mason_lsp = {
    "williamboman/mason-lspconfig.nvim",
    lazy = false,
    config = mason_lsp_config,
}

local lspconfig_table = {
    "neovim/nvim-lspconfig",
    lazy = false,
    config = lspconfig_config,
    dependencies = {
        "hrsh7th/nvim-cmp",
        "hrsh7th/cmp-nvim-lsp",
        "hrsh7th/cmp-path",
    }
}

return {
    mason,
    mason_lsp,
    lspconfig_table,
}
