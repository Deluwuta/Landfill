local servers = require("configs.lsp_servers")

local M = {}

-- Helper functions
-- LSP deny formatting
local lsp_formatting = function(bufnr)
    local deny_formatting = {
        "astro",
        "clangd",
        "gopls",
        "rust_analyzer",
        "lua_ls",
        "ts_ls"
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

M.on_attach = function(client, bufnr)
    vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

    local function opts(desc)
        return { buffer = bufnr, desc = "LSP " .. desc }
    end

    -- Keybinds
    vim.keymap.set("n", "<Leader>p", vim.lsp.buf.hover, opts "")
    vim.keymap.set("n", "<Leader>i", vim.lsp.buf.definition, opts "")
    vim.keymap.set("n", "<Leader>r", vim.lsp.buf.rename, opts "")

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
end

M.on_init = function(client, _)
    if client.supports_method "textDocument/semanticTokens" then
        client.server_capabilities.semanticTokensProvider = nil
    end
end

M.capabilities = vim.lsp.protocol.make_client_capabilities()
-- M.capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())

M.capabilities.textDocument.completion.completionItem = {
    documentationFormat = { "markdown", "plaintext" },
    snippetSupport = true,
    preselectSupport = true,
    insertReplaceSupport = true,
    labelDetailsSupport = true,
    deprecatedSupport = true,
    commitCharactersSupport = true,
    tagSupport = { valueSet = { 1 } },
    resolveSupport = {
        properties = {
            "documentation",
            "detail",
            "additionalTextEdits",
        },
    },
}

M.defaults = function()
    dofile(vim.g.base46_cache .. "lsp")
    local lspconfig = require("lspconfig")
    local options = {
        on_attach = M.on_attach,
        capabilities = M.capabilities,
        on_init = M.on_init,
    }
    for _, server in ipairs(servers) do
        if server == "lua_ls" then
            options.settings = {
                Lua = { 
                    diagnostics = { globals = { "vim" } },
                    workspace = {
                        library = {},
                        maxPreload = 100000,
                        preloadFileSize = 10000,
                    },
                },
            }
        end
        lspconfig[server].setup(options)
    end
end

-- Even in lazy.nvim, I litterally copy-pasted this, idk
local lspconfig_config = function ()
    local lspconfig = require("lspconfig")
    local opts = {}

    for _, server in pairs(servers) do
        if server == "lua_ls" then
            opts.settings = { Lua = { diagnostics = { globals = { "vim" } } } }
        end
        lspconfig[server].setup(opts)
    end
end

local nvim_lspconfig = {
    "neovim/nvim-lspconfig",
    event = "User FilePost",
    config = M.defaults
}

local lsp_signature = {
    "ray-x/lsp_signature.nvim",
    event = "InsertEnter",
    opts = {
        bind = true,
        handler_opts = { border = "rounded" }
    },
    config = function (_, opts)
        require'lsp_signature'.on_attach(opts)
    end
}

return { nvim_lspconfig }
