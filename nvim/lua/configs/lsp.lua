require("mason").setup({})

local servers = {
    "bashls",        -- bash
    "clangd",        -- C, C--
    -- "hls",           -- haskell
    -- "html",          -- html
    "lua_ls",        -- lua
    "pyright",       -- pyton
    "rust_analyzer", -- rust
    -- "texlab",        -- latex
}

require("mason-lspconfig").setup({
    ensure_installed = servers,
    automatic_installation = true,
})


-- I litterally copy-pasted this, idk
local lspconfig = require "lspconfig"

local lsp_formatting = function(bufnr)
    local deny_formatting = { "astro", "clangd", "gopls", "html", "rust_analyzer", "lua_ls", "tsserver" }
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
