-- Minimal Neovim: treesitter highlighting + native LSP (TypeScript / JavaScript).
-- No framework, one plugin (nvim-treesitter). Requires Neovim 0.12+.
-- Deps installed via mise: neovim, tree-sitter, typescript, typescript-language-server.

-- 1. Sensible defaults --------------------------------------------------------
vim.g.mapleader = ' '
vim.o.number = true
vim.o.mouse = 'a'
vim.o.clipboard = 'unnamedplus'
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.termguicolors = true          -- needed for treesitter colors (truecolor terminal)
vim.o.signcolumn = 'yes'
vim.o.expandtab = true
vim.o.shiftwidth = 2
vim.o.tabstop = 2
vim.o.undofile = true

-- 2. Plugins, via the built-in vim.pack manager ------------------------------
vim.pack.add({
  { src = 'https://github.com/nvim-treesitter/nvim-treesitter', version = 'main' },
  { src = 'https://github.com/catppuccin/nvim', name = 'catppuccin' },
})

-- Colorscheme
require('catppuccin').setup({ flavour = 'mocha' })
vim.cmd.colorscheme('catppuccin')

-- 3. Treesitter highlighting --------------------------------------------------
-- install() is idempotent + async; a no-op once parsers exist.
require('nvim-treesitter').install({
  'bash', 'dockerfile', 'json', 'lua', 'markdown', 'markdown_inline',
  'toml', 'yaml', 'vim', 'vimdoc',
  'javascript', 'typescript', 'tsx',
})
-- Turn highlighting on for any buffer that has a parser.
vim.api.nvim_create_autocmd('FileType', {
  callback = function() pcall(vim.treesitter.start) end,
})

-- 4. LSP: native, no nvim-lspconfig -------------------------------------------
-- typescript-language-server + typescript are installed globally by mise; point
-- ts_ls at the mise typescript lib so it also works on standalone files outside
-- a project (matches the eglot tsserver.path setup).
vim.lsp.config('ts_ls', {
  cmd = { 'typescript-language-server', '--stdio' },
  filetypes = { 'javascript', 'javascriptreact', 'typescript', 'typescriptreact' },
  root_markers = { 'tsconfig.json', 'jsconfig.json', 'package.json', '.git' },
  init_options = {
    tsserver = {
      path = vim.fn.expand('~/.local/share/mise/installs/npm-typescript/latest/lib/node_modules/typescript/lib'),
    },
  },
})
vim.lsp.enable('ts_ls')

-- Enable autocompletion on attach. Built-in default keymaps (0.11+):
-- K=hover, grn=rename, gra=code action, grr=references, gri=implementation,
-- grt=type def, gO=document symbols, [d/]d=prev/next diagnostic.
vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(ev)
    local client = vim.lsp.get_client_by_id(ev.data.client_id)
    if client and client:supports_method('textDocument/completion') then
      vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = true })
    end
  end,
})
vim.diagnostic.config({ virtual_text = true })
