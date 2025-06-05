vim.cmd('syntax on')
vim.cmd.colorscheme('base16-default-dark')

vim.api.nvim_set_option_value('mouse', 'a', {})
vim.api.nvim_set_option_value('scrolloff', 5, {})
vim.api.nvim_set_option_value('laststatus', 3, {})
vim.api.nvim_set_option_value('showmode', false, {})
vim.api.nvim_set_option_value('termguicolors', true, {})
vim.api.nvim_set_option_value('pumheight', 10, {})
vim.api.nvim_set_option_value('undofile', true, {})
vim.api.nvim_set_option_value('undodir', os.getenv('XDG_STATE_HOME') .. '/nvim/undo', {})

vim.api.nvim_set_option_value('number', true, {})
vim.api.nvim_set_option_value('relativenumber', true, {})
vim.api.nvim_set_option_value('wrap', false, {})
vim.api.nvim_set_option_value('cursorline', true, {})
vim.api.nvim_set_option_value('signcolumn', 'yes', {})
vim.api.nvim_set_option_value('list', true, {})

vim.opt.shortmess:append({ C = true })

vim.diagnostic.config({
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = ' ',
      [vim.diagnostic.severity.WARN] = ' ',
      [vim.diagnostic.severity.INFO] = ' ',
      [vim.diagnostic.severity.HINT] = ' ',
    },
    numhl = {
      [vim.diagnostic.severity.ERROR] = 'DiagnosticSignError',
      [vim.diagnostic.severity.WARN] = 'DiagnosticSignWarn',
      [vim.diagnostic.severity.INFO] = 'DiagnosticSignInfo',
      [vim.diagnostic.severity.HINT] = 'DiagnosticSignHint',
    },
  },
})

vim.api.nvim_create_autocmd('CursorHold', {
  pattern = '*',
  callback = function ()
    vim.diagnostic.open_float(nil, { focusable = false })
  end
})

vim.wo.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
