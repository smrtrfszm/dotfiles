vim.cmd.colorscheme('base16-default-dark')

vim.api.nvim_set_option('mouse', 'a')
vim.api.nvim_set_option('scrolloff', 5)
vim.api.nvim_set_option('laststatus', 3)
vim.api.nvim_set_option('showmode', false)
vim.api.nvim_set_option('termguicolors', true)
vim.api.nvim_set_option('pumheight', 10)

vim.api.nvim_win_set_option(0, 'number', true)
vim.api.nvim_win_set_option(0, 'relativenumber', true)
vim.api.nvim_win_set_option(0, 'wrap', false)
vim.api.nvim_win_set_option(0, 'cursorline', true)
vim.api.nvim_win_set_option(0, 'signcolumn', 'yes')
vim.api.nvim_win_set_option(0, 'list', true)

vim.api.nvim_set_option('undofile', true)
vim.api.nvim_set_option('undodir', os.getenv('XDG_STATE_HOME') .. '/nvim/undo')

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

vim.cmd('syntax on')

vim.opt.shortmess:append({ C = true })

vim.api.nvim_create_autocmd('CursorHold', {
  pattern = '*',
  callback = function ()
    vim.diagnostic.open_float(nil, { focusable = false })
  end
})
