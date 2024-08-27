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

vim.api.nvim_set_option('undofile', true)
vim.api.nvim_set_option('undodir', os.getenv('XDG_STATE_HOME') .. '/nvim/undo')

vim.fn.sign_define('DiagnosticSignError', { text = ' ', texthl = 'DiagnosticSignError'})
vim.fn.sign_define('DiagnosticSignWarn', { text = ' ', texthl = 'DiagnosticSignWarn'})
vim.fn.sign_define('DiagnosticSignInfo', { text = ' ', texthl = 'DiagnosticSignInfo'})
vim.fn.sign_define('DiagnosticSignHint', { text = '', texthl = 'DiagnosticSignHint'})

vim.cmd('syntax on')

vim.filetype.add({
  pattern = {
    ['.*/hypr/.*%.conf'] = 'hyprlang',
    ['.*/kube/config'] = 'yaml',
    ['.*.fsh'] = 'glsl',
    ['.*.vsh'] = 'glsl',
    ['.*.Containerfile'] = 'dockerfile',
    ['.*/containers/containers.conf'] = 'toml',
    ['.*.mtxcfg'] = 'json',
    ['.*/waybar/config'] = 'jsonc',
  },
})

vim.opt.shortmess:append({ C = true })

vim.api.nvim_create_autocmd('CursorHold', {
  pattern = '*',
  callback = function ()
    vim.diagnostic.open_float(nil, { focusable = false })
  end
})
