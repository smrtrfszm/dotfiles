vim.api.nvim_set_option('mouse', 'a')
vim.api.nvim_set_option('scrolloff', 5)
vim.api.nvim_set_option('laststatus', 2)
vim.api.nvim_set_option('showmode', false)

vim.api.nvim_win_set_option(0, 'number', true)
vim.api.nvim_win_set_option(0, 'relativenumber', true)
vim.api.nvim_win_set_option(0, 'wrap', false)
vim.api.nvim_win_set_option(0, 'cursorline', true)
vim.api.nvim_win_set_option(0, 'signcolumn', 'yes')

vim.api.nvim_buf_set_option(0, 'undofile', true)

