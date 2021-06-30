local vim = _G.vim

vim.api.nvim_set_option('mouse', 'a')
vim.api.nvim_set_option('scrolloff', 5)
vim.api.nvim_set_option('laststatus', 2)
vim.api.nvim_set_option('showmode', false)
vim.api.nvim_set_option('termguicolors', true)
vim.api.nvim_set_option('pumheight', 10)

vim.api.nvim_win_set_option(0, 'number', true)
vim.api.nvim_win_set_option(0, 'relativenumber', true)
vim.api.nvim_win_set_option(0, 'wrap', false)
vim.api.nvim_win_set_option(0, 'cursorline', true)
vim.api.nvim_win_set_option(0, 'signcolumn', 'yes')

vim.api.nvim_buf_set_option(0, 'undofile', true)

vim.cmd('syntax on')

vim.cmd('set shortmess+=c')


vim.cmd('autocmd Filetype javascript setlocal tabstop=2 shiftwidth=2 expandtab softtabstop colorcolumn=121')
vim.cmd('autocmd Filetype typescript setlocal tabstop=2 shiftwidth=2 expandtab softtabstop colorcolumn=121')
vim.cmd('autocmd Filetype typescriptreact setlocal tabstop=3 shiftwidth=2 expandtab softtabstop colorcolumn=121')
vim.cmd('autocmd Filetype html setlocal tabstop=2 shiftwidth=2 expandtab softtabstop colorcolumn=121')
vim.cmd('autocmd Filetype css setlocal tabstop=2 shiftwidth=2 expandtab softtabstop colorcolumn=121')
vim.cmd('autocmd Filetype scss setlocal tabstop=2 shiftwidth=2 expandtab softtabstop colorcolumn=121')
vim.cmd('autocmd Filetype haskell setlocal tabstop=4 shiftwidth=4 expandtab softtabstop')
vim.cmd('autocmd Filetype java setlocal colorcolumn=121')
vim.cmd('autocmd Filetype c setlocal tabstop=4 shiftwidth=4 expandtab softtabstop colorcolumn=81')
vim.cmd('autocmd Filetype cpp setlocal tabstop=4 shiftwidth=4 expandtab softtabstop colorcolumn=121')
vim.cmd('autocmd Filetype rust setlocal tabstop=4 shiftwidth=4 expandtab softtabstop colorcolumn=121')
vim.cmd('autocmd Filetype yaml setlocal tabstop=2 shiftwidth=2 expandtab softtabstop')
vim.cmd('autocmd Filetype lua setlocal tabstop=2 shiftwidth=2 expandtab softtabstop colorcolumn=121')
vim.cmd('autocmd Filetype sh setlocal tabstop=4 shiftwidth=4 expandtab softtabstop colorcolumn=80')
