vim.g.mapleader = ' '
vim.keymap.set('n', '<space>', '<nop>')

vim.keymap.set({'', 'i'}, '<up>', '<nop>')
vim.keymap.set({'', 'i'}, '<right>', '<nop>')
vim.keymap.set({'', 'i'}, '<down>', '<nop>')
vim.keymap.set({'', 'i'}, '<left>', '<nop>')

vim.keymap.set('v', '<tab>', '>gv')
vim.keymap.set('v', '<s-tab>', '<gv')

vim.keymap.set('n', '<leader><leader>', '<c-^>')
vim.keymap.set('i', '<c-j>', '<esc>')
vim.keymap.set('n', '<leader>w', '<cmd>w<cr>')
vim.keymap.set('n', '<c-f>', function () require('telescope.builtin').find_files() end)
vim.keymap.set('n', 'gd', function () vim.lsp.buf.definition() end)
vim.keymap.set('n', '<leader>', function () vim.lsp.buf.rename() end)
vim.keymap.set('n', '<leader>g', '<cmd>LazyGit<cr>')
