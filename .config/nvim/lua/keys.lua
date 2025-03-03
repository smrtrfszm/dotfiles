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
vim.keymap.set('n', 'gd', function () require('telescope.builtin').lsp_definitions() end)
vim.keymap.set('n', 'gr', function () require('telescope.builtin').lsp_references() end)
vim.keymap.set('n', '<leader>r', vim.lsp.buf.rename)
vim.keymap.set('n', 'ca', vim.lsp.buf.code_action)
vim.keymap.set('n', '<leader>f', function () vim.lsp.buf.format({ async = true }) end)

vim.keymap.set('n', '<leader>y', '"+y')
vim.keymap.set('v', '<leader>y', '"+y')

vim.keymap.set('n', 'q:', '<nop>')
