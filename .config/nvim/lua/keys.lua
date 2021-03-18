-- Helper function
local function key(mode, key, action)
  vim.api.nvim_set_keymap(mode, key, action, {noremap = true, silent = true})
end

-- Set leader key
vim.g.mapleader = ' '
key('n', '<Space>', '<NOP>')

-- Disable arrom keys
key('', '<Up>', '<nop>')
key('', '<Right>', '<nop>')
key('', '<Down>', '<nop>')
key('', '<Left>', '<nop>')
key('i', '<Up>', '<nop>')
key('i', '<Right>', '<nop>')
key('i', '<Down>', '<nop>')
key('i', '<Left>', '<nop>')

-- Indentation
key('v', '<Tab>', '>gv')
key('v', '<S-Tab>', '<gv')

key('n', '<Leader><Leader>', '<C-^>')
key('i', '<C-j>', '<esc>')
key('n', '<Leader>w', '<cmd>w<cr>')
key('n', '<C-f>', '<cmd>Telescope git_files<cr>')
key('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>')
key('n', '<Leader>g', '<cmd>LazyGit<cr>')
