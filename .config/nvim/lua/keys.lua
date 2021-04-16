-- Helper function
local function key(mode, key, action)
  vim.api.nvim_set_keymap(mode, key, action, {noremap = true, silent = true})
end

local function expr(mode, key, action)
  vim.api.nvim_set_keymap(mode, key, action, {expr = true, noremap = true, silent = true})
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

-- Smart tabs
expr('i', '<TAB>', 'pumvisible() ? "<C-n>" : "<TAB>"')
expr('i', '<S-TAB>', 'pumvisible() ? "<C-p>" : "<S-TAB>"')

key('n', '<leader><leader>', '<C-^>')
key('i', '<C-j>', '<esc>')
key('n', '<leader>w', '<cmd>w<cr>')
key('n', '<C-f>', '<cmd>lua require(\'telescope.builtin\').find_files()<cr>')
key('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>')
key('n', '<leader>g', '<cmd>LazyGit<cr>')

expr('i', '<C-Space>', 'compe#complete()')
expr('i', '<CR>', 'compe#confirm("<CR")')
