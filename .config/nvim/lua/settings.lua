local vim = _G.vim

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

vim.api.nvim_buf_set_option(0, 'undofile', true)

vim.cmd('syntax on')

vim.cmd('set shortmess+=c')

local indentations = {
  javascript      = { width=2, style='space', ruler=121 },
  typescript      = { width=2, style='space', ruler=121 },
  typescriptreact = { width=2, style='space', ruler=121 },
  svelte          = { width=2, style='space', ruler=121 },
  html            = { width=2, style='space', ruler=121 },
  css             = { width=2, style='space', ruler=121 },
  scss            = { width=2, style='space', ruler=121 },
  haskell         = { width=4, style='space' },
  java            = { width=2, style='space', ruler=121 },
  c               = { width=4, style='space', ruler=81  },
  cpp             = { width=4, style='space', ruler=121 },
  rust            = { width=4, style='space', ruler=121 },
  yaml            = { width=2, style='space', ruler=121 },
  lua             = { width=2, style='space', ruler=121 },
  sh              = { width=4, style='space', ruler=81  },
  json            = { width=2, style='space', ruler=121 },
  cmake           = { width=4, style='space', ruler=121 },
  go              = { width=4, style='tab',   ruler=121 },
  xml             = { width=2, style='space' },
}

for lang, options in pairs(indentations) do
  local cmd = 'autocmd Filetype ' .. lang .. ' setlocal'
  cmd = cmd .. ' tabstop=' .. options.width .. ' shiftwidth=' .. options.width .. '  smarttab'

  if options.style == 'space' then
    cmd = cmd .. ' expandtab softtabstop'
  end

  if options.ruler then
    cmd = cmd .. ' colorcolumn=' .. options.ruler
  end

  vim.cmd(cmd)
end
