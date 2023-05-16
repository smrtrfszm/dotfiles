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
vim.api.nvim_set_option('undodir', os.getenv("XDG_STATE_HOME") .. "/nvim/undo")

vim.fn.sign_define("DiagnosticSignError", { text = " ", texthl = "DiagnosticSignError"})
vim.fn.sign_define("DiagnosticSignWarn", { text = " ", texthl = "DiagnosticSignWarn"})
vim.fn.sign_define("DiagnosticSignInfo", { text = " ", texthl = "DiagnosticSignInfo"})
vim.fn.sign_define("DiagnosticSignHint", { text = "", texthl = "DiagnosticSignHint"})

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
  zig             = { width=4, style='space', ruler=121 },
  php             = { width=2, style='space', ruler=121 },
  toml            = { width=4, style='space', ruler=121 },
  blueprint       = { width=4, style='space', ruler=121 },
}

for lang, options in pairs(indentations) do
  vim.api.nvim_create_autocmd('Filetype', {
    pattern = lang,
    callback = function ()
      vim.opt_local.tabstop = options.width
      vim.opt_local.shiftwidth = options.width
      vim.opt_local.smarttab = true

      if options.style == 'space' then
        vim.opt_local.expandtab = true
        vim.opt_local.softtabstop = options.width
      end

      if options.ruler then
        vim.opt_local.colorcolumn = { options.ruler }
      end
    end
  })
end
