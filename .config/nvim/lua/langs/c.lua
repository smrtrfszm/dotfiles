return {
  {
    'nvim-treesitter',
    opts = function (_, opts)
      vim.list_extend(opts.ensure_installed, { 'c', 'cpp', 'meson', 'cmake', 'asm', 'make' })
    end,
  },

  {
    'nvim-lspconfig',
    opts = {
      servers = {
        clangd = {
        },
        cmake = {
        },
      }
    }
  },

  {
    'settings',
    opts = {
      ['c'] = { width = 4, style = 'space', ruler = 81  },
      ['cmake'] = { width = 4, style = 'space', ruler = 121 },
      ['cpp'] = { width = 4, style = 'space', ruler = 121 },
      ['meson'] = { width = 2, style = 'space', ruler = 121 },
    },
  },
}
