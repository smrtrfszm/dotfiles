return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'swift' })
    end,
  },

  {
    'settings',
    opts = {
      ['swift'] = { width = 4, style = 'space', ruler = 121 },
    },
  },
}
