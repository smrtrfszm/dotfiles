return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'markdown' })
    end,
  },

  {
    'settings',
    opts = {
      ['markdown'] = { width = 4, style = 'space', ruler = 80 },
    },
  },
}
