return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'sql' })
    end,
  },

  {
    'settings',
    opts = {
      ['sql'] = { width = 2, style = 'space', ruler = 121 },
    },
  },
}
