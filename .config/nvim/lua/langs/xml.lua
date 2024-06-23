return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'xml' })
    end,
  },

  {
    'settings',
    opts = {
      ['xml'] = { width = 2, style = 'space' },
    },
  },
}
