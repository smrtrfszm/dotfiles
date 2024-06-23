return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'zig' })
    end,
  },

  {
    'settings',
    opts = {
      ['zig'] = { width = 4, style = 'space', ruler = 121 },
    },
  },
}
