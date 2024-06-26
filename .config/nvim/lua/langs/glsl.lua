return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'glsl' })
    end,
  },

  {
    'settings',
    opts = {
      ['glsl'] = { width=4, style='space', ruler=81 },
    },
  },
}
