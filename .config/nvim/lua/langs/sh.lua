local settings = require('settings2')

settings.set({
  ['sh'] = { width = 4, style = 'space', ruler = 81 },
  ['zsh'] = { width = 4, style = 'space', ruler = 81 },
});

return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'bash' })
    end,
  },
}
