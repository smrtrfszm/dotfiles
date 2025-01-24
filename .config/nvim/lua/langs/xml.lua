local settings = require('settings2')

settings.set({
  ['xml'] = { width = 2, style = 'space' },
});

return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'xml' })
    end,
  },
}
