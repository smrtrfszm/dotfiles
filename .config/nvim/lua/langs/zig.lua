local settings = require('settings2')

settings.set({
  ['zig'] = { width = 4, style = 'space', ruler = 121 },
});

return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'zig' })
    end,
  },
}
