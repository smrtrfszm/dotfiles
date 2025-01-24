local settings = require('settings2')

settings.set({
  ['glsl'] = { width=4, style='space', ruler=81 },
});

return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'glsl' })
    end,
  },
}
