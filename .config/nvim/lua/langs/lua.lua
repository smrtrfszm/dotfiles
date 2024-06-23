return {
  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'lua' })
    end,
  },

  {
    'nvim-lspconfig',
    opts = {
      servers = {
        lua_ls = {
          settings = {
            Lua = {
              diagnostics = {
                globals = { 'vim' },
              }
            }
          }
        }
      },
    },
  },

  {
    'settings',
    opts = {
      ['lua'] = { width = 2, style = 'space', ruler = 121 },
    },
  },
}
