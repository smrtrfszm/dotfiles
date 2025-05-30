local function get_opts(name)
  local plugin = require('lazy.core.config').plugins[name]
  if not plugin then
    return {}
  end

  local Plugin = require('lazy.core.plugin')
  return Plugin.values(plugin, 'opts', false)
end

return {
  {
    'nvim-cmp',
    dependencies = {
      {
        'Saecki/crates.nvim',
        dependencies = { 'nvim-lua/plenary.nvim' },
        event = { 'BufRead Cargo.toml' },
        config = true,
      },
    },
    opts = function(_, opts)
      local cmp = require('cmp')
      opts.sources = cmp.config.sources(vim.list_extend(opts.sources, {
        { name = 'crates' },
      }))
    end,
  },

  {
    'nvim-treesitter',
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'rust', 'toml' })
    end,
  },

  {
    'mason.nvim',
    optional = true,
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { 'codelldb' })
    end,
  },

  {
    'nvim-lspconfig',
    opts = {
      servers = {
        rust_analyzer = {
          settings = {
            ['rust-analyzer'] = {
              cargo = {
                allFeatures = true,
                loadOutDirsFromCheck = true,
                runBuildScripts = true,
              },
              checkOnSave = {
                allFeatures = true,
                command = 'clippy',
                extraArgs = { '--no-deps' },
              },
              procMacro = {
                enable = true,
                ignored = {
                },
              },
            },
          },
        },
      },
    },
  },
}
