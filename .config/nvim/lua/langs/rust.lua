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
    "nvim-cmp",
    dependencies = {
      {
        "Saecki/crates.nvim",
        dependencies = { 'nvim-lua/plenary.nvim' },
        event = { "BufRead Cargo.toml" },
        config = true,
      },
    },
    opts = function(_, opts)
      local cmp = require("cmp")
      opts.sources = cmp.config.sources(vim.list_extend(opts.sources, {
        { name = "crates" },
      }))
    end,
  },

  {
    "nvim-treesitter",
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { "ron", "rust", "toml" })
    end,
  },

  {
    "mason.nvim",
    optional = true,
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { "codelldb" })
    end,
  },


  {
    "simrat39/rust-tools.nvim",
    lazy = true,
    opts = function()
      local ok, mason_registry = pcall(require, "mason-registry")
      local adapter
      if ok then
        -- rust tools configuration for debugging support
        local codelldb = mason_registry.get_package("codelldb")
        local extension_path = codelldb:get_install_path() .. "/extension/"
        local codelldb_path = extension_path .. "adapter/codelldb"
        local liblldb_path = vim.fn.has("mac") == 1 and extension_path .. "lldb/lib/liblldb.dylib"
          or extension_path .. "lldb/lib/liblldb.so"
        adapter = require("rust-tools.dap").get_codelldb_adapter(codelldb_path, liblldb_path)
      end
      return {
        dap = {
          adapter = adapter,
          hover_actions = {
            border = 'none',
          }
        },
        tools = {
          inlay_hints = {
            only_current_line = true,
          },
        },
      }
    end,
    config = function() end,
  },

  {
    "nvim-lspconfig",
    opts = {
      servers = {
        rust_analyzer = {
          -- keys = {
          --   { "K", "<cmd>RustHoverActions<cr>", desc = "Hover Actions (Rust)" },
          --   { "<leader>cR", "<cmd>RustCodeAction<cr>", desc = "Code Action (Rust)" },
          --   { "<leader>dr", "<cmd>RustDebuggables<cr>", desc = "Run Debuggables (Rust)" },
          -- },
          settings = {
            ["rust-analyzer"] = {
              cargo = {
                allFeatures = true,
                loadOutDirsFromCheck = true,
                runBuildScripts = true,
              },
              checkOnSave = {
                allFeatures = true,
                command = "clippy",
                extraArgs = { "--no-deps" },
              },
              procMacro = {
                enable = true,
                ignored = {
                  ["async-trait"] = { "async_trait" },
                },
              },
            },
          },
        },
      },
      setup = {
        rust_analyzer = function(_, opts)
          local rust_tools_opts = get_opts("rust-tools.nvim")
          require("rust-tools").setup(vim.tbl_deep_extend("force", rust_tools_opts or {}, { server = opts }))
          return true
        end,
      },
    },
  },

  {
    'settings',
    opts = {
      ['rust'] = { width = 4, style = 'space', ruler = 121 },
      ['toml'] = { width = 4, style = 'space', ruler = 121 },
    },
  },
}
