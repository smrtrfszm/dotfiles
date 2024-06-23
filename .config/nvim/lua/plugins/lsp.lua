return {
  {
    'neovim/nvim-lspconfig',
    event = { 'BufReadPre', 'BufNewFile' },
    dependencies = {
      'mason.nvim',
      'williamboman/mason-lspconfig.nvim',
    },
    opts = {
      servers = {},
      setup = {},
    },
    config = function (_, opts)
      local servers = opts.servers

      local capabilities = vim.tbl_deep_extend('force',
        vim.lsp.protocol.make_client_capabilities(),
        require('cmp_nvim_lsp').default_capabilities()
      )

      local function setup(server)
        local server_opts = vim.tbl_deep_extend('force', {
          capabilities = vim.deepcopy(capabilities),
        }, servers[server] or {})

        if opts.setup[server] then
          if opts.setup[server](server, server_opts) then
            return
          end
        elseif opts.setup['*'] then
          if opts.setup['*'](server, server_opts) then
            return
          end
        end

        require('lspconfig')[server].setup(server_opts)
      end

      local have_mason, mason_lspconfig = pcall(require, 'mason-lspconfig')
      local all_mason_lsp_servers = {}

      if have_mason then
        all_mason_lsp_servers = vim.tbl_keys(require('mason-lspconfig.mappings.server').lspconfig_to_package)
      end

      local ensure_installed = {}

      for server, server_opts in pairs(servers) do
        if server_opts then
          server_opts = server_opts == true and {} or server_opts

          if server_opts.mason == false or not vim.tbl_contains(all_mason_lsp_servers, server) then
            setup(server)
          else
            ensure_installed[#ensure_installed + 1] = server
          end
        end
      end

      if have_mason then
        mason_lspconfig.setup({ ensure_installed = ensure_installed, handlers = { setup } })
      end
    end,
  },

  {
    'williamboman/mason.nvim',
    cmd = 'Mason',
    opts = {
      ensure_installed = {}
    },
    config = function (_, opts)
      require('mason').setup(opts)

      local registry = require('mason-registry')

      local function ensure_installed()
        for _, tool in ipairs(opts.ensure_installed) do
          local p = registry.get_package(tool)

          if not p:is_installed() then
            p:install()
          end
        end
      end

      if registry.refresh then
        registry.refresh(ensure_installed)
      else
        ensure_installed()
      end
    end,
  }
}
