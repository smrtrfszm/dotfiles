local lsp_installer = require('nvim-lsp-installer')
local lsp_installer_servers = require('nvim-lsp-installer.servers')

local servers = {'angularls', 'bashls', 'clangd', 'cmake', 'cssls', 'dockerls', 'emmet_ls', 'gopls', 'graphql', 'html', 'sumneko_lua', 'pyright', 'rust_analyzer', 'tsserver'}

for _, server in pairs(servers) do
  local available, requested = lsp_installer_servers.get_server(server)
  if available then
    if not requested:is_installed() then
      requested:install()
    end
  end
end

lsp_installer.on_server_ready(function(server)
  server:setup {}
end)
