vim.filetype.add({
  extension = {
    fsh = 'glsl',
    vsh = 'glsl',
    Containerfile = 'dockerfile',
    mtxcfg = 'json',
  },
  pattern = {
    ['.*/kube/config'] = 'yaml',
    ['.*/containers/containers.conf'] = 'toml',
    ['.*/containers/storage.conf'] = 'toml',
    ['.*/waybar/config'] = 'jsonc',
    ['.env.*'] = 'sh',
  },
})
