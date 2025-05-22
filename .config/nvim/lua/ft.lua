vim.filetype.add({
  extension = {
    fsh = 'glsl',
    vsh = 'glsl',
    Containerfile = 'dockerfile',
    mtxcfg = 'json',
    j2 = 'jinja',
  },
  pattern = {
    ['.*/kube/config'] = 'yaml',
    ['.*/containers/containers.conf'] = 'toml',
    ['.*/containers/storage.conf'] = 'toml',
    ['.*/waybar/config'] = 'jsonc',
    ['.env.*'] = 'sh',
    ['.whitesource'] = 'json',
  },
})
