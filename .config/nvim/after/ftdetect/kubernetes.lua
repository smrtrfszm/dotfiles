vim.filetype.add({
  extension = {
    kubeconfig = 'yaml',
  },
  pattern = {
    ['${XDG_CONFIG_HOME}/kube/config'] = 'yaml',
    ['${XDG_CONFIG_HOME}/kube/kuberc'] = 'yaml',
  },
})
