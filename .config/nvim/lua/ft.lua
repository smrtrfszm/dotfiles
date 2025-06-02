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
    ['.yamllint'] = 'yaml',

    -- From: https://github.com/mfussenegger/nvim-ansible/blob/bba61168b7aef735e7f950fdfece5ef6c388eacf/ftdetect/ansible.lua#L4-L13
    ['.*/defaults/.*%.ya?ml'] = 'yaml.ansible',
    ['.*/host_vars/.*%.ya?ml'] = 'yaml.ansible',
    ['.*/group_vars/.*%.ya?ml'] = 'yaml.ansible',
    ['.*/group_vars/.*/.*%.ya?ml'] = 'yaml.ansible',
    ['.*/playbook.*%.ya?ml'] = 'yaml.ansible',
    ['.*/playbooks/.*%.ya?ml'] = 'yaml.ansible',
    ['.*/roles/.*/tasks/.*%.ya?ml'] = 'yaml.ansible',
    ['.*/roles/.*/handlers/.*%.ya?ml'] = 'yaml.ansible',
    ['.*/tasks/.*%.ya?ml'] = 'yaml.ansible',
    ['.*/molecule/.*%.ya?ml'] = 'yaml.ansible',
  },
})
