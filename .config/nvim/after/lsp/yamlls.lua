return {
  settings = {
    yaml = {
      schemas = {
        kubernetes = '{k8s,kubernetes}/*.{yml,yaml}',
        ['https://json.schemastore.org/github-workflow'] = '.github/workflows/*',
        ['https://json.schemastore.org/github-action'] = '.github/action.{yml,yaml}',
        ['https://json.schemastore.org/dependabot-v2'] = '.github/dependabot.{yml,yaml}',
        ['https://gitlab.com/gitlab-org/gitlab/-/raw/master/app/assets/javascripts/editor/schema/ci.json'] = '*gitlab-ci*.{yml,yaml}',
        ['https://raw.githubusercontent.com/compose-spec/compose-spec/master/schema/compose-spec.json'] = '*(docker-)?compose*.{yml,yaml}',
        ['https://raw.githubusercontent.com/jesseduffield/lazygit/refs/heads/master/schema/config.json'] = '*lazygit/config.{yml,yaml}',
      }
    }
  }
}
