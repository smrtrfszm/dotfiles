local config = require('kommentary.config')

config.configure_language('default', {
    prefer_single_line_comments = true,
    use_consistent_indentation = true,
    ignore_whitespace = false,
})
