let g:lightline = {
\ 	'active': {
\ 		'left': [
\ 			['mode', 'paste'],
\ 			['gitbrance', 'readonly', 'filename', 'modified']
\ 		]
\ 	},
\ 	'component_function': {
\ 		'gitbranch': 'FugitiveHead'
\ 	},
\ }
