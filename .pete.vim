let ext = expand('%:e')

if ext == 'hs'
	call pete#conf#default()
	let g:tool_path = ''
	let g:tool_mode = 3

elseif ext == 'hamlet'
	let g:tool_path = ''

elseif ext == 'julius'
	let g:tool_path = ''

elseif ext == 'cassius'
	let g:tool_path = ''

elseif ext == 'lucius'
	let g:tool_path = ''

else
	call pete#conf#default()
endif
