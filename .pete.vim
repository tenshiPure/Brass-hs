let ext = expand('%:e')

if ext == 'hs'
	call pete#conf#default()
	let g:tool_path = ''
	let g:tool_mode = 3

elseif ext == 'hamlet'
	let g:tool_path = ''
	let g:tool_mode = 0

elseif ext == 'julius'
	let g:tool_path = ''
	let g:tool_mode = 0

elseif ext == 'cassius'
	let g:tool_path = ''
	let g:tool_mode = 0

elseif ext == 'lucius'
	let g:tool_path = ''
	let g:tool_mode = 0

else
	call pete#conf#default()
endif
