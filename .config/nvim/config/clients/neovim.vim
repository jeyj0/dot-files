" customize fzf to open in floating window
let $FZF_DEFAULT_OPTS .= ' --layout=reverse'

function! FloatingFZF()
    let l:buf = nvim_create_buf(v:false, v:true)

    let l:screenHeight = &lines
    let l:screenWidth = &columns

    " breakpoints width
    if l:screenWidth <= 160
        let l:winWidth = l:screenWidth
        let l:winOffsetX = 0
    else
        let l:winWidth = screenWidth / 4 * 3
        let l:winOffsetX = screenWidth / 8
    endif

    " breakpoints height
    if l:screenHeight <= 30
        let l:winHeight = screenHeight - 2
        let l:winOffsetY = 0
    else
        let l:winHeight = screenHeight / 4 * 3
        let l:winOffsetY = screenHeight / 8
    endif

    let l:opts = {
        \ 'relative': 'editor',
        \ 'row': winOffsetY,
        \ 'col': winOffsetX,
        \ 'width': winWidth,
        \ 'height': winHeight,
        \ 'style': 'minimal'
        \ }

    let l:win = nvim_open_win(l:buf, v:true, l:opts)
    call setwinvar(l:win, '&winhl', 'NormalFloat:TabLine')
endfunction

let g:fzf_layout = { 'window': 'call FloatingFZF()' }

