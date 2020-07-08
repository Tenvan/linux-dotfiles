function! myspacevim#before() abort

  let g:neomake_enabled_c_makers = ['clang']

  nnoremap x "_x
  nnoremap d "_d
  nnoremap D "_D
  vnoremap d "_d

  nnoremap <leader>d ""d
  nnoremap <leader>D ""D
  vnoremap <leader>d ""d
  nnoremap jk <Esc>

endfunction

function! myspacevim#after() abort
 
endfunction

