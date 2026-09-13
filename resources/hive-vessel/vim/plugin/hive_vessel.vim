" hive-vessel -- connect Vim to a hive :vim-channel executor.
"
"   :HiveVesselConnect localhost:7922
"
" The channel is opened in JSON mode, so the server drives Vim with channel
" commands (:help channel-commands) that call into autoload/hive_vessel.vim.
"
" SPDX-License-Identifier: MIT

if exists('g:loaded_hive_vessel')
  finish
endif
let g:loaded_hive_vessel = 1

function! HiveVesselConnect(address) abort
  if exists('g:hive_vessel_channel') && ch_status(g:hive_vessel_channel) ==# 'open'
    call ch_close(g:hive_vessel_channel)
  endif
  let g:hive_vessel_channel = ch_open(a:address, {'mode': 'json', 'waittime': 2000})
  return ch_status(g:hive_vessel_channel)
endfunction

command! -nargs=1 HiveVesselConnect echo HiveVesselConnect(<q-args>)
