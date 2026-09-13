vim9script
# hive-vessel editor wire v1: connect Vim to hive (reference client).
# SPDX-License-Identifier: MIT

if exists('g:loaded_hive_wire') || v:version < 900 || !has('channel')
  finish
endif
g:loaded_hive_wire = 1

import autoload 'hive_wire.vim'

def g:HiveOp(op: string, params: any): dict<any>
  return hive_wire.Op(op, params)
enddef

command! HiveWireConnect hive_wire.Connect()
command! HiveWireDisconnect hive_wire.Disconnect()
command! HiveWireStatus echo hive_wire.Status()
command! -nargs=1 HiveWireShowTerminal hive_wire.ShowTerminal(<q-args>)

augroup hive_wire
  autocmd!
  autocmd VimEnter * hive_wire.Start()
  autocmd FocusGained * hive_wire.SendEvent('focus', {})
  autocmd VimLeavePre * hive_wire.Disconnect()
augroup END
