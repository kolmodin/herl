{-# LANGUAGE OverloadedStrings #-}
module AtomTableErlang
       (basic, am_exit, am_EXIT, am_throw, am_error, am_nocatch,
        am_badmatch, am_case_clause, am_try_clause, am_badfun, am_badarity,
        am_undef, am_erlang, am_now, am_get_stacktrace, am_sign_plus,
        am_sign_minus, am_sign_mult, am_spawn, am_io, am_format)
       where
import AtomTable
import ETerm (AtomNo(..))
 
basic :: AtomTable
basic
  = fromList
      ["exit", "EXIT", "throw", "error", "nocatch", "badmatch",
       "case_clause", "try_clause", "badfun", "badarity", "undef",
       "erlang", "now", "get_stacktrace", "+", "-", "*", "spawn", "io",
       "format"]
 
am_exit :: AtomNo
am_exit = AtomNo 0
 
am_EXIT :: AtomNo
am_EXIT = AtomNo 1
 
am_throw :: AtomNo
am_throw = AtomNo 2
 
am_error :: AtomNo
am_error = AtomNo 3
 
am_nocatch :: AtomNo
am_nocatch = AtomNo 4
 
am_badmatch :: AtomNo
am_badmatch = AtomNo 5
 
am_case_clause :: AtomNo
am_case_clause = AtomNo 6
 
am_try_clause :: AtomNo
am_try_clause = AtomNo 7
 
am_badfun :: AtomNo
am_badfun = AtomNo 8
 
am_badarity :: AtomNo
am_badarity = AtomNo 9
 
am_undef :: AtomNo
am_undef = AtomNo 10
 
am_erlang :: AtomNo
am_erlang = AtomNo 11
 
am_now :: AtomNo
am_now = AtomNo 12
 
am_get_stacktrace :: AtomNo
am_get_stacktrace = AtomNo 13
 
am_sign_plus :: AtomNo
am_sign_plus = AtomNo 14
 
am_sign_minus :: AtomNo
am_sign_minus = AtomNo 15
 
am_sign_mult :: AtomNo
am_sign_mult = AtomNo 16
 
am_spawn :: AtomNo
am_spawn = AtomNo 17
 
am_io :: AtomNo
am_io = AtomNo 18
 
am_format :: AtomNo
am_format = AtomNo 19