{-# LANGUAGE PatternSynonyms #-}
{- HLINT ignore "Use camelCase" -}
module Apigen.Patterns where

pattern SYM_APIGEN_IGNORE :: Int
pattern SYM_APIGEN_IGNORE = 0

pattern TY_void, TY_char, TY_bool, TY_int8_t, TY_uint8_t, TY_int16_t, TY_uint16_t, TY_int32_t, TY_uint32_t, TY_int64_t, TY_uint64_t, TY_size_t :: Int
pattern TY_void     = 1
pattern TY_char     = 2
pattern TY_bool     = 3
pattern TY_int8_t   = 4
pattern TY_uint8_t  = 5
pattern TY_int16_t  = 6
pattern TY_uint16_t = 7
pattern TY_int32_t  = 8
pattern TY_uint32_t = 9
pattern TY_int64_t  = 10
pattern TY_uint64_t = 11
pattern TY_size_t   = 12

pattern SYM_abs, SYM_max :: Int
pattern SYM_abs = 13
pattern SYM_max = 14
