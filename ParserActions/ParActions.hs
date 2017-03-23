{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParActions where
import AbsActions
import LexActions
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn18 :: (String) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (String)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (IdAct) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (IdAct)
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (Actions) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (Actions)
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: ([Action]) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> ([Action])
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (Action) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (Action)
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: ([IdAct]) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> ([IdAct])
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (Program) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (Program)
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: ([Args]) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> ([Args])
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (Ass) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (Ass)
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: (Arith) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> (Arith)
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (Val) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (Val)
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (Type) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (Type)
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: (Template) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> (Template)
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (Args) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (Args)
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (Params) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> (Params)
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: ([Param]) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> ([Param])
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: (Param) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> (Param)
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x00\x00\x6a\x00\xaa\x00\xaa\x00\x9a\x00\xaa\x00\xaa\x00\x98\x00\xaa\x00\xaa\x00\x92\x00\xaf\x00\xa8\x00\xa8\x00\xa7\x00\x00\x00\x00\x00\xa4\x00\x00\x00\xa4\x00\xac\x00\xa2\x00\xa1\x00\x00\x00\x00\x00\x9f\x00\x9e\x00\x00\x00\x9c\x00\x00\x00\x9c\x00\x00\x00\x01\x00\x00\x00\x9c\x00\x90\x00\x00\x00\x9c\x00\x97\x00\x9c\x00\x9c\x00\xa9\x00\xab\x00\x89\x00\x80\x00\x77\x00\x8d\x00\x77\x00\x00\x00\x00\x00\x00\x00\xa5\x00\xa3\x00\x8e\x00\x8a\x00\x00\x00\x3b\x00\x6b\x00\x30\x00\x74\x00\x62\x00\x73\x00\x5e\x00\x5e\x00\x5e\x00\x88\x00\x86\x00\x86\x00\x86\x00\x00\x00\xfb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x5e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x69\x00\x68\x00\x64\x00\x5a\x00\x4b\x00\x00\x00\x00\x00\x53\x00\x00\x00\xfb\xff\x34\x00\x00\x00\x6a\x00\x46\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x61\x00\x33\x00\x84\x00\x5c\x00\x4d\x00\x2c\x00\x05\x00\x27\x00\x6d\x00\x1f\x00\x25\x00\x03\x00\x20\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x66\x00\x00\x00\x00\x00\x5f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x55\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x32\x00\x7e\x00\x00\x00\x7e\x00\x00\x00\x00\x00\x22\x00\x0e\x00\x23\x00\x3f\x00\x1e\x00\x58\x00\x51\x00\x4a\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x00\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xed\xff\xed\xff\xe6\xff\xe2\xff\x00\x00\xdf\xff\x00\x00\x00\x00\xd4\xff\xd3\xff\x00\x00\x00\x00\xcd\xff\x00\x00\x00\x00\x00\x00\xf0\xff\xc9\xff\x00\x00\xef\xff\x00\x00\xcb\xff\x00\x00\x00\x00\xcf\xff\xd0\xff\x00\x00\x00\x00\xd1\xff\x00\x00\xd2\xff\x00\x00\xd6\xff\xd4\xff\xd8\xff\x00\x00\xd4\xff\xd9\xff\x00\x00\x00\x00\x00\x00\x00\x00\xde\xff\x00\x00\x00\x00\xe2\xff\x00\x00\xd9\xff\x00\x00\xeb\xff\xe3\xff\xe4\xff\x00\x00\x00\x00\x00\x00\x00\x00\xed\xff\xe6\xff\x00\x00\xe6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe2\xff\xdf\xff\xd4\xff\xd4\xff\xd4\xff\xe1\xff\xdf\xff\xd5\xff\xd7\xff\xce\xff\xcc\xff\x00\x00\xca\xff\xdd\xff\xdc\xff\xda\xff\xdb\xff\x00\x00\x00\x00\x00\x00\x00\x00\xcd\xff\xea\xff\xec\xff\x00\x00\xe8\xff\xdf\xff\x00\x00\xe0\xff\xe6\xff\x00\x00\xe5\xff\xe9\xff\xe7\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x0e\x00\x01\x00\x00\x00\x01\x00\x01\x00\x01\x00\x01\x00\x0d\x00\x01\x00\x01\x00\x10\x00\x11\x00\x08\x00\x0d\x00\x01\x00\x0d\x00\x10\x00\x11\x00\x0f\x00\x10\x00\x0f\x00\x10\x00\x0f\x00\x10\x00\x10\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x01\x00\x07\x00\x00\x00\x07\x00\x01\x00\x07\x00\x01\x00\x0d\x00\x01\x00\x0d\x00\x0b\x00\x0d\x00\x00\x00\x01\x00\x0e\x00\x0c\x00\x09\x00\x0c\x00\x01\x00\x07\x00\x02\x00\x03\x00\x03\x00\x06\x00\x08\x00\x0d\x00\x0a\x00\x0b\x00\x0c\x00\x09\x00\x0e\x00\x0f\x00\x01\x00\x11\x00\x12\x00\x08\x00\x05\x00\x0a\x00\x0b\x00\x0c\x00\x02\x00\x0e\x00\x00\x00\x01\x00\x11\x00\x12\x00\x01\x00\x04\x00\x06\x00\x00\x00\x01\x00\x06\x00\x0a\x00\x02\x00\x01\x00\x06\x00\x00\x00\x01\x00\x05\x00\x0a\x00\x02\x00\x01\x00\x06\x00\x00\x00\x01\x00\x05\x00\x0a\x00\x02\x00\x03\x00\x06\x00\x00\x00\x01\x00\x04\x00\x0a\x00\x02\x00\x02\x00\x06\x00\x00\x00\x01\x00\x11\x00\x0a\x00\x0f\x00\x08\x00\x06\x00\x0a\x00\x0b\x00\x0c\x00\x0a\x00\x0e\x00\x01\x00\x06\x00\x11\x00\x04\x00\x12\x00\x06\x00\x01\x00\x08\x00\x09\x00\x04\x00\x10\x00\x06\x00\x01\x00\x08\x00\x09\x00\x04\x00\x12\x00\x06\x00\x01\x00\x08\x00\x09\x00\x01\x00\x01\x00\x03\x00\x11\x00\x05\x00\x0d\x00\x07\x00\x0d\x00\x10\x00\x11\x00\x10\x00\x11\x00\x03\x00\x12\x00\x05\x00\x0d\x00\x07\x00\x0d\x00\x10\x00\x11\x00\x10\x00\x11\x00\x01\x00\x0d\x00\x01\x00\x0d\x00\x10\x00\x11\x00\x10\x00\x11\x00\x01\x00\x04\x00\x12\x00\x11\x00\x04\x00\x12\x00\x11\x00\x04\x00\x12\x00\xff\xff\x12\x00\x10\x00\xff\xff\x11\x00\xff\xff\x11\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x59\x00\x43\x00\x18\x00\x19\x00\x11\x00\x27\x00\x11\x00\x1c\x00\x11\x00\x11\x00\x11\x00\x14\x00\x28\x00\x25\x00\x55\x00\x1a\x00\x11\x00\x14\x00\x4d\x00\x15\x00\x4b\x00\x15\x00\x14\x00\x15\x00\x12\x00\x18\x00\x19\x00\x18\x00\x19\x00\x18\x00\x19\x00\x1e\x00\x5f\x00\x56\x00\x4e\x00\x1c\x00\x52\x00\x1c\x00\x2a\x00\x25\x00\x2a\x00\x1f\x00\x2a\x00\x18\x00\x19\x00\x16\x00\x54\x00\x26\x00\x1d\x00\x2b\x00\x29\x00\x3d\x00\x3b\x00\x39\x00\x4a\x00\x35\x00\x2a\x00\x36\x00\x37\x00\x38\x00\x5f\x00\x39\x00\xee\xff\x2d\x00\x14\x00\xee\xff\x35\x00\x53\x00\x36\x00\x37\x00\x38\x00\x62\x00\x39\x00\x20\x00\x21\x00\x14\x00\xff\xff\x2b\x00\x18\x00\x22\x00\x20\x00\x21\x00\x2c\x00\x4f\x00\x61\x00\x2d\x00\x22\x00\x20\x00\x21\x00\x46\x00\x50\x00\x5b\x00\x2d\x00\x22\x00\x20\x00\x21\x00\x2e\x00\x51\x00\x3a\x00\x3b\x00\x22\x00\x20\x00\x21\x00\x5c\x00\x48\x00\x5d\x00\x5e\x00\x22\x00\x20\x00\x21\x00\x14\x00\x49\x00\x58\x00\x35\x00\x22\x00\x36\x00\x37\x00\x38\x00\x23\x00\x39\x00\x2f\x00\x59\x00\x14\x00\x62\x00\xff\xff\x31\x00\x2f\x00\x32\x00\x33\x00\x3c\x00\x11\x00\x31\x00\x2f\x00\x32\x00\x33\x00\x30\x00\xff\xff\x31\x00\x3f\x00\x32\x00\x33\x00\x43\x00\x40\x00\x44\x00\x14\x00\x45\x00\x25\x00\x46\x00\x1c\x00\x11\x00\x14\x00\x11\x00\x14\x00\x44\x00\xff\xff\x45\x00\x25\x00\x46\x00\x1c\x00\x11\x00\x14\x00\x11\x00\x14\x00\x41\x00\x25\x00\x42\x00\x1c\x00\x11\x00\x14\x00\x11\x00\x14\x00\x43\x00\x48\x00\xff\xff\x14\x00\x4d\x00\xff\xff\x14\x00\x18\x00\xff\xff\x00\x00\xff\xff\x11\x00\x00\x00\x14\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (15, 54) [
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54)
	]

happy_n_terms = 19 :: Int
happy_n_nonterms = 17 :: Int

happyReduce_15 = happySpecReduce_1  0# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn18
		 (happy_var_1
	)}

happyReduce_16 = happySpecReduce_1  1# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_IdAct happy_var_1)) -> 
	happyIn19
		 (IdAct (happy_var_1)
	)}

happyReduce_17 = happySpecReduce_1  2# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (AbsActions.Actions (reverse happy_var_1)
	)}

happyReduce_18 = happySpecReduce_0  3# happyReduction_18
happyReduction_18  =  happyIn21
		 ([]
	)

happyReduce_19 = happySpecReduce_3  3# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_2 of { happy_var_2 -> 
	happyIn21
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_20 = happySpecReduce_1  4# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (AbsActions.ActProg happy_var_1
	)}

happyReduce_21 = happySpecReduce_3  4# happyReduction_21
happyReduction_21 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_2 of { happy_var_2 -> 
	happyIn22
		 (AbsActions.ActBlock happy_var_2
	)}

happyReduce_22 = happyReduce 6# 4# happyReduction_22
happyReduction_22 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut30 happy_x_3 of { happy_var_3 -> 
	case happyOut25 happy_x_5 of { happy_var_5 -> 
	happyIn22
		 (AbsActions.ActCreate happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_23 = happyReduce 4# 4# happyReduction_23
happyReduction_23 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut19 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 (AbsActions.ActBang happy_var_3
	) `HappyStk` happyRest}

happyReduce_24 = happyReduce 6# 4# happyReduction_24
happyReduction_24 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut23 happy_x_3 of { happy_var_3 -> 
	case happyOut22 happy_x_6 of { happy_var_6 -> 
	happyIn22
		 (AbsActions.ActCond happy_var_3 happy_var_6
	) `HappyStk` happyRest}}

happyReduce_25 = happySpecReduce_0  4# happyReduction_25
happyReduction_25  =  happyIn22
		 (AbsActions.ActSkip
	)

happyReduce_26 = happyReduce 5# 4# happyReduction_26
happyReduction_26 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_3 of { happy_var_3 -> 
	case happyOut32 happy_x_4 of { happy_var_4 -> 
	happyIn22
		 (AbsActions.ActLog happy_var_3 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_27 = happySpecReduce_1  4# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (AbsActions.ActArith happy_var_1
	)}

happyReduce_28 = happySpecReduce_1  4# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (AbsActions.ActAssig happy_var_1
	)}

happyReduce_29 = happySpecReduce_0  5# happyReduction_29
happyReduction_29  =  happyIn23
		 ([]
	)

happyReduce_30 = happySpecReduce_2  5# happyReduction_30
happyReduction_30 happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_2 of { happy_var_2 -> 
	happyIn23
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_31 = happyReduce 4# 6# happyReduction_31
happyReduction_31 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (AbsActions.Prog happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_32 = happySpecReduce_0  7# happyReduction_32
happyReduction_32  =  happyIn25
		 ([]
	)

happyReduce_33 = happySpecReduce_1  7# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 ((:[]) happy_var_1
	)}

happyReduce_34 = happySpecReduce_3  7# happyReduction_34
happyReduction_34 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_35 = happySpecReduce_3  8# happyReduction_35
happyReduction_35 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 (AbsActions.Ass happy_var_1 happy_var_3
	)}}

happyReduce_36 = happySpecReduce_3  8# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 (AbsActions.AssInc happy_var_1 happy_var_3
	)}}

happyReduce_37 = happySpecReduce_3  8# happyReduction_37
happyReduction_37 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 (AbsActions.AssDec happy_var_1 happy_var_3
	)}}

happyReduce_38 = happySpecReduce_1  9# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 (AbsActions.Arith happy_var_1
	)}

happyReduce_39 = happySpecReduce_1  10# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (AbsActions.ValMC happy_var_1
	)}

happyReduce_40 = happySpecReduce_2  10# happyReduction_40
happyReduction_40 happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_2 of { happy_var_2 -> 
	happyIn28
		 (AbsActions.ValV happy_var_1 happy_var_2
	)}}

happyReduce_41 = happySpecReduce_1  10# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (AbsActions.ValS happy_var_1
	)}

happyReduce_42 = happySpecReduce_2  10# happyReduction_42
happyReduction_42 happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_2 of { happy_var_2 -> 
	happyIn28
		 (AbsActions.ValNew happy_var_2
	)}

happyReduce_43 = happySpecReduce_0  10# happyReduction_43
happyReduction_43  =  happyIn28
		 (AbsActions.ValNil
	)

happyReduce_44 = happySpecReduce_0  11# happyReduction_44
happyReduction_44  =  happyIn29
		 (AbsActions.TypeNil
	)

happyReduce_45 = happySpecReduce_1  11# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (AbsActions.Type happy_var_1
	)}

happyReduce_46 = happySpecReduce_1  12# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 (AbsActions.Temp happy_var_1
	)}

happyReduce_47 = happySpecReduce_1  13# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 (AbsActions.ArgsId happy_var_1
	)}

happyReduce_48 = happySpecReduce_1  13# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 (AbsActions.ArgsS happy_var_1
	)}

happyReduce_49 = happySpecReduce_2  13# happyReduction_49
happyReduction_49 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 (AbsActions.ArgsNew happy_var_2
	)}

happyReduce_50 = happySpecReduce_0  14# happyReduction_50
happyReduction_50  =  happyIn32
		 (AbsActions.ParamsNil
	)

happyReduce_51 = happySpecReduce_2  14# happyReduction_51
happyReduction_51 happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_2 of { happy_var_2 -> 
	happyIn32
		 (AbsActions.Params happy_var_2
	)}

happyReduce_52 = happySpecReduce_1  15# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 ((:[]) happy_var_1
	)}

happyReduce_53 = happySpecReduce_3  15# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_3 of { happy_var_3 -> 
	happyIn33
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_54 = happySpecReduce_1  16# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 (AbsActions.Param happy_var_1
	)}

happyNewToken action sts stk [] =
	happyDoAction 18# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TL happy_dollar_dollar) -> cont 16#;
	PT _ (T_IdAct happy_dollar_dollar) -> cont 17#;
	_ -> happyError' (tk:tks)
	}

happyError_ 18# tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pActions tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut20 x))

pListAction tks = happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut21 x))

pAction tks = happySomeParser where
  happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut22 x))

pListIdAct tks = happySomeParser where
  happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut23 x))

pProgram tks = happySomeParser where
  happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut24 x))

pListArgs tks = happySomeParser where
  happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut25 x))

pAss tks = happySomeParser where
  happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut26 x))

pArith tks = happySomeParser where
  happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut27 x))

pVal tks = happySomeParser where
  happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut28 x))

pType tks = happySomeParser where
  happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut29 x))

pTemplate tks = happySomeParser where
  happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (happyOut30 x))

pArgs tks = happySomeParser where
  happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (happyOut31 x))

pParams tks = happySomeParser where
  happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (happyOut32 x))

pListParam tks = happySomeParser where
  happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (happyOut33 x))

pParam tks = happySomeParser where
  happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (happyOut34 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 9 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 46 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
