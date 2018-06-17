module Bittrex.ProtoUtil where
import       Bittrex.Proto


nounce (BtrMsgState s) = btrStateNounce s
nounce (BtrMsgDelta d) = btrDeltaNounce d

marketName (BtrMsgState s) = btrStateMarketName s
marketName (BtrMsgDelta d) = btrDeltaMarketName d

