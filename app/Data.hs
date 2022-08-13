module Data where

type Col = Int 

type Row = Int

data GameState = Running | Won Player | Draw deriving (Show,Eq)

data SlotState = Empty | Player Player deriving (Show,Eq)

data Slot = Slot {rPos:: Row, cPos:: Col, slotState:: SlotState} deriving Show

type BoardState = [Slot]

data Player = Red | Yellow deriving (Eq, Show)
