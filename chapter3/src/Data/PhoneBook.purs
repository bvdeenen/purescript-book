module Data.PhoneBook where

import Data.List
import Data.Maybe

import Control.Plus (empty)

type Entry = { firstName :: String, lastName :: String, phone :: String } 

type PhoneBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName ++ ", " ++ entry.firstName ++ ": " ++ entry.phone


emptyBook :: PhoneBook
emptyBook = empty

insertEntry :: Entry -> PhoneBook -> PhoneBook
insertEntry = Cons
 
findEntry :: String -> String -> PhoneBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry 
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

printEntry :: String -> String -> PhoneBook -> Maybe String
printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book


book = insertEntry {firstName:"Bart", lastName:"van Deenen", phone:"020"} 
    (insertEntry {firstName:"Bart", lastName:"van Deenen", phone:"0644841161"}  
    (insertEntry {firstName:"Maja", lastName:"Vlaming", phone:"0616891921"} 
    emptyBook))

findPhone :: String -> PhoneBook -> Maybe Entry
findPhone phone = head <<< filter filterEntry 
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.phone == phone 

entryExists :: String -> String -> PhoneBook -> Boolean
entryExists firstName lastName = not <<< null <<< filter filterEntry 
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

listBook :: List Entry -> List String
listBook = mapMaybe $ \e -> Just $ showEntry e 

filterDup :: PhoneBook -> PhoneBook
filterDup phonebook = Data.List.nubBy comp phonebook
  where
  comp :: Entry -> Entry -> Boolean
  comp a b = a.firstName == b.firstName && a.lastName == b.lastName

