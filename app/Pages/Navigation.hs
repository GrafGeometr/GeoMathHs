module Pages.Navigation where

import Imports

feed :: App Response
feed = do
    $(template "feed")

contests :: App Response
contests = do
    $(template "contests")

collections :: App Response
collections = do
    $(template "collections")

editor :: App Response
editor = do
    $(template "editor")
