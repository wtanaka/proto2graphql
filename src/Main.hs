#!/usr/bin/env runhaskell
--
-- Copyright 2016 Wesley Tanaka <http://wtanaka.com/>
--
-- This file is part of gitstat.
--
-- gitstat is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- gitstat is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with gitstat.  If not, see <http://www.gnu.org/licenses/>.
import qualified Data.ByteString.Lazy as BSL
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)

proto2graphql :: BSL.ByteString -> BSL.ByteString
proto2graphql x = (encodeUtf8 . pack) "Hello"

main :: IO ()
main = do
   BSL.interact $ proto2graphql

