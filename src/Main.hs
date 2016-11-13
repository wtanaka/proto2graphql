#!/usr/bin/env runhaskell
--
-- Copyright 2016 Wesley Tanaka <http://wtanaka.com/>
--
-- This file is part of proto2graphql
--
-- proto2graphql is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- proto2graphql is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with proto2graphql.  If not, see <http://www.gnu.org/licenses/>.


import Control.Monad.State
import Data.ByteString.Lazy.UTF8 (toString)
-- import Data.Either (Either(..))
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (pack)
import Debug.Trace (trace)
import qualified Data.ByteString.Lazy as BSL
import qualified Text.GraphQLSchema.AST as GQLS
import qualified Text.ProtocolBuffers.Reflections as PBR
import System.Directory (getCurrentDirectory)
import System.FilePath
import Text.DescriptorProtos.FieldDescriptorProto.Type(Type(..))
import Text.DescriptorProtos.FileDescriptorProto (FileDescriptorProto)
import Text.Parsec.Error (ParseError)
import Text.ProtocolBuffers.Basic (EnumCode(..))
import Text.ProtocolBuffers.Basic (FieldType(..))
import Text.ProtocolBuffers.Basic (utf8)
import Text.ProtocolBuffers.Identifiers (checkDIString)
import Text.ProtocolBuffers.Identifiers (FIName(..))
import Text.ProtocolBuffers.Identifiers (mangle)
import Text.ProtocolBuffers.Identifiers (MName(..))
import Text.ProtocolBuffers.ProtoCompile.MakeReflections (makeProtoInfo)
import Text.ProtocolBuffers.ProtoCompile.MakeReflections (serializeFDP)
import Text.ProtocolBuffers.ProtoCompile.Parser (parseProto)
import Text.ProtocolBuffers.ProtoCompile.Resolve (loadCodeGenRequest)
import Text.ProtocolBuffers.ProtoCompile.Resolve (loadProto)
import Text.ProtocolBuffers.ProtoCompile.Resolve (LocalFP(..))
import Text.ProtocolBuffers.ProtoCompile.Resolve (makeNameMaps)
import Text.ProtocolBuffers.ProtoCompile.Resolve (makeNameMaps)

newtype ParseErrors = ParseErrors [String]
   deriving (Show)

toPrefix :: String -> [MName String]
toPrefix s = case checkDIString s of
               Left msg -> error $ "Bad module name in options:"++show s++"\n"++msg
               Right (True,_) -> error $ "Bad module name in options (cannot start with '.'): "++show s
               Right (False,ms) -> map mangle ms

parseString :: BSL.ByteString -> Either ParseError FileDescriptorProto
parseString = let
   filename = "example.proto"
   fdp = (parseProto filename)
   nameMap = makeNameMaps [] []
   -- protoInfo = (makeProtoInfo () )
   in fdp

handleParseResult :: Either ParseError FileDescriptorProto -> BSL.ByteString
handleParseResult x =
   case x of
      Left parseError -> (encodeUtf8 . pack . show) parseError
      -- Right fileDescProto -> serializeFDP fileDescProto
      Right fileDescProto -> encodeUtf8 . pack $ "success"

proto2graphql :: BSL.ByteString -> BSL.ByteString
proto2graphql stdin =
   (handleParseResult . parseString) stdin

protoFieldType :: PBR.FieldInfo -> Type
protoFieldType = toEnum . getFieldType . PBR.typeCode

-- | linear search EnumInfo
findEnum :: [PBR.EnumInfo] -> PBR.FieldInfo
   -> State ParseErrors (Maybe PBR.EnumInfo)
findEnum [] fieldInfo = do
   ParseErrors e <- get
   put $ ParseErrors ("newError":e)
   return Nothing
findEnum (x:xs) fieldInfo = do
   rest <- findEnum xs fieldInfo
   return rest
   --let fName = PBR.fieldName fieldInfo
    --  in trace ("findEnum found " ++ show x) return rest

graphQLEnum :: PBR.EnumInfo -> GQLS.Type
graphQLEnum enumInfo = GQLS.EnumType {
   GQLS.enumName = nameOfEnum enumInfo,
   GQLS.enumValues = map enumValue $ PBR.enumValues enumInfo
   }

makeEnum :: [PBR.EnumInfo] -> PBR.FieldInfo
   -> State ParseErrors (Maybe GQLS.Type)
makeEnum allPbEnums pbField = do
   maybeEnumInfo <- findEnum allPbEnums pbField
   return $ maybe Nothing (Just . graphQLEnum) $ maybeEnumInfo

typeOfField :: [PBR.EnumInfo] -> PBR.FieldInfo
   -> State ParseErrors (Maybe GQLS.Type)
typeOfField allPbEnums pbField = case protoFieldType pbField of
   TYPE_BOOL -> return $ Just GQLS.BooleanType
   TYPE_BYTES -> return $ Just GQLS.StringType
   TYPE_DOUBLE -> return $ Just GQLS.FloatType
   TYPE_ENUM -> do
      gqlType <- makeEnum allPbEnums pbField
      return $ maybe Nothing Just gqlType
   TYPE_FIXED32 -> return $ Just GQLS.IntType
   TYPE_FIXED64 -> return $ Just GQLS.IntType
   TYPE_FLOAT -> return $ Just GQLS.FloatType
   TYPE_GROUP -> error ("unimplemented " ++ show TYPE_GROUP)
   TYPE_INT32 -> return $ Just GQLS.IntType
   TYPE_INT64 -> return $ Just GQLS.IntType
   TYPE_MESSAGE -> error ("unimplemented " ++ show TYPE_MESSAGE)
   TYPE_SFIXED32 -> return $ Just GQLS.IntType
   TYPE_SFIXED64 -> return $ Just GQLS.IntType
   TYPE_SINT32 -> return $ Just GQLS.IntType
   TYPE_SINT64 -> return $ Just GQLS.IntType
   TYPE_STRING -> return $ Just GQLS.StringType
   TYPE_UINT32 -> return $ Just GQLS.IntType
   TYPE_UINT64 -> return $ Just GQLS.IntType


nameOfField :: PBR.FieldInfo -> String
nameOfField = toString . utf8 . fiName . PBR.protobufName' . PBR.fieldName

nameOfEnum :: PBR.EnumInfo -> String
nameOfEnum = toString . utf8 . fiName . PBR.protobufName .  PBR.enumName

enumValue :: (EnumCode, String) -> GQLS.EnumValue
enumValue x = GQLS.EnumValue {
   GQLS.evName = snd x,
   GQLS.evValue = show $ getEnumCode $ fst x
   }


foobar :: PBR.FieldInfo -> GQLS.Type -> Maybe GQLS.ObjectField
foobar pbField ft = Just GQLS.ObjectField {
      GQLS.fieldName = nameOfField pbField,
      GQLS.fieldType = ft
   }

-- | Linear search Enum
graphQLField :: [PBR.EnumInfo] -> PBR.FieldInfo
   -> State ParseErrors (Maybe GQLS.ObjectField)
graphQLField enumInfos pbField = do
   fieldType <- (typeOfField enumInfos pbField)
   return $ maybe Nothing (foobar pbField) (fieldType :: (Maybe GQLS.Type))

mname2string :: MName a -> a
mname2string (MName str) = str

-- | TODO:
graphQLObject :: [PBR.EnumInfo] -> PBR.DescriptorInfo
   -> State ParseErrors GQLS.Type
graphQLObject allPbEnums descInfo = do
   let pbFieldList = trace ("graphQLObject " ++ show (toList (PBR.fields descInfo))) toList (PBR.fields descInfo)
   gqlfields <- mapM (graphQLField allPbEnums) pbFieldList
   trace (show gqlfields) return GQLS.ObjectType {
      GQLS.objName = mname2string $ PBR.baseName $ PBR.descName descInfo,
      GQLS.objFields = catMaybes gqlfields
   }

graphQL' :: PBR.ProtoInfo -> State ParseErrors [GQLS.Type]
graphQL' protoInfo = do
   let enumInfos = PBR.enums protoInfo :: [PBR.EnumInfo]
   let diconvert = graphQLObject []
   result <- mapM diconvert (PBR.messages protoInfo)
   return result

graphQL :: PBR.ProtoInfo -> ([GQLS.Type], ParseErrors)
graphQL protoInfo = runState (graphQL' protoInfo) (ParseErrors [])

main :: IO ()
main = do
   cwd <- getCurrentDirectory
   pwd <- fmap LocalFP getCurrentDirectory
   (env, fdps) <- loadProto [pwd] $ LocalFP (joinPath [cwd, "example.proto"])
   let
      nameMap = either error id (makeNameMaps [] [] env)
      protoInfos = map piMaker fdps where
         piMaker fdps = makeProtoInfo (False, False, False) nameMap fdps
      in do
         putStrLn $ show $ map graphQL protoInfos
