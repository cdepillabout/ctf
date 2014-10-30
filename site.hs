--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Control.Exception (finally)
import Control.Monad (filterM, forM, liftM, unless)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Lazy.Char8 as LazyByteStringChar8
import Data.ByteString.Search (replace)
import Data.ByteString.Search.Substitution ()
import Data.Functor ((<$>))
import Data.List (isSuffixOf, sort, stripPrefix)
import Data.Maybe (fromJust)
import Data.Monoid (mappend)
import Debug.Trace
import Hakyll
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>), takeDirectory)


writeupsDir :: FilePath
writeupsDir = "writeups"

indexFileName :: FilePath
indexFileName = "index.md"

outputDir :: FilePath
outputDir = "_site"

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mbool monad = flip unless monad =<< mbool

createIndexMdFiles :: IO ()
createIndexMdFiles = getRecursiveDirs writeupsDir >>= mapM_ writeIndexMd
  where
    writeIndexMd :: FilePath -> IO ()
    writeIndexMd dir = do
        let fullPath = dir </> indexFileName
        unlessM (doesFileExist fullPath) $ writeFile fullPath ""

getRecursiveDirs :: FilePath -> IO [FilePath]
getRecursiveDirs topDir = filterM doesDirectoryExist =<< getDirRecursiveContents topDir

getDirRecursiveContents :: FilePath -> IO [FilePath]
getDirRecursiveContents dir = do
    names <- getDirectoryContents dir
    let properNames = removeDotDirs names
    paths <- forM properNames $ \name -> do
        let path = dir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then (path :) <$> getDirRecursiveContents path
            else return [path]
    return (concat paths)

removeDotDirs :: [FilePath] -> [FilePath]
removeDotDirs = filter (`notElem` [".", ".."])

addDirectoryListToIndexHtml :: IO ()
addDirectoryListToIndexHtml = do
    allFiles <- getDirRecursiveContents outputDir
    let indexHtmlList = filter isIndexHtml allFiles
    mapM_ addFileList indexHtmlList
  where
    isIndexHtml :: FilePath -> Bool
    isIndexHtml path = (path == "index.html" || "/index.html" `isSuffixOf` path)

    addFileList :: FilePath -> IO ()
    addFileList path = do
        dirContents <- fmap (sort . removeDotDirs) $ getDirContentsOfFile path
        let htmlFileList = LazyByteStringChar8.pack $ createHtmlFileList dirContents
        fileContents <- ByteString.readFile path
        let replacementByteString = ByteStringChar8.pack "REPLACE_THIS_WITH_FILE_LIST"
        let newFileContents = replace replacementByteString htmlFileList fileContents
        LazyByteString.writeFile path newFileContents

    createHtmlFileList :: [FilePath] -> String
    createHtmlFileList paths = "<ul>" ++ lis ++ "</ul>"
      where
        lis = concatMap (\path -> "<li>" ++ path ++ "</li>") paths

    getDirContentsOfFile :: FilePath -> IO [FilePath]
    getDirContentsOfFile "index.html" = getDirectoryContents outputDir
    getDirContentsOfFile file = getDirectoryContents $ takeDirectory file



main :: IO ()
main = do
        createIndexMdFiles
        -- hakyll exits with the exitWith function (returning a meaningful
        -- return code if it succeeds or fails) when running build, but we
        -- want to do something after it, so we wrap it in a finally
        finally doHakyll addDirectoryListToIndexHtml

--------------------------------------------------------------------------------
doHakyll :: IO ()
doHakyll = hakyll $ do

    {-
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        -- route idRoute
        route $ constRoute "_old/indexwhatwhat.html"
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    -}
    match "templates/*" $ compile templateCompiler

    match (fromGlob $ writeupsDir ++ "/**index.md") $ do
        route $ stripOffCtfPrefix `composeRoutes` setExtension "html"
        compile $ do
            --ident <- getUnderlying
            --traceShowM ident
            pandocCompiler
                >>= loadAndApplyTemplate "templates/index.html"   postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match (fromGlob $ writeupsDir ++ "/**") $ do
        route stripOffCtfPrefix
        compile copyFileCompiler

--------------------------------------------------------------------------------

stripOffCtfPrefix :: Routes
stripOffCtfPrefix = customRoute $ fromJust . stripPrefix (writeupsDir ++ "/") . toFilePath

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
