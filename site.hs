--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (filterM, forM, join, liftM, unless)
import Data.Functor ((<$>))
import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Data.Monoid (mappend)
import Debug.Trace ()
import Hakyll
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))


writeupsDir :: String
writeupsDir = "writeups"

indexFileName :: String
indexFileName = "index.md"

unlessM :: Monad m => m Bool -> m () -> m ()
--unlessM mbool monad = flip unless monad =<< mbool
unlessM = join . liftM unless

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
getDirRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then (path :) <$> getDirRecursiveContents path
            else return [path]
    return (concat paths)

main :: IO ()
main = do
        createIndexMdFiles
        doHakyll

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
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
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
