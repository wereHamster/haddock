-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Html
-- Copyright   :  (c) Simon Marlow      2003-2006,
--                    David Waern       2006-2009,
--                    Mark Lentczner    2010,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module Haddock.Backends.Xhtml (
  ppHtml, copyHtmlBits,
  ppHtmlIndex, ppHtmlContents,
) where


import Prelude hiding (div)

import Haddock.Backends.Xhtml.Decl
import Haddock.Backends.Xhtml.DocMarkup
import Haddock.Backends.Xhtml.Layout
import Haddock.Backends.Xhtml.Names
import Haddock.Backends.Xhtml.Themes
import Haddock.Backends.Xhtml.Types
import Haddock.Backends.Xhtml.Utils
import Haddock.ModuleTree
import Haddock.Types
import Haddock.Version
import Haddock.Utils
import Text.XHtml hiding ( name, title, p, quote )
import qualified Text.XHtml as H
import Haddock.GhcUtils

import Control.Monad         ( when, unless )
import Data.Char             ( toUpper, isSpace, isLetter, isAscii )
import Data.List             ( sortBy, groupBy, nubBy, intercalate, isPrefixOf, intersperse )
import Data.Maybe
import System.FilePath hiding ( (</>) )
import System.Directory
import Data.Map              ( Map )
import qualified Data.Map as Map hiding ( Map )
import Data.Set (Set)
import qualified Data.Set as Set hiding ( Set )
import Data.Ord              ( comparing )
import Data.Function

import DynFlags (Language(..))
import GHC hiding ( NoLink, moduleInfo,LexicalFixity(..) )
import Name
import Module

--------------------------------------------------------------------------------
-- * Generating HTML documentation
--------------------------------------------------------------------------------


ppHtml :: DynFlags
       -> String                       -- ^ Title
       -> Maybe String                 -- ^ Package
       -> [Interface]
       -> FilePath                     -- ^ Destination directory
       -> Maybe (MDoc GHC.RdrName)     -- ^ Prologue text, maybe
       -> Themes                       -- ^ Themes
       -> Maybe String                 -- ^ The mathjax URL (--mathjax)
       -> SourceURLs                   -- ^ The source URL (--source)
       -> WikiURLs                     -- ^ The wiki URL (--wiki)
       -> Maybe String                 -- ^ The contents URL (--use-contents)
       -> Maybe String                 -- ^ The index URL (--use-index)
       -> Bool                         -- ^ Whether to use unicode in output (--use-unicode)
       -> QualOption                   -- ^ How to qualify names
       -> Bool                         -- ^ Output pretty html (newlines and indenting)
       -> IO ()

ppHtml dflags doctitle maybe_package ifaces odir prologue
        themes maybe_mathjax_url maybe_source_url maybe_wiki_url
        maybe_contents_url maybe_index_url unicode
        qual debug =  do
  let
    visible_ifaces = filter visible ifaces
    visible i = OptHide `notElem` ifaceOptions i

  when (isNothing maybe_contents_url) $
    ppHtmlContents dflags odir doctitle maybe_package
        themes maybe_mathjax_url maybe_index_url maybe_source_url maybe_wiki_url
        (map toInstalledIface visible_ifaces)
        False -- we don't want to display the packages in a single-package contents
        prologue debug (makeContentsQual qual)

  when (isNothing maybe_index_url) $
    ppHtmlIndex odir doctitle maybe_package
      themes maybe_mathjax_url maybe_contents_url maybe_source_url maybe_wiki_url
      (map toInstalledIface visible_ifaces) debug

  mapM_ (ppHtmlModule odir doctitle themes maybe_package (map toInstalledIface visible_ifaces)
           maybe_mathjax_url maybe_source_url maybe_wiki_url
           maybe_contents_url maybe_index_url unicode qual debug) visible_ifaces


copyHtmlBits :: FilePath -> FilePath -> Themes -> IO ()
copyHtmlBits odir libdir themes = do
  let
    libhtmldir = joinPath [libdir, "html"]
    copyCssFile f = copyFile f (combine odir (takeFileName f))
    copyLibFile f = copyFile (joinPath [libhtmldir, f]) (joinPath [odir, f])
  mapM_ copyCssFile (cssFiles themes)
  mapM_ copyLibFile [
      "js.cookie.js",
      "mousetrap.min.js",
      "jquery.min.js",
      "jquery.nanoscroller.min.js",
      "haddock.js"
      ]
  return ()


headHtml :: String -> Maybe String -> Themes -> Maybe String -> Html
headHtml docTitle miniPage themes mathjax_url =
  header << [
    meta ! [httpequiv "Content-Type", content "text/html; charset=UTF-8"],
    meta ! [H.name "viewport", content "width=device-width, initial-scale=1.0"],
    thetitle << docTitle,
    styleSheet themes,
    jsFile mjUrl,
    jsFile "js.cookie.js",
    jsFile "mousetrap.min.js",
    jsFile "jquery.min.js",
    jsFile "jquery.nanoscroller.min.js",
    jsFile "haddock.js",
    jsCode "window.onload = function() { haddock._initPage(); }",
    jsFile "https://use.typekit.net/lly1jgo.js",
    jsCode "try{Typekit.load({ async: true });}catch(e){}"
    ]
  where
    setSynopsis = maybe "" (\p -> "setSynopsis(\"" ++ p ++ "\");") miniPage
    mjUrl = maybe "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML" id mathjax_url
    jsFile src = script ! [H.src src, thetype "text/javascript"] << H.noHtml
    jsCode code = script ! [thetype "text/javascript"] <<
                    primHtml ("//<![CDATA[\n" ++ code ++ "\n//]]>\n")


srcButton :: SourceURLs -> Maybe Interface -> Maybe Html
srcButton (Just src_base_url, _, _, _) Nothing =
  Just (anchor ! [href src_base_url] << "Source")
srcButton (_, Just src_module_url, _, _) (Just iface) =
  let url = spliceURL (Just $ ifaceOrigFilename iface)
                      (Just $ ifaceMod iface) Nothing Nothing src_module_url
   in Just (anchor ! [href url] << "Source")
srcButton _ _ =
  Nothing


wikiButton :: WikiURLs -> Maybe Module -> Maybe Html
wikiButton (Just wiki_base_url, _, _) Nothing =
  Just (anchor ! [href wiki_base_url] << "User Comments")

wikiButton (_, Just wiki_module_url, _) (Just mdl) =
  let url = spliceURL Nothing (Just mdl) Nothing Nothing wiki_module_url
   in Just (anchor ! [href url] << "User Comments")

wikiButton _ _ =
  Nothing


contentsButton :: Maybe String -> Maybe Html
contentsButton maybe_contents_url
  = Just (anchor ! [href url] << "Contents")
  where url = fromMaybe contentsHtmlFile maybe_contents_url


indexButton :: Maybe String -> Maybe Html
indexButton maybe_index_url
  = Just (anchor ! [href url] << "Index")
  where url = fromMaybe indexHtmlFile maybe_index_url



newtype BodyClasses = BodyClasses [String]
newtype ContentsTab = ContentsTab Html

bodyHtml :: String -> Maybe Interface
    -> Maybe String -> [InstalledInterface]
    -> SourceURLs -> WikiURLs
    -> Maybe String -> Maybe String
    -> BodyClasses -> Maybe ContentsTab
    -> Html -> Html
bodyHtml doctitle iface
           maybePackage instIfaces
           maybe_source_url maybe_wiki_url
           maybe_contents_url maybe_index_url
           (BodyClasses bodyClassList) maybeContentsTab
           pageContent =
  body ! [theclass (unwords bodyClassList)] << [
    H.thediv ! [H.identifier "page"] << [
      H.thediv ! [H.identifier "page-header"] << [
        H.h1 << "Haskell",
        pageMenu
      ],
      H.thediv ! [H.identifier "content"] << pageContent,
      H.thediv ! [H.identifier "sidebar"] << [
        H.thediv ! [H.identifier "sidebar-header"] << [
          H.thediv ! [H.identifier "sidebar-logo"] << [
            H.anchor ! [H.identifier "sidebar-logo-link",
                        H.href "https://haskell.org"] << H.noHtml
          ],
          H.thediv ! [H.identifier "sidebar-title"] << [
            H.thespan ! [H.identifier "sidebar-title-text"] << sidebarTitle
          ]
        ],
        H.thediv ! [H.identifier "sidebar-tabs"] << [
          sidebarTab "sidebar-pages-tab" << genPagesTab instIfaces,
          maybeContentsTabHtml
        ]
      ],
      H.thediv ! [H.identifier "footer"] << H.paragraph << (
        "Produced by " +++
        (anchor ! [href projectUrl] << toHtml projectName) +++
        (" version " ++ projectVersion)
      )
    ]
  ]
  where
    pageMenu = H.ulist ! [H.identifier "page-menu"] << pageMenuButtons

    pageMenuButtons =
      catMaybes [
        pageSourceButton maybe_source_url iface,
        pageWikiButton maybe_wiki_url (fmap ifaceMod iface)
      ] ++ [
        docContentsButton maybe_contents_url,
        docIndexButton maybe_index_url
      ]

    sidebarTitle =
      case maybePackage of
        Just package -> package
        Nothing      -> doctitle

    maybeContentsTabHtml =
      case maybeContentsTab of
        Just (ContentsTab html) -> sidebarTab "sidebar-contents-tab" << html
        Nothing                 -> H.noHtml

    sidebarTab elementId html =
      H.thediv ! [H.identifier elementId,
                  H.theclass "sidebar-tab"] <<
        H.thediv ! [H.identifier (elementId ++ "-content"),
                    H.theclass "mini sidebar-tab-content"] <<
          html


moduleInfo :: Interface -> Html
moduleInfo iface =
   let
      info = ifaceInfo iface

      doOneEntry :: (String, HaddockModInfo GHC.Name -> Maybe String) -> Maybe HtmlTable
      doOneEntry (fieldName, field) =
        field info >>= \a -> return (th << fieldName <-> td << a)

      entries :: [HtmlTable]
      entries = maybeToList copyrightsTable ++ mapMaybe doOneEntry [
        ("License",hmi_license),
        ("Maintainer",hmi_maintainer),
        ("Stability",hmi_stability),
        ("Portability",hmi_portability),
        ("Safe Haskell",hmi_safety),
        ("Language", lg)
        ] ++ extsForm
        where
          lg inf = case hmi_language inf of
            Nothing -> Nothing
            Just Haskell98 -> Just "Haskell98"
            Just Haskell2010 -> Just "Haskell2010"

          multilineRow :: String -> [String] -> HtmlTable
          multilineRow title xs = (th ! [valign "top"]) << title <-> td << (toLines xs)
            where toLines = mconcat . intersperse br . map toHtml

          copyrightsTable :: Maybe HtmlTable
          copyrightsTable = fmap (multilineRow "Copyright" . split) (hmi_copyright info)
            where split = map (trim . filter (/= ',')) . lines

          extsForm
            | OptShowExtensions `elem` ifaceOptions iface =
              let fs = map (dropOpt . show) (hmi_extensions info)
              in case map stringToHtml fs of
                [] -> []
                [x] -> extField x -- don't use a list for a single extension
                xs -> extField $ unordList xs ! [theclass "extension-list"]
            | otherwise = []
            where
              extField x = return $ th << "Extensions" <-> td << x
              dropOpt x = if "Opt_" `isPrefixOf` x then drop 4 x else x
  in
      case entries of
        [] -> noHtml
        _ -> thediv ! [identifier "module-info"] << [
          h3 << "Information",
          table ! [H.theclass "info"] << aboves entries
          ]


--------------------------------------------------------------------------------
-- * Standard page URLs
--------------------------------------------------------------------------------

getPageSourceUrl :: SourceURLs -> Maybe Interface -> Maybe String
getPageSourceUrl (Just baseUrl, _, _, _) Nothing = Just baseUrl
getPageSourceUrl (_, Just moduleUrl, _, _) (Just iface) =
  Just (spliceURL (Just (ifaceOrigFilename iface))
                  (Just (ifaceMod iface))
                  Nothing Nothing moduleUrl)
getPageSourceUrl _ _ = Nothing

getPageWikiUrl :: WikiURLs -> Maybe Module -> Maybe String
getPageWikiUrl (Just baseUrl, _, _) Nothing = Just baseUrl
getPageWikiUrl (_, Just moduleUrl, _) (Just mdl) =
    Just (spliceURL Nothing (Just mdl) Nothing Nothing moduleUrl)
getPageWikiUrl _ _ = Nothing

--------------------------------------------------------------------------------
-- * Page menu buttons
--------------------------------------------------------------------------------

data ExistsInSidebar = NotInSidebar | ExistsInSidebar

makePageMenuButton :: String -> ExistsInSidebar -> String -> Html
makePageMenuButton label existsInSidebar url =
  H.li ! liAttrs << (H.anchor ! [H.href url] << label)
  where
    liAttrs = case existsInSidebar of
      ExistsInSidebar -> [H.theclass "exists-in-sidebar"]
      NotInSidebar    -> []

pageSourceButton :: SourceURLs -> Maybe Interface -> Maybe Html
pageSourceButton sourceUrls maybeIface =
  case getPageSourceUrl sourceUrls maybeIface of
    Just url -> Just (makePageMenuButton "Source" NotInSidebar url)
    Nothing  -> Nothing

pageWikiButton :: WikiURLs -> Maybe Module -> Maybe Html
pageWikiButton sourceUrls maybeMdl =
  case getPageWikiUrl sourceUrls maybeMdl of
    Just url -> Just (makePageMenuButton "Wiki" NotInSidebar url)
    Nothing  -> Nothing

docContentsButton :: Maybe String -> Html
docContentsButton maybeContentsUrl =
  makePageMenuButton "Contents" ExistsInSidebar url
  where
    url = fromMaybe contentsHtmlFile maybeContentsUrl

docIndexButton :: Maybe String -> Html
docIndexButton maybeIndexUrl =
  makePageMenuButton "Index" ExistsInSidebar url
  where
    url = fromMaybe indexHtmlFile maybeIndexUrl

--------------------------------------------------------------------------------
-- * Generate the module contents
--------------------------------------------------------------------------------


ppHtmlContents
   :: DynFlags
   -> FilePath
   -> String
   -> Maybe String
   -> Themes
   -> Maybe String
   -> Maybe String
   -> SourceURLs
   -> WikiURLs
   -> [InstalledInterface] -> Bool -> Maybe (MDoc GHC.RdrName)
   -> Bool
   -> Qualification  -- ^ How to qualify names
   -> IO ()
ppHtmlContents dflags odir doctitle _maybe_package
  themes mathjax_url maybe_index_url
  maybe_source_url maybe_wiki_url ifaces showPkgs prologue debug qual = do
  let tree = mkModuleTree dflags showPkgs
              [(instMod iface, toInstalledDescription iface) | iface <- ifaces]
      html =
        headHtml doctitle Nothing themes mathjax_url +++
        bodyHtml doctitle Nothing
               _maybe_package ifaces
               maybe_source_url maybe_wiki_url
               Nothing maybe_index_url (BodyClasses []) Nothing << [
                 ppPrologue qual doctitle prologue,
                 ppModuleTree qual tree
               ]
  createDirectoryIfMissing True odir
  writeFile (joinPath [odir, contentsHtmlFile]) (renderToString debug html)


ppPrologue :: Qualification -> String -> Maybe (MDoc GHC.RdrName) -> Html
ppPrologue _ _ Nothing = noHtml
ppPrologue qual title (Just doc) =
  thediv ! [identifier "contents-prologue"] << (h1 << title +++ docElement thediv (rdrDocToHtml qual doc))


ppModuleTree :: Qualification -> [ModuleTree] -> Html
ppModuleTree qual ts =
  thediv ! [identifier "module-list"] << (h1 << "Modules" +++ mkNodeList qual [] "n" ts)


mkNodeList :: Qualification -> [String] -> String -> [ModuleTree] -> Html
mkNodeList qual ss p ts = case ts of
  [] -> noHtml
  _ -> unordList (zipWith (mkNode qual ss) ps ts)
  where
    ps = [ p ++ '.' : show i | i <- [(1::Int)..]]


mkNode :: Qualification -> [String] -> String -> ModuleTree -> Html
mkNode qual ss p (Node s leaf pkg srcPkg short ts) =
  htmlModule <+> shortDescr +++ htmlPkg +++ subtree
  where
    modAttrs = case (ts, leaf) of
      (_:_, False) -> collapseControl p True "module"
      (_,   _    ) -> [theclass "module"]

    cBtn = case (ts, leaf) of
      (_:_, True) -> thespan ! collapseControl p True "" << H.noHtml
      (_,   _   ) -> noHtml
      -- We only need an explicit collapser button when the module name
      -- is also a leaf, and so is a link to a module page. Indeed, the
      -- spaceHtml is a minor hack and does upset the layout a fraction.

    htmlModule = thespan ! modAttrs << (cBtn +++
      if leaf
        then ppModule (mkModule (stringToUnitId (fromMaybe "" pkg))
                                       (mkModuleName mdl))
        else toHtml s
      )

    mdl = intercalate "." (reverse (s:ss))

    shortDescr =
      case short of
        Just s -> H.thespan ! [H.theclass "short-descr"] << origDocToHtml qual s
        Nothing -> H.noHtml

    htmlPkg =
      case srcPkg of
        Just pkg -> H.thespan ! [H.theclass "package"] << pkg
        Nothing  -> H.noHtml

    subtree = mkNodeList qual (s:ss) p ts ! collapseSection p True ""



genPagesTab :: [InstalledInterface] -> Html
genPagesTab ifaces =
  H.toHtml [
    H.unordList [
      standardPageLink "Contents" "index.html",
      standardPageLink "Index" "doc-index.html"
    ],
    H.thediv ! [H.theclass "module-list"] << [
      H.h1 ! [H.theclass "mini-item"] << "Modules",
      H.ulist << map moduleLink (genTabModuleList ifaces)
    ]
  ]
  where
    moduleLink m = H.li ! [H.theclass "module"] << m
    standardPageLink label url =
      H.anchor ! [H.href url, H.theclass "mini-item", H.target "_parent"] <<
        label

--------------------------------------------------------------------------------

genTabModuleList :: [InstalledInterface] -> [Html]
genTabModuleList ifaces =
  map (\(name, mdl) -> makeLink name mdl)
    $ nubBy ((==) `on` fst)
    $ sortBy (comparing fst)
    $ mods
  where
    mods = [(moduleString mdl, mdl) | mdl <- map instMod ifaces]
    makeLink name mdl =
      H.anchor ! [H.href (moduleHtmlFile mdl), H.theclass "mini-item",
                  H.target "_parent"] <<
        name

--------------------------------------------------------------------------------
-- * Generate the index
--------------------------------------------------------------------------------

type Index = Map String (Set IndexEntry)

data IndexEntry = IndexEntry !GHC.Name !GHC.Module !IndexType !Bool
  deriving (Eq, Ord)

data IndexType = TypeOrClassIT | ConstructorIT | FunctionIT
  deriving (Eq, Ord)

--------------------------------------------------------------------------------

buildIndex :: [InstalledInterface] -> Index
buildIndex ifaces =
  Map.unionsWith Set.union (map getIfaceIndex ifaces)
  where
    getIfaceIndex :: InstalledInterface -> Index
    getIfaceIndex iface =
      makeIndex (instExports iface)
      where
        makeIndex [] = Map.empty
        makeIndex (name:xs) =
          insertIndexEntry (occNameString (nameOccName name))
                           (makeEntry name) (makeIndex xs)
        makeEntry name =
          IndexEntry name mdl (getEntryType name) (Set.member name visible)
        visible = Set.fromList (instVisibleExports iface)
        mdl = instMod iface

    insertIndexEntry :: String -> IndexEntry -> Index -> Index
    insertIndexEntry name entry index =
      Map.insert name (Set.insert entry existingEntries) index
      where
        existingEntries = case Map.lookup name index of
          Just entries -> entries
          Nothing      -> Set.empty

    getEntryType name
      | not (isValOcc on) = TypeOrClassIT
      | isDataOcc on      = ConstructorIT
      | otherwise         = FunctionIT
      where on = nameOccName name


ppHtmlIndex :: FilePath
            -> String
            -> Maybe String
            -> Themes
            -> Maybe String
            -> Maybe String
            -> SourceURLs
            -> WikiURLs
            -> [InstalledInterface]
            -> Bool
            -> IO ()
ppHtmlIndex odir doctitle _maybe_package themes
  maybe_mathjax_url maybe_contents_url maybe_source_url maybe_wiki_url ifaces debug = do
  createDirectoryIfMissing True odir
  writeFile (joinPath [odir, indexHtmlFile]) (renderToString debug html)
  when doLetterPages $ do
    let allHtml = makeIndexPage True Nothing allIndex
    mapM_ writeLetterIndex alphabetChars
    writeFile (joinPath [odir, subIndexHtmlFile allName])
              (renderToString debug allHtml)
    when (not (null otherIndex)) $ do
      let otherHtml = makeIndexPage True Nothing otherIndex
      writeFile (joinPath [odir, subIndexHtmlFile otherName])
                (renderToString debug otherHtml)
  where
    html = makeIndexPage doLetterPages Nothing
                                 (if doLetterPages then [] else allIndex)

    indexName ch = "Index" ++ maybe "" (\c -> " - " ++ [c]) ch

    makeIndexPage showAlphabet ch items =
      headHtml (doctitle ++ " (" ++ indexName ch ++ ")") Nothing themes maybe_mathjax_url +++
      bodyHtml doctitle Nothing
               _maybe_package ifaces
               maybe_source_url maybe_wiki_url
               maybe_contents_url Nothing
               (BodyClasses []) Nothing << [
        if showAlphabet then alphabetHtml else H.noHtml,
        H.thediv ! [H.identifier "index"] <<
          if null items
            then [H.p ! [H.theclass "no-items"] << "Select a letter."]
            else [H.h1 << indexName ch, genIndexTable items]
      ]

    -- Basic indices
    ----------------

    allIndex =
      sortBy cmp (Map.toList (buildIndex ifaces))
      where
        cmp (n1,_) (n2,_) = comparing (map toUpper) n1 n2

    otherIndex =
      filter func allIndex
      where
        func (name,_) = Set.notMember (toUpper (head name)) alphabetCharSet

    doLetterPages = length allIndex > 150

    writeLetterIndex ch =
      unless (null items) $
        writeFile (joinPath [odir, subIndexHtmlFile [ch]])
                  (renderToString debug html)
      where
        html = makeIndexPage True (Just ch) items
        items = filter filterFunc allIndex
        filterFunc (name,_) = toUpper (head name) == ch

    -- Alphabet
    -----------

    alphabetHtml =
      H.thediv ! [H.identifier "alphabet"] <<
        H.unordList (map makeLink linkNames)
      where
        makeLink name =
          H.anchor ! ([H.href (subIndexHtmlFile name)] ++ nonAsciiClass) << name
          where
            nonAsciiClass =
              if all isAscii name then [] else [H.theclass "non-ascii"]
        linkNames =
          map (\c -> [c]) alphabetChars ++
          if haveOtherChars then [otherName] else [] ++
          [allName]

    allName   = "All"
    otherName = "Other"

    basicSpecials = ":!#$%&*+./<=>?@\\^|-~_"

    initialCharSet =
      Set.fromList (map (\(name,_) -> toUpper (head name)) allIndex)

    initialLetterSet = Set.filter isLetter initialCharSet

    alphabetChars =
      Set.toAscList initialLetterSet ++
      filter (\ch -> Set.member ch initialCharSet) basicSpecials

    alphabetCharSet = Set.fromList alphabetChars

    haveOtherChars = not (initialLetterSet == alphabetCharSet)

    -- Index table
    --------------

    genIndexTable items = H.table << H.aboves (concatMap genIndexEntry items)

    genIndexEntry (name, entrySet) =
      if allSame (map getEntryType entryList)
        then [H.besides [nameTd, genLinksTd entryList]]
        else
          [H.besides [nameTd, H.td << H.noHtml]] ++
          genCategorizedEntries entryList
      where
        entryList = Set.toAscList entrySet
        nameTd = H.td ! [H.theclass "src"] << name

    genCategorizedEntries entryList =
      catMaybes [
        makeRow "Type/Class"  (filterByType TypeOrClassIT),
        makeRow "Constructor" (filterByType ConstructorIT),
        makeRow "Function"    (filterByType FunctionIT)
      ]
      where
        filterByType t = filter (\e -> getEntryType e == t) entryList
        makeRow _ [] = Nothing
        makeRow rowName rowEntries =
          Just (H.besides [
            H.td ! [H.theclass "cat"] << rowName,
            genLinksTd rowEntries
          ])

    genLinksTd entryList =
      H.td ! [H.theclass "modules"] << links
      where
        links = hsep (punctuate comma (map makeLink entryList))
        makeLink (IndexEntry name mdl _ visible) =
          if visible
            then linkId mdl (Just name) << moduleString mdl
            else H.toHtml (moduleString mdl)

    -- Utilities
    ------------

    getEntryType (IndexEntry _ _ t _) = t

--------------------------------------------------------------------------------
-- * Generate the HTML page for a module
--------------------------------------------------------------------------------


ppHtmlModule
        :: FilePath -> String -> Themes
        -> Maybe String -> [InstalledInterface]
        -> Maybe String -> SourceURLs -> WikiURLs
        -> Maybe String -> Maybe String -> Bool -> QualOption
        -> Bool -> Interface -> IO ()
ppHtmlModule odir doctitle themes
  maybePackage instIfaces
  maybe_mathjax_url maybe_source_url maybe_wiki_url
  maybe_contents_url maybe_index_url unicode qual debug iface = do
  let
      mdl = ifaceMod iface
      aliases = ifaceModuleAliases iface
      mdl_str = moduleString mdl
      real_qual = makeModuleQual qual aliases mdl
      exports = numberSectionHeadings (ifaceRnExportItems iface)
      ctsTab = genModuleContentsTab mdl iface unicode real_qual
      html =
        headHtml mdl_str (Just $ "mini_" ++ moduleHtmlFile mdl) themes maybe_mathjax_url +++
        bodyHtml doctitle (Just iface)
               maybePackage instIfaces
               maybe_source_url maybe_wiki_url
               maybe_contents_url maybe_index_url
               (BodyClasses ["has-module-prologue"]) (Just (ContentsTab ctsTab)) << [
          h1 ! [theclass "module-name"] << mdl_str,
          -- thediv ! [identifier "module-prologue"] << [
          --   moduleInfo iface,
          --   ppModuleContents real_qual exports (not . null $ ifaceRnOrphanInstances iface)
          -- ],
          genInterfaceDocs maybe_source_url maybe_wiki_url iface unicode real_qual
        ]

  createDirectoryIfMissing True odir
  writeFile (joinPath [odir, moduleHtmlFile mdl]) (renderToString debug html)

ppHtmlModuleMiniSynopsis :: FilePath -> String -> Themes
  -> Maybe String -> Interface -> Bool -> Qualification -> Bool -> IO ()
ppHtmlModuleMiniSynopsis odir _doctitle themes maybe_mathjax_url iface unicode qual debug = do
  let mdl = ifaceMod iface
      html =
        headHtml (moduleString mdl) Nothing themes maybe_mathjax_url +++
        miniBody <<
          (divModuleHeader << sectionName << moduleString mdl +++
           miniSynopsis mdl iface unicode qual)
  createDirectoryIfMissing True odir
  writeFile (joinPath [odir, "mini_" ++ moduleHtmlFile mdl]) (renderToString debug html)


ifaceToHtml :: SourceURLs -> WikiURLs -> Interface -> Bool -> Qualification -> Html
ifaceToHtml maybe_source_url maybe_wiki_url iface unicode qual
  = ppModuleContents qual exports (not . null $ ifaceRnOrphanInstances iface) +++
    description +++
    -- synopsis +++
    divInterface (maybe_doc_hdr +++ bdy +++ orphans)
  where
    exports = numberSectionHeadings (ifaceRnExportItems iface)

    -- todo: if something has only sub-docs, or fn-args-docs, should
    -- it be measured here and thus prevent omitting the synopsis?
    has_doc ExportDecl { expItemMbDoc = (Documentation mDoc mWarning, _) } = isJust mDoc || isJust mWarning
    has_doc (ExportNoDecl _ _) = False
    has_doc (ExportModule _) = False
    has_doc _ = True

    no_doc_at_all = not (any has_doc exports)

    description | isNoHtml doc = doc
                | otherwise    = divDescription $ doc
                where doc = docSection Nothing qual (ifaceRnDoc iface)

        -- omit the synopsis if there are no documentation annotations at all
    synopsis
      | no_doc_at_all = noHtml
      | otherwise
      = divSynopsis $
            paragraph ! collapseControl "syn" False "caption" << "Synopsis" +++
            shortDeclList (
                mapMaybe (processExport True linksInfo unicode qual) exports
            ) ! (collapseSection "syn" False "" ++ collapseToggle "syn")

        -- if the documentation doesn't begin with a section header, then
        -- add one ("Documentation").
    maybe_doc_hdr
      = case exports of
          [] -> noHtml
          ExportGroup {} : _ -> noHtml
          _ -> h1 << "Documentation"

    bdy =
      foldr (+++) noHtml $
        mapMaybe (processExport False linksInfo unicode qual) exports

    orphans =
      ppOrphanInstances linksInfo (ifaceRnOrphanInstances iface) False unicode qual

    linksInfo = (maybe_source_url, maybe_wiki_url)


miniSynopsis :: Module -> Interface -> Bool -> Qualification -> Html
miniSynopsis mdl iface unicode qual =
    divInterface << concatMap (processForMiniSynopsis mdl unicode qual) exports
  where
    exports = numberSectionHeadings (ifaceRnExportItems iface)


processForMiniSynopsis :: Module -> Bool -> Qualification -> ExportItem DocName
                       -> [Html]
processForMiniSynopsis mdl unicode qual ExportDecl { expItemDecl = L _loc decl0 } =
  ((divTopDecl <<).(declElem <<)) <$> case decl0 of
    TyClD d -> let b = ppTyClBinderWithVarsMini mdl d in case d of
        (FamDecl decl)    -> [ppTyFamHeader True False decl unicode qual]
        (DataDecl{})   -> [keyword "data" <+> b]
        (SynDecl{})    -> [keyword "type" <+> b]
        (ClassDecl {}) -> [keyword "class" <+> b]
    SigD (TypeSig lnames _) ->
      map (ppNameMini Prefix mdl . nameOccName . getName . unLoc) lnames
    _ -> []
processForMiniSynopsis _ _ qual (ExportGroup lvl _id txt) =
  [groupTag lvl << docToHtml Nothing qual (mkMeta txt)]
processForMiniSynopsis _ _ _ _ = []


ppNameMini :: Notation -> Module -> OccName -> Html
ppNameMini notation mdl nm =
  thespan ! [theclass "main-name"]
    << ppBinder' notation nm


ppTyClBinderWithVarsMini :: Module -> TyClDecl DocName -> Html
ppTyClBinderWithVarsMini mdl decl =
  let n = tcdName decl
      ns = tyvarNames $ tcdTyVars decl -- it's safe to use tcdTyVars, see code above
  in ppTypeApp n [] ns (\is_infix -> ppNameMini is_infix mdl . nameOccName . getName) ppParamNameMini

ppModuleContents :: Qualification
                 -> [ExportItem DocName]
                 -> Bool -- ^ Orphans sections
                 -> Html
ppModuleContents qual exports orphan
  | null sections && not orphan  = noHtml
  | otherwise                    = contentsDiv
 where
  contentsDiv = thediv ! [identifier "table-of-contents"] << (
      h3 << "Contents" +++
      unordList sections)

  (sections, _leftovers{-should be []-}) = process 0 exports
  orphanSection
    | orphan =  [ linkedAnchor "section.orphans" << "Orphan instances" ]
    | otherwise = []

  process :: Int -> [ExportItem DocName] -> ([Html],[ExportItem DocName])
  process _ [] = ([], [])
  process n items@(ExportGroup lev id0 doc : rest)
    | lev <= n  = ( [], items )
    | otherwise = ( html:secs, rest2 )
    where
      html = linkedAnchor (groupId id0)
             << docToHtmlNoAnchors (Just id0) qual (mkMeta doc) +++ mk_subsections ssecs
      (ssecs, rest1) = process lev rest
      (secs,  rest2) = process n   rest1
  process n (_ : rest) = process n rest

  mk_subsections [] = noHtml
  mk_subsections ss = unordList ss

-- we need to assign a unique id to each section heading so we can hyperlink
-- them from the contents:
numberSectionHeadings :: [ExportItem DocName] -> [ExportItem DocName]
numberSectionHeadings = go 1
  where go :: Int -> [ExportItem DocName] -> [ExportItem DocName]
        go _ [] = []
        go n (ExportGroup lev _ doc : es)
          = ExportGroup lev (show n) doc : go (n+1) es
        go n (other:es)
          = other : go n es

--------------------------------------------------------------------------------

genInterfaceDocs
    :: SourceURLs -> WikiURLs -> Interface -> Bool -> Qualification -> Html
genInterfaceDocs maybeSourceUrl maybeWikiUrl iface unicode qual =
  H.toHtml [
    description,
    -- synopsis,
    H.thediv ! [H.identifier "interface"] << [
      maybeDocHeader,
      docBody
    ]
  ]
  where
    exports = numberSectionHeadings (ifaceRnExportItems iface)

    description
      | H.isNoHtml doc = doc
      | otherwise = H.thediv ! [theclass "module-description"] << [doc]
      where
        doc = docSection Nothing qual (ifaceRnDoc iface)

    -- Omit the synopsis if there are no documentation annotations at all
    synopsis
      | shouldOmitSynopsis exports = H.noHtml
      | otherwise =
          H.thediv ! [H.identifier "synopsis"] << (
            H.h1 ! collapseControl "syn" False "" << "Synopsis" +++
            shortDeclList (
              mapMaybe (processExport True linksInfo unicode qual) exports
            ) ! (collapseSection "syn" False "" ++ collapseToggle "syn")
          )

    -- If the documentation doesn't begin with a section header, then add one
    -- ("Documentation").
    maybeDocHeader = case exports of
        []                -> H.noHtml
        ExportGroup{} : _ -> H.noHtml
        _                 -> H.h1 << "Documentation"

    docBody = H.toHtml processedExports

    processedExports =
      mapMaybe (processExport False linksInfo unicode qual) exports

    linksInfo = (maybeSourceUrl, maybeWikiUrl)

processExport :: Bool -> LinksInfo -> Bool -> Qualification
              -> ExportItem DocName -> Maybe Html
processExport _ _ _ _ ExportDecl { expItemDecl = L _ (InstD _) } = Nothing -- Hide empty instances
processExport summary _ _ qual (ExportGroup lev id0 doc)
  = nothingIf summary $ groupHeading lev id0 << docToHtml (Just id0) qual (mkMeta doc)
processExport summary links unicode qual (ExportDecl decl doc subdocs insts fixities splice)
  = processDecl summary $ ppDecl summary links decl doc insts fixities subdocs splice unicode qual
processExport summary _ _ qual (ExportNoDecl y [])
  = processDeclOneLiner summary $ ppDocName qual Prefix True y
processExport summary _ _ qual (ExportNoDecl y subs)
  = processDeclOneLiner summary $
      ppDocName qual Prefix True y
      +++ parenList (map (ppDocName qual Prefix True) subs)
processExport summary _ _ qual (ExportDoc doc)
  = nothingIf summary $ docSection_ Nothing qual doc
processExport summary _ _ _ (ExportModule mdl)
  = processDeclOneLiner summary $ toHtml "module" <+> ppModule mdl


nothingIf :: Bool -> a -> Maybe a
nothingIf True _ = Nothing
nothingIf False a = Just a


processDecl :: Bool -> Html -> Maybe Html
processDecl True = Just
processDecl False = Just . divTopDecl

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

processDeclOneLiner :: Bool -> Html -> Maybe Html
processDeclOneLiner True = Just
processDeclOneLiner False = Just . divTopDecl . declElem

groupHeading :: Int -> String -> Html -> Html
groupHeading lev id0 = groupTag lev ! [identifier (groupId id0)]

groupTag :: Int -> Html -> Html
groupTag lev
  | lev == 1  = h1
  | lev == 2  = h2
  | lev == 3  = h3
  | otherwise = h4

--------------------------------------------------------------------------------
-- * Module contents tab
--------------------------------------------------------------------------------

genModuleContentsTab :: Module -> Interface -> Bool -> Qualification -> Html
genModuleContentsTab mdl iface unicode qual =
  H.thediv ! [H.theclass "contents module"] <<
    H.ulist << (standardPages ++ members)
  where
    standardPages = [
        standardPage "#description" "Description",
        if not (shouldOmitSynopsis exports)
          then standardPage "#synopsis" "Synopsis"
          else H.noHtml
      ]
    standardPage href name =
      H.li ! [H.theclass "std"] << H.h1 <<
        H.anchor ! [H.theclass "mini-item", H.href href] << name
    exports = numberSectionHeadings (ifaceRnExportItems iface)
    members = processForModuleContentsTab mdl unicode qual exports

--------------------------------------------------------------------------------

processForModuleContentsTab
    :: Module -> Bool -> Qualification -> [ExportItem DocName] -> [Html]
processForModuleContentsTab mdl unicode qual items =
  let (result, _) = process 0 items in result
  where
    process :: Int -> [ExportItem DocName] -> ([Html], [ExportItem DocName])
    process _ [] = ([], [])
    process level (item:remaining) = case item of
      ExportDecl { expItemDecl = L _ itemDecl } ->
        (fmap resultToHtml results ++ remainingMembers, remainingAfterThis)
        where
          (remainingMembers, remainingAfterThis) =
             process level remaining
          resultToHtml (html, url) =
            H.li ! [H.theclass "top"] <<
              H.anchor ! [H.theclass "mini-item top", H.href url] << html
          results = processExportItem itemDecl
      ExportGroup level' groupId title ->
        if level' > level
          then -- This is a child group.
            let
              hash = "#g:" ++ groupId
              titleHtml = docToHtml Nothing qual (mkMeta title)
              childLiClasses = unwords (
                  ["group"] ++
                  if not (null childMembers) then [] else ["no-items"]
                )
              childLi =
                H.li ! [H.theclass childLiClasses] << [
                  groupTag level <<
                    H.anchor ! [H.theclass "mini-item", H.href hash] << titleHtml,
                  H.ulist ! [H.theclass "members"] << childMembers
                ]
              (childMembers, remainingAfterChild) =
                process level' remaining
              (remainingMembers, remainingAfterThis) =
                process level remainingAfterChild
            in
              (childLi:remainingMembers, remainingAfterThis)
          else -- This is another group of the same/greater level.
            ([], item:remaining)
      _ -> process level remaining

    processExportItem itemDecl = case itemDecl of
      TyClD d ->
        [(html, url)]
        where
          html = case d of
            (FamDecl   decl) -> [ppTyFamHeader True False decl unicode qual]
            (DataDecl  {})   -> [keyword "data" <+> b]
            (SynDecl   {})   -> [keyword "type" <+> b]
            (ClassDecl {})   -> [keyword "class" <+> b]
          url = moduleNameUrl mdl (nameOccName (getName (tcdName d)))
          b = ppTyClBinderWithVarsMini mdl d
      SigD (TypeSig lnames _) ->
        map process lnames
        where
          process lname = (html, url)
            where
              occName = nameOccName (getName (unLoc lname))
              html    = [ppNameMini Prefix mdl occName]
              url     = moduleNameUrl mdl occName
      _ -> []

--------------------------------------------------------------------------------


ppParamNameMini :: Name -> Html
ppParamNameMini nm = thespan ! [theclass "param"] << ppTyName nm


--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (== x) xs

-- TODO: if something has only sub-docs, or fn-args-docs, should it be measured
-- here and thus prevent omitting the synopsis?
exportHasDoc :: ExportItem n -> Bool
exportHasDoc ExportDecl {expItemMbDoc = (Documentation mDoc mWarning, _)} =
  isJust mDoc || isJust mWarning
exportHasDoc (ExportNoDecl _ _) = False
exportHasDoc (ExportModule _) = False
exportHasDoc _ = False

noDocumentedExports :: [ExportItem n] -> Bool
noDocumentedExports = not . any exportHasDoc

shouldOmitSynopsis :: [ExportItem n] -> Bool
shouldOmitSynopsis = noDocumentedExports

--------------------------------------------------------------------------------
