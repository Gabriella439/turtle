{-# Language CPP #-}
{-# Options_GHC -Wno-deprecations #-}

module Main (main) where

import System.FilePath (splitExtensions)
import Test.Tasty
import Test.Tasty.HUnit
import Turtle

main :: IO ()
main = defaultMain $ testGroup "system-filepath tests"
    [ test_Root
    , test_Directory
    , test_StripPrefix
    , test_Filename
    , test_Dirname
    , test_Basename
    , test_Absolute
    , test_Relative
    , test_SplitDirectories
    , test_SplitExtension
    ]

test_Root :: TestTree
test_Root = testCase "root" $ do
    "" @=? root ""
    "/" @=? root "/"
    "" @=? root "foo"
    "/" @=? root "/foo"

test_Directory :: TestTree
test_Directory = testCase "directory" $ do
    "./" @=? directory ""
    "/" @=? directory "/"
    "/foo/" @=? directory "/foo/bar"
    "/foo/bar/" @=? directory "/foo/bar/"
    "./" @=? directory "."
    "../" @=? directory ".."
    "../" @=? directory "../foo"
    "../foo/" @=? directory "../foo/"
    "./" @=? directory "foo"
    "foo/" @=? directory "foo/bar"

test_Filename :: TestTree
test_Filename = testCase "filename" $ do
    "" @=? filename ""
    "" @=? filename "/"
    "" @=? filename "/foo/"
    "bar" @=? filename "/foo/bar"
    "bar.txt" @=? filename "/foo/bar.txt"

test_Dirname :: TestTree
test_Dirname = testCase "dirname" $ do
    "" @=? dirname ""
    "" @=? dirname "/"
    "" @=? dirname "foo"
    "foo" @=? dirname "foo/bar"
    "bar" @=? dirname "foo/bar/"
    "bar" @=? dirname "foo/bar/baz.txt"

    -- the directory name will be re-parsed to a file name.
    let dirnameExts q = extensions (dirname q)
    ["d"] @=? dirnameExts "foo.d/bar"

    -- reparsing preserves good/bad encoding state
    ["\xB1", "\x76A"] @=? dirnameExts "foo.\xB1.\xDD\xAA/bar"

test_Basename :: TestTree
test_Basename = testCase "basename" $ do

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    "bar" @=? basename "c:\\foo\\bar"
    "bar" @=? basename "c:\\foo\\bar.txt"
#else
    "bar" @=? basename "/foo/bar"
    "bar" @=? basename "/foo/bar.txt"
#endif

test_Absolute :: TestTree
test_Absolute = testCase "absolute" $ do
    let myAssert q = assertBool ("absolute " ++ show q) $ absolute q
    let myAssert' q = assertBool ("not $ absolute " ++ show q) $ not $ absolute q

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    myAssert "c:\\"
    myAssert "c:\\foo\\bar"
    myAssert' ""
    myAssert' "foo\\bar"
    myAssert' "\\foo\\bar"
#else
    myAssert "/"
    myAssert "/foo/bar"
    myAssert' ""
    myAssert' "foo/bar"
#endif


test_Relative :: TestTree
test_Relative = testCase "relative" $ do
    let myAssert q = assertBool ("relative " ++ show q) $ relative q
    let myAssert' q = assertBool ("not $ relative " ++ show q) $ not $ relative q

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    myAssert' "c:\\"
    myAssert' "c:\\foo\\bar"
    myAssert ""
    myAssert "foo\\bar"
    myAssert' "\\foo\\bar"
#else
    myAssert' "/"
    myAssert' "/foo/bar"
    myAssert ""
    myAssert "foo/bar"
#endif

test_StripPrefix :: TestTree
test_StripPrefix = testCase "stripPrefix" $ do
    Just "" @=? stripPrefix "" ""
    Just "/" @=? stripPrefix "" "/"
    Just "" @=? stripPrefix "/" "/"
    Just "foo" @=? stripPrefix "/" "/foo"
    Just "foo/bar" @=? stripPrefix "/" "/foo/bar"
    Just "bar" @=? stripPrefix "/foo/" "/foo/bar"
    Just "bar/baz" @=? stripPrefix "/foo/" "/foo/bar/baz"
    Just ".txt" @=? stripPrefix "/foo/bar" "/foo/bar.txt"
    Just ".gz" @=? stripPrefix "/foo/bar.txt" "/foo/bar.txt.gz"

    -- Test ignoring non-matching prefixes
    Nothing @=? stripPrefix "/foo" "/foo/bar"
    Nothing @=? stripPrefix "/foo/bar/baz" "/foo"
    Nothing @=? stripPrefix "/foo/baz/" "/foo/bar/qux"
    Nothing @=? stripPrefix "/foo/bar/baz" "/foo/bar/qux"
    Nothing @=? stripPrefix "/foo/bar/baz" "/foo/bar/qux"

test_SplitDirectories :: TestTree
test_SplitDirectories = testCase "splitDirectories" $ do
    [] @=? splitDirectories ""
    ["/"] @=? splitDirectories "/"
    ["/", "a"] @=? splitDirectories "/a"
    ["/", "ab/", "cd"] @=? splitDirectories "/ab/cd"
    ["/", "ab/", "cd/"] @=? splitDirectories "/ab/cd/"
    ["ab/", "cd"] @=? splitDirectories "ab/cd"
    ["ab/", "cd/"] @=? splitDirectories "ab/cd/"
    ["ab/", "cd.txt"] @=? splitDirectories "ab/cd.txt"
    ["ab/", "cd/", ".txt"] @=? splitDirectories "ab/cd/.txt"
    ["ab/", ".", "cd"] @=? splitDirectories "ab/./cd"

test_SplitExtension :: TestTree
test_SplitExtension = testCase "splitExtension" $ do
    ("", Nothing) @=? splitExtension ""
    ("foo", Nothing) @=? splitExtension "foo"
    ("foo", Just "") @=? splitExtension "foo."
    ("foo", Just "a") @=? splitExtension "foo.a"
    ("foo.a/", Nothing) @=? splitExtension "foo.a/"
    ("foo.a/bar", Nothing) @=? splitExtension "foo.a/bar"
    ("foo.a/bar", Just "b") @=? splitExtension "foo.a/bar.b"
    ("foo.a/bar.b", Just "c") @=? splitExtension "foo.a/bar.b.c"

extensions :: FilePath -> [String]
extensions p = case splitExtensions p of
    (p', "") -> [p']
    (p', q ) -> extensions p' ++ [q]
